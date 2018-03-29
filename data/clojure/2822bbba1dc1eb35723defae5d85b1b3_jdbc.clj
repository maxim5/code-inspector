;; The MIT License
;;
;; Copyright (c) 2010 Erik Soehnel
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; provide a basic jdbc-interface for the cdc-system
(ns cdc.jdbc
  (:use [clojure.pprint :only [pprint]])
  (:require [cdc.mysql-binlog :as binlog]
            ;;[clojure.contrib.sql.internal :as sql]
            )
  (:import (java.sql DriverManager
                     DriverPropertyInfo)
           (java.util Properties)
           java.util.concurrent.LinkedBlockingQueue))
;; do not import the classes we are implementing

(def jdbc-state (ref {:connection nil
                      :statement nil
                      :resultset nil
                      :binlog-state nil
                      :queue nil}))

(defn set-new-queue
  "set a new queue"
  []
  (let [queue (LinkedBlockingQueue. 10)
        event-fn #(do (println "event-fn: got" (count %) "events!")
                      (.put #^LinkedBlockingQueue queue %))]
    (dosync (alter jdbc-state assoc :queue queue)
            (send (:binlog-state @jdbc-state) assoc :event-fn event-fn))))

(defn throwf-sql-not-supported [& args]
  (throw (java.sql.SQLFeatureNotSupportedException. (apply format (or args [""])))))

(defn throwf-unsupported [& args]
  (throw (UnsupportedOperationException. (apply format (or args [""])))))

(defn throwf-illegal [& args]
  (throw (IllegalArgumentException. (apply format (or args [""])))))

(defn throwf [& [format-string & args]]
  (throw (Exception. (apply format (str "cdc.jdbc: " format-string) args))))

(defn driver-property-info [& {n :name,
                               v :value,
                               d :description,
                               c :choices,
                               r? :required}]
  (let [info (DriverPropertyInfo. (str n) (str v))]
    (set! (.choices info) (into-array String (map str c)))
    (set! (.description info) (str d))
    (set! (.required info) (boolean r?))
    info))

;;(def _ee (binlog/read-binlog "/var/log/mysql/binlog.001024"))

(deftype ResultSetMetaData [schema table row]
  java.sql.ResultSetMetaData 
  (getCatalogName [t c] "") ;; Gets the designated column's table's catalog name.
  (getColumnClassName [t c] (type (nth row (dec c)))) ;;Returns the fully-qualified name of the Java class whose instances are manufactured if the method ResultSet.getObject is called to retrieve a value from the column.
  (getColumnCount [t] (count row)) ;; int; Returns the number of columns in this ResultSet object.
  (getColumnDisplaySize [t c] 1024) ;; int; Indicates the designated column's normal maximum width in characters.
  (getColumnLabel [t c] (str c)) ;; String; Gets the designated column's suggested title for use in printouts and displays.
  (getColumnName [t c] (str c)) ;; String; Get the designated column's name.
  (getColumnType [t c] ;; int; Retrieves the designated column's SQL type.
                 (let [v (nth row (dec c))]
                   (cond (decimal? v) java.sql.Types/DECIMAL 
                         (integer? v) java.sql.Types/INTEGER
                         (string? v) java.sql.Types/VARCHAR
                         :else (throwf "unsupported type: %s" (type v)))))
  (getColumnTypeName [t c] (str (.getColumnType t c))) ;; String; Retrieves the designated column's database-specific type name.
  (getPrecision [t c] 10) ;; int; Get the designated column's specified column size.
  (getScale [t c] 10) ;; int; Gets the designated column's number of digits to right of the decimal point.
  (getSchemaName [t c] schema) ;; String; Get the designated column's table's schema.
  (getTableName [t c] table) ;; String; Gets the designated column's table name.
  (isAutoIncrement [t c] false) ;; boolean; Indicates whether the designated column is automatically numbered.
  (isCaseSensitive [t c] true) ;; boolean; Indicates whether a column's case matters.
  (isCurrency [t c] false) ;; boolean; Indicates whether the designated column is a cash value.
  (isDefinitelyWritable [t c] false) ;; boolean; Indicates whether a write on the designated column will definitely succeed.
  (isNullable [t c] java.sql.ResultSetMetaData/columnNullableUnknown) ;; int; Indicates the nullability of values in the designated column.
  (isReadOnly [t c] true) ;; boolean; Indicates whether the designated column is definitely not writable.
  (isSearchable [t c] false) ;; boolean; Indicates whether the designated column can be used in a where clause.
  (isSigned [t c] true) ;; boolean; Indicates whether values in the designated column are signed numbers.
  (isWritable [t c] false)) ;; boolean; Indicates whether it is possible for a write on the designated column to succeed.
  
(defn rows-delta-type
  "Return a fn that, given an event returns a seq of rows according to
  the quantifier."
  [{r :rows, t :type, tbl :table}]
  (mapcat (condp = t
            'WRITE_ROWS_EVENT #(list (conj % "insert"))
            'DELETE_ROWS_EVENT #(list (conj % "delete"))
            'UPDATE_ROWS_EVENT #(list (conj (first %) "update-before")
                                      (conj (second %) "update"))
            ;; ignore non-data events
            nil)
          r))

(def _example-statement "select * from \"foo\".\"auto\" where _delta_type = '  insert'")

(defn tokenize-statement [s]
  (loop [[t & more :as s] (.split s " ")
         ast []]
    (cond (empty? s) ast
          (or (= t "\"") (= t "'"))
          (recur (drop-while #(not= % t) more)
                 (->> more
                      (take-while #(not= % t))
                      (map #(if (= "" %) " " %))
                      (apply str t)
                      (conj ast)))
          (= t "")
          (recur more ast)
          :else
          (recur more (conj ast t)))))

(defn parse-statement [s]
  (loop [[t & more :as s] (tokenize-statement s)
         ast {:select nil :from nil :where nil}]
    (cond (empty? s) ast
          (#{"select"} t)
          (recur (next more) (assoc ast :select (first more)))
          (#{"from"} t)
          (recur (drop-while #(not= % "where") more)
                 (assoc ast :from (apply str (take-while #(not= % "where") more))))
          (= t "where")
          (assoc ast :where (next s))
          :else
          (throwf-illegal "unknown statement: %s" t))))

;;(parse-statement _example-statement)

(defn really-lazy-concat
  "circumvent a bug in mapcat which prevents full lazyness."
  ([r]
     (lazy-seq (really-lazy-concat (first r) (rest r))))
  ([s r]
     (lazy-seq
      (if (empty? s)
        (if (empty? r)
          nil
          (really-lazy-concat r))
        (cons (first s)
              (really-lazy-concat (rest s) r))))))

(defn create-resultset-seq
  "given a simple sql expression and queue, return lazy seq of vectors
  from queue."
  [sql queue]
  (let [{table :from [_ _ dtype] :where} (parse-statement sql)
        dtype (when dtype (.replace dtype "'" ""))
        [table-name schema-name] (-> table ;; for now, don't allow dots in table names
                                     (.replace "\"" "")
                                     (.split "\\.")
                                     reverse)
        s (->> (repeatedly #(.take queue))
               (really-lazy-concat)
               (filter #(and (= (:table-name %) table-name)
                             (= (:db-name %) schema-name)))
               (map rows-delta-type)
               (really-lazy-concat)
               (filter (if dtype
                         #(= (last %) dtype)
                         identity))
               ;; in jdbc, one must call .next on the resultset to get the first row
               (cons nil))]
    s))

(defmacro getcol []
  `(if-let [v# (nth (first ~'rows) (dec ~'i))]
     (do (set! ~'was-null? false)
         v#)
     (do (set! ~'was-null? true)
         nil)))

(defmacro getcol-by-name []
  `(if-let [v# (nth (first ~'rows) (.findColumn ~'t ~'c))]
     (do (set! ~'was-null? false)
         v#)
     (do (set! ~'was-null? true)
         nil)))

;; generic resultset implementation
;; please, single threaded access only!
;; mind the gap, nothing coordinated
;; jdbc column numbering is 1<=colNum<=colCount!!!
(deftype ResultSet [#^{:volatile-mutable true} rows ;; a set of vectors of columnvalues, starting with a nil
                    #^{:volatile-mutable true} was-null?
                    sql]
  java.sql.ResultSet
  ;; essential methods
  (close [t] (dosync (set-new-queue)
                     (alter jdbc-state assoc :resultset nil)))
  (isClosed [t])
  (next [t]
        ;; moves cursor one row forward if there is a row
        (if-let [r (next rows)]
          (do (set! rows r)
              true)
          false))
  (wasNull [t] (boolean was-null?)) ;; whether the last access to any col was sql-null
  (getMetaData [t]
               ;; generate resultset metadata using the first row
               (ResultSetMetaData. "" "" (second rows))) ;; ResultSetMetaData
  (findColumn [t label]
              ;; returns the colnumber from the colname,
              ;; colnames are currently only printed numbers: "1", "2" ...
              (Integer/valueOf label))
  ;; data getters
  (^String getString [t ^int i] (getcol))
  (^String getString [t ^String c] (getcol-by-name))
  (^boolean getBoolean [t ^int i])
  (^boolean getBoolean [t ^String c])
  (^byte getByte [t ^int i] (getcol))
  (^byte getByte [t ^String c] (getcol-by-name))
  (^short getShort [t ^int i] (getcol))
  (^short getShort [t ^String c] (getcol-by-name))
  (^int getInt [t ^int i] (getcol))
  (^int getInt [t ^String c] (getcol-by-name))
  (^long getLong [t ^int i] (getcol))
  (^long getLong [t ^String c] (getcol-by-name))
  (^float getFloat [t ^int i] (getcol))
  (^float getFloat [t ^String c] (getcol-by-name))
  (^double getDouble [t ^int i] (getcol))
  (^double getDouble [t ^String c] (getcol-by-name))
  (^BigDecimal getBigDecimal [t ^int i] (java.math.BigDecimal. (str (getcol))))
  (^BigDecimal getBigDecimal [t ^String c] (java.math.BigDecimal. (str (getcol-by-name))))
  (^BigDecimal getBigDecimal [t ^int i ^int scale] (.getBigDecimal t i))
  (^BigDecimal getBigDecimal [t ^String c ^int scale] (.getBigDecimal t c))
  (^bytes getBytes [t ^int i])
  (^bytes getBytes [t ^String c])
  (^java.sql.Date getDate [t ^int i])
  (^java.sql.Date getDate [t ^String c])
  (^java.sql.Date getDate [t ^int i ^java.util.Calendar cal])
  (^java.sql.Date getDate [t ^String c ^java.util.Calendar cal])
  (^java.sql.Time getTime [t ^int i])
  (^java.sql.Time getTime [t ^String c])
  (^java.sql.Time getTime [t ^int i ^java.util.Calendar cal])
  (^java.sql.Time getTime [t ^String c ^java.util.Calendar cal])
  (^java.sql.Timestamp getTimestamp [t ^int i])
  (^java.sql.Timestamp getTimestamp [t ^String c])
  (^java.sql.Timestamp getTimestamp [t ^int i ^java.util.Calendar cal])
  (^java.sql.Timestamp getTimestamp [t ^String c ^java.util.Calendar cal])
  (^java.io.InputStream getAsciiStream [t ^int i])
  (^java.io.InputStream getAsciiStream [t ^String c])
  (^java.io.InputStream getUnicodeStream [t ^int i])
  (^java.io.InputStream getUnicodeStream [t ^String c])
  (^java.io.InputStream getBinaryStream [t ^int i])
  (^java.io.InputStream getBinaryStream [t ^String c])
  (getObject [t ^int i] (getcol))
  (getObject [t ^String c] (getcol-by-name))
  (getObject [t ^int i ^java.util.Map m]) ;; get object with mapping
  (getObject [t ^String c ^java.util.Map m])
  (^java.io.Reader getCharacterStream [t ^int i])
  (^java.io.Reader getCharacterStream [t ^String c])
  (^java.sql.Ref getRef [t ^int i])
  (^java.sql.Ref getRef [t ^String c])
  (^java.sql.Blob getBlob [t ^int i])
  (^java.sql.Blob getBlob [t ^String c])
  (^java.sql.Clob getClob [t ^int i])
  (^java.sql.Clob getClob [t ^String c])
  (^java.sql.Array getArray [t ^int i])
  (^java.sql.Array getArray [t ^String c])
  (^java.net.URL getURL [t ^int i])
  (^java.net.URL getURL [t ^String c])
  (^java.sql.NClob getNClob [t ^int i])
  (^java.sql.NClob getNClob [t ^String c])
  (^java.sql.SQLXML getSQLXML [t ^int i])
  (^java.sql.SQLXML getSQLXML [t ^String c])
  (^String getNString [t ^int i])
  (^String getNString [t ^String c])
  (^java.io.Reader getNCharacterStream [t ^int i])
  (^java.io.Reader getNCharacterStream [t ^String c])
  
  ;; unsupported cursor methods
  (getCursorName [t] (throwf-sql-not-supported))  
  (absolute [t r] (throwf-sql-not-supported))
  (afterLast [t] (throwf-sql-not-supported))
  (beforeFirst [t] (throwf-sql-not-supported))
  (relative [t r] (throwf-sql-not-supported))
  (isBeforeFirst [t] (throwf-sql-not-supported))
  (isAfterLast [t] (throwf-sql-not-supported))
  (isFirst [t] (throwf-sql-not-supported))
  (isLast [t] (throwf-sql-not-supported))
  (first [t] (throwf-sql-not-supported))
  (last [t] (throwf-sql-not-supported))
  (previous [t] (throwf-sql-not-supported))
  (setFetchDirection [t dir] (throwf-sql-not-supported))
  (getFetchDirection [t] 0)
  (moveToInsertRow [t] (throwf-sql-not-supported))
  (moveToCurrentRow [t] (throwf-sql-not-supported))
  
  ;; misc
  (setFetchSize [t s])
  (getFetchSize [t] 0)
  (getType [t] java.sql.ResultSet/TYPE_FORWARD_ONLY)
  (getConcurrency [t] java.sql.ResultSet/CONCUR_READ_ONLY)
  (getRow [t] (throwf-sql-not-supported))
  (getWarnings [t] )
  (clearWarnings [t])
  (^java.sql.RowId getRowId [t ^int i] (throwf-sql-not-supported))
  (^java.sql.RowId getRowId [t ^String c] (throwf-sql-not-supported))
  (getHoldability [t] java.sql.ResultSet/HOLD_CURSORS_OVER_COMMIT)
  
  ;; update functions
  (rowUpdated [t] (throwf-sql-not-supported))
  (rowDeleted [t] (throwf-sql-not-supported))
  (rowInserted [t] (throwf-sql-not-supported))

  (updateRow [t] (throwf-sql-not-supported))
  (deleteRow [t] (throwf-sql-not-supported))
  (insertRow [t] (throwf-sql-not-supported))
  (cancelRowUpdates [t] (throwf-sql-not-supported)))

(defn create-resultset [sql]
  (ResultSet. (create-resultset-seq sql (:queue @jdbc-state)) false sql))

(defn create-resultset-from-seq
  "Return a resultset implementation from the given seq of vectors.
  a string may be supplied for debug purposes which is stored in the
  resultset sql field."
  [s & [sql-string]]
  (ResultSet. (cons nil s) false sql-string))

(deftype Statement []
  java.sql.Statement
  ;; essential methods
  (close [t] (dosync (when-let [rs (:resultset @jdbc-state)] (.close rs))
                     (alter jdbc-state assoc :statement nil)))
  (executeQuery [t sql]
                (dosync (if-let [rs (:resultset @jdbc-state)]
                          (throwf "Close Resultset %s first." rs)
                          (let [rs (create-resultset sql)]
                            (alter jdbc-state assoc :resultset rs)
                            rs))))
  (execute [t sql] (.executeQuery t sql) true)
  ;; we do not have autogenerated keys
  (^boolean execute [t ^String sql ^int _] (.execute t sql))
  (^boolean execute [t ^String sql ^ints _] (boolean (.execute t sql)))
  (^boolean execute [t ^String sql ^"[Ljava.lang.String;" _] (boolean (.execute t sql)))

  (getResultSet [t] (:resultset @jdbc-state))
  
  ;; misc, mostly unsupported or ignored
  (addBatch [t s] (throwf-unsupported))
  (executeBatch [t] (throwf-unsupported))
  (cancel [t] (.close t))
  (clearBatch [t] (throwf-unsupported))
  (clearWarnings [t] (throwf-unsupported))
  (executeUpdate [t sql] (throwf-unsupported))
  (^int executeUpdate [t ^String sql ^int _] (throwf-unsupported))
  (^int executeUpdate [t ^String sql ^ints _] (throwf-unsupported))
  (^int executeUpdate [t ^String sql ^"[Ljava.lang.String;" _] (throwf-unsupported))
  
  (getConnection [t] (:connection @jdbc-state))
  (getFetchDirection [t] java.sql.ResultSet/FETCH_UNKNOWN)
  (getFetchSize [t] 1)
  (getGeneratedKeys [t] (throwf-sql-not-supported))
  (getMaxFieldSize [t] 0)     ;; unlimited
  (getMoreResults [t] false)  ;; only ever one rs
  (getMoreResults [t _] false)
  (getQueryTimeout [t] 0) ;; no timeout
  (getResultSetConcurrency [t] java.sql.ResultSet/CONCUR_READ_ONLY)
  (getResultSetHoldability [t] java.sql.ResultSet/HOLD_CURSORS_OVER_COMMIT)
  (getResultSetType [t] java.sql.ResultSet/TYPE_FORWARD_ONLY)
  (getUpdateCount [t] -1)
  (getWarnings [t] nil)
  (isClosed [t] false)
  (isPoolable [t] false)
  (setCursorName [t s] (throwf-sql-not-supported))
  (setEscapeProcessing [t flag]) ;; ignore
  (setFetchDirection [t dir])    ;; ignore
  (setFetchSize [t n])           ;; ignore
  (setMaxFieldSize [t s]) ;; ignore, may be useful to obey to for varchars
  (setMaxRows [t s])      ;; ignore
  (setPoolable [t flag])  ;; ignore
  (setQueryTimeout [t seconds]))

(deftype ConnectionMetadata []
  java.sql.DatabaseMetaData
  (^boolean allProceduresAreCallable [_] false) ;; Retrieves whether the current user can call all the procedures returned by the method getProcedures.
  (^boolean allTablesAreSelectable [_] false) ;; Retrieves whether the current user can use all the tables returned by the method getTables in a SELECT statement.
  (^boolean autoCommitFailureClosesAllResultSets [_] false) ;; Retrieves whether a SQLException while autoCommit is true inidcates that all open ResultSets are closed, even ones that are holdable.
  (^boolean dataDefinitionCausesTransactionCommit [_] false) ;; Retrieves whether a data definition statement within a transaction forces the transaction to commit.
  (^boolean dataDefinitionIgnoredInTransactions [_] true) ;; Retrieves whether this database ignores a data definition statement within a transaction.
  (^boolean deletesAreDetected [_ ^int type] false) ;; Retrieves whether or not a visible row delete can be detected by calling the method ResultSet.rowDeleted.
  (^boolean doesMaxRowSizeIncludeBlobs [_] false) ;; Retrieves whether the return value for the method getMaxRowSize includes the SQL data types LONGVARCHAR and LONGVARBINARY.
  (^java.sql.ResultSet getAttributes [_ ^String catalog, ^String schemaPattern, ^String typeNamePattern, ^String attributeNamePattern] (create-resultset-from-seq [])) ;; Retrieves a description of the given attribute of the given type for a userdefined type (UDT) that is available in the given schema and catalog.
  (^java.sql.ResultSet getBestRowIdentifier [_ ^String catalog, ^String schema, ^String table, ^int scope, ^boolean nullable] (create-resultset-from-seq [])) ;; Retrieves a description of a table's optimal set of columns that uniquely identifies a row.
  (^java.sql.ResultSet getCatalogs [_] (create-resultset-from-seq [])) ;; Retrieves the catalog names available in this database.
  (^String getCatalogSeparator [_] ".") ;; Retrieves the String that this database uses as the separator between a catalog and table name.
  (^String getCatalogTerm [_] "") ;; Retrieves the database vendor's preferred term for "catalog".
  (^java.sql.ResultSet getClientInfoProperties [_] (create-resultset-from-seq [])) ;; Retrieves a list of the client info properties that the driver supports.
  (^java.sql.ResultSet getColumnPrivileges [_ ^String catalog, ^String schema, ^String table, ^String columnNamePattern] (create-resultset-from-seq [])) ;; Retrieves a description of the access rights for a table's columns.
  (^java.sql.ResultSet getColumns [_ ^String catalog, ^String schemaPattern, ^String tableNamePattern, ^String columnNamePattern] (create-resultset-from-seq [])) ;; Retrieves a description of table columns available in the specified catalog.
  (^java.sql.Connection getConnection [_] (:connection @jdbc-state)) ;; Retrieves the connection that produced this metadata object.
  (^java.sql.ResultSet getCrossReference [_ ^String parentCatalog, ^String parentSchema, ^String parentTable, ^String foreignCatalog, ^String foreignSchema, ^String foreignTable] (create-resultset-from-seq [])) ;; Retrieves a description of the foreign key columns in the given foreign key table that reference the primary key or the columns representing a unique constraint of the parent table (could be the same or a different table).
  (^int getDatabaseMajorVersion [_] -1) ;; Retrieves the major version number of the underlying database.
  (^int getDatabaseMinorVersion [_] -1) ;; Retrieves the minor version number of the underlying database.
  (^String getDatabaseProductName [_] "mysql") ;; Retrieves the name of this database product.
  (^String getDatabaseProductVersion [_] "0.1") ;; Retrieves the version number of this database product.
  (^int getDefaultTransactionIsolation [_] java.sql.Connection/TRANSACTION_NONE) ;; Retrieves this database's default transaction isolation level.
  (^int getDriverMajorVersion [_] 0) ;; Retrieves this JDBC driver's major version number.
  (^int getDriverMinorVersion [_] 1) ;; Retrieves this JDBC driver's minor version number.
  (^String getDriverName [_] "mysql binlog cdc jdbc interface") ;; Retrieves the name of this JDBC driver.
  (^String getDriverVersion [_] "0.1") ;; Retrieves the version number of this JDBC driver as a String.
  (^java.sql.ResultSet getExportedKeys [_ ^String catalog, ^String schema, ^String table] (create-resultset-from-seq [])) ;; Retrieves a description of the foreign key columns that reference the given table's primary key columns (the foreign keys exported by a table).
  (^String getExtraNameCharacters [_] "") ;; Retrieves all the "extra" characters that can be used in unquoted identifier names (those beyond az, AZ, 09 and _).
  (^java.sql.ResultSet getFunctionColumns [_ ^String catalog, ^String schemaPattern, ^String functionNamePattern, ^String columnNamePattern] (create-resultset-from-seq [])) ;; Retrieves a description of the given catalog's system or user function parameters and return type.
  (^java.sql.ResultSet getFunctions [_ ^String catalog, ^String schemaPattern, ^String functionNamePattern] (create-resultset-from-seq [])) ;; Retrieves a description of the system and user functions available in the given catalog.
  (^String getIdentifierQuoteString [_] "\"") ;; Retrieves the string used to quote SQL identifiers.
  (^java.sql.ResultSet getImportedKeys [_ ^String catalog, ^String schema, ^String table] (create-resultset-from-seq [])) ;; Retrieves a description of the primary key columns that are referenced by the given table's foreign key columns (the primary keys imported by a table).
  (^java.sql.ResultSet getIndexInfo [_ ^String catalog, ^String schema, ^String table, ^boolean unique, ^boolean approximate] (create-resultset-from-seq [])) ;; Retrieves a description of the given table's indices and statistics.
  (^int getJDBCMajorVersion [_] -1) ;; Retrieves the major JDBC version number for this driver.
  (^int getJDBCMinorVersion [_] -1) ;; Retrieves the minor JDBC version number for this driver.
  (^int getMaxBinaryLiteralLength [_] -1) ;; Retrieves the maximum number of hex characters this database allows in an inline binary literal.
  (^int getMaxCatalogNameLength [_] -1) ;; Retrieves the maximum number of characters that this database allows in a catalog name.
  (^int getMaxCharLiteralLength [_] -1) ;; Retrieves the maximum number of characters this database allows for a character literal.
  (^int getMaxColumnNameLength [_] -1) ;; Retrieves the maximum number of characters this database allows for a column name.
  (^int getMaxColumnsInGroupBy [_] -1) ;; Retrieves the maximum number of columns this database allows in a GROUP BY clause.
  (^int getMaxColumnsInIndex [_] -1) ;; Retrieves the maximum number of columns this database allows in an index.
  (^int getMaxColumnsInOrderBy [_] -1) ;; Retrieves the maximum number of columns this database allows in an ORDER BY clause.
  (^int getMaxColumnsInSelect [_] -1) ;; Retrieves the maximum number of columns this database allows in a SELECT list.
  (^int getMaxColumnsInTable [_] -1) ;; Retrieves the maximum number of columns this database allows in a table.
  (^int getMaxConnections [_] -1) ;; Retrieves the maximum number of concurrent connections to this database that are possible.
  (^int getMaxCursorNameLength [_] -1) ;; Retrieves the maximum number of characters that this database allows in a cursor name.
  (^int getMaxIndexLength [_] -1) ;; Retrieves the maximum number of bytes this database allows for an index, including all of the parts of the index.
  (^int getMaxProcedureNameLength [_] -1) ;; Retrieves the maximum number of characters that this database allows in a procedure name.
  (^int getMaxRowSize [_] -1) ;; Retrieves the maximum number of bytes this database allows in a single row.
  (^int getMaxSchemaNameLength [_] -1) ;; Retrieves the maximum number of characters that this database allows in a schema name.
  (^int getMaxStatementLength [_] -1) ;; Retrieves the maximum number of characters this database allows in an SQL statement.
  (^int getMaxStatements [_] -1) ;; Retrieves the maximum number of active statements to this database that can be open at the same time.
  (^int getMaxTableNameLength [_] -1) ;; Retrieves the maximum number of characters this database allows in a table name.
  (^int getMaxTablesInSelect [_] -1) ;; Retrieves the maximum number of tables this database allows in a SELECT statement.
  (^int getMaxUserNameLength [_] -1) ;; Retrieves the maximum number of characters this database allows in a user name.
  (^String getNumericFunctions [_] "") ;; Retrieves a commaseparated list of math functions available with this database.
  (^java.sql.ResultSet getPrimaryKeys [_ ^String catalog, ^String schema, ^String table] (create-resultset-from-seq [])) ;; Retrieves a description of the given table's primary key columns.
  (^java.sql.ResultSet getProcedureColumns [_ ^String catalog, ^String schemaPattern, ^String procedureNamePattern, ^String columnNamePattern] (create-resultset-from-seq [])) ;; Retrieves a description of the given catalog's stored procedure parameter and result columns.
  (^java.sql.ResultSet getProcedures [_ ^String catalog, ^String schemaPattern, ^String procedureNamePattern] (create-resultset-from-seq [])) ;; Retrieves a description of the stored procedures available in the given catalog.
  (^String getProcedureTerm [_] "procedure") ;; Retrieves the database vendor's preferred term for "procedure".
  (^int getResultSetHoldability [_] java.sql.ResultSet/HOLD_CURSORS_OVER_COMMIT) ;; Retrieves this database's default holdability for ResultSet objects.
  (^java.sql.RowIdLifetime getRowIdLifetime [_] java.sql.RowIdLifetime/ROWID_UNSUPPORTED) ;; Indicates whether or not this data source supports the SQL ROWID type, and if so the lifetime for which a RowId object remains valid.
  (^java.sql.ResultSet getSchemas [_] (create-resultset-from-seq [])) ;; Retrieves the schema names available in this database.
  (^java.sql.ResultSet getSchemas [_ ^String catalog, ^String schemaPattern] (create-resultset-from-seq [])) ;; Retrieves the schema names available in this database.
  (^String getSchemaTerm [_] "schema") ;; Retrieves the database vendor's preferred term for "schema".
  (^String getSearchStringEscape [_] "") ;; Retrieves the string that can be used to escape wildcard characters.
  (^String getSQLKeywords [_] "") ;; Retrieves a commaseparated list of all of this database's SQL keywords that are NOT also SQL:2003 keywords.
  (^int getSQLStateType [_] -1) ;; Indicates whether the SQLSTATE returned by SQLException.getSQLState is X/Open (now known as Open Group) SQL CLI or SQL:2003.
  (^String getStringFunctions [_] "") ;; Retrieves a commaseparated list of string functions available with this database.
  (^java.sql.ResultSet getSuperTables [_ ^String catalog, ^String schemaPattern, ^String tableNamePattern] (create-resultset-from-seq [])) ;; Retrieves a description of the table hierarchies defined in a particular schema in this database.
  (^java.sql.ResultSet getSuperTypes [_ ^String catalog, ^String schemaPattern, ^String typeNamePattern] (create-resultset-from-seq [])) ;; Retrieves a description of the userdefined type (UDT) hierarchies defined in a particular schema in this database.
  (^String getSystemFunctions [_] "") ;; Retrieves a commaseparated list of system functions available with this database.
  (^java.sql.ResultSet getTablePrivileges [_ ^String catalog, ^String schemaPattern, ^String tableNamePattern] (create-resultset-from-seq [])) ;; Retrieves a description of the access rights for each table available in a catalog.
  (^java.sql.ResultSet getTables [_ ^String catalog, ^String schemaPattern, ^String tableNamePattern, ^"[Ljava.lang.String;" types] (create-resultset-from-seq [])) ;;String[] ;; Retrieves a description of the tables available in the given catalog.
  (^java.sql.ResultSet getTableTypes [_] (create-resultset-from-seq [])) ;; Retrieves the table types available in this database.
  (^String getTimeDateFunctions [_] "") ;; Retrieves a commaseparated list of the time and date functions available with this database.
  (^java.sql.ResultSet getTypeInfo [_] (create-resultset-from-seq [])) ;; Retrieves a description of all the data types supported by this database.
  (^java.sql.ResultSet getUDTs [_ ^String catalog, ^String schemaPattern, ^String typeNamePattern, ^ints types] (create-resultset-from-seq [])) ;; Retrieves a description of the userdefined types (UDTs) defined in a particular schema.
  (^String getURL [_] "") ;; Retrieves the URL for this DBMS.
  (^String getUserName [_] "") ;; Retrieves the user name as known to this database.
  (^java.sql.ResultSet getVersionColumns [_ ^String catalog, ^String schema, ^String table] (create-resultset-from-seq [])) ;; Retrieves a description of a table's columns that are automatically updated when any value in a row is updated.
  (^boolean insertsAreDetected [_ ^int type] false) ;; Retrieves whether or not a visible row insert can be detected by calling the method ResultSet.rowInserted.
  (^boolean isCatalogAtStart [_] false) ;; Retrieves whether a catalog appears at the start of a fully qualified table name.
  (^boolean isReadOnly [_] true) ;; Retrieves whether this database is in readonly mode.
  (^boolean locatorsUpdateCopy [_] false) ;; Indicates whether updates made to a LOB are made on a copy or directly to the LOB.
  (^boolean nullPlusNonNullIsNull [_] false) ;; Retrieves whether this database supports concatenations between NULL and nonNULL values being NULL.
  (^boolean nullsAreSortedAtEnd [_] false) ;; Retrieves whether NULL values are sorted at the end regardless of sort order.
  (^boolean nullsAreSortedAtStart [_] false) ;; Retrieves whether NULL values are sorted at the start regardless of sort order.
  (^boolean nullsAreSortedHigh [_] false) ;; Retrieves whether NULL values are sorted high.
  (^boolean nullsAreSortedLow [_] false) ;; Retrieves whether NULL values are sorted low.
  (^boolean othersDeletesAreVisible [_ ^int type] false) ;; Retrieves whether deletes made by others are visible.
  (^boolean othersInsertsAreVisible [_ ^int type] false) ;; Retrieves whether inserts made by others are visible.
  (^boolean othersUpdatesAreVisible [_ ^int type] false) ;; Retrieves whether updates made by others are visible.
  (^boolean ownDeletesAreVisible [_ ^int type] false) ;; Retrieves whether a result set's own deletes are visible.
  (^boolean ownInsertsAreVisible [_ ^int type] false) ;; Retrieves whether a result set's own inserts are visible.
  (^boolean ownUpdatesAreVisible [_ ^int type] false) ;; Retrieves whether for the given type of ResultSet object, the result set's own updates are visible.
  (^boolean storesLowerCaseIdentifiers [_] false) ;; Retrieves whether this database treats mixed case unquoted SQL identifiers as case insensitive and stores them in lower case.
  (^boolean storesLowerCaseQuotedIdentifiers [_] false) ;; Retrieves whether this database treats mixed case quoted SQL identifiers as case insensitive and stores them in lower case.
  (^boolean storesMixedCaseIdentifiers [_] false) ;; Retrieves whether this database treats mixed case unquoted SQL identifiers as case insensitive and stores them in mixed case.
  (^boolean storesMixedCaseQuotedIdentifiers [_] false) ;; Retrieves whether this database treats mixed case quoted SQL identifiers as case insensitive and stores them in mixed case.
  (^boolean storesUpperCaseIdentifiers [_] false) ;; Retrieves whether this database treats mixed case unquoted SQL identifiers as case insensitive and stores them in upper case.
  (^boolean storesUpperCaseQuotedIdentifiers [_] false) ;; Retrieves whether this database treats mixed case quoted SQL identifiers as case insensitive and stores them in upper case.
  (^boolean supportsAlterTableWithAddColumn [_] false) ;; Retrieves whether this database supports ALTER TABLE with add column.
  (^boolean supportsAlterTableWithDropColumn [_] false) ;; Retrieves whether this database supports ALTER TABLE with drop column.
  (^boolean supportsANSI92EntryLevelSQL [_] false) ;; Retrieves whether this database supports the ANSI92 entry level SQL grammar.
  (^boolean supportsANSI92FullSQL [_] false) ;; Retrieves whether this database supports the ANSI92 full SQL grammar supported.
  (^boolean supportsANSI92IntermediateSQL [_] false) ;; Retrieves whether this database supports the ANSI92 intermediate SQL grammar supported.
  (^boolean supportsBatchUpdates [_] false) ;; Retrieves whether this database supports batch updates.
  (^boolean supportsCatalogsInDataManipulation [_] false) ;; Retrieves whether a catalog name can be used in a data manipulation statement.
  (^boolean supportsCatalogsInIndexDefinitions [_] false) ;; Retrieves whether a catalog name can be used in an index definition statement.
  (^boolean supportsCatalogsInPrivilegeDefinitions [_] false) ;; Retrieves whether a catalog name can be used in a privilege definition statement.
  (^boolean supportsCatalogsInProcedureCalls [_] false) ;; Retrieves whether a catalog name can be used in a procedure call statement.
  (^boolean supportsCatalogsInTableDefinitions [_] false) ;; Retrieves whether a catalog name can be used in a table definition statement.
  (^boolean supportsColumnAliasing [_] false) ;; Retrieves whether this database supports column aliasing.
  (^boolean supportsConvert [_] false) ;; Retrieves whether this database supports the JDBC scalar function CONVERT for the conversion of one JDBC type to another.
  (^boolean supportsConvert [_ ^int fromType, ^int toType] false) ;; Retrieves whether this database supports the JDBC scalar function CONVERT for conversions between the JDBC types fromType and toType.
  (^boolean supportsCoreSQLGrammar [_] false) ;; Retrieves whether this database supports the ODBC Core SQL grammar.
  (^boolean supportsCorrelatedSubqueries [_] false) ;; Retrieves whether this database supports correlated subqueries.
  (^boolean supportsDataDefinitionAndDataManipulationTransactions [_] false) ;; Retrieves whether this database supports both data definition and data manipulation statements within a transaction.
  (^boolean supportsDataManipulationTransactionsOnly [_] false) ;; Retrieves whether this database supports only data manipulation statements within a transaction.
  (^boolean supportsDifferentTableCorrelationNames [_] false) ;; Retrieves whether, when table correlation names are supported, they are restricted to being different from the names of the tables.
  (^boolean supportsExpressionsInOrderBy [_] false) ;; Retrieves whether this database supports expressions in ORDER BY lists.
  (^boolean supportsExtendedSQLGrammar [_] false) ;; Retrieves whether this database supports the ODBC Extended SQL grammar.
  (^boolean supportsFullOuterJoins [_] false) ;; Retrieves whether this database supports full nested outer joins.
  (^boolean supportsGetGeneratedKeys [_] false) ;; Retrieves whether autogenerated keys can be retrieved after a statement has been executed
  (^boolean supportsGroupBy [_] false) ;; Retrieves whether this database supports some form of GROUP BY clause.
  (^boolean supportsGroupByBeyondSelect [_] false) ;; Retrieves whether this database supports using columns not included in the SELECT statement in a GROUP BY clause provided that all of the columns in the SELECT statement are included in the GROUP BY clause.
  (^boolean supportsGroupByUnrelated [_] false) ;; Retrieves whether this database supports using a column that is not in the SELECT statement in a GROUP BY clause.
  (^boolean supportsIntegrityEnhancementFacility [_] false) ;; Retrieves whether this database supports the SQL Integrity Enhancement Facility.
  (^boolean supportsLikeEscapeClause [_] false) ;; Retrieves whether this database supports specifying a LIKE escape clause.
  (^boolean supportsLimitedOuterJoins [_] false) ;; Retrieves whether this database provides limited support for outer joins.
  (^boolean supportsMinimumSQLGrammar [_] false) ;; Retrieves whether this database supports the ODBC Minimum SQL grammar.
  (^boolean supportsMixedCaseIdentifiers [_] false) ;; Retrieves whether this database treats mixed case unquoted SQL identifiers as case sensitive and as a result stores them in mixed case.
  (^boolean supportsMixedCaseQuotedIdentifiers [_] false) ;; Retrieves whether this database treats mixed case quoted SQL identifiers as case sensitive and as a result stores them in mixed case.
  (^boolean supportsMultipleOpenResults [_] false) ;; Retrieves whether it is possible to have multiple ResultSet objects returned from a CallableStatement object simultaneously.
  (^boolean supportsMultipleResultSets [_] false) ;; Retrieves whether this database supports getting multiple ResultSet objects from a single call to the method execute.
  (^boolean supportsMultipleTransactions [_] false) ;; Retrieves whether this database allows having multiple transactions open at once (on different connections).
  (^boolean supportsNamedParameters [_] false) ;; Retrieves whether this database supports named parameters to callable statements.
  (^boolean supportsNonNullableColumns [_] false) ;; Retrieves whether columns in this database may be defined as nonnullable.
  (^boolean supportsOpenCursorsAcrossCommit [_] false) ;; Retrieves whether this database supports keeping cursors open across commits.
  (^boolean supportsOpenCursorsAcrossRollback [_] false) ;; Retrieves whether this database supports keeping cursors open across rollbacks.
  (^boolean supportsOpenStatementsAcrossCommit [_] false) ;; Retrieves whether this database supports keeping statements open across commits.
  (^boolean supportsOpenStatementsAcrossRollback [_] false) ;; Retrieves whether this database supports keeping statements open across rollbacks.
  (^boolean supportsOrderByUnrelated [_] false) ;; Retrieves whether this database supports using a column that is not in the SELECT statement in an ORDER BY clause.
  (^boolean supportsOuterJoins [_] false) ;; Retrieves whether this database supports some form of outer join.
  (^boolean supportsPositionedDelete [_] false) ;; Retrieves whether this database supports positioned DELETE statements.
  (^boolean supportsPositionedUpdate [_] false) ;; Retrieves whether this database supports positioned UPDATE statements.
  (^boolean supportsResultSetConcurrency [_ ^int type, ^int concurrency] false) ;; Retrieves whether this database supports the given concurrency type in combination with the given result set type.
  (^boolean supportsResultSetHoldability [_ ^int holdability] false) ;; Retrieves whether this database supports the given result set holdability.
  (^boolean supportsResultSetType [_ ^int type] false) ;; Retrieves whether this database supports the given result set type.
  (^boolean supportsSavepoints [_] false) ;; Retrieves whether this database supports savepoints.
  (^boolean supportsSchemasInDataManipulation [_] false) ;; Retrieves whether a schema name can be used in a data manipulation statement.
  (^boolean supportsSchemasInIndexDefinitions [_] false) ;; Retrieves whether a schema name can be used in an index definition statement.
  (^boolean supportsSchemasInPrivilegeDefinitions [_] false) ;; Retrieves whether a schema name can be used in a privilege definition statement.
  (^boolean supportsSchemasInProcedureCalls [_] false) ;; Retrieves whether a schema name can be used in a procedure call statement.
  (^boolean supportsSchemasInTableDefinitions [_] false) ;; Retrieves whether a schema name can be used in a table definition statement.
  (^boolean supportsSelectForUpdate [_] false) ;; Retrieves whether this database supports SELECT FOR UPDATE statements.
  (^boolean supportsStatementPooling [_] false) ;; Retrieves whether this database supports statement pooling.
  (^boolean supportsStoredFunctionsUsingCallSyntax [_] false) ;; Retrieves whether this database supports invoking userdefined or vendor functions using the stored procedure escape syntax.
  (^boolean supportsStoredProcedures [_] false) ;; Retrieves whether this database supports stored procedure calls that use the stored procedure escape syntax.
  (^boolean supportsSubqueriesInComparisons [_] false) ;; Retrieves whether this database supports subqueries in comparison expressions.
  (^boolean supportsSubqueriesInExists [_] false) ;; Retrieves whether this database supports subqueries in EXISTS expressions.
  (^boolean supportsSubqueriesInIns [_] false) ;; Retrieves whether this database supports subqueries in IN expressions.
  (^boolean supportsSubqueriesInQuantifieds [_] false) ;; Retrieves whether this database supports subqueries in quantified expressions.
  (^boolean supportsTableCorrelationNames [_] false) ;; Retrieves whether this database supports table correlation names.
  (^boolean supportsTransactionIsolationLevel [_ ^int level] false) ;; Retrieves whether this database supports the given transaction isolation level.
  (^boolean supportsTransactions [_] false) ;; Retrieves whether this database supports transactions.
  (^boolean supportsUnion [_] false) ;; Retrieves whether this database supports SQL UNION.
  (^boolean supportsUnionAll [_] false) ;; Retrieves whether this database supports SQL UNION ALL.
  (^boolean updatesAreDetected [_ ^int type] false) ;; Retrieves whether or not a visible row update can be detected by calling the method ResultSet.rowUpdated.
  (^boolean usesLocalFilePerTable [_] false) ;; Retrieves whether this database uses a file for each table.
  (^boolean usesLocalFiles [_] true)) ;; Retrieves whether this database stores tables in a local file.

(deftype Connection [binlog-state]
  java.sql.Connection
  (close [_]
         ;; Releases this Connection object's database and JDBC
         ;; resources immediately instead of waiting for them to be
         ;; automatically released.
         (dosync (when-let [st (:statement @jdbc-state)] (.close st))
                 (alter jdbc-state assoc :closed true :connection nil)
                 (send-off (:binlog-state @jdbc-state) binlog/cdc-stop)))
  (createStatement [t]
                   (dosync (if (:statement @jdbc-state)
                             (throwf "Close statement %s first" (:statement @jdbc-state))
                             (let [s (Statement.)]
                               (alter jdbc-state assoc :statement s)
                               s))))
  (createStatement [t rs-type rs-cncrcy] (.createStatement t))
  (createStatement [t rs-type rs-cncrcy rs-holdabilityb] (.createStatement t))
  ;; (^java.sql.Statement createStatement [t]) ;; Creates a Statement object for sending SQL statements to the database.
  ;; (^java.sql.Statement createStatement [t ^int resultSetType, ^int resultSetConcurrency]) ;; Creates a Statement object that will generate ResultSet objects with the given type and concurrency.
  ;; (^java.sql.Statement createStatement [t ^int resultSetType, ^int resultSetConcurrency, ^int resultSetHoldability]) ;; Creates a Statement object that will generate ResultSet objects with the given type, concurrency, and holdability.

  (getMetaData [t] (ConnectionMetadata.))

  ;; unimplemented
  (^void clearWarnings [t]) ;; Clears all warnings reported for this Connection object.
  (^void commit [t]) ;; Makes all changes made since the previous commit/rollback permanent and releases any database locks currently held by this Connection object.
  (^java.sql.Array createArrayOf [t ^String typeName, ^"[Ljava.lang.Object;" elements]) ;; Factory method for creating Array objects.
  (^java.sql.Blob createBlob [t]) ;; Constructs an object that implements the Blob interface.
  (^java.sql.Clob createClob [t]) ;; Constructs an object that implements the Clob interface.
  (^java.sql.NClob createNClob [t]) ;; Constructs an object that implements the NClob interface.
  (^java.sql.SQLXML createSQLXML [t]) ;; Constructs an object that implements the SQLXML interface.
  (^java.sql.Struct createStruct [t ^String typeName, ^"[Ljava.lang.Object;" attributes]) ;; Factory method for creating Struct objects.
  (^boolean getAutoCommit [t] false) ;; Retrieves the current autocommit mode for this Connection object.
  (^String getCatalog [t] "") ;; Retrieves this Connection object's current catalog name.
  (^java.util.Properties getClientInfo [t] {}) ;; Returns a list containing the name and current value of each client info property supported by the driver.
  (^String getClientInfo [t ^String name] "") ;; Returns the value of the client info property specified by name.
  (^int getHoldability [t] 0) ;; Retrieves the current holdability of ResultSet objects created using this Connection object.
  (^int getTransactionIsolation [t] 0) ;; Retrieves this Connection object's current transaction isolation level.
  (^java.util.Map getTypeMap [t] {}) ;; Retrieves the Map object associated with this Connection object.
  (^java.sql.SQLWarning getWarnings [t]) ;; Retrieves the first warning reported by calls on this Connection object.
  (^boolean isClosed [t] (:closed @jdbc-state)) ;; Retrieves whether this Connection object has been closed.
  (^boolean isReadOnly [t] true) ;; Retrieves whether this Connection object is in readonly mode.
  (^boolean isValid [t ^int timeout] true) ;; Returns true if the connection has not been closed and is still valid.
  (^String nativeSQL [t ^String sql] sql) ;; Converts the given SQL statement into the system's native SQL grammar.
  (^java.sql.CallableStatement prepareCall [t ^String sql]) ;; Creates a CallableStatement object for calling database stored procedures.
  (^java.sql.CallableStatement prepareCall [t ^String sql, ^int resultSetType, ^int resultSetConcurrency]) ;; Creates a CallableStatement object that will generate ResultSet objects with the given type and concurrency.
  (^java.sql.CallableStatement prepareCall [t ^String sql, ^int resultSetType, ^int resultSetConcurrency, ^int resultSetHoldability]) ;; Creates a CallableStatement object that will generate ResultSet objects with the given type and concurrency.
  (^java.sql.PreparedStatement prepareStatement [t ^String sql]) ;; Creates a PreparedStatement object for sending parameterized SQL statements to the database.
  (^java.sql.PreparedStatement prepareStatement [t ^String sql, ^int autoGeneratedKeys]) ;; Creates a default PreparedStatement object that has the capability to retrieve autogenerated keys.
  (^java.sql.PreparedStatement prepareStatement [t ^String sql, ^ints columnIndexes]) ;; Creates a default PreparedStatement object capable of returning the autogenerated keys designated by the given array.
  (^java.sql.PreparedStatement prepareStatement [t ^String sql, ^int resultSetType, ^int resultSetConcurrency]) ;; Creates a PreparedStatement object that will generate ResultSet objects with the given type and concurrency.
  (^java.sql.PreparedStatement prepareStatement [t ^String sql, ^int resultSetType, ^int resultSetConcurrency, ^int resultSetHoldability]) ;; Creates a PreparedStatement object that will generate ResultSet objects with the given type, concurrency, and holdability.
  (^java.sql.PreparedStatement prepareStatement [t ^String sql, ^"[Ljava.lang.String;" columnNames]) ;; Creates a default PreparedStatement object capable of returning the autogenerated keys designated by the given array.
  (^void releaseSavepoint [t ^java.sql.Savepoint savepoint]) ;; Removes the specified Savepoint and subsequent Savepoint objects from the current transaction.
  (^void rollback [t]) ;; Undoes all changes made in the current transaction and releases any database locks currently held by this Connection object.
  (^void rollback [t ^java.sql.Savepoint savepoint]) ;; Undoes all changes made after the given Savepoint object was set.
  (^void setAutoCommit [t ^boolean autoCommit]) ;; Sets this connection's autocommit mode to the given state.
  (^void setCatalog [t ^String catalog]) ;; Sets the given catalog name in order to select a subspace of this Connection object's database in which to work.
  (^void setClientInfo [t ^Properties properties]) ;; Sets the value of the connection's client info properties.
  (^void setClientInfo [t ^String name, ^String value]) ;; Sets the value of the client info property specified by name to the value specified by value.
  (^void setHoldability [t ^int holdability]) ;; Changes the default holdability of ResultSet objects created using this Connection object to the given holdability.
  (^void setReadOnly [t ^boolean readOnly]) ;; Puts this connection in readonly mode as a hint to the driver to enable database optimizations.
  (^java.sql.Savepoint setSavepoint [t]) ;; Creates an unnamed savepoint in the current transaction and returns the new Savepoint object that represents it.
  (^java.sql.Savepoint setSavepoint [t ^String name]) ;; Creates a savepoint with the given name in the current transaction and returns the new Savepoint object that represents it.
  (^void setTransactionIsolation [t ^int level]) ;; Attempts to change the transaction isolation level for this Connection object to the one given.
  (^void setTypeMap [t ^java.util.Map map]) ;; Installs the given TypeMap object as the type map for this Connection object.

  java.sql.Wrapper
  (isWrapperFor [_ iface])
  (unwrap [_ iface]))

(defn create-connection [binlog-index-file user pwd]
  (let [state (binlog/cdc-init (fn [_]))
        ;; mysql-conn (sql/get-connection
        ;;             {:classname "com.mysql.jdbc.Driver"
        ;;              :subprotocol "mysql"
        ;;              :subname "//localhost"
        ;;              ;;:username user
        ;;              :user user
        ;;              :password pwd})
        conn (Connection. state)]
    (dosync (alter jdbc-state assoc
                   :statement nil
                   :resultset nil
                   :connection conn
                   :binlog-state state
                   :closed false))
    (set-new-queue)
    (send state binlog/cdc-start binlog-index-file)
    conn))

