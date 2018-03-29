/*
 * Stream.java
 *
 * Created on April 28, 2007, 4:21 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.wisc.VegaLibrary;

import java.sql.*;
import java.util.*;
/**
 *
 * @author lawinslow
 */
        public class Stream{
            private int id;
            private String selSql;
            
            private int siteID;
            private int variableID;
            private int methodID;
            private int sourceID;
            private int offsetTypeID;
            private double offsetValueID;
            private int aggMethodID;
            private int unitID;
            private String aggSpan;
            double utcOffset;
            int repID;
            
            
              
            
            private static ArrayList retrieved;
            
            public static synchronized Stream getStream(Site site,
                    Variable variable,int method,Source source,
                    OffsetType offsettype,OffsetValue offsetvalue,
                    AggMethod aggmethod,AggSpan aggspan,Unit unit,
                    int repID,Connection conn){
                
                Stream tmp;
                if(retrieved != null){
                    for(Iterator<Stream> it = retrieved.iterator(); it.hasNext();){
                        tmp = it.next();
                        if(VegaVersionInfo.dbVersion >1){
                            if(tmp.getSiteID() == site.getID() &&
                                tmp.getVariableID() == variable.getID() &&
                                tmp.getMethodID() == method &&
                                tmp.getSourceID() == source.getID() &&
                                tmp.getOffsetTypeID() == offsettype.getID() &&
                                tmp.getOffsetValueID() == offsetvalue.getValue() &&
                                tmp.getAggMethID() == aggmethod.getID() &&
                                tmp.getAggSpan().compareToIgnoreCase(aggspan.getSpan())==0 &&
                                tmp.getUnitID() == unit.getID() &&
                                tmp.getRepID() == repID){
                                return tmp;
                            }       
                        }else{
                            if(tmp.getSiteID() == site.getID() &&
                                tmp.getVariableID() == variable.getID() &&
                                tmp.getMethodID() == method &&
                                tmp.getSourceID() == source.getID() &&
                                tmp.getOffsetTypeID() == offsettype.getID() &&
                                tmp.getOffsetValueID() == offsetvalue.getValue() &&
                                tmp.getAggMethID() == aggmethod.getID() &&
                                tmp.getAggSpan().compareToIgnoreCase(aggspan.getSpan())==0 &&
                                tmp.getUnitID() == unit.getID()){
                                return tmp;
                            }                            
                        }
                            

                    }//for loop
                    //if we get here, none were matches, create a new one
                    tmp = new Stream(site,variable,method,source,offsettype,offsetvalue,aggmethod,aggspan,unit,repID,conn);
                    retrieved.add(tmp);
                    return tmp;
                }else{
                    retrieved = new ArrayList<Stream>();
                    tmp = new Stream(site,variable,method,source,offsettype,offsetvalue,aggmethod,aggspan,unit,repID,conn);
                    retrieved.add(tmp);
                    return tmp;
                }
            }//getStream
            
            
            public Stream(Site site,Variable variable,int method, 
                    Source source,OffsetType offsettype,OffsetValue offsetvalue,
                    AggMethod aggmethod,AggSpan aggspan,Unit unit,
                    int repID,Connection conn){
                
                siteID = site.getID();
                variableID = variable.getID();
                methodID = method;
                sourceID = source.getID();
                offsetTypeID = offsettype.getID();
                offsetValueID = offsetvalue.getValue();
                aggMethodID = aggmethod.getID();
                aggSpan = aggspan.getSpan();
                unitID = unit.getID();
                
                //Ignore these if dbVersion is 1
                this.repID = repID;
                
                
                String methodSql;
                if(method < 1){
                    methodSql = " is null ";
                }else{
                    methodSql = " = " + String.valueOf(method);
                }
                
                try{
                    String sql;
                    
                    if(VegaVersionInfo.dbVersion > 1){
                        sql = "SELECT StreamID FROM streams WHERE SecurityID = 1 AND "+ //need to add additional sec support
                                " SiteID "+site.getSqlWherePart()+ " AND VariableID "+variable.getSqlWherePart()+
                                " AND SensorID "+methodSql+" AND SourceID "+source.getSqlWherePart()+
                                " AND OffsetTypeID "+offsettype.getSqlWherePart()+" AND round(OffsetValue,5) "+ offsetvalue.getSqlWherePart() +
                                " AND AggMethodID "+aggmethod.getSqlWherePart()+ " AND AggSpan "+aggspan.getSqlWherePart() + 
                                " AND UnitID "+ unit.getSqlWherePart()+" AND RepID='"+repID+"'";
                    }else{
                        sql = "SELECT StreamID FROM streams WHERE SecurityID = 1 AND "+ //need to add additional sec support
                                " SiteID "+site.getSqlWherePart()+ " AND VariableID "+variable.getSqlWherePart()+
                                " AND MethodID "+methodSql+" AND SourceID "+source.getSqlWherePart()+
                                " AND OffsetType "+offsettype.getSqlWherePart()+" AND round(OffsetValue,5) "+ offsetvalue.getSqlWherePart() +
                                " AND AggMethodID "+aggmethod.getSqlWherePart()+ " AND AggSpan "+aggspan.getSqlWherePart() + 
                                " AND UnitID "+ unit.getSqlWherePart();
                    }

                    selSql = sql;
                    
                    Statement st = conn.createStatement();
                    ResultSet rs = st.executeQuery(sql);
                    
                    //if there is one available, the first call to next is True
                    //and then we can get our StreamID
                    if(rs.next()){
                        id = rs.getInt("StreamID");
                    }else{//otherwise we must insert
                        //insert code to insert stream
                        PreparedStatement insert;
                        
                        if(VegaVersionInfo.dbVersion >1){
                            insert = conn.prepareStatement(
                            "INSERT INTO streams(siteid,variableid,sensorid,sourceid,offsettypeid,offsetvalue,securityid,aggspan,aggmethodid,unitid,repid) "+
                                    "VALUES(?,?,?,?,?,?,1,?,?,?,?)");
                        }else{
                            insert = conn.prepareStatement(
                                "INSERT INTO streams(siteid,variableid,methodid,sourceid,offsettype,offsetvalue,securityid,aggspan,aggmethodid,unitid) "+
                                        "VALUES(?,?,?,?,?,?,1,?,?,?)");
                        }

                        insert.setInt(1,site.getID());
                        insert.setInt(2,variable.getID());
                        
                        insert.setInt(4,source.getID());
                        
                        if(method < 1){
                            insert.setNull(3,java.sql.Types.INTEGER);
                        }else{
                            insert.setInt(3,method);
                        }
                        
                        if(offsettype.getID() <= 0){
                            insert.setNull(5,java.sql.Types.INTEGER);
                        }else{
                            insert.setInt(5,offsettype.getID());
                        }
                        
                        if(offsetvalue.isNull()){
                            insert.setNull(6,java.sql.Types.DOUBLE);
                        }else{
                            insert.setDouble(6,offsetvalue.getValue());
                        }
                        insert.setString(7,aggspan.getSpan());
                        insert.setInt(8,aggmethod.getID());
                        insert.setInt(9,unit.getID());
                        
                        if(VegaVersionInfo.dbVersion >1){
                            insert.setInt(10,repID);
                        }

                        // insert data
                        insert.execute();
                        System.out.println("Value Inserted");
                        // re-execute select statement to get ID
                        rs = st.executeQuery(sql);
                        
                        if(rs.next()){
                            id = rs.getInt("StreamID");
                        }else{
                            System.out.println("Serious StreamID error");
                            id = -1;
                        } 
                        insert.close();
                    }//if(rs.next()) else{
                    
                    st.close();
                }catch(SQLException sx){
                    id = -1;
                    System.out.println(sx.getMessage());
                    System.out.println("Failure Getting id");
                   
                }//catch
            }// public Stream
            public int getID() throws Exception{
                if(id < 0){
                    throw new Exception("SEL used:"+selSql + "\n"+
                            "INSERT used: ");
                    
                }else{
                      return id;  
                }
            }

            public int getSiteID(){
                return siteID;
            }
            public int getVariableID(){
                return variableID;
            }
            public int getMethodID(){
                return methodID;
            }
            public int getSourceID(){
                return sourceID;
            }
            public int getOffsetTypeID(){
                return offsetTypeID;
            }
            public double getOffsetValueID(){
                return offsetValueID;
            }
            public int getAggMethID(){
                return aggMethodID;
            }
            public String getAggSpan(){
                return aggSpan;
            }
            public int getUnitID(){
                return unitID;
            }
            public double getUtcOffset(){
                return utcOffset;
            }
            public int getRepID(){
                return repID;
            }
                    
        }// class Stream
