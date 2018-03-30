' Create/update ALEKS tracking worksheets by teacher.
' This workbook lives in the same folder as the ALEKS repository database, aleks.sqlite.
' We read a list of teachers from the "teacher file", an Excel workbook in the same folder. (See TeacherFileName.)
'   Teacher file columns, with column names in row 1:
'     teacher name, matching the name used in ALEKS
'     teacher email address [not currently used]
'     path to folder where ALEKS tracking worksheet should be maintained
' To temporarily change the teacher file, you can place the standard rows below the
' temporary rows, with a blank row in between. We'll stop processing at the first blank row.
' When you open this workbook, the tracking worksheet maintenance routine runs automatically.
' It does ask you before creating or updating worksheets.

' Settings, constants, and global values:
Option Explicit
Option Base 1  'TAKE NOTE!
Const Title = "ALEKS Tracking Repository"  'for message boxes
Const TeacherFileName = "teachers.xlsx"
Const StrandSymbols = "NPGMD"  'in order they appear on worksheets
Public RepositoryFolder As String
Public FolderDelimiter As String  'folder delimiter \, /, :
Public schoolYear As Integer  'four-digit school (ending) year

Sub CreateWorksheets()
    ' This is our main routine.
    
    ' Variables for interacting with SQLite:
    Dim db As Long              'SQLite database connection handle
    Dim sql As String           'SQL string
    Dim stmt As Long            'SQLite prepared statement handle
    
    ' Variables for the teacher file and teacher information:
    Dim teacherBook As Workbook 'file of teachers for which to create ALEKS tracking sheets
    Dim teacherList As Range
    Dim nTeachers As Integer
    Dim teacher() As String     'list of teacher names
    Dim teacherEmail() As String
    Dim teacherPath() As String 'path to each teacher's ALEKS folder
    Dim teacherRowId() As Long  'corresponding rowid's from teacher table
    Dim unmatchedTeachers As String
    Dim teacherRowIdString As String  'for use in SQL strings
    Dim schoolYearPresent() As Integer  'list of school years found in repository
    Dim nSchoolYearsPresent As Integer
    
    ' Variables for ALEKS tracking worksheets:
    Dim fs As Object            'file system object to interact with file system
    Dim trackingBookPathName As String
    Dim trackingBook As Workbook
    Dim newTrackingBook As Boolean  'true when we're creating a new tracking worksheet for a teacher

    Dim i As Integer            'loop index and counter
    
    ' Set global values.
    RepositoryFolder = ThisWorkbook.Path
    FolderDelimiter = Mid$(ThisWorkbook.fullName, Len(ThisWorkbook.Path) + 1, 1)
    
    ' Open our ALEKS repository.
    db = AleksDb(RepositoryFolder & FolderDelimiter & "aleks.sqlite")
    If db = 0 Then Exit Sub  'error message already displayed

    ' Read the teacher file.
    Set teacherBook = Workbooks.Open(RepositoryFolder & FolderDelimiter & TeacherFileName)
    Set teacherList = teacherBook.Sheets(1).Range("A1").CurrentRegion
    nTeachers = teacherList.Rows.Count - 1  'but watch for blank teacher names
    ReDim Preserve teacher(nTeachers), teacherEmail(nTeachers), _
        teacherPath(nTeachers), teacherRowId(nTeachers)
    For i = 1 To nTeachers
        teacher(i) = teacherList.Cells(1 + i, 1).Value
        teacherEmail(i) = teacherList.Cells(1 + i, 2).Value
        teacherPath(i) = teacherList.Cells(1 + i, 3).Value
    Next i
    teacherBook.Close
    
    ' Get corresponding rowid's from the ALEKS repository.
    For i = 1 To nTeachers
        sql = "select rowid from teacher" _
            & " where name = """ & teacher(i) & """;"
        stmt = prepare(db, sql)
        If execStep(stmt) = SQLITE_ROW Then
            teacherRowId(i) = columnValue(stmt, 0)
        End If
    Next i
    finalize stmt
    
    ' Report unmatched teacher names.
    unmatchedTeachers = ""
    For i = 1 To nTeachers
        If teacherRowId(i) = 0 Then
            If unmatchedTeachers <> "" Then
                unmatchedTeachers = unmatchedTeachers + "; "
            End If
            unmatchedTeachers = unmatchedTeachers + teacher(i)
        End If
    Next i
    If unmatchedTeachers <> "" Then
        MsgBox "The following teacher names are not in the ALEKS repository: " _
            & unmatchedTeachers & "." & Chr$(13) & Chr$(10) & Chr$(13) & Chr$(10) & "Continue?", _
            vbYesNo + vbQuestion, Title
    End If
    
    ' Construct a string of teacher rowid's separated by commas, for use in SQL expressions.
    teacherRowIdString = ""
    For i = 1 To nTeachers
        If teacherRowId(i) <> 0 Then
            If i > 1 Then teacherRowIdString = teacherRowIdString & ", "
            teacherRowIdString = teacherRowIdString & teacherRowId(i)
        End If
    Next i
    
    ' Use the most recent school year.
    sql = "select distinct case when substr(assessDate, 6, 2) < ""09"" then cast(assessDate as integer) - 1 else cast(assessDate as integer) end" _
        & " from report" _
        & " where teacher in (" & teacherRowIdString & ")" _
        & " order by assessDate;"
    stmt = prepare(db, sql)
    nSchoolYearsPresent = 0
    Do
        If execStep(stmt) <> SQLITE_ROW Then Exit Do
        nSchoolYearsPresent = nSchoolYearsPresent + 1
        ReDim Preserve schoolYearPresent(nSchoolYearsPresent)
        schoolYearPresent(nSchoolYearsPresent) = SQLite3ColumnInt32(stmt, 0)
    Loop
    finalize stmt
    If UBound(schoolYearPresent) < 1 Then  'nothing
        MsgBox "I didn't find any student ALEKS data in the repository.", vbCritical, Title
        SQLite3Close db
        Exit Sub
    Else  'use most recent year
        schoolYear = schoolYearPresent(UBound(schoolYearPresent))
    End If
    
    ' Create a file system object to work with the file system.
    Set fs = CreateObject("Scripting.FileSystemObject")
    
    ' Update or create each worksheet.
    For i = 1 To nTeachers
        If teacherRowId(i) <> 0 Then  'teacher exists in repository
            trackingBookPathName = RepositoryFolder & FolderDelimiter & "ALEKS tracking worksheet - " & alphanum(teacher(i)) & ".xlsx"
            ' First try to open an existing worksheet.
            If fs.FileExists(trackingBookPathName) Then
                newTrackingBook = False
                ' Save backup copy of file, overwriting any previous backup.
                fs.CopyFile trackingBookPathName, "Backup copy of " & trackingBookPathName, True
                Set trackingBook = Workbooks.Open(trackingBookPathName)
            Else 'create new tracking worksheet
                newTrackingBook = True
                Workbooks.Add xlWBATWorksheet  '(must have one sheet)
            End If
            FillBook db, teacher(i), teacherRowId(i), newTrackingBook
            If newTrackingBook Then
                Worksheets(1).Visible = False  'hide original sheet (deleting would require user confirmation)
                ActiveWorkbook.SaveAs trackingBookPathName
            End If
            ActiveWorkbook.Close SaveChanges:=True
        End If
    Next i
    SQLite3Close db
    i = MsgBox("Done! Close the master workbook?", vbInformation + vbYesNo, Title)
    If i = vbYes Then
        ThisWorkbook.Close
    End If
End Sub

Public Sub FillBook(db As Long, teacher As String, teacherRowId As Long, newTrackingBook As Boolean)
    ' Populate a math worksheet for this teacher from the ALEKS repository.
    
    Dim sql As String
    Dim stmt As Long        'SQLite prepared statement handle
    Dim courseRow() As Long, courseName() As String, grade() As String, nCourses As Integer
    Dim iCourse As Integer  'loop index
    Dim SheetName As String
    
    ' Obtain a list of courses/grades for this teacher, sorted by grade, for the specified school year.
    nCourses = 0
    sql = "select distinct course.rowid, course.name, report.grade from report join course on course.rowid = report.course" _
        & " where report.teacher = " & teacherRowId & " and report.assessDate >= """ & SchoolYearBegan & """" _
        & " order by report.grade, course.name;"
    stmt = prepare(db, sql)
    Do
        If execStep(stmt) <> SQLITE_ROW Then Exit Do
        nCourses = nCourses + 1
        ReDim Preserve courseRow(nCourses), courseName(nCourses), grade(nCourses)
        courseRow(nCourses) = SQLite3ColumnInt32(stmt, 0)
        courseName(nCourses) = SQLite3ColumnText(stmt, 1)
        grade(nCourses) = SQLite3ColumnText(stmt, 2)
    Loop
    finalize stmt
    If nCourses = 0 Then
        MsgBox "Teacher " & dbValue(db, "teacher", "name", teacherRowId) & " has no courses in the ALEKS repository.", vbExclamation, Title
    Else
        ' Update or create a worksheet for each course/grade.
        For iCourse = 1 To nCourses
            ' Look for course sheet.
            
            SheetName = iCourse & " - grade " & grade(iCourse)
            ThisWorkbook.Sheets("template").Copy After:=Sheets(Sheets.Count)
            Sheets(Sheets.Count).Select
            Sheets(Sheets.Count).name = SheetName
            FillSheet Sheets(SheetName), db, teacher, teacherRowId, courseRow(iCourse), grade(iCourse), (iCourse = 1)
            Sheets(Sheets.Count).Protect
        Next iCourse
    End If
End Sub

Public Sub FillSheet(Sh As Worksheet, db As Long, teacher As String, teacherRowId As Long, courseRow As Long, grade As String, firstSheet As Boolean)
    Dim sql As String, stmt As Long, reportStmt As Long
    Dim reportRow As Long, studentRow() As Long, nStudents As Integer, iStudent As Integer
    Dim teacherCell As Range, yearCell As Range
    ' Set worksheet singletons.
    If firstSheet Then
        Sh.Parent.names("teacher").RefersToRange = teacher
        Sh.names("grade").RefersToRange = grade
        Sh.Parent.names("school_year").RefersToRange = (schoolYear - 1) & "-" & schoolYear
    Else
        Set teacherCell = Sh.names("teacher").RefersToRange
        Sh.names("teacher").Delete
        teacherCell.Formula = "=teacher"
        teacherCell.Locked = True
        Sh.names("grade").RefersToRange = grade
        Set yearCell = Sh.names("school_year").RefersToRange
        Sh.names("school_year").Delete
        yearCell.Formula = "=school_year"
        yearCell.Locked = True
    End If
    ' Find students for this teacher/course/grade/school year.
    nStudents = 0
    sql = "select distinct student from report" _
        & " where grade = " & grade & " and teacher = " & teacherRowId & " and course = " & courseRow _
        & " and assessDate > """ & SchoolYearBegan & """;"
    stmt = prepare(db, sql)
    Do
        If execStep(stmt) <> SQLITE_ROW Then Exit Do
        nStudents = nStudents + 1
        ReDim Preserve studentRow(nStudents)
        studentRow(nStudents) = SQLite3ColumnInt32(stmt, 0)
    Loop
    finalize stmt
    ' Add student results to the sheet.
    For iStudent = 1 To nStudents
        ' Find latest report for this student/teacher/course.
        sql = "select rowid from report" _
            & " where student = " & studentRow(iStudent) & " and teacher = " & teacherRowId & " and course = " & courseRow _
            & " order by batch desc limit 1;"
        reportStmt = prepare(db, sql)
        If execStep(reportStmt) = SQLITE_ROW Then
            reportRow = SQLite3ColumnInt32(reportStmt, 0)
            finalize reportStmt
            Application.ScreenUpdating = False
            addStudent Sh, db, reportRow, studentRow(iStudent)
            Application.ScreenUpdating = True
        Else
            finalize reportStmt
            '[report problem]
        End If
    Next iStudent
End Sub

Public Sub addStudent(Sh As Worksheet, db As Long, report As Long, student As Long)
    Dim col As Integer  'add student in this column
    Dim stuIndex As Integer  'column number within student name area
    Dim sql As String, stmt As Long
    col = nextStudentColumn(Sh)
    stuIndex = col - Sh.names("last_name").RefersToRange.Column + 1
    Sh.names("last_name").RefersToRange.Cells(stuIndex) = dbValue(db, "student", "lastName", student)
    Sh.names("first_initial").RefersToRange.Cells(stuIndex) = dbValue(db, "student", "substr(Name,1,1)", student) & "."
    ' Get ALEKS items, with strand and standard info and mastery flags, for this student report.
    sql = "select strand.symbol, standard.grade, standard.number, standard.text, item.text, mastery.mastered"
    sql = sql + " from reportedStandard"
    sql = sql + " join mastery on mastery.reportedStandard = reportedStandard.rowid"
    sql = sql + " join item on mastery.item = item.rowid "
    sql = sql + " join standard on item.standard = standard.rowid"
    sql = sql + " join strand on standard.strand = strand.number"
    sql = sql + " where reportedStandard.report = " & report & ";"
    stmt = prepare(db, sql)
    Application.Calculation = xlCalculationManual
    Do
        If execStep(stmt) <> SQLITE_ROW Then Exit Do
        addItemMastery Sh, col, _
            strandSymbol:=SQLite3ColumnText(stmt, 0), _
            grade:=SQLite3ColumnText(stmt, 1), _
            standardNum:=SQLite3ColumnInt32(stmt, 2), _
            standardText:=SQLite3ColumnText(stmt, 3), _
            itemText:=SQLite3ColumnText(stmt, 4), _
            mastered:=SQLite3ColumnInt32(stmt, 5)
    Loop
    Application.Calculation = xlCalculationAutomatic
    finalize stmt
End Sub

Public Sub addItemMastery(Sh As Worksheet, col As Integer, strandSymbol As String, grade As String, standardNum As Integer, standardText As String, itemText As String, mastered As Boolean)
    Dim strandRange As Range  'rows for this strand (we may add a row or two)
    Dim standardRange As Range
    Dim standardDesignator As String  'e.g., "7.P.1"
    Dim standardCell As Range, itemCell As Range  'results of Find attempts
    Dim row As Range, rowNum As Integer  'working row
    standardDesignator = grade & "." & strandSymbol & "." & standardNum
    Set strandRange = Sh.names("strand_" & strandSymbol).RefersToRange
    ' Find or insert a row for this standard.
    Set standardCell = strandRange.Columns(1).Find(What:=standardDesignator, LookAt:=xlWhole)
    If (standardCell Is Nothing) Then
        ' Insert a new standard row.
        rowNum = strandRange.row + strandRange.Rows.Count
        Sh.Rows(rowNum).Insert
        Set row = Sh.Rows(rowNum)
        ' Update named strand region.
        Set strandRange = SetNamedStrandRange(Sh, strandSymbol, rowNum)
        ' Format and fill in standard row.
        row.style = Sh.Parent.Styles("20% - Accent1")
        row.AutoFit
        row.FormatConditions.Delete
        row.Locked = True
        row.Cells(1).HorizontalAlignment = xlRight
        row.Cells(1) = standardDesignator
        row.Cells(2) = standardText
    Else
        Set row = Sh.Rows(standardCell.row)  'existing standard header
    End If
    ' row = the standard header row
    Set standardRange = standardRows(strandRange, row)  'rows for this standard
    ' Find or insert a row for this ALEKS item.
    Set itemCell = standardRange.Columns(2).Find(What:=itemText, LookAt:=xlWhole)
    If (itemCell Is Nothing) Then
        ' Insert a new item row.
        rowNum = standardRange.row + standardRange.Rows.Count
        Sh.Rows(rowNum).Insert
        Set row = Sh.Rows(rowNum)
        ' Update the named strand region.
        ' (standardRange is stale now, but we're done with it)
        Set strandRange = SetNamedStrandRange(Sh, strandSymbol, rowNum)
        row.style = Sh.Parent.Styles("Normal")
        row.AutoFit  'adjust height
        row.HorizontalAlignment = xlCenter
        row.Cells(2).HorizontalAlignment = xlLeft
        row.FormatConditions.Delete
        row.Locked = False
        row.Cells(1).Locked = True
        row.Cells(2).Locked = True
        row.Cells(2).style = Sh.Parent.Styles("ALEKS Item")
        row.Cells(2) = itemText
    Else
        Set row = Sh.Rows(itemCell.row)  'existing item row
    End If
    ' row = ALEKS item row
    ' Fill in the mastery cell if this student has mastered the item in ALEKS assessment.
    If mastered Then
        row.Cells(col) = 3
    End If
End Sub

Public Function SetNamedStrandRange(Sh As Worksheet, Symbol As String, IncludeRow As Integer) As Range
    ' Set the name strand_<Symbol> to cover rows up until the next strand header row.
    ' Include row IncludeRow, which may or may not be the last row in the range.
    ' (This is to handle extending the last strand range, which has only blank rows below it.)
    ' Return the range.
    Dim NextSymbol As Variant, FirstRow As Integer, LastRow As Integer
    Dim ThisName As name
    Set ThisName = Sh.names("strand_" & Symbol)
    FirstRow = ThisName.RefersToRange.row
    NextSymbol = Mid(StrandSymbols, InStr(StrandSymbols, Symbol) + 1, 1)
    If NextSymbol <> "" Then
        ' Assign the range up until the next strand range.
        LastRow = Sh.names("strand_" & NextSymbol).RefersToRange.row - 1
    Else
        ' Extend the range to include the last populated row and the row numbered IncludeRow.
        LastRow = ThisName.RefersToRange.row + ThisName.RefersToRange.Rows.Count - 1
        Do While Sh.Rows(LastRow).Cells(1).Value <> "" Or Sh.Rows(LastRow).Cells(2) <> "" Or LastRow < IncludeRow
            LastRow = LastRow + 1
        Loop
    End If
    ThisName.RefersToR1C1 = "='" & Sh.name & "'!R" & FirstRow & ":R" & LastRow
    Set SetNamedStrandRange = ThisName.RefersToRange
End Function

Public Function standardRows(strandRange As Range, startingRow As Range) As Range
    Dim cell As Range
    Dim lastStrandRow As Integer
    Dim lastStandardRow As Integer
    lastStrandRow = strandRange.Rows(strandRange.Rows.Count).row
    Set cell = startingRow.Cells(1)
    Do
        Set cell = cell.Offset(1)
    Loop Until cell.row > lastStrandRow Or cell.Value <> ""
    lastStandardRow = cell.row - 1
    Set standardRows = startingRow.Worksheet.Range(startingRow, startingRow.Offset(lastStandardRow - startingRow.row))
End Function

Public Function nextStudentColumn(Sh As Worksheet) As Integer
    ' Find the first unused student column.
    Dim cell As Variant
    For Each cell In Sh.names("last_name").RefersToRange.Cells
        If cell.Value = "" Then
            nextStudentColumn = cell.Column
            Exit For
        End If
    Next cell
End Function

Public Function quote(s As String) As String
    ' Return a string in single-quotes, with any single-quote marks in the string doubled.
    ' This works for an empty argument string, returning '' (literally--a pair of quote characters).
    Dim q As String   'the quoted result -> 's'
    Dim i As Integer  'index through character in s
    q = "'"  'the opening quote mark
    For i = 1 To Len(s)
        If Mid$(s, i, 1) = "'" Then
            q = q & "''"  ' doubled quote character
        Else
            q = q & Mid$(s, i, 1)
        End If
    Next i
    q = q & "'"  'the closing quote mark
    quote = q
End Function

Public Sub BuildItemRows(grade As Integer)
    ' The template sheet should already contain MCAS strand ranges
    ' named strand_N, strand_P, etc., for each of the five strands.
    ' The first row of each range contains the strand header.
    ' We'll insert new rows in each range for the MCAS standards and
    ' ALEKS items we find in our example data set.
    Dim Sh As Worksheet  'the template sheet on which we'll build
    Dim standards As Range  'standards list, col 1 = 3.N.1, col 2 = text
    Dim items As Range  'items list, col 1 = text, col 2 = 3.N.1 style standard
    Dim standardRow As Range, itemRow As Range  'iterators
    Dim standard As String, item As String  'values
    Dim standardIncluded As Boolean
    Dim newRow As Range
    
    Set Sh = Worksheets(1)  'the first worksheet should be our template
    Set standards = names("standards").RefersToRange
    Set items = names("items").RefersToRange
    
    For Each standardRow In standards.Rows
        standard = standardRow.Cells(1).Value
        standardIncluded = False
        For Each itemRow In items.Rows
            If itemRow.Cells(2).Value = standard Then
                item = itemRow.Cells(1).Value
                If standardIncluded Then
                    Set newRow = expandStrand(Sh, standard, "Normal")
                    newRow.Cells(2).style = Sh.Parent.Styles("ALEKS Item")
                    newRow.Cells(2) = item
                Else
                    Set newRow = expandStrand(Sh, standard, "20% - Accent1")
                    newRow.Cells(1).HorizontalAlignment = xlRight
                    newRow.Cells(1) = standard
                    standardIncluded = True
                End If
            End If
        Next itemRow
    Next standardRow
End Sub

Public Function strandOf(standard As String) As String
    ' Extract strand letter out of "3.N.1" style standard designator.
    Dim dot1 As Integer, dot2 As Integer
    
    dot1 = InStr(standard, ".")
    dot2 = InStr(dot1 + 1, standard, ".")
    strandOf = Mid(standard, dot1 + 1, dot2 - dot1 - 1)
End Function

Public Function expandStrand(Sh As Worksheet, standard As String, style As String) As Range
    ' Insert a row before the last row in the strand range that includes this
    ' standard. Apply the indicated style and return the new row as a range.
    Dim strandRange As Range, newRow As Range

    Set strandRange = Sh.names("strand_" + strandOf(standard)).RefersToRange
    Sh.Rows(strandRange.row + strandRange.Rows.Count - 1).Insert
    Set newRow = Sh.Rows(strandRange.row + strandRange.Rows.Count - 2)
    newRow.style = Sh.Parent.Styles(style)
    newRow.AutoFit
    Set expandStrand = newRow
End Function

Public Sub EmptyStrands(Sh As Worksheet)
    ' Remove all standard and item rows from each strand in the given worksheet.
    Dim names As Variant, name As Variant
    Dim r As Range
    
    names = Array("strand_N", "strand_P", "strand_G", "strand_M", "strand_D")
    For Each name In names
        Set r = Sh.names(name).RefersToRange
        If r.Rows.Count > 2 Then
            Sh.Range(r.Rows(3), r.Rows(r.Rows.Count)).Delete
        End If
    Next name
End Sub

Function AleksDb(dbPath As String) As Long
    ' Return an SQLite database connection handle to the ALEKS repository,
    ' or display an error message and return 0.
    Dim fileSys, dbFile, initVal, openVal
    Dim handle As Long
    AleksDb = 0
    Set fileSys = CreateObject("Scripting.FileSystemObject")
    On Error GoTo NOFILE
        Set dbFile = fileSys.GetFile(dbPath)
    On Error GoTo 0
    Let initVal = SQLite3Initialize
    If initVal = SQLITE_INIT_OK Then
        Let openVal = SQLite3Open(dbPath, handle)
        If openVal = 0 Then
            AleksDb = handle  'success
        Else
            MsgBox "Opening the ALEKS repository, SQLite3Open returned " & openVal & ", meaning I'm not sure what.", vbExclamation, Title
        End If
    Else
        MsgBox "Error initializing the SQLite database interface: " & Err.LastDllError, vbCritical, Title
    End If
    Exit Function
NOFILE:
    MsgBox "I can't find the ALEKS repository at """ & dbPath & """", vbCritical, Title
End Function

Public Function fetchValue(db As Long, sql As String) As Variant
    ' Fetch the first value from the repository, given a SQL SELECT string.
    Dim stmt As Long, result
    stmt = prepare(db, sql)
    result = execStep(stmt)
    If result = SQLITE_ROW Then
        fetchValue = columnValue(stmt, 0)
    Else
        fetchValue = Null
    End If
End Function

Public Function dbValue(db As Long, table As String, field As String, rowid As Long) As Variant
    ' Fetch a value from the repository by table, rowid, and field name.
    ' The field argument can be a field name or any SQLite expression.
    Dim sql As String, stmt As Long, result
    sql = "select " & field & " from " & table & " where rowid = " & rowid & ";"
    result = SQLite3PrepareV2(db, sql, stmt)
    If result <> 0 Then
        MsgBox "Unexpected statement preparation value " & result & " for statement " & sql, vbCritical, Title
        SQLite3Close db
        End
    End If
    result = execStep(stmt)
    If result = SQLITE_ROW Then
        dbValue = columnValue(stmt, 0)
    End If
    finalize stmt
End Function

Function columnValue(ByVal stmtHandle As Long, ByVal ZeroBasedColIndex As Long) As Variant
    Select Case SQLite3ColumnType(stmtHandle, ZeroBasedColIndex)
        Case SQLITE_INTEGER:
            columnValue = SQLite3ColumnInt32(stmtHandle, ZeroBasedColIndex)
        Case SQLITE_FLOAT:
            columnValue = SQLite3ColumnDouble(stmtHandle, ZeroBasedColIndex)
        Case SQLITE_TEXT:
            columnValue = SQLite3ColumnText(stmtHandle, ZeroBasedColIndex)
        Case SQLITE_BLOB:
            columnValue = SQLite3ColumnText(stmtHandle, ZeroBasedColIndex)
        Case SQLITE_NULL:
            columnValue = Null
    End Select
End Function


Public Function prepare(db As Long, sql As String) As Long
    ' Return a prepared SQLite3 statement handle, or display an error message and halt.
    Dim prepped As Variant, stmt As Long
    prepped = SQLite3PrepareV2(db, sql, stmt)
    If prepped <> 0 Then
        MsgBox "Unexpected statement preparation value " & prepped & " for statement " & sql, vbCritical, Title
        SQLite3Close db
        End
    End If
    prepare = stmt
End Function

Public Function execStep(stmt) As Long
    ' Execute prepared SQLite statement (up to one row) and return status.
    execStep = SQLite3Step(stmt)
End Function

Public Function finalize(stmt As Long) As Long
    ' Finalize a prepared SQLite3 statement, given its handle.
    finalize = SQLite3Finalize(stmt)
End Function

Public Function SchoolYearBegan() As String
    ' Return the begin date of the school year as a YYYY-MM-DD string.
    ' This is the same format used by the assessDate field in the report table.
    SchoolYearBegan = schoolYear & "-09-01"
End Function

Public Function twoDigits(n As Integer) As String
    ' If a number is less than 10, pad it with a leading zero.
    twoDigits = n
    If (n < 10) Then
        twoDigits = "0" & twoDigits
    End If
End Function

Public Function alphanum(s1 As String) As String
    ' Return a string after removing characters other than letters, digits, space, and period.
    Dim i As Long, s2 As String
    Dim legal As String
    legal = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz .0123456789"
    s2 = ""
    For i = 1 To Len(s1) Step 1
        If InStr(legal, Mid$(s1, i, 1)) > 0 Then s2 = s2 & Mid$(s1, i, 1)
    Next i
    alphanum = s2
End Function
