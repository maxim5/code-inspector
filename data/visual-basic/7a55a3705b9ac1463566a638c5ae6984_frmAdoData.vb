
Option Explicit On

'*******************************************************************
' Program: xmlVisio
' Author: Albert E Edlund
' Date:
'
' Purpose:
' Working with visio drawing in an xml format
'
'
'
'*******************************************************************


Imports System
Imports System.Collections.Generic
Imports System.Collections.ObjectModel
Imports System.Text
Imports System.Text.StringBuilder

Imports Microsoft.Practices.EnterpriseLibrary.Common
Imports Microsoft.Practices.EnterpriseLibrary.Data
Imports Microsoft.Practices.EnterpriseLibrary.ExceptionHandling
Imports Microsoft.Practices.EnterpriseLibrary.ExceptionHandling.Logging

Imports System.IO
Imports System.Windows.Forms
Imports System.Xml
Imports System.Xml.Serialization
Imports System.Xml.Linq
Imports System.Xml.XPath

Imports <xmlns="http://schemas.microsoft.com/visio/2003/core">
Imports <xmlns:vx="http://schemas.microsoft.com/visio/2006/extension">
Imports <xmlns:v14="http://schemas.microsoft.com/office/visio/2010/extension">

Imports ADODB
Imports System.Data
Imports System.Data.OleDb
Imports System.Data.Sql




Public Class frmAdoData


#Region " Properties and Declarations "


    Private _dataPath As String
    Public Property dataPath As String
        Get
            dataPath = _dataPath
        End Get
        Set(value As String)
            _dataPath = value
        End Set
    End Property



    Public _xDoc As XDocument
    Public Property xDoc As XDocument
        Get
            Return Me._xDoc
        End Get
        Set(value As XDocument)
            If (value Is Nothing) Then
                Throw New System.ArgumentNullException("Value", _
                    "xDoc is Null.")
            End If
            Me._xDoc = value
        End Set
    End Property

    Private _nsDict As Dictionary(Of String, System.Xml.Linq.XNamespace)
    Public Property nsDict As Dictionary(Of String, System.Xml.Linq.XNamespace)
        Get
            Return Me._nsDict
        End Get
        Set(value As Dictionary(Of String, System.Xml.Linq.XNamespace))
            If (value Is Nothing) Then
                Throw New System.ArgumentNullException("Value", _
                    "nsDict is Null.")
            End If
            Me._nsDict = value
        End Set
    End Property

    Private _lngRsId As Long
    Public Property lngRsId As Long
        Get
            lngRsId = _lngRsId
        End Get
        Set(value As Long)
            _lngRsId = value
        End Set
    End Property

    Private _lngRsConnId As Long
    Public Property lngRsConnId As Long
        Get
            lngRsConnId = _lngRsConnId
        End Get
        Set(value As Long)
            _lngRsConnId = value
        End Set
    End Property

    Private _strRsName As String
    Public Property strRsName As String
        Get
            strRsName = _strRsName
        End Get
        Set(value As String)
            _strRsName = value
        End Set
    End Property

    ' data connection filename attribute
    Private _strDcFileName As String
    Public Property strDcFileName As String
        Get
            strDcFileName = _strDcFileName
        End Get
        Set(value As String)
            _strDcFileName = value
        End Set
    End Property

    ' dataconnection connection string
    Private _strDcConn As String
    Public Property strDcConn As String
        Get
            strDcConn = _strDcConn
        End Get
        Set(value As String)
            _strDcConn = value
        End Set
    End Property

    Private _strDcCmd As String
    Public Property strDcCmd As String
        Get
            strDcCmd = _strDcCmd
        End Get
        Set(value As String)
            _strDcCmd = value
        End Set
    End Property




#End Region




#Region " Document Data "




    '
    ' read v2007 read data connections
    '
    Private Sub getDataConnections()


        Try

            ' first check for a v2007/v12 implementation
            Dim qryDataConnection = Me.xDoc...<vx:DataConnection>
            Dim intConnection As Integer = qryDataConnection.Count

            For Each data_connection In qryDataConnection

                Dim listOfAttributes As IEnumerable(Of XAttribute) = _
                    From attr In data_connection.Attributes() _
                    Select attr

                For Each attr As XAttribute In listOfAttributes
                    Console.WriteLine("{0}={1}", attr.Name.LocalName, attr.Value)
                Next attr

            Next data_connection


            Me.TabControl1.SelectTab(tabADOdata)

        Catch err As Exception
            Dim rethrow As Boolean = ExceptionPolicy.HandleException(err, "Log Only Policy")
            If (rethrow) Then
                Throw
            End If
        End Try

    End Sub

    '
    ' read v2007 data connection by connection id
    '
    Private Sub getDataConnections(ByVal lngId As Long)


        Try

            Dim lngConnId As Long

            ' first check for a v2007/v12 implementation
            Dim qryDataConnection = Me.xDoc...<vx:DataConnection>
            Dim intConnection As Integer = qryDataConnection.Count

            For Each connection In qryDataConnection

                Dim listOfAttributes As IEnumerable(Of XAttribute) = _
                    From att In connection.Attributes() _
                    Select att

                For Each att As XAttribute In listOfAttributes

                    Select Case LCase(att.Name.LocalName)
                        Case "id"
                            lngConnId = CLng(att.Value)
                        Case "filename"
                            ' get the filename
                            strDcFileName = CStr(att.Value)
                        Case "connectionstring"
                            strDcConn = CStr(att.Value)
                        Case "command"
                            strDcCmd = CStr(att.Value)
                    End Select

                Next att

                If lngConnId = lngId Then Exit For

            Next connection

            Me.TabControl1.SelectTab(tabADOdata)

        Catch err As Exception
            Dim rethrow As Boolean = ExceptionPolicy.HandleException(err, "Log Only Policy")
            If (rethrow) Then
                Throw
            End If
        End Try

    End Sub


    '
    ' pass in a listview control and load it with the datarecordsets
    '
    Private Sub getDataRecordSets(ByRef lvControl As ListView)

        Try

            ' first check for a v2007/v12 implementation
            Dim qryRecordsets = xDoc...<vx:DataRecordSet>
            Dim intRecordsets As Integer = qryRecordsets.Count

            lvControl.Items.Clear()
            lvControl.Columns.Clear()
            lvControl.View = View.Details
            lvControl.AllowColumnReorder = True
            lvControl.FullRowSelect = True
            lvControl.Columns.Add("Name", 100, HorizontalAlignment.Left)
            lvControl.Columns.Add("Id", 40, HorizontalAlignment.Right)
            lvControl.Columns.Add("ConnId", 40, HorizontalAlignment.Right)

            Dim lvItem As ListViewItem

            For Each recordset In qryRecordsets

                lngRsId = -1
                lngRsConnId = -1
                strRsName = "NoName"

                Dim listOfAttributes As IEnumerable(Of XAttribute) = _
                    From att In recordset.Attributes() _
                    Select att

                For Each att As XAttribute In listOfAttributes
                    ' Console.WriteLine("{0}={1}", att.Name.LocalName, att.Value)
                    Select Case LCase(att.Name.LocalName)
                        Case "id"
                            lngRsId = CLng(att.Value)
                        Case "name"
                            ' get the recordset name
                            strRsName = CStr(att.Value)
                        Case "connectionid"
                            lngRsConnId = CLng(att.Value)
                    End Select
                Next att

                lvItem = New ListViewItem(strRsName)
                lvItem.SubItems.Add(CStr(lngRsId))
                lvItem.SubItems.Add(CStr(lngRsConnId))

                ' add it to the listview on the data page
                lvControl.Items.Add(lvItem)

                ' get the first recordset table date
                If lvControl.Items.Count = 1 And _
                    lvControl.Name = "lvAdoRecordsets" Then

                    ' get the data/definitions
                    getAdoData(recordset)
                    ' get the dataconnection information
                    getDataConnections(lngRsConnId)
                    ' flag the user fields so they know where it came from
                    Me.txtDcFileName.Text = strDcFileName
                    Me.txtDcConnectionString.Text = strDcConn
                    Me.txtDcCommand.Text = strDcCmd
                    'make sure we are on the correct tab
                    Me.TabControl1.SelectTab(tabADOdata)

                End If
                'getDataColumnSet(recordset)

            Next recordset



        Catch err As Exception
            Dim rethrow As Boolean = ExceptionPolicy.HandleException(err, "Log Only Policy")
            If (rethrow) Then
                Throw
            End If
        End Try

    End Sub

    '
    ' read v2007 schema DataRecordSet query for selected recordset id
    '
    Private Sub getDataRecordSetInfo(ByVal intRsId As Integer)

        Try

            ' first check for a v2007/v12 implementation
            Dim qryRecordsets = Me.xDoc...<vx:DataRecordSet>
            Dim intRecordsets As Integer = qryRecordsets.Count

            Me.txtDcCommand.Text = ""
            Me.txtDcConnectionString.Text = ""
            Me.txtDcFileName.Text = ""

            For Each recordset In qryRecordsets

                lngRsId = -1
                lngRsConnId = -1
                strRsName = "NoName"
                Me.txtSchema.Text = ""


                Dim listOfAttributes As IEnumerable(Of XAttribute) = _
                    From att In recordset.Attributes() _
                    Select att
                For Each att As XAttribute In listOfAttributes
                    ' Console.WriteLine("{0}={1}", att.Name.LocalName, att.Value)
                    Select Case LCase(att.Name.LocalName)
                        Case "id"
                            lngRsId = CLng(att.Value)
                        Case "name"
                            ' get the recordset name
                            strRsName = CStr(att.Value)
                        Case "connectionid"
                            lngRsConnId = CLng(att.Value)
                    End Select
                Next att


                If lngRsId = intRsId Then

                    getAdoData(recordset)
                    getDataConnections(lngRsConnId)
                    Me.txtDcFileName.Text = strDcFileName
                    Me.txtDcConnectionString.Text = strDcConn
                    Me.txtDcCommand.Text = strDcCmd

                    Exit For
                End If

                'getDataColumnSet(recordset)

            Next recordset

            Me.TabControl1.SelectTab(tabADOdata)

        Catch err As Exception
            Dim rethrow As Boolean = ExceptionPolicy.HandleException(err, "Log Only Policy")
            If (rethrow) Then
                Throw
            End If
        End Try

    End Sub

    '
    ' read v2007 schema AdoData query, source is datarecordset
    ' this is where the local schema and data is stored
    '
    Private Sub getAdoData(ByVal shapeSource As XElement)


        ' first check for a v2007/v12 implementation
        Dim qryAdoData = shapeSource...<vx:ADOData>
        Dim intAdoData As Integer = qryAdoData.Count

        Dim rsData As ADODB.Recordset = New ADODB.Recordset
        Dim dsData As System.Data.DataSet = New System.Data.DataSet
        Dim swXml As StringWriter = New StringWriter

        Try

            For Each dataAdo In qryAdoData

                Dim listOfAttributes As IEnumerable(Of XAttribute) = _
                    From att In dataAdo.Attributes() _
                    Select att
                For Each att As XAttribute In listOfAttributes
                    ' Console.WriteLine(att)
                Next

                Dim listOfElements As IEnumerable(Of XElement) = _
                    From elemData In dataAdo.Elements() _
                    Select elemData

                If listOfElements.Count > 0 Then

                    For Each elemData As XElement In listOfElements

                        ' make a recordset out of the elemData
                        rsData = RecordsetFromXmlString(elemData.ToString)

                        ' make a dataset out of the recordset
                        Dim myDA As OleDb.OleDbDataAdapter = New OleDb.OleDbDataAdapter
                        myDA.Fill(dsData, rsData, strRsName)

                        ' add the data to the datagridview
                        With dgvData
                            .DataSource = dsData
                            .DataMember = strRsName
                        End With

                        'also add the schema to the xml text box
                        dsData.WriteXmlSchema(swXml)
                        Me.txtSchema.Text = swXml.ToString
                        System.Windows.Forms.Application.DoEvents()
                    Next elemData ' should only have been one

                End If ' test to make sure we got at least one 

            Next dataAdo

        Catch err As Exception
            Dim rethrow As Boolean = ExceptionPolicy.HandleException(err, "Log Only Policy")
            If (rethrow) Then
                Throw
            End If
        End Try

    End Sub

    '
    '
    '
    Private Function RecordsetFromXmlString(strXml As String) _
        As ADODB.Recordset

        Dim rsData As ADODB.Recordset = New ADODB.Recordset
        Dim oStream As ADODB.Stream = New ADODB.Stream


        Try

            oStream.Open()
            oStream.WriteText(strXml)
            oStream.Position = 0
            rsData.Open(oStream)

        Catch err As Exception
            Dim rethrow As Boolean = ExceptionPolicy.HandleException(err, "Log Only Policy")
            If (rethrow) Then
                Throw
            End If

        Finally
            If oStream.State = ObjectStateEnum.adStateOpen Then
                oStream.Close()
            End If
            oStream = Nothing
            RecordsetFromXmlString = rsData
            rsData = Nothing
        End Try

    End Function

    '
    '
    '
    Private Sub getExternalData(ByVal RecordSource As XElement)

        Try

            Dim dictAttribute As Dictionary(Of String, System.Xml.Linq.XElement) = New Dictionary(Of String, System.Xml.Linq.XElement)
            Dim attrName As String

            '
            ' walk through the schema definition
            '
            Dim qrySchema = RecordSource.Elements().Where(Function(e) e.Name.LocalName = "Schema")
            Dim intSchema As Integer = qrySchema.Count
            If intSchema = 1 Then
                Dim elemSchema As XElement = qrySchema(0)
                Dim qryElementType = elemSchema.Elements().Where(Function(e) e.Name.LocalName = "ElementType")
                Dim intElementType As Integer = qryElementType.Count
                If intElementType = 1 Then
                    Dim elemAttributeType As XElement = qryElementType(0)
                    Dim qryAttributeType = elemAttributeType.Elements().Where(Function(e) e.Name.LocalName = "AttributeType")
                    Dim intAttributeType As Integer = qryAttributeType.Count
                    If intAttributeType > 0 Then
                        For Each attrType In qryAttributeType
                            Dim qryAttrName = attrType.Attributes.Where(Function(e) e.Name.LocalName = "name")
                            Dim intAttrCt As Integer = qryAttrName.Count
                            If intAttrCt > 0 Then
                                ' there may be more than one "name" attribute based on namespace
                                Dim strName As String = qryAttrName(0).ToString
                                dictAttribute.Add(strName, attrType)
                            End If
                        Next
                    End If
                End If
            End If

        Catch err As Exception
            Dim rethrow As Boolean = ExceptionPolicy.HandleException(err, "Log Only Policy")
            If (rethrow) Then
                Throw
            End If
        End Try



    End Sub


#End Region






#Region " File Operations "


    '
    '
    '
    Private Function openVDX_File() As XDocument

        Dim document As XmlDocument
        document = New XmlDocument

        Try

            OpenFileDialog.InitialDirectory = Me.dataPath
            OpenFileDialog.FileName = "*.vdx"
            OpenFileDialog.Filter = "visio drawings (*.vdx)|*.vsd|All files (*.*)|*.*"
            OpenFileDialog.FilterIndex = 1
            OpenFileDialog.RestoreDirectory = True

            Dim strPath As String = ""
            Dim dlgResult As System.Windows.Forms.DialogResult = OpenFileDialog.ShowDialog

            If dlgResult = System.Windows.Forms.DialogResult.OK Then

                Me.txtFileName.Text = OpenFileDialog.FileName

                xDoc = XDocument.Load(Me.txtFileName.Text)

            End If

        Catch err As Exception
            Dim rethrow As Boolean = ExceptionPolicy.HandleException(err, "Log Only Policy")
            If (rethrow) Then
                Throw
            End If
        End Try



        Return xDoc

    End Function



#End Region



    '
    ' init
    '
    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        txtFileName.Text = "c:\applications\xmlVisio\xmlVisio\Drawings\vbaAppConnect.vdx"

        Me._xDoc = Nothing

    End Sub


    '
    ' open vdx file, get any recordsets and list them, load form with first recordset found
    '
    Private Sub OpenToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles OpenToolStripMenuItem.Click
        openVDX_File()
        'getDataConnections(1)
        getDataRecordSets(Me.lvAdoRecordsets)
        ' get the recordset id of the first listviewitem
        Dim subitemTop As ListViewItem.ListViewSubItem = Me.lvAdoRecordsets.TopItem.SubItems(1)
        getDataRecordSetInfo(CInt(subitemTop.Text))

    End Sub


    '
    ' get out
    '
    Private Sub ExitToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles ExitToolStripMenuItem.Click
        Me._xDoc = Nothing
        Me.Close()
    End Sub

    '
    ' load data based upon selected recordset
    '
    Private Sub lvAdoRecordsets_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles lvAdoRecordsets.SelectedIndexChanged

        Dim rsItems As ListView.SelectedListViewItemCollection = lvAdoRecordsets.SelectedItems
        Dim rsItem As ListViewItem
        Dim rsSubItem As ListViewItem.ListViewSubItem

        If 0 < rsItems.Count Then
            rsItem = rsItems(0)
            rsSubItem = rsItem.SubItems(1)
            getDataRecordSetInfo(CInt(rsSubItem.Text))
        End If

    End Sub



    '
    '
    '
    Private Sub frmExternalData_Activated(sender As Object, e As System.EventArgs) Handles Me.Activated

        'getDataConnections(1)
        getDataRecordSets(Me.lvAdoRecordsets)
        ' get the recordset id of the first listviewitem
        Dim subitemTop As ListViewItem.ListViewSubItem = Me.lvAdoRecordsets.TopItem.SubItems(1)
        getDataRecordSetInfo(CInt(subitemTop.Text))

    End Sub

    '
    '
    '
    Private Sub AboutToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles AboutToolStripMenuItem.Click
        frmAbout.ShowDialog()
    End Sub


End Class