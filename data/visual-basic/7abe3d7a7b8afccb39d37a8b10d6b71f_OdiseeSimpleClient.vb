'
' Odisee(R)
' Copyright (C) 2011-2013 art of coding UG, http://www.art-of-coding.eu
' Copyright (C) 2005-2010 Informationssysteme Ralf Bensmann, http://www.bensmann.com
'

Option Strict On
Option Explicit On

Imports System.Xml
Imports System.Net

''' <summary>
''' A client supporting just one request.
''' </summary>
''' <remarks></remarks>
Public Class OdiseeSimpleClient

#Region "Properties"

    ''' <summary>
    ''' The service URL.
    ''' </summary>
    ''' <remarks></remarks>
    Private serviceURL As String

    ''' <summary>
    ''' The username (for HTTP BASIC authentication).
    ''' </summary>
    ''' <remarks></remarks>
    Private username As String

    ''' <summary>
    ''' The password (for HTTP BASIC authentication).
    ''' </summary>
    ''' <remarks></remarks>
    Private password As String

    ''' <summary>
    ''' The auth key (e.g. used for non-public templates).
    ''' </summary>
    ''' <remarks></remarks>
    Private authKey As String

    ''' <summary>
    ''' The Odisee request XML document.
    ''' </summary>
    ''' <remarks></remarks>
    Private __xmlDoc As XmlDocument
    Public ReadOnly Property xmlDoc As XmlDocument
        Get
            Return __xmlDoc
        End Get
    End Property

    ''' <summary>
    ''' XML document/root element: &lt;odisee>&lt;/odisee>.
    ''' </summary>
    Private xmlRoot As XmlElement

    ''' <summary>
    ''' The actual (= last appended) &lt;request> element in &lt;odisee>.
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public ReadOnly Property actualRequest As XmlElement
        Get
            Return CType(xmlRoot.SelectSingleNode(OdiseeConstant.LAST_REQUEST), XmlElement)
        End Get
    End Property

#End Region

#Region "Odisee Client Factory Methods"

    '
    ' Private constructor (used with factory methods).
    '
    Private Sub New()
        ' Create fresh XML document and add root element
        __xmlDoc = New XmlDocument()
        xmlRoot = __xmlDoc.CreateElement("odisee")
        __xmlDoc.AppendChild(xmlRoot)
    End Sub

    '
    ' Factory method for creating a Odisee XML client.
    '
    Public Shared Function createClient(ByVal serviceURL As String) As OdiseeSimpleClient
        Dim odiseeClient As OdiseeSimpleClient = New OdiseeSimpleClient()
        ' Set service URL
        odiseeClient.serviceURL = serviceURL
        ' Return instance of OdiseeClient
        Return odiseeClient
    End Function

    '
    ' Factory method for creating a Odisee XML client.
    '
    Public Shared Function createClient(ByVal serviceURL As String, ByVal authKey As String) As OdiseeSimpleClient
        Dim odiseeClient As OdiseeSimpleClient = New OdiseeSimpleClient()
        ' Set service URL
        odiseeClient.serviceURL = serviceURL
        ' Set authkey
        odiseeClient.authKey = authKey
        ' Return instance of OdiseeClient
        Return odiseeClient
    End Function

    '
    ' Factory method for creating a Odisee XML client.
    '
    Public Shared Function createClient(ByVal serviceURL As String, ByVal username As String, ByVal password As String) As OdiseeSimpleClient
        Dim odiseeClient As OdiseeSimpleClient = New OdiseeSimpleClient()
        ' Set service URL
        odiseeClient.serviceURL = serviceURL
        ' Remember username and password
        odiseeClient.username = username
        odiseeClient.password = password
        ' Return instance of OdiseeClient
        Return odiseeClient
    End Function

#End Region

#Region "Odisee Request API Methods"

    ''' <summary>
    ''' Post process, merge the resulting document with another. Adds a &lt;action type="merge-with"> element to &lt;post-process>,
    ''' XPath //request[last()]/post-process/action.
    ''' </summary>
    ''' <param name="filepath"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function mergeDocumentAtEnd(ByVal filepath As String) As OdiseeSimpleClient
        Dim xmlElement As XmlElement = __xmlDoc.CreateElement("action")
        xmlElement.SetAttribute("type", "merge-with")
        xmlElement.AppendChild(__xmlDoc.CreateElement("result-placeholder"))
        Dim inputFileElement As XmlElement = __xmlDoc.CreateElement("input")
        inputFileElement.SetAttribute("file", filepath)
        xmlElement.AppendChild(inputFileElement)
        ' Append in last instructions-element
        Helper.Xml.appendPostProcessInstruction(actualRequest, xmlElement)
        Return Me
    End Function

    ''' <summary>
    ''' Append a 'execute macro' instruction (in //request/instructions[last()]).
    ''' </summary>
    ''' <param name="macroName"></param>
    ''' <param name="location"></param>
    ''' <param name="language"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function executeMacro(ByVal macroName As String, ByVal location As String, ByVal language As String, ByVal parameters As List(Of String)) As OdiseeSimpleClient
        Dim xmlElement As XmlElement = __xmlDoc.CreateElement("macro")
        xmlElement.SetAttribute("name", macroName)
        xmlElement.SetAttribute("location", location)
        xmlElement.SetAttribute("language", language)
        If Not IsNothing(parameters) And parameters.Count > 0 Then
            For Each e As String In parameters
                Dim parameterElement As XmlElement = __xmlDoc.CreateElement("parameter")
                parameterElement.InnerText = e
                xmlElement.AppendChild(parameterElement)
            Next
        End If
        ' Append in last instructions-element
        Helper.Xml.appendToLastInstruction(actualRequest, xmlElement)
        Return Me
    End Function

    ''' <summary>
    ''' Execute a Basic macro contained in the actual template. Append a 'execute macro' instruction (in //request/instructions[last()]).
    ''' </summary>
    ''' <param name="macroName"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function executeBasicMacroInDocument(ByVal macroName As String) As OdiseeSimpleClient
        Return executeMacro(macroName, "document", "Basic", Nothing)
    End Function

    ''' <summary>
    ''' Set a value in a table cell.
    ''' </summary>
    ''' <param name="tableName"></param>
    ''' <param name="coordinate"></param>
    ''' <param name="cellValue"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function setTableCellValue(ByVal tableName As String, ByVal coordinate As String, ByVal cellValue As String) As OdiseeSimpleClient
        Dim xmlElement As XmlElement = __xmlDoc.CreateElement("userfield")
        xmlElement.SetAttribute("name", tableName & "!" & coordinate)
        xmlElement.InnerText = cellValue
        ' Append in last instructions-element
        Helper.Xml.appendToLastInstruction(actualRequest, xmlElement)
        Return Me
    End Function

    ''' <summary>
    ''' Set a value in a userfield. Userfields are global variables.
    ''' </summary>
    ''' <param name="userfieldName"></param>
    ''' <param name="userfieldValue"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function setUserfield(ByVal userfieldName As String, ByVal userfieldValue As String) As OdiseeSimpleClient
        Dim xmlElement As XmlElement = __xmlDoc.CreateElement("userfield")
        xmlElement.SetAttribute("name", userfieldName)
        xmlElement.InnerText = userfieldValue
        ' Append in last instructions-element
        Helper.Xml.appendToLastInstruction(actualRequest, xmlElement)
        '
        Return Me
    End Function

    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="archiveToDatabase"></param>
    ''' <param name="archiveToFilesystem"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function setArchive(ByVal archiveToDatabase As Boolean, ByVal archiveToFilesystem As Boolean) As OdiseeSimpleClient
        Dim archiveElement As XmlElement = __xmlDoc.CreateElement("archive")
        archiveElement.SetAttribute("database", archiveToDatabase.ToString.ToLower)
        archiveElement.SetAttribute("files", archiveToFilesystem.ToString.ToLower)
        actualRequest.AppendChild(archiveElement)
        '
        Return Me
    End Function

    ''' <summary>
    ''' Use latest version of template.
    ''' </summary>
    ''' <param name="template">Name of template (filename w/o extension)</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function setLatestTemplate(ByVal template As String, ByVal outputFormat As String) As OdiseeSimpleClient
        Dim templateElement As XmlElement = __xmlDoc.CreateElement("template")
        actualRequest.AppendChild(templateElement)
        templateElement.SetAttribute("name", template)
        ' Use latest revision of template
        templateElement.SetAttribute("revision", "LATEST")
        ' Output format by file extension
        templateElement.SetAttribute("outputFormat", outputFormat)
        ' Append element to request
        actualRequest.AppendChild(templateElement)
        '
        Return Me
    End Function

    ''' <summary>
    ''' Create a new Odisee request and adds it to the XML document (XPath //odisee/request[last()]).
    ''' </summary>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Private Function createRequest() As OdiseeSimpleClient
        ' <request>
        Dim requestElement As XmlElement = __xmlDoc.CreateElement("request")
        requestElement.SetAttribute("id", "1")
        requestElement.SetAttribute("name", "OdiseeClient")
        xmlRoot.AppendChild(requestElement)
        ' Odisee Server group
        Dim oooElement As XmlElement = __xmlDoc.CreateElement("ooo")
        oooElement.SetAttribute("group", "group0")
        requestElement.AppendChild(oooElement)
        ' Fluent API
        Return Me
    End Function

    ''' <summary>
    ''' Create a new Odisee request and adds it to the XML document (XPath //odisee/request[last()]).
    ''' </summary>
    ''' <param name="template"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function createRequest(ByVal template As String, Optional ByVal outputFormat As String = "pdf") As XmlElement
        ' Create request
        createRequest()
        ' Set template
        setLatestTemplate(template, outputFormat)
        ' Archiving
        setArchive(False, False)
        ' <instructions>
        Dim instructionsElement As XmlElement = __xmlDoc.CreateElement("instructions")
        actualRequest.AppendChild(instructionsElement)
        ' Return created XmlElement which can be used for calling functions when creating multiple requests.
        Return actualRequest
    End Function

    ''' <summary>
    ''' Process request with an Odisee server using credentials set when an instance was created through one of the factory methods.
    ''' </summary>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function process() As WebResponse
        ' Check state
        ' Service URL set?
        If IsNothing(serviceURL) Then
            Throw New Exception(OdiseeConstant.ERR_NO_SERVICE_URL)
        End If
        ' Username w/o password?
        If Not IsNothing(username) And IsNothing(password) Then
            Throw New Exception(OdiseeConstant.ERR_NO_AUTH_INFO)
        End If
        ' Send Odisee request XML document through HTTP POST
        Dim webResponse As WebResponse
        If Not IsNothing(username) And Not IsNothing(password) Then
            webResponse = Helper.HttpPost.doDigestAuthPost(__xmlDoc, New Uri(serviceURL), username, password)
        Else
            webResponse = Helper.HttpPost.doDigestAuthPost(__xmlDoc, New Uri(serviceURL))
        End If
        ' Return response from Odisee HTTP server
        Return webResponse
    End Function

#End Region

End Class
