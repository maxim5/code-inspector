Imports System.Diagnostics.Contracts
Imports CookComputing.XmlRpc
Imports System.Security.Cryptography.X509Certificates
Imports System.Net
Imports System.Text

Namespace XmlRpc.Interfaces

    ''' <summary>
    ''' Defines general methods to search media into Fotolia repository.
    ''' </summary>
    ''' <remarks></remarks>
    <XmlRpcUrl("http://api.fotolia.com/Xmlrpc/rpc")>
    Public Interface ISearch
        Inherits IFotoliaBase

#Region "xmlrpc.test"
        ''' <summary>
        ''' This method is a test method which returns success if connection is valid.
        ''' </summary>
        ''' <param name="apiKey">Authentication.</param>
        ''' <returns></returns>
        ''' <remarks>You need a valid api key to use the API</remarks>
        <XmlRpcMethod("xmlrpc.test")>
        Function Test(apiKey As XmlRpc.Authentication) As XmlRpcStruct

        ''' <summary>
        ''' This method is a test method which returns success if connection is valid using Async mode.
        ''' </summary>
        ''' <param name="apiKey">Authentication.</param>
        ''' <returns></returns>
        ''' <remarks>You need a valid api key to use the API</remarks>
        <XmlRpcBegin("xmlrpc.test")>
        Function BeginTest(apiKey As XmlRpc.Authentication, acb As AsyncCallback) As IAsyncResult

        ''' <summary>
        ''' This method is a test method which returns success if connection is valid using Async mode.
        ''' </summary>
        ''' <param name="apiKey">Authentication.</param>
        ''' <returns></returns>
        ''' <remarks>You need a valid api key to use the API</remarks>
        <XmlRpcBegin("xmlrpc.test")>
        Function BeginTest(apiKey As XmlRpc.Authentication, acb As AsyncCallback, state As Object) As IAsyncResult

        ''' <summary>
        ''' This method implements the End part of BeginTest method.
        ''' </summary>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcEnd("xmlrpc.test")>
        Function EndTest(iasr As IAsyncResult) As XmlRpcStruct
#End Region

#Region "xmlrpc.getData"

        ''' <summary>
        ''' This methods returns fotolia data.
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcMethod("xmlrpc.getData")>
        Function GetData(apiKey As XmlRpc.Authentication) As XmlRpcStruct


        ''' <summary>
        ''' This methods returns fotolia data using async pattern.
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="acb">The async callback.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getData")>
        Function BeginGetData(apiKey As XmlRpc.Authentication, acb As AsyncCallback) As IAsyncResult

        ''' <summary>
        ''' This methods returns fotolia data using async pattern.
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="acb">The async callback.</param>
        ''' <param name="state">The object state.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getData")>
        Function BeginGetData(apiKey As XmlRpc.Authentication, acb As AsyncCallback, state As Object) As IAsyncResult

        ''' <summary>
        ''' Implements the End part of async pattern.
        ''' </summary>
        ''' <param name="iasr">The async result instance.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcEnd("xmlrpc.getData")>
        Function EndGetData(iasr As IAsyncResult) As XmlRpcStruct
#End Region

#Region "xmlrpc.getCategories1"
        ''' <summary>
        ''' This method returns childs of a parent category in fotolia representative category system. 
        ''' This method could be used to display a part of the category system or the all tree. 
        ''' Fotolia categories system counts three levels
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="language_id">The language id.</param>
        ''' <param name="id">The category parent id. (0 fo first level categories).</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcMethod("xmlrpc.getCategories1")>
        Function GetCategories1(apiKey As XmlRpc.Authentication, language_id As Integer, id As Integer) As XmlRpcStruct

        ''' <summary>
        ''' This method returns childs of a parent category (in async mode) in fotolia representative category system. 
        ''' This method could be used to display a part of the category system or the all tree. 
        ''' Fotolia categories system counts three levels
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="language_id">The language id.</param>
        ''' <param name="id">The category parent id. (0 fo first level categories).</param>
        ''' <param name="acb">The async callback to manage results.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getCategories1")>
        Function BeginGetCategories1(apiKey As XmlRpc.Authentication,
                                     language_id As Integer,
                                     id As Integer,
                                     acb As AsyncCallback) As IAsyncResult

        ''' <summary>
        ''' This method returns childs of a parent category (in async mode) in fotolia representative category system. 
        ''' This method could be used to display a part of the category system or the all tree. 
        ''' Fotolia categories system counts three levels
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="language_id">The language id.</param>
        ''' <param name="id">The category parent id. (0 fo first level categories).</param>
        ''' <param name="acb">The async callback to manage results</param>
        ''' <param name="state">The user state.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getCategories1")>
        Function BeginGetCategories1(apiKey As XmlRpc.Authentication,
                                     language_id As Integer,
                                     id As Integer,
                                     acb As AsyncCallback,
                                     state As Object) As IAsyncResult

        ''' <summary>
        ''' The end part of Begin method.
        ''' </summary>
        ''' <param name="iasr">The async result used to retrieve method results.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcEnd("xmlrpc.getCategories1")>
        Function EndGetCategories1(iasr As IAsyncResult) As XmlRpcStruct
#End Region

#Region "xmlrpc.getCategories2"
        ''' <summary>
        ''' This method returns childs of a parent category in fotolia conceptual category system. 
        ''' This method could be used to display a part of the category system or the all tree. 
        ''' Fotolia categories system counts three levels.
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="language_id">The language id.</param>
        ''' <param name="id">The category parent id. (0 fo first level categories).</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcMethod("xmlrpc.getCategories2")>
        Function GetCategories2(apiKey As XmlRpc.Authentication, language_id As Integer, id As Integer) As XmlRpcStruct

        ''' <summary>
        ''' This method returns childs of a parent category in fotolia conceptual category system. 
        ''' This method could be used to display a part of the category system or the all tree. 
        ''' Fotolia categories system counts three levels.
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="language_id">The language id.</param>
        ''' <param name="id">The category parent id. (0 fo first level categories).</param>
        ''' <param name="acb">The async callback to manage results.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getCategories2")>
        Function BeginGetCategories2(apiKey As XmlRpc.Authentication,
                                     language_id As Integer,
                                     id As Integer,
                                     acb As AsyncCallback) As IAsyncResult

        ''' <summary>
        ''' This method returns childs of a parent category in fotolia conceptual category system. 
        ''' This method could be used to display a part of the category system or the all tree. 
        ''' Fotolia categories system counts three levels.
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="language_id">The language id.</param>
        ''' <param name="id">The category parent id. (0 fo first level categories).</param>
        ''' <param name="acb">The async callback to manage results</param>
        ''' <param name="state">The user state.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getCategories2")>
        Function BeginGetCategories2(apiKey As XmlRpc.Authentication,
                                     language_id As Integer,
                                     id As Integer,
                                     acb As AsyncCallback,
                                     state As Object) As IAsyncResult

        ''' <summary>
        ''' The end part of Begin method.
        ''' </summary>
        ''' <param name="iasr">The async result used to retrieve method results.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcEnd("xmlrpc.getCategories2")>
        Function EndGetCategories2(iasr As IAsyncResult) As XmlRpcStruct
#End Region

#Region "xmlrpc.getTag"


        ''' <summary>
        ''' This method returns most searched tag and most used tag on fotolia website. 
        ''' This method may help you to create a tags cloud. 
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="language_id">The language id.</param>
        ''' <param name="type">The type of tag searched ("Used" o "Searched"). 
        ''' Used : Most used tags to index media
        ''' Searched : Most used tags to search for media
        ''' </param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcMethod("xmlrpc.getTags")>
        Function GetTags(apiKey As XmlRpc.Authentication, language_id As Integer, type As String) As XmlRpcStruct()


        ''' <summary>
        ''' This method returns most searched tag and most used tag on fotolia website ion async mode. 
        ''' This method may help you to create a tags cloud. 
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="language_id">The language id.</param>
        ''' <param name="type">The type of tag searched ("Used" o "Searched"). 
        ''' Used : Most used tags to index media
        ''' Searched : Most used tags to search for media
        ''' </param>
        ''' <param name="acb">The async callback called when method will finish.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getTags")>
        Function BeginGetTags(apiKey As XmlRpc.Authentication, language_id As Integer, type As String, acb As AsyncCallback) As IAsyncResult

        ''' <summary>
        ''' This method returns most searched tag and most used tag on fotolia website ion async mode. 
        ''' This method may help you to create a tags cloud. 
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="language_id">The language id.</param>
        ''' <param name="type">The type of tag searched ("Used" o "Searched"). 
        ''' Used : Most used tags to index media
        ''' Searched : Most used tags to search for media
        ''' </param>
        ''' <param name="acb">The async callback called when method will finish.</param>
        ''' <param name="state">The user state</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getTags")>
        Function BeginGetTags(apiKey As XmlRpc.Authentication, language_id As Integer, type As String, acb As AsyncCallback, state As Object) As IAsyncResult

        ''' <summary>
        ''' The end part of Begin method.
        ''' </summary>
        ''' <param name="iasr">The async result used to retrieve method results.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcEnd("xmlrpc.getTags")>
        Function EndGetTags(iasr As IAsyncResult) As XmlRpcStruct()
#End Region

#Region "xmlrpc.getGalleries"


        ''' <summary>
        ''' This methods returns public galleries for a defined language 
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="language_id">The language id.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcMethod("xmlrpc.getGalleries")>
        Function GetGalleries(apiKey As XmlRpc.Authentication, language_id As Integer) As XmlRpcStruct()


        ''' <summary>
        ''' This methods returns public galleries for a defined language in async mode.
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="language_id">The language id.</param>
        ''' <param name="acb">The async callback called when method will finish.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getGalleries")>
        Function BeginGetGalleries(apiKey As XmlRpc.Authentication, language_id As Integer, acb As AsyncCallback) As IAsyncResult

        ''' <summary>
        ''' This methods returns public galleries for a defined language in async mode.
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="language_id">The language id.</param>
        ''' <param name="acb">The async callback called when method will finish.</param>
        ''' <param name="state">The user state</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getGalleries")>
        Function BeginGetGalleries(apiKey As XmlRpc.Authentication, language_id As Integer, acb As AsyncCallback, state As Object) As IAsyncResult

        ''' <summary>
        ''' The end part of Begin method.
        ''' </summary>
        ''' <param name="iasr">The async result used to retrieve method results.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcEnd("xmlrpc.getGalleries")>
        Function EndGetGalleries(iasr As IAsyncResult) As XmlRpcStruct()
#End Region

#Region "xmlrpc.getColors"
        ''' <summary>
        ''' This method returns childs of a parent colors in the Fotolia color scheme. 
        ''' If no parent is provided first level colors are returned. 
        ''' This method can be used to display color and subcolors for color search queries. 
        ''' </summary>
        ''' <param name="apiKey">The API key. You need a valid api key to use the API</param>
        ''' <param name="id">Parent color ID. If this field is not populated (=0) first level colors are returned.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcMethod("xmlrpc.getColors")>
        Function GetColors(apiKey As XmlRpc.Authentication, id As Integer) As XmlRpcStruct

        ''' <summary>
        ''' This method returns childs of a parent colors (in async mode) in the Fotolia color scheme. 
        ''' If no parent is provided first level colors are returned. 
        ''' This method can be used to display color and subcolors for color search queries. 
        ''' </summary>
        ''' <param name="apiKey">The API key. You need a valid api key to use the API</param>
        ''' <param name="id">Parent color ID. If this field is not populated (=0) first level colors are returned.</param>
        ''' <param name="acb">The async callback to manage results</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getColors")>
        Function BeginGetColors(apiKey As XmlRpc.Authentication, id As Integer, acb As AsyncCallback) As IAsyncResult

        ''' <summary>
        ''' This method returns childs of a parent colors (in async mode) in the Fotolia color scheme. 
        ''' If no parent is provided first level colors are returned. 
        ''' This method can be used to display color and subcolors for color search queries. 
        ''' </summary>
        ''' <param name="apiKey">The API key. You need a valid api key to use the API</param>
        ''' <param name="id">Parent color ID. If this field is not populated (=0) first level colors are returned.</param>
        ''' <param name="acb">The async callback to manage results</param>
        ''' <param name="state">The user state.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getColors")>
        Function BeginGetColors(apiKey As XmlRpc.Authentication, id As Integer, acb As AsyncCallback, state As Object) As IAsyncResult

        ''' <summary>
        ''' The end part of Begin method.
        ''' </summary>
        ''' <param name="iasr">The async result used to retrieve method results.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcEnd("xmlrpc.getColors")>
        Function EndGetColors(iasr As IAsyncResult) As XmlRpcStruct
#End Region

#Region "xmlrpc.getCountries"

        ''' <summary>
        ''' This method returns Fotolia list of countries. 
        ''' </summary>
        ''' <param name="apiKey">you need a valid api key to use the API.</param>
        ''' <param name="languageId">The language id.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcMethod("xmlrpc.getCountries")>
        Function GetCountries(apiKey As XmlRpc.Authentication, languageId As Integer) As XmlRpcStruct()

        ''' <summary>
        ''' This method returns Fotolia list of countries in async mode. 
        ''' </summary>
        ''' <param name="apiKey">you need a valid api key to use the API.</param>
        ''' <param name="languageId">The language id.</param>
        ''' <param name="acb">The async callback.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getCountries")>
        Function BeginGetCountries(apiKey As XmlRpc.Authentication, languageId As Integer, acb As AsyncCallback) As IAsyncResult

        ''' <summary>
        ''' This method returns Fotolia list of countries in async mode. 
        ''' </summary>
        ''' <param name="apiKey">you need a valid api key to use the API.</param>
        ''' <param name="languageId">The language id.</param>
        ''' <param name="acb">The async callback.</param>
        ''' <param name="state">The user state.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getCountries")>
        Function BeginGetCountries(apiKey As XmlRpc.Authentication, languageId As Integer, acb As AsyncCallback, state As Object) As IAsyncResult


        ''' <summary>
        ''' Ends the get countries.
        ''' </summary>
        ''' <param name="iasr"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcEnd("xmlrpc.getCountries")>
        Function EndGetCountries(iasr As IAsyncResult) As XmlRpcStruct()

#End Region

#Region "xmlrpc.getSearchResults"
        ''' <summary>
        ''' Gets the search results.
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="searchParams">The search params.</param>
        ''' <param name="result_columns">The result_columns.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcMethod("xmlrpc.getSearchResults")>
        Function GetSearchResults(apiKey As Authentication, searchParams As SearchRequest, result_columns As String()) As XmlRpcStruct

        ''' <summary>
        ''' Gets the search results in async mode.
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="searchParams">The search params.</param>
        ''' <param name="result_columns">The result_columns.</param>
        ''' <param name="acb">The async callback.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getSearchResults")>
        Function BeginGetSearchResults(apiKey As XmlRpc.Authentication, searchParams As SearchRequest,
                                       result_columns As String(), acb As AsyncCallback) As IAsyncResult

        ''' <summary>
        ''' Gets the search results in async mode.
        ''' </summary>
        ''' <param name="apiKey">The API key.</param>
        ''' <param name="searchParams">The search params.</param>
        ''' <param name="result_columns">The result_columns.</param>
        ''' <param name="acb">The async callback.</param>
        ''' <param name="state">The user state object.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcBegin("xmlrpc.getSearchResults")>
        Function BeginGetSearchResults(apiKey As XmlRpc.Authentication, searchParams As SearchRequest,
                                       result_columns As String(), acb As AsyncCallback, state As Object) As IAsyncResult

        ''' <summary>
        ''' Ends the get search results.
        ''' </summary>
        ''' <param name="iasr">The async result.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <XmlRpcEnd("xmlrpc.getSearchResults")>
        Function EndGetSearchResults(iasr As IAsyncResult) As XmlRpcStruct

#End Region
    End Interface

End Namespace