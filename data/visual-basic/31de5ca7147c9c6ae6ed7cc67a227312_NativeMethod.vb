' Written by Jonathan Dibble, Microsoft Corporation
' CODE IS PROVIDED AS-IS WITH NO WARRIENTIES EXPRESSED OR IMPLIED.

Imports System.Runtime.InteropServices
Namespace WMPRemote

#Region "Useful COM Enums"
    ''' <summary>
    ''' Represents a collection of frequently used HRESULT values.
    ''' You may add more HRESULT VALUES, I've only included the ones used 
    ''' in this project.
    ''' </summary>
    Public Enum HResults
        ''' <summary>
        ''' HRESULT S_OK
        ''' </summary>
        S_OK = CInt(&H0)
        ''' <summary>
        ''' HRESULT S_FALSE
        ''' </summary>
        S_FALSE = CInt(&H1)
        ''' <summary>
        ''' HRESULT E_NOINTERFACE
        ''' </summary>
        E_NOINTERFACE = CInt(&H80004002)
        ''' <summary>
        ''' HRESULT E_NOTIMPL
        ''' </summary>
        E_NOTIMPL = CInt(&H80004001)
    End Enum

    ''' <summary>
    ''' Enumeration for <see cref="IOleObject.GetMiscStatus"/>
    ''' </summary>
    Public Enum DVASPECT
        ''' <summary>
        ''' See MSDN for more information.
        ''' </summary>
        Content = 1
        ''' <summary>
        ''' See MSDN for more information.
        ''' </summary>
        Thumbnail = 2
        ''' <summary>
        ''' See MSDN for more information.
        ''' </summary>
        Icon = 3
        ''' <summary>
        ''' See MSDN for more information.
        ''' </summary>
        DocPrint = 4
    End Enum
#End Region

#Region "IWMPRemoteMediaServices"
    ''' <summary>
    ''' Interface used by Media Player to determine WMP Remoting status.
    ''' </summary>
    <ComImport(), ComVisible(True), InterfaceType(ComInterfaceType.InterfaceIsIUnknown), Guid("CBB92747-741F-44fe-AB5B-F1A48F3B2A59")> _
    Public Interface IWMPRemoteMediaServices

        ''' <summary>
        ''' Service type.
        ''' </summary>
        ''' <returns><code>Remote</code> if the control is to be remoted (attached to WMP.) 
        ''' <code>Local</code>if this is an independent WMP instance not connected to WMP application.  If you want local, you shouldn't bother
        ''' using this control!
        ''' </returns>
        Function GetServiceType() As <MarshalAs(UnmanagedType.BStr)> String

        ''' <summary>
        ''' Value to display in Windows Media Player Switch To Application menu option (under View.)
        ''' </summary>
        ''' <returns></returns>
        Function GetApplicationName() As <MarshalAs(UnmanagedType.BStr)> String

        ''' <summary>
        ''' Not in use, see MSDN for details.
        ''' </summary>
        ''' <param name="name"></param>
        ''' <param name="dispatch"></param>
        ''' <returns></returns>
        <PreserveSig()> _
        Function GetScriptableObject(<MarshalAs(UnmanagedType.BStr)> ByRef name As String, <MarshalAs(UnmanagedType.IDispatch)> ByRef dispatch As Object) As <MarshalAs(UnmanagedType.U4)> HResults

        ''' <summary>
        ''' Not in use, see MSDN for details.
        ''' </summary>
        ''' <param name="file"></param>
        ''' <returns></returns>
        <PreserveSig()> _
        Function GetCustomUIMode(<MarshalAs(UnmanagedType.BStr)> ByRef file As String) As <MarshalAs(UnmanagedType.U4)> HResults
    End Interface

#End Region

#Region "IOleServiceProvider"
    ''' <summary>
    ''' Interface used by Windows Media Player to return an instance of IWMPRemoteMediaServices.
    ''' </summary>
    <ComImport(), GuidAttribute("6d5140c1-7436-11ce-8034-00aa006009fa"), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown), ComVisible(True)> _
    Public Interface IOleServiceProvider
        ''' <summary>
        ''' Similar to QueryInterface, riid will contain the Guid of an object to return.
        ''' In our project we will look for <see cref="IWMPRemoteMediaServices"/> Guid and return the object
        ''' that implements that interface.
        ''' </summary>
        ''' <param name="guidService"></param>
        ''' <param name="riid">The Guid of the desired Service to provide.</param>
        ''' <returns>A pointer to the interface requested by the Guid.</returns>
        Function QueryService(ByRef guidService As Guid, ByRef riid As Guid) As IntPtr
    End Interface

    ''' <summary>
    ''' This is an example of an INCORRECT entry - do not use, unless you want your app to break.
    ''' </summary>
    <ComImport(), GuidAttribute("6d5140c1-7436-11ce-8034-00aa006009fa"), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown), ComVisible(True)> _
    Public Interface BadIOleServiceProvider
        ''' <summary>
        ''' This is incorrect because it causes our return interface to be boxed
        ''' as an object and a COM callee may not get the correct pointer.
        ''' </summary>
        ''' <param name="guidService"></param>
        ''' <param name="riid"></param>
        ''' <returns></returns>
        ''' <example>
        ''' For an example of a correct definition, look at <see cref="IOleServiceProvider"/>.
        ''' </example>
        Function QueryService(ByRef guidService As Guid, ByRef riid As Guid) As <MarshalAs(UnmanagedType.[Interface])> Object
    End Interface
#End Region

#Region "IOleClientSite"
    ''' <summary>
    ''' Need to implement this interface so we can pass it to <see cref="IOleObject.SetClientSite"/>.
    ''' All functions return E_NOTIMPL.  We don't need to actually implement anything to get
    ''' the remoting to work.
    ''' </summary>
    <ComImport(), ComVisible(True), Guid("00000118-0000-0000-C000-000000000046"), InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>
    Public Interface IOleClientSite
        ''' <summary>
        ''' See MSDN for more information.  Throws <see cref="COMException"/> with id of E_NOTIMPL.
        ''' </summary>
        ''' <exception cref="COMException">E_NOTIMPL</exception>
        Sub SaveObject()

        ''' <summary>
        ''' See MSDN for more information.  Throws <see cref="COMException"/> with id of E_NOTIMPL.
        ''' </summary>
        ''' <exception cref="COMException">E_NOTIMPL</exception>
        Function GetMoniker(dwAssign As UInteger, dwWhichMoniker As UInteger) As <MarshalAs(UnmanagedType.[Interface])> Object

        ''' <summary>
        ''' See MSDN for more information.  Throws <see cref="COMException"/> with id of E_NOTIMPL.
        ''' </summary>
        ''' <exception cref="COMException">E_NOTIMPL</exception>
        Function GetContainer() As <MarshalAs(UnmanagedType.Interface)> Object

        ''' <summary>
        ''' See MSDN for more information.  Throws <see cref="COMException"/> with id of E_NOTIMPL.
        ''' </summary>
        ''' <exception cref="COMException">E_NOTIMPL</exception>
        Sub ShowObject()

        ''' <summary>
        ''' See MSDN for more information.  Throws <see cref="COMException"/> with id of E_NOTIMPL.
        ''' </summary>
        ''' <exception cref="COMException">E_NOTIMPL</exception>
        Sub OnShowWindow(fShow As Boolean)

        ''' <summary>
        ''' See MSDN for more information.  Throws <see cref="COMException"/> with id of E_NOTIMPL.
        ''' </summary>
        ''' <exception cref="COMException">E_NOTIMPL</exception>
        Sub RequestNewObjectLayout()
    End Interface
#End Region

#Region "IOleObject"
    ''' <summary>
    ''' This interface is implemented by WMP ActiveX/COM control.
    ''' The only function we need is <see cref="IOleObject.SetClientSite"/>.
    ''' </summary>
    <ComImport(), ComVisible(True), Guid("00000112-0000-0000-C000-000000000046"), InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>
    Public Interface IOleObject
        ''' <summary>
        ''' Used to pass our custom <see cref="IOleClientSite"/> object to WMP.  The object we pass must also
        ''' implement <see cref="IOleServiceProvider"/> to work right.
        ''' </summary>
        ''' <param name="pClientSite">The <see cref="IOleClientSite"/> to pass.</param>
        Sub SetClientSite(pClientSite As IOleClientSite)

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Function GetClientSite() As <MarshalAs(UnmanagedType.Interface)> IOleClientSite

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Sub SetHostNames(<MarshalAs(UnmanagedType.LPWStr)> szContainerApp As String, <MarshalAs(UnmanagedType.LPWStr)> szContainerObj As String)

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Sub Close(dwSaveOption As UInteger)

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Sub SetMoniker(dwWhichMoniker As UInteger, pmk As Object)

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Function GetMoniker(dwAssign As UInteger, dwWhichMoniker As UInteger) As <MarshalAs(UnmanagedType.Interface)> Object

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Sub InitFromData(pDataObject As Object, fCreation As Boolean, dwReserved As UInteger)

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Function GetClipboardData(dwReserved As UInteger) As Object


        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Sub DoVerb(iVerb As UInteger, lpmsg As UInteger, <MarshalAs(UnmanagedType.Interface)> pActiveSite As Object, lindex As UInteger, hwndParent As UInteger, lprcPosRect As UInteger)

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Function EnumVerbs() As <MarshalAs(UnmanagedType.Interface)> Object

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Sub Update()

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        <PreserveSig()> _
        Function IsUpToDate() As <MarshalAs(UnmanagedType.U4)> HResults

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Function GetUserClassID() As Guid

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Function GetUserType(dwFormOfType As UInteger) As <MarshalAs(UnmanagedType.LPWStr)> String

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Sub SetExtent(dwDrawAspect As UInteger, <MarshalAs(UnmanagedType.Interface)> psizel As Object)

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Function GetExtent(dwDrawAspect As UInteger) As <MarshalAs(UnmanagedType.Interface)> Object

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Function Advise(<MarshalAs(UnmanagedType.Interface)> pAdvSink As Object) As UInteger

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Sub Unadvise(dwConnection As UInteger)

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Function EnumAdvise() As <MarshalAs(UnmanagedType.Interface)> Object

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Function GetMiscStatus(<MarshalAs(UnmanagedType.U4)> dwAspect As DVASPECT) As UInteger

        ''' <summary>
        ''' Implemented by Windows Media Player ActiveX control.
        ''' See MSDN for more information.
        ''' </summary>
        Sub SetColorScheme(<MarshalAs(UnmanagedType.Interface)> pLogpal As Object)
    End Interface
#End Region

End Namespace
