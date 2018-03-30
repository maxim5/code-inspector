
Call CreateWebFolder( GetCurrentPath(), "Updater2QuickStarts" )

WScript.Echo "QuickStarts Installed"

'Create a Web folder 
Sub CreateWebFolder( folderPath, folderName )
  Dim vRoot, vDir, tempDir

  Set vRoot = GetObject("IIS://localhost/W3svc/1/Root")
  Set vDir = Nothing
	
  For Each tempDir In vRoot
    If tempDir.Name = folderName Then Set vDir = tempDir
  Next

  If vDir Is Nothing Then
    Set vDir = vRoot.Create("IIsWebVirtualDir", folderName)
  End If
	
  vDir.AccessRead = true
  vDir.Path = folderPath
	
  vDir.DirBrowseFlags = &H4000003E     
	
  '  BITS likes dir browse        
  vDir.EnableDirBrowsing = True
  'vDir.AppCreate( true )
  vDir.AccessScript = False
	

  ' Add the needed mime types to the virtual dir
	Dim MimeMaps(1)
	Set aMimeMap = CreateObject("MimeMap")
	aMimeMap.Extension = ".msp"
	aMimeMap.MimeType = "application/octet-stream"
	set MimeMaps(0) = aMimeMap
	Set aMimeMap = CreateObject("MimeMap")
	aMimeMap.Extension = ".exe"
	aMimeMap.MimeType = "application/octet-stream"
	set MimeMaps(1) = aMimeMap
	vDir.PutEx 2, "MimeMap", MimeMaps
	
	vDir.SetInfo
End Sub

Function GetCurrentPath()
  Dim fso
  
  Set fso = CreateObject("Scripting.FileSystemObject")
  GetCurrentPath = fso.GetAbsolutePathName( "." )
  
  Set fso = Nothing

End Function
