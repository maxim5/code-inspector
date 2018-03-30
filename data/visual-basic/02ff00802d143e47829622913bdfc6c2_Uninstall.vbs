
Call DeleteWebFolder( "Updater2QuickStarts" )

WScript.Echo "QuickStarts Uninstalled"

'Create a Web folder 
Sub DeleteWebFolder( folderName )
  Dim vRoot, vDir, tempDir

  Set vRoot = GetObject("IIS://localhost/W3svc/1/Root")
  Set vDir = Nothing
	
  For Each tempDir In vRoot
    If tempDir.Name = folderName Then Set vDir = tempDir
  Next

  If Not ( vDir Is Nothing ) Then
    vDir.AppDelete
    vRoot.Delete "IIsWebVirtualDir", folderName
  End If
	
End Sub
