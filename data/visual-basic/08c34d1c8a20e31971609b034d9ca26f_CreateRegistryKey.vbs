'----------------------------------------------------------'
'                     Create Registry Key                  '
'  Imput full registry key path and this will generate it  '
'----------------------------------------------------------'

Function CreateRegKey(sKey)
	Dim wshShell
	Set wshShell = CreateObject( "WScript.Shell" )
	
	wshShell.RegWrite sKey, ""
	WriteLog("Created Registry Key... " & sKey)
	
	Set wshShell = nothing

End Function