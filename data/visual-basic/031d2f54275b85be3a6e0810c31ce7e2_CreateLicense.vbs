' Create license.wxp to be included into the MSI
'                                                         -- NelsonA

Dim licfs
Dim licfile
Dim licout
Dim line

set licfs = CreateObject("Scripting.FileSystemObject")
set licfile = licfs.OpenTextFile("license.rtf", 1)
set licout = licfs.OpenTextFile("license.wxm", 2, -1)

licout.Write "<Text><![CDATA["
licout.Write licfile.ReadAll
licout.Write "]]></Text>"

set licfile = Nothing
set licout = Nothing
set licfs = Nothing
