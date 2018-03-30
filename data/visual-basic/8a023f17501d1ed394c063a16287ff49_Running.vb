#Region "import"
Imports System.IO
Imports Microsoft.Win32
Imports System.Diagnostics
Imports System.Runtime.InteropServices
Imports System.Data.SqlClient
Imports System.Data
Imports System.Data.OleDb
Imports System.Net
Imports System.Net.Mail
Imports System.Text
Imports System.Reflection
Imports System.ComponentModel
Imports System.Threading
Imports System.Threading.Tasks
Imports AIOTClient
#End Region

Public Class Running

#Region "link"
    Sub forums()
        'Jalankan browser web default dan link ke situs web
        System.Diagnostics.Process.Start _
            ("http://o2psoftlabsforums.8forum.net/t37-autopatcherupdate-feature")
    End Sub
#End Region

#Region "donate"
    Sub paypal()
        'Jalankan browser web default dan link ke situs web
        System.Diagnostics.Process.Start _
            ("https://www.paypal.com/cgi-bin/webscr")
    End Sub

    Sub moneybookers()
        'Jalankan browser web default dan link ke situs web
        System.Diagnostics.Process.Start _
            ("https://www.moneybookers.com/app/")
    End Sub

    Sub bri()
        'Jalankan browser web default dan link ke situs web
        System.Diagnostics.Process.Start _
            ("http://www.bri.co.id/")
    End Sub
#End Region

#Region "process"
    Sub RFInalterable()
        Try
            Dim myProcess As Process = System.Diagnostics.Process.Start("Explorer.exe")
            MsgBox(myProcess.ProcessName)
        Catch except As Exception
        End Try
    End Sub

    Sub Help()
        Try
            Dim myProcess As Process = System.Diagnostics.Process.Start("Help\AIOTCHelp.chm")
            MsgBox(myProcess.ProcessName)
        Catch except As Exception
        End Try
    End Sub

    Sub Openinal()
        Try
            Dim myProcess As Process = System.Diagnostics.Process.Start("RF\AIOTRF.exe")
            MsgBox(myProcess.ProcessName)
        Catch except As Exception
        End Try
    End Sub
#End Region

End Class
