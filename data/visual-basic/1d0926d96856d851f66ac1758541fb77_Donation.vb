#Region "import"
Imports System.IO
Imports Microsoft.Win32
Imports System.Diagnostics
Imports System.Runtime.InteropServices
Imports System.Net
Imports System.Net.Mail
Imports System.Text
Imports System.Reflection
Imports Microsoft.Win32.SafeHandles
Imports System.ComponentModel
Imports System.Math
Imports AIOTData
Imports O2Killer
#End Region

Public Class Donation

#Region "list add variable"
    Dim run As New AIOTData.Running
#End Region

#Region "command"
    Private Sub paypal_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles paypal.Click
        run.paypal()
    End Sub

    Private Sub skrill_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles skrill.Click
        run.moneybookers()
    End Sub

    Private Sub bri_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles bri.Click
        run.bri()
    End Sub

    Private Sub btnclose_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnclose.Click
        Close()
    End Sub
#End Region

End Class