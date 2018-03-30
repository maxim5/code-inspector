#Region "import"
Imports System.IO
Imports Microsoft.Win32
Imports System.Diagnostics
Imports System.Runtime.InteropServices
Imports System.Data.SqlClient
Imports System.Data
Imports System.Data.OleDb
#End Region

Public Class RegisterRF

#Region "load"
    Public Sub masuk()
        Try
            Dim com As New SqlClient.SqlCommand("insert into tbl_rfaccount(id,password, accounttype, birthdate) values ((CONVERT(binary(13),'" & Me.nid.Text & "')),(CONVERT(binary(13),'" & Me.npass.Text & "')),1,'" & Me.nbirth.Text & "')", ConnUser.conuserc)
            com.Connection.Open()
            com.ExecuteNonQuery()
            com.Connection.Close()
            sdbuser = New SqlClient.SqlDataAdapter(com)

        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Exclamation, " ERROR ")
        End Try
    End Sub

    Private Sub Register_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Try
            ConnUser.conuserc.Open()
            MsgBox("Connection Open ! ")
            ConnUser.conuserc.Close()
            nok.Text = "Submit"
        Catch ex As Exception
            MsgBox("Can not open connection ! ")
            nok.Text = "Fail"
        End Try
    End Sub
#End Region

#Region "command"
    Private Sub nok_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles nok.Click
        If nok.Text = "Fail" Then
            AIOTFRF.AllInOneTools.ShowBalloonTip(500, "Warning", "SQL Not Connected", ToolTipIcon.Info)
            Close()
        ElseIf nok.Text = "Submit" Then
            AIOTFRF.AllInOneTools.ShowBalloonTip(500, "Warning", "SQL Connected!Ready For Register", ToolTipIcon.Info)
            If Me.nid.Text = "" Then
                MsgBox("ID Not Fill", MsgBoxStyle.Exclamation, "ID")
                Me.nid.Focus()
            ElseIf Me.npass.Text = "" Then
                MsgBox("Pass Not Fill", MsgBoxStyle.Exclamation, "Password")
                Me.npass.Focus()
            ElseIf Me.nbirth.Text = "" Then
                MsgBox("BirthDay Not Fill(YYYY-MM-DD)", MsgBoxStyle.Exclamation, "Birthday")
                Me.nbirth.Focus()
            Else
                masuk()
                MsgBox("Save Success!!", MsgBoxStyle.Exclamation, "Success!!!")
            End If
            Close()
        End If
    End Sub

    Private Sub btncancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btncancel.Click
        Close()
    End Sub
#End Region

End Class