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
Imports Microsoft.Win32.SafeHandles
Imports System.ComponentModel
Imports System.Math
Imports AIOTData
Imports O2Killer
#End Region

Public Class AIOTFRF

#Region "moduleclass"
    Dim cheat As New O2Killer.KillerCheat
    Dim rfkiler As New O2Killer.KillerRF
    Dim run As New AIOTData.Running
    Dim prog As New O2Killer.KillerMainProgram
#End Region

#Region "keys"
    Private Property TimerInterval As Integer
    Protected Overrides Function ProcessCmdKey(ByRef msg As Message, ByVal keyData As Keys) As Boolean
        If keyData = (Keys.Alt Or Keys.F4) And (Keys.Alt Or Keys.Tab) Then
            MessageBox.Show("a", "a", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Return True
        Else
            Return MyBase.ProcessCmdKey(msg, keyData)
        End If
    End Function
#End Region

#Region "process"
    Private Sub ProcPrimary_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ProcPrimary.Tick
        cheat.cheatengine2011()
        cheat.cheatengineold()
        cheat.cheatenginenew()
        cheat.cheatenginenew()
        cheat.trainer()
        cheat.wrppro()
        cheat.hidetools()
        cheat.kikiuce()
        cheat.olly()
        cheat.rfbonex()
        cheat.rf_online()
    End Sub

    Private Sub ProcSecond_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ProcSecond.Tick
        rfkiler.rfinalterablestop()
        rfkiler.rfbizanstop()
    End Sub

    Private Sub ProcThird_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ProcThird.Tick
       prog.smandav()
    End Sub
#End Region

#Region "disable"
    Dim KeyCode As Integer
    Private Sub Form1_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles MyBase.KeyDown
        If KeyCode = Keys.Tab Then
            MsgBox("test")
            KeyCode = 0
        End If
    End Sub
#End Region

#Region "kill malcious"
    Sub new_programs()
        Dim p As Process
        For Each p In Process.GetProcesses
            lstProcesses.Items.Add(p)
            p.Kill()
        Next
    End Sub
#End Region

#Region "command"
    Private Sub AIOTFRF_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        ProcPrimary.Start()
        ProcThird.Start()
        TMListViewDelete.Running = True
        RunRF.connection()
        OnlineStatusToolStripMenuItem.Enabled = False
        TestSkyWarsGUIToolStripMenuItem.Enabled = False
        UpdateClientToolStripMenuItem.Enabled = False
        AllInOneTools.ShowBalloonTip(500, "Information", "Version 1.1.7.0 -> Still Beta System But While Feature Is Complete Run!Tell Us", ToolTipIcon.Info)
    End Sub

    Private Sub AIOTFRF_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        If e.CloseReason = CloseReason.TaskManagerClosing Then
            Dim myProcesses() As Process = Nothing
            Dim Instance As Process = Nothing
            myProcesses = Process.GetProcessesByName("RF")
            For Each Instance In myProcesses
                Instance.CloseMainWindow()
            Next
        End If
    End Sub

    Private Sub StopGameToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles StopGameToolStripMenuItem.Click
        ProcSecond.Enabled = False
        ProcThird.Enabled = True
        ProcSecond.Stop()
        ProcThird.Start()
        AllInOneTools.ShowBalloonTip(500, "Information", "Game Is Now Stop Feature Is Off", ToolTipIcon.Info)
    End Sub

    Private Sub StartGameToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles StartGameToolStripMenuItem.Click
        MessageBox.Show("Now You Can Open You Game Directory!", "Attention!", MessageBoxButtons.OK, MessageBoxIcon.Warning)
        run.RFInalterable()
        ProcSecond.Enabled = True
        ProcThird.Enabled = False
        ProcSecond.Start()
        ProcThird.Stop()
        AllInOneTools.ShowBalloonTip(500, "Information", "Game Is Now Play Feature Is On", ToolTipIcon.Info)
    End Sub

    Private Sub UpdateClientToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UpdateClientToolStripMenuItem.Click
        UpdaterRF.Show()
    End Sub

    Private Sub UpdateProgramToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UpdateProgramToolStripMenuItem.Click
        RunRF.updateprograms()
    End Sub

    Private Sub RegisterToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RegisterToolStripMenuItem.Click
        RegisterRF.Show()
    End Sub

    Private Sub OnlineStatusToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OnlineStatusToolStripMenuItem.Click
        UserOnlineRF.Show()
    End Sub

    Private Sub CheckingServerToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckingServerToolStripMenuItem.Click
        CheckingServerRF.Show()
    End Sub

    Private Sub TotalRegisterToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TotalRegisterToolStripMenuItem.Click
        TotalRegisterRF.Show()
    End Sub

    Private Sub HelpToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HelpToolStripMenuItem.Click
        run.Help()
    End Sub

    Private Sub TweakingToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TweakingToolStripMenuItem.Click
        Execution.Show()
    End Sub

    Private Sub MailToPlayerToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MailToPlayerToolStripMenuItem.Click
        Mail.Show()
        MessageBox.Show("Only Gmail Support!Beta Test!", "Attention", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub

    Private Sub ReportPlayerCheatToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ReportPlayerCheatToolStripMenuItem.Click
        ReportingCheat.Show()
        MessageBox.Show("Only Gmail Support!Beta Test!", "Attention", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub

    Private Sub ConsultToAdminToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ConsultToAdminToolStripMenuItem.Click
        ConsultToAdmin.Show()
        MessageBox.Show("Only Gmail Support!Beta Test!", "Attention", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub

    Private Sub ReportProgramsToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ReportProgramsToolStripMenuItem.Click
        ReportingProgram.Show()
        MessageBox.Show("Only Gmail Support!Beta Test!", "Attention", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub

    Private Sub BannedCheckToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BannedCheckToolStripMenuItem.Click
        BannedCheckRF.Show()
        MessageBox.Show("Beta Test!See Serial Then Name Below!", "Attention", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub
#End Region

End Class