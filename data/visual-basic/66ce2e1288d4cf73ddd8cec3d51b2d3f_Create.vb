Imports System.Net
Public Class Create

    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        Timer1.Start()
        Button8.Enabled = False
        Dim startInfo As New ProcessStartInfo(Main.TextBox1.Text)
        startInfo.WindowStyle = ProcessWindowStyle.Minimized
        Process.Start(startInfo)
        Main.TextBox3.Text = ComboBox2.Text()
        Main.TextBox4.Text = ComboBox1.Text()
        Main.TextBox5.Text = TextBox1.Text()
        Main.TextBox6.Text = TextBox2.Text()
        Main.Button2.Enabled = True
        Main.TextBox2.Enabled = True
        Main.TextBox8.Enabled = True
        Main.Button7.Enabled = True
        Dim getIP As String = New WebClient().DownloadString("http://automation.whatismyip.com/n09230945.asp")
        Main.TextBox7.Text = getIP
    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        ProgressBar1.Increment(1)

        If ProgressBar1.Value = 5 Then
            SendKeys.Send(ComboBox2.Text)
            SendKeys.Send("{ENTER}")
        ElseIf ProgressBar1.Value = 7 Then
            SendKeys.Send(ComboBox1.Text)
            SendKeys.Send("{ENTER}")
        ElseIf ProgressBar1.Value = 9 Then
            SendKeys.Send(TextBox1.Text)
            SendKeys.Send("{ENTER}")
        ElseIf ProgressBar1.Value = 11 Then
            SendKeys.Send(TextBox2.Text)
            SendKeys.Send("{ENTER}")
            Main.ListBox1.Items.Add("(" + DateTime.Now & ") - Creating Server...")
        ElseIf ProgressBar1.Value = 100 Then
            ProgressBar1.Value = 0
            Timer1.Stop()
            Main.ListBox1.Items.Add("(" + DateTime.Now & ") - Server Created Successfully!")
            Main.Button3.Enabled = True
            Main.Button4.Enabled = True
            Main.ListBox2.Enabled = True
            Main.ListBox3.Enabled = True
            Button8.Enabled = True
            Me.Hide()
        End If
    End Sub
End Class