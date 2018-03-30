Imports MyDLL

Public Class Form4

    Dim Nav As New MyDLL.RecordNav

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Dim C As String = TextBox7.Text

        If CheckBox1.Checked Then
            Form1.RichTextBox1.Text = Form1.RichTextBox1.Text & "<TimeTable>" & vbCr _
                                      & "<Type>School Timetable</Type>" & vbCr
        End If

        If CheckBox2.Checked Then
            Form1.RichTextBox2.Text = Form1.RichTextBox2.Text & _
            "<FlowDocument xmlns=" & C & "http://schemas.microsoft.com/winfx/2006/xaml/presentation" & C & ">" & vbCr
        End If
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        Dim Count As String = TextBox6.Text

        If CheckBox1.Checked Then
            Dim Counter As XElement = _
            <Counter>
                <Count><%= Count %></Count>
            </Counter>

            Form1.RichTextBox1.Text = Form1.RichTextBox1.Text & Counter.ToString & vbCr & "</TimeTable>"
        End If

        If CheckBox2.Checked Then
            Form1.RichTextBox2.Text = Form1.RichTextBox2.Text & "</FlowDocument>"
        End If
    End Sub

    Private Sub WriteData()
        Dim ID As Integer = TextBox6.Text
        Dim Day As String = TextBox1.Text
        Dim Lesson1 As String = TextBox2.Text
        Dim Lesson2 As String = TextBox3.Text
        Dim Lesson3 As String = TextBox4.Text
        Dim Lesson4 As String = TextBox5.Text
        Dim Lesson5 As String = TextBox8.Text
        Dim Lesson6 As String = TextBox9.Text
        Dim Lesson7 As String = TextBox10.Text
        Dim Lesson8 As String = TextBox11.Text
        Dim AfS As String = TextBox12.Text

        If CheckBox3.Checked Then
            ID = ID + 1
            TextBox6.Text = ID
        End If

        If CheckBox1.Checked Then
            Dim Dayxml As XElement = _
            <Day>
                <Day id=<%= ID %>><%= Day %></Day>
                <Lesson1 id=<%= ID %>><%= Lesson1 %></Lesson1>
                <Lesson2 id=<%= ID %>><%= Lesson2 %></Lesson2>
                <Lesson3 id=<%= ID %>><%= Lesson3 %></Lesson3>
                <Lesson4 id=<%= ID %>><%= Lesson4 %></Lesson4>
                <Lesson5 id=<%= ID %>><%= Lesson5 %></Lesson5>
                <Lesson6 id=<%= ID %>><%= Lesson6 %></Lesson6>
                <Lesson7 id=<%= ID %>><%= Lesson7 %></Lesson7>
                <Lesson8 id=<%= ID %>><%= Lesson8 %></Lesson8>
                <AfS id=<%= ID %>><%= AfS %></AfS>
            </Day>

            Form1.RichTextBox1.Text = Form1.RichTextBox1.Text & Dayxml.ToString & vbCr

        End If

        If CheckBox2.Checked Then

            Dim Dayxaml As XElement = _
            <List FontFamily="Arial" FontSize="11pt">
                <ListItem><Paragraph>ID:<%= ID %></Paragraph></ListItem>
                <ListItem><Paragraph>Day:<%= Day %></Paragraph></ListItem>
                <ListItem><Paragraph>Lesson 1:<%= Lesson1 %></Paragraph></ListItem>
                <ListItem><Paragraph>Lesson 2:<%= Lesson2 %></Paragraph></ListItem>
                <ListItem><Paragraph>Lesson 3:<%= Lesson3 %></Paragraph></ListItem>
                <ListItem><Paragraph>Lesson 4:<%= Lesson4 %></Paragraph></ListItem>
                <ListItem><Paragraph>Lesson 5:<%= Lesson5 %></Paragraph></ListItem>
                <ListItem><Paragraph>Lesson 6:<%= Lesson6 %></Paragraph></ListItem>
                <ListItem><Paragraph>Lesson 7:<%= Lesson7 %></Paragraph></ListItem>
                <ListItem><Paragraph>Lesson 8:<%= Lesson8 %></Paragraph></ListItem>
                <ListItem><Paragraph>After School:<%= AfS %></Paragraph></ListItem>
            </List>

            Form1.RichTextBox2.Text = Form1.RichTextBox2.Text & Dayxaml.ToString & vbCr
        End If
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        WriteData()

    End Sub

    Private Sub LoadData()
        Try
            Me.Text = "School Timetable Database" & " - " & Form1.TextBox3.Text

            Dim ID As String = TextBox6.Text

            Dim TimeTable As XElement = XElement.Load(Form1.TextBox7.Text & Form1.TextBox3.Text & ".xml")

            Dim Day As XElement = _
                <Root>
                    <%= From el In TimeTable.<Day>.<Day> _
                        Where el.Attribute("id") = ID _
                        Select el %>
                </Root>
            TextBox1.Text = Day

            Dim Lesson1 As XElement = _
                 <Root>
                     <%= From el In TimeTable.<Day>.<Lesson1> _
                         Where el.Attribute("id") = ID _
                         Select el %>
                 </Root>
            TextBox2.Text = Lesson1

            Dim Lesson2 As XElement = _
         <Root>
             <%= From el In TimeTable.<Day>.<Lesson2> _
                 Where el.Attribute("id") = ID _
                 Select el %>
         </Root>
            TextBox3.Text = Lesson2

            Dim Lesson3 As XElement = _
         <Root>
             <%= From el In TimeTable.<Day>.<Lesson3> _
                 Where el.Attribute("id") = ID _
                 Select el %>
         </Root>
            TextBox4.Text = Lesson3

            Dim Lesson4 As XElement = _
         <Root>
             <%= From el In TimeTable.<Day>.<Lesson4> _
                 Where el.Attribute("id") = ID _
                 Select el %>
         </Root>
            TextBox5.Text = Lesson4

            Dim Lesson5 As XElement = _
         <Root>
             <%= From el In TimeTable.<Day>.<Lesson5> _
                 Where el.Attribute("id") = ID _
                 Select el %>
         </Root>
            TextBox8.Text = Lesson5

            Dim Lesson6 As XElement = _
         <Root>
             <%= From el In TimeTable.<Day>.<Lesson6> _
                 Where el.Attribute("id") = ID _
                 Select el %>
         </Root>
            TextBox9.Text = Lesson6

            Dim Lesson7 As XElement = _
         <Root>
             <%= From el In TimeTable.<Day>.<Lesson7> _
                 Where el.Attribute("id") = ID _
                 Select el %>
         </Root>
            TextBox10.Text = Lesson7

            Dim Lesson8 As XElement = _
         <Root>
             <%= From el In TimeTable.<Day>.<Lesson8> _
                 Where el.Attribute("id") = ID _
                 Select el %>
         </Root>
            TextBox11.Text = Lesson8

            Dim AfS As XElement = _
         <Root>
             <%= From el In TimeTable.<Day>.<AfS> _
                 Where el.Attribute("id") = ID _
                 Select el %>
         </Root>
            TextBox12.Text = AfS

            Dim Count As XElement = _
       <Root>
           <%= From el In TimeTable.<Counter>.<Count> _
               Select el %>
       </Root>

            Dim Counter As String = Count

            Label7.Text = "There are " & Counter & " record(s) in the XML file"
        Catch ex As Exception
            MsgBox("No file has been loaded.")
        End Try
    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        LoadData()

    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        Try
            Do Until TextBox6.Text = TextBox13.Text
                CheckBox3.CheckState = CheckState.Unchecked
                Dim ID As Integer = TextBox6.Text
                ID = ID + 1
                TextBox6.Text = ID
                LoadData()
                WriteData()
            Loop
        Catch ex As Exception
            MsgBox("Choose a proper range.")
        End Try
    End Sub

    Private Sub ToolStripButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton1.Click
        Nav.GoToFirst(TextBox6)

        LoadData()
    End Sub

    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click
        Nav.PrevRec(TextBox6)

        LoadData()
    End Sub

    Private Sub ToolStripButton3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton3.Click
        Nav.NextRec(TextBox6)

        LoadData()
    End Sub

    Private Sub ToolStripButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton4.Click
        Nav.GoToLast(TextBox6, Form1.TextBox7.Text & Form1.TextBox3.Text & ".xml")

        LoadData()
    End Sub
End Class