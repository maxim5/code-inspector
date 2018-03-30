' ========================================================================
' EveHQ - An Eve-Online™ character assistance application
' Copyright © 2005-2012  EveHQ Development Team
' 
' This file is part of EveHQ.
'
' EveHQ is free software: you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation, either version 3 of the License, or
' (at your option) any later version.
'
' EveHQ is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.
'
' You should have received a copy of the GNU General Public License
' along with EveHQ.  If not, see <http://www.gnu.org/licenses/>.
'=========================================================================
Imports System.Windows.Forms
Imports DevComponents.AdvTree
Imports DevComponents.DotNetBar

Public Class frmRequiredSkills

    Dim TrainedSkillStyle As ElementStyle
    Dim HQFSkillStyle As ElementStyle
    Dim NotTrainedSkillStyle As ElementStyle
    Dim FittingName As String

#Region "Property Variables"
    Private reqSkills As New ArrayList
    Private reqPilot As EveHQ.Core.Pilot
    Private reqHPilot As HQFPilot
    Private SkillList As New SortedList(Of String, Integer)
#End Region

#Region "Properties"

    Private WriteOnly Property ForceUpdate() As Boolean
        Set(ByVal value As Boolean)
            If value = True Then
                HQFEvents.StartUpdateShipInfo = reqPilot.Name
            End If
        End Set
    End Property

    Public Property Skills() As ArrayList
        Get
            Return reqSkills
        End Get
        Set(ByVal value As ArrayList)
            reqSkills = value
            Call Me.DrawSkillsTable()
        End Set
    End Property

    Public Property Pilot() As EveHQ.Core.Pilot
        Get
            Return reqPilot
        End Get
        Set(ByVal value As EveHQ.Core.Pilot)
            reqPilot = value
            reqHPilot = CType(HQFPilotCollection.HQFPilots(reqPilot.Name), HQFPilot)
            Me.Text = "Required Skills - " & reqPilot.Name
        End Set
    End Property

#End Region

#Region "Form Constructor"

    Public Sub New(Fitting As String)

        ' This call is required by the designer.
        InitializeComponent()

        ' Set Fitting Name
        FittingName = Fitting

        ' Set Styles
        TrainedSkillStyle = adtSkills.Styles("Skill").Copy
        HQFSkillStyle = adtSkills.Styles("Skill").Copy
        NotTrainedSkillStyle = adtSkills.Styles("Skill").Copy
        TrainedSkillStyle.TextColor = Drawing.Color.LimeGreen
        HQFSkillStyle.TextColor = Drawing.Color.Orange
        NotTrainedSkillStyle.TextColor = Drawing.Color.Red
    End Sub

#End Region

#Region "Skill Display Routines"

    Private Sub DrawSkillsTable()
        Dim aSkill As EveHQ.Core.PilotSkill
        Dim hSkill As HQFSkill
        Dim aLevel As Integer = 0

        ' Compress the list of required skills into the smallest possible list
        Dim newSkills As New SortedList
        For Each rSkill As ReqSkill In reqSkills
            If newSkills.Contains(rSkill.Name & " (Lvl " & rSkill.ReqLevel & ") - " & rSkill.NeededFor) = False Then
                newSkills.Add(rSkill.Name & " (Lvl " & rSkill.ReqLevel & ") - " & rSkill.NeededFor, rSkill)
            End If
        Next

        ' Draw the list
        adtSkills.BeginUpdate()
        adtSkills.Nodes.Clear()
        For Each rSkill As ReqSkill In newSkills.Values
            Dim newSkill As New Node
            newSkill.Text = rSkill.Name
            newSkill.Cells.Add(New Cell(rSkill.ReqLevel.ToString))
            If SkillList.ContainsKey(rSkill.Name) = False Then
                SkillList.Add(rSkill.Name, rSkill.ReqLevel)
            Else
                If SkillList(rSkill.Name) < rSkill.ReqLevel Then
                    SkillList(rSkill.Name) = rSkill.ReqLevel
                End If
            End If
            If reqPilot.PilotSkills.Contains(rSkill.Name) = True Then
                aSkill = CType(reqPilot.PilotSkills(rSkill.Name), Core.PilotSkill)
                newSkill.Cells.Add(New Cell(aSkill.Level.ToString))
            Else
                newSkill.Cells.Add(New Cell("0"))
            End If
            If reqHPilot.SkillSet.Contains(rSkill.Name) = True Then
                hSkill = CType(reqHPilot.SkillSet(rSkill.Name), HQFSkill)
                newSkill.Cells.Add(New Cell(hSkill.Level.ToString))
            Else
                newSkill.Cells.Add(New Cell("0"))
            End If
            newSkill.Cells.Add(New Cell(rSkill.NeededFor))
            Dim reqLevel As Integer = CInt(newSkill.Cells(1).Text)
            Dim actLevel As Integer = CInt(newSkill.Cells(2).Text)
            Dim hqfLevel As Integer = CInt(newSkill.Cells(3).Text)
            If actLevel >= reqLevel And hqfLevel >= reqLevel Then
                newSkill.Style = TrainedSkillStyle
            Else
                If hqfLevel >= reqLevel Then
                    newSkill.Style = HQFSkillStyle
                Else
                    newSkill.Style = NotTrainedSkillStyle
                End If
            End If
            adtSkills.Nodes.Add(newSkill)
            ' Check for sub skills
            Call Me.DisplaySubSkills(newSkill, rSkill.ID)
        Next
        adtSkills.EndUpdate()

        ' Calculate the Queue Time
        Call Me.CalculateQueueTime()

    End Sub

    Private Sub DisplaySubSkills(ByVal parentSkill As Node, ByVal pSkillID As String)
        Dim aSkill As EveHQ.Core.PilotSkill
        Dim pSkill As EveHQ.Core.EveSkill = EveHQ.Core.HQ.SkillListID(pSkillID)

        If pSkill.PreReqSkills.Count > 0 Then
            For Each preReqSkill As String In pSkill.PreReqSkills.Keys
                If EveHQ.Core.HQ.SkillListID.ContainsKey(preReqSkill) Then
                    Dim newSkill As New Node
                    newSkill.Text = EveHQ.Core.SkillFunctions.SkillIDToName(preReqSkill)
                    Dim rSkill As HQFSkill = CType(reqHPilot.SkillSet(newSkill.Text), HQFSkill)
                    newSkill.Cells.Add(New Cell(pSkill.PreReqSkills(preReqSkill).ToString))
                    If SkillList.ContainsKey(newSkill.Text) = False Then
                        SkillList.Add(newSkill.Text, pSkill.PreReqSkills(preReqSkill))
                    Else
                        If SkillList(newSkill.Text) < pSkill.PreReqSkills(preReqSkill) Then
                            SkillList(newSkill.Text) = pSkill.PreReqSkills(preReqSkill)
                        End If
                    End If
                    If reqPilot.PilotSkills.Contains(newSkill.Text) = True Then
                        aSkill = CType(reqPilot.PilotSkills(newSkill.Text), Core.PilotSkill)
                        newSkill.Cells.Add(New Cell(aSkill.Level.ToString))
                    Else
                        newSkill.Cells.Add(New Cell("0"))
                    End If
                    newSkill.Cells.Add(New Cell(rSkill.Level.ToString))
                    Dim reqLevel As Integer = CInt(newSkill.Cells(1).Text)
                    Dim actLevel As Integer = CInt(newSkill.Cells(2).Text)
                    Dim hqfLevel As Integer = CInt(newSkill.Cells(3).Text)
                    If actLevel >= reqLevel And hqfLevel >= reqLevel Then
                        newSkill.Style = TrainedSkillStyle
                    Else
                        If hqfLevel >= reqLevel Then
                            newSkill.Style = HQFSkillStyle
                        Else
                            newSkill.Style = NotTrainedSkillStyle
                        End If
                    End If
                    parentSkill.Nodes.Add(newSkill)
                    Call Me.DisplaySubSkills(newSkill, preReqSkill)
                End If

            Next
        End If

    End Sub

    Private Sub CalculateQueueTime()
        Dim nPilot As EveHQ.Core.Pilot = reqPilot
        Dim newQueue As New EveHQ.Core.SkillQueue
        newQueue.Name = "HQFQueue"
        newQueue.IncCurrentTraining = False
        newQueue.Primary = False

        ' Add the skills we have to the training queue (in any order, no learning skills will be applied)
        Dim skill As Integer = 0
        For Each rSkill As ReqSkill In reqSkills
            Dim skillName As String = rSkill.Name
            Dim skillLevel As Integer = CInt(rSkill.ReqLevel)
            Dim qItem As New EveHQ.Core.SkillQueueItem
            qItem.Name = skillName
            qItem.FromLevel = 0
            qItem.ToLevel = skillLevel
            qItem.Pos = skill + 1
            qItem.Key = qItem.Name & qItem.FromLevel & qItem.ToLevel
            newQueue = EveHQ.Core.SkillQueueFunctions.AddSkillToQueue(nPilot, skillName, skill + 1, newQueue, skillLevel, True, True, "HQF: " & FittingName)
        Next

        ' Build the Queue
        Dim aQueue As ArrayList = EveHQ.Core.SkillQueueFunctions.BuildQueue(nPilot, newQueue, False, True)

        ' Display the time results
        lblQueueTime.Text = "Estimated Queue Time: " & EveHQ.Core.SkillFunctions.TimeToString(newQueue.QueueTime)

    End Sub

#End Region

#Region "Button Routines"

    Private Sub btnClose_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnClose.Click
        Me.Close()
    End Sub

    Private Sub btnAddToQueue_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAddToQueue.Click
        Call Me.AddNeededSkillsToQueue()
    End Sub

    Private Sub AddNeededSkillsToQueue()
        Dim NeededSkills As New List(Of String)
        For Each NeededSkill As HQF.ReqSkill In reqSkills
            NeededSkills.Add(NeededSkill.Name & NeededSkill.ReqLevel)
        Next
        Dim selQ As New EveHQ.Core.frmSelectQueue(reqPilot.Name, NeededSkills, "HQF: " & FittingName)
        selQ.ShowDialog()
        EveHQ.Core.SkillQueueFunctions.StartQueueRefresh = True
    End Sub

    Private Sub btnSetSkillsToRequirements_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnSetSkillsToRequirements.Click
        For Each requiredSkill As String In SkillList.Keys
            Dim MyHQFSkill As HQFSkill = CType(reqHPilot.SkillSet(requiredSkill), HQFSkill)
            If MyHQFSkill.Level < SkillList(requiredSkill) Then
                MyHQFSkill.Level = SkillList(requiredSkill)
            End If
        Next
        ForceUpdate = True
        Call Me.UpdateReqSkills()
        Call Me.DrawSkillsTable()
    End Sub

    Private Sub UpdateReqSkills()
        For Each rSkill As ReqSkill In reqSkills
            If SkillList.ContainsKey(rSkill.Name) = True Then
                rSkill.CurLevel = SkillList(rSkill.Name)
            End If
        Next
    End Sub

#End Region

End Class