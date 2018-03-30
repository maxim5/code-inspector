'
' DotNetNuke® - http://www.dotnetnuke.com
' Copyright (c) 2002-2010
' by DotNetNuke Corporation
'
' Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated 
' documentation files (the "Software"), to deal in the Software without restriction, including without limitation 
' the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and 
' to permit persons to whom the Software is furnished to do so, subject to the following conditions:
'
' The above copyright notice and this permission notice shall be included in all copies or substantial portions 
' of the Software.
'
' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
' TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
' THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF 
' CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
' DEALINGS IN THE SOFTWARE.
'

Imports DotNetNuke.HttpModules.RequestFilter
Imports DotNetNuke.UI.Utilities
Imports DotNetNuke.UI.WebControls
Imports DotNetNuke.Services.Localization
Imports System.Collections.Generic
Imports System.IO
Imports System.Web.UI.WebControls
Imports System.Xml
Imports System.Xml.Serialization

Namespace DotNetNuke.Modules.Admin.Host

    ''' <summary>
    ''' The FriendlyUrls PortalModuleBase is used to edit the friendly urls
    ''' for the application.
    ''' </summary>
    ''' <remarks>
    ''' </remarks>
    ''' <history>
    ''' 	[cnurse]	07/06/2006 Created
    ''' </history>
    Partial Class RequestFilters
        Inherits DotNetNuke.Entities.Modules.PortalModuleBase
#Region "Private Fields"
        Private _Rules As List(Of RequestFilterRule)
#End Region

#Region "Private Methods"

        ''' <summary>
        ''' Adds a confirmation dialog to the delete button.
        ''' </summary>
        ''' <param name="e"></param>
        ''' <remarks></remarks>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        Private Shared Sub AddConfirmActiontoDeleteButton(ByVal e As System.Web.UI.WebControls.DataListItemEventArgs)
            Dim cmdDelete As ImageButton = CType(e.Item.FindControl("cmdDelete"), ImageButton)
            ClientAPI.AddButtonConfirm(cmdDelete, Localization.GetString("DeleteItem"))
        End Sub

        ''' <summary>
        ''' Binds the selected values of the Request Filter Rule dropdown lists.
        ''' </summary>
        ''' <param name="e"></param>
        ''' <remarks></remarks>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        Private Sub BindDropDownValues(ByVal e As System.Web.UI.WebControls.DataListItemEventArgs)
            Dim rule As RequestFilterRule = CType(e.Item.DataItem, RequestFilterRule)

            Dim ddlOperator As DropDownList = CType(e.Item.FindControl("ddlOperator"), DropDownList)
            If ddlOperator IsNot Nothing AndAlso rule IsNot Nothing Then
                ddlOperator.SelectedValue = rule.Operator.ToString
            End If

            Dim ddlAction As DropDownList = CType(e.Item.FindControl("ddlAction"), DropDownList)
            If ddlAction IsNot Nothing AndAlso rule IsNot Nothing Then
                ddlAction.SelectedValue = rule.Action.ToString
            End If
        End Sub

        ''' <summary>
        ''' BindRules updates the datalist with the values of the current list of rules.
        ''' </summary>
        ''' <remarks></remarks>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        Private Sub BindRules()
            rptRules.DataSource = Rules
            rptRules.DataBind()
        End Sub

#End Region

#Region "Properties"

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' Gets and sets the mode of the control
        ''' </summary>
        ''' <value></value>
        ''' <returns>
        ''' The mode is used to determine when the user is creating a new rule 
        ''' and allows the system to know to remove "blank" rules if the user cancels
        ''' the edit.
        ''' </returns>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Property AddMode() As Boolean
            Get
                Dim _Mode As Boolean = Null.NullBoolean
                If Not ViewState("Mode") Is Nothing Then
                    _Mode = CType(ViewState("Mode"), Boolean)
                End If
                Return _Mode
            End Get
            Set(ByVal value As Boolean)
                ViewState("Mode") = value
            End Set
        End Property

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' Reads and writes to the list of Request Filter rules
        ''' </summary>
        ''' <value>
        ''' Generic List(Of RequestFilterRule)
        ''' </value>
        ''' <returns>
        ''' </returns>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Property Rules() As List(Of RequestFilterRule)
            Get
                If _Rules Is Nothing Then
                    _Rules = RequestFilterSettings.GetSettings().Rules
                End If
                Return _Rules
            End Get
            Set(ByVal value As List(Of RequestFilterRule))
                _Rules = value
            End Set
        End Property

#End Region

#Region "Protected methods"

        ''' <summary>
        ''' Retrieves the list of rules from the viewstate rather than constantly 
        ''' re-reading the configuration file.
        ''' </summary>
        ''' <param name="savedState"></param>
        ''' <remarks></remarks>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        Protected Overrides Sub LoadViewState(ByVal savedState As Object)
            Dim myState As Object() = CType(savedState, Object())
            If Not (myState(0) Is Nothing) Then
                MyBase.LoadViewState(myState(0))
            End If
            If Not (myState(1) Is Nothing) Then
                Dim configRules As New List(Of RequestFilterRule)

                ' Deserialize into RewriterConfiguration
                configRules = DirectCast(XmlUtils.Deserialize(CStr(myState(1)), configRules.GetType), List(Of RequestFilterRule))
                Rules = configRules
            End If
        End Sub

        ''' <summary>
        ''' Saves the rules to the viewstate to avoid constantly re-reading
        ''' the configuration file.
        ''' </summary>
        ''' <returns></returns>
        ''' <remarks></remarks>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        Protected Overrides Function SaveViewState() As Object
            Dim configRules As List(Of RequestFilterRule) = New List(Of RequestFilterRule)
            configRules = Rules

            Dim baseState As Object = MyBase.SaveViewState()
            Dim allStates(2) As Object
            allStates(0) = baseState
            allStates(1) = XmlUtils.Serialize(configRules)

            Return allStates
        End Function

#End Region

#Region "Event Handlers"

        ''' <summary>
        ''' Page_Load runs when the control is loaded.
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        Private Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load


            'Bind the rules (as long as not postback)
            If Not Page.IsPostBack Then
                BindRules()
            End If

        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' AddRule runs when the Add button is clicked
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Protected Sub AddRule(ByVal sender As Object, ByVal e As System.EventArgs) Handles cmdAddRule.Click

            'Add a new empty rule and set the editrow to the new row
            Rules.Add(New RequestFilterRule)
            rptRules.EditItemIndex = Rules.Count - 1

            'Set the AddMode to true
            AddMode = True

            'Rebind the collection
            BindRules()

        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' DeleteRule runs when the Delete button for a specified rule is clicked
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Protected Sub DeleteRule(ByVal source As Object, ByVal e As DataListCommandEventArgs) Handles rptRules.DeleteCommand

            'Get the index of the row to delete
            Dim index As Integer = e.Item.ItemIndex

            'Remove the rule from the rules collection
            Rules.RemoveAt(index)

            Try
                'Save the new collection
                RequestFilterSettings.Save(Rules)
            Catch ex As UnauthorizedAccessException
                lblErr.InnerText = Localization.GetString("unauthorized", Me.LocalResourceFile)
                lblErr.Visible = True
                'This forces the system to reload the settings from DotNetNuke.Config
                'since we have already deleted the entry from the Rules list.
                Rules = Nothing
            End Try

            'Rebind the collection
            BindRules()

        End Sub

        ''' <summary>
        ''' EditRule runs when the Edit button is clicked.
        ''' </summary>
        ''' <param name="source"></param>
        ''' <param name="e"></param>
        ''' <remarks></remarks>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        Protected Sub EditRule(ByVal source As Object, ByVal e As DataListCommandEventArgs) Handles rptRules.EditCommand
            lblErr.Visible = True

            'Set the AddMode to false
            AddMode = False

            'Set the editrow
            rptRules.EditItemIndex = e.Item.ItemIndex

            'Rebind the collection
            BindRules()

        End Sub

        ''' <summary>
        ''' SaveRule runs when the Save button is clicked.
        ''' </summary>
        ''' <param name="source"></param>
        ''' <param name="e"></param>
        ''' <remarks>
        ''' The Save button is displayed for a specific request filter rule
        ''' when the user enters the edit mode.
        ''' </remarks>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        Protected Sub SaveRule(ByVal source As Object, ByVal e As DataListCommandEventArgs) Handles rptRules.UpdateCommand

            'Get the index of the row to save
            Dim index As Integer = rptRules.EditItemIndex

            Dim rule As RequestFilterRule = Rules(index)
            Dim txtServerVar As TextBox = CType(e.Item.FindControl("txtServerVar"), TextBox)
            Dim txtValue As TextBox = CType(e.Item.FindControl("txtValue"), TextBox)
            Dim txtLocation As TextBox = CType(e.Item.FindControl("txtLocation"), TextBox)
            Dim ddlOperator As DropDownList = CType(e.Item.FindControl("ddlOperator"), DropDownList)
            Dim ddlAction As DropDownList = CType(e.Item.FindControl("ddlAction"), DropDownList)

            If txtServerVar.Text <> "" And txtValue.Text <> "" Then
                rule.ServerVariable = txtServerVar.Text
                rule.Location = txtLocation.Text
                rule.Operator = CType([Enum].Parse(GetType(RequestFilterOperatorType), ddlOperator.SelectedValue), RequestFilterOperatorType)
                rule.Action = CType([Enum].Parse(GetType(RequestFilterRuleType), ddlAction.SelectedValue), RequestFilterRuleType)

                ' A rule value may be a semicolon delimited list of values.  So we need to use a helper function to 
                ' parse the list.  If this is a regex, then only one value is supported.
                rule.SetValues(txtValue.Text, rule.Operator)

                'Save the modified collection
                RequestFilterSettings.Save(Rules)
            Else
                If AddMode Then
                    'Remove the temporary added row
                    Rules.RemoveAt(Rules.Count - 1)
                End If
            End If

            AddMode = False

            'Reset Edit Index
            rptRules.EditItemIndex = -1
            BindRules()

        End Sub
        ''' <summary>
        ''' CancelEdit runs when the Cancel button is clicked.
        ''' </summary>
        ''' <param name="source"></param>
        ''' <param name="e"></param>
        ''' <remarks>
        ''' The Cancel button is displayed for a specific request filter rule
        ''' when the user enters the edit mode.  Clicking the cancel button will
        ''' return the user to normal view mode with saving any of their changes
        ''' to the specific Request Filter Rule.
        ''' </remarks>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        Protected Sub CancelEdit(ByVal source As Object, ByVal e As DataListCommandEventArgs) Handles rptRules.CancelCommand

            If AddMode Then
                'Remove the temporary added row
                Rules.RemoveAt(Rules.Count - 1)
                AddMode = False
            End If

            'Clear editrow
            rptRules.EditItemIndex = -1

            'Rebind the collection
            BindRules()

        End Sub

        ''' <summary>
        ''' The ItemDataBound event is used to set the value of the Operator and Action
        ''' dropdownlists based on the current values for the specific Request Filter Rule.
        ''' </summary>
        ''' <param name="sender"></param>
        ''' <param name="e"></param>
        ''' <remarks></remarks>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        Protected Sub rptRules_ItemDataBound(ByVal sender As Object, ByVal e As System.Web.UI.WebControls.DataListItemEventArgs) Handles rptRules.ItemDataBound
            Select Case e.Item.ItemType
                Case ListItemType.AlternatingItem
                    AddConfirmActiontoDeleteButton(e)
                Case ListItemType.Item
                    AddConfirmActiontoDeleteButton(e)
                Case ListItemType.EditItem
                    BindDropDownValues(e)
            End Select
        End Sub

        ''' <summary>
        ''' The PreRender event is used to disable the "Add Rule" button when the user is in edit mode
        ''' </summary>
        ''' <param name="sender"></param>
        ''' <param name="e"></param>
        ''' <remarks></remarks>
        ''' <history>
        ''' 	[jbrinkman]	5/28/2007  Created
        ''' </history>
        Protected Sub Page_PreRender(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.PreRender
            ' If the user is editing a rule, then disable the "Add Rule" button
            cmdAddRule.Enabled = rptRules.EditItemIndex = -1
        End Sub
#End Region

    End Class

End Namespace

