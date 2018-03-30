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

Imports DotNetNuke.Entities.Modules
Imports DotNetNuke.Entities.Modules.Actions
Imports DotNetNuke.Security.Roles
Imports DotNetNuke.Services.Localization
Imports DotNetNuke.UI.Utilities
Imports DotNetNuke.UI.WebControls

Namespace DotNetNuke.Modules.Admin.Security

	''' -----------------------------------------------------------------------------
	''' <summary>
	''' The Roles PortalModuleBase is used to manage the Security Roles for the
	''' portal.
	''' </summary>
    ''' <remarks>
	''' </remarks>
	''' <history>
	''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
	'''                       and localisation
	''' </history>
	''' -----------------------------------------------------------------------------
    Partial Class Roles
        Inherits Entities.Modules.PortalModuleBase
        Implements Entities.Modules.IActionable

#Region "Private Members"

        Private RoleGroupId As Integer = -1

#End Region

#Region "Private Methods"

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' BindData gets the roles from the Database and binds them to the DataGrid
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
        '''                       and localisation
        '''     [cnurse]    01/05/2006  Updated to reflect Use of Role Groups
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub BindData()

            ' Get the portal's roles from the database
            Dim objRoles As New RoleController
            Dim arrRoles As ArrayList

            If RoleGroupId < -1 Then
                arrRoles = objRoles.GetPortalRoles(PortalId)
            Else
                arrRoles = objRoles.GetRolesByGroup(PortalId, RoleGroupId)
            End If
            grdRoles.DataSource = arrRoles

            If RoleGroupId < 0 Then
                lnkEditGroup.Visible = False
                cmdDelete.Visible = False
            Else
                lnkEditGroup.Visible = True
                lnkEditGroup.NavigateUrl = EditUrl("RoleGroupId", RoleGroupId.ToString, "EditGroup")
                cmdDelete.Visible = Not (arrRoles.Count > 0)
                ClientAPI.AddButtonConfirm(cmdDelete, Services.Localization.Localization.GetString("DeleteItem"))
            End If

            DotNetNuke.Services.Localization.Localization.LocalizeDataGrid(grdRoles, Me.LocalResourceFile)

            grdRoles.DataBind()

        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' BindGroups gets the role Groups from the Database and binds them to the DropDown
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        '''     [cnurse]    01/05/2006  Created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub BindGroups()

            Dim liItem As ListItem
            Dim arrGroups As ArrayList = RoleController.GetRoleGroups(PortalId)

            If arrGroups.Count > 0 Then
                cborolegroups.Items.Clear()
                cboRoleGroups.Items.Add(New ListItem(Localization.GetString("AllRoles"), "-2"))

                liItem = New ListItem(Localization.GetString("GlobalRoles"), "-1")
                If RoleGroupId < 0 Then
                    liItem.Selected = True
                End If
                cboRoleGroups.Items.Add(liItem)

                For Each roleGroup As RoleGroupInfo In arrGroups
                    liItem = New ListItem(roleGroup.RoleGroupName, roleGroup.RoleGroupID.ToString)
                    If RoleGroupId = roleGroup.RoleGroupID Then
                        liItem.Selected = True
                    End If
                    cboRoleGroups.Items.Add(liItem)
                Next
                trGroups.Visible = True
            Else
                RoleGroupId = -2
                trGroups.Visible = False
            End If

            BindData()

        End Sub

#End Region

#Region "Public Methods"

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' FormatPeriod filters out Null values from the Period column of the Grid
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
        '''                       and localisation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Public Function FormatPeriod(ByVal period As Integer) As String
            Dim _FormatPeriod As String = Null.NullString
            Try
                If period <> Null.NullInteger Then
                    _FormatPeriod = period.ToString
                End If
            Catch exc As Exception    'Module failed to load
                ProcessModuleLoadException(Me, exc)
            End Try
            Return _FormatPeriod
        End Function

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' FormatPrice correctly formats the fee
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
        '''                       and localisation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Public Function FormatPrice(ByVal price As Single) As String
            Dim _FormatPrice As String = Null.NullString
            Try
                If price <> Null.NullSingle Then
                    _FormatPrice = price.ToString("##0.00")
                End If
            Catch exc As Exception    'Module failed to load
                ProcessModuleLoadException(Me, exc)
            End Try
            Return _FormatPrice
        End Function

#End Region

#Region "Event Handlers"

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' Page_Init runs when the control is initialised
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
        '''                       and localisation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub Page_Init(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Init

            For Each column As DataGridColumn In grdRoles.Columns
                If column.GetType Is GetType(ImageCommandColumn) Then
                    'Manage Delete Confirm JS
                    Dim imageColumn As ImageCommandColumn = CType(column, ImageCommandColumn)
                    imageColumn.Visible = Me.IsEditable
                    If imageColumn.CommandName = "Delete" Then
                        imageColumn.OnClickJS = Localization.GetString("DeleteItem")
                    End If
                    'Manage Edit Column NavigateURLFormatString
                    If imageColumn.CommandName = "Edit" Then
                        'The Friendly URL parser does not like non-alphanumeric characters
                        'so first create the format string with a dummy value and then
                        'replace the dummy value with the FormatString place holder
                        Dim formatString As String = EditUrl("RoleID", "KEYFIELD", "Edit")
                        formatString = formatString.Replace("KEYFIELD", "{0}")
                        imageColumn.NavigateURLFormatString = formatString
                    End If
                    'Manage Roles Column NavigateURLFormatString
                    If imageColumn.CommandName = "UserRoles" Then
                        'The Friendly URL parser does not like non-alphanumeric characters
                        'so first create the format string with a dummy value and then
                        'replace the dummy value with the FormatString place holder
                        Dim formatString As String = NavigateURL(TabId, "User Roles", "RoleId=KEYFIELD", "mid=" + ModuleId.ToString())
                        formatString = formatString.Replace("KEYFIELD", "{0}")
                        imageColumn.NavigateURLFormatString = formatString
                    End If

                    'Localize Image Column Text
                    If imageColumn.CommandName <> "" Then
                        imageColumn.Text = Localization.GetString(imageColumn.CommandName, Me.LocalResourceFile)
                    End If
                End If
            Next
        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' Page_Load runs when the control is loaded
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
        '''                       and localisation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub Page_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
            Try
                If Not Page.IsPostBack Then
                    If Not (Request.QueryString("RoleGroupID") Is Nothing) Then
                        RoleGroupId = Int32.Parse(Request.QueryString("RoleGroupID"))
                    End If
                    BindGroups()
                End If
            Catch exc As Exception    'Module failed to load
                ProcessModuleLoadException(Me, exc)
            End Try

        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' Runs when the Index of the RoleGroups combo box changes
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	01/06/2006  created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub cboRoleGroups_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cboRoleGroups.SelectedIndexChanged

            RoleGroupId = Int32.Parse(cboRoleGroups.SelectedValue)
            BindData()

        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' Runs when the Delete Button is clicked to delete a role group
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	01/06/2006  created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub cmdDelete_Click(ByVal sender As Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles cmdDelete.Click

            RoleGroupId = Int32.Parse(cboRoleGroups.SelectedValue)
            If RoleGroupId > -1 Then
                RoleController.DeleteRoleGroup(PortalId, RoleGroupId)
                RoleGroupId = -1
            End If
            BindGroups()

        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' grdRoles_ItemDataBound runs when a row in the grid is bound
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	11/28/2008 Created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub grdRoles_ItemDataBound(ByVal sender As Object, ByVal e As System.Web.UI.WebControls.DataGridItemEventArgs) Handles grdRoles.ItemDataBound
            Dim item As DataGridItem = e.Item

            If item.ItemType = ListItemType.Item Or _
                    item.ItemType = ListItemType.AlternatingItem Or _
                    item.ItemType = ListItemType.SelectedItem Then

                Dim imgColumnControl As Control = item.Controls(0).Controls(0)
                If TypeOf imgColumnControl Is HyperLink Then
                    Dim editLink As HyperLink = CType(imgColumnControl, HyperLink)
                    Dim role As RoleInfo = CType(item.DataItem, RoleInfo)

                    editLink.Visible = role.RoleName <> PortalSettings.AdministratorRoleName OrElse (PortalSecurity.IsInRole(PortalSettings.AdministratorRoleName))
                End If

                imgColumnControl = item.Controls(1).Controls(0)
                If TypeOf imgColumnControl Is HyperLink Then
                    Dim rolesLink As HyperLink = CType(imgColumnControl, HyperLink)
                    Dim role As RoleInfo = CType(item.DataItem, RoleInfo)

                    rolesLink.Visible = role.RoleName <> PortalSettings.AdministratorRoleName OrElse (PortalSecurity.IsInRole(PortalSettings.AdministratorRoleName))
                End If

            End If
        End Sub

#End Region

#Region "Optional Interfaces"

        Public ReadOnly Property ModuleActions() As ModuleActionCollection Implements Entities.Modules.IActionable.ModuleActions
            Get
                Dim Actions As New ModuleActionCollection
                Actions.Add(GetNextActionID, Localization.GetString(ModuleActionType.AddContent, LocalResourceFile), ModuleActionType.AddContent, "", "add.gif", EditUrl(), False, SecurityAccessLevel.Edit, True, False)
                Actions.Add(GetNextActionID, Localization.GetString("AddGroup.Action", LocalResourceFile), ModuleActionType.AddContent, "", "add.gif", EditUrl("EditGroup"), False, SecurityAccessLevel.Edit, True, False)
                Actions.Add(GetNextActionID, Localization.GetString("UserSettings.Action", LocalResourceFile), ModuleActionType.AddContent, "", "settings.gif", EditUrl("UserSettings"), False, SecurityAccessLevel.Edit, True, False)
                Return Actions
            End Get
        End Property

#End Region

    End Class

End Namespace
