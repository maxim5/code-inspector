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
Imports DotNetNuke.Entities.Profile
Imports DotNetNuke.Security.Profile
Imports DotNetNuke.Services.Localization
Imports DotNetNuke.UI.Skins.Controls.ModuleMessage
Imports DotNetNuke.UI.Utilities
Imports DotNetNuke.UI.WebControls
Imports DotNetNuke.Common.Lists

Namespace DotNetNuke.Modules.Admin.Users

    ''' -----------------------------------------------------------------------------
    ''' <summary>
    ''' The Users PortalModuleBase is used to manage the Registered Users of a portal
    ''' </summary>
    ''' <remarks>
    ''' </remarks>
    ''' <history>
    ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
    '''                       and localisation
    '''     [cnurse]    02/16/2006  Updated to reflect custom profile definitions
    ''' </history>
    ''' -----------------------------------------------------------------------------
    Partial Class UserAccounts
        Inherits Entities.Modules.PortalModuleBase
        Implements Entities.Modules.IActionable

#Region "Private Members"

        Private _Filter As String = ""
        Private _FilterProperty As String = ""
        Private _CurrentPage As Integer = 1
        Private _Users As ArrayList = New ArrayList

#End Region

#Region "Protected Members"

        Protected TotalPages As Integer = -1
        Protected TotalRecords As Integer

        Protected Property CurrentPage() As Integer
            Get
                Return _CurrentPage
            End Get
            Set(ByVal Value As Integer)
                _CurrentPage = Value
            End Set
        End Property

        Protected Property Filter() As String
            Get
                Return _Filter
            End Get
            Set(ByVal Value As String)
                _Filter = Value
            End Set
        End Property

        Protected Property FilterProperty() As String
            Get
                Return _FilterProperty
            End Get
            Set(ByVal Value As String)
                _FilterProperty = Value
            End Set
        End Property

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' Gets whether we are dealing with SuperUsers
        ''' </summary>
        ''' <history>
        ''' 	[cnurse]	03/02/2006  Created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Protected ReadOnly Property IsSuperUser() As Boolean
            Get
                If PortalSettings.ActiveTab.ParentId = PortalSettings.SuperTabId Then
                    Return True
                Else
                    Return False
                End If
            End Get
        End Property

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' Gets the Page Size for the Grid
        ''' </summary>
        ''' <history>
        ''' 	[cnurse]	03/02/2006  Created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Protected ReadOnly Property PageSize() As Integer
            Get
                Dim setting As Object = UserModuleBase.GetSetting(UsersPortalId, "Records_PerPage")
                Return CType(setting, Integer)
            End Get
        End Property

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' Gets a flag that determines whether to suppress the Pager (when not required)
        ''' </summary>
        ''' <history>
        ''' 	[cnurse]	08/10/2006  Created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Protected ReadOnly Property SuppressPager() As Boolean
            Get
                Dim setting As Object = UserModuleBase.GetSetting(UsersPortalId, "Display_SuppressPager")
                Return CType(setting, Boolean)
            End Get
        End Property

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' Gets the Portal Id whose Users we are managing
        ''' </summary>
        ''' <history>
        ''' 	[cnurse]	03/02/2006  Created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Protected ReadOnly Property UsersPortalId() As Integer
            Get
                Dim intPortalId As Integer = PortalId
                If IsSuperUser Then
                    intPortalId = Null.NullInteger
                End If
                Return intPortalId
            End Get
        End Property

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' Gets and sets the Filter to use
        ''' </summary>
        ''' <history>
        ''' 	[cnurse]	03/09/2006  Created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Protected ReadOnly Property UserFilter(ByVal newFilter As Boolean) As String
            Get
                Dim page As String = IIf(Not String.IsNullOrEmpty(CurrentPage), "currentpage=" & CurrentPage, "").ToString()
                Dim filterString As String
                Dim filterPropertyString As String

                If Not newFilter Then
                    filterString = IIf(Not String.IsNullOrEmpty(Filter), "filter=" & Filter, "").ToString()
                    filterPropertyString = IIf(Not String.IsNullOrEmpty(FilterProperty), "filterproperty=" & FilterProperty, "").ToString()
                Else
                    filterString = IIf(Not String.IsNullOrEmpty(txtSearch.Text), "filter=" & Server.UrlEncode(txtSearch.Text), "").ToString()
                    filterPropertyString = IIf(Not String.IsNullOrEmpty(ddlSearchType.SelectedValue.ToString()), "filterproperty=" & ddlSearchType.SelectedValue.ToString(), "").ToString()
                End If

                If Not String.IsNullOrEmpty(filterString) Then
                    filterString += "&"
                End If

                If Not String.IsNullOrEmpty(filterPropertyString) Then
                    filterString += filterPropertyString + "&"
                End If

                If Not String.IsNullOrEmpty(page) Then
                    filterString += page
                End If

                Return filterString
            End Get
        End Property

        Protected Property Users() As ArrayList
            Get
                Return _Users
            End Get
            Set(ByVal Value As ArrayList)
                _Users = Value
            End Set
        End Property

#End Region

#Region "Private Methods"

        Private Function AddSearchItem(ByVal name As String) As ListItem
            Dim propertyName As String = Null.NullString
            If Not Request.QueryString("filterProperty") Is Nothing Then
                propertyName = Request.QueryString("filterProperty")
            End If

            Dim text As String = Localization.GetString(name, Me.LocalResourceFile)
            If text = "" Then text = name
            Dim li As ListItem = New ListItem(text, name)
            If name = propertyName Then
                li.Selected = True
            End If
            Return li
        End Function

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' BindData gets the users from the Database and binds them to the DataGrid
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
        '''                       and localisation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub BindData()
            BindData(Nothing, Nothing)
        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' BindData gets the users from the Database and binds them to the DataGrid
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <param name="SearchText">Text to Search</param>
        ''' <param name="SearchField">Field to Search</param>
        ''' <history>
        ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
        '''                       and localisation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub BindData(ByVal SearchText As String, ByVal SearchField As String)

            CreateLetterSearch()

            Dim strQuerystring As String = Null.NullString

            If SearchText <> "" Then
                strQuerystring += "filter=" + SearchText
            End If

            If SearchText = Localization.GetString("Unauthorized") Then
                Users = UserController.GetUnAuthorizedUsers(UsersPortalId)
                ctlPagingControl.Visible = False
            ElseIf SearchText = Localization.GetString("OnLine") Then
                Users = UserController.GetOnlineUsers(UsersPortalId)
                ctlPagingControl.Visible = False
            ElseIf SearchText = Localization.GetString("All") Then
                Users = UserController.GetUsers(UsersPortalId, CurrentPage - 1, PageSize, TotalRecords)
            ElseIf SearchText <> "None" Then
                Select Case SearchField
                    Case "Email"
                        Users = UserController.GetUsersByEmail(UsersPortalId, SearchText + "%", CurrentPage - 1, PageSize, TotalRecords)
                    Case "Username"
                        Users = UserController.GetUsersByUserName(UsersPortalId, SearchText + "%", CurrentPage - 1, PageSize, TotalRecords)
                    Case Else
                        Users = UserController.GetUsersByProfileProperty(UsersPortalId, SearchField, SearchText + "%", CurrentPage - 1, PageSize, TotalRecords)
                        strQuerystring += "&filterProperty=" + SearchField
                End Select
            End If
            If SuppressPager And ctlPagingControl.Visible Then
                ctlPagingControl.Visible = (PageSize < TotalRecords)
            End If

            grdUsers.DataSource = Users
            grdUsers.DataBind()

            ctlPagingControl.TotalRecords = TotalRecords
            ctlPagingControl.PageSize = PageSize
            ctlPagingControl.CurrentPage = CurrentPage

            ctlPagingControl.QuerystringParams = strQuerystring
            ctlPagingControl.TabID = TabId
        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' Builds the letter filter
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
        '''                       and localisation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub CreateLetterSearch()

            Dim filters As String = Localization.GetString("Filter.Text", Me.LocalResourceFile)

            filters += "," + Localization.GetString("All")
            filters += "," + Localization.GetString("OnLine")
            filters += "," + Localization.GetString("Unauthorized")

            Dim strAlphabet As String() = filters.Split(","c)
            rptLetterSearch.DataSource = strAlphabet
            rptLetterSearch.DataBind()

        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' Deletes all unauthorized users
        ''' </summary>
        ''' <history>
        ''' 	[cnurse]	03/02/2006	Created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub DeleteUnAuthorizedUsers()
            Try
                UserController.DeleteUnauthorizedUsers(PortalId)

                BindData(Filter, ddlSearchType.SelectedItem.Value)

            Catch exc As Exception    'Module failed to load
                ProcessModuleLoadException(Me, exc)
            End Try
        End Sub

#End Region

#Region "Public Methods"

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' DisplayAddress correctly formats an Address
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
        '''                       and localisation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Public Function DisplayAddress(ByVal Unit As Object, ByVal Street As Object, ByVal City As Object, ByVal Region As Object, ByVal Country As Object, ByVal PostalCode As Object) As String
            Dim _Address As String = Null.NullString
            Try
                _Address = FormatAddress(Unit, Street, City, Region, Country, PostalCode)
            Catch exc As Exception    'Module failed to load
                ProcessModuleLoadException(Me, exc)
            End Try
            Return _Address
        End Function

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' DisplayEmail correctly formats an Email Address
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
        '''                       and localisation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Public Function DisplayEmail(ByVal Email As String) As String
            Dim _Email As String = Null.NullString
            Try
                If Not Email Is Nothing Then
                    _Email = HtmlUtils.FormatEmail(Email, False)
                End If
            Catch exc As Exception    'Module failed to load
                ProcessModuleLoadException(Me, exc)
            End Try
            Return _Email
        End Function

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' DisplayDate correctly formats the Date
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
        '''                       and localisation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Public Function DisplayDate(ByVal UserDate As Date) As String
            Dim _Date As String = Null.NullString
            Try
                If Not Null.IsNull(UserDate) Then
                    _Date = UserDate.ToString
                Else
                    _Date = ""
                End If
            Catch exc As Exception    'Module failed to load
                ProcessModuleLoadException(Me, exc)
            End Try
            Return _Date
        End Function

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' FormatURL correctly formats the Url for the Edit User Link
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
        '''                       and localisation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Protected Function FormatURL(ByVal strKeyName As String, ByVal strKeyValue As String) As String
            Dim _URL As String = Null.NullString
            Try
                If Filter <> "" Then
                    _URL = EditUrl(strKeyName, strKeyValue, "", "filter=" & Filter)
                Else
                    _URL = EditUrl(strKeyName, strKeyValue)
                End If
            Catch exc As Exception    'Module failed to load
                ProcessModuleLoadException(Me, exc)
            End Try
            Return _URL
        End Function

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' FilterURL correctly formats the Url for filter by first letter and paging
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	9/10/2004	Updated to reflect design changes for Help, 508 support
        '''                       and localisation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Protected Function FilterURL(ByVal Filter As String, ByVal CurrentPage As String) As String
            Dim _URL As String = Null.NullString
            If Filter <> "" Then
                If CurrentPage <> "" Then
                    _URL = Common.Globals.NavigateURL(TabId, "", "filter=" & Filter, "currentpage=" & CurrentPage)
                Else
                    _URL = Common.Globals.NavigateURL(TabId, "", "filter=" & Filter)
                End If
            Else
                If CurrentPage <> "" Then
                    _URL = Common.Globals.NavigateURL(TabId, "", "currentpage=" & CurrentPage)
                Else
                    _URL = Common.Globals.NavigateURL(TabId, "")
                End If
            End If
            Return _URL

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

            If Not Request.QueryString("CurrentPage") Is Nothing Then
                CurrentPage = CType(Request.QueryString("CurrentPage"), Integer)
            End If

            If Not Request.QueryString("filter") Is Nothing Then
                Filter = Request.QueryString("filter")
            End If

            If Not Request.QueryString("filterproperty") Is Nothing Then
                FilterProperty = Request.QueryString("filterproperty")
            End If

            If Filter = "" Then
                'Get Default View
                Dim setting As Object = UserModuleBase.GetSetting(UsersPortalId, "Display_Mode")
                Dim mode As DisplayMode = CType(setting, DisplayMode)
                Select Case mode
                    Case DisplayMode.All
                        Filter = Localization.GetString("All")
                    Case DisplayMode.FirstLetter
                        Filter = Localization.GetString("Filter.Text", Me.LocalResourceFile).Substring(0, 1)
                    Case DisplayMode.None
                        Filter = "None"
                End Select
            End If

            For Each column As DataGridColumn In grdUsers.Columns
                Dim isVisible As Boolean
                Dim header As String = column.HeaderText
                If header = "" Or header.ToLower = "username" Then
                    isVisible = True
                Else
                    Dim settingKey As String = "Column_" + header
                    Dim setting As Object = UserModuleBase.GetSetting(UsersPortalId, settingKey)
                    isVisible = CType(setting, Boolean)
                End If

                If column.GetType Is GetType(ImageCommandColumn) Then
                    isVisible = Me.IsEditable

                    'Manage Delete Confirm JS
                    Dim imageColumn As ImageCommandColumn = CType(column, ImageCommandColumn)
                    If imageColumn.CommandName = "Delete" Then
                        imageColumn.OnClickJS = Localization.GetString("DeleteItem")
                    End If
                    'Manage Edit Column NavigateURLFormatString
                    If imageColumn.CommandName = "Edit" Then
                        'The Friendly URL parser does not like non-alphanumeric characters
                        'so first create the format string with a dummy value and then
                        'replace the dummy value with the FormatString place holder
                        Dim formatString As String = EditUrl("UserId", "KEYFIELD", "Edit", UserFilter(False))
                        formatString = formatString.Replace("KEYFIELD", "{0}")
                        imageColumn.NavigateURLFormatString = formatString
                    End If
                    'Manage Roles Column NavigateURLFormatString
                    If imageColumn.CommandName = "UserRoles" Then
                        If IsHostMenu Then
                            isVisible = False
                        Else
                            'The Friendly URL parser does not like non-alphanumeric characters
                            'so first create the format string with a dummy value and then
                            'replace the dummy value with the FormatString place holder
                            Dim formatString As String = EditUrl("UserId", "KEYFIELD", "User Roles", UserFilter(False))
                            formatString = formatString.Replace("KEYFIELD", "{0}")
                            imageColumn.NavigateURLFormatString = formatString
                        End If
                    End If

                    'Localize Image Column Text
                    If imageColumn.CommandName <> "" Then
                        imageColumn.Text = Localization.GetString(imageColumn.CommandName, Me.LocalResourceFile)
                    End If
                End If

                column.Visible = isVisible
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

                'Add an Action Event Handler to the Skin
                AddActionHandler(AddressOf ModuleAction_Click)

                If Not Page.IsPostBack Then
                    'Load the Search Combo
                    ddlSearchType.Items.Add(AddSearchItem("Username"))
                    ddlSearchType.Items.Add(AddSearchItem("Email"))
                    Dim profileProperties As ProfilePropertyDefinitionCollection = ProfileController.GetPropertyDefinitionsByPortal(PortalId, False)
                    For Each definition As ProfilePropertyDefinition In profileProperties
                        Dim controller As New ListController
                        Dim imageDataType As ListEntryInfo = controller.GetListEntryInfo("DataType", "Image")
                        If imageDataType Is Nothing OrElse definition.DataType = imageDataType.EntryID Then
                            Exit For
                        End If
                        ddlSearchType.Items.Add(AddSearchItem(definition.PropertyName))
                    Next

                    'Localize the Headers
                    Localization.LocalizeDataGrid(grdUsers, Me.LocalResourceFile)
                    BindData(Filter, ddlSearchType.SelectedItem.Value)

                    'Sent controls to current Filter
                    If (Filter <> "" And Filter.ToUpper() <> "NONE") And FilterProperty <> "" Then
                        txtSearch.Text = Filter
                        ddlSearchType.SelectedValue = FilterProperty
                    End If
                End If

            Catch exc As Exception    'Module failed to load
                ProcessModuleLoadException(Me, exc)
            End Try

        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' ModuleAction_Click handles all ModuleAction events raised from the skin
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <param name="sender"> The object that triggers the event</param>
        ''' <param name="e">An ActionEventArgs object</param>
        ''' <history>
        ''' 	[cnurse]	03/02/2006	Created
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub ModuleAction_Click(ByVal sender As Object, ByVal e As ActionEventArgs)
            Select Case e.Action.CommandArgument
                Case "Delete"
                    DeleteUnAuthorizedUsers()
            End Select
        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' btnSearch_Click runs when the user searches for accounts by username or email
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[dancaron]	10/28/2004	Intial Version
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub btnSearch_Click(ByVal sender As System.Object, ByVal e As System.Web.UI.ImageClickEventArgs) Handles btnSearch.Click
            CurrentPage = 1
            txtSearch.Text = txtSearch.Text.Trim
            Response.Redirect(NavigateURL(TabId, "", UserFilter(True)))
        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' grdUsers_DeleteCommand runs when the icon in the delete column is clicked
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	01/05/2007	Intial documentation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub grdUsers_DeleteCommand(ByVal source As Object, ByVal e As System.Web.UI.WebControls.DataGridCommandEventArgs) Handles grdUsers.DeleteCommand
            Try
                Dim userId As Integer = Int32.Parse(e.CommandArgument.ToString)

                Dim user As UserInfo = UserController.GetUserById(UsersPortalId, userId)

                If Not user Is Nothing Then
                    If UserController.DeleteUser(user, True, False) Then
                        UI.Skins.Skin.AddModuleMessage(Me, Localization.GetString("UserDeleted", Me.LocalResourceFile), ModuleMessageType.GreenSuccess)
                    Else
                        UI.Skins.Skin.AddModuleMessage(Me, Localization.GetString("UserDeleteError", Me.LocalResourceFile), ModuleMessageType.RedError)
                    End If
                End If

                If txtSearch.Text <> "" Then
                    Filter = txtSearch.Text
                End If
                BindData(Filter, ddlSearchType.SelectedItem.Value)

            Catch exc As Exception    'Module failed to load
                ProcessModuleLoadException(Me, exc)
            End Try
        End Sub

        ''' -----------------------------------------------------------------------------
        ''' <summary>
        ''' grdUsers_ItemDataBound runs when a row in the grid is bound
        ''' </summary>
        ''' <remarks>
        ''' </remarks>
        ''' <history>
        ''' 	[cnurse]	01/05/2007	Intial documentation
        ''' </history>
        ''' -----------------------------------------------------------------------------
        Private Sub grdUsers_ItemDataBound(ByVal sender As Object, ByVal e As System.Web.UI.WebControls.DataGridItemEventArgs) Handles grdUsers.ItemDataBound
            Dim item As DataGridItem = e.Item

            If item.ItemType = ListItemType.Item Or _
                    item.ItemType = ListItemType.AlternatingItem Or _
                    item.ItemType = ListItemType.SelectedItem Then

                Dim imgColumnControl As Control = item.Controls(0).Controls(0)
                If TypeOf imgColumnControl Is HyperLink Then
                    Dim editLink As HyperLink = CType(imgColumnControl, HyperLink)
                    Dim user As UserInfo = CType(item.DataItem, UserInfo)

                    editLink.Visible = Not user.IsInRole(PortalSettings.AdministratorRoleName) OrElse (PortalSecurity.IsInRole(PortalSettings.AdministratorRoleName))
                End If

                imgColumnControl = item.Controls(1).Controls(0)
                If TypeOf imgColumnControl Is ImageButton Then
                    Dim delImage As ImageButton = CType(imgColumnControl, ImageButton)
                    Dim user As UserInfo = CType(item.DataItem, UserInfo)

                    delImage.Visible = Not (user.UserID = PortalSettings.AdministratorId) AndAlso _
                                            (Not user.IsInRole(PortalSettings.AdministratorRoleName) OrElse _
                                            (PortalSecurity.IsInRole(PortalSettings.AdministratorRoleName))) AndAlso _
                                            Not user.UserID = UserId

                End If

                imgColumnControl = item.Controls(2).Controls(0)
                If TypeOf imgColumnControl Is HyperLink Then
                    Dim rolesLink As HyperLink = CType(imgColumnControl, HyperLink)
                    Dim user As UserInfo = CType(item.DataItem, UserInfo)

                    rolesLink.Visible = Not user.IsInRole(PortalSettings.AdministratorRoleName) OrElse (PortalSecurity.IsInRole(PortalSettings.AdministratorRoleName))
                End If

                imgColumnControl = item.Controls(3).FindControl("imgOnline")
                If TypeOf imgColumnControl Is System.Web.UI.WebControls.Image Then
                    Dim userOnlineImage As System.Web.UI.WebControls.Image = CType(imgColumnControl, System.Web.UI.WebControls.Image)
                    Dim user As UserInfo = CType(item.DataItem, UserInfo)

                    userOnlineImage.Visible = user.Membership.IsOnLine
                End If
            End If
        End Sub

#End Region

#Region "Optional Interfaces"

        Public ReadOnly Property ModuleActions() As ModuleActionCollection Implements Entities.Modules.IActionable.ModuleActions
            Get
                Dim Actions As New ModuleActionCollection
                Dim FilterParams(IIf(txtSearch.Text.Trim() = "", 2, 3)) As String

                If (txtSearch.Text.Trim() = "") Then
                    FilterParams.SetValue("filter=" & Filter, 0)
                    FilterParams.SetValue("currentpage=" & CurrentPage, 1)
                Else
                    FilterParams.SetValue("filter=" & txtSearch.Text, 0)
                    FilterParams.SetValue("filterproperty=" & IIf(ddlSearchType Is Nothing, "", ddlSearchType.SelectedValue.ToString()), 1)
                    FilterParams.SetValue("currentpage=" & CurrentPage, 2)
                End If

                Actions.Add(GetNextActionID, Localization.GetString(ModuleActionType.AddContent, LocalResourceFile), ModuleActionType.AddContent, "", "add.gif", EditUrl(), False, SecurityAccessLevel.Edit, True, False)
                If Not IsSuperUser Then
                    Actions.Add(GetNextActionID, Localization.GetString("DeleteUnAuthorized.Action", LocalResourceFile), ModuleActionType.AddContent, "Delete", "delete.gif", "", "confirm('" + ClientAPI.GetSafeJSString(Localization.GetString("DeleteItems.Confirm")) + "')", True, SecurityAccessLevel.Edit, True, False)
                End If
                If ProfileProviderConfig.CanEditProviderProperties Then
                    Actions.Add(GetNextActionID, Localization.GetString("ManageProfile.Action", LocalResourceFile), ModuleActionType.AddContent, "", "icon_profile_16px.gif", EditUrl("ManageProfile"), False, SecurityAccessLevel.Edit, True, False)
                End If
                Actions.Add(GetNextActionID, Localization.GetString("UserSettings.Action", LocalResourceFile), ModuleActionType.AddContent, "", "settings.gif", EditUrl("UserSettings"), False, SecurityAccessLevel.Edit, True, False)
                Return Actions
            End Get
        End Property

#End Region

    End Class

End Namespace
