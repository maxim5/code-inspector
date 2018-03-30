#Region " Imports "

Imports BlazeApps.Library.Reports

#End Region

Partial Class Usercontrols_Reports_HighestRank
    Inherits UserControlBase

#Region " Enums, Memebers and Properties "

    Public Property ReportTitle() As String
        Get
            Return GetStringFromVS("ReportTitle")
        End Get
        Set(ByVal value As String)
            ViewState.Add("ReportTitle", value)
        End Set
    End Property

    Public Property ReportType() As BlazeApps.Library.Base.ItemType
        Get
            Return CType(ViewState("ReportType"), BlazeApps.Library.Base.ItemType)
        End Get
        Set(ByVal value As BlazeApps.Library.Base.ItemType)
            ViewState.Add("ReportType", value)
        End Set
    End Property

    Public Property ReportDisplayCount() As Integer
        Get
            Return GetIntegerFromVS("ReportDisplayCount")
        End Get
        Set(ByVal value As Integer)
            ViewState.Add("ReportDisplayCount", value)
        End Set
    End Property

#End Region

#Region " Page Events "

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        If Me.Page.IsPostBack = False Then
            Dim List As New ReportList

            Me.uxHighestRankedItemsLabel.Text = ReportTitle

            With Me.uxHigestRankedRepeater
                .DataSource = List.GetTopNItemsHighestRank(ReportDisplayCount, ReportType)
                .DataBind()
            End With
        End If
    End Sub

#End Region

#Region " Save Data "



#End Region

#Region " Load Data "



#End Region

#Region " Form Controls Events "

    Protected Sub uxHigestRankedRepeater_ItemDataBound(ByVal sender As Object, ByVal e As System.Web.UI.WebControls.RepeaterItemEventArgs) Handles uxHigestRankedRepeater.ItemDataBound

        If e.Item.ItemType = ListItemType.Item Or e.Item.ItemType = ListItemType.AlternatingItem Then

            If ReportType = BlazeApps.Library.Base.ItemType.Forums Then
                e.Item.Visible = ShowItem(ConvertToGuid(e.Item.DataItem("itemid").ToString), BlazeApps.Library.Base.ItemType.Forums)
            Else
                e.Item.Visible = ShowItem(ConvertToGuid(e.Item.DataItem("itemid").ToString))
            End If

        End If

    End Sub

#End Region

#Region " Helpers and Methods "



#End Region

End Class
