Imports Securex.PPF.Ppfclient.Business.DataHolders

Public Class TaskWord

    Public Sub Save(ByVal filename As String)
        Try
            If Not (Facade.Instance.WordFactory.Word.HasWordDocument) Then
                GenerateDocument()
            End If
            Facade.Instance.WordFactory.Word.SaveAs(filename)
        Catch ex As Exception
            ThrowException(ex.Message, ex.InnerException)
        End Try
    End Sub

    Public Function GetPreview() As String
        GetPreview = Nothing
        Try
            GenerateDocument()
            GetPreview = Facade.Instance.WordFactory.Word.PreviewDocumentAsHtml()
        Catch exWord As WordExecption
            ThrowException(exWord.Message, exWord.InnerException)
        Catch ex As Exception
            ThrowException("Unable to generate preview.", ex.InnerException)
        End Try
    End Function

    Private Sub GenerateDocument()
        Dim currentRevision As ProjectProposalRevision = TaskProjectProposalRevision.Instance.GetCurrentProjectProposalRevision
        'load template
        Facade.Instance.WordFactory.Word.OpenTemplate()

        'text
        Dim mainDocumentPartDatalist As IList(Of AbstractWordData) = New List(Of AbstractWordData)()
        Dim categoryDescription As String = Nothing
        Dim subCategoryDescription As String = Nothing
        If (currentRevision.SubCategory IsNot Nothing) Then
            categoryDescription = currentRevision.SubCategory.Category.Description
            subCategoryDescription = currentRevision.SubCategory.Description
        End If
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.CATEGORY]", categoryDescription)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.SUBCATEGORY]", subCategoryDescription)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.TITEL]", currentRevision.Project.Name)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.XYZ]", currentRevision.RevisionInformation.RevisionNumber)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.OMSCHRIJVING]", currentRevision.ProjectDescription)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.INITIATOR]", currentRevision.Creator.ToString())
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.SPONSOR]", currentRevision.SponsorName)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.COORDINATOR]", currentRevision.CoordinatorName)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.EDITOR]", currentRevision.Creator.ToString())
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.PUSHER]", currentRevision.PusherName)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.BUDGET]", currentRevision.Budget)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.DATUM]", currentRevision.RevisionInformation.DateModified)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[PROJECT.EDITOR]", currentRevision.Creator.ToString())
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.STATUS]", currentRevision.RevisionInformation.RevisionStatus.Name)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.WAAROM]", currentRevision.Why)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.OBJECTIEVEN]", currentRevision.Objectifs)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.PROJECTINCLUDE]", currentRevision.Include)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.PROJECTEXCLUDE]", currentRevision.Exclude)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.ASIS]", currentRevision.AsIs)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.TOBE]", currentRevision.ToBe)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.KOSTEN]", currentRevision.Cost)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.BATEN]", currentRevision.Benefits)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.ALIGNATIONS]", currentRevision.Alignation)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.PRECONDITIONS]", currentRevision.Preconditions)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.CONSTRAINTS]", currentRevision.Constraints)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.DEPENDENCIES]", currentRevision.Dependencies)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.RISICOS]", currentRevision.Risk)
        ReplacePlaceHolderByString(mainDocumentPartDatalist, "[VERSION.SUPPORT]", currentRevision.Support)

        'history
        Dim properties As IList(Of String) = New List(Of String)()
        properties.Add("Id")
        properties.Add("RevisionInformation")
        Dim revisionHistory As IList(Of ProjectProposalRevision) = TaskProjectProposalRevision.Instance.GetAllRevisionsForProjectPrososalNameId(currentRevision.Project.Id, properties)
        Dim historyList As IList(Of String) = New List(Of String)()
        Dim revisionHistoryEnumerator As IEnumerator(Of ProjectProposalRevision) = revisionHistory.GetEnumerator()
        While (revisionHistoryEnumerator.MoveNext())
            historyList.Add(revisionHistoryEnumerator.Current.RevisionInformation.RevisionNumber)
            historyList.Add(revisionHistoryEnumerator.Current.RevisionInformation.DateModified.ToString())
            historyList.Add(revisionHistoryEnumerator.Current.RevisionInformation.RevisedBy)
            historyList.Add(revisionHistoryEnumerator.Current.RevisionInformation.Description)
        End While
        ReplacePlaceHolderByTable(mainDocumentPartDatalist, "[VERSION.CHANGEHISTORY]", historyList, New List(Of String)(New String() {"Version", "Revision Date", "Revised By", "Description"}))

        'distribution list
        Dim distributionList As IList(Of String) = New List(Of String)()
        distributionList.Add(String.Format("{0}, {1}", currentRevision.Auditor.Name, ""))
        distributionList.Add("Review")
        ReplacePlaceHolderByTable(mainDocumentPartDatalist, "[VERSION.DISTRIBUTIONLIST]", distributionList, New List(Of String)(New String() {"Name & Title", "Purpose"}))

        'teammembers and reporting
        Dim teamMemberList As IList(Of String) = New List(Of String)()
        Dim communicationList As IList(Of String) = New List(Of String)()
        Dim teamMemberEnumerator As IEnumerator(Of TeamMember) = currentRevision.TeamMembers.GetEnumerator()
        While (teamMemberEnumerator.MoveNext())
            'teammembers
            Dim rolename As String = Nothing
            If (teamMemberEnumerator.Current.Role IsNot Nothing) Then
                rolename = teamMemberEnumerator.Current.Role.Name
            End If
            teamMemberList.Add(rolename)
            teamMemberList.Add(teamMemberEnumerator.Current.EmployeeName)
            teamMemberList.Add(teamMemberEnumerator.Current.Department)
            teamMemberList.Add(teamMemberEnumerator.Current.EmailAddress)
            'communication
            communicationList.Add(teamMemberEnumerator.Current.EmployeeName)
            Dim reportTo As String = Nothing
            If (teamMemberEnumerator.Current.ReportTo IsNot Nothing) Then
                reportTo = teamMemberEnumerator.Current.ReportTo.EmployeeName
            End If
            communicationList.Add(reportTo)
        End While
        ReplacePlaceHolderByTable(mainDocumentPartDatalist, "[VERSION.TEAMLEDEN]", teamMemberList, New List(Of String)(New String() {"Role", "Contact (Name)", "Service/Dept", "e-Mail"}))
        ReplacePlaceHolderByTable(mainDocumentPartDatalist, "[VERSION.COMMUNICATIE]", communicationList, New List(Of String)(New String() {"Reporter", "Report To"}))

        'workpackages
        Dim workpackageList As IList(Of String) = New List(Of String)()
        Dim workpackageEnumerator As IEnumerator(Of WorkPackage) = currentRevision.WorkPackages.GetEnumerator()
        While (workpackageEnumerator.MoveNext())
            workpackageList.Add(workpackageEnumerator.Current.Name)
            workpackageList.Add(workpackageEnumerator.Current.EstimatedTime)
            workpackageList.Add(workpackageEnumerator.Current.ResponsibleName)
            workpackageList.Add(workpackageEnumerator.Current.DueDate.ToShortDateString())
        End While
        ReplacePlaceHolderByTable(mainDocumentPartDatalist, "[VERSION.WANNEER]", workpackageList, New List(Of String)(New String() {"Work Package", "Time estimated", "Responsible", "Due Date"}))
        'update text part
        Facade.Instance.WordFactory.Word.GenerateMainDocument(mainDocumentPartDatalist)

        'header
        Dim headerPartDatalist As IList(Of AbstractWordData) = New List(Of AbstractWordData)()
        ReplacePlaceHolderByString(headerPartDatalist, "[VERSION.TITEL]", currentRevision.Project.Name)
        'update header part
        Facade.Instance.WordFactory.Word.GenerateHeaderDocument(headerPartDatalist)

        'footer
        Dim footerPartDatalist As IList(Of AbstractWordData) = New List(Of AbstractWordData)()
        ReplacePlaceHolderByString(footerPartDatalist, "[VERSION.CATEGORY]", categoryDescription)
        ReplacePlaceHolderByString(footerPartDatalist, "[VERSION.SUBCATEGORY]", subCategoryDescription)
        'update footer part
        Facade.Instance.WordFactory.Word.GenerateFooterDocument(footerPartDatalist)
    End Sub

    Private Sub ReplacePlaceHolderByString(ByVal dataList As IList(Of AbstractWordData), ByVal find As String, ByVal replaceWith As String)
        dataList.Add(New WordTextData() With {.SearchValue = find, .ReplaceWith = replaceWith})
    End Sub

    Private Sub ReplacePlaceHolderByTable(ByVal dataList As IList(Of AbstractWordData), ByVal find As String, ByVal tableData As IList(Of String), ByVal tableHeaders As IList(Of String))
        dataList.Add(New WordTableData() With {.SearchValue = find, .TableData = tableData, .TableHeaders = tableHeaders})
    End Sub

    Private Sub ThrowException(ByVal message As String, ByVal exception As Exception)
        Throw New Exception(message, exception)
    End Sub
End Class
