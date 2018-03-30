Option Strict On
Option Explicit On
Option Infer Off
Imports Securex.PPF.Ppfclient.Business.DataHolders

Public Class TaskProjectProposalRevision
    Private Shared m_TaskProjectProposalRevision As TaskProjectProposalRevision = New TaskProjectProposalRevision()
    Private m_Selection As ProjectProposalRevisionSelection

    Private Sub New()
        InternalSelection = New ProjectProposalRevisionSelection()
    End Sub

    Public Shared ReadOnly Property Instance() As TaskProjectProposalRevision
        Get
            Return m_TaskProjectProposalRevision
        End Get
    End Property

    Public Sub Cancel()
        InternalSelection.Undo()
    End Sub

    Public Sub SelectProjectProposal(ByVal projectProposalRevision As ProjectProposalRevision)
        Try
            InternalSelection.Add(projectProposalRevision)
        Catch ex As Exception
            Throw New Exception("Unable to update projectProposal revision.")
        End Try
    End Sub

    Public Sub UpdateAuditorDescription(ByVal auditorDescription As String)
        Try
            InternalSelection.UpdateAuditorDescription(auditorDescription)
        Catch ex As Exception
            Throw New Exception("Unable to update auditor revision review description.")
        End Try
    End Sub

    Public Sub UpdateRevisionStatus(ByVal revisionStatus As RevisionStatus)
        Try
            InternalSelection.UpdateRevisionStatus(revisionStatus)
        Catch ex As Exception
            Throw New Exception("Unable to update revision status.")
        End Try
    End Sub

    Public Sub UpdateRevisionLastModified(ByVal dateModified As DateTime)
        Try
            InternalSelection.UpdateRevisionLastModified(dateModified)
        Catch ex As Exception
            Throw New Exception("Unable to update revision last modified.")
        End Try
    End Sub

    Public Sub UpdateProjectName(ByVal name As String)
        Try
            Dim projectName As ProjectProposalName = InternalSelection.GetSelection().Project
            projectName.Name = name
            InternalSelection.UpdateProjectProposalName(projectName)
        Catch ex As Exception
            Throw New Exception("Unable to update project name.")
        End Try
    End Sub

    Public Sub UpdateDescription(ByVal description As String)
        Try
            InternalSelection.UpdateDescription(description)
        Catch ex As Exception
            Throw New Exception("Unable to update description.")
        End Try
    End Sub

    Public Sub UpdateSubCategory(ByVal subCategory As SubCategory)
        Try
            subCategory.Category = TaskProjectProposalRevision.Instance.InternalSelection.GetSelection().SubCategory.Category
            InternalSelection.UpdateSubCategory(subCategory)
        Catch ex As Exception
            Throw New Exception("Unable to update subcategory.")
        End Try
    End Sub

    Public Sub UpdateCategory(ByVal category As Category)
        Try
            Dim subCategory As SubCategory = TaskProjectProposalRevision.Instance.InternalSelection.GetSelection().SubCategory
            If (subCategory Is Nothing) Then
                subCategory = New SubCategory()
            End If
            subCategory.Category = category
            InternalSelection.UpdateSubCategory(subCategory)
        Catch ex As Exception
            Throw New Exception("Unable to update category.")
        End Try
    End Sub

    Public Sub UpdateCreator(ByVal employeeId As String)
        Try
            Dim employee As Employee = TaskEmployee.Instance.GetEmployeeForEmployeeID(employeeId)
            Dim creator As Creator = New Creator()
            creator.EmailAddress = employee.EmailAddress
            creator.EmployeeId = employee.EmployeeId
            creator.Name = employee.Name
            creator.SirName = employee.SirName
            InternalSelection.UpdateCreator(creator)
        Catch ex As Exception
            Throw New Exception("Unable to update creator.")
        End Try
    End Sub

    Public Sub UpdateAuditor(ByVal employeeId As String)
        Try
            Dim employee As Employee = TaskEmployee.Instance.GetEmployeeForEmployeeID(employeeId)
            Dim auditor As Auditor = New Auditor()
            auditor.EmailAddress = employee.EmailAddress
            auditor.EmployeeId = employee.EmployeeId
            auditor.Name = employee.Name
            auditor.SirName = employee.SirName
            InternalSelection.UpdateAuditor(auditor)
        Catch ex As Exception
            Throw New Exception("Unable to update auditor.")
        End Try
    End Sub

    Public Sub UpdateSponsor(ByVal sponsorId As String, ByVal sponsorName As String)
        Try
            InternalSelection.UpdateSponsor(sponsorId, sponsorName)
        Catch ex As Exception
            Throw New Exception("Unable to update sponsor id and/or sponsor name.")
        End Try
    End Sub

    Public Sub UpdatePusher(ByVal pusherId As String, ByVal pusherName As String)
        Try
            InternalSelection.UpdatePusher(pusherId, pusherName)
        Catch ex As Exception
            Throw New Exception("Unable to update pusher id and/or pusher name.")
        End Try
    End Sub

    Public Sub UpdateCoordinator(ByVal coordinatorId As String, ByVal coordinatorName As String)
        Try
            InternalSelection.UpdateCoordinator(coordinatorId, coordinatorName)
        Catch ex As Exception
            Throw New Exception("Unable to update coordinator id and/or coordinator name.")
        End Try
    End Sub

    Public Sub UpdateBudget(ByVal budget As String)
        Try
            InternalSelection.UpdateBudget(budget)
        Catch ex As Exception
            Throw New Exception("Unable to update budget.")
        End Try
    End Sub

    Public Sub UpdateWhy(ByVal why As String)
        Try
            InternalSelection.UpdateWhy(why)
        Catch ex As Exception
            Throw New Exception("Unable to update why.")
        End Try
    End Sub

    Public Sub UpdateInclude(ByVal include As String)
        Try
            InternalSelection.UpdateInclude(include)
        Catch ex As Exception
            Throw New Exception("Unable to update include.")
        End Try
    End Sub

    Public Sub UpdateExclude(ByVal exclude As String)
        Try
            InternalSelection.UpdateExclude(exclude)
        Catch ex As Exception
            Throw New Exception("Unable to update exclude.")
        End Try
    End Sub

    Public Sub UpdateAsIs(ByVal asIs As String)
        Try
            InternalSelection.UpdateAsIs(asIs)
        Catch ex As Exception
            Throw New Exception("Unable to update asIs.")
        End Try
    End Sub

    Public Sub UpdateToBe(ByVal toBe As String)
        Try
            InternalSelection.UpdateToBe(toBe)
        Catch ex As Exception
            Throw New Exception("Unable to update toBe.")
        End Try
    End Sub

    Public Sub UpdateObjectifs(ByVal objectifs As String)
        Try
            InternalSelection.UpdateObjectifs(objectifs)
        Catch ex As Exception
            Throw New Exception("Unable to update objectifs.")
        End Try
    End Sub

    Public Sub UpdateCost(ByVal cost As String)
        Try
            InternalSelection.UpdateCost(cost)
        Catch ex As Exception
            Throw New Exception("Unable to update cost.")
        End Try
    End Sub

    Public Sub UpdateBenefits(ByVal benefits As String)
        Try
            InternalSelection.UpdateBenefits(benefits)
        Catch ex As Exception
            Throw New Exception("Unable to update benefits.")
        End Try
    End Sub

    Public Sub UpdateAlignation(ByVal alignation As String)
        Try
            InternalSelection.UpdateAlignation(alignation)
        Catch ex As Exception
            Throw New Exception("Unable to update alignation.")
        End Try
    End Sub

    Public Sub UpdatePreconditions(ByVal preconditions As String)
        Try
            InternalSelection.UpdatePreconditions(preconditions)
        Catch ex As Exception
            Throw New Exception("Unable to update preconditions.")
        End Try
    End Sub

    Public Sub UpdateConstraints(ByVal constraints As String)
        Try
            InternalSelection.UpdateConstraints(constraints)
        Catch ex As Exception
            Throw New Exception("Unable to update constraints.")
        End Try
    End Sub

    Public Sub UpdateDependencies(ByVal dependencies As String)
        Try
            InternalSelection.UpdateDependencies(dependencies)
        Catch ex As Exception
            Throw New Exception("Unable to update dependencies.")
        End Try
    End Sub

    Public Sub UpdateRisk(ByVal risk As String)
        Try
            InternalSelection.UpdateRisk(risk)
        Catch ex As Exception
            Throw New Exception("Unable to update risk.")
        End Try
    End Sub

    Public Sub UpdateSupport(ByVal support As String)
        Try
            InternalSelection.UpdateSupport(support)
        Catch ex As Exception
            Throw New Exception("Unable to update support.")
        End Try
    End Sub

    Public Sub UpdateRevisedBy(ByVal name As String)
        Try
            InternalSelection.UpdateRevisedBy(name)
        Catch ex As Exception
            Throw New Exception("Unable to update revised by.")
        End Try
    End Sub

    Public Sub AddOrUpdateWorkPackage(ByVal workPackage As WorkPackage)
        Try
            InternalSelection.AddOrUpdateWorkPackage(workPackage)
        Catch ex As Exception
            Throw New Exception("Unable to add/update workpackage.")
        End Try
    End Sub

    Public Sub RemoveWorkPackage(ByVal id As Integer)
        Try
            InternalSelection.RemoveWorkPackage(id)
        Catch ex As Exception
            Throw New Exception("Unable to remove workpackage.")
        End Try
    End Sub

    Public Sub AddOrUpdateMilestone(ByVal workPackageId As Integer, ByVal milestone As Milestone)
        Try
            InternalSelection.AddOrUpdateMilestone(workPackageId, milestone)
        Catch ex As Exception
            Throw New Exception("Unable to add/update workpackage.")
        End Try
    End Sub

    Public Sub RemoveMilestone(ByVal workPackageId As Integer, ByVal milestoneId As Integer)
        Try
            InternalSelection.RemoveMilestone(workPackageId, milestoneId)
        Catch ex As Exception
            Throw New Exception("Unable to remove workpackage.")
        End Try
    End Sub

    Public Sub AddOrUpdateTeamMember(ByVal teamMember As TeamMember)
        Try
            InternalSelection.AddOrUpdateTeamMember(teamMember)
        Catch ex As Exception
            Throw New Exception("Unable to add/update teammember.")
        End Try
    End Sub

    Public Sub RemoveTeamMember(ByVal id As Integer)
        Try
            InternalSelection.RemoveTeamMember(id)
        Catch ex As Exception
            Throw New Exception("Unable to remove teammember.")
        End Try
    End Sub

    Public Sub ClearSelection()
        Try
            InternalSelection.Clear()
        Catch ex As Exception
            Throw New Exception("Unable to clear selection.")
        End Try
    End Sub

    Public Sub Save(ByVal currentProjectProposalRevision As ProjectProposalRevision, ByVal newProjectProposalRevision As ProjectProposalRevision)
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Facade.Instance.RepositoryFactory.ProjectProposalRevision.Save(session, currentProjectProposalRevision)
            Dim savedProject As ProjectProposalRevision = Facade.Instance.RepositoryFactory.ProjectProposalRevision.Save(session, newProjectProposalRevision)
            InternalSelection.Clear()
            InternalSelection.Add(savedProject)
            session.CommitAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    Public Sub Save(ByVal currentProjectProposalRevision As ProjectProposalRevision)
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Dim savedProject As ProjectProposalRevision = Facade.Instance.RepositoryFactory.ProjectProposalRevision.Save(session, currentProjectProposalRevision)
            InternalSelection.Clear()
            InternalSelection.Add(savedProject)
            session.CommitAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    Public Function GetSelection() As ProjectProposalRevision
        Return InternalSelection.GetSelection()
    End Function

    Public Function GetAllRevisionsForProjectPrososalNameId(ByVal projectProposalNameId As Integer, ByVal properties As IList(Of String)) As IList(Of ProjectProposalRevision)
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            GetAllRevisionsForProjectPrososalNameId = Facade.Instance.RepositoryFactory.ProjectProposalRevision.GetAllRevisionsByProjectPrososalNameId(session, projectProposalNameId, properties)
            session.CancelAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Function

    Public Function GetProjectProposalRevisionById(ByVal id As Integer) As ProjectProposalRevision
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            GetProjectProposalRevisionById = Facade.Instance.RepositoryFactory.ProjectProposalRevision.GetProjectProposalRevisionById(session, id)(0)
            session.CancelAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Function

    Public Function GetCreator() As Creator
        Try
            Return InternalSelection.GetSelection().Creator
        Catch ex As Exception
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Function

    Public Function IsEditable() As Boolean
        Return TaskInformation.Instance.IsCreator(InternalSelection.GetSelection().Creator) AndAlso TaskProjectProposalRevision.Instance.GetSelection().RevisionInformation.RevisionStatus.Name.Equals(TaskRevisionStatus.DRAFT) AndAlso Not TaskRevisionStatus.Instance.IsReadOnly(InternalSelection.GetSelection())
    End Function

    Public Function IsReviewed() As Boolean
        Return TaskRevisionStatus.Instance.IsReviewing(InternalSelection.GetSelection())
    End Function

    Public Function IsRevisionStatusChanged() As Boolean
        Return InternalSelection.IsRevisionStatusChanged()
    End Function

    Public Function AreRequiredFieldsFilledIn() As Boolean
        Dim result As Boolean = InternalSelection.GetSelection().Project.Name IsNot Nothing
        result = result AndAlso InternalSelection.GetSelection().ProjectDescription IsNot Nothing
        result = result AndAlso InternalSelection.GetSelection().Creator.Name IsNot Nothing
        result = result AndAlso InternalSelection.GetSelection().Auditor.Name IsNot Nothing
        If (InternalSelection.GetSelection().SubCategory IsNot Nothing) Then
            result = result AndAlso InternalSelection.GetSelection().SubCategory.Category.Description IsNot Nothing
            result = result AndAlso InternalSelection.GetSelection().SubCategory.Description IsNot Nothing
        End If
        Return result
    End Function

    Public Function GetRequiredFieldNames() As IList(Of String)
        Dim result As IList(Of String) = New List(Of String)()
        result.Add("Project name")
        result.Add("Description")
        result.Add("Creator")
        result.Add("Auditor")
        If (InternalSelection.GetSelection().SubCategory IsNot Nothing) Then
            result.Add("Category")
            result.Add("SubCategory")
        End If
        Return result
    End Function

    Public ReadOnly Property GetWorkPackageById(ByVal id As Integer) As WorkPackage
        Get
            Return InternalSelection.GetWorkPackageById(id)
        End Get
    End Property

    Public ReadOnly Property GetMilestonesForWorkPackage(ByVal id As Integer) As IList(Of Milestone)
        Get
            Return InternalSelection.GetMilestonesForWorkPackage(id)
        End Get
    End Property

    Public ReadOnly Property GetMilestoneById(ByVal workpackageId As Integer, ByVal milestineId As Integer) As Milestone
        Get
            Return InternalSelection.GetMilestoneById(workpackageId, milestineId)
        End Get
    End Property

    Public ReadOnly Property GetTeamMemberById(ByVal id As Integer) As TeamMember
        Get
            Return InternalSelection.GetTeamMemberById(id)
        End Get
    End Property

    Public ReadOnly Property GetAllTeamMembers() As IList(Of TeamMember)
        Get
            Return InternalSelection.GetAllTeamMembers()
        End Get
    End Property

    Public ReadOnly Property GetCurrentProjectProposalRevision() As ProjectProposalRevision
        Get
            Return InternalSelection.GetSelection()
        End Get
    End Property

    Private Property InternalSelection() As ProjectProposalRevisionSelection
        Get
            Return m_Selection
        End Get
        Set(ByVal value As ProjectProposalRevisionSelection)
            m_Selection = value
        End Set
    End Property
End Class
