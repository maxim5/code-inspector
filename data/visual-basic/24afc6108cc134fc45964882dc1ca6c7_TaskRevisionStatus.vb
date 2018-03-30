Option Strict On
Option Explicit On
Option Infer Off
Imports Securex.FIT.Fitclient.Business.DataHolders

Public Class TaskRevisionStatus
    Public Const DRAFT As String = "Draft"
    Public Const REVIEW As String = "Review"
    Public Const RELEASED As String = "Released"
    Public Const REJECTED As String = "Rejected"
    Private Shared m_TaskRevisionStatus As TaskRevisionStatus = New TaskRevisionStatus()

    Private Sub New()
    End Sub

    Public Shared ReadOnly Property Instance() As TaskRevisionStatus
        Get
            Return m_TaskRevisionStatus
        End Get
    End Property

    ''' <summary>
    ''' Is the project read only
    ''' </summary>
    ''' <param name="project">Project proposal revision object</param>
    ''' <returns>Returns true or false</returns>
    ''' <remarks></remarks>
    Public ReadOnly Property IsReadOnly(ByVal project As ProjectProposalRevision) As Boolean
        Get
            Return project.RevisionInformation.RevisionStatus.Name.Equals(RELEASED) OrElse project.RevisionInformation.RevisionStatus.Name.Equals(REJECTED) OrElse Not project.IsCurrentRevision
        End Get
    End Property

    ''' <summary>
    ''' Is the project status reviewing
    ''' </summary>
    ''' <param name="project">Project proposal revision object</param>
    ''' <returns>Returns true or false</returns>
    ''' <remarks></remarks>
    Public ReadOnly Property IsReviewing(ByVal project As ProjectProposalRevision) As Boolean
        Get
            Return project.RevisionInformation.RevisionStatus.Name.Equals(REVIEW)
        End Get
    End Property

    ''' <summary>
    ''' Get all revision statuses
    ''' </summary>
    ''' <returns>Returns as IList of revisionstatuses</returns>
    ''' <remarks></remarks>
    Public Function GetAllRevisionStatuses() As IList(Of RevisionStatus)
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            GetAllRevisionStatuses = Facade.Instance.RepositoryFactory.RevisionStatus.GetAllRevisionStatuses(session)
            session.CancelAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Function


    ''' <summary>
    ''' Get revision status by id
    ''' </summary>
    ''' <param name="id">Id of the revisionstatus</param>
    ''' <returns>Returns the requested revisionstatus</returns>
    ''' <remarks></remarks>
    Public Function GetRevisionStatusById(ByVal id As Integer) As RevisionStatus
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Dim revisionStatuses As IList(Of RevisionStatus) = Facade.Instance.RepositoryFactory.RevisionStatus.GetRevisionStatusById(session, id)
            GetRevisionStatusById = New RevisionStatus()
            If (revisionStatuses IsNot Nothing AndAlso revisionStatuses.Count > 0) Then
                GetRevisionStatusById = revisionStatuses(0)
            End If
            session.CancelAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Function

    ''' <summary>
    ''' Get revision status by name
    ''' </summary>
    ''' <param name="name">Name of the revisionstatus</param>
    ''' <returns>Returns the requested revisionstatus</returns>
    ''' <remarks></remarks>
    Public Function GetRevisionStatusByName(ByVal name As String) As RevisionStatus
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Dim revisionStatuses As IList(Of RevisionStatus) = Facade.Instance.RepositoryFactory.RevisionStatus.GetRevisionStatusByName(session, name)
            GetRevisionStatusByName = New RevisionStatus()
            If (revisionStatuses IsNot Nothing AndAlso revisionStatuses.Count > 0) Then
                GetRevisionStatusByName = revisionStatuses(0)
            End If
            session.CancelAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Function
End Class
