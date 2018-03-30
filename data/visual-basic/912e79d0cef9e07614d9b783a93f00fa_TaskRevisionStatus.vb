Option Strict On
Option Explicit On
Option Infer Off
Imports Securex.PPF.Ppfclient.Business.DataHolders

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

    Public ReadOnly Property IsReadOnly(ByVal project As ProjectProposalRevision) As Boolean
        Get
            Return project.RevisionInformation.RevisionStatus.Name.Equals(RELEASED) OrElse project.RevisionInformation.RevisionStatus.Name.Equals(REJECTED) OrElse Not project.IsCurrentRevision
        End Get
    End Property

    Public ReadOnly Property IsReviewing(ByVal project As ProjectProposalRevision) As Boolean
        Get
            Return project.RevisionInformation.RevisionStatus.Name.Equals(REVIEW)
        End Get
    End Property

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
