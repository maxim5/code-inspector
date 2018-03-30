Option Strict On
Option Explicit On
Option Infer Off
Imports Securex.PPF.Ppfclient.Business.DataHolders

Public Class TaskRole
    Private Shared m_TaskRole As TaskRole = New TaskRole()

    Private Sub New()
    End Sub

    Public Shared ReadOnly Property Instance() As TaskRole
        Get
            Return m_TaskRole
        End Get
    End Property

    Public Function GetAllRoles() As IList(Of Role)
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            GetAllRoles = Facade.Instance.RepositoryFactory.Role.GetAllRoles(session)
            session.CancelAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Function

    Public Function GetRoleById(ByVal id As Integer) As Role
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Dim roles As IList(Of Role) = Facade.Instance.RepositoryFactory.Role.GetRoleById(session, id)
            If (roles IsNot Nothing AndAlso roles.Count > 0) Then
                GetRoleById = roles(0)
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
