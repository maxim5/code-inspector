Option Strict On
Option Explicit On
Option Infer Off
Imports Securex.FIT.Fitclient.Business.DataHolders

Public Class TaskRole
    Private Shared m_TaskRole As TaskRole = New TaskRole()
    Private m_Selection As RoleSelection

    Private Sub New()
        Selection = New RoleSelection()
    End Sub

    Public Shared ReadOnly Property Instance() As TaskRole
        Get
            Return m_TaskRole
        End Get
    End Property

    ''' <summary>
    ''' Add this role to the selection
    ''' </summary>
    ''' <param name="role">Role object</param>
    ''' <remarks></remarks>
    Public Sub Add(ByVal role As Role)
        Try
            Selection.Add(role)
        Catch ex As Exception
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    ''' <summary>
    ''' Update name
    ''' </summary>
    ''' <param name="name">Role name</param>
    ''' <remarks></remarks>
    Public Sub UpdateName(ByVal name As String)
        Try
            Selection.Update(name)
        Catch ex As Exception
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    ''' <summary>
    ''' Clear the selection
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub ClearSelection()
        Try
            Selection.Clear()
        Catch ex As Exception
            Throw New Exception("Unable to clear selection.")
        End Try
    End Sub

    ''' <summary>
    ''' Save the category
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub Save()
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Facade.Instance.RepositoryFactory.Role.Save(session, Selection.GetSelection())
            session.CommitAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    ''' <summary>
    ''' Remove the role by id
    ''' </summary>
    ''' <param name="id">Id of the role</param>
    ''' <remarks></remarks>
    Public Sub Remove(ByVal id As Integer)
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Dim roles As IList(Of Role) = Facade.Instance.RepositoryFactory.Role.GetRoleById(session, id)
            Facade.Instance.RepositoryFactory.Role.Remove(session, roles(0))
            session.CommitAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    ''' <summary>
    ''' Get all roles
    ''' </summary>
    ''' <returns>Retuens an IList of roles</returns>
    ''' <remarks></remarks>
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

    ''' <summary>
    ''' Get the role for id
    ''' </summary>
    ''' <param name="id">Id of the role</param>
    ''' <returns>Returns the requested role</returns>
    ''' <remarks></remarks>
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

    ''' <summary>
    ''' Get the role for name
    ''' </summary>
    ''' <param name="name">Name of the role</param>
    ''' <returns>Returns the requested role</returns>
    ''' <remarks></remarks>
    Public Function GetRoleByName(ByVal name As String) As Role
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Dim roles As IList(Of Role) = Facade.Instance.RepositoryFactory.Role.GetRoleByName(session, name)
            GetRoleByName = New Role()
            If (roles IsNot Nothing AndAlso roles.Count > 0) Then
                GetRoleByName = roles(0)
            End If
            session.CancelAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Function

    Private Property Selection() As RoleSelection
        Get
            Return m_Selection
        End Get
        Set(ByVal value As RoleSelection)
            m_Selection = value
        End Set
    End Property
End Class
