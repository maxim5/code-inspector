Option Strict On
Option Explicit On
Option Infer Off
Imports Securex.FIT.Fitclient.Business.DataHolders

Public Class TaskFormDescription
    Private Shared m_TaskFormDescription As TaskFormDescription = New TaskFormDescription()
    Private m_Selection As DescriptionSelection

    Private Sub New()
        Selection = New DescriptionSelection()
    End Sub

    Public Shared ReadOnly Property Instance() As TaskFormDescription
        Get
            Return m_TaskFormDescription
        End Get
    End Property

    ''' <summary>
    ''' Add this formDescription to the selection
    ''' </summary>
    ''' <param name="formDescription">Formdescription object</param>
    ''' <remarks></remarks>
    Public Sub Add(ByVal formDescription As FormDescription)
        Try
            Selection.Add(formDescription)
        Catch ex As Exception
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    ''' <summary>
    ''' Update form description
    ''' </summary>
    ''' <param name="description">Form description</param>
    ''' <param name="formName">Form name</param>
    ''' <remarks></remarks>
    Public Sub UpdateFormDescription(ByVal description As String, ByVal formName As String)
        Try
            Selection.Update(description, formName)
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
    ''' Save
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub Save()
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Facade.Instance.RepositoryFactory.FormDescription.Save(session, Selection.GetSelection())
            session.CommitAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    ''' <summary>
    ''' Remove the form description by id
    ''' </summary>
    ''' <param name="id">Formdescription id</param>
    ''' <remarks></remarks>
    Public Sub Remove(ByVal id As Integer)
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Dim formDescriptions As IList(Of FormDescription) = Facade.Instance.RepositoryFactory.FormDescription.GetFormDescriptionById(session, id)
            Facade.Instance.RepositoryFactory.FormDescription.Remove(session, formDescriptions(0))
            session.CommitAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    ''' <summary>
    ''' Get all form descriptions
    ''' </summary>
    ''' <returns>Returns an IList of form descriptions</returns>
    ''' <remarks></remarks>
    Public Function GetAllFormDescriptions() As IList(Of FormDescription)
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            GetAllFormDescriptions = Facade.Instance.RepositoryFactory.FormDescription.GetAllFormDescriptions(session)
            session.CancelAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Function

    ''' <summary>
    ''' Get form description by name
    ''' </summary>
    ''' <param name="formName">Formdescription name</param>
    ''' <returns>Returns the requested formdescription</returns>
    ''' <remarks></remarks>
    Public Function GetDescriptionByFormName(ByVal formName As String) As FormDescription
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            GetDescriptionByFormName = New FormDescription()
            Dim formDescriptions As IList(Of FormDescription) = Facade.Instance.RepositoryFactory.FormDescription.GetFormDescriptionByFormName(session, formName)
            If (formDescriptions IsNot Nothing AndAlso formDescriptions.Count > 0) Then
                GetDescriptionByFormName = formDescriptions(0)
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
    ''' Get form description by id
    ''' </summary>
    ''' <param name="id">Formdescription id</param>
    ''' <returns>Returns the requested formdescription</returns>
    ''' <remarks></remarks>
    Public Function GetDescriptionById(ByVal id As Integer) As FormDescription
        Dim session As ISessionDao = Nothing
        Dim formDescription As FormDescription = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            GetDescriptionById = New FormDescription()
            Dim formDescriptions As IList(Of FormDescription) = Facade.Instance.RepositoryFactory.FormDescription.GetFormDescriptionById(session, id)
            If (formDescriptions IsNot Nothing AndAlso formDescriptions.Count > 0) Then
                GetDescriptionById = formDescriptions(0)
            End If
            session.CancelAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Function

    Private Property Selection() As DescriptionSelection
        Get
            Return m_Selection
        End Get
        Set(ByVal value As DescriptionSelection)
            m_Selection = value
        End Set
    End Property
End Class
