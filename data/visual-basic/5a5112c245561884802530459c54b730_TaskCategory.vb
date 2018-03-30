Option Strict On
Option Explicit On
Option Infer Off
Imports Securex.FIT.Fitclient.Business
Imports Securex.FIT.Fitclient.Business.DataHolders

Public Class TaskCategory
    Private Shared m_TaskCategory As TaskCategory = New TaskCategory()
    Private m_Selection As CategorySelection

    Private Sub New()
        Selection = New CategorySelection()
    End Sub

    Public Shared ReadOnly Property Instance() As TaskCategory
        Get
            Return m_TaskCategory
        End Get
    End Property

    ''' <summary>
    ''' Add this category to the selection
    ''' </summary>
    ''' <param name="category">Category object</param>
    ''' <remarks></remarks>
    Public Sub Add(ByVal category As Category)
        Try
            Selection.Add(category)
        Catch ex As Exception
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    ''' <summary>
    ''' Update description
    ''' </summary>
    ''' <param name="description">Category description</param>
    ''' <remarks></remarks>
    Public Sub UpdateCategory(ByVal description As String)
        Try
            Selection.Update(description)
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
            Facade.Instance.RepositoryFactory.Category.Save(session, Selection.GetSelection())
            session.CommitAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    ''' <summary>
    ''' Remove the category by id
    ''' </summary>
    ''' <param name="id">Id of the catagory</param>
    ''' <remarks></remarks>
    Public Sub Remove(ByVal id As Integer)
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Dim categories As IList(Of Category) = Facade.Instance.RepositoryFactory.Category.GetCategoryById(session, id)
            Facade.Instance.RepositoryFactory.Category.Remove(session, categories(0))
            session.CommitAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    ''' <summary>
    ''' Get all categories
    ''' </summary>
    ''' <returns>Returns an IList of categories</returns>
    ''' <remarks></remarks>
    Public Function GetAllCategories() As IList(Of Category)
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            GetAllCategories = Facade.Instance.RepositoryFactory.Category.GetAllCategories(session)
            session.CancelAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Function

    ''' <summary>
    ''' Get category by id
    ''' </summary>
    ''' <param name="id">Category id</param>
    ''' <returns>Returns the requested catagory</returns>
    ''' <remarks></remarks>
    Public Function GetCategoryById(ByVal id As Integer) As Category
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Dim categories As IList(Of Category) = Facade.Instance.RepositoryFactory.Category.GetCategoryById(session, id)
            GetCategoryById = New Category()
            If (categories IsNot Nothing AndAlso categories.Count > 0) Then
                GetCategoryById = categories(0)
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
    ''' Get the category for description
    ''' </summary>
    ''' <param name="description">Description of the category</param>
    ''' <returns>Returns the requested catagory</returns>
    ''' <remarks></remarks>
    Public Function GetCategoryByDescription(ByVal description As String) As Category
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Dim categories As IList(Of Category) = Facade.Instance.RepositoryFactory.Category.GetCategoryByDescription(session, description)
            GetCategoryByDescription = New Category()
            If (categories IsNot Nothing AndAlso categories.Count > 0) Then
                GetCategoryByDescription = categories(0)
            End If
            session.CancelAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Function

    Private Property Selection() As CategorySelection
        Get
            Return m_Selection
        End Get
        Set(ByVal value As CategorySelection)
            m_Selection = value
        End Set
    End Property
End Class
