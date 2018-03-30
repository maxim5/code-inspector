Option Strict On
Option Explicit On
Option Infer Off
Imports Securex.FIT.Fitclient.Business.DataHolders

Public Class TaskSubCategory
    Private Shared m_TaskSubCategory As TaskSubCategory = New TaskSubCategory()
    Private m_Selection As SubCategorySelection

    Private Sub New()
        Selection = New SubCategorySelection()
    End Sub

    Public Shared ReadOnly Property Instance() As TaskSubCategory
        Get
            Return m_TaskSubCategory
        End Get
    End Property

    ''' <summary>
    ''' Add this subcategory to the selection
    ''' </summary>
    ''' <param name="subCategory">Subcategory object</param>
    ''' <remarks></remarks>
    Public Sub Add(ByVal subCategory As SubCategory)
        Try
            Selection.Add(subCategory)
        Catch ex As Exception
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    ''' <summary>
    ''' Update description
    ''' </summary>
    ''' <param name="description">Subcategory description</param>
    ''' <remarks></remarks>
    Public Sub UpdateSubCategory(ByVal description As String)
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
    ''' Save the subcategory
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub Save()
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Facade.Instance.RepositoryFactory.SubCategory.Save(session, Selection.GetSelection())
            session.CommitAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    ''' <summary>
    ''' Remove the subcategory by id
    ''' </summary>
    ''' <param name="id"></param>
    ''' <remarks></remarks>
    Public Sub Remove(ByVal id As Integer)
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            Dim subCategories As IList(Of SubCategory) = Facade.Instance.RepositoryFactory.SubCategory.GetSubCategoryById(session, id)
            Facade.Instance.RepositoryFactory.SubCategory.Remove(session, subCategories(0))
            session.CommitAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Sub

    ''' <summary>
    ''' Get the subcategory for description
    ''' </summary>
    ''' <param name="description">Description of the subcategory</param>
    ''' <returns>Returns the requested subcatagory</returns>
    ''' <remarks></remarks>
    Public Function GetSubCategoryByDescription(ByVal description As String) As SubCategory
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            GetSubCategoryByDescription = New SubCategory()
            Dim subCategories As IList(Of SubCategory) = Facade.Instance.RepositoryFactory.SubCategory.GetSubCategoryByDescription(session, description)
            If (subCategories IsNot Nothing AndAlso subCategories.Count > 0) Then
                GetSubCategoryByDescription = subCategories(0)
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
    ''' Get all subcategories for the specified category id
    ''' </summary>
    ''' <param name="categoryId">Category id</param>
    ''' <returns>Returns an IList of subcatagories</returns>
    ''' <remarks></remarks>
    Public Function GetSubCategoriesByCategoryId(ByVal categoryId As Integer) As IList(Of SubCategory)
        Dim session As ISessionDao = Nothing
        Try
            session = Facade.Instance.RepositoryFactory.CreateSession()
            GetSubCategoriesByCategoryId = Facade.Instance.RepositoryFactory.SubCategory.GetCategoryByCategoryId(session, categoryId)
            session.CancelAndClose()
        Catch ex As RepositoryException
            If (session IsNot Nothing) Then
                session.CancelAndClose()
            End If
            Throw New Exception(ex.Message, ex.InnerException)
        End Try
    End Function

    Private Property Selection() As SubCategorySelection
        Get
            Return m_Selection
        End Get
        Set(ByVal value As SubCategorySelection)
            m_Selection = value
        End Set
    End Property
End Class
