Option Strict On
Option Explicit On
Option Infer Off

Public Class Facade
    Private Shared m_Facade As Facade = New Facade()
    Private m_RepositoryFactory As IRepositoryFactory
    Private m_RunningEnvironmentFactory As IRunningEnvironment
    Private m_Information As IInformation
    Private m_WordFactory As IWordFactory

    Private Sub New()
    End Sub

    Public Shared ReadOnly Property Instance() As Facade
        Get
            Return m_Facade
        End Get
    End Property

    Public Property RepositoryFactory() As IRepositoryFactory
        Get
            Return m_RepositoryFactory
        End Get
        Set(ByVal value As IRepositoryFactory)
            m_RepositoryFactory = value
        End Set
    End Property

    Public Property RunningEnvironmentFactory() As IRunningEnvironment
        Get
            Return m_RunningEnvironmentFactory
        End Get
        Set(ByVal value As IRunningEnvironment)
            m_RunningEnvironmentFactory = value
        End Set
    End Property

    Public Property Information() As IInformation
        Get
            Return m_Information
        End Get
        Set(ByVal value As IInformation)
            m_Information = value
        End Set
    End Property

    Public Property WordFactory() As IWordFactory
        Get
            Return m_WordFactory
        End Get
        Set(ByVal value As IWordFactory)
            m_WordFactory = value
        End Set
    End Property
End Class
