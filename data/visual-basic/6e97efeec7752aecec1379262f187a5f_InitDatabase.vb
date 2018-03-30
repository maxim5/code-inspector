Imports System.IO
Imports System.Data.SqlClient
Imports Securex.FIT.Fitclient.Data.SQL
Imports Securex.FIT.Fitclient.Business

Public Class InitDatabase
    Private Shared m_InitDatabase As InitDatabase = New InitDatabase()
    Private m_DatabaseScript As String

    Private Sub New()
    End Sub

    Public Shared ReadOnly Property Instance() As InitDatabase
        Get
            Return m_InitDatabase
        End Get
    End Property

    Public Sub Initialize()
        LoadDatabaseScript()
        IntializeDatabase()

        Dim username As String = "8393"
        Dim applicationVersion As String = "V1.00"
        Dim applicationName As String = "FIT"
        Dim applicationId As String = "23"

        Dim facade As Facade = facade.Instance
        facade.RunningEnvironmentFactory = RunningEnvironment.Instance
        Dim environment As String = RunningEnvironment.PRODUCTION
        facade.RunningEnvironmentFactory.SetEnvironment = RunningEnvironment.UNITTESTING
        facade.RepositoryFactory = SqlFactory.Instance
    End Sub

    Public Sub IntializeDatabase()
        Dim connenctionString As String = "Data Source=10.40.10.188,1433;Network Library=DBMSSOCN;User ID=usDEV;Password=DEV"
        Dim connection As SqlConnection = Nothing
        Try
            connection = New SqlConnection(connenctionString)
            connection.Open()
            Dim command As SqlCommand = connection.CreateCommand()
            command.Connection = connection
            'drop database
            command.CommandText = "if exists(select name from master.dbo.sysdatabases where name = N'FIT_Test') drop database [FIT_Test]" '" if exists PPF_Test drop database" '"if exist(sselect * from sys.databases where name = PPF_Test) drop database [PPF_Test]"
            command.ExecuteNonQuery()
            'create new database
            command.CommandText = "create database [FIT_Test] collate Latin1_General_CI_AS"
            command.ExecuteNonQuery()
            'select database
            command.CommandText = "use [FIT_Test]"
            command.ExecuteNonQuery()
            'create tables
            command.CommandText = DatabaseScript
            command.ExecuteNonQuery()
        Catch ex As Exception
            'ignore
        Finally
            Try
                If (connection IsNot Nothing) Then
                    connection.Close()
                End If
            Catch ex As Exception
                'ignore
            End Try
        End Try
    End Sub

    Private Sub LoadDatabaseScript()
        Dim dte As EnvDTE.DTE = System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE")
        Dim solutionPath As String = dte.Solution.FullName
        solutionPath = solutionPath.Substring(0, solutionPath.Length - (solutionPath.Split("\").Last().Length + 1))
        DatabaseScript = File.OpenText(String.Format("{0}\fit.sql", solutionPath)).ReadToEnd()
        DatabaseScript = DatabaseScript.Replace("GO", "")
    End Sub

    Private Property DatabaseScript() As String
        Get
            Return m_DatabaseScript
        End Get
        Set(ByVal value As String)
            m_DatabaseScript = value
        End Set
    End Property
End Class
