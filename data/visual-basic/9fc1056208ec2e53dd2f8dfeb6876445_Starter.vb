Option Strict On
Option Explicit On
Option Infer Off
Imports Securex.PPF.Ppfclient.Business
Imports Securex.PPF.Ppfclient.Data.SQL
Imports Securex.PPF.Ppfclient.Word2007
Imports Securex.PPF.Ppfclient.Gui
Imports Securex.CustomControls
Imports System
Imports System.Windows.Forms

Public Class Starter

    <STAThread()> _
    Shared Sub Main()
        Dim isDebugMode As Boolean
#If DEBUG Then
        isDebugMode = True
#End If
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(False)

        'authenticate user
        Dim username As String = Environment.UserName
        Dim applicationVersion As String = "V1.00"
        Dim applicationName As String = "PPF"
        Dim applicationId As String = "23"
        Dim checkLogin As CheckLogin = New CheckLogin(applicationName, applicationVersion, applicationId, isDebugMode, username)
        checkLogin.ShowDialog()

        If (checkLogin.IsAuthenticated) Then
            Dim facade As Facade = facade.Instance
            facade.RunningEnvironmentFactory = RunningEnvironment.Instance
            facade.RunningEnvironmentFactory.SetDevelopmentEnvironment = isDebugMode
            facade.RepositoryFactory = SqlFactory.Instance
            facade.Information = TaskInformation.Instance
            facade.Information.SetUserID = checkLogin.UserName
            facade.Information.SetApplicationName = applicationName
            facade.Information.SetApplicationVersion = applicationVersion
            facade.WordFactory = WordFactory.Instance
            checkLogin.Close()
            checkLogin.Dispose()

            'start application
            Application.Run(New FormMain())
        End If
    End Sub
End Class
