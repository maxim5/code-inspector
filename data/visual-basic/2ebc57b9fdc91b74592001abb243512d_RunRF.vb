#Region " Imports "
Imports System.IO
Imports Microsoft.Win32
Imports System.Diagnostics
Imports System.Runtime.InteropServices
Imports System.Data.SqlClient
Imports System.Data
Imports System.Data.OleDb
Imports System.Net
Imports System.Net.Mail
Imports System.Text
Imports System.Reflection
Imports Microsoft.Win32.SafeHandles
Imports System.ComponentModel
Imports System.Math
Imports AIOTData
#End Region

Module RunRF

#Region "Running"
    Sub linkinalterable()
        Try
            Process.Start("http://o2psoftlabsportal.comeze.com/")
        Catch weberrt As System.Net.WebException
        Catch except As Exception
        End Try
    End Sub

    Public Function GotInternet() As Boolean
        Dim req As System.Net.HttpWebRequest
        Dim res As System.Net.HttpWebResponse
        GotInternet = False
        Try
            req = CType(System.Net.HttpWebRequest.Create("http://o2psoftlabshq.comyr.com/"), System.Net.HttpWebRequest)
            res = CType(req.GetResponse(), System.Net.HttpWebResponse)
            req.Abort()
            If res.StatusCode = System.Net.HttpStatusCode.OK Then
                GotInternet = True
            End If
        Catch weberrt As System.Net.WebException
            GotInternet = False
        Catch except As Exception
            GotInternet = False
        End Try
    End Function

    Sub connection()
        If GotInternet() = True Then
            MessageBox.Show("You Are Connected Internet!", "Status", MessageBoxButtons.OK, MessageBoxIcon.Information)
            updateprograms()
            AIOTFRF.UpdateToolStripMenuItem.Enabled = True
            AIOTFRF.FeatureToolStripMenuItem.Enabled = True
        Else
            MessageBox.Show("You Not Connected Internet!Please Connect For Future Update", "Status", MessageBoxButtons.OK, MessageBoxIcon.Warning)
            AIOTFRF.UpdateToolStripMenuItem.Enabled = False
            AIOTFRF.FeatureToolStripMenuItem.Enabled = False
        End If
    End Sub
#End Region

#Region "update"
    Sub updateprograms()
        Dim link As New AIOTData.Running
        Dim webupdate As New WebClient
        Dim update As String = webupdate.DownloadString("http://o2psoftlabshq.comyr.com/connection/update/updateAIOT.inf")
        If update.Contains(Application.ProductVersion) Then
            MessageBox.Show("Product Already Update", "Attention", MessageBoxButtons.OK, MessageBoxIcon.Information)
            AIOTFRF.UpdateProgramToolStripMenuItem.Visible = False
            linkinalterable()
        Else
            MessageBox.Show("Update Program Available!Please Update For Protect!", "Warning", MessageBoxButtons.OK, MessageBoxIcon.Warning)
            AIOTFRF.UpdateProgramToolStripMenuItem.Visible = True
            forums()
        End If
    End Sub
#End Region

#Region "link"
    Sub forums()
        Try
            Process.Start("http://o2psoftlabsforums.8forum.net/t37-autopatcherupdate-feature")
        Catch weberrt As System.Net.WebException
        Catch except As Exception
        End Try
    End Sub
#End Region

End Module
