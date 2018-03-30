Public Class Generic

    Public name As String
    Public data_type As DataType
    Public ini_value As String
    Public Sub New()
        Me.name = ""
        ini_value = ""
    End Sub

    Public Sub New(ByVal name As String, ByVal data_type As DataType)
        Me.name = name
        Me.data_type = data_type
    End Sub

    Public Overrides Function ToString() As String
        Dim str As String
        str = name + " : " + data_type.usgString
        If ini_value <> "" Then
            str += " := " + ini_value
        End If
        Return str
    End Function
End Class
