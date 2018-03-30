Imports MatrixDataStructures

'For Integer Matrices only. This seperation of int/float saves time for most operation.
Public Class FloodFillMatrix3DInt16

    '6/26 Way Recursive FloodFill Algorithm
    'The function will keep calling itself more and more until eventually all pixels are filled. 
    '26 Way Algorithm will pass though pixel boundaries 1 pixel thick.


    Dim newValue, oldValue As Int16 'values to replace if necessary
    Dim boxX, boxY, boxZ As Integer 'value of BOX[x=width,y=height,z=depth] within which values should be replaced

    'We use the recursive algorithm only since we expect boxes to be small. If there is a out of memory exception, we will have to choose a different method.
    'Help from http://student.kuleuven.be/~m0216922/CG/floodfill.html

    Public Enum FloodFillType
        WhereValueMatches = 1
        InsideABox = 2
    End Enum

    Dim CurrentFloodFillingType As New FloodFillType

    Sub New(ByVal newValueToBePutIn As Int16, ByVal oldValueToReplace As Int16)
        Me.newValue = newValueToBePutIn
        Me.oldValue = oldValueToReplace
        Me.boxX = 0
        Me.boxY = 0
        Me.boxZ = 0
        Me.CurrentFloodFillingType = FloodFillType.WhereValueMatches
    End Sub
    Sub New(ByVal boxX As Integer, ByVal boxY As Integer, ByVal boxZ As Integer, ByVal valuetoDetect As Int16, ByVal ValuetoPutIn As Int16)
        Me.boxX = boxX
        Me.boxY = boxY
        Me.boxZ = boxZ
        Me.newValue = ValuetoPutIn
        Me.oldValue = valuetoDetect
        Me.CurrentFloodFillingType = FloodFillType.InsideABox
    End Sub
    Sub New()

    End Sub

    Public Sub FloodFill6(ByRef Data As Matrix3DInt16, ByVal Seedx As Integer, ByVal Seedy As Integer, ByVal Seedz As Integer)

        If Seedx > Data.x - 1 Or Seedx < 0 Then Exit Sub
        If Seedy > Data.y - 1 Or Seedy < 0 Then Exit Sub
        If Seedz > Data.z - 1 Or Seedz < 0 Then Exit Sub

        Select Case Me.CurrentFloodFillingType
            Case FloodFillType.InsideABox
                Dim Detected As Boolean = True
                'First detect that the new gap matches the size of Box
                For i As Integer = 0 To boxX - 1
                    For j As Integer = 0 To boxY - 1
                        For k As Integer = 0 To boxZ - 1
                            If Data(Seedx + i, Seedy + j, Seedz + k) <> oldValue Then Detected = False
                        Next
                    Next
                Next
                'If it is of the size of the box or bigger
                If Detected Then
                    Me.CurrentFloodFillingType = FloodFillType.WhereValueMatches
                    FloodFill6(Data, Seedx, Seedy, Seedz)
                    Me.CurrentFloodFillingType = FloodFillType.InsideABox 'reset
                End If

            Case FloodFillType.WhereValueMatches
                If Data(Seedx, Seedy, Seedz) = Me.oldValue Then Data(Seedx, Seedy, Seedz) = Me.newValue
                Me.FloodFill6(Data, Seedx + 1, Seedy, Seedz)
                Me.FloodFill6(Data, Seedx - 1, Seedy, Seedz)
                Me.FloodFill6(Data, Seedx, Seedy + 1, Seedz)
                Me.FloodFill6(Data, Seedx, Seedy - 1, Seedz)
                Me.FloodFill6(Data, Seedx, Seedy, Seedz + 1)
                Me.FloodFill6(Data, Seedx, Seedy, Seedz - 1)
        End Select
    End Sub

    Public Sub FloodFill26(ByRef Data As Matrix3DInt16, ByVal Seedx As Integer, ByVal Seedy As Integer, ByVal Seedz As Integer)

        If Seedx > Data.x - 1 Or Seedx < 0 Then Exit Sub
        If Seedy > Data.y - 1 Or Seedy < 0 Then Exit Sub
        If Seedz > Data.z - 1 Or Seedz < 0 Then Exit Sub

        Select Case Me.CurrentFloodFillingType
            Case FloodFillType.InsideABox
                Dim Detected As Boolean = True
                'First detect that the new gap matches the size of Box
                For i As Integer = 0 To boxX - 1
                    For j As Integer = 0 To boxY - 1
                        For k As Integer = 0 To boxZ - 1
                            If Data(Seedx + i, Seedy + j, Seedz + k) <> oldValue Then Detected = False
                        Next
                    Next
                Next
                'If it is of the size of the box or bigger
                If Detected Then
                    Me.CurrentFloodFillingType = FloodFillType.WhereValueMatches
                    FloodFill26(Data, Seedx, Seedy, Seedz)
                    Me.CurrentFloodFillingType = FloodFillType.InsideABox 'reset
                End If

            Case FloodFillType.WhereValueMatches
                If Data(Seedx, Seedy, Seedz) = Me.oldValue Then Data(Seedx, Seedy, Seedz) = Me.newValue
                '6 from before
                Me.FloodFill26(Data, Seedx + 1, Seedy, Seedz)
                Me.FloodFill26(Data, Seedx - 1, Seedy, Seedz)
                Me.FloodFill26(Data, Seedx, Seedy + 1, Seedz)
                Me.FloodFill26(Data, Seedx, Seedy - 1, Seedz)
                Me.FloodFill26(Data, Seedx, Seedy, Seedz + 1)
                Me.FloodFill26(Data, Seedx, Seedy, Seedz - 1)
                'now all diagonals
                Me.FloodFill26(Data, Seedx - 1, Seedy + 1, Seedz)
                Me.FloodFill26(Data, Seedx - 1, Seedy - 1, Seedz)
                Me.FloodFill26(Data, Seedx - 1, Seedy, Seedz + 1)
                Me.FloodFill26(Data, Seedx - 1, Seedy, Seedz - 1)
                Me.FloodFill26(Data, Seedx - 1, Seedy + 1, Seedz + 1)
                Me.FloodFill26(Data, Seedx - 1, Seedy - 1, Seedz - 1)
                Me.FloodFill26(Data, Seedx - 1, Seedy + 1, Seedz - 1)
                Me.FloodFill26(Data, Seedx - 1, Seedy - 1, Seedz + 1)
                Me.FloodFill26(Data, Seedx, Seedy + 1, Seedz + 1)
                Me.FloodFill26(Data, Seedx, Seedy + 1, Seedz - 1)
                Me.FloodFill26(Data, Seedx, Seedy - 1, Seedz - 1)
                Me.FloodFill26(Data, Seedx, Seedy - 1, Seedz + 1)
                Me.FloodFill26(Data, Seedx + 1, Seedy + 1, Seedz)
                Me.FloodFill26(Data, Seedx + 1, Seedy - 1, Seedz)
                Me.FloodFill26(Data, Seedx + 1, Seedy, Seedz + 1)
                Me.FloodFill26(Data, Seedx + 1, Seedy, Seedz - 1)
                Me.FloodFill26(Data, Seedx + 1, Seedy + 1, Seedz + 1)
                Me.FloodFill26(Data, Seedx + 1, Seedy + 1, Seedz - 1)
                Me.FloodFill26(Data, Seedx + 1, Seedy - 1, Seedz - 1)
                Me.FloodFill26(Data, Seedx + 1, Seedy - 1, Seedz + 1)
        End Select
    End Sub
    Public Sub JustFillBlobs(ByRef data As Matrix3DInt16, ByRef CSF As Matrix3DInt16, ByVal VolumeThreshold As Integer, Optional ByVal Cutoff As Single = 0.75)
        Dim KernelSize As Integer = 5 'In the kernel check for number of grey and WM probability and remove blob only when probability of WM  > 0.75
        Dim BlobInformation As New Matrix3DSingle(data.x, data.y, data.z)
        Dim BlobCounter As Long = 1
        For i As Integer = 0 To data.x - 1
            For j As Integer = 0 To data.y - 1
                For k As Integer = 0 To data.z - 1
                    If data(i, j, k) = 0 And CSF(i, j, k) <> 1 Then
                        If BlobInformation(i, j, k) = 0 Then
                            BlobInformation(i, j, k) = BlobCounter
                            BlobCounter += 1
                        Else
                            'already labelled.. copy label to current and put other labels
                        End If
                        'check neighbours
                        If i < 1 Or i > data.x - 2 Or j < 1 Or j > data.y - 2 Or k < 1 Or k > data.z - 2 Then Exit For
                        If data(i - 1, j - 1, k - 1) = 0 Then BlobInformation(i - 1, j - 1, k - 1) = BlobInformation(i, j, k)
                        If data(i - 1, j - 1, k) = 0 Then BlobInformation(i - 1, j - 1, k) = BlobInformation(i, j, k)
                        If data(i - 1, j - 1, k + 1) = 0 Then BlobInformation(i - 1, j - 1, k + 1) = BlobInformation(i, j, k)
                        If data(i - 1, j, k - 1) = 0 Then BlobInformation(i - 1, j, k - 1) = BlobInformation(i, j, k)
                        If data(i - 1, j, k) = 0 Then BlobInformation(i - 1, j, k) = BlobInformation(i, j, k)
                        If data(i - 1, j, k + 1) = 0 Then BlobInformation(i - 1, j, k + 1) = BlobInformation(i, j, k)
                        If data(i - 1, j + 1, k - 1) = 0 Then BlobInformation(i - 1, j + 1, k - 1) = BlobInformation(i, j, k)
                        If data(i - 1, j + 1, k) = 0 Then BlobInformation(i - 1, j + 1, k) = BlobInformation(i, j, k)
                        If data(i - 1, j + 1, k + 1) = 0 Then BlobInformation(i - 1, j + 1, k + 1) = BlobInformation(i, j, k)

                        If data(i, j - 1, k - 1) = 0 Then BlobInformation(i, j - 1, k - 1) = BlobInformation(i, j, k)
                        If data(i, j - 1, k) = 0 Then BlobInformation(i, j - 1, k) = BlobInformation(i, j, k)
                        If data(i, j - 1, k + 1) = 0 Then BlobInformation(i, j - 1, k + 1) = BlobInformation(i, j, k)
                        If data(i, j, k - 1) = 0 Then BlobInformation(i, j, k - 1) = BlobInformation(i, j, k)
                        If data(i, j, k) = 0 Then BlobInformation(i, j, k) = BlobInformation(i, j, k)
                        If data(i, j, k + 1) = 0 Then BlobInformation(i, j, k + 1) = BlobInformation(i, j, k)
                        If data(i, j + 1, k - 1) = 0 Then BlobInformation(i, j + 1, k - 1) = BlobInformation(i, j, k)
                        If data(i, j + 1, k) = 0 Then BlobInformation(i, j + 1, k) = BlobInformation(i, j, k)
                        If data(i, j + 1, k + 1) = 0 Then BlobInformation(i, j + 1, k + 1) = BlobInformation(i, j, k)

                        If data(i + 1, j - 1, k - 1) = 0 Then BlobInformation(i + 1, j - 1, k - 1) = BlobInformation(i, j, k)
                        If data(i + 1, j - 1, k) = 0 Then BlobInformation(i + 1, j - 1, k) = BlobInformation(i, j, k)
                        If data(i + 1, j - 1, k + 1) = 0 Then BlobInformation(i + 1, j - 1, k + 1) = BlobInformation(i, j, k)
                        If data(i + 1, j, k - 1) = 0 Then BlobInformation(i + 1, j, k - 1) = BlobInformation(i, j, k)
                        If data(i + 1, j, k) = 0 Then BlobInformation(i + 1, j, k) = BlobInformation(i, j, k)
                        If data(i + 1, j, k + 1) = 0 Then BlobInformation(i + 1, j, k + 1) = BlobInformation(i, j, k)
                        If data(i + 1, j + 1, k - 1) = 0 Then BlobInformation(i + 1, j + 1, k - 1) = BlobInformation(i, j, k)
                        If data(i + 1, j + 1, k) = 0 Then BlobInformation(i + 1, j + 1, k) = BlobInformation(i, j, k)
                        If data(i + 1, j + 1, k + 1) = 0 Then BlobInformation(i + 1, j + 1, k + 1) = BlobInformation(i, j, k)
                    End If
                Next
            Next
        Next
        Dim BlobLength(BlobCounter + 1) As Long
        For i As Integer = 0 To data.x - 1
            For j As Integer = 0 To data.y - 1
                For k As Integer = 0 To data.z - 1
                    If BlobInformation(i, j, k) <> 0 Then BlobLength(BlobInformation(i, j, k)) += 1
                Next
            Next
        Next
        'now that we have length of all blobs
        Dim gmcount As Integer = 0
        Dim wmcount As Integer = 0
        For i As Integer = KernelSize To data.x - 1 - KernelSize
            For j As Integer = KernelSize To data.y - 1 - KernelSize
                For k As Integer = KernelSize To data.z - 1 - KernelSize
                    If BlobLength(BlobInformation(i, j, k)) < VolumeThreshold And CSF(i, j, k) <> 1 Then
                        'In a kernel window check the probability of WM & GM so that you dont remove edges at Gm WM boundary
                        For l As Integer = -1 * KernelSize To KernelSize
                            For m As Integer = -1 * KernelSize To KernelSize
                                For n As Integer = -1 * KernelSize To KernelSize
                                    If data(i + l, j + m, k + n) = 1 Then wmcount += 1 Else gmcount += 1
                                Next
                            Next
                        Next
                        If wmcount / (wmcount + gmcount) > Cutoff Then data(i, j, k) = 1
                    End If
                Next
            Next
        Next
        BlobInformation.Dispose()
    End Sub
    
End Class

'Exact Copy, but for Decimal Matrices
Public Class FloodFillMatrix3DSingle

    '6/26 Way Recursive FloodFill Algorithm
    'The function will keep calling itself more and more until eventually all pixels are filled. 
    '26 Way Algorithm will pass though pixel boundaries 1 pixel thick.


    Dim newValue, oldValue As Single 'values to replace if necessary
    Dim boxX, boxY, boxZ As Integer 'value of BOX[x=width,y=height,z=depth] within which values should be replaced

    'We use the recursive algorithm only since we expect boxes to be small. If there is a out of memory exception, we will have to choose a different method.
    'Help from http://student.kuleuven.be/~m0216922/CG/floodfill.html

    Public Enum FloodFillType
        WhereValueMatches = 1
        InsideABox = 2
    End Enum

    Dim CurrentFloodFillingType As New FloodFillType

    Sub New(ByVal newValueToBePutIn As Single, ByVal oldValueToReplace As Single)
        Me.newValue = newValueToBePutIn
        Me.oldValue = oldValueToReplace
        Me.boxX = 0
        Me.boxY = 0
        Me.boxZ = 0
        Me.CurrentFloodFillingType = FloodFillType.WhereValueMatches
    End Sub
    Sub New(ByVal boxX As Integer, ByVal boxY As Integer, ByVal boxZ As Integer, ByVal valuetoDetect As Single, ByVal ValuetoPutIn As Single)
        Me.boxX = boxX
        Me.boxY = boxY
        Me.boxZ = boxZ
        Me.newValue = ValuetoPutIn
        Me.oldValue = valuetoDetect
        Me.CurrentFloodFillingType = FloodFillType.InsideABox
    End Sub

    Public Sub FloodFill6(ByRef Data As Matrix3DSingle, ByVal Seedx As Integer, ByVal Seedy As Integer, ByVal Seedz As Integer)

        If Seedx > Data.x - 1 Or Seedx < 0 Then Exit Sub
        If Seedy > Data.y - 1 Or Seedy < 0 Then Exit Sub
        If Seedz > Data.z - 1 Or Seedz < 0 Then Exit Sub

        Select Case Me.CurrentFloodFillingType
            Case FloodFillType.InsideABox
                Dim Detected As Boolean = True
                'First detect that the new gap matches the size of Box
                For i As Integer = 0 To boxX - 1
                    For j As Integer = 0 To boxY - 1
                        For k As Integer = 0 To boxZ - 1
                            If Data(Seedx + i, Seedy + j, Seedz + k) <> oldValue Then Detected = False
                        Next
                    Next
                Next
                'If it is of the size of the box or bigger
                If Detected Then
                    Me.CurrentFloodFillingType = FloodFillType.WhereValueMatches
                    FloodFill6(Data, Seedx, Seedy, Seedz)
                    Me.CurrentFloodFillingType = FloodFillType.InsideABox 'reset
                End If

            Case FloodFillType.WhereValueMatches
                If Data(Seedx, Seedy, Seedz) = Me.oldValue Then Data(Seedx, Seedy, Seedz) = Me.newValue
                Me.FloodFill6(Data, Seedx + 1, Seedy, Seedz)
                Me.FloodFill6(Data, Seedx - 1, Seedy, Seedz)
                Me.FloodFill6(Data, Seedx, Seedy + 1, Seedz)
                Me.FloodFill6(Data, Seedx, Seedy - 1, Seedz)
                Me.FloodFill6(Data, Seedx, Seedy, Seedz + 1)
                Me.FloodFill6(Data, Seedx, Seedy, Seedz - 1)
        End Select
    End Sub

    Public Sub FloodFill26(ByRef Data As Matrix3DSingle, ByVal Seedx As Integer, ByVal Seedy As Integer, ByVal Seedz As Integer)

        If Seedx > Data.x - 1 Or Seedx < 0 Then Exit Sub
        If Seedy > Data.y - 1 Or Seedy < 0 Then Exit Sub
        If Seedz > Data.z - 1 Or Seedz < 0 Then Exit Sub

        Select Case Me.CurrentFloodFillingType
            Case FloodFillType.InsideABox
                Dim Detected As Boolean = True
                'First detect that the new gap matches the size of Box
                For i As Integer = 0 To boxX - 1
                    For j As Integer = 0 To boxY - 1
                        For k As Integer = 0 To boxZ - 1
                            If Data(Seedx + i, Seedy + j, Seedz + k) <> oldValue Then Detected = False
                        Next
                    Next
                Next
                'If it is of the size of the box or bigger
                If Detected Then
                    Me.CurrentFloodFillingType = FloodFillType.WhereValueMatches
                    FloodFill26(Data, Seedx, Seedy, Seedz)
                    Me.CurrentFloodFillingType = FloodFillType.InsideABox 'reset
                End If

            Case FloodFillType.WhereValueMatches
                If Data(Seedx, Seedy, Seedz) = Me.oldValue Then Data(Seedx, Seedy, Seedz) = Me.newValue
                '6 from before
                Me.FloodFill26(Data, Seedx + 1, Seedy, Seedz)
                Me.FloodFill26(Data, Seedx - 1, Seedy, Seedz)
                Me.FloodFill26(Data, Seedx, Seedy + 1, Seedz)
                Me.FloodFill26(Data, Seedx, Seedy - 1, Seedz)
                Me.FloodFill26(Data, Seedx, Seedy, Seedz + 1)
                Me.FloodFill26(Data, Seedx, Seedy, Seedz - 1)
                'now all diagonals
                Me.FloodFill26(Data, Seedx - 1, Seedy + 1, Seedz)
                Me.FloodFill26(Data, Seedx - 1, Seedy - 1, Seedz)
                Me.FloodFill26(Data, Seedx - 1, Seedy, Seedz + 1)
                Me.FloodFill26(Data, Seedx - 1, Seedy, Seedz - 1)
                Me.FloodFill26(Data, Seedx - 1, Seedy + 1, Seedz + 1)
                Me.FloodFill26(Data, Seedx - 1, Seedy - 1, Seedz - 1)
                Me.FloodFill26(Data, Seedx - 1, Seedy + 1, Seedz - 1)
                Me.FloodFill26(Data, Seedx - 1, Seedy - 1, Seedz + 1)
                Me.FloodFill26(Data, Seedx, Seedy + 1, Seedz + 1)
                Me.FloodFill26(Data, Seedx, Seedy + 1, Seedz - 1)
                Me.FloodFill26(Data, Seedx, Seedy - 1, Seedz - 1)
                Me.FloodFill26(Data, Seedx, Seedy - 1, Seedz + 1)
                Me.FloodFill26(Data, Seedx + 1, Seedy + 1, Seedz)
                Me.FloodFill26(Data, Seedx + 1, Seedy - 1, Seedz)
                Me.FloodFill26(Data, Seedx + 1, Seedy, Seedz + 1)
                Me.FloodFill26(Data, Seedx + 1, Seedy, Seedz - 1)
                Me.FloodFill26(Data, Seedx + 1, Seedy + 1, Seedz + 1)
                Me.FloodFill26(Data, Seedx + 1, Seedy + 1, Seedz - 1)
                Me.FloodFill26(Data, Seedx + 1, Seedy - 1, Seedz - 1)
                Me.FloodFill26(Data, Seedx + 1, Seedy - 1, Seedz + 1)
        End Select

    End Sub
End Class