Class MainWindow 

    Private pages As List(Of Page)
    Private requests As List(Of PageRequest)

    Public Sub New()
        InitializeComponent()

        pages = New List(Of Page)
        For i = 1 To 7
            pages.Add(New Page With {.Name = i})
        Next

        requests = New List(Of PageRequest)
    End Sub

    Private Sub GenerateRequests()
        Dim rnd As New Random

        ' Generate requests
        requests.Clear()
        Dim requestCount As Integer = CType(Resources("Requests"), IntegerSingleton).Value

        For i = 0 To requestCount - 1
            requests.Add(New PageRequest() With {.Page = pages(rnd.Next(pages.Count))})
        Next

        RequestsItemsControl.DataContext = requests
        RequestsItemsControl.Items.Refresh()
    End Sub

    Private Sub Compute()
        ' Generate frames
        Dim frames As New List(Of Frame)
        Dim frameCount As Integer = CType(Resources("FrameSize"), IntegerSingleton).Value

        For i = 0 To frameCount - 1
            frames.Add(New Frame())
        Next

        ' Get algorithm
        Dim algorithm As PageReplacementAlgorithm
        If OptimalRadioButton.IsChecked.HasValue AndAlso OptimalRadioButton.IsChecked.Value Then
            algorithm = PageReplacementAlgorithm.Optimal
        ElseIf LruRadioButton.IsChecked.HasValue AndAlso LruRadioButton.IsChecked.Value Then
            algorithm = PageReplacementAlgorithm.Lru
        Else
            algorithm = PageReplacementAlgorithm.Fifo
        End If

        ' Evaluate
        Dim eval As PageReplacementEvaluation
        eval = (New PageReplacementEvaluator()).Evaluate(requests, frames, algorithm)

        ' Output
        EvaluationContentControl.DataContext = eval
        PageFaultCountTextBlock.Text = eval.PageFaultCount
    End Sub

    Private Sub GenerateAndCompute()
        GenerateRequests()
        Compute()
    End Sub

End Class
