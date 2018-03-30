Imports Icm.Geometry.SegmentFigure
Imports Icm.Geometry
<TestFixture()>
Public Class BasefigureTest

    <Test(), Category("Icm.Geometry")>
    Public Sub ScaleTest()

        Dim factorX As Double = 3
        Dim factorY As Double = 2

        Dim segmentfig As New SegmentFigure("Segmento", {""}, GPoint.At(1, 1), GPoint.At(2, 2))
        Dim actual As BaseFigure = segmentfig

        Dim resultado As SegmentFigure
        resultado = CType(actual.ScaleT(factorX, factorY), SegmentFigure)

        Dim expected As New SegmentFigure("RESULTADO", {""}, GPoint.At(1, 1), GPoint.At(6, 4))
        Assert.IsTrue(expected.Points(1).X = resultado.Points(1).X And expected.Points(1).Y = resultado.Points(1).Y)
        Assert.IsFalse(actual Is resultado)

    End Sub

    <Test(), Category("Icm.Geometry")>
    Public Sub RotateTest()

        Dim angulo As Double = 0
        Const epsilon As Double = 0.00000001
        Dim segmentfig As New SegmentFigure("Segmento", {""}, GPoint.At(1, 1), GPoint.At(2, 2))
        Dim actual As BaseFigure = segmentfig

        segmentfig = CType(actual.RotateT(angulo), SegmentFigure)

        Dim expected As New SegmentFigure("Segmento", {""}, GPoint.At(1, 1), GPoint.At(2, 2))
        Assert.IsTrue(expected.Points(1).Distance(segmentfig.Points(1)) < epsilon)
        Assert.IsFalse(actual Is segmentfig)

    End Sub

    <Test(), Category("Icm.Geometry")>
    Public Sub TranslateTest()

        Dim factorX As Double = 2
        Dim factorY As Double = 2

        Dim segmentfig As New SegmentFigure("Segmento", {""}, GPoint.At(1, 1), GPoint.At(2, 2))
        Dim actual As BaseFigure = segmentfig

        Dim resultado As SegmentFigure
        resultado = CType(actual.TranslateT(factorX, factorY), SegmentFigure)

        Dim expected As New SegmentFigure("Segmento", {""}, GPoint.At(3, 3), GPoint.At(4, 4))
        Assert.IsTrue(expected.Points(1).X = resultado.Points(1).X And expected.Points(1).Y = resultado.Points(1).Y)
        Assert.IsFalse(actual Is resultado)

    End Sub

End Class
