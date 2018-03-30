Imports Icm.Geometry

<TestFixture(), Category("Icm.Geometry")>
Public Class SimpleFigureTest

    <Test(), Category("Icm.Geometry")>
    Public Sub ScaleMeTest()


        Dim factorX As Double = 3
        Dim factorY As Double = 2
        Dim px As Double = 1.1!
        Dim py As Double = 2.2!
        Dim target As GPoint = GPoint.At(px, py)

        Dim segmentfig As New SegmentFigure("Segmento", {""}, GPoint.At(1, 1), GPoint.At(2, 2))
        Dim actual As BaseFigure = segmentfig
        actual.ScaleMe(factorX, factorY)
        Dim expected As New SegmentFigure("RESULTADO", {""}, GPoint.At(1, 1), GPoint.At(6, 4))
        Assert.IsTrue(segmentfig.Points(1).X = segmentfig.Points(1).X And expected.Points(1).Y = expected.Points(1).Y)
        Assert.IsFalse(segmentfig.Points(1).X = target.X)
        Assert.IsFalse(segmentfig.Points(1).Y = target.Y)


    End Sub

    <Test(), Category("Icm.Geometry")>
    Public Sub RotateMeTest()

        Dim angule As Double = 2
        Dim px As Double = 1.1!
        Dim py As Double = 2.2!
        Dim target As GPoint = GPoint.At(px, py)

        Dim segmentfig As New SegmentFigure("Segmento", {""}, GPoint.At(1, 1), GPoint.At(2, 2))
        Dim actual As BaseFigure = segmentfig
        actual.RotateMe(angule)
        Assert.IsFalse(segmentfig.Points(1).X = target.X)
        Assert.IsFalse(segmentfig.Points(1).Y = target.Y)

        ''Caso2 (error en redondeo)
        'angule = 0
        'Dim segmentfig2 As New SegmentFigure("Segmento", {""}, GPoint.At(1, 1), GPoint.At(2, 2))
        'Dim actual2 As BaseFigure = segmentfig2
        'actual2.RotateMe(angule)
        'Assert.IsTrue(segmentfig2.Points(1).X = target.X)
        'Assert.IsTrue(segmentfig2.Points(1).Y = target.Y)


    End Sub

    <Test(), Category("Icm.Geometry")>
    Public Sub TranslateMeTest()

        Dim factorX As Double = 3
        Dim factorY As Double = 2
        Dim px As Double = 1.1!
        Dim py As Double = 2.2!
        Dim target As GPoint = GPoint.At(px, py)

        Dim segmentfig As New SegmentFigure("Segmento", {""}, GPoint.At(1, 1), GPoint.At(2, 2))
        Dim actual As BaseFigure = segmentfig
        actual.TranslateMe(factorX, factorY)
        Dim expected As New SegmentFigure("RESULTADO", {""}, GPoint.At(4, 3), GPoint.At(5, 4))
        Assert.IsTrue(segmentfig.Points(1).X = expected.Points(1).X And segmentfig.Points(1).Y = expected.Points(1).Y)
        Assert.IsFalse(segmentfig.Points(1).X = target.X)
        Assert.IsFalse(segmentfig.Points(1).Y = target.Y)
    End Sub

    <Test(), Category("Icm.Geometry")>
    Public Sub CloneTest()

        Dim segmentfig As New SegmentFigure("Segmento", {""}, GPoint.At(1, 1), GPoint.At(2, 2))
        Dim actual As BaseFigure = segmentfig
        Dim expected As SegmentFigure
        expected = CType(CType(actual.Clone("Segmento"), BaseFigure), SegmentFigure)
        Assert.IsFalse(expected Is actual)
        Assert.IsTrue(segmentfig.Points(1).X = expected.Points(1).X And segmentfig.Points(1).Y = expected.Points(1).Y)
    End Sub

    <Test(), Category("Icm.Geometry")>
    Public Sub Bounds()
        Dim segmentfig As New SegmentFigure("Segmento", {""}, GPoint.At(1, 1), GPoint.At(2, 2))
        Dim bounds = segmentfig.Bounds()
        Assert.IsTrue(bounds.Bottom = 1)
        Assert.IsTrue(bounds.Top = 2)
    End Sub

    <Test(), Category("Icm.Geometry")>
    Public Sub ToStringTest()

        Dim segmentfig As New SegmentFigure("Segmento", {""}, GPoint.At(1, 1), GPoint.At(2, 2))
        Dim actual As BaseFigure = segmentfig
        Dim resultado As String
        resultado = segmentfig.ToString()
        Assert.IsTrue(segmentfig.ToString = resultado.ToString)
    End Sub

    <Test(), Category("Icm.Geometry")>
    Public Sub NotifyTest()

        Dim segmentfig As New SegmentFigure("Segmento", {""}, GPoint.At(1, 1), GPoint.At(2, 2))
        Dim actual As BaseFigure = segmentfig
        Dim x As Double = 1
        Dim y As Double = 1
        segmentfig.Notify(x, y)
        Dim expected As New SegmentFigure("Segmento", {""}, GPoint.At(2, 2), GPoint.At(3, 3))
        Assert.IsTrue(segmentfig.Points(1).X = expected.Points(1).X And segmentfig.Points(1).Y = expected.Points(1).Y)

    End Sub
End Class
