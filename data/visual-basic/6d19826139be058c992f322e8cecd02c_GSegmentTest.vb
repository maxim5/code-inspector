Imports Icm.Geometry

<TestFixture()>
Public Class GSegmentTest

    <Test(), Category("Icm.Geometry")>
    Public Sub RotateTest()

        'Caso1
        Dim angule As Double = 3
        Dim actual As GSegment
        Dim other As GSegment = New GSegment(GPoint.At(1, 1), GPoint.At(2, 2))
        Dim expected As GSegment
        expected = New GSegment(GPoint.At(-1.1311125046603125, -0.84887248854057817), GPoint.At(-2.2622250093206251, -1.6977449770811563))
        actual = RotateT(other, angule)
        Assert.AreEqual(expected.p1_, actual.p1_)
        Assert.AreEqual(expected.p2_, actual.p2_)

        'Caso 2
        angule = 0
        expected = New GSegment(GPoint.At(1, 1), GPoint.At(2, 2))
        actual = RotateT(other, angule)
        Assert.AreEqual(expected.p1_, actual.p1_)
        Assert.AreEqual(expected.p2_, actual.p2_)

        'Caso 3
        angule = -2
        expected = New GSegment(GPoint.At(0.4931505902785393, -1.3254442633728241), GPoint.At(0.98630118055707861, -2.6508885267456481))
        actual = RotateT(other, angule)
        Assert.AreEqual(expected.p1_, actual.p1_)
        Assert.AreEqual(expected.p2_, actual.p2_)

    End Sub

    <Test(), Category("Icm.Geometry")>
    Public Sub ScaleTest()

        'Caso 1
        Dim actual As GSegment
        Dim other As GSegment = New GSegment(GPoint.At(1, 1), GPoint.At(2, 2))
        Dim expected As GSegment
        Dim factorX As Double = 0
        Dim factorY As Double = 0
        actual = ScaleT(other, factorX, factorY)
        expected = New GSegment(GPoint.At(0, 0), GPoint.At(0, 0))
        Assert.AreEqual(expected.p1_, actual.p1_)
        Assert.AreEqual(expected.p2_, actual.p2_)

        'Caso 2
        factorX = 1
        factorY = 1
        expected = New GSegment(GPoint.At(1, 1), GPoint.At(2, 2))
        actual = ScaleT(other, factorX, factorY)
        Assert.AreEqual(expected.p1_, actual.p1_)
        Assert.AreEqual(expected.p2_, actual.p2_)

        'Caso 3
        factorX = -2
        factorY = -4
        expected = New GSegment(GPoint.At(-2, -4), GPoint.At(-4, -8))
        actual = ScaleT(other, factorX, factorY)
        Assert.AreEqual(expected.p1_, actual.p1_)
        Assert.AreEqual(expected.p2_, actual.p2_)

    End Sub

    <Test(), Category("Icm.Geometry")>
    Public Sub TraslateTest()

        'Caso 1
        Dim factorX As Double = 2
        Dim factorY As Double = 3
        Dim other As GSegment = New GSegment(GPoint.At(1, 1), GPoint.At(2, 2))
        Dim expected As GSegment = New GSegment(GPoint.At(3, 4), GPoint.At(4, 5))
        Dim actual As GSegment
        actual = TranslateT(other, factorX, factorY)
        Assert.AreEqual(expected.p1_, actual.p1_)
        Assert.AreEqual(expected.p2_, actual.p2_)

        'Caso 2
        factorX = -1
        factorY = -3
        expected = New GSegment(GPoint.At(0, -2), GPoint.At(1, -1))
        actual = TranslateT(other, factorX, factorY)
        Assert.AreEqual(expected.p1_, actual.p1_)
        Assert.AreEqual(expected.p2_, actual.p2_)

        'Caso 3
        factorX = 0
        factorY = 0
        expected = New GSegment(GPoint.At(1, 1), GPoint.At(2, 2))
        actual = TranslateT(other, factorX, factorY)
        Assert.AreEqual(expected.p1_, actual.p1_)
        Assert.AreEqual(expected.p2_, actual.p2_)

    End Sub

    <Test(), Category("Icm.Geometry")>
    Public Sub Intersection()

        'Caso1 (intersection INSIDE)
        Dim actual As GSegment = New GSegment(GPoint.At(1, 1), GPoint.At(1, 3))
        Dim other As GSegment = New GSegment(GPoint.At(2, 3), GPoint.At(0, 1))
        Dim resultado As IntersectionData
        resultado = actual.Intersection(other)
        Assert.IsTrue(resultado.Type = IntersectionType.InsideIntersecting)

        'Caso2 (intersection PARALLEL)
        actual = New GSegment(GPoint.At(1, 1), GPoint.At(1, 3))
        other = New GSegment(GPoint.At(2, 1), GPoint.At(2, 3))
        resultado = actual.Intersection(other)
        Assert.IsTrue(resultado.Type = IntersectionType.Parallel)

        'Caso3 (intersecting ENDINTERSECTING)
        actual = New GSegment(GPoint.At(1, 1), GPoint.At(1, 3))
        other = New GSegment(GPoint.At(1, 1), GPoint.At(0, 2))
        resultado = actual.Intersection(other)
        Assert.IsTrue(resultado.Type = IntersectionType.EndIntersecting)

        'Caso4 (intersecting ENDINTERSECTING2)
        actual = New GSegment(GPoint.At(1, 1), GPoint.At(1, 3))
        other = New GSegment(GPoint.At(1, 3), GPoint.At(2, 4))
        resultado = actual.Intersection(other)
        Assert.IsTrue(resultado.Type = IntersectionType.EndIntersecting)

        'Caso5 (intersecting NOINTERSECTING)
        actual = New GSegment(GPoint.At(1, 1), GPoint.At(1, 3))
        other = New GSegment(GPoint.At(2, 2), GPoint.At(6, 2))
        resultado = actual.Intersection(other)
        Assert.IsTrue(resultado.Type = IntersectionType.NonIntersecting)

        'Caso6 (intersecting COINCIDENT)
        actual = New GSegment(GPoint.At(1, 1), GPoint.At(1, 3))
        other = New GSegment(GPoint.At(1, 4), GPoint.At(1, 7))
        resultado = actual.Intersection(other)
        Assert.IsTrue(resultado.Type = IntersectionType.Coincident)

        'Caso7
        actual = New GSegment(GPoint.At(1, 1), GPoint.At(1, 3))
        other = New GSegment(GPoint.At(1, 2), GPoint.At(1, 4))
        resultado = actual.Intersection(other)
        Assert.IsTrue(resultado.Type = IntersectionType.Coincident)

    End Sub


End Class
