Imports Icm.Geometry

'''<summary>
'''This is a test class for GPointTest and is intended
'''to contain all GPointTest Unit Tests
'''</summary>
<TestFixture(), Category("Icm.Geometry")>
Public Class GPointTest

#Region "Additional test attributes"
    '
    'You can use the following additional attributes as you write your tests:
    '
    'Use ClassInitialize to run code before running the first test in the class
    '<ClassInitialize()>  _
    'Public Shared Sub MyClassInitialize(ByVal testContext As TestContext)
    'End Sub
    '
    'Use ClassCleanup to run code after all tests in a class have run
    '<ClassCleanup()>  _
    'Public Shared Sub MyClassCleanup()
    'End Sub
    '
    'Use TestInitialize to run code before running each test
    '<TestInitialize()>  _
    'Public Sub MyTestInitialize()
    'End Sub
    '
    'Use TestCleanup to run code after each test has run
    '<TestCleanup()>  _
    'Public Sub MyTestCleanup()
    'End Sub
    '
#End Region


    '''<summary>
    '''A test for Distance
    '''</summary>
    <Test(), Category("Icm.Geometry")>
    Public Sub DistanceTest()
        Dim px As Double = 0.0!
        Dim py As Double = 0.0!
        Dim target As GPoint = GPoint.At(px, py)
        Dim other As GPoint = GPoint.At(3, 4)
        Dim expected As Double = 0.0!
        Dim actual As Double

        'Caso 1
        px = 2.6299999999999999
        py = 4.5
        target = GPoint.At(px, py)
        expected = 0.62201286160335956
        actual = target.Distance(other)
        Assert.AreEqual(expected, actual)

        'Caso 2
        px = 3
        py = 4
        target = GPoint.At(px, py)
        expected = 0.0
        actual = target.Distance(other)
        Assert.AreEqual(expected, actual)

        'Caso 3
        px = -3
        py = -4
        target = GPoint.At(px, py)
        expected = 10.0
        actual = target.Distance(other)
        Assert.AreEqual(expected, actual)

    End Sub

    '''<summary>
    '''A test for MeanPoint
    '''</summary>
    <Test(), Category("Icm.Geometry")>
    Public Sub MeanPointTest()
        Dim px As Double = 0.0!
        Dim py As Double = 0.0!
        Dim target As GPoint = GPoint.At(px, py)
        Dim other As GPoint = GPoint.At(3, 4)
        Dim expected As GPoint
        Dim actual As GPoint

        'Caso 1
        px = 3
        py = 4
        target = GPoint.At(px, py)
        expected = GPoint.At(3, 4)
        actual = target.Mean(other)
        Assert.AreEqual(expected, actual)

        'Caso 
        px = 2.6299999999999999
        py = 4.5
        target = GPoint.At(px, py)
        expected = GPoint.At(2.8149999999999999, 4.25)
        actual = target.Mean(other)
        Assert.AreEqual(expected, actual)

        'Caso 3
        px = -3
        py = -4
        target = GPoint.At(px, py)
        expected = GPoint.At(0.0, 0.0)
        actual = target.Mean(other)
        Assert.AreEqual(expected, actual)

    End Sub

    <Test(), Category("Icm.Geometry")>
    Public Sub RotateTest()

        'Caso 1
        Dim angule As Double = 3
        Dim actual As GPoint
        Dim other As GPoint = GPoint.At(3, 4)
        Dim expected As GPoint
        expected = GPoint.At(-3.5344575220408054, -3.53660996222218)
        actual = RotateT(other, angule)
        Assert.AreEqual(expected, actual)

        'Caso 2
        angule = 0
        other = GPoint.At(3, 5)
        expected = GPoint.At(3, 5)
        actual = RotateT(other, angule)
        Assert.AreEqual(expected, actual)

        'Caso 3
        angule = 10
        other = GPoint.At(0, 0)
        expected = GPoint.At(0, 0)
        actual = RotateT(other, angule)
        Assert.IsTrue(expected.Distance(actual) < Double.Epsilon)

        'Caso 4
        angule = Rad(90)
        other = GPoint.At(5, 8)
        expected = GPoint.At(-8, 5)
        actual = RotateT(other, angule)
        Const epsilon As Double = 0.00000000001
        Assert.IsTrue(expected.Distance(actual) < epsilon)

    End Sub


    <Test(), Category("Icm.Geometry")>
    Public Sub TraslateTest()

        'Caso 1
        Dim dx As Double = 0
        Dim dy As Double = 0
        Dim other As GPoint = GPoint.At(3, 4)
        Dim expected As GPoint
        Dim actual As GPoint
        expected = GPoint.At(3, 4)
        actual = TranslateT(other, dx, dy)
        Assert.AreEqual(expected, actual)

        'Caso2
        dx = -3
        dy = 5
        other = GPoint.At(3, 4)
        expected = GPoint.At(0, 9)
        actual = TranslateT(other, dx, dy)
        Assert.AreEqual(expected, actual)

        'Caso3
        dx = 3
        dy = 5
        other = GPoint.At(3, 4)
        expected = GPoint.At(6, 9)
        actual = TranslateT(other, dx, dy)
        Assert.AreEqual(expected, actual)

        'Caso3
        dx = 3
        dy = -5
        other = GPoint.At(3, 4)
        expected = GPoint.At(6, -1)
        actual = TranslateT(other, dx, dy)
        Assert.AreEqual(expected, actual)

    End Sub


    <Test(), Category("Icm.Geometry")>
    Public Sub ScaleTest()

        'Caso 1
        Dim factorX As Double = 0
        Dim factorY As Double = 0
        Dim other As GPoint = GPoint.At(3, 4)
        Dim expected As GPoint
        Dim actual As GPoint

        expected = GPoint.At(0, 0)
        actual = ScaleT(other, factorX, factorY)
        Assert.IsTrue(expected.Distance(actual) < Double.Epsilon)

        'Caso 2
        factorX = 2
        factorY = 3
        expected = GPoint.At(6, 12)
        actual = ScaleT(other, factorX, factorY)
        Assert.AreEqual(expected, actual)

        'Caso 3
        factorX = -3
        factorY = 3
        expected = GPoint.At(-9, 12)
        actual = ScaleT(other, factorX, factorY)
        Assert.AreEqual(expected, actual)

    End Sub

End Class
