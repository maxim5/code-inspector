Imports System.Runtime.CompilerServices

''' <summary>
''' The implementors can be applied affine transformations to.
''' </summary>
''' <remarks>
''' Note to implementors:
''' You can choose either to transform the object itself, in which case the funcion must return a reference
''' to self, or create and return a new transformed object.
''' 
''' Note to clients:
''' It cannot be assumed that the functions transform the object itself, so the next code can fail
''' if the real class use the "copy" semantics:
''' <code>
''' Class MyPoint
''' Implements IAffineTransformation
''' 
''' ' ...implementation of a 2D point
''' 
''' End Class
''' 
''' Dim p As MyPoint(2, 15)
''' p.Scale(2)
''' Debug.Assert(p.X = 4)
''' </code>
''' Instead it would be better to use the function result:
''' <code>
''' Dim p As MyPoint(2, 15)
''' p = p.Scale(2)
''' Debug.Assert(p.X = 4)
''' </code>
''' 
''' </remarks>
Public Interface IAffineTransformable

    Function Scale(ByVal factorX As Double, ByVal factorY As Double) As IAffineTransformable
    Function Translate(ByVal x As Double, ByVal y As Double) As IAffineTransformable
    Function Rotate(ByVal angle As Double) As IAffineTransformable

End Interface

Public Module IAffineTranformableExtensions

    ''' <summary>
    ''' Scale function that return the same type.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="tf"></param>
    ''' <param name="factorX"></param>
    ''' <param name="factorY"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <Extension()>
    Function ScaleT(Of T As IAffineTransformable)(ByVal tf As T, ByVal factorX As Double, ByVal factorY As Double) As T
        Return DirectCast(tf.Scale(factorX, factorY), T)
    End Function

    ''' <summary>
    ''' Translate function that return the same type.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="tf"></param>
    ''' <param name="x"></param>
    ''' <param name="y"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <Extension()>
    Function TranslateT(Of T As IAffineTransformable)(ByVal tf As T, ByVal x As Double, ByVal y As Double) As T
        Return DirectCast(tf.Translate(x, y), T)
    End Function

    ''' <summary>
    ''' Rotate function that return the same type.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="tf"></param>
    ''' <param name="angle"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <Extension()>
    Function RotateT(Of T As IAffineTransformable)(ByVal tf As T, ByVal angle As Double) As T
        Return DirectCast(tf.Rotate(angle), T)
    End Function


    ''' <summary>
    ''' Proportional scaling that return the same type.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="tf"></param>
    ''' <param name="factor"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <Extension()>
    Public Function EscalarP(Of T As IAffineTransformable)(ByVal tf As T, ByVal factor As Double) As T
        Return DirectCast(tf.Scale(factor, factor), T)
    End Function

    ''' <summary>
    ''' Scaling respect to a center
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="tf"></param>
    ''' <param name="factor"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <Extension()>
    Public Function EscalarC(Of T As IAffineTransformable)(ByVal tf As T, ByVal factor As Double, ByVal x As Double, ByVal y As Double) As T
        Return DirectCast(tf.Translate(-x, -y).EscalarP(factor).Translate(x, y), T)
    End Function

    ''' <summary>
    ''' Rotate respect to a center
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="tf"></param>
    ''' <param name="angle"></param>
    ''' <param name="x"></param>
    ''' <param name="y"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <Extension()>
    Public Function RotarC(Of T As IAffineTransformable)(ByVal tf As T, ByVal angle As Double, ByVal x As Double, ByVal y As Double) As T
        Return DirectCast(tf.Translate(-x, -y).Rotate(angle).Translate(x, y), T)
    End Function

    ''' <summary>
    ''' Rotate respect to a center
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="tf"></param>
    ''' <param name="angle"></param>
    ''' <param name="p"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <Extension()>
    Public Function RotarC(Of T As IAffineTransformable)(ByVal tf As T, ByVal angle As Double, ByVal p As GPoint) As T
        Return DirectCast(tf.RotarC(angle, p.X, p.Y), T)
    End Function

    ''' <summary>
    ''' Polar translation (magnitude + angle)
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="tf"></param>
    ''' <param name="angle"></param>
    ''' <param name="magnitude"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <Extension()>
    Public Function TrasladarPolar(Of T As IAffineTransformable)(ByVal tf As T, ByVal angle As Double, ByVal magnitude As Double) As T
        Return DirectCast(tf.Translate(magnitude * Math.Cos(angle), magnitude * Math.Sin(angle)), T)
    End Function

End Module