Namespace HBBSoft.Pivot
    Public Module Factory
        Public Function CreateDataSet() As DataSet
            Return New DataSet(gManageID.NewID)
        End Function

        Public Function CreateCellSet() As CellSet
            Return New CellSet(gManageID.NewID)
        End Function

        Public Function CreateDimensionTree() As DimensionTree
            Return New DimensionTree(gManageID.NewID)
        End Function

        Public Function CreateDimension() As Dimension
            Return New Dimension(gManageID.NewID)
        End Function

        Public Function CreateLevel() As Level
            Return New Level(gManageID.NewID)
        End Function

        Public Function CreateMeasure() As Measure
            Return New Measure(gManageID.NewID)
        End Function

        Public Function CreateMember() As Member
            Return New Member(gManageID.NewID)
        End Function

        Public Function CreateSegment(ByVal BeginValue As Decimal, ByVal BeginValueCompareModel As EnumBeginValueCompareModel, ByVal EndValue As Decimal, ByVal EndValueCompareModel As EnumEndValueCompareModel, ByVal ImageURL As String) As Segment
            Return New Segment(BeginValue, BeginValueCompareModel, EndValue, EndValueCompareModel, ImageURL)
        End Function
    End Module
End Namespace