Imports System
Imports System.Collections
Imports System.Linq
Imports System.Text

Public Class FactoryAggregate
    Public Changes As New List(Of IEvent)
    ReadOnly _state As FactoryState
    Public Sub New(state As FactoryState)
        _state = state
    End Sub
    Public Sub AssignEmployeeToFactory(employeeName As String)
        If (_state.ListOfEmployeeNames.Contains(employeeName)) Then
            Fail(":> the name of '{0}' only one employee can have", employeeName)
            Return
        End If
        If employeeName.Equals("bender") Then
            Fail(":> Guys with name 'bender' are trouble.")
            Return
        End If
        DoPaperWork("Assign employee to the factory")
        RecordThat(New EmployeeAssignedToFactory(employeeName))
    End Sub
    Sub Fail(message As String, ParamArray args() As Object)
        Throw New InvalidOperationException(String.Format(message, args))
    End Sub
    Public Sub TransferShipmentToCargoBay(shipmentName As String, ParamArray parts() As CarPart)
        If _state.ListOfEmployeeNames.Count.Equals(0) Then
            Fail(":> There has to be somebody at factory in order to accept shipment")
            Return
        End If
        If parts.Length.Equals(0) Then
            Fail(":> Empty shipments are not accepted!")
            Return
        End If
        If _state.ShipementsWaitingToBeUnloaded.Count > 2 Then
            Fail(":> More than two shipments can't fit into this cargo bay:(")
            Return
        End If
        DoRealWork("opening cargo bay doors")
        RecordThat(New ShipmentTransferredToCargoBay(shipmentName, parts))

        Dim totalCountOfParts = parts.Sum(Function(p) p.Quantity)
        If totalCountOfParts > 10 Then
            RecordThat(New CurseWordUttered With {.TheWord = "Boltov tebe v korobky perdach",
                                                  .Meaning = "awe in the face of the amount of parts delivered"})
        End If
    End Sub
    Sub DoPaperWork(workName As String)

    End Sub
    Sub DoRealWork(workName As String)

    End Sub
    Sub RecordThat(e As IEvent)
        Changes.Add(e)
        _state.Mutate(e)
    End Sub
End Class
Public Class FactoryState
    Public Sub New(events As IEnumerable(Of IEvent))
        For Each [event] In events
            Mutate([event])
        Next

    End Sub

    Public ReadOnly ListOfEmployeeNames As New List(Of String)
    Public ReadOnly ShipementsWaitingToBeUnloaded As New List(Of CarPart())

    Public Sub AnnouceInsideFactory(e As EmployeeAssignedToFactory)
        ListOfEmployeeNames.Add(e.EmployeeName)
    End Sub
    Public Sub AnnouceInsideFactory(e As ShipmentTransferredToCargoBay)
        ShipementsWaitingToBeUnloaded.Add(e.CarParts)
    End Sub
    Public Sub AnnouceInsideFactory(e As CurseWordUttered)

    End Sub
    Public Sub Mutate(e As IEvent)
        Dim [event] As Object = e
        AnnouceInsideFactory([event])
    End Sub
End Class
<Serializable>
Public Class EmployeeAssignedToFactory
    Implements IEvent

    Public EmployeeName As String

    Public Sub New(employeeName As String)
        Me.EmployeeName = employeeName
    End Sub
End Class
<Serializable>
Public Class CurseWordUttered
    Implements IEvent
    Public TheWord As String
    Public Meaning As String

    Public Overrides Function ToString() As String
        Return String.Format("'{0}' was heard within the walls. It meant:\r\n    '{1}'", TheWord, Meaning)
    End Function
End Class
<Serializable>
Public Class ShipmentTransferredToCargoBay
    Implements IEvent
    Public ShipmentName As String
    Public CarParts() As CarPart

    Public Sub New(shipmentName As String, ParamArray carParts() As CarPart)
        Me.ShipmentName = shipmentName
        Me.CarParts = carParts
    End Sub
    Public Overrides Function ToString() As String
        Dim builder As New StringBuilder
        builder.AppendFormat("Shipment '{0}' transferred to cargo bay:", ShipmentName).AppendLine()
        For Each carPart In CarParts
            builder.AppendFormat("    {0} {1} pcs", carPart.Name, carPart.Quantity).AppendLine()
        Next
        Return builder.ToString
    End Function

End Class

Public Interface IEvent

End Interface
<Serializable>
Public NotInheritable Class CarPart
    Public Name As String
    Public Quantity As Integer

    Public Sub New(name As String, quantity As Integer)
        Me.Name = name
        Me.Quantity = quantity
    End Sub

End Class