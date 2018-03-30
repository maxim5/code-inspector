Option Strict On
Option Explicit On
Option Infer On

Imports System
Imports System.Linq
Imports System.Collections.Generic
Imports GenDotNet.TemplateSupport.Strings

Public Class TypeInfo
   Inherits BaseInfo

   Private mNestingContainers As New List(Of TypeInfo)
   Private mTypeCategory As TypeCategory
   Private mModifiers As TypeModifiers
   Private mInheritsFrom As TypeInfo
   Private mInterfaces As New InterfaceInfoCollection
   Private mGenerics As New GenericInfoCollection

   Public Sub New( _
            ByVal name As String, _
            ByVal scope As Scope, _
            ByVal typecategory As TypeCategory, _
            ByVal modifiers As TypeModifiers, _
            ByVal ParamArray generics() As String)
      MyBase.New(name, scope)
      Me.mTypeCategory = typecategory
      Me.mModifiers = modifiers
      For Each generic In generics
         Me.Generics.Add(generic)
      Next
   End Sub

   Public Sub New( _
            ByVal name As String, _
            ByVal scope As Scope, _
            ByVal typecategory As TypeCategory, _
            ByVal ParamArray generics() As String)
      Me.New(name, scope, typecategory, TypeModifiers.None, generics)
   End Sub

   Public Sub New( _
            ByVal name As String, _
            ByVal scope As Scope, _
            ByVal ParamArray generics() As String)
      Me.New(name, scope, TemplateSupport.TypeCategory.Class, generics)
   End Sub

   Public Sub New( _
            ByVal name As String, _
            ByVal ParamArray generics() As String)
      Me.New(name, Scope.Unknown, generics)
   End Sub

   Public Property TypeCategory() As TypeCategory
      Get
         Return mTypeCategory
      End Get
      Set(ByVal value As TypeCategory)
         mTypeCategory = value
      End Set
   End Property

   Public Property Modifiers() As TypeModifiers
      Get
         Return mModifiers
      End Get
      Set(ByVal value As TypeModifiers)
         mModifiers = value
      End Set
   End Property

   Public ReadOnly Property NestingContainers() As List(Of TypeInfo)
      Get
         Return mNestingContainers
      End Get
   End Property

   Public Property InheritsFrom() As TypeInfo
      Get
         Return mInheritsFrom
      End Get
      Set(ByVal value As TypeInfo)
         mInheritsFrom = value
      End Set
   End Property

   Public ReadOnly Property Interfaces() As InterfaceInfoCollection
      Get
         Return mInterfaces
      End Get
   End Property

   Public ReadOnly Property Generics() As GenericInfoCollection
      Get
         Return mGenerics
      End Get
   End Property

End Class

Namespace VB
   Public Module TypeInfoExtensions
      <System.Runtime.CompilerServices.Extension()> _
      Public Function OutputName(ByVal info As TypeInfo) As String
         Dim ret = String.Empty
         ret &= info.Name & OutputGenericsList(info)
         Return ret
      End Function

      <System.Runtime.CompilerServices.Extension()> _
      Public Function OutputInheritsDecaration(ByVal info As TypeInfo) As String
         Dim ret = String.Empty
         ret &= info.Name & OutputGenericsList(info)
         Return ret
      End Function

      <System.Runtime.CompilerServices.Extension()> _
      Public Function OutputGenericsList(ByVal info As TypeInfo) As String
         Dim ret = String.Empty
         If info.Generics.Count > 0 Then
            ret &= "(Of "
            Dim last = info.Generics.Last
            For Each generic In info.Generics
               ret &= generic.Name
               If generic IsNot last Then
                  ret &= ", "
               End If
            Next
            ret &= ")"
         End If
         Return ret
      End Function

      <System.Runtime.CompilerServices.Extension()> _
      Public Function OutputDeclaration(ByVal info As TypeInfo) As String
         Dim ret = info.Name
         If info.Generics.Count > 0 Then
            ret &= "(Of "
            Dim last = info.Generics.Last
            For Each generic In info.Generics
               ret &= generic.Name
               If generic.Constraints.Count > 0 Then
                  ret &= " As "
                  If generic.Constraints.Count > 1 Then
                     ret &= "{"
                     Dim lastConstraint = generic.Constraints.Last
                     For Each constraint In generic.Constraints
                        ret &= constraint.OutputName
                        If constraint IsNot lastConstraint Then
                           ret &= ", "
                        End If
                     Next
                     ret &= "}"
                  Else
                     ret &= generic.Constraints(0).OutputName
                  End If
               End If
               If generic IsNot last Then
                  ret &= ", "
               End If
            Next
            ret &= ")"
         End If
         Return ret
      End Function

      <System.Runtime.CompilerServices.Extension()> _
      Public Function OutputOpening(ByVal info As TypeInfo) As String
         Return _
<code>
   <%= info.Attributes.Output %>
   <%= Strings.nl %><%= info.Scope.Output.WithSpace %>
   <%= info.Modifiers.Output.WithSpace %>
   <%= info.TypeCategory.Output.WithSpace %>
   <%= info.OutputDeclaration %>
   <%= If(info.InheritsFrom Is Nothing, String.Empty, _
      <code>
   Inherits <%= info.InheritsFrom.OutputName %>
      </code>.Value) %>
   <%= If(info.Interfaces Is Nothing OrElse info.Interfaces.Count = 0, String.Empty, info.Interfaces.OutputDeclaration()) %>
   <%= Strings.nl %>
</code>.Value
      End Function

      <System.Runtime.CompilerServices.Extension()> _
      Public Function OutputClose(ByVal info As TypeInfo) As String
         Return _
<code>
      End <%= info.TypeCategory.Output %><%= nl %>
</code>.Value
      End Function
   End Module
End Namespace

Namespace CSharp
   Public Module TypeInfoExtensions
      <System.Runtime.CompilerServices.Extension()> _
      Public Function OutputName(ByVal info As TypeInfo) As String
         Dim ret = String.Empty
         ret &= info.Name & OutputGenericsList(info)
         Return ret
      End Function

      <System.Runtime.CompilerServices.Extension()> _
      Public Function OutputInheritsDecaration(ByVal info As TypeInfo) As String
         Dim ret = String.Empty
         ret &= info.Name & OutputGenericsList(info)
         Return ret
      End Function

      <System.Runtime.CompilerServices.Extension()> _
      Public Function OutputGenericsList(ByVal info As TypeInfo) As String
         Dim ret = String.Empty
         If info.Generics.Count > 0 Then
            ret &= "<"
            Dim last = info.Generics.Last
            For Each generic In info.Generics
               ret &= generic.Name
               If generic IsNot last Then
                  ret &= ", "
               End If
            Next
            ret &= ">"
         End If
         Return ret
      End Function

      <System.Runtime.CompilerServices.Extension()> _
      Public Function OutputDeclaration(ByVal info As TypeInfo) As String
         Dim ret = info.Name
         If info.Generics.Count > 0 Then
            ret &= "<"
            Dim last = info.Generics.Last
            For Each generic In info.Generics
               ret &= generic.Name

               If generic IsNot last Then
                  ret &= ", "
               End If
            Next
            For Each generic In info.Generics
               If generic.Constraints.Count > 0 Then
                  ret &= " where " & generic.Name & ": "
                  Dim lastConstraint = generic.Constraints.Last
                  For Each constraint In generic.Constraints
                     ret &= constraint.OutputName
                     If constraint IsNot lastConstraint Then
                        ret &= ", "
                     End If
                  Next
                  ret &= generic.Constraints(0).OutputName
               End If
            Next
            ret &= ">"
         End If
         Return ret
      End Function


      <System.Runtime.CompilerServices.Extension()> _
      Public Function OutputOpening(ByVal info As TypeInfo) As String
         Return _
<code>
   <%= info.Attributes.Output %>
   <%= Strings.nl %><%= info.Scope.Output.WithSpace %>
   <%= info.Modifiers.Output.WithSpace %>
   <%= info.TypeCategory.Output.WithSpace %>
   <%= info.OutputDeclaration %>
         : <%= info.InheritsFrom.OutputName %>
   {
</code>.Value
      End Function

      <System.Runtime.CompilerServices.Extension()> _
      Public Function OutputClose(ByVal info As TypeInfo) As String
         Return _
<code>
   }
</code>.Value
      End Function
   End Module
End Namespace


