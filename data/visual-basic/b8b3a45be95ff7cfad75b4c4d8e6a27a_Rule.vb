Imports System
Imports System.Text
Imports System.Collections

' VB.NET Translation of GoldParser, by Reggie Wilbanks <AppDeveloper@starband.net> converted (by hand) from,
' C# Translation of GoldParser, by Marcus Klimstra <klimstra@home.nl>.
' Based on GOLDParser by Devin Cook <http:'www.devincook.com/goldparser>.
Namespace GoldParser

  ' The Rule class is used to represent the logical structures of the grammar.
  ' Rules consist of a head containing a nonterminal followed by a series of
  ' both nonterminals and terminals.
    Friend Class Rule

        Private m_ruleNT As Symbol ' non-terminal rule
        Private m_ruleSymbols As ArrayList
        Private m_tableIndex As Integer

        ' constructor

        ' Creates a new Rule.
        Friend Sub New(ByVal p_tableIndex As Integer, ByVal p_head As symbol)

            m_ruleSymbols = New ArrayList()
            m_tableIndex = p_tableIndex
            m_ruleNT = p_head
        End Sub

        ' friend properties

        ' Gets the index of this <c>Rule</c> in the GoldParser's rule-table.
        Friend ReadOnly Property TableIndex() As Integer

            Get
                TableIndex = m_tableIndex
            End Get

        End Property

        ' Gets the head symbol of this rule.
        Friend ReadOnly Property RuleNonTerminal() As Symbol

            Get
                RuleNonTerminal = m_ruleNT
            End Get
        End Property

        ' Gets the number of symbols in the body (right-hand-side) of the rule.
        Friend ReadOnly Property SymbolCount() As Integer

            Get
                SymbolCount = m_ruleSymbols.Count
            End Get
        End Property

        ' friend properties

        ' The name of this rule.
        Friend ReadOnly Property Name() As String

            Get
                Name = "<" + m_ruleNT.Name + ">"
            End Get
        End Property

        ' The definition of this rule.
        Friend ReadOnly Property Definition() As String

            Get

                Dim result As New StringBuilder()
                Dim enumerator As IEnumerator = m_ruleSymbols.GetEnumerator()

                Do While enumerator.MoveNext()

                    Dim symbol As Symbol
                    symbol = CType(enumerator.Current, Symbol)
                    result.Append(symbol.ToString()).Append(" ")
                Loop

                Definition = result.ToString()
            End Get
        End Property

        '
        Friend ReadOnly Property ContainsOneNonTerminal() As Boolean

            Get
                If m_ruleSymbols.Count > 0 Then
                    Dim p_ruleSymbol As Symbol = CType(m_ruleSymbols(0), Symbol)
                    ContainsOneNonTerminal = (m_ruleSymbols.Count = 1) And (p_ruleSymbol.Kind = 0)
                Else
                    ContainsOneNonTerminal = False
                End If
            End Get
        End Property

        ' friend methods

        ' Returns the symbol in the body of the rule with the specified index. 
        Friend Function GetSymbol(ByVal p_index As Integer) As Symbol

            If (p_index >= 0 And p_index < m_ruleSymbols.Count) Then
                GetSymbol = CType(m_ruleSymbols(p_index), Symbol)
            Else
                GetSymbol = Nothing
            End If
        End Function

        ' Returns the Backus-Noir representation of this <c>Rule</c>.
        Public Overrides Function ToString() As String

            ToString = Name + " ::= " + Definition
        End Function

        ' equals ?

        '
        Friend Sub AddItem(ByVal p_symbol As Symbol)

            m_ruleSymbols.Add(p_symbol)
        End Sub
    End Class
end namespace
