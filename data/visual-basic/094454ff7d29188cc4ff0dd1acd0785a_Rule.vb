Public Enum eRuleWeaponTypes As Integer
    notapplicable = 0
    bow = 1
    crossbow = 2
    atlan = 3
    sword = 4
    ua = 5
    axe = 6
    mace = 7
    dagger = 8
    staff = 9
    spear = 10
    twohanded = 11
    mage = 12
End Enum

Public Class rule

    Public Function EmptyRule() As Boolean
        Return Not (minarmorlevel > 0 Or maxburden > 0 Or maxcraft > 0 Or keywords <> String.Empty Or weapontype <> eRuleWeaponTypes.notapplicable Or anyset Or (Specificset IsNot Nothing AndAlso Specificset.Count > 0) Or (spells IsNot Nothing AndAlso spells.Count > 0))
    End Function

    'general
    Public appliesToFlag As Integer
    Public name As String
    Public info As String
    Public enabled As Boolean

    Public maxburden As Integer
    Public maxcraft As Integer
    Public maxvalue As Integer

    Public keywords As String
    Public keywordsnot As String
    Public tradebotonly As Boolean
    Public tradebot As Boolean
    Public wavfile As String

    'weapon
    Public weapontype As eRuleWeaponTypes
    Public weaponsubtype As Integer
    Public minmcmodattackbonus As Integer
    Public minmeleebonus As Integer
    Public minmagicdbonus As Double

    Public damage As damagerange()
    Public damagetypeFlag As Integer

    'armor
    Public minarmorlevel As Integer
    Public armorcoverageFlag As Integer
    Public armortypeFlag As Integer

    'requirements
    Public spellmatches As Integer
    Public spells As Integerlist

    Public Specificset As Integerlist
    Public anyset As Boolean
    Public ivoryable As Boolean

    Structure minmax
        Public min As Integer
        Public max As Integer
    End Structure

    Structure damagerange
        Public enabled As Boolean
        Public minwield As Integer
        Public maxwield As Integer
        Public mindamage As Double
        Public maxdamage As Double
    End Structure

    Private Sub initnew()
        enabled = True
        name = String.Empty
        info = String.Empty
        maxburden = -1
        keywords = String.Empty
        maxvalue = -1
        spellmatches = 1
        maxcraft = -1
        weapontype = eRuleWeaponTypes.notapplicable
        minmagicdbonus = -1
        minmeleebonus = -1
        minmcmodattackbonus = -1
        ReDim damage(3)
        damage(3).maxdamage = -1
        damage(3).maxwield = -1
        damage(3).mindamage = -1
        damage(3).minwield = -1
        damage(3).enabled = True

        damage(2).maxdamage = -1
        damage(2).maxwield = -1
        damage(2).mindamage = -1
        damage(2).minwield = -1
        damage(2).enabled = True

        damage(1).maxdamage = -1
        damage(1).maxwield = -1
        damage(1).mindamage = -1
        damage(1).minwield = -1
        damage(1).enabled = True

        damage(0).maxdamage = -1
        damage(0).maxwield = -1
        damage(0).mindamage = -1
        damage(0).minwield = -1
        damage(0).enabled = True
        minarmorlevel = -1
    End Sub

    Public Sub New()
        initnew()
    End Sub

    Public Sub New(ByVal rname As String, ByVal rinfo As String, ByVal rweapontype As eRuleWeaponTypes, ByVal rweaponsubtype As Integer, ByVal rminmeleebonus As Integer, ByVal rminmcmodattackbonus As Integer)
        initnew()
        name = rname
        info = rinfo
        weapontype = rweapontype
        weaponsubtype = rweaponsubtype
        minmeleebonus = rminmeleebonus
        minmcmodattackbonus = rminmcmodattackbonus
    End Sub


End Class

Public Class RulesCollection
    Inherits CollectionBase

    Public Overridable Sub Insert(ByVal index As Integer, ByVal value As Rule)
        If MyBase.List.Contains(value) = False Then
            If index > Me.Count - 1 Then
                MyBase.List.Add(value)
            Else
                MyBase.List.Insert(index, value)
            End If

        End If
    End Sub
    Public Overridable Function Add(ByVal value As Rule) As Integer
        If MyBase.List.Contains(value) = False Then
            Return MyBase.List.Add(value)
        End If
    End Function
    Public Overridable Function Contains(ByVal value As Rule) As Boolean
        Return MyBase.List.Contains(value)
    End Function

    Public Overridable Sub Remove(ByVal value As Rule)
        If MyBase.List.Contains(value) Then
            MyBase.List.Remove(value)
        End If
    End Sub

    Default Public Overridable Property Item(ByVal index As Integer) As Rule
        Get
            Return DirectCast(MyBase.List.Item(index), Rule)
        End Get
        Set(ByVal value As Rule)
            MyBase.List.Item(index) = value
        End Set
    End Property

End Class