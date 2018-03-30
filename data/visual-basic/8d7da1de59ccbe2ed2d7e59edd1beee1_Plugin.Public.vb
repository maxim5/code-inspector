'Interface for other plugins

Public Enum eMagicSchool
    Creature = 1
    Life = 2
    Item = 4
End Enum

Partial Public Class Plugin
    Private Shared mInstance As Plugin
    Private mBussy As Boolean
    Private mPause As Boolean
    Private mBuffsPending As Integer
    Private mSecsTillFirstBuffL As Integer
    Private mSecsTillFirstBuffC As Integer
    Private mSecsTillFirstBuffI As Integer
    Private mUpdateTime As Date ' To limit the call to bufftime remaining, it is to resource heavy to do it realtime  
    Public Event buffbotcompleted(ByVal playername As String, ByVal errormsg As String)
    Public Event buffbotstarted(ByVal playername As String, ByVal buffcount As Integer)

    Public ReadOnly Property Bussy() As Boolean
        Get
            Return mBussy
        End Get
    End Property

    Public Shared ReadOnly Property Instance() As Plugin
        Get
            Return mInstance
        End Get
    End Property

    Public Property Pause() As Boolean
        Get
            Return mPause
        End Get
        Set(ByVal value As Boolean)
            mPause = value
        End Set
    End Property

    Public ReadOnly Property BuffsPending() As Integer
        Get
            Return mBuffsPending ' + mPendingGembuffs
        End Get
    End Property

    Public ReadOnly Property PendingBuffs() As Integer
        Get
            Return mBuffsPending
        End Get
    End Property

    Public ReadOnly Property Buffing() As Boolean
        Get
            Return mbuffing
        End Get
    End Property

    Public Function ArchmageEnduranceAugmentation() As Integer

        Return mcharconfig.ArchmageEnduranceAugmentation
    End Function

    Public Function setprofile(ByVal id As Integer) As Integer
        If id >= 0 Then
            If mcharconfig.profile <> id Then
                cboProfile.Selected = id
                'loadbuffprofile()
                'mbuffselectionChanged = True
            End If
        End If
    End Function

    Public Function StartBotBuffs() As Integer
        If validateprebuff(True) Then

        End If
        'For Each kvp As KeyValuePair(Of Integer, CharConfig.Integerlist) In mcharconfig.selfbuffweaponbuffs
        '    Dim objitem As Decal.Adapter.Wrappers.WorldObject = GetFindItemFromInventory(kvp.Key)
        '    If Not objitem Is Nothing Then
        '        If objitem.Category = eObjectFlags.Caster Then
        '            mwand = objitem '' first wand in the row
        '            Exit For
        '        End If
        '    End If
        'Next

        UpdateStatus(0)
        If mBuffsPending > 0 Then
            mbuffing = True
        End If
    End Function

    'target=0 cancel  all
    Public Function CancelBotBuffs(ByVal target As Integer) As Integer
        Dim count As Integer = 0
        For Each d As KeyValuePair(Of String, BuffInfo) In mBotBuffs

            If target = 0 OrElse d.Value.TargetId = target Then

                d.Value.suspended = True
            End If
        Next

        UpdateStatus(0)
        mbuffing = (mBuffsPending > 0)
    End Function

    Public Function CancelBotBuffs(ByVal player As String) As Integer
        Dim count As Integer = 0

        If Not String.IsNullOrEmpty(player) Then
            For Each d As KeyValuePair(Of String, BuffInfo) In mBotBuffs
                If Not String.IsNullOrEmpty(d.Value.player) AndAlso d.Value.player = player Then
                    If Not d.Value.suspended Then
                        count += 1
                        d.Value.suspended = True
                    End If

                End If
            Next
            UpdateStatus(0)
            mbuffing = (mBuffsPending > 0)
            If Not mbuffing Then
                FinishedBuffing()
            End If
        End If
        Return count
    End Function

    Public Function AddBotBuff(ByVal spellid As Integer, ByVal TargetID As Integer, ByVal player As String, ByVal heading As Double) As Integer
        Dim b As New BuffInfo
        b.TargetId = TargetID
        b.SpellId = spellid
        b.player = player
        b.heading = heading
        Dim xcount As Integer = 0

        If mBotBuffs Is Nothing Then
            mBotBuffs = New Dictionary(Of String, BuffInfo)
        End If

        Dim spell As Decal.Filters.Spell
        spell = mFileService.SpellTable.GetById(spellid)
        If Not spell Is Nothing Then

            Select Case spell.School.Id 'convert to flags for bitwise or
                Case 4
                    b.school = eMagicSchool.Creature
                Case 3
                    b.school = eMagicSchool.Item
                Case 2
                    b.school = eMagicSchool.Life
            End Select
            b.duration = spell.Duration
            b.duration += (b.duration * mcharconfig.ArchmageEnduranceAugmentation * 0.2)

            If Not Core.CharacterFilter.IsSpellKnown(spellid) Then
                wtcw2("mBotBuffs, You don't know that spell: " & spellid)
            ElseIf Not mBotBuffs.ContainsKey(b.Key) Then
                mBotBuffs.Add(b.Key, b)
                wtcw2("mBotBuffs, adding: " & b.Key & spell.Name & " duration " & b.duration & spellid)
                xcount += 1
            ElseIf mBotBuffs.Item(b.Key) Is Nothing Then
                mBotBuffs.Item(b.Key) = b
                xcount += 1
            Else
                wtcw2("Updating buff: " & b.Key & spell.Name)
                If Not b.suspended Then
                    xcount += 1
                End If
                mBotBuffs.Item(b.Key).suspended = False
            End If
        Else
            wtcw2("addbuff Decal.Filters.Spell is null " & spellid)
        End If

        Return xcount
    End Function

    Public Sub StartBuffs(ByVal schoolid As eMagicSchool, ByVal bBuffPending As Boolean)
        mNoManaforspellCounter = 0
        If validateprebuff(True) Then

        End If

        If mwand Is Nothing Then
            wtcw2("No wand set")
            Return
        End If

        For Each kvp As KeyValuePair(Of String, BuffInfo) In mBuffs
            If (schoolid And kvp.Value.school) = kvp.Value.school Then
                If bBuffPending Then
                    If kvp.Value.secondsremaining < mcharconfig.PendingbuffsTimeout Then
                        kvp.Value.forcedRebuff = True
                    End If
                Else
                    kvp.Value.forcedRebuff = True
                End If
            End If
        Next

        UpdateStatus(0)

        If mBuffsPending > 0 Then
            'insertsetprebuffs(schoolid)
            mbuffing = True
            wtcw("StartBuffs Pending " & mBuffsPending)
        Else
            wtcw("No buffs pending ")
        End If

    End Sub


    'Public Function Startbuffs(ByVal xcount As Integer) As Integer
    '    Dim totalspells As Integer
    '    Dim durationavg As Integer = 0
    '    Dim interval As Integer

    '    If xcount > 0 Then
    '        For Each d As KeyValuePair(Of String, BuffInfo) In mBuffs
    '            If Not d.Value.suspended Then
    '                totalspells += 1
    '                d.Value.suspended = True
    '                durationavg += d.Value.duration
    '            End If
    '        Next

    '        If totalspells > 0 Then
    '            durationavg /= totalspells
    '            interval = durationavg / (totalspells / xcount)
    '        End If

    '    End If

    '    'updatebuffs:
    '    'if interval passed then:
    '    'for i = 0 to xcount
    '    'find first spell to expire
    '    'flag as forced rebuff

    '    Return interval 'nextinterval
    'End Function


    Public Function BuffTimeRemaining(ByVal school As eMagicSchool) As Integer
        Dim sexpired As Integer = -1

        If (school And eMagicSchool.Creature) = eMagicSchool.Creature Then
            sexpired = mSecsTillFirstBuffC
        End If

        If (school And eMagicSchool.Life) = eMagicSchool.Life Then
            If mSecsTillFirstBuffL >= 0 And (sexpired = -1 Or mSecsTillFirstBuffL < sexpired) Then
                sexpired = mSecsTillFirstBuffL
            End If
        End If

        If (school And eMagicSchool.Item) = eMagicSchool.Item Then
            If mSecsTillFirstBuffI >= 0 And (sexpired = -1 Or mSecsTillFirstBuffI < sexpired) Then
                sexpired = mSecsTillFirstBuffI
            End If
        End If

        If sexpired > 0 Then
            sexpired = CInt(sexpired - DateDiff(DateInterval.Second, mUpdateTime, Now))
        End If

        If sexpired <= 0 Then
            Return 0
        Else
            Return sexpired
        End If

    End Function

End Class
