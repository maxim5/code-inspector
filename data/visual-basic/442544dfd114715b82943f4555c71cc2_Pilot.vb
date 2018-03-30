' ========================================================================
' EveHQ - An Eve-Online™ character assistance application
' Copyright © 2005-2012  EveHQ Development Team
' 
' This file is part of EveHQ.
'
' EveHQ is free software: you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation, either version 3 of the License, or
' (at your option) any later version.
'
' EveHQ is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.
'
' You should have received a copy of the GNU General Public License
' along with EveHQ.  If not, see <http://www.gnu.org/licenses/>.
'=========================================================================
Imports System.Xml

<Serializable()> Public Class Pilot

    Public Name As String = ""
    Public ID As String = ""
    Public Account As String = ""
    Public AccountPosition As String = ""
    Public Race As String = ""
    Public Blood As String = ""
    Public Gender As String = ""
    Public Corp As String = ""
    Public CorpID As String = ""
    Public Isk As Double = 0
    Public CloneName As String = ""
    Public CloneSP As String = ""
    Public SkillPoints As Integer = 0
    Public Training As Boolean = False
    Public TrainingStartTime As Date = Now
    Public TrainingStartTimeActual As Date = Now
    Public TrainingEndTime As Date = Now
    Public TrainingEndTimeActual As Date = Now
    Public TrainingSkillID As String = ""
    Public TrainingSkillName As String = ""
    Public TrainingStartSP As Integer = 0
    Public TrainingEndSP As Integer = 0
    Public TrainingCurrentSP As Integer = 0
    Public TrainingCurrentTime As Long = 0
    Public TrainingSkillLevel As Integer = 0
    Public TrainingNotifiedNow As Boolean = False
    Public TrainingNotifiedEarly As Boolean = False
    Public CAtt As Integer = 0
    Public IAtt As Integer = 0
    Public MAtt As Integer = 0
    Public PAtt As Integer = 0
    Public WAtt As Integer = 0
    Public CImplant As Integer = 0
    Public IImplant As Integer = 0
    Public MImplant As Integer = 0
    Public PImplant As Integer = 0
    Public WImplant As Integer = 0
    Public CImplantA As Integer = 0
    Public IImplantA As Integer = 0
    Public MImplantA As Integer = 0
    Public PImplantA As Integer = 0
    Public WImplantA As Integer = 0
    Public CImplantM As Integer = 0
    Public IImplantM As Integer = 0
    Public MImplantM As Integer = 0
    Public PImplantM As Integer = 0
    Public WImplantM As Integer = 0
    Public UseManualImplants As Boolean = False
    Public CAttT As Double = 0
    Public IAttT As Double = 0
    Public MAttT As Double = 0
    Public PAttT As Double = 0
    Public WAttT As Double = 0
    Public PilotSkills As New Collection
    Public QueuedSkills As New SortedList(Of Long, PilotQueuedSkill)
    Public QueuedSkillTime As Long
    Public Certificates As New ArrayList
    Public PrimaryQueue As String = ""
    Public ActiveQueue As New EveHQ.Core.SkillQueue
    Public ActiveQueueName As String = ""
    <NonSerialized()> Public TrainingQueues As New SortedList
    Public Blueprints As New Collection
    Public CacheFileTime As Date
    Public CacheExpirationTime As Date
    Public TrainingFileTime As Date
    Public TrainingExpirationTime As Date
    Public Updated As Boolean = False
    Public LastUpdate As String = ""
    Public Active As Boolean = True
    Public KeySkills(53) As String
    Public Standings As New SortedList(Of Long, PilotStanding)
    Public CorpRoles As New List(Of CorporationRoles)
    Public Enum KeySkill
        Mining = 1
        MiningUpgrades = 2
        Astrogeology = 3
        MiningBarge = 4
        MiningDrone = 5
        Exhumers = 6
        Refining = 7
        RefiningEfficiency = 8
        Metallurgy = 9
        Research = 10
        Science = 11
        Industry = 12
        ProductionEfficiency = 13
        ArkonorProc = 14
        BistotProc = 15
        CrokiteProc = 16
        DarkOchreProc = 17
        GneissProc = 18
        HedbergiteProc = 19
        HemorphiteProc = 20
        JaspetProc = 21
        KerniteProc = 22
        MercoxitProc = 23
        OmberProc = 24
        PlagioclaseProc = 25
        PyroxeresProc = 26
        ScorditeProc = 27
        SpodumainProc = 28
        VeldsparProc = 29
        IceProc = 30
        IceHarvesting = 31
        DeepCoreMining = 32
        MiningForeman = 33
        MiningDirector = 34
        Learning = 35
        JumpDriveOperation = 36
        JumpDriveCalibration = 37
        JumpFuelConservation = 38
        JumpFreighters = 39
        ScrapMetalProc = 40
        Accounting = 41
        BrokerRelations = 42
        Daytrading = 43
        MarginTrading = 44
        Marketing = 45
        Procurement = 46
        Retail = 47
        Trade = 48
        Tycoon = 49
        Visibility = 50
        Wholesale = 51
        Diplomacy = 52
        Connections = 53
	End Enum
End Class

Public Enum CorporationRoles As Long
    Director = 1
    PersonnelManager = 128
    Accountant = 256
    SecurityOfficer = 512
    FactoryManager = 1024
    StationManager = 2048
    Auditor = 4096
    HangarCanTake1 = 8192
    HangarCanTake2 = 16384
    HangarCanTake3 = 32768
    HangarCanTake4 = 65536
    HangarCanTake5 = 131072
    HangarCanTake6 = 262144
    HangarCanTake7 = 524288
    HangarCanQuery1 = 1048576
    HangarCanQuery2 = 2097152
    HangarCanQuery3 = 4194304
    HangarCanQuery4 = 8388608
    HangarCanQuery5 = 16777216
    HangarCanQuery6 = 33554432
    HangarCanQuery7 = 67108864
    AccountCanTake1 = 134217728
    AccountCanTake2 = 268435456
    AccountCanTake3 = 536870912
    AccountCanTake4 = 1073741824
    AccountCanTake5 = 2147483648
    AccountCanTake6 = 4294967296
    AccountCanTake7 = 8589934592
    AccountCanQuery1 = 17179869184
    AccountCanQuery2 = 34359738368
    AccountCanQuery3 = 68719476736
    AccountCanQuery4 = 137438953472
    AccountCanQuery5 = 274877906944
    AccountCanQuery6 = 549755813888
    AccountCanQuery7 = 1099511627776
    EquipmentConfig = 2199023255552
    ContainerCanTake1 = 4398046511104
    ContainerCanTake2 = 8796093022208
    ContainerCanTake3 = 17592186044416
    ContainerCanTake4 = 35184372088832
    ContainerCanTake5 = 70368744177664
    ContainerCanTake6 = 140737488355328
    ContainerCanTake7 = 281474976710656
    CanRentOffice = 562949953421312
    CanRentFactorySlot = 1125899906842624
    CanRentResearchSlot = 2251799813685248
    JuniorAccountant = 4503599627370496
    StarbaseConfig = 9007199254740992
    Trader = 18014398509481984
    ChatManager = 36028797018963968
    ContractManager = 72057594037927936
    InfrastructureTacticalOfficer = 144115188075855872
    StarbaseCaretaker = 288230376151711744
End Enum

<Serializable()> Public Class PilotSkill
    Implements System.ICloneable
    Public ID As String
    Public Name As String
    Public GroupID As String
    Public Flag As Integer
    Public Rank As Integer
    Public SP As Integer
    Public Level As Integer
    Public LevelUp(5) As Integer
    Public Function Clone() As Object Implements System.ICloneable.Clone
        Dim R As EveHQ.Core.PilotSkill = CType(Me.MemberwiseClone, EveHQ.Core.PilotSkill)
        Return R
    End Function
End Class

<Serializable()> Public Class PilotQueuedSkill
    Public Position As Integer
    Public SkillID As Integer
    Public Level As Integer
    Public StartSP As Long
    Public EndSP As Long
    Public StartTime As DateTime
    Public EndTime As DateTime
End Class

Public Class PilotSortTrainingTime
    Public cName As String
    Public cTrainingEndTime As Date
    Public Property Name() As String
        Get
            Return cName
        End Get
        Set(ByVal value As String)
            cName = value
        End Set
    End Property
    Public Property TrainingEndTime() As Date
        Get
            Return cTrainingEndTime
        End Get
        Set(ByVal value As Date)
            cTrainingEndTime = value
        End Set
    End Property
End Class

<Serializable()> Public Class PilotStanding
    Public Type As StandingType
    Public ID As Long ' Key for Standings
    Public Name As String
    Public Standing As Double
End Class

Public Enum StandingType
    Unknown = -1
    Agent = 0
    Faction = 1
    NPCCorporation = 2
    PlayerCorp = 3
End Enum