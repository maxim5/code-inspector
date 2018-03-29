unit GlobalRecordsAndEnums;
{
  Also holds global functions, mostly related to type conversions
}

interface

uses
  SysUtils, ComCtrls, Graphics, StdCtrls,

  dialogs;

type
  ERace = (Amarr, Caldari, Gallente, Minmatar, RACE_None);
  EComponentType = (Powercore, CPU, Capacitor, Propulsion, Sensor, Shield, Armor, Weapon);
  EInventionType = (Normal, Ship, Rig, INVTYPE_None);
  EActivity = (ACT_Manufacture, ACT_Copying, ACT_Invention, ACT_None);
  EColumn = (COL_None=0, COL_ID=1, COL_Name=2, COL_InventionType=3, COL_Datacore1=4, COL_Datacore2=5, COL_T2Item1=6, COL_T2Item2=7, COL_InventionProfit=8, COL_BuildCost=9, COL_Race=10, COL_T1ItemProfit=11, COL_T2RevenueDay=12);
const
  COL_End=11;
type
  EItemType = (IT_T1Item, IT_T2Item, IT_AdvancedMaterial, IT_T2Component, IT_RAM, IT_Tradegoods, IT_Datacore, IT_Mineral, IT_CapComponent, IT_CapT2Component, IT_Salvage, IT_DataInterface, IT_Decryptor, IT_Unknown);
  // Decryptors are included, but they are treated separately because of their influence on invention jobs!
  ESkill = (SK_None,
            SK_AmarrEncryption, SK_CaldariEncryption, SK_GallenteEncryption, SK_MinmatarEncryption,
            SK_AmarrianStarship, SK_CaldariStarship, SK_Electromagnetic, SK_Electronic, SK_GallenteanStarship, SK_Graviton, SK_HighEnergy, SK_Hydromagnetic, SK_Laser, SK_Mechanical, SK_MinmatarStarship, SK_Molecular, SK_Nanite, SK_Nuclear, SK_Plasma, SK_Quantum, SK_Rocket, SK_Science, SK_Industry, SK_ProductionEfficiency);
  EShoppingKind = (SK_Invention, SK_Manufacture);
  EShopType = (ST_Item, ST_Buy, ST_Manufacture);
//  ELocation = (LOC_POSLab, LOC_POSAdvancedLab, LOC_NPCStation, LOC_Outpost, LOC_None);
  EFaction = (FAC_None, FAC_Amarr, FAC_Caldari, FAC_Gallente, FAC_Minmatar, FAC_Ammatar, FAC_Thukker, FAC_Angel, FAC_Guristas, FAC_Jove, FAC_Sansha, FAC_Syndicate, FAC_Khanid, FAC_ORE);

  SNewItem = record
    Name: String[100];
    ID: Integer;
    ItemType: EItemType;
    Volume: Double;
    Race: ERace;
    BuildTime: Integer;
    ProductivityModifier: Integer;
    InventionTime: Integer;
    InventionType: EInventionType;
    InventionRace: ERace;
    CopyTime: Integer;
    ProductionLimit: Integer;
    BaseChance: Double;
    WasteFactor: Integer;
    BuildType: EInventionType;
    PortionSize: Integer;

    UsedInAmount: Integer;
    RequirementAmount: Integer;
    InventedWithAmount: Integer;
  end;

  SItemIDAmount = record
    ItemID: Integer;
    Activity: EActivity;
    Amount: Integer;
    ApplyWaste: Boolean;
    Damage: Double;
  end;

  SNewItemUserSetting = record
    ID: Integer;
    Price: Double;
    Manufacture: Boolean;
    BPOME: Integer;
    CategoryID: Integer;
    SubcategoryID: Integer;
    GroupID: Integer;
  end;

  SDecryptorType = record
    Name: String[50];
    ID: Integer;
    Probability: Double;
    ME: Integer;
    PE: Integer;
    Runs: Integer;
  end;

  SDecryptor = record
    ID: Integer;
    TypeID: Integer;
    Name: String[50];
    Race: ERace;
  end;

  SDecryptorUserSetting = record
    ID: Integer;
    Price: Double;
  end;

  SCategory = record
    ID: Integer;
    Name: String[50];
  end;

  SSubCategory = record
    ID: Integer;
    Name: String[50];
    Category: Integer;
  end;

  SGroup = record
    ID: Integer;
    Name: String[50];
    SubCategory: Integer;
  end;

  SSettings = record
    UseFees: Boolean;
    SalesTax: Double;
    BrokersFee: Double;
    AppLeft: Integer;
    AppTop: Integer;
    AppWidth: Integer;
    AppHeight: Integer;
    Columns: Array [0..9] of EColumn;
    SortColumn: EColumn;
    SortDirection: Boolean;
    ItemsSortColumn: Integer;
    ItemsSortDirection: Boolean;
    ItemsFilter: Array [-1..11] of Boolean;
    FilterAmarr: Boolean;
    FilterCaldari: Boolean;
    FilterGallente: Boolean;
    FilterMinmatar: Boolean;
    ImportPricesDefaultSettingIndex: Integer;
    ShoppingItemsSortColumn: Integer;
    ShoppingItemsSortDirection: Boolean;
    ShoppingBuySortColumn: Integer;
    ShoppingBuySortDirection: Boolean;
    ShoppingManufactureSortColumn: Integer;
    ShoppingManufactureSortDirection: Boolean;
    SettingsActiveTab: Integer;
    ResultFilterItems: Boolean;
    ResultsConfigurationSortColumn: Integer;
    ResultsConfigurationSortDirection: Boolean;
  end;

  SSettings_v043 = record
    UseFees: Boolean;
    SalesTax: Double;
    BrokersFee: Double;
    AppLeft: Integer;
    AppTop: Integer;
    AppWidth: Integer;
    AppHeight: Integer;
    Columns: Array [0..COL_End] of EColumn;
    SortColumn: EColumn;
    SortDirection: Boolean;
    ItemsSortColumn: Integer;
    ItemsSortDirection: Boolean;
    ItemsFilter: Array [-1..11] of Boolean;
    FilterAmarr: Boolean;
    FilterCaldari: Boolean;
    FilterGallente: Boolean;
    FilterMinmatar: Boolean;
    ImportPricesDefaultSettingIndex: Integer;
    ShoppingItemsSortColumn: Integer;
    ShoppingItemsSortDirection: Boolean;
    ShoppingBuySortColumn: Integer;
    ShoppingBuySortDirection: Boolean;
    ShoppingManufactureSortColumn: Integer;
    ShoppingManufactureSortDirection: Boolean;
    SettingsActiveTab: Integer;
    ResultFilterItems: Boolean;
    ResultsConfigurationSortColumn: Integer;
    ResultsConfigurationSortDirection: Boolean;
    // New in version 0.43:
    // changed "Columns" property to hold 11 columns
    ImportPricesRegions0: Double;
    ImportPricesRegions1: Double;
    ImportPricesRegions2: Double;
    SettingsAPIUsername: String[50];
    SettingsAPIKey: String[50];
  end;

  SSettings_v044 = record
    UseFees: Boolean;
    SalesTax: Double;
    BrokersFee: Double;
    AppLeft: Integer;
    AppTop: Integer;
    AppWidth: Integer;
    AppHeight: Integer;
    Columns: Array [0..COL_End] of EColumn;
    SortColumn: EColumn;
    SortDirection: Boolean;
    ItemsSortColumn: Integer;
    ItemsSortDirection: Boolean;
    ItemsFilter: Array [-1..11] of Boolean;
    FilterAmarr: Boolean;
    FilterCaldari: Boolean;
    FilterGallente: Boolean;
    FilterMinmatar: Boolean;
    ImportPricesDefaultSettingIndex: Integer;
    ShoppingItemsSortColumn: Integer;
    ShoppingItemsSortDirection: Boolean;
    ShoppingBuySortColumn: Integer;
    ShoppingBuySortDirection: Boolean;
    ShoppingManufactureSortColumn: Integer;
    ShoppingManufactureSortDirection: Boolean;
    SettingsActiveTab: Integer;
    ResultFilterItems: Boolean;
    ResultsConfigurationSortColumn: Integer;
    ResultsConfigurationSortDirection: Boolean;
    ImportPricesRegions0: Double;
    ImportPricesRegions1: Double;
    ImportPricesRegions2: Double;
    SettingsAPIUsername: String[100];
    SettingsAPIKey: String[100];
    // New in version 0.44
    // changed API username & key to length 100
    SortColumn2: EColumn;
    SortDirection2: Boolean;
    SortColumn3: EColumn;
    SortDirection3: Boolean;
    ItemsSortColumn2: Integer;
    ItemsSortDirection2: Boolean;
    ItemsSortColumn3: Integer;
    ItemsSortDirection3: Boolean;
  end;



  SDefault = record
    NormalAmarrDecryptorID: Integer;
    NormalCaldariDecryptorID: Integer;
    NormalGallenteDecryptorID: Integer;
    NormalMinmatarDecryptorID: Integer;
    NormalBPCRuns: Integer;
    NormalUseMaxRunsBPC: Boolean;
    NormalMetaItemPrice: Double;
    NormalMetaItemLevel: Integer;
    ShipAmarrDecryptorID: Integer;
    ShipCaldariDecryptorID: Integer;
    ShipGallenteDecryptorID: Integer;
    ShipMinmatarDecryptorID: Integer;
    ShipBPCRuns: Integer;
    ShipUseMaxRunsBPC: Boolean;
    ShipMetaItemPrice: Double;
    ShipMetaItemLevel: Integer;
    RigAmarrDecryptorID: Integer;
    RigCaldariDecryptorID: Integer;
    RigGallenteDecryptorID: Integer;
    RigMinmatarDecryptorID: Integer;
    RigBPCRuns: Integer;
    RigUseMaxRunsBPC: Boolean;
    RigMetaItemPrice: Double;
    RigMetaItemLevel: Integer;
  end;

  SColor = record
    ColorShowInfo: Boolean;
    ColorEditItems: Boolean;
    ColorShoppingList: Boolean;

    T1Item: TColor;
    T2Item: TColor;
    AdvancedMaterial: TColor;
    T2Component: TColor;
    RAM: TColor;
    Tradegoods: TColor;
    Datacore: TColor;
    Mineral: TColor;
    CapComponent: TColor;
    CapT2Component: TColor;
    Salvage: TColor;
    DataInterface: TColor;
    Unknown: TColor;

    CustomColorA: String[20];
    CustomColorB: String[20];
    CustomColorC: String[20];
    CustomColorD: String[20];
    CustomColorE: String[20];
    CustomColorF: String[20];
    CustomColorG: String[20];
    CustomColorH: String[20];
    CustomColorI: String[20];
    CustomColorJ: String[20];
    CustomColorK: String[20];
    CustomColorL: String[20];
    CustomColorM: String[20];
    CustomColorN: String[20];
    CustomColorO: String[20];
    CustomColorP: String[20];
  end;

  SSuccess = record
    Formula: String[255];
    UseFormula: Boolean;
    StillUseDecryptor: Boolean;
    BaseNormalChance: Double;
    BaseShipChance: Double;
    BaseRigChance: Double;
  end;

  SSkills = record
    AmarrEncryption: Integer;
    CaldariEncryption: Integer;
    GallenteEncryption: Integer;
    MinmatarEncryption: Integer;
    AmarrianStarship: Integer;
    CaldariStarship: Integer;
    Electromagnetic: Integer;
    Electronic: Integer;
    GallenteanStarship: Integer;
    Graviton: Integer;
    HighEnergy: Integer;
    Hydromagnetic: Integer;
    Laser: Integer;
    Mechanical: Integer;
    MinmatarStarship: Integer;
    Molecular: Integer;
    Nanite: Integer;
    Nuclear: Integer;
    Plasma: Integer;
    Quantum: Integer;
    Rocket: Integer;
    Science: Integer;
    Industry: Integer;
    ProductionEfficiency: Integer;
    BeancounterF: Integer;
    BeancounterG: Integer;
    BeancounterK: Integer;
    BeancounterCI: Integer;
    Shaman: Integer;
    Cleric: Integer;
    Draftsman: Integer;
  end;

  SInstallation = record
    CopyInstallation: Double;
    CopyHourly: Double;
    CopyTimeModifier: Double;
    CopyMaterialModifier: Double;
    InventionInstallation: Double;
    InventionHourly: Double;
    InventionTimeModifier: Double;
    InventionMaterialModifier: Double;
    ManufactureInstallation: Double;
    ManufactureHourly: Double;
    ManufactureTimeModifier: Double;
    ManufactureMaterialModifier: Double;
  end;

  SInventionTypeDefault = record
    InvType: EInventionType;
    AmarrDecryptorID: Integer;
    CaldariDecryptorID: Integer;
    GallenteDecryptorID: Integer;
    MinmatarDecryptorID: Integer;
    MaxRuns: Boolean;
    MetaItemPrice: Double;
    MetaItemLevel: Integer;
  end;

  SLocation = record
    Name: String[100];
    ID: Integer;
    MaterialModifier: Double;
    TimeModifier: Double;
    Activity: EActivity;
  end;

  SShoppingItem = record
    DecryptorID: Integer;
    NewItemID: Integer;
    Amount: Integer;
    Kind: EShoppingKind;
    ME: Integer;
    Extrawastemodifier: Double;
    ShopType: EShopType;
  end;


  SResult = record
    ItemID: Integer;
    ConfigurationID: Integer;
    Success: Boolean;
    Date: TDateTime;
  end;

  SConfiguration = record
    Name: String[255];
    ID: Integer;
    DecryptorTypeID: Integer;
    Metalevel: Integer;
    BPCMaxRuns: Boolean;
    BPCRuns: Integer;
    BPCME: Integer;
    BPCPE: Integer;
    LocationID: Integer;
  end;

  SRegion = record
    Name: String[255];
    ID: Integer;
    Faction: EFaction;
  end;

  function EItemTypeToString(item: EItemType): String;
  function EItemTypeToStringShort(item: EItemType): String;
  function EItemTypeToInteger(item: EItemType): Integer;
  function ERaceToString(item: ERace): String;
  function ERaceToStringShort(item: ERace): String;
  function EInventionTypeToString(item: EInventionType): String;
  function EInventionTypeToStringShort(item: EInventionType): String;
  function EActivityToString(item: EActivity): String;
  function ESkillToString(skill: ESkill): String;

  function StringToEItemType(s: String): EItemType;
  function StringToERace(s: String): ERace;
  function StringToEInventionType(s: String): EInventionType;
  function StringToEActivity(s: String): EActivity;
  function StringToEFaction(s: String): EFaction;

  function SecondsToString(seconds: Integer): String;
  function ShouldRetrieve(Evetime: String): Boolean;
  function CorrectPrice(value: String): String;
  function CutString(str: String; lbl: TLabel): String;
  function MarketGroupIDToEItemType(i: Integer): EItemType;
  function Sort(Item1, Item2: TListItem; numsort: Boolean; column: Integer; direction: Boolean): Integer;
var
  settings: TFormatSettings;

  global_ds: Char; // Decimal separator
  global_ts: Char; // Thousand separator
  global_delimiter: String; // delimiter that SQL server uses when exporting query results
  i: Integer;
  adminhax: Boolean; // true if application was started with /adminhax-flag. Enabled creating and editing of items
  global_version: Double; // Current version number. Used to determine which loading function to use.

implementation

uses
  StrUtils, Windows, Math;

function EItemTypeToString(item: EItemType): String;
begin
  case item of
    IT_T1Item: result := 'Tech 1';
    IT_T2Item: result := 'Tech 2';
    IT_AdvancedMaterial: result := 'Advanced material';
    IT_T2Component: result := 'Component';
    IT_RAM: result := 'R.A.M.';
    IT_Tradegoods: result := 'Trade goods';
    IT_Datacore: result := 'Datacore';
    IT_Mineral: result := 'Mineral';
    IT_CapComponent: result := 'Capital component';
    IT_CapT2Component: result := 'Capital T2 component';
    IT_Salvage: result := 'Salvage';
    IT_DataInterface: result := 'Interface';
    IT_Decryptor: result := 'Decryptor';
  else result := 'n/a';
  end;
end;

function EItemTypeToStringShort(item: EItemType): String;
begin
  case item of
    IT_T1Item: result := 'Tech 1';
    IT_T2Item: result := 'Tech 2';
    IT_AdvancedMaterial: result := 'Advmat';
    IT_T2Component: result := 'Component';
    IT_RAM: result := 'RAM';
    IT_Tradegoods: result := 'Tradegoods';
    IT_Datacore: result := 'Datacore';
    IT_Mineral: result := 'Mineral';
    IT_CapComponent: result := 'Capcomponent';
    IT_CapT2Component: result := 'CapT2component';
    IT_Salvage: result := 'Salvage';
    IT_DataInterface: result := 'Interface';
    IT_Decryptor: result := 'Decryptor';
  else result := 'n/a';
  end;
end;

function EItemTypeToInteger(item: EItemType): Integer;
begin
  case item of
    IT_T1Item: result := 0;
    IT_T2Item: result := 1;
    IT_AdvancedMaterial: result := 2;
    IT_T2Component: result := 3;
    IT_RAM: result := 4;
    IT_Tradegoods: result := 5;
    IT_Datacore: result := 6;
    IT_Mineral: result := 7;
    IT_CapComponent: result := 8;
    IT_CapT2Component: result := 9;
    IT_Salvage: result := 10;
    IT_DataInterface: result := 11;
  else result := -1;
  end;
end;

function ERaceToString(item: ERace): String;
begin
  case item of
    Amarr: result := 'Amarr (Occult)';
    Caldari: result := 'Caldari (Esoteric)';
    Gallente: result := 'Gallente (Incognito)';
    Minmatar: result := 'Minmatar (Cryptic)';
  else result := 'n/a';
  end;
end;

function ERaceToStringShort(item: ERace): String;
begin
  case item of
    Amarr: result := 'Amarr';
    Caldari: result := 'Caldari';
    Gallente: result := 'Gallente';
    Minmatar: result := 'Minmatar';
  else result := 'n/a';
  end;
end;

function EInventionTypeToString(item: EInventionType): String;
begin
  case item of
    Normal: result := 'Normal (Data interface)';
    Ship: result := 'Ship (Ship Data interface)';
    Rig: result := 'Rig (Tuner Data interface)';
  else result := 'n/a';
  end;
end;

function EInventionTypeToStringShort(item: EInventionType): String;
begin
  case item of
    Normal: result := 'Normal';
    Ship: result := 'Ship';
    Rig: result := 'Rig';
  else result := 'n/a';
  end;
end;

function EActivityToString(item: EActivity): String;
begin
  case item of
    ACT_Manufacture: result := 'Manufacturing';
    ACT_Copying: result := 'Copying';
    ACT_Invention: result := 'Invention';
    ACT_None: result := 'None';
  else result := 'None';
  end;
end;

function ESkillToString(skill: ESkill): String;
begin
  case skill of
    SK_AmarrEncryption: result := 'Amarr Encryption Methods';
    SK_CaldariEncryption: result := 'Caldari Encryption Methods';
    SK_GallenteEncryption: result := 'Gallente Encryption Methods';
    SK_MinmatarEncryption: result := 'Minmatar Encryption Methods';
    SK_AmarrianStarship: result := 'Amarrian Starship Engineering';
    SK_CaldariStarship: result := 'Caldari Starship Engineering';
    SK_Electromagnetic: result := 'Electromagnetic Physics';
    SK_Electronic: result := 'Electronic Engineering';
    SK_GallenteanStarship: result := 'Gallentean Starship Engineering';
    SK_Graviton: result := 'Graviton Physics';
    SK_HighEnergy: result := 'High Energy Physics';
    SK_Hydromagnetic: result := 'Hydromagnetic Physics';
    SK_Laser: result := 'Laser Physics';
    SK_Mechanical: result := 'Mechanical Engineering';
    SK_MinmatarStarship: result := 'Minmatar Starship Engineering';
    SK_Molecular: result := 'Molecular Engineering';
    SK_Nanite: result := 'Nanite Engineering';
    SK_Nuclear: result := 'Nuclear Physics';
    SK_Plasma: result := 'Plasma Physics';
    SK_Quantum: result := 'Quantum Physics';
    SK_Rocket: result := 'Rocket Science';
    SK_Science: result := 'Science';
    SK_Industry: result := 'Industry';
    SK_ProductionEfficiency: result := 'Production Efficiency';
  else
    result := 'Other';
  end;
end;


function StringToEItemType(s: String): EItemType;
begin
  if (UpperCase(s) = 'TECH 1') then result := IT_T1Item
  else if (UpperCase(s) = 'TECH 2') then result := IT_T2Item
  else if (UpperCase(s) = 'ADVMAT') then result := IT_AdvancedMaterial
  else if (UpperCase(s) = 'COMPONENT') then result := IT_T2Component
  else if (UpperCase(s) = 'RAM') then result := IT_RAM
  else if (UpperCase(s) = 'TRADEGOODS') then result := IT_Tradegoods
  else if (UpperCase(s) = 'DATACORE') then result := IT_Datacore
  else if (UpperCase(s) = 'MINERAL') then result := IT_Mineral
  else if (UpperCase(s) = 'CAPCOMPONENT') then result := IT_CapComponent
  else if (UpperCase(s) = 'CAPT2COMPONENT') then result := IT_CapT2Component
  else if (UpperCase(s) = 'SALVAGE') then result := IT_Salvage
  else if (UpperCase(s) = 'INTERFACE') then result := IT_DataInterface
  else if (UpperCase(s) = 'DECRYPTOR') then result := IT_Decryptor
  else raise Exception.Create('Bad element type: '+ s);
end;

function StringToERace(s: String): ERace;
begin
  if ((UpperCase(s) = 'AMARR') or (s = 'a') or (s = '4')) then result := Amarr
  else if ((UpperCase(s) = 'CALDARI') or (s = 'c') or (s = '1')) then result := Caldari
  else if ((UpperCase(s) = 'GALLENTE') or (s = 'g') or (s = '8')) then result := Gallente
  else if ((UpperCase(s) = 'MINMATAR') or (s = 'm') or (s = '2')) then result := Minmatar
  else if ((UpperCase(s) = 'N/A')) then result := RACE_None 
  else raise Exception.Create('Bad race: '+ s);
end;

function StringToEInventionType(s: String): EInventionType;
begin
  if (UpperCase(s) = 'NORMAL') then result := Normal
  else if (UpperCase(s) = 'SHIP') then result := Ship
  else if (UpperCase(s) = 'RIG') then result := Rig
  else if (UpperCase(s) = 'N/A') then result := INVTYPE_None
  else raise Exception.Create('Bad invention type: '+ s);
end;

function StringToEActivity(s: String): EActivity;
begin
  if (s = '0') then result := ACT_None
  else if ((s = 'Manufacturing') or (s = '1')) then result := ACT_Manufacture
//  else if (s = '2') then result := ResearchTechnology
//  else if (s = '3') then result := ResearchPE
//  else if (s = '4') then result := ResearchME
  else if ((s = 'Copying') or (s = '5')) then result := ACT_Copying
//  else if (s = '6') then result := Duplicating
//  else if (s = '7') then result := ReverseEngineering
  else if ((s = 'Invention') or (s = '8')) then result := ACT_Invention
  else if ((s = 'None') or (s = '0')) then result := ACT_None
  else raise Exception.Create('Bad activity: '+ s);
end;

function StringToEFaction(s: String): EFaction;
begin
  if (s = '500003') then result := FAC_Amarr
  else if (s = '500001') then result := FAC_Caldari
  else if (s = '500004') then result := FAC_Gallente
  else if (s = '500002') then result := FAC_Minmatar
  else if (s = '500007') then result := FAC_Ammatar
  else if (s = '500015') then result := FAC_Thukker
  else if (s = '500011') then result := FAC_Angel
  else if (s = '500010') then result := FAC_Guristas
  else if (s = '500005') then result := FAC_Jove
  else if (s = '500019') then result := FAC_Sansha
  else if (s = '500009') then result := FAC_Syndicate
  else if (s = '500008') then result := FAC_Khanid
  else if (s = '500014') then result := FAC_ORE
  else raise Exception.Create('Bad faction: '+ s);
end;

function SecondsToString(seconds: Integer): String;
var
  s, m, h, d, w: Integer;
begin
  s := seconds;
  result := '';
  w := 0; d := 0; h := 0; m := 0;
  while (s >= 604800) do begin
    s := s - 604800;
    w := w + 1;
  end;
  while (s >= 86400) do begin
    s := s - 86400;
    d := d + 1;
  end;
  while (s >= 3600) do begin
    s := s - 3600;
    h := h + 1;
  end;
  while (s >= 60) do begin
    s := s - 60;
    m := m + 1;
  end;

  if w>0 then result := result + ' ' + IntToStr(w) + 'w';
  if d>0 then result := result + ' ' + IntToStr(d) + 'd';
  if h>0 then result := result + ' ' + IntToStr(h) + 'h';
  if m>0 then result := result + ' ' + IntToStr(m) + 'm';
  if s>0 then result := result + ' ' + IntToStr(s) + 's';

  if (seconds > 0) then result := RightStr(result, Length(result) -1)
  else result := 'n/a';
end;

function ShouldRetrieve(Evetime: String): Boolean;
// checks if the given time (which should be the content of <cachedUntil> of the API) is less than the current time
var
  syear, smonth, sday: String;
  year, month, day: Integer;
  time, cacheduntil, nowUTC: TDateTime;
  timezone: TTimeZoneInformation;
begin
  result := false;
  syear := LeftStr(Evetime, Pos('-', Evetime) -1);
  Evetime := RightStr(Evetime, Length(Evetime) - Pos('-', Evetime));
  smonth := LeftStr(Evetime, Pos('-', Evetime) -1);
  Evetime := RightStr(Evetime, Length(Evetime) - Pos('-', Evetime));
  sday := LeftStr(Evetime, Pos(' ', Evetime) -1);
  Evetime := RightStr(Evetime, Length(Evetime) - Pos(' ', Evetime));
  Evetime := StringReplace(Evetime, ':', settings.TimeSeparator, [rfReplaceAll]);
  // transform day, month, year and time to proper datetime object
  if (TryStrToInt(syear, year) and TryStrToInt(smonth, month) and TryStrToInt(sday, day) and TryStrToDateTime(Evetime, time)) then begin
    cacheduntil := (365 * (year-1900)) + ((year-1900) div 4);
    while (month <> 0) do begin
      case month of
        1: cacheduntil := cacheduntil;
        2: cacheduntil := cacheduntil + 31;
        3: if (year mod 4) = 0 then cacheduntil := cacheduntil + 29 else cacheduntil := cacheduntil + 28;
        4: cacheduntil := cacheduntil + 31;
        5: cacheduntil := cacheduntil + 30;
        6: cacheduntil := cacheduntil + 31;
        7: cacheduntil := cacheduntil + 30;
        8: cacheduntil := cacheduntil + 31;
        9: cacheduntil := cacheduntil + 31;
        10: cacheduntil := cacheduntil + 30;
        11: cacheduntil := cacheduntil + 31;
        12: cacheduntil := cacheduntil + 30;
      end;
      month := month - 1;
    end;
    cacheduntil := cacheduntil + day;
    cacheduntil := cacheduntil + time;
    GetTimeZoneInformation(timezone);
    nowUTC := Now + (timezone.Bias / 1440) + (timezone.DaylightBias / 1440);
//    ShowMessage(DateTimeToStr(cacheduntil));
//    ShowMessage(DateTimeToStr(nowUTC));
    if (cacheduntil < nowUTC) then begin
      result := true;
    end;
  end;
end;

function CorrectPrice(value: String): String;
// corrects a price value. Changes the decimal separator and adds 0s for k, m and b.
var
  lastchar: Char;
begin
  if (Length(value) > 0) then begin
    lastchar := value[Length(value)];
    result := value;
    if ((lastchar = Chr(46)) and (lastchar <> global_ds)) then begin   //Chr(46) = '.'
      result := LeftStr(value, Length(value) -1) + global_ds;
    end
    else begin
      if (UpperCase(lastchar) = 'B') then result := LeftStr(value, Length(value) -1) + '000000000';
      if (UpperCase(lastchar) = 'M') then result := LeftStr(value, Length(value) -1) + '000000';
      if (UpperCase(lastchar) = 'K') then result := LeftStr(value, Length(value) -1) + '000';
    end;
  end
  else result := value;
end;

function CutString(str: String; lbl: TLabel): String;
// cuts the given string so it fits on a label of width 330.
var
  adddots: Boolean;
  s: String;
begin
  adddots := false;
  s := str;
  while (lbl.Canvas.TextWidth(s) > 155) do begin
    s := LeftStr(s, Length(s) - 1);
    adddots := true;
  end;
  if (adddots) then s := s + '...';
  result := s;
end;

function MarketGroupIDToEItemType(i: Integer): EItemType;
begin
  if (false) then result := IT_T1Item
  else if (false) then result := IT_T2Item
  else if (i = 499) then result := IT_AdvancedMaterial
  else if ((i >= 802) and (i <= 805)) then result := IT_T2Component // 802: Amarr, 803: Caldari, 804: Gallente, 805: Minmatar
  else if (i = 398) then result := IT_RAM
  else if (i = 20) then result := IT_Tradegoods
  else if (i = 966) then result := IT_Datacore
  else if (i = 18) then result := IT_Mineral
  else if (i = 781) then result := IT_CapComponent
  else if ((i >= 1099) and (i <= 1102)) then result := IT_CapT2Component // 1099: Amarr, 1100: Caldari, 1101: Gallente, 1102: Minmatar
  else if (i = 942) then result := IT_Salvage
  else if (i = 980) then result := IT_DataInterface
  else result := IT_Unknown;
end;

function Sort(Item1, Item2: TListItem; numsort: Boolean; column: Integer; direction: Boolean): Integer;
var
  subitem: Integer;
  comp: Integer;
  num1, num2: Double;
begin
  if (column = 0) then begin
    if (numsort) then begin
      if (not TryStrToFloat(StringReplace(StringReplace(Item1.Caption, global_ds, '', [rfReplaceAll]), global_ts, '', [rfReplaceAll]), num1)) then
        num1 := -999;
      if (not TryStrToFloat(StringReplace(StringReplace(Item2.Caption, global_ds, '', [rfReplaceAll]), global_ts, '', [rfReplaceAll]), num2)) then
        num2 := -999;
//      num1 := StrToFloat(StringReplace(StringReplace(Item1.Caption, global_ts, '', [rfReplaceAll]), global_ds, '', [rfReplaceAll]));
//      num2 := StrToFloat(StringReplace(StringReplace(Item2.Caption, global_ts, '', [rfReplaceAll]), global_ds, '', [rfReplaceAll]));
      comp := CompareValue(num1, num2);
    end
    else comp := CompareText(Item1.Caption, Item2.Caption)
  end
  else begin
    subitem := column -1;
    if (numsort) then begin
      if (not TryStrToFloat(StringReplace(StringReplace(Item1.SubItems[subitem], global_ds, '', [rfReplaceAll]), global_ts, '', [rfReplaceAll]), num1)) then
        num1 := -999;
      if (not TryStrToFloat(StringReplace(StringReplace(Item2.SubItems[subitem], global_ds, '', [rfReplaceAll]), global_ts, '', [rfReplaceAll]), num2)) then
        num2 := -999;
//      num1 := StrToFloat(StringReplace(StringReplace(Item1.SubItems[subitem], global_ts, '', [rfReplaceAll]), global_ds, '', [rfReplaceAll]));
//      num2 := StrToFloat(StringReplace(StringReplace(Item2.SubItems[subitem], global_ts, '', [rfReplaceAll]), global_ds, '', [rfReplaceAll]));
      comp := CompareValue(num1, num2);
    end
    else comp := CompareText(Item1.SubItems[subitem], Item2.SubItems[subitem]);
  end;

  if (direction) then result := comp
  else begin
    if (comp < 0) then result := 1
    else if (comp > 0) then result := -1
    else result := comp;
  end;
end;

initialization

DecimalSeparator:='.';
ThousandSeparator:=' ';
global_ds := DecimalSeparator;
global_ts := ThousandSeparator;
global_version := 0.49;
global_delimiter := '|';


adminhax := false;
if (ParamCount > 0) then begin
  for i := 1 to ParamCount do begin
    if (UpperCase(ParamStr(i)) = '/ADMINHAX') then begin
      adminhax := true;
    end;
  end;
end;
//adminhax := true;

end.

{Packaged ship volumes:
- 500m?: Shuttles (31)
- 2.500m?: Frigs (25), Interceptors (831), Covops (830), EAS (893), Assault Ships (324), Stealth Bombers (834)
- 3.750m?: Mining Barges (463), Exhumers (543)
- 5.000m?: Destroyers (420), Interdictors (541)
- 10.000m?: Cruiser (26), HAC (358), Heavy Dictor (894), Logistics (832), Combat Recons (906), Force Recons (833)
- 15.000m?: BC (419), CS (540)
- 20.000m?: Industrials (28), Transport Ships (380)
- 50.000m?: BS (27), BlackOPs (898), Marauder (900)
- 1.000.000m?: Freighters (513), JF (902)}
