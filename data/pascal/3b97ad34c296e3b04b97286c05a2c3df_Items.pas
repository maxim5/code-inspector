unit Items;

interface

uses
  GlobalRecordsAndEnums, Category, Classes;

type
  TNewItem = class;
  TItemAmount = class;

  SRequirementsListItem = record
    Item: TNewItem;
    Amount: Integer;
    Damage: Double;
  end;
  SRequirementsList = array of SRequirementsListItem;

  TNewItem = class
//    private
    protected
      _usedIn: TList;       // holds TItemAmount instances
      _requirements: TList; // holds TItemAmount instances

      _name: String;
      _id: Integer;
      _itemtype: EItemType;
      _volume: Double;
      _race: ERace;
      _buildtime: Integer;
      _productivitymodifier: Integer;
      _inventiontime: Integer;
      _inventiontype: EInventionType;
      _inventionrace: ERace;
      _copytime: Integer;
      _productionlimit: Integer;
      _basechance: Double;
      _wastefactor: Integer;
      _buildtype: EInventionType;  // special property that holds if an item is a ship or not (in which case it is normal)
                                   // needed because waste on T2 ships is calculated differently from other items.
      _portionsize: Integer;

      _price: Double;
      _manufacture: Boolean;
      _bpome: Integer;
      _category: TCategory;
      _subcategory: TSubcategory;
      _group: TGroup;

      // derived properties (won't be saved to HD), needed for invention chance calculation
      _encryptionskill: ESkill;
      _skill1: ESkill;
      _skill2: ESkill;
      _datacore1amount: Integer;
      _datacore2amount: Integer;


      function GetUsedInCount: Integer;
      function GetRequirementsCount: Integer;
      function GetCanBeManufactured: Boolean;
      procedure WriteBuildType(value: EInventionType);
      function GetActivityCost(runs, me: Integer; activity: EActivity; extrawastemodifier: Double): Double;
    public
      constructor Create(Name: String; ID: Integer; ItemType: EItemType); overload;
      constructor Create(item: SNewItem); overload;
      destructor Destroy; reintroduce;

      procedure AddUsedIn(ID: Integer; Item: TNewItem; Amount: Integer; Damage: Double = 1); overload;
      procedure AddUsedIn(itemamount: TItemAmount); overload;
      function GetUsedIn(index: Integer): TItemAmount;
      procedure RemoveUsedIn(ID: Integer);
      procedure RemoveUsedInByIndex(index: Integer);

      procedure AddRequirement(ID: Integer; Item: TNewItem; Activity: EActivity; Amount: Integer; ApplyWaste: Boolean; Damage: Double = 1); overload;
      procedure AddRequirement(itemamount: TItemAmount); overload;
      function GetRequirement(index: Integer): TItemAmount;
      procedure RemoveRequirement(ID: Integer; activity: EActivity);
      procedure RemoveRequirementByIndex(index: Integer);

      function ToSNewItem: SNewItem;
      function ToSNewItemUserSetting: SNewItemUserSetting;
      procedure LoadUserSetting(settings: SNewItemUserSetting);
      procedure SetLinkedItem(itemamount: TItemAmount; linkeditem: TNewItem);

      function GetRequirementsList(runs, me: Integer; activity: EActivity; extrawastemodifier: Double): SRequirementsList;
      function GetBuildCost(runs, me: Integer; extrawastemodifier: Double): Double;
      function GetInventionCost(runs: Integer; extrawastemodifier: Double): Double;
      function GetCopyCost(runs: Integer; extrawastemodifier: Double): Double;

      // Should only be written when the application was started with /adminhax
      property Name: String read _name write _name;
      property ID: Integer read _id write _id;
      property ItemType: EItemType read _itemtype write _itemtype;
      property Volume: Double read _volume write _volume;
      property Race: ERace read _race write _race;
      property BuildTime: Integer read _buildtime write _buildtime;
      property ProductivityModifier: Integer read _productivityModifier write _productivityModifier;
      property InventionTime: Integer read _inventiontime write _inventiontime;
      property InventionType: EInventionType read _inventiontype write _inventiontype;
      property InventionRace: ERace read _inventionrace write _inventionrace;
      property CopyTime: Integer read _copytime write _copytime;
      property ProductionLimit: Integer read _productionlimit write _productionlimit;
      property BaseChance: Double read _basechance write _basechance;
      property WasteFactor: Integer read _wastefactor write _wastefactor;
      property BuildType: EInventionType read _buildtype write WriteBuildType;
      property PortionSize: Integer read _portionsize write _portionsize;

      property Price: Double read _price write _price;
      property Manufacture: Boolean read _manufacture write _manufacture;
      property BPOME: Integer read _bpome write _bpome;
      // only used for T1 items, category, subcategory and group allow T1 items to be organised on the main form
      property Category: TCategory read _category write _category;
      property Subcategory: TSubcategory read _subcategory write _subcategory;
      property Group: TGroup read _group write _group;

      property UsedInCount: Integer read GetUsedInCount;
      property RequirementsCount: Integer read GetRequirementsCount;
      property CanBeManufactured: Boolean read GetCanBeManufactured;
      property EncryptionSkill: ESkill read _encryptionskill;
      property ScienceSkill1: ESkill read _skill1;
      property ScienceSkill2: ESkill read _skill2;
      property Datacore1Amount: Integer read _datacore1amount;
      property Datacore2Amount: Integer read _datacore2amount;
  end;
  TNewItemsList = array of TNewItem;

  TItemAmount = class
    // requirement of a TNewItem, f.e. tritanium on a Punisher.
    public
      ID: Integer;    // Same as item.ID, but needed then loading items (at that point you only know the ID of the item)
      Item: TNewItem;
      Activity: EActivity;
      Amount: Integer;
      ApplyWaste: Boolean; // determines if this specific item is subject to waste when manufacturing
      Damage: Double;
  end;


implementation

uses
  Math, StrUtils;

function TNewItem.GetUsedInCount: Integer;
begin
  result := _usedIn.Count;
end;

function TNewItem.GetRequirementsCount: Integer;
begin
  result := _requirements.Count;
end;

function TNewItem.GetCanBeManufactured: Boolean;
var
  i: Integer;
begin
  result := false;
  if (ItemType <> IT_T2Item) then begin
    for i := 0 to RequirementsCount -1 do begin
      if (GetRequirement(i).Activity = ACT_Manufacture) then begin
        result := true;
        break;
      end;
    end;
  end;
end;

procedure TNewItem.WriteBuildType(value: EInventionType);
var
  i: Integer;
  itemamount: TItemAmount;
begin
  _buildtype := value;
  { Now set the ApplyWaste attribute of the items that are needed in manufacturing to the right value:
    if BuildType = Normal, waste should only be applied to items of type:
        - Mineral
        - Capital components (I guess, no such modules exist)
        - Capital T2 components (I guess, no such modules exist)
    if BuildType = Ship, waste should only be applied to items of type:
        - Mineral
        - T2Component
        - Advanced material
        - Trade goods
        - Capital components
        - Capital T2 components
  }

  // need to delete and re-add the itemamount-record because otherwise it is not saved
  for i := RequirementsCount -1 downto 0 do begin
    itemamount := GetRequirement(i);
    if (itemamount.Activity = ACT_Manufacture) then begin
      case itemamount.Item.ItemType of
        IT_T1Item: itemamount.ApplyWaste := false;
        IT_T2Item: itemamount.ApplyWaste := false;
        IT_AdvancedMaterial: itemamount.ApplyWaste := true;
        IT_T2Component: itemamount.ApplyWaste := (_buildtype = Ship);
        IT_RAM: itemamount.ApplyWaste := false;
        IT_Tradegoods: itemamount.ApplyWaste := (_buildtype = Ship);
        IT_Datacore: itemamount.ApplyWaste := false;
        IT_Mineral: itemamount.ApplyWaste := true;
        IT_CapComponent: itemamount.ApplyWaste := true;
        IT_CapT2Component: itemamount.ApplyWaste := true;
        IT_Salvage: itemamount.ApplyWaste := true;
        IT_DataInterface: itemamount.ApplyWaste := false;
        IT_Unknown: itemamount.ApplyWaste := false;
      end;
    end;
  end;
end;

constructor TNewItem.Create(Name: String; ID: Integer; ItemType: EItemType);
begin
  _name := Name;
  _id := ID;
  _itemtype := ItemType;
  _volume := 0;
  _race := RACE_None;
  _buildtime := 0;
  _productivitymodifier := 1;
  _inventiontime := 0;
  _inventiontype := INVTYPE_None;
  _inventionrace := RACE_None;
  _copytime := 0;
  _productionlimit := 0;
  _basechance := 0;
  _wastefactor := 0;
  _buildtype := normal;
  _portionsize := 1;

  _price := 0;
  _manufacture := false;
  _bpome := 0;
  _category := nil;
  _subcategory := nil;
  _group := nil;

  _usedIn := TList.Create;
  _requirements := TList.Create;

  _encryptionskill := SK_None;
  _skill1 := SK_None;
  _skill2 := SK_None;
  _datacore1amount := 0;
  _datacore2amount := 0;
end;

constructor TNewitem.Create(item: SNewItem);
begin
  _name := item.Name;
  _id := item.ID;
  _itemtype := item.ItemType;
  _volume := item.Volume;
  _race := item.Race;
  _buildtime := item.BuildTime;
  _productivitymodifier := item.ProductivityModifier;
  _inventiontime := item.InventionTime;
  _inventiontype := item.InventionType;
  _inventionrace := item.InventionRace;
  _copytime := item.CopyTime;
  _productionlimit := item.ProductionLimit;
  _basechance := item.BaseChance;
  _wastefactor := item.WasteFactor;
  _buildtype := item.BuildType;
  _portionsize := item.PortionSize;

  _price := 0;
  _manufacture := false;
  _bpome := 0;
  _category := nil;
  _subcategory := nil;
  _group := nil;

  _usedIn := TList.Create;
  _requirements := TList.Create;

  case item.InventionRace of
    Amarr: _encryptionskill := SK_AmarrEncryption;
    Caldari: _encryptionskill := SK_CaldariEncryption;
    Gallente: _encryptionskill := SK_GallenteEncryption;
    Minmatar: _encryptionskill := SK_MinmatarEncryption;
  end;
  _skill1 := SK_None;
  _skill2 := SK_None;
  _datacore1amount := 0;
  _datacore2amount := 0;
end;

destructor TNewItem.Destroy;
var
  i: Integer;
begin
  for i := 0 to _requirements.Count -1 do begin
    GetRequirement(i).Destroy;
  end;
  Inherited Destroy;
end;

procedure TNewItem.AddUsedIn(ID: Integer; Item: TNewItem; Amount: Integer; Damage: Double = 1);
var
  itemamount: TItemAmount;
begin
  itemamount := TItemAmount.Create;
  itemamount.ID := ID;
  itemamount.Item := Item;
  itemamount.Activity := ACT_None;
  itemamount.Amount := Amount;
  itemamount.ApplyWaste := false;
  itemamount.Damage := Damage;
  AddUsedIn(itemamount);
end;

procedure TNewItem.AddUsedIn(itemamount: TItemAmount);
begin
  _usedIn.Add(itemamount);
end;

function TNewItem.GetUsedIn(index: Integer): TItemAmount;
begin
  if (_usedIn.Count > index) then
    result := TItemAmount(_usedIn[index])
  else result := nil;

//  result := TItemAmount(_usedIn[index]);
end;

procedure TNewItem.RemoveUsedIn(ID: Integer);
var
  i: Integer;
  itemamount: TItemAmount;
begin
  for i := 0 to _usedIn.Count -1 do begin
    itemamount := TItemAmount(_usedIn[i]);
    if (itemamount.ID = ID) then begin
      _usedIn.Delete(i);
      itemamount.Destroy;
    end;
  end;
end;

procedure TNewItem.RemoveUsedInByIndex(index: Integer);
begin
  _usedIn.Delete(i);
end;

procedure TNewItem.AddRequirement(ID: Integer; Item: TNewItem; Activity: EActivity; Amount: Integer; ApplyWaste: Boolean; Damage: Double = 1);
var
  itemamount: TItemAmount;
begin
  itemamount := TItemAmount.Create;
  itemamount.ID := ID;
  itemamount.Item := Item;
  itemamount.Activity := Activity;
  itemamount.Amount := Amount;
  itemamount.ApplyWaste := ApplyWaste;
  itemamount.Damage := Damage;
  AddRequirement(itemamount);

  if ((item <> nil) and (item.ItemType = IT_Datacore)) then begin
    if (_skill1 = SK_None) then begin
      if (AnsiContainsText(item.Name, 'Amarrian Starship Engineering')) then begin _skill1 := SK_AmarrianStarship; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Caldari Starship Engineering')) then begin _skill1 := SK_CaldariStarship; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Electromagnetic Physics')) then begin _skill1 := SK_Electromagnetic; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Electronic Engineering')) then begin _skill1 := SK_Electronic; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Gallentean Starship Engineering')) then begin _skill1 := SK_GallenteanStarship; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Graviton Physics')) then begin _skill1 := SK_Graviton; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'High Energy Physics')) then begin _skill1 := SK_HighEnergy; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Hydromagnetic Physics')) then begin _skill1 := SK_Hydromagnetic; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Laser Physics')) then begin _skill1 := SK_Laser; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Mechanical Engineering')) then begin _skill1 := SK_Mechanical; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Minmatar Starship Engineering')) then begin _skill1 := SK_MinmatarStarship; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Molecular Engineering')) then begin _skill1 := SK_Molecular; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Nanite Engineering')) then begin _skill1 := SK_Nanite; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Nuclear Physics')) then begin _skill1 := SK_Nuclear; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Plasma Physics')) then begin _skill1 := SK_Plasma; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Quantum Physics')) then begin _skill1 := SK_Quantum; _datacore1amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Rocket Science')) then begin _skill1 := SK_Rocket; _datacore1amount := Amount; end;
    end
    else if (_skill2 = SK_None) then begin
      if (AnsiContainsText(item.Name, 'Amarrian Starship Engineering')) then begin _skill2 := SK_AmarrianStarship; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Caldari Starship Engineering')) then begin _skill2 := SK_CaldariStarship; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Electromagnetic Physics')) then begin _skill2 := SK_Electromagnetic; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Electronic Engineering')) then begin _skill2 := SK_Electronic; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Gallentean Starship Engineering')) then begin _skill2 := SK_GallenteanStarship; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Graviton Physics')) then begin _skill2 := SK_Graviton; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'High Energy Physics')) then begin _skill2 := SK_HighEnergy; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Hydromagnetic Physics')) then begin _skill2 := SK_Hydromagnetic; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Laser Physics')) then begin _skill2 := SK_Laser; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Mechanical Engineering')) then begin _skill2 := SK_Mechanical; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Minmatar Starship Engineering')) then begin _skill2 := SK_MinmatarStarship; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Molecular Engineering')) then begin _skill2 := SK_Molecular; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Nanite Engineering')) then begin _skill2 := SK_Nanite; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Nuclear Physics')) then begin _skill2 := SK_Nuclear; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Plasma Physics')) then begin _skill2 := SK_Plasma; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Quantum Physics')) then begin _skill2 := SK_Quantum; _datacore2amount := Amount; end;
      if (AnsiContainsText(item.Name, 'Rocket Science')) then begin _skill2 := SK_Rocket; _datacore2amount := Amount; end;
    end;
  end;
end;

procedure TNewItem.AddRequirement(itemamount: TItemAmount);
begin
  _requirements.Add(itemamount);
end;

function TNewItem.GetRequirement(index: Integer): TItemAmount;
begin
  result := TItemAmount(_requirements[index]);
end;

procedure TNewItem.RemoveRequirement(ID: Integer; activity: EActivity);
var
  i: Integer;
  itemamount: TItemAmount;
begin
  for i := 0 to _requirements.Count -1 do begin
    itemamount := TItemAmount(_requirements[i]);
    if ((itemamount.ID = ID) and (itemamount.Activity = activity)) then begin
      _requirements.Delete(i);
      itemamount.Destroy;
    end;
  end;
end;

procedure TNewItem.RemoveRequirementByIndex(index: Integer);
begin
  TItemAmount(_requirements[index]).Destroy;
  _requirements.Delete(index);
end;

procedure TNewItem.SetLinkedItem(itemamount: TItemAmount; linkeditem: TNewItem);
begin
  itemamount.Item := linkeditem;
  if (linkeditem.ItemType = IT_Datacore) then begin
    if (_skill1 = SK_None) then begin
      if (AnsiContainsText(linkeditem.Name, 'Amarrian Starship Engineering')) then begin _skill1 := SK_AmarrianStarship; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Caldari Starship Engineering')) then begin _skill1 := SK_CaldariStarship; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Electromagnetic Physics')) then begin _skill1 := SK_Electromagnetic; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Electronic Engineering')) then begin _skill1 := SK_Electronic; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Gallentean Starship Engineering')) then begin _skill1 := SK_GallenteanStarship; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Graviton Physics')) then begin _skill1 := SK_Graviton; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'High Energy Physics')) then begin _skill1 := SK_HighEnergy; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Hydromagnetic Physics')) then begin _skill1 := SK_Hydromagnetic; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Laser Physics')) then begin _skill1 := SK_Laser; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Mechanical Engineering')) then begin _skill1 := SK_Mechanical; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Minmatar Starship Engineering')) then begin _skill1 := SK_MinmatarStarship; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Molecular Engineering')) then begin _skill1 := SK_Molecular; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Nanite Engineering')) then begin _skill1 := SK_Nanite; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Nuclear Physics')) then begin _skill1 := SK_Nuclear; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Plasma Physics')) then begin _skill1 := SK_Plasma; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Quantum Physics')) then begin _skill1 := SK_Quantum; _datacore1amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Rocket Science')) then begin _skill1 := SK_Rocket; _datacore1amount := itemamount.Amount; end;
    end
    else if (_skill2 = SK_None) then begin
      if (AnsiContainsText(linkeditem.Name, 'Amarrian Starship Engineering')) then begin _skill2 := SK_AmarrianStarship; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Caldari Starship Engineering')) then begin _skill2 := SK_CaldariStarship; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Electromagnetic Physics')) then begin _skill2 := SK_Electromagnetic; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Electronic Engineering')) then begin _skill2 := SK_Electronic; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Gallentean Starship Engineering')) then begin _skill2 := SK_GallenteanStarship; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Graviton Physics')) then begin _skill2 := SK_Graviton; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'High Energy Physics')) then begin _skill2 := SK_HighEnergy; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Hydromagnetic Physics')) then begin _skill2 := SK_Hydromagnetic; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Laser Physics')) then begin _skill2 := SK_Laser; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Mechanical Engineering')) then begin _skill2 := SK_Mechanical; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Minmatar Starship Engineering')) then begin _skill2 := SK_MinmatarStarship; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Molecular Engineering')) then begin _skill2 := SK_Molecular; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Nanite Engineering')) then begin _skill2 := SK_Nanite; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Nuclear Physics')) then begin _skill2 := SK_Nuclear; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Plasma Physics')) then begin _skill2 := SK_Plasma; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Quantum Physics')) then begin _skill2 := SK_Quantum; _datacore2amount := itemamount.Amount; end;
      if (AnsiContainsText(linkeditem.Name, 'Rocket Science')) then begin _skill2 := SK_Rocket; _datacore2amount := itemamount.Amount; end;
    end;
  end;
end;

function TNewItem.GetRequirementsList(runs, me: Integer; activity: EActivity; extrawastemodifier: Double): SRequirementsList;
// Returns items that are needed in some activity for this item. Applies waste and calculates damage.
// Use for displaying activity requirements
// Used internally when requesting build/invention cost
// extrawastemodifier is the total extra (or less) waste that needs to be applied because of skills or installation (Note that it is a modifier, so set it to 1 for no change)

var
  waste: Double;
  i, len: Integer;
  itemamount: TItemAmount;
  damageamount: Double;
  finalamount: Double;
begin
  if (me < 0) then waste := ((abs(me) +1) * (WasteFactor / 100))
  else waste := (((WasteFactor / 100) / (me + 1)) );
  SetLength(result, 0);
  for i := 0 to RequirementsCount -1 do begin
    itemamount := GetRequirement(i);
    if (itemamount.Activity = activity) then begin
      len := Length(result);
      Setlength(result, len +1);
      result[len].Item := itemamount.Item;
      if (itemamount.Damage > 0) then begin
        damageamount := itemamount.Amount * itemamount.Damage;
        result[len].Damage := 1;
        if (itemamount.ApplyWaste) then begin
          // Trinity:
          //result[len].Amount := Round(Ceil(runs * damageamount) * (1+waste) * extrawastemodifier);
          // EA:
//          result[len].Amount := runs * Round(SimpleRoundTo((Ceil(damageamount) * (1+waste) * extrawastemodifier), 0));
          finalamount := runs * Round(SimpleRoundTo((damageamount * (1+waste) * extrawastemodifier), 0));
        end
        else
          // Trinity:
          //result[len].Amount := Ceil(damageamount * runs);
          // EA:
//          result[len].Amount := runs * Ceil(damageamount);
          finalamount := runs * damageamount;
        result[len].Amount := Ceil(finalamount);
      end
      else begin
        result[len].Damage := 0;
        result[len].Amount := itemamount.Amount;
      end;
    end;
  end;
end;

function TNewItem.GetActivityCost(runs, me: Integer; activity: EActivity; extrawastemodifier: Double): Double;
// extrawastemodifier is the total extra (or less) waste that needs to be applied because of skills or installation (Note that it is a modifier, so set it to 1 for no change)
// Does not include installation costs
var
  i: Integer;
  list: SRequirementsList;
begin
  list := GetRequirementsList(runs, me, activity, extrawastemodifier);
  result := 0;
  for i := 0 to Length(list) -1 do begin
    result := result + (list[i].Item.Price * list[i].Amount);
  end;
end;

function TNewItem.GetBuildCost(runs, me: Integer; extrawastemodifier: Double): Double;
begin
  result := GetActivityCost(runs, me, ACT_Manufacture, extrawastemodifier);
end;

function TNewItem.GetInventionCost(runs: Integer; extrawastemodifier: Double): Double;
begin
  result := GetActivityCost(runs, MAXINT-1, ACT_Invention, extrawastemodifier);
end;

function TNewItem.GetCopyCost(runs: Integer; extrawastemodifier: Double): Double;
begin
  result := GetActivityCost(runs, MAXINT-1, ACT_Copying, extrawastemodifier);
end;

function TNewItem.ToSNewItem: SNewItem;
var
  item: SNewItem;
begin
  item.Name := _name;
  item.ID := _id;
  item.ItemType := _itemtype;
  item.Volume := _volume;
  item.Race := _race;
  item.BuildTime := _buildtime;
  item.ProductivityModifier := _productivitymodifier;
  item.InventionTime := _inventiontime;
  item.InventionType := _inventiontype;
  item.InventionRace := _inventionrace;
  item.CopyTime := _copytime;
  item.ProductionLimit := _productionlimit;
  item.BaseChance := _basechance;
  item.WasteFactor := _wastefactor;
  item.BuildType := _buildtype;
  item.PortionSize := _portionsize;

  item.UsedInAmount := _usedIn.Count;
  item.RequirementAmount := _requirements.Count;
  result := item;
end;

function TNewItem.ToSNewItemUserSetting: SNewItemUserSetting;
var
  item: SNewItemUserSetting;
begin
  item.ID := _id;
  item.Price := _price;
  item.Manufacture := _manufacture;
  item.BPOME := _bpome;
  if (_category = nil) then item.CategoryID := -1
  else item.CategoryID := _category.ID;
  if (_subcategory = nil) then item.SubcategoryID := -1
  else item.SubcategoryID := _subcategory.ID;
  if (_group = nil) then item.GroupID := -1
  else item.GroupID := _group.ID;
  result := item;
end;

procedure TNewItem.LoadUserSetting(settings: SNewItemUserSetting);
begin
  if (_id = settings.ID) then begin
    _price := settings.Price;
    _manufacture := settings.Manufacture;
    _bpome := settings.BPOME;
    // load category, subcategory and group in a place where you can actually fetch the instances (inventionCalc for example)
  end;
end;

end.
