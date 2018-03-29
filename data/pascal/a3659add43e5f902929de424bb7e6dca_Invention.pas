unit Invention;
{
  Holds the class that calculates invention results. Used to display -all- information on the main form (so also build cost, which T2 items a certain T1 item has, etc.)


  The following things are needed for this, and how that information is obtained:
  Needed                 | obtained from
  -----------------------+-----------------------
  T1 item                | parameter of constructor
  Datacore cost          | price property of datacore
  BPC cost               | default setting for installation cost; time needed from item **skill dependant**
  Meta item cost         | default setting, edited by means of an edit box on the main form
  Decryptor              | default setting, edited by means of a combobox on the main form
  Success probability    | TSuccess, accessible via inventionCalc.Success

  Results of invention, and how that information is obtained:
  Result                 | obtained from
  -----------------------+-----------------------
  Invention cost         | datacore cost + bpc cost + decryptor cost + meta item cost
  Output BPC runs        | max(1, (BPC runs / BPC max runs) * (T2 item production limit) / 10 + decryptor modifier)
  Output BPC ME & PE     | -4 + decryptor ME modifier
  Invention revenue      | (T2 item price - T2 item build cost - factory hourly cost * T2 item manufacturing time - factory installation cost) * output BPC runs * #items per run **skill dependant**
}
interface

uses
  GlobalRecordsAndEnums, Items, Decryptor, InventionCalc;

type
  TInvention = class
    private
      _t1item: TNewItem;
      _decryptor: TDecryptor;
      _metaitemcost: Double;
      _metaitemlevel: Integer;
      _bpcruns: Integer;
      _copyinstallationtimemodifier: Double;
      _inventioninstallationtimemodifier: Double;
      _manufacturinginstallationtimemodifier: Double;
      _manufacturinginstallationwastemodifier: Double;

      _success: Double; // just a placeholder so chances don't have to be calculated every time
      _t2item: TNewItem;

      procedure WriteT2Item(value: TNewItem);
    public
      constructor Create(item: TNewItem; invCalc: TInventionCalc);

      function GetSingleRunCopyTime: Integer;
      function GetMaxRunCopyTime: Integer;

      function GetInstallationCost(activity: EActivity): Double;

      function GetRequiredMaterialsCost: Double;
      function GetDecryptorCost: Double;
      function GetInventionJobCost: Double;
      function GetTotalCopyTime: Integer;
      function GetTotalInventionTime: Integer;

      function GetOutputME: Integer;
      function GetOutputPE: Integer;
      function GetOutputRuns: Integer;
      function GetT2ItemBuildCost: Double;
      function GetTotalT2ManufactureTime: Integer;
      function GetT2ManufactureTime: Integer;
      function GetComponentsManufactureTime: Integer;

      function GetT2ItemInventionRevenue: Double;

      function GetSuccess: Double;
      function GetAttempts: Double;
      function GetSuccessCost: Double;
      function GetProfitPerSuccess: Double;
      function GetProfitPerJob: Double;
      function GetTotalInventionCost: Double; // i.e, the price a T2 item should have to make 0 profit.

      function GetHighestProfitPerJob: Double;
      function GetBestProfitPerJobWithDecryptor: Double;

      function GetProfitPerManufactureDay: Double;
      function GetBestProfitPerManufactureDay: Double;

      property T1Item: TNewItem read _t1item;
      property T2Item: TNewItem read _t2item write WriteT2Item;
      property Decryptor: TDecryptor read _decryptor write _decryptor;

      property BPCRuns: Integer read _bpcruns write _bpcruns;
      property MetaItemCost: Double read _metaitemcost write _metaitemcost ;
      property MetaItemLevel: Integer read _metaitemlevel write _metaitemlevel;
      property CopyInstallationTimeModifier: Double read _copyinstallationtimemodifier write _copyinstallationtimemodifier;
      property InventionInstallationTimeModifier: Double read _inventioninstallationtimemodifier write _inventioninstallationtimemodifier;
      property ManufacturingInstallationTimeModifier: Double read _manufacturinginstallationtimemodifier write _manufacturinginstallationtimemodifier;
      property ManufacturingInstallationWasteModifier: Double read _manufacturinginstallationwastemodifier write _manufacturinginstallationwastemodifier;
  end;

var
  inventionCalc: TInventionCalc;

implementation

uses
  Math, UserSettings;

procedure TInvention.WriteT2Item(value: TNewItem);
var
  i: Integer;
begin
  for i := 0 to _t1item.UsedInCount -1 do begin
    if (_t1item.GetUsedIn(i).Item = value) then _t2item := value;
  end;
end;

constructor TInvention.Create(item: TNewItem; invCalc: TInventionCalc);
begin
  Inherited Create;
  inventionCalc := invCalc;
  case item.InventionType of
    Normal: begin
              case item.InventionRace of
                Amarr: self._decryptor := inventionCalc.GetDecryptorByID(inventionCalc.UserSettings.Default.NormalAmarrDecryptorID);
                Caldari: self._decryptor := inventionCalc.GetDecryptorByID(inventionCalc.UserSettings.Default.NormalCaldariDecryptorID);
                Gallente: self._decryptor := inventionCalc.GetDecryptorByID(inventionCalc.UserSettings.Default.NormalGallenteDecryptorID);
                Minmatar: self._decryptor := inventionCalc.GetDecryptorByID(inventionCalc.UserSettings.Default.NormalMinmatarDecryptorID);
              end;
              self._metaitemcost := inventionCalc.UserSettings.Default.NormalMetaItemPrice;
              self._metaitemlevel := inventionCalc.UserSettings.Default.NormalMetaItemLevel;
              if (inventionCalc.UserSettings.Default.NormalUseMaxRunsBPC) then
                self._bpcruns := item.ProductionLimit
              else
                self._bpcruns := Min(inventionCalc.UserSettings.Default.NormalBPCRuns, item.ProductionLimit);
            end;
    Ship  : begin
              case item.InventionRace of
                Amarr: self._decryptor := inventionCalc.GetDecryptorByID(inventionCalc.UserSettings.Default.ShipAmarrDecryptorID);
                Caldari: self._decryptor := inventionCalc.GetDecryptorByID(inventionCalc.UserSettings.Default.ShipCaldariDecryptorID);
                Gallente: self._decryptor := inventionCalc.GetDecryptorByID(inventionCalc.UserSettings.Default.ShipGallenteDecryptorID);
                Minmatar: self._decryptor := inventionCalc.GetDecryptorByID(inventionCalc.UserSettings.Default.ShipMinmatarDecryptorID);
              end;
              self._metaitemcost := inventionCalc.UserSettings.Default.ShipMetaItemPrice;
              self._metaitemlevel := inventionCalc.UserSettings.Default.ShipMetaItemLevel;
              if (inventionCalc.UserSettings.Default.ShipUseMaxRunsBPC) then
                self._bpcruns := item.ProductionLimit
              else
                self._bpcruns := Min(inventionCalc.UserSettings.Default.ShipBPCRuns, item.ProductionLimit);
            end;
    Rig   : begin
              case item.InventionRace of
                Amarr: self._decryptor := inventionCalc.GetDecryptorByID(inventionCalc.UserSettings.Default.RigAmarrDecryptorID);
                Caldari: self._decryptor := inventionCalc.GetDecryptorByID(inventionCalc.UserSettings.Default.RigCaldariDecryptorID);
                Gallente: self._decryptor := inventionCalc.GetDecryptorByID(inventionCalc.UserSettings.Default.RigGallenteDecryptorID);
                Minmatar: self._decryptor := inventionCalc.GetDecryptorByID(inventionCalc.UserSettings.Default.RigMinmatarDecryptorID);
              end;
              self._metaitemcost := inventionCalc.UserSettings.Default.RigMetaItemPrice;
              self._metaitemlevel := inventionCalc.UserSettings.Default.RigMetaItemLevel;
              if (inventionCalc.UserSettings.Default.RigUseMaxRunsBPC) then
                self._bpcruns := item.ProductionLimit
              else
                self._bpcruns := Min(inventionCalc.UserSettings.Default.RigBPCRuns, item.ProductionLimit);
            end;
  end;
  self._t1item := item;
  self._copyinstallationtimemodifier := 1;
  self._inventioninstallationtimemodifier := 1;
  self._manufacturinginstallationtimemodifier := 1;
  self._manufacturinginstallationwastemodifier := 1;
  self._success := 0;
  self._t2Item := _t1item.GetUsedIn(0).Item;
end;

//==============================
//====== Static functions ======
//==============================

// For some reason actual copy times ingame are twice the copy times as taken from the database export...

function TInvention.GetSingleRunCopyTime: Integer;
begin
  result := Ceil(_t1item.CopyTime * (1-(0.05*inventionCalc.UserSettings.Skills.Science)) * inventionCalc.UserSettings.Installations.CopyTimeModifier * inventionCalc.UserSettings.Skills.GetBeancounterKModifier / _t1item.ProductionLimit * 2);
end;

function TInvention.GetMaxRunCopyTime: Integer;                                                                                                                                                      
begin
  result := Ceil(_t1item.CopyTime * (1-(0.05*inventionCalc.UserSettings.Skills.Science)) * inventionCalc.UserSettings.Installations.CopyTimeModifier * inventionCalc.UserSettings.Skills.GetBeancounterKModifier) * 2;
end;

//====================================
//====== General cost functions ======
//====================================

function TInvention.GetInstallationCost(activity: EActivity): Double;
begin
  case activity of
    ACT_Copying: result := inventionCalc.UserSettings.Installations.CopyInstallation + (inventionCalc.UserSettings.Installations.CopyHourly * (GetTotalCopyTime / 3600));
    ACT_Invention: result := inventionCalc.UserSettings.Installations.InventionInstallation + (inventionCalc.UserSettings.Installations.InventionHourly * (GetTotalInventionTime / 3600));
    ACT_Manufacture: result := inventionCalc.UserSettings.Installations.ManufactureInstallation + (inventionCalc.UserSettings.Installations.ManufactureHourly * (GetT2ManufactureTime / 3600));
  else
    result := 0;
  end;
end;

//======================================
//====== Invention cost functions ======
//======================================

function TInvention.GetRequiredMaterialsCost: Double;
begin
  result := _t1item.GetInventionCost(1, inventionCalc.UserSettings.Installations.InventionMaterialModifier);
end;

function TInvention.GetDecryptorCost: Double;
begin
  if (_decryptor <> nil) then result := _decryptor.Price
  else result := 0;
end;

function TInvention.GetInventionJobCost: Double;
begin
  result := _t1item.GetInventionCost(1, inventionCalc.UserSettings.Installations.InventionMaterialModifier); // requiredmaterial cost
  result := result + self.GetDecryptorCost;
  result := result + self.MetaItemCost;
  result := result + self.GetInstallationCost(ACT_Copying);
  result := result + self.GetInstallationCost(ACT_Invention);
end;

function TInvention.GetTotalCopyTime: Integer;
begin
  //result := _bpcruns * self.GetSingleRunCopyTime;
  result := Ceil(_t1item.CopyTime * (1-(0.05*inventionCalc.UserSettings.Skills.Science)) * inventionCalc.UserSettings.Installations.CopyTimeModifier * inventionCalc.UserSettings.Skills.GetBeancounterKModifier / _t1item.ProductionLimit * _bpcruns * 2);
end;

function TInvention.GetTotalInventionTime: Integer;
begin
  result := Round(SimpleRoundTo(_t1item.InventionTime * inventionCalc.UserSettings.Installations.InventionTimeModifier, 0));
end;

//=========================================
//====== Invention revenue functions ======
//=========================================

function TInvention.GetOutputME: Integer;
begin
  if (_decryptor <> nil) then result := _decryptor.ME -4
  else result := -4;
end;

function TInvention.GetOutputPE: Integer;
begin
  if (_decryptor <> nil) then result := _decryptor.PE -4
  else result := -4;
end;

function TInvention.GetOutputRuns: Integer;
var
  decr: Integer;
begin
  if (_decryptor <> nil) then decr := _decryptor.Runs
  else decr := 0;
  if (_t2item = nil) then result := 0
  else result := Min(Max(1, Floor((_bpcruns / _t1item.ProductionLimit) * Floor(_t2item.ProductionLimit / 10) + decr)), _t2item.ProductionLimit);
end;

function TInvention.GetT2ItemBuildCost: Double;
begin
  if (_t2item = nil) then result := 0
  else result := _t2item.GetBuildCost(1, GetOutputME, inventionCalc.UserSettings.Installations.ManufactureMaterialModifier * inventionCalc.UserSettings.Skills.GetProductionEfficiencyModifier);
end;

function TInvention.GetTotalT2ManufactureTime: Integer;
begin
    result:=GetT2ManufactureTime+GetComponentsManufactureTime;
end;

function TInvention.GetComponentsManufactureTime: Integer;
var i:integer;
    itemamount:TItemAmount;
    linkeditem:TNewItem;
begin
    result:=0;

    for i := _t2item.RequirementsCount-1 downto 0 do begin
        itemamount := _t2item.GetRequirement(i);
        linkeditem := TNewItem(inventionCalc.GetItem(itemamount.ID));
        if (linkeditem.ItemType in [IT_T1Item, IT_T2Item, IT_T2Component, IT_RAM, IT_CapComponent, IT_CapT2Component]) then
        begin
            result:=result+Round(itemamount.Amount*linkeditem.BuildTime*itemamount.Damage);
        end;
    end;
end;

function TInvention.GetT2ManufactureTime: Integer;
begin
  if (GetOutputPE >= 0) then begin
    result := Ceil(
                   _t2item.BuildTime *
                   (1-
                      (_t2item.ProductivityModifier/_t2item.BuildTime) *
                      (GetOutputPE/(1+GetOutputPE))
                   ) *
                   (1-(0.04*inventionCalc.UserSettings.Skills.Industry)) *
                   inventionCalc.UserSettings.Installations.ManufactureTimeModifier *
                   inventionCalc.UserSettings.Skills.GetBeancounterFModifier
                  );
  end
  else begin
    result := Ceil(_t2item.BuildTime * (1+
                      (_t2item.ProductivityModifier/_t2item.BuildTime) *
                      (abs(GetOutputPE) + 1)
                   ) *
                   (1-(0.04*inventionCalc.UserSettings.Skills.Industry)) *
                   inventionCalc.UserSettings.Installations.ManufactureTimeModifier *
                   inventionCalc.UserSettings.Skills.GetBeancounterFModifier
                  );
  end;
end;

function TInvention.GetT2ItemInventionRevenue: Double;
var
  broker, sales: Double;
begin
  if (_t2item = nil) then result := 0
//  else result := (_t2item.Price * GetOutputRuns * _t2item.PortionSize) - (GetT2ItemBuildCost + GetInstallationCost(ACT_Manufacture)) * GetOutputRuns;
  else result := ((_t2item.Price * GetOutputRuns * _t2item.PortionSize) - _t2item.GetBuildCost(GetOutputRuns, GetOutputME, inventionCalc.UserSettings.Installations.ManufactureMaterialModifier * inventionCalc.UserSettings.Skills.GetProductionEfficiencyModifier) - (GetOutputRuns * GetInstallationCost(ACT_Manufacture)));
  if (inventionCalc.UserSettings.Settings.UseFees) then begin
    broker := result * inventionCalc.UserSettings.Settings.BrokersFee;
    sales := result * inventionCalc.UserSettings.Settings.SalesTax;
    result := result - broker - sales;
  end;
end;

//========================================================
//============ Total invention cost functions ============
//========================================================

function TInvention.GetSuccess: Double;
var
  decrprob: Double;
begin
  result := 0;
  if (_decryptor = nil) then decrprob := 1
  else decrprob := _decryptor.Probability;
  _success := inventionCalc.UserSettings.Success.GetSuccess(_t1item.InventionType,
                                                            _t2item.BaseChance,
                                                            inventionCalc.UserSettings.Skills.GetSkillLevel(_t1item.EncryptionSkill),
                                                            inventionCalc.UserSettings.Skills.GetSkillLevel(_t1item.ScienceSkill1),
                                                            inventionCalc.UserSettings.Skills.GetSkillLevel(_t1item.ScienceSkill2),
                                                            _t1item.Datacore1Amount,
                                                            _t1item.Datacore2Amount,
                                                            decrprob,
                                                            _metaitemlevel
                                                            );
  result := _success;
end;

function TInvention.GetAttempts: Double;
begin
  if (_success > 0) then result := Power(GetSuccess, -1)  // the number of attemps is the inverse of the success %, f.e. if you have a 20% chance, you need 5 attempts to be successful
  else result := 0;
end;

function TInvention.GetSuccessCost: Double;
var cost, attempts: Double;
begin
  cost := self.GetInventionJobCost;
  attempts := self.GetAttempts;
  result := cost * attempts;
end;

function TInvention.GetProfitPerSuccess: Double;
begin
  result := self.GetT2ItemInventionRevenue - self.GetSuccessCost;
end;

function TInvention.GetProfitPerJob: Double;
var revenue, success_cost: Double;
begin
  if (self.GetAttempts > 0) then
  begin
    revenue := self.GetT2ItemInventionRevenue;
    success_cost := self.GetSuccessCost;
    result := ( revenue - success_cost) / self.GetAttempts
  end
  else result := 0;
end;

function TInvention.GetTotalInventionCost;
var
  buildcosts, inventioncosts: Double;
begin
  buildcosts := _t2item.GetBuildCost(
                                 GetOutputRuns, GetOutputME, inventionCalc.UserSettings.Installations.ManufactureMaterialModifier * inventionCalc.UserSettings.Skills.GetProductionEfficiencyModifier
                                 ) +
                (GetOutputRuns * GetInstallationCost(ACT_Manufacture));
  inventioncosts := GetSuccessCost;
  result := (inventioncosts + buildcosts) / (GetOutputRuns * _t2item.PortionSize);
end;

function TInvention.GetHighestProfitPerJob: Double;
var
  i: Integer;
begin
  if (_success = 0) then _success := self.GetSuccess;
  result := 0 - MaxInt;
  for i := 0 to _t1item.UsedInCount -1 do begin
    _t2item := _t1item.GetUsedIn(i).Item;
    result := Max(result, self.GetProfitPerJob);
  end;
end;

function TInvention.GetBestProfitPerJobWithDecryptor: Double;
var
  i,j: Integer;
begin
  result := 0 - MaxInt;
  for i := 0 to _t1item.UsedInCount -1 do begin
    _t2item := _t1item.GetUsedIn(i).Item;
    for j := 0 to inventionCalc.DecryptorTypeCount do begin //LastDecriptor will return nil, i.e. no decryptor
       _decryptor := inventionCalc.GetDecryptor(_t1item.inventionRace, j);
       _success := self.GetSuccess;
       if self.GetProfitPerJob > result then begin
           result := self.GetProfitPerJob;
       end;
    end;
  end;
end;

function TInvention.GetProfitPermanufactureDay: Double;
begin
     result:=GetT2ItemInventionRevenue/(GetTotalT2ManufactureTime*GetOutputRuns)*3600*24;
end;

function TInvention.GetBestProfitPermanufactureDay: Double;
var
  i,j: Integer;
begin
  result := 0 - MaxInt;
  for i := 0 to _t1item.UsedInCount -1 do begin
    _t2item := _t1item.GetUsedIn(i).Item;
    for j := 0 to inventionCalc.DecryptorTypeCount do begin //LastDecriptor will return nil, i.e. no decryptor
       _decryptor := inventionCalc.GetDecryptor(_t1item.inventionRace, j);
       _success := self.GetSuccess;
       if self.GetProfitPermanufactureDay > result then begin
           result := self.GetProfitPermanufactureDay;
       end;
    end;
  end;
end;




end.




