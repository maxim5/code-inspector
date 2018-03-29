unit UserSettingsGui;

interface

uses
  InventionCalc, GlobalRecordsAndEnums, UserSettings, Decryptor, Location,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  XMLDoc, XMLIntf,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP;

type
  TfrmUserSettings = class(TForm)
    ButtonPanel: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    PageControl: TPageControl;
    tsSettings: TTabSheet;
    tsDefaults: TTabSheet;
    tsColors: TTabSheet;
    tsSuccess: TTabSheet;
    tsSkills: TTabSheet;
    tsInstallations: TTabSheet;
    set_gbSales: TGroupBox;
    set_lblSalesTax: TLabel;
    set_lblBrokersFee: TLabel;
    set_edtSalesTax: TEdit;
    set_edtBrokersFee: TEdit;
    set_chkUseSales: TCheckBox;

    // Installations
    ins_gbCopy: TGroupBox;
    ins_lblCopyInstallation: TLabel;
    ins_lblCopyHourlyCost: TLabel;
    ins_edtCopyHourlyCost: TEdit;
    ins_edtCopyInstallationCost: TEdit;
    ins_gbInvention: TGroupBox;
    ins_lblInventionInstallationCost: TLabel;
    ins_lblInventionHourlyCost: TLabel;
    ins_edtInventionHourlyCost: TEdit;
    ins_edtInventionInstallationCost: TEdit;
    ins_gbManufacture: TGroupBox;
    ins_lblManufactureInstallationCost: TLabel;
    ins_lblManufactureHourlyCost: TLabel;
    ins_edtManufactureHourlyCost: TEdit;
    ins_edtManufactureInstallationCost: TEdit;
    ins_lblCopyTimeModifier: TLabel;
    ins_lblInventionTimeModifier: TLabel;
    ins_lblManufactureTimeModifier: TLabel;
    ins_edtCopyTimeModifier: TEdit;
    ins_edtCopyMaterialModifier: TEdit;
    ins_lblCopyMaterialModifier: TLabel;
    ins_lblInventionMaterialModifier: TLabel;
    ins_edtInventionMaterialModifier: TEdit;
    ins_edtInventionTimeModifier: TEdit;
    ins_lblManufactureMaterialModifier: TLabel;
    ins_edtManufactureTimeModifier: TEdit;
    ins_edtManufactureMaterialModifier: TEdit;
    // Installations end
    // Skills
    ski_udAmarrEncryption: TUpDown;
    ski_edtAmarrEncryption: TEdit;
    ski_udCaldariEncryption: TUpDown;
    ski_edtCaldariEncryption: TEdit;
    ski_udGallenteEncryption: TUpDown;
    ski_edtGallenteEncryption: TEdit;
    ski_udMinmatarEncryption: TUpDown;
    ski_edtMinmatarEncryption: TEdit;
    ski_udAmarrStarship: TUpDown;
    ski_edtAmarrStarship: TEdit;
    ski_udCaldariStarship: TUpDown;
    ski_edtCaldariStarship: TEdit;
    ski_udElectromagnetic: TUpDown;
    ski_edtElectromagnetic: TEdit;
    ski_udElectronic: TUpDown;
    ski_edtElectronic: TEdit;
    ski_udGallenteStarship: TUpDown;
    ski_edtGallenteStarship: TEdit;
    ski_udGraviton: TUpDown;
    ski_edtGraviton: TEdit;
    ski_udHighEnergy: TUpDown;
    ski_edtHighEnergy: TEdit;
    ski_udHydromagnetic: TUpDown;
    ski_edtHydromagnetic: TEdit;
    ski_udLaser: TUpDown;
    ski_edtLaser: TEdit;
    ski_udMechanical: TUpDown;
    ski_edtMechanical: TEdit;
    ski_udMinmatarStarship: TUpDown;
    ski_edtMinmatarStarship: TEdit;
    ski_udMolecular: TUpDown;
    ski_edtMolecular: TEdit;
    ski_udNanite: TUpDown;
    ski_edtNanite: TEdit;
    ski_udNuclear: TUpDown;
    ski_edtNuclear: TEdit;
    ski_udPlasma: TUpDown;
    ski_edtPlasma: TEdit;
    ski_udQuantum: TUpDown;
    ski_edtQuantum: TEdit;
    ski_udRocket: TUpDown;
    ski_edtRocket: TEdit;
    ski_udScience: TUpDown;
    ski_edtScience: TEdit;
    ski_udIndustry: TUpDown;
    ski_edtIndustry: TEdit;
    ski_udProdEff: TUpDown;
    ski_edtProdEff: TEdit;
    ski_gbInventionSkills: TGroupBox;
    ski_gbOtherSkills: TGroupBox;
    ski_lblAmarrEncryption: TLabel;
    ski_lblGallenteEncryption: TLabel;
    ski_lblCaldariEncryption: TLabel;
    ski_lblMinmatarEncryption: TLabel;
    ski_lblCaldariStarship: TLabel;
    ski_lblElectronic: TLabel;
    ski_lblElectromagnetic: TLabel;
    ski_lblGallenteStarship: TLabel;
    ski_lblAmarrStarship: TLabel;
    ski_lblHighEnergy: TLabel;
    ski_lblMechanical: TLabel;
    ski_lblHydromagnetic: TLabel;
    ski_lblGraviton: TLabel;
    ski_lblMinmatarStarship: TLabel;
    ski_lblLaser: TLabel;
    ski_lblNuclear: TLabel;
    ski_lblRocket: TLabel;
    ski_lblMolecular: TLabel;
    ski_lblQuantum: TLabel;
    ski_lblNanite: TLabel;
    ski_lblPlasma: TLabel;
    ski_lblProdEff: TLabel;
    ski_lblIndustry: TLabel;
    ski_lblScience: TLabel;
    ski_gbAPI: TGroupBox;
    ski_lblUserID: TLabel;
    ski_edtUserID: TEdit;
    ski_lblAPIkey: TLabel;
    ski_edtAPIkey: TEdit;
    ski_btnRetrieveCharacters: TButton;
    ski_lblCharacters: TLabel;
    ski_cbCharacters: TComboBox;
    ski_btnRetrieveSkills: TButton;
    ski_gbImplants: TGroupBox;
    ski_lblBeancounterG: TLabel;
    ski_lblBeancounterF: TLabel;
    ski_lblBeancounterK: TLabel;
    ski_cbBeancounterF: TComboBox;
    ski_cbBeancounterG: TComboBox;
    ski_cbBeancounterK: TComboBox;
    ski_lblBeancounterCI: TLabel;
    ski_lblShaman: TLabel;
    ski_lblCleric: TLabel;
    ski_lblDraftsman: TLabel;
    ski_cbBeancounterCI: TComboBox;
    ski_cbShaman: TComboBox;
    ski_cbCleric: TComboBox;
    ski_cbDraftsman: TComboBox;
    // Skills end
    // Invention formula
    inv_gbInventionSuccess: TGroupBox;
    inv_lblNormal: TLabel;
    inv_lblShip: TLabel;
    inv_chkFormula: TCheckBox;
    inv_edtNormal: TEdit;
    inv_edtShip: TEdit;
    inv_lblRig: TLabel;
    inv_edtRig: TEdit;
    inv_chkDecryptor: TCheckBox;
    inv_gbFormula: TGroupBox;
    inv_lblFormula: TLabel;
    inv_lblBase: TLabel;
    inv_lblEncrSkill: TLabel;
    inv_lblSkill1: TLabel;
    inv_lblSkill2: TLabel;
    inv_lblDatacore1: TLabel;
    inv_lblDatacore2: TLabel;
    inv_lblDecryptor: TLabel;
    inv_lblMeta: TLabel;
    inv_lblBaseDesc: TLabel;
    inv_lblEncrSkillDesc: TLabel;
    inv_lblSkill1Desc: TLabel;
    inv_lblSkill2Desc: TLabel;
    inv_lblDatacore1Desc: TLabel;
    inv_lblDatacore2Desc: TLabel;
    inv_lblDecryptorDesc: TLabel;
    inv_lblMetaDesc: TLabel;
    inv_lblOperators: TLabel;
    inv_lblWhitespace: TLabel;
    inv_edtFormula: TMemo;
    inv_lblImportant1: TLabel;
    inv_lblImportant2: TLabel;
    inv_lblPosition: TLabel;
    inv_btnTest: TButton;
    inv_btnReset: TButton;
    // Invention formula end
    // Colors
    col_gbColors: TGroupBox;
    col_lblT1Item: TLabel;
    col_lblT2Item: TLabel;
    col_lblAdvancedMaterial: TLabel;
    col_lblT2Component: TLabel;
    col_lblRAM: TLabel;
    col_lblTradegoods: TLabel;
    col_lblDatacore: TLabel;
    col_lblMineral: TLabel;
    col_lblCapComponent: TLabel;
    col_lblCapT2Component: TLabel;
    col_lblSalvage: TLabel;
    col_lblDataInterface: TLabel;
    col_lblchgCapComponent: TLabel;
    col_lblchgMineral: TLabel;
    col_lblchgDatacore: TLabel;
    col_lblchgTradegoods: TLabel;
    col_lblchgRAM: TLabel;
    col_lblchgT2Component: TLabel;
    col_lblchgAdvancedMaterial: TLabel;
    col_lblchgT2Item: TLabel;
    col_lblchgDataInterface: TLabel;
    col_lblchgSalvage: TLabel;
    col_lblchgCapT2Component: TLabel;
    col_lblchgT1Item: TLabel;
    col_gbExamples: TGroupBox;
    col_lblexT1Item: TLabel;
    col_lblexT2Item: TLabel;
    col_lblexAdvancedMaterial: TLabel;
    col_lblexT2Component: TLabel;
    col_lblexRAM: TLabel;
    col_lblexTradegoods: TLabel;
    col_lblexDatacore: TLabel;
    col_lblexMineral: TLabel;
    col_lblexCapComponent: TLabel;
    col_lblexCapT2Component: TLabel;
    col_lblexSalvage: TLabel;
    col_lblexDataInterface: TLabel;
    col_lstExample: TListView;
    col_gbColorWindows: TGroupBox;
    col_chkShowInfo: TCheckBox;
    col_chkEditItems: TCheckBox;
    col_gbColorAll: TGroupBox;
    col_lblColorAll: TLabel;
    col_lblchgColorAll: TLabel;
    // Colors end
    // Defaults
    def_gbNormal: TGroupBox;
    def_nor_gbDecryptor: TGroupBox;
    def_nor_lblAmarr: TLabel;
    def_nor_lblCaldari: TLabel;
    def_nor_lblGallente: TLabel;
    def_nor_lblMinmatar: TLabel;
    def_nor_cbAmarr: TComboBox;
    def_nor_cbCaldari: TComboBox;
    def_nor_cbGallente: TComboBox;
    def_nor_cbMinmatar: TComboBox;
    def_nor_gbMetaItem: TGroupBox;
    def_nor_gbBPCruns: TGroupBox;
    def_nor_lblBPCRuns: TLabel;
    def_nor_tbCopy: TTrackBar;
    def_nor_chkMaxRuns: TCheckBox;
    def_nor_edtMetaItem: TEdit;
    def_nor_cbMetaItem: TComboBox;
    def_nor_lblMetaItemPrice: TLabel;
    def_nor_lblMetaItemLevel: TLabel;
    def_gbRig: TGroupBox;
    def_ship_gbMetaItem: TGroupBox;
    def_ship_lblMetaItemPrice: TLabel;
    def_ship_lblMetaItemLevel: TLabel;
    def_ship_edtMetaItem: TEdit;
    def_ship_cbMetaitem: TComboBox;
    def_ship_gbBPCruns: TGroupBox;
    def_ship_lblBPCRuns: TLabel;
    def_ship_tbCopy: TTrackBar;
    def_ship_chkMaxRuns: TCheckBox;
    def_ship_gbDecryptor: TGroupBox;
    def_ship_lblAmarr: TLabel;
    def_ship_lblCaldari: TLabel;
    def_ship_lblGallente: TLabel;
    def_ship_lblMinmatar: TLabel;
    def_ship_cbAmarr: TComboBox;
    def_ship_cbCaldari: TComboBox;
    def_ship_cbGallente: TComboBox;
    def_ship_cbMinmatar: TComboBox;
    def_gbShip: TGroupBox;
    def_rig_gbMetaItem: TGroupBox;
    def_rig_lblMetaItemPrice: TLabel;
    def_rig_lblMetaItemLevel: TLabel;
    def_rig_edtMetaItem: TEdit;
    def_rig_cbMetaItem: TComboBox;
    def_rig_gbBPCruns: TGroupBox;
    def_rig_lblBPCRuns: TLabel;
    def_rig_tbCopy: TTrackBar;
    def_rig_chkMaxRuns: TCheckBox;
    def_rig_gbDecryptor: TGroupBox;
    def_rig_lblAmarr: TLabel;
    def_rig_lblCaldari: TLabel;
    def_rig_lblGallente: TLabel;
    def_rig_lblMinmatar: TLabel;
    def_rig_cbAmarr: TComboBox;
    def_rig_cbCaldari: TComboBox;
    def_rig_cbGallente: TComboBox;
    def_rig_cbMinmatar: TComboBox;
    def_nor_btnCopy: TButton;
    def_ship_btnCopy: TButton;
    def_rig_btnCopy: TButton;
    lstInstallations: TListView;
    ins_btnUseInstallation: TButton;
    Col_chkShoppingList: TCheckBox;
    btnHelp: TButton;
    Label1: TLabel;
    ski_lblAPIsite: TLabel;
    ski_btnAllV: TButton;

    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure ski_btnRetrieveCharactersClick(Sender: TObject);
    procedure ski_btnRetrieveSkillsClick(Sender: TObject);
    procedure inv_edtFormulaKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure inv_edtFormulaClick(Sender: TObject);
    procedure inv_btnResetClick(Sender: TObject);
    procedure inv_btnTestClick(Sender: TObject);
    procedure inv_chkFormulaClick(Sender: TObject);
//    procedure PageControlChange(Sender: TObject);
    procedure col_lblchgT1ItemClick(Sender: TObject);
    procedure col_lblchgT2ItemClick(Sender: TObject);
    procedure col_lblchgAdvancedMaterialClick(Sender: TObject);
    procedure col_lblchgT2ComponentClick(Sender: TObject);
    procedure col_lblchgRAMClick(Sender: TObject);
    procedure col_lblchgTradegoodsClick(Sender: TObject);
    procedure col_lblchgDatacoreClick(Sender: TObject);
    procedure col_lblchgMineralClick(Sender: TObject);
    procedure col_lblchgCapComponentClick(Sender: TObject);
    procedure col_lblchgCapT2ComponentClick(Sender: TObject);
    procedure col_lblchgSalvageClick(Sender: TObject);
    procedure col_lblchgDataInterfaceClick(Sender: TObject);
    procedure col_lstExampleCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure col_lblchgColorAllClick(Sender: TObject);
    procedure def_nor_tbCopyChange(Sender: TObject);
    procedure def_nor_btnCopyClick(Sender: TObject);
    procedure def_nor_chkMaxRunsClick(Sender: TObject);
    procedure def_ship_tbCopyChange(Sender: TObject);
    procedure def_ship_btnCopyClick(Sender: TObject);
    procedure def_ship_chkMaxRunsClick(Sender: TObject);
    procedure def_rig_tbCopyChange(Sender: TObject);
    procedure def_rig_btnCopyClick(Sender: TObject);
    procedure def_rig_chkMaxRunsClick(Sender: TObject);
    procedure set_chkUseSalesClick(Sender: TObject);
    procedure ins_btnUseInstallationClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure ski_lblAPIsiteClick(Sender: TObject);
    procedure ski_btnAllVClick(Sender: TObject);
    private
      inventionCalc: TInventionCalc;
      usersettings: TUserSettings;

      procedure LoadSettings;
      procedure LoadDefaults;
      procedure LoadColors;
      procedure LoadSuccess;
      procedure LoadSkills;
      procedure LoadInstallations;
      function CheckSettings: Boolean;
      function CheckDefaults: Boolean;
      function CheckColors: Boolean;
      function CheckSuccess: Boolean;
      function CheckSkills: Boolean;
      function CheckInstallations: Boolean;
      procedure SaveSettings;
      procedure SaveDefaults;
      procedure SaveColors;
      procedure SaveSuccess;
      procedure SaveSkills;
      procedure SaveInstallations;

      procedure LoadCustomColors(dlg: TColorDialog);
      procedure SaveCustomColors(dlg: TColorDialog);
    public
      constructor Create(AOwner: TComponent; invCalc: TInventionCalc; settings: TUserSettings); reintroduce;
  end;

var
  frmUserSettings: TfrmUserSettings;

implementation

{$R *.dfm}

uses
  StrUtils, ExtActns;

constructor TfrmUserSettings.Create(AOwner: TComponent; invCalc: TInventionCalc; settings: TUserSettings);
begin
  Inherited Create(AOwner);
  inventionCalc := invCalc;
  usersettings := settings;
  LoadSettings;
  LoadDefaults;
  LoadColors;
  LoadSuccess;
  LoadSkills;
  LoadInstallations;

  PageControl.ActivePageIndex := inventionCalc.UserSettings.Settings.SettingsActiveTab;
end;

procedure TfrmUserSettings.btnHelpClick(Sender: TObject);
var
  run: TFileRun;
begin
  run := TFileRun.Create(self);
  run.FileName := 'help\EditSettings.html';
  run.Execute;
  run.Free;
end;

procedure TfrmUserSettings.btnOkClick(Sender: TObject);
begin
  if (not CheckSettings) then PageControl.ActivePage := tsSettings
  else if (not CheckDefaults) then PageControl.ActivePage := tsDefaults
  else if (not CheckColors) then PageControl.ActivePage := tsColors
  else if (not CheckSuccess) then PageControl.ActivePage := tsSuccess
  else if (not CheckSkills) then PageControl.ActivePage := tsSkills
  else if (not CheckInstallations) then PageControl.ActivePage := tsInstallations
  else begin
    SaveSettings;
    SaveDefaults;
    SaveColors;
    SaveSuccess;
    SaveSkills;
    SaveInstallations;
    inventionCalc.UserSettings.Settings.SettingsActiveTab := PageControl.ActivePageIndex;
    self.Close;
    self.Release;
  end;
end;

procedure TfrmUserSettings.btnCancelClick(Sender: TObject);
begin
  inventionCalc.UserSettings.Settings.SettingsActiveTab := PageControl.ActivePageIndex;
  self.Close;
  self.Release;
end;

// ======================
// ====== Settings ======
// ======================

procedure TfrmUserSettings.LoadSettings;
begin
  set_chkUseSales.Checked := usersettings.Settings.UseFees;
  set_chkUseSalesClick(nil);
  set_edtSalesTax.Text := FloatToStr(usersettings.Settings.SalesTax);
  set_edtBrokersFee.Text := FloatToStr(usersettings.Settings.BrokersFee);
end;

function TfrmUserSettings.CheckSettings: Boolean;
var
  d: Double;
begin
  result := true;

  set_edtSalesTax.Color := clWindow;
  set_edtBrokersFee.Color := clWindow;

  set_edtSalesTax.Text := StringReplace(set_edtSalesTax.Text, '.', global_ds, [rfReplaceAll]);
  set_edtBrokersFee.Text := StringReplace(set_edtBrokersFee.Text, '.', global_ds, [rfReplaceAll]);

  if ((not TryStrToFloat(set_edtSalesTax.Text, d)) or (d > 1) or (d < 0)) then begin
    result := false;
    set_edtSalesTax.Color := clRed;
  end;
  if ((not TryStrToFloat(set_edtBrokersFee.Text, d)) or (d > 1) or (d < 0)) then begin
    result := false;
    set_edtBrokersFee.Color := clRed;
  end;
end;

procedure TfrmUserSettings.SaveSettings;
begin
  usersettings.Settings.UseFees := set_chkUseSales.Checked;
  usersettings.Settings.SalesTax := StrToFloat(set_edtSalesTax.Text);
  usersettings.Settings.BrokersFee := StrToFloat(set_edtBrokersFee.Text);
end;

procedure TfrmUserSettings.set_chkUseSalesClick(Sender: TObject);
begin
  set_lblSalesTax.Enabled := set_chkUseSales.Checked;
  set_lblBrokersFee.Enabled := set_chkUseSales.Checked;
  set_edtSalesTax.Enabled := set_chkUseSales.Checked;
  set_edtBrokersFee.Enabled := set_chkUseSales.Checked;
end;

// ======================
// ====== Defaults ======
// ======================

procedure TfrmUserSettings.LoadDefaults;
var
  i: Integer;
  decryptor: TDecryptor;
begin
  def_nor_cbAmarr.Items.AddObject('None', nil);
  def_nor_cbAmarr.ItemIndex := 0;
  def_ship_cbAmarr.Items.AddObject('None', nil);
  def_ship_cbAmarr.ItemIndex := 0;
  def_rig_cbAmarr.Items.AddObject('None', nil);
  def_rig_cbAmarr.ItemIndex := 0;
  for i := 0 to inventionCalc.AmarrDecryptorCount -1 do begin
    decryptor := inventionCalc.GetDecryptor(Amarr, i);
    def_nor_cbAmarr.Items.AddObject(decryptor.Name, decryptor);
    if (decryptor.ID = usersettings.Default.NormalAmarrDecryptorID) then def_nor_cbAmarr.ItemIndex := i+1;
    def_ship_cbAmarr.Items.AddObject(decryptor.Name, decryptor);
    if (decryptor.ID = usersettings.Default.ShipAmarrDecryptorID) then def_ship_cbAmarr.ItemIndex := i+1;
    def_rig_cbAmarr.Items.AddObject(decryptor.Name, decryptor);
    if (decryptor.ID = usersettings.Default.RigAmarrDecryptorID) then def_rig_cbAmarr.ItemIndex := i+1;
  end;

  def_nor_cbCaldari.Items.AddObject('None', nil);
  def_nor_cbCaldari.ItemIndex := 0;
  def_ship_cbCaldari.Items.AddObject('None', nil);
  def_ship_cbCaldari.ItemIndex := 0;
  def_rig_cbCaldari.Items.AddObject('None', nil);
  def_rig_cbCaldari.ItemIndex := 0;
  for i := 0 to inventionCalc.CaldariDecryptorCount -1 do begin
    decryptor := inventionCalc.GetDecryptor(Caldari, i);
    def_nor_cbCaldari.Items.AddObject(decryptor.Name, decryptor);
    if (decryptor.ID = usersettings.Default.NormalCaldariDecryptorID) then def_nor_cbCaldari.ItemIndex := i+1;
    def_ship_cbCaldari.Items.AddObject(decryptor.Name, decryptor);
    if (decryptor.ID = usersettings.Default.ShipCaldariDecryptorID) then def_ship_cbCaldari.ItemIndex := i+1;
    def_rig_cbCaldari.Items.AddObject(decryptor.Name, decryptor);
    if (decryptor.ID = usersettings.Default.RigCaldariDecryptorID) then def_rig_cbCaldari.ItemIndex := i+1;
  end;

  def_nor_cbGallente.Items.AddObject('None', nil);
  def_nor_cbGallente.ItemIndex := 0;
  def_ship_cbGallente.Items.AddObject('None', nil);
  def_ship_cbGallente.ItemIndex := 0;
  def_rig_cbGallente.Items.AddObject('None', nil);
  def_rig_cbGallente.ItemIndex := 0;
  for i := 0 to inventionCalc.GallenteDecryptorCount -1 do begin
    decryptor := inventionCalc.GetDecryptor(Gallente, i);
    def_nor_cbGallente.Items.AddObject(decryptor.Name, decryptor);
    if (decryptor.ID = usersettings.Default.NormalGallenteDecryptorID) then def_nor_cbGallente.ItemIndex := i+1;
    def_ship_cbGallente.Items.AddObject(decryptor.Name, decryptor);
    if (decryptor.ID = usersettings.Default.ShipGallenteDecryptorID) then def_ship_cbGallente.ItemIndex := i+1;
    def_rig_cbGallente.Items.AddObject(decryptor.Name, decryptor);
    if (decryptor.ID = usersettings.Default.RigGallenteDecryptorID) then def_rig_cbGallente.ItemIndex := i+1;
  end;

  def_nor_cbMinmatar.Items.AddObject('None', nil);
  def_nor_cbMinmatar.ItemIndex := 0;
  def_ship_cbMinmatar.Items.AddObject('None', nil);
  def_ship_cbMinmatar.ItemIndex := 0;
  def_rig_cbMinmatar.Items.AddObject('None', nil);
  def_rig_cbMinmatar.ItemIndex := 0;
  for i := 0 to inventionCalc.MinmatarDecryptorCount -1 do begin
    decryptor := inventionCalc.GetDecryptor(Minmatar, i);
    def_nor_cbMinmatar.Items.AddObject(decryptor.Name, decryptor);
    if (decryptor.ID = usersettings.Default.NormalMinmatarDecryptorID) then def_nor_cbMinmatar.ItemIndex := i+1;
    def_ship_cbMinmatar.Items.AddObject(decryptor.Name, decryptor);
    if (decryptor.ID = usersettings.Default.ShipMinmatarDecryptorID) then def_ship_cbMinmatar.ItemIndex := i+1;
    def_rig_cbMinmatar.Items.AddObject(decryptor.Name, decryptor);
    if (decryptor.ID = usersettings.Default.RigMinmatarDecryptorID) then def_rig_cbMinmatar.ItemIndex := i+1;
  end;

  def_nor_cbMetaItem.Items.Add('0');
  def_nor_cbMetaItem.Items.Add('1');
  def_nor_cbMetaItem.Items.Add('2');
  def_nor_cbMetaItem.Items.Add('3');
  def_nor_cbMetaItem.Items.Add('4');
  def_nor_cbMetaItem.ItemIndex := usersettings.Default.NormalMetaItemLevel;
  def_nor_edtMetaItem.Text := FloatToStr(usersettings.Default.NormalMetaItemPrice);

  def_ship_cbMetaItem.Items.Add('0');
  def_ship_cbMetaItem.Items.Add('1');
  def_ship_cbMetaItem.Items.Add('2');
  def_ship_cbMetaItem.Items.Add('3');
  def_ship_cbMetaItem.Items.Add('4');
  def_ship_cbMetaItem.ItemIndex := usersettings.Default.ShipMetaItemLevel;
  def_ship_edtMetaItem.Text := FloatToStr(usersettings.Default.ShipMetaItemPrice);

  def_rig_cbMetaItem.Items.Add('0');
  def_rig_cbMetaItem.Items.Add('1');
  def_rig_cbMetaItem.Items.Add('2');
  def_rig_cbMetaItem.Items.Add('3');
  def_rig_cbMetaItem.Items.Add('4');
  def_rig_cbMetaItem.ItemIndex := usersettings.Default.RigMetaItemLevel;
  def_rig_edtMetaItem.Text := FloatToStr(usersettings.Default.RigMetaItemPrice);

  def_nor_tbCopy.Position := usersettings.Default.NormalBPCRuns;
  def_nor_lblBPCRuns.Caption := IntToStr(usersettings.Default.NormalBPCRuns);
  def_nor_chkMaxRuns.Checked := usersettings.Default.NormalUseMaxRunsBPC;

  def_ship_tbCopy.Position := usersettings.Default.ShipBPCRuns;
  def_ship_lblBPCRuns.Caption := IntToStr(usersettings.Default.ShipBPCRuns);
  def_ship_chkMaxRuns.Checked := usersettings.Default.ShipUseMaxRunsBPC;

  def_rig_tbCopy.Position := usersettings.Default.RigBPCRuns;
  def_rig_lblBPCRuns.Caption := IntToStr(usersettings.Default.RigBPCRuns);
  def_rig_chkMaxRuns.Checked := usersettings.Default.RigUseMaxRunsBPC;
end;

function TfrmUserSettings.CheckDefaults: Boolean;
var
  d: Double;
begin
  result := true;
  def_nor_edtMetaItem.Color := clWindow;
  def_ship_edtMetaItem.Color := clWindow;
  def_rig_edtMetaItem.Color := clWindow;

  def_nor_edtMetaItem.Text := StringReplace(def_nor_edtMetaItem.Text, '.', global_ds, [rfReplaceAll]);
  def_ship_edtMetaItem.Text := StringReplace(def_ship_edtMetaItem.Text, '.', global_ds, [rfReplaceAll]);
  def_rig_edtMetaItem.Text := StringReplace(def_rig_edtMetaItem.Text, '.', global_ds, [rfReplaceAll]);

  if (not TryStrToFloat(def_nor_edtMetaItem.Text, d)) then begin
    result := false;
    def_nor_edtMetaItem.Color := clRed;
  end;
  if (not TryStrToFloat(def_ship_edtMetaItem.Text, d)) then begin
    result := false;
    def_ship_edtMetaItem.Color := clRed;
  end;
  if (not TryStrToFloat(def_rig_edtMetaItem.Text, d)) then begin
    result := false;
    def_rig_edtMetaItem.Color := clRed;
  end;
end;

procedure TfrmUserSettings.SaveDefaults;
begin
  if (def_nor_cbAmarr.Items.Objects[def_nor_cbAmarr.ItemIndex] = nil) then usersettings.Default.NormalAmarrDecryptorID := -1
  else usersettings.Default.NormalAmarrDecryptorID := TDecryptor(def_nor_cbAmarr.Items.Objects[def_nor_cbAmarr.ItemIndex]).ID;
  if (def_nor_cbCaldari.Items.Objects[def_nor_cbCaldari.ItemIndex] = nil) then usersettings.Default.NormalCaldariDecryptorID := -1
  else usersettings.Default.NormalCaldariDecryptorID := TDecryptor(def_nor_cbCaldari.Items.Objects[def_nor_cbCaldari.ItemIndex]).ID;
  if (def_nor_cbGallente.Items.Objects[def_nor_cbGallente.ItemIndex] = nil) then usersettings.Default.NormalGallenteDecryptorID := -1
  else usersettings.Default.NormalGallenteDecryptorID := TDecryptor(def_nor_cbGallente.Items.Objects[def_nor_cbGallente.ItemIndex]).ID;
  if (def_nor_cbMinmatar.Items.Objects[def_nor_cbMinmatar.ItemIndex] = nil) then usersettings.Default.NormalMinmatarDecryptorID := -1
  else usersettings.Default.NormalMinmatarDecryptorID := TDecryptor(def_nor_cbMinmatar.Items.Objects[def_nor_cbMinmatar.ItemIndex]).ID;
  usersettings.Default.NormalBPCRuns := StrToInt(def_nor_lblBPCRuns.Caption);
  usersettings.Default.NormalUseMaxRunsBPC := def_nor_chkMaxRuns.Checked;
  usersettings.Default.NormalMetaItemPrice := StrToInt(def_nor_edtMetaItem.Text);
  usersettings.Default.NormalMetaItemLevel := def_nor_cbMetaItem.ItemIndex;

  if (def_ship_cbAmarr.Items.Objects[def_ship_cbAmarr.ItemIndex] = nil) then usersettings.Default.ShipAmarrDecryptorID := -1
  else usersettings.Default.ShipAmarrDecryptorID := TDecryptor(def_ship_cbAmarr.Items.Objects[def_ship_cbAmarr.ItemIndex]).ID;
  if (def_ship_cbCaldari.Items.Objects[def_ship_cbCaldari.ItemIndex] = nil) then usersettings.Default.ShipCaldariDecryptorID := -1
  else usersettings.Default.ShipCaldariDecryptorID := TDecryptor(def_ship_cbCaldari.Items.Objects[def_ship_cbCaldari.ItemIndex]).ID;
  if (def_ship_cbGallente.Items.Objects[def_ship_cbGallente.ItemIndex] = nil) then usersettings.Default.ShipGallenteDecryptorID := -1
  else usersettings.Default.ShipGallenteDecryptorID := TDecryptor(def_ship_cbGallente.Items.Objects[def_ship_cbGallente.ItemIndex]).ID;
  if (def_ship_cbMinmatar.Items.Objects[def_ship_cbMinmatar.ItemIndex] = nil) then usersettings.Default.ShipMinmatarDecryptorID := -1
  else usersettings.Default.ShipMinmatarDecryptorID := TDecryptor(def_ship_cbMinmatar.Items.Objects[def_ship_cbMinmatar.ItemIndex]).ID;
  usersettings.Default.ShipBPCRuns := StrToInt(def_ship_lblBPCRuns.Caption);
  usersettings.Default.ShipUseMaxRunsBPC := def_ship_chkMaxRuns.Checked;
  usersettings.Default.ShipMetaItemPrice := StrToInt(def_ship_edtMetaItem.Text);
  usersettings.Default.ShipMetaItemLevel := def_ship_cbMetaItem.ItemIndex;

  if (def_rig_cbAmarr.Items.Objects[def_rig_cbAmarr.ItemIndex] = nil) then usersettings.Default.RigAmarrDecryptorID := -1
  else usersettings.Default.RigAmarrDecryptorID := TDecryptor(def_rig_cbAmarr.Items.Objects[def_rig_cbAmarr.ItemIndex]).ID;
  if (def_rig_cbCaldari.Items.Objects[def_rig_cbCaldari.ItemIndex] = nil) then usersettings.Default.RigCaldariDecryptorID := -1
  else usersettings.Default.RigCaldariDecryptorID := TDecryptor(def_rig_cbCaldari.Items.Objects[def_rig_cbCaldari.ItemIndex]).ID;
  if (def_rig_cbGallente.Items.Objects[def_rig_cbGallente.ItemIndex] = nil) then usersettings.Default.RigGallenteDecryptorID := -1
  else usersettings.Default.RigGallenteDecryptorID := TDecryptor(def_rig_cbGallente.Items.Objects[def_rig_cbGallente.ItemIndex]).ID;
  if (def_rig_cbMinmatar.Items.Objects[def_rig_cbMinmatar.ItemIndex] = nil) then usersettings.Default.RigMinmatarDecryptorID := -1
  else usersettings.Default.RigMinmatarDecryptorID := TDecryptor(def_rig_cbMinmatar.Items.Objects[def_rig_cbMinmatar.ItemIndex]).ID;
  usersettings.Default.RigBPCRuns := StrToInt(def_rig_lblBPCRuns.Caption);
  usersettings.Default.RigUseMaxRunsBPC := def_rig_chkMaxRuns.Checked;
  usersettings.Default.RigMetaItemPrice := StrToInt(def_rig_edtMetaItem.Text);
  usersettings.Default.RigMetaItemLevel := def_rig_cbMetaItem.ItemIndex;
end;

procedure TfrmUserSettings.def_nor_tbCopyChange(Sender: TObject);
begin
  def_nor_lblBPCRuns.Caption := IntToStr(def_nor_tbCopy.Position);
end;

procedure TfrmUserSettings.def_nor_btnCopyClick(Sender: TObject);
var
  s: String;
  i: Integer;
  askagain: Boolean;
begin
  s := def_nor_lblBPCRuns.Caption;
  askagain := true;
  while (askagain) do begin
    if (InputQuery('Invention Calculator - Defaults', 'How many runs should a normal BPC have by default?', s)) then begin
      if (TryStrToInt(s, i)) then begin
        def_nor_lblBPCRuns.Caption := IntToStr(i);
        askagain := false;
      end;
    end
    else askagain := false;
  end;
end;

procedure TfrmUserSettings.def_nor_chkMaxRunsClick(Sender: TObject);
begin
  def_nor_tbCopy.Enabled := (not def_nor_chkMaxRuns.Checked);
  def_nor_btnCopy.Enabled := (not def_nor_chkMaxRuns.Checked);
  def_nor_lblBPCRuns.Enabled := (not def_nor_chkMaxRuns.Checked);
end;

procedure TfrmUserSettings.def_ship_tbCopyChange(Sender: TObject);
begin
  def_ship_lblBPCRuns.Caption := IntToStr(def_ship_tbCopy.Position);
end;

procedure TfrmUserSettings.def_ship_btnCopyClick(Sender: TObject);
var
  s: String;
  i: Integer;
  askagain: Boolean;
begin
  s := def_ship_lblBPCRuns.Caption;
  askagain := true;
  while (askagain) do begin
    if (InputQuery('Invention Calculator - Defaults', 'How many runs should a ship BPC have by default?', s)) then begin
      if (TryStrToInt(s, i)) then begin
        def_ship_lblBPCRuns.Caption := IntToStr(i);
        askagain := false;
      end;
    end
    else askagain := false;
  end;
end;

procedure TfrmUserSettings.def_ship_chkMaxRunsClick(Sender: TObject);
begin
  def_ship_tbCopy.Enabled := (not def_ship_chkMaxRuns.Checked);
  def_ship_btnCopy.Enabled := (not def_ship_chkMaxRuns.Checked);
  def_ship_lblBPCRuns.Enabled := (not def_ship_chkMaxRuns.Checked);
end;

procedure TfrmUserSettings.def_rig_tbCopyChange(Sender: TObject);
begin
  def_rig_lblBPCRuns.Caption := IntToStr(def_rig_tbCopy.Position);
end;

procedure TfrmUserSettings.def_rig_btnCopyClick(Sender: TObject);
var
  s: String;
  i: Integer;
  askagain: Boolean;
begin
  s := def_rig_lblBPCRuns.Caption;
  askagain := true;
  while (askagain) do begin
    if (InputQuery('Invention Calculator - Defaults', 'How many runs should a rig BPC have by default?', s)) then begin
      if (TryStrToInt(s, i)) then begin
        def_rig_lblBPCRuns.Caption := IntToStr(i);
        askagain := false;
      end;
    end
    else askagain := false;
  end;
end;

procedure TfrmUserSettings.def_rig_chkMaxRunsClick(Sender: TObject);
begin
  def_rig_tbCopy.Enabled := (not def_rig_chkMaxRuns.Checked);
  def_rig_btnCopy.Enabled := (not def_rig_chkMaxRuns.Checked);
  def_rig_lblBPCRuns.Enabled := (not def_rig_chkMaxRuns.Checked);
end;

// ====================
// ====== Colors ======
// ====================

procedure TfrmUserSettings.Label1Click(Sender: TObject);
var
  run: TBrowseURL;
begin
  run := TBrowseURL.Create(self);
  run.URL := 'http://games.chruker.dk/eve_online/invention_chance.php';
  run.Execute;
  run.Free;
end;

procedure TfrmUserSettings.LoadColors;
var
  listitem: TListItem;
begin
  col_chkShowInfo.Checked := usersettings.Colors.ColorShowInfo;
  col_chkEditItems.Checked := usersettings.Colors.ColorEditItems;
  Col_chkShoppingList.Checked := usersettings.Colors.ColorShoppingList;

  col_lblexT1Item.Font.Color := usersettings.Colors.T1Item;
  col_lblexT2Item.Font.Color := usersettings.Colors.T2Item;
  col_lblexAdvancedMaterial.Font.Color := usersettings.Colors.AdvancedMaterial;
  col_lblexT2Component.Font.Color := usersettings.Colors.T2Component;
  col_lblexRAM.Font.Color := usersettings.Colors.RAM;
  col_lblexTradegoods.Font.Color := usersettings.Colors.Tradegoods;
  col_lblexDatacore.Font.Color := usersettings.Colors.Datacore;
  col_lblexMineral.Font.Color := usersettings.Colors.Mineral;
  col_lblexCapComponent.Font.Color := usersettings.Colors.CapComponent;
  col_lblexCapT2Component.Font.Color := usersettings.Colors.CapT2Component;
  col_lblexSalvage.Font.Color := usersettings.Colors.Salvage;
  col_lblexDataInterface.Font.Color := usersettings.Colors.DataInterface;
  listitem := col_lstExample.Items.Add; listitem.Caption := 'Prophecy'; listitem.Data := TObject(usersettings.Colors.T1Item);
  listitem := col_lstExample.Items.Add; listitem.Caption := 'Invulnerability Field II'; listitem.Data := TObject(usersettings.Colors.T2Item);
  listitem := col_lstExample.Items.Add; listitem.Caption := 'Nanotransistors'; listitem.Data := TObject(usersettings.Colors.AdvancedMaterial);
  listitem := col_lstExample.Items.Add; listitem.Caption := 'Antimatter Reactor Unit'; listitem.Data := TObject(usersettings.Colors.T2Component);
  listitem := col_lstExample.Items.Add; listitem.Caption := 'R.A.M.- Starship Tech'; listitem.Data := TObject(usersettings.Colors.RAM);
  listitem := col_lstExample.Items.Add; listitem.Caption := 'Robotics'; listitem.Data := TObject(usersettings.Colors.Tradegoods);
  listitem := col_lstExample.Items.Add; listitem.Caption := 'Datacore - Electronic Engineering'; listitem.Data := TObject(usersettings.Colors.Datacore);
  listitem := col_lstExample.Items.Add; listitem.Caption := 'Tritanium'; listitem.Data := TObject(usersettings.Colors.Mineral);
  listitem := col_lstExample.Items.Add; listitem.Caption := 'Capital Jump Drive'; listitem.Data := TObject(usersettings.Colors.CapComponent);
  listitem := col_lstExample.Items.Add; listitem.Caption := 'Captial Tungsten Carbide Armor Plate'; listitem.Data := TObject(usersettings.Colors.CapT2Component);
  listitem := col_lstExample.Items.Add; listitem.Caption := 'Fried Interface Circuit'; listitem.Data := TObject(usersettings.Colors.Salvage);
  listitem := col_lstExample.Items.Add; listitem.Caption := 'Incognito Tuner Data Interface'; listitem.Data := TObject(usersettings.Colors.DataInterface);
end;

function TfrmUserSettings.CheckColors: Boolean;
begin
  result := true;
end;

procedure TfrmUserSettings.SaveColors;
begin
  usersettings.Colors.ColorShowInfo := col_chkShowInfo.Checked;
  usersettings.Colors.ColorEditItems := col_chkEditItems.Checked;
  usersettings.Colors.ColorShoppingList := col_chkShoppingList.Checked;

  usersettings.Colors.T1Item := col_lblexT1Item.Font.Color;
  usersettings.Colors.T2Item := col_lblexT2Item.Font.Color;
  usersettings.Colors.AdvancedMaterial := col_lblexAdvancedMaterial.Font.Color;
  usersettings.Colors.T2Component := col_lblexT2Component.Font.Color;
  usersettings.Colors.RAM := col_lblexRAM.Font.Color;
  usersettings.Colors.Tradegoods := col_lblexTradegoods.Font.Color;
  usersettings.Colors.Datacore := col_lblexDatacore.Font.Color;
  usersettings.Colors.Mineral := col_lblexMineral.Font.Color;
  usersettings.Colors.CapComponent := col_lblexCapComponent.Font.Color;
  usersettings.Colors.CapT2Component := col_lblexCapT2Component.Font.Color;
  usersettings.Colors.Salvage := col_lblexSalvage.Font.Color;
  usersettings.Colors.DataInterface := col_lblexDataInterface.Font.Color;
end;

procedure TfrmUserSettings.LoadCustomColors(dlg: TColorDialog);
begin
  dlg.CustomColors.Add(usersettings.Colors.CustomColorA);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorB);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorC);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorD);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorE);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorF);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorG);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorH);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorI);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorJ);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorK);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorL);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorM);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorN);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorO);
  dlg.CustomColors.Add(usersettings.Colors.CustomColorP);
end;

procedure TfrmUserSettings.SaveCustomColors(dlg: TColorDialog);
begin
  usersettings.Colors.CustomColorA := dlg.CustomColors[0];
  usersettings.Colors.CustomColorB := dlg.CustomColors[1];
  usersettings.Colors.CustomColorC := dlg.CustomColors[2];
  usersettings.Colors.CustomColorD := dlg.CustomColors[3];
  usersettings.Colors.CustomColorE := dlg.CustomColors[4];
  usersettings.Colors.CustomColorF := dlg.CustomColors[5];
  usersettings.Colors.CustomColorG := dlg.CustomColors[6];
  usersettings.Colors.CustomColorH := dlg.CustomColors[7];
  usersettings.Colors.CustomColorI := dlg.CustomColors[8];
  usersettings.Colors.CustomColorJ := dlg.CustomColors[9];
  usersettings.Colors.CustomColorK := dlg.CustomColors[10];
  usersettings.Colors.CustomColorL := dlg.CustomColors[11];
  usersettings.Colors.CustomColorM := dlg.CustomColors[12];
  usersettings.Colors.CustomColorN := dlg.CustomColors[13];
  usersettings.Colors.CustomColorO := dlg.CustomColors[14];
  usersettings.Colors.CustomColorP := dlg.CustomColors[15];
end;

procedure TfrmUserSettings.col_lstExampleCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  col_lstExample.Canvas.Font.Color := TColor(Item.Data);
end;

procedure TfrmUserSettings.col_lblchgColorAllClick(Sender: TObject);
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(self);
  dlg.Color := clBlack;
  LoadCustomColors(dlg);
  if (dlg.Execute) then begin
    col_lblexT1Item.Font.Color := dlg.Color;
    col_lblexT2Item.Font.Color := dlg.Color;
    col_lblexAdvancedMaterial.Font.Color := dlg.Color;
    col_lblexT2Component.Font.Color := dlg.Color;
    col_lblexRAM.Font.Color := dlg.Color;
    col_lblexTradegoods.Font.Color := dlg.Color;
    col_lblexDatacore.Font.Color := dlg.Color;
    col_lblexMineral.Font.Color := dlg.Color;
    col_lblexCapComponent.Font.Color := dlg.Color;
    col_lblexCapT2Component.Font.Color := dlg.Color;
    col_lblexSalvage.Font.Color := dlg.Color;
    col_lblexDataInterface.Font.Color := dlg.Color;
    col_lstExample.Items[0].Data := TObject(dlg.Color);
    col_lstExample.Items[1].Data := TObject(dlg.Color);
    col_lstExample.Items[2].Data := TObject(dlg.Color);
    col_lstExample.Items[3].Data := TObject(dlg.Color);
    col_lstExample.Items[4].Data := TObject(dlg.Color);
    col_lstExample.Items[5].Data := TObject(dlg.Color);
    col_lstExample.Items[6].Data := TObject(dlg.Color);
    col_lstExample.Items[7].Data := TObject(dlg.Color);
    col_lstExample.Items[8].Data := TObject(dlg.Color);
    col_lstExample.Items[9].Data := TObject(dlg.Color);
    col_lstExample.Items[10].Data := TObject(dlg.Color);
    col_lstExample.Items[11].Data := TObject(dlg.Color);
    col_lstExample.Repaint;
    SaveCustomColors(dlg);
  end;
  dlg.Free;
end;

procedure TfrmUserSettings.col_lblchgT1ItemClick(Sender: TObject);
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(self);
  dlg.Color := col_lblexT1Item.Font.Color;
  LoadCustomColors(dlg);
  if (dlg.Execute) then begin
    col_lblexT1Item.Font.Color := dlg.Color;
    col_lstExample.Items[0].Data := TObject(dlg.Color);
    col_lstExample.Repaint;
    SaveCustomColors(dlg);
  end;
  dlg.Free;
end;

procedure TfrmUserSettings.col_lblchgT2ItemClick(Sender: TObject);
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(self);
  dlg.Color := col_lblexT2Item.Font.Color;
  LoadCustomColors(dlg);
  if (dlg.Execute) then begin
    col_lblexT2Item.Font.Color := dlg.Color;
    col_lstExample.Items[1].Data := TObject(dlg.Color);
    col_lstExample.Repaint;
    SaveCustomColors(dlg);
  end;
  dlg.Free;
end;

procedure TfrmUserSettings.col_lblchgAdvancedMaterialClick(
  Sender: TObject);
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(self);
  dlg.Color := col_lblexAdvancedMaterial.Font.Color;
  LoadCustomColors(dlg);
  if (dlg.Execute) then begin
    col_lblexAdvancedMaterial.Font.Color := dlg.Color;
    col_lstExample.Items[2].Data := TObject(dlg.Color);
    col_lstExample.Repaint;
    SaveCustomColors(dlg);
  end;
  dlg.Free;
end;

procedure TfrmUserSettings.col_lblchgT2ComponentClick(Sender: TObject);
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(self);
  dlg.Color := col_lblexT2Component.Font.Color;
  LoadCustomColors(dlg);
  if (dlg.Execute) then begin
    col_lblexT2Component.Font.Color := dlg.Color;
    col_lstExample.Items[3].Data := TObject(dlg.Color);
    col_lstExample.Repaint;
    SaveCustomColors(dlg);
  end;
  dlg.Free;
end;

procedure TfrmUserSettings.col_lblchgRAMClick(Sender: TObject);
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(self);
  dlg.Color := col_lblexRAM.Font.Color;
  LoadCustomColors(dlg);
  if (dlg.Execute) then begin
    col_lblexRAM.Font.Color := dlg.Color;
    col_lstExample.Items[4].Data := TObject(dlg.Color);
    col_lstExample.Repaint;
    SaveCustomColors(dlg);
  end;
  dlg.Free;
end;

procedure TfrmUserSettings.col_lblchgTradegoodsClick(Sender: TObject);
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(self);
  dlg.Color := col_lblexTradegoods.Font.Color;
  LoadCustomColors(dlg);
  if (dlg.Execute) then begin
    col_lblexTradegoods.Font.Color := dlg.Color;
    col_lstExample.Items[5].Data := TObject(dlg.Color);
    col_lstExample.Repaint;
    SaveCustomColors(dlg);
  end;
  dlg.Free;
end;

procedure TfrmUserSettings.col_lblchgDatacoreClick(Sender: TObject);
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(self);
  dlg.Color := col_lblexDatacore.Font.Color;
  LoadCustomColors(dlg);
  if (dlg.Execute) then begin
    col_lblexDatacore.Font.Color := dlg.Color;
    col_lstExample.Items[6].Data := TObject(dlg.Color);
    col_lstExample.Repaint;
    SaveCustomColors(dlg);
  end;
  dlg.Free;
end;

procedure TfrmUserSettings.col_lblchgMineralClick(Sender: TObject);
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(self);
  dlg.Color := col_lblexMineral.Font.Color;
  LoadCustomColors(dlg);
  if (dlg.Execute) then begin
    col_lblexMineral.Font.Color := dlg.Color;
    col_lstExample.Items[7].Data := TObject(dlg.Color);
    col_lstExample.Repaint;
    SaveCustomColors(dlg);
  end;
  dlg.Free;
end;

procedure TfrmUserSettings.col_lblchgCapComponentClick(Sender: TObject);
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(self);
  dlg.Color := col_lblexCapComponent.Font.Color;
  LoadCustomColors(dlg);
  if (dlg.Execute) then begin
    col_lblexCapComponent.Font.Color := dlg.Color;
    col_lstExample.Items[8].Data := TObject(dlg.Color);
    col_lstExample.Repaint;
    SaveCustomColors(dlg);
  end;
  dlg.Free;
end;

procedure TfrmUserSettings.col_lblchgCapT2ComponentClick(Sender: TObject);
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(self);
  dlg.Color := col_lblexCapT2Component.Font.Color;
  LoadCustomColors(dlg);
  if (dlg.Execute) then begin
    col_lblexCapT2Component.Font.Color := dlg.Color;
    col_lstExample.Items[9].Data := TObject(dlg.Color);
    col_lstExample.Repaint;
    SaveCustomColors(dlg);
  end;
  dlg.Free;
end;

procedure TfrmUserSettings.col_lblchgSalvageClick(Sender: TObject);
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(self);
  dlg.Color := col_lblexSalvage.Font.Color;
  LoadCustomColors(dlg);
  if (dlg.Execute) then begin
    col_lblexSalvage.Font.Color := dlg.Color;
    col_lstExample.Items[10].Data := TObject(dlg.Color);
    col_lstExample.Repaint;
    SaveCustomColors(dlg);
  end;
  dlg.Free;
end;

procedure TfrmUserSettings.col_lblchgDataInterfaceClick(Sender: TObject);
var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(self);
  dlg.Color := col_lblexDataInterface.Font.Color;
  LoadCustomColors(dlg);
  if (dlg.Execute) then begin
    col_lblexDataInterface.Font.Color := dlg.Color;
    col_lstExample.Items[11].Data := TObject(dlg.Color);
    col_lstExample.Repaint;
    SaveCustomColors(dlg);
  end;
  dlg.Free;
end;

// =====================
// ====== Success ======
// =====================

procedure TfrmUserSettings.LoadSuccess;
begin
  inv_chkFormula.Checked := not usersettings.Success.UseFormula;
  inv_chkFormulaClick(nil);
  inv_edtNormal.Text := FloatToStr(usersettings.Success.BaseNormalChance);
  inv_edtShip.Text := FloatToStr(usersettings.Success.BaseShipChance);
  inv_edtRig.Text := FloatToStr(usersettings.Success.BaseRigChance);
  inv_chkDecryptor.Checked := usersettings.Success.StillUseDecryptor;
  inv_edtFormula.Text := usersettings.Success.Formula;
end;

function TfrmUserSettings.CheckSuccess: Boolean;
var
  s: String;
  d: Double;
begin
  result := true;
  inv_edtNormal.Color := clWindow;
  inv_edtShip.Color := clWindow;
  inv_edtRig.Color := clWindow;
  inv_edtFormula.Color := clWindow;

  inv_edtNormal.Text := StringReplace(inv_edtNormal.Text, '.', global_ds, [rfReplaceAll]);
  inv_edtShip.Text := StringReplace(inv_edtShip.Text, '.', global_ds, [rfReplaceAll]);
  inv_edtRig.Text := StringReplace(inv_edtRig.Text, '.', global_ds, [rfReplaceAll]);

  s := usersettings.Success.SetFormula(inv_edtFormula.Text);
  if (s <> '') then begin
    MessageDlg(s, mtError, [mbOk], 0);
    inv_edtFormula.Color := clRed;
    result := false;
  end;
  if (not TryStrToFloat(inv_edtNormal.Text, d)) then begin
    result := false;
    inv_edtNormal.Color := clRed;
  end;
  if (not TryStrToFloat(inv_edtShip.Text, d)) then begin
    result := false;
    inv_edtShip.Color := clRed;
  end;
  if (not TryStrToFloat(inv_edtRig.Text, d)) then begin
    result := false;
    inv_edtRig.Color := clRed;
  end;
end;

procedure TfrmUserSettings.SaveSuccess;
begin
  usersettings.Success.UseFormula := not inv_chkFormula.Checked;
  usersettings.Success.BaseNormalChance := StrToFloat(inv_edtNormal.Text);
  usersettings.Success.BaseShipChance := StrToFloat(inv_edtShip.Text);
  usersettings.Success.BaseRigChance := StrToFloat(inv_edtRig.Text);
  usersettings.Success.StillUseDecryptor := inv_chkDecryptor.Checked;
//  usersettings.Success.Formula := inv_edtFormula.Text; // isn't needed, CheckSuccess already called SetFormula, which also saves the formula
end;

procedure TfrmUserSettings.inv_edtFormulaKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inv_lblPosition.Caption := IntToStr(inv_edtFormula.SelStart);
end;

procedure TfrmUserSettings.inv_edtFormulaClick(Sender: TObject);
begin
  inv_lblPosition.Caption := IntToStr(inv_edtFormula.SelStart);
end;

procedure TfrmUserSettings.inv_btnResetClick(Sender: TObject);
begin
  // %Base% * (1+(0.01*%EncrSkill%)) * (1+((%Skill1% + %Skill2%) * (0.1 / (5 - %Meta%)))) * %Decryptor%
  // Source: http://games.chruker.dk/eve_online/invention_chance.php
  inv_edtFormula.Text := '%Base% * (1+(0'+global_ds+'01*%EncrSkill%)) * (1+((%Skill1% + %Skill2%) * (0'+global_ds+'1 / (5 - %Meta%)))) * %Decryptor%';
//  inv_edtFormula.Text := '%Base% * (1+(0'+dc+'01*%EncrSkill%)) * (1+(0'+dc+'005*%Skill1%)) * (1+(0'+dc+'005*%Skill2%)) * %Decryptor% * (1+(0'+dc+'1 * %Meta%))';
end;

procedure TfrmUserSettings.inv_btnTestClick(Sender: TObject);
begin
  Showmessage(usersettings.Success.SetFormula(inv_edtFormula.Text, true));
end;

procedure TfrmUserSettings.inv_chkFormulaClick(Sender: TObject);
begin
  inv_lblNormal.Enabled := inv_chkFormula.Checked;
  inv_lblShip.Enabled := inv_chkFormula.Checked;
  inv_lblRig.Enabled := inv_chkFormula.Checked;
  inv_edtNormal.Enabled := inv_chkFormula.Checked;
  inv_edtShip.Enabled := inv_chkFormula.Checked;
  inv_edtRig.Enabled := inv_chkFormula.Checked;
  inv_chkDecryptor.Enabled := inv_chkFormula.Checked;
  inv_edtFormula.Enabled := (not inv_chkFormula.Checked);
  inv_btnTest.Enabled := (not inv_chkFormula.Checked);
  inv_btnReset.Enabled := (not inv_chkFormula.Checked);
end;

// ====================
// ====== Skills ======
// ====================

procedure TfrmUserSettings.ski_lblAPIsiteClick(Sender: TObject);
var
  run: TBrowseURL;
begin
  run := TBrowseURL.Create(self);
  run.URL := 'http://myeve.eve-online.com/api/';
  run.Execute;
  run.Free;
end;

procedure TfrmUserSettings.LoadSkills;
begin
  ski_udAmarrEncryption.Position := usersettings.Skills.AmarrEncryption;
  ski_udCaldariEncryption.Position := usersettings.Skills.CaldariEncryption;
  ski_udGallenteEncryption.Position := usersettings.Skills.GallenteEncryption;
  ski_udMinmatarEncryption.Position := usersettings.Skills.MinmatarEncryption;
  ski_udAmarrStarship.Position := usersettings.Skills.AmarrianStarship;
  ski_udCaldariStarship.Position := usersettings.Skills.CaldariStarship;
  ski_udElectromagnetic.Position := usersettings.Skills.Electromagnetic;
  ski_udElectronic.Position := usersettings.Skills.Electronic;
  ski_udGallenteStarship.Position := usersettings.Skills.GallenteanStarship;
  ski_udGraviton.Position := usersettings.Skills.Graviton;
  ski_udHighEnergy.Position := usersettings.Skills.HighEnergy;
  ski_udHydromagnetic.Position := usersettings.Skills.Hydromagnetic;
  ski_udLaser.Position := usersettings.Skills.Laser;
  ski_udMechanical.Position := usersettings.Skills.Mechanical;
  ski_udMinmatarStarship.Position := usersettings.Skills.MinmatarStarship;
  ski_udMolecular.Position := usersettings.Skills.Molecular;
  ski_udNanite.Position := usersettings.Skills.Nanite;
  ski_udNuclear.Position := usersettings.Skills.Nuclear;
  ski_udPlasma.Position := usersettings.Skills.Plasma;
  ski_udQuantum.Position := usersettings.Skills.Quantum;
  ski_udRocket.Position := usersettings.Skills.Rocket;
  ski_udScience.Position := usersettings.Skills.Science;
  ski_udIndustry.Position := usersettings.Skills.Industry;
  ski_udProdEff.Position := usersettings.Skills.ProductionEfficiency;

  ski_cbBeancounterF.ItemIndex := usersettings.Skills.BeancounterF;
  ski_cbBeancounterG.ItemIndex := usersettings.Skills.BeancounterG;
  ski_cbBeancounterK.ItemIndex := usersettings.Skills.BeancounterK;
  ski_cbBeancounterCI.ItemIndex := usersettings.Skills.BeancounterCI;
  ski_cbShaman.ItemIndex := usersettings.Skills.Shaman;
  ski_cbCleric.ItemIndex := usersettings.Skills.Cleric;
  ski_cbDraftsman.ItemIndex := usersettings.Skills.Draftsman;

  ski_edtUserID.Text := usersettings.Settings.SettingsAPIUsername;
  ski_edtAPIkey.Text := usersettings.Settings.SettingsAPIKey;
end;

function TfrmUserSettings.CheckSkills: Boolean;
begin
  result := true;
end;

procedure TfrmUserSettings.SaveSkills;
begin
  usersettings.Skills.AmarrEncryption := StrToInt(ski_edtAmarrEncryption.Text);
  usersettings.Skills.CaldariEncryption := StrToInt(ski_edtCaldariEncryption.Text);
  usersettings.Skills.GallenteEncryption := StrToInt(ski_edtGallenteEncryption.Text);
  usersettings.Skills.MinmatarEncryption := StrToInt(ski_edtMinmatarEncryption.Text);
  usersettings.Skills.AmarrianStarship := StrToInt(ski_edtAmarrStarship.Text);
  usersettings.Skills.CaldariStarship := StrToInt(ski_edtCaldariStarship.Text);
  usersettings.Skills.Electromagnetic := StrToInt(ski_edtElectromagnetic.Text);
  usersettings.Skills.Electronic := StrToInt(ski_edtElectronic.Text);
  usersettings.Skills.GallenteanStarship := StrToInt(ski_edtGallenteStarship.Text);
  usersettings.Skills.Graviton := StrToInt(ski_edtGraviton.Text);
  usersettings.Skills.HighEnergy := StrToInt(ski_edtHighEnergy.Text);
  usersettings.Skills.Hydromagnetic := StrToInt(ski_edtHydromagnetic.Text);
  usersettings.Skills.Laser := StrToInt(ski_edtLaser.Text);
  usersettings.Skills.Mechanical := StrToInt(ski_edtMechanical.Text);
  usersettings.Skills.MinmatarStarship := StrToInt(ski_edtMinmatarStarship.Text);
  usersettings.Skills.Molecular := StrToInt(ski_edtMolecular.Text);
  usersettings.Skills.Nanite := StrToInt(ski_edtNanite.Text);
  usersettings.Skills.Nuclear := StrToInt(ski_edtNuclear.Text);
  usersettings.Skills.Plasma := StrToInt(ski_edtPlasma.Text);
  usersettings.Skills.Quantum := StrToInt(ski_edtQuantum.Text);
  usersettings.Skills.Rocket := StrToInt(ski_edtRocket.Text);
  usersettings.Skills.Science := StrToInt(ski_edtScience.Text);
  usersettings.Skills.Industry := StrToInt(ski_edtIndustry.Text);
  usersettings.Skills.ProductionEfficiency := StrToInt(ski_edtProdEff.Text);

  usersettings.Skills.BeancounterF := ski_cbBeancounterF.ItemIndex;
  usersettings.Skills.BeancounterG := ski_cbBeancounterG.ItemIndex;
  usersettings.Skills.BeancounterK := ski_cbBeancounterK.ItemIndex;
  usersettings.Skills.BeancounterCI := ski_cbBeancounterCI.ItemIndex;
  usersettings.Skills.Shaman := ski_cbShaman.ItemIndex;
  usersettings.Skills.Cleric := ski_cbCleric.ItemIndex;
  usersettings.Skills.Draftsman := ski_cbDraftsman.ItemIndex;

  usersettings.Settings.SettingsAPIUsername := ski_edtUserID.Text;
  usersettings.Settings.SettingsAPIKey := ski_edtAPIkey.Text;
end;

procedure TfrmUserSettings.ski_btnAllVClick(Sender: TObject);
begin
  ski_udAmarrEncryption.Position := 5;
  ski_udCaldariEncryption.Position := 5;
  ski_udGallenteEncryption.Position := 5;
  ski_udMinmatarEncryption.Position := 5;
  ski_udAmarrStarship.Position := 5;
  ski_udCaldariStarship.Position := 5;
  ski_udElectromagnetic.Position := 5;
  ski_udElectronic.Position := 5;
  ski_udGallenteStarship.Position := 5;
  ski_udGraviton.Position := 5;
  ski_udHighEnergy.Position := 5;
  ski_udHydromagnetic.Position := 5;
  ski_udLaser.Position := 5;
  ski_udMechanical.Position := 5;
  ski_udMinmatarStarship.Position := 5;
  ski_udMolecular.Position := 5;
  ski_udNanite.Position := 5;
  ski_udNuclear.Position := 5;
  ski_udPlasma.Position := 5;
  ski_udQuantum.Position := 5;
  ski_udRocket.Position := 5;
  ski_udScience.Position := 5;
  ski_udIndustry.Position := 5;
  ski_udProdEff.Position := 5;
end;

procedure TfrmUserSettings.ski_btnRetrieveCharactersClick(Sender: TObject);
var
  parameters: TStringList;
  result, errormessage: String;
  Fout: TextFile;
  http: TIdHTTP;
  retrieve, receivedresult: Boolean;
  XMLDoc: TXMLDocument;

  rowset, row: IXMLNode;
  i: Integer;
begin
  if (not DirectoryExists('res\API')) then
    CreateDir('res\API');
  if ((ski_edtUserID.Text <> '') and (ski_edtAPIkey.Text <> '')) then begin
    if (not DirectoryExists('res\API\' + ski_edtUserID.Text)) then
      CreateDir('res\API\' + ski_edtUserID.Text);
    if (FileExists('res\API\' + ski_edtUserID.Text + '\Characters.xml')) then begin
      // if it exists, check if it is still cached
      XMLDoc := TXMLDocument.Create(self);
      XMLDoc.LoadFromFile('res\API\' + ski_edtUserID.Text + '\Characters.xml');
      retrieve := ShouldRetrieve(XMLDoc.DocumentElement.ChildNodes['cachedUntil'].Text);
      XMLDoc.Free;
    end
    else retrieve := true;
    receivedresult := true;
    if (retrieve) then begin
      // retrieve xml file from eve website
      parameters := TStringList.Create;
      parameters.Add('userID=' + ski_edtUserID.Text);
      parameters.Add('apiKey=' + ski_edtAPIkey.Text);
      http := TIdHTTP.Create;
      try
        result := http.Post('http://api.eve-online.com/account/Characters.xml.aspx', parameters);
      except
        on e: Exception do begin
          if (AnsiContainsText(e.Message, '404 Not Found')) then errormessage := e.Message + #10#13#10#13 + 'Most likely the API is down.'
          else errormessage := e.Message;
          MessageDlg(errormessage, mtError, [mbOk], 0);
          receivedresult := false;
        end;
      end;
      if (receivedresult) then begin
        // write xml file to HD
        AssignFile(Fout, 'res\API\' + ski_edtUserID.Text + '\Characters.xml');
        ReWrite(Fout);
        Write(Fout, result);
        CloseFile(Fout);
      end;
      parameters.Free;
      http.Free;
    end;
    if (receivedresult) then begin
      // parse xml file: fill ski_cbCharacters with all characters on the account
      XMLDoc := TXMLDocument.Create(self);
      XMLDoc.LoadFromFile('res\API\' + ski_edtUserID.Text + '\Characters.xml');
      if (XMLDoc.DocumentElement.ChildNodes['error'].IsTextElement) then begin
        MessageDlg(XMLDoc.DocumentElement.ChildNodes['error'].Text, mtError, [mbOk], 0);
      end
      else begin
        ski_cbCharacters.Items.Clear;
        rowset := XMLDoc.DocumentElement.ChildNodes['result'].ChildNodes['rowset'];
        for i := 0 to rowset.ChildNodes.Count -1 do begin
          row := rowset.ChildNodes[i];
          ski_cbCharacters.Items.AddObject(row.Attributes['name'], TObject(StrToInt(row.Attributes['characterID'])));
        end;
        if (rowset.ChildNodes.Count > 0) then ski_cbCharacters.ItemIndex := 0;
      end;
      XMLDoc.Free;
    end;
  end
  else
    MessageDlg('Enter your user ID and API key', mtError, [mbOk], 0);
end;

procedure TfrmUserSettings.ski_btnRetrieveSkillsClick(Sender: TObject);
var
  parameters: TStringList;
  result, errormessage: String;
  Fout: TextFile;
  http: TIdHTTP;
  retrieve, receivedresult: Boolean;
  XMLDoc: TXMLDocument;

  rowset, row: IXMLNode;
  i, skillid, skilllevel: Integer;
begin
  if ((ski_edtUserID.Text <> '') and (ski_edtAPIkey.Text <> '') and (ski_cbCharacters.ItemIndex >= 0)) then begin
    if (not DirectoryExists('res\API\' + ski_edtUserID.Text)) then
      CreateDir('res\API\' + ski_edtUserID.Text);
    if (FileExists('res\API\' + ski_edtUserID.Text + '\CharacterSheet.xml')) then begin
      // if it exists, check if it is still cached
      XMLDoc := TXMLDocument.Create(self);
      XMLDoc.LoadFromFile('res\API\' + ski_edtUserID.Text + '\CharacterSheet.xml');
      retrieve := ShouldRetrieve(XMLDoc.DocumentElement.ChildNodes['cachedUntil'].Text);
      XMLDoc.Free;
    end
    else retrieve := true;
    receivedresult := true;
    if (retrieve) then begin
      // retrieve xml file from eve website
      parameters := TStringList.Create;
      parameters.Add('userID=' + ski_edtUserID.Text);
      parameters.Add('apiKey=' + ski_edtAPIkey.Text);
      parameters.Add('characterID=' + IntToStr(Integer(ski_cbCharacters.Items.Objects[ski_cbCharacters.ItemIndex])));
      http := TIdHTTP.Create;
      try
        result := http.Post('http://api.eve-online.com/char/CharacterSheet.xml.aspx', parameters);
      except
        on e: Exception do begin
          if (AnsiContainsText(e.Message, '404 Not Found')) then errormessage := e.Message + #10#13#10#13 + 'Most likely the API is down.'
          else errormessage := e.Message;
          MessageDlg(errormessage, mtError, [mbOk], 0);
          receivedresult := false;
        end;
      end;
      if (receivedresult) then begin
        // write xml file to HD
        AssignFile(Fout, 'res\API\' + ski_edtUserID.Text + '\CharacterSheet.xml');
        ReWrite(Fout);
        Write(Fout, result);
        CloseFile(Fout);
      end;
      parameters.Free;
      http.Free;
    end;
    if (receivedresult) then begin
      // parse xml file: read all necessary skills and change editbox values
      XMLDoc := TXMLDocument.Create(self);
      XMLDoc.LoadFromFile('res\API\' + ski_edtUserID.Text + '\CharacterSheet.xml');
      if (XMLDoc.DocumentElement.ChildNodes['error'].IsTextElement) then begin
        MessageDlg(XMLDoc.DocumentElement.ChildNodes['error'].Text, mtError, [mbOk], 0);
      end
      else begin
        ski_edtAmarrEncryption.Text := '0';
        ski_edtCaldariEncryption.Text := '0';
        ski_edtGallenteEncryption.Text := '0';
        ski_edtMinmatarEncryption.Text := '0';
        ski_edtAmarrStarship.Text := '0';
        ski_edtCaldariStarship.Text := '0';
        ski_edtElectromagnetic.Text := '0';
        ski_edtElectronic.Text := '0';
        ski_edtGallenteStarship.Text := '0';
        ski_edtGraviton.Text := '0';
        ski_edtHighEnergy.Text := '0';
        ski_edtHydromagnetic.Text := '0';
        ski_edtLaser.Text := '0';
        ski_edtMechanical.Text := '0';
        ski_edtMinmatarStarship.Text := '0';
        ski_edtMolecular.Text := '0';
        ski_edtNanite.Text := '0';
        ski_edtNuclear.Text := '0';
        ski_edtPlasma.Text := '0';
        ski_edtQuantum.Text := '0';
        ski_edtRocket.Text := '0';
        ski_edtScience.Text := '0';
        ski_edtIndustry.Text := '0';
        ski_edtProdEff.Text := '0';

        rowset := XMLDoc.DocumentElement.ChildNodes['result'].ChildNodes['rowset'];
        for i := 0 to rowset.ChildNodes.Count -1 do begin
          row := rowset.ChildNodes[i];
          if (TryStrToInt(row.Attributes['typeID'], skillid) and TryStrToInt(row.Attributes['level'], skilllevel)) then begin
            case skillid of
              23087: ski_edtAmarrEncryption.Text := IntToStr(skilllevel);
              21790: ski_edtCaldariEncryption.Text := IntToStr(skilllevel);
              23121: ski_edtGallenteEncryption.Text := IntToStr(skilllevel);
              21791: ski_edtMinmatarEncryption.Text := IntToStr(skilllevel);
              11444: ski_edtAmarrStarship.Text := IntToStr(skilllevel);
              11454: ski_edtCaldariStarship.Text := IntToStr(skilllevel);
              11448: ski_edtElectromagnetic.Text := IntToStr(skilllevel);
              11453: ski_edtElectronic.Text := IntToStr(skilllevel);
              11450: ski_edtGallenteStarship.Text := IntToStr(skilllevel);
              11446: ski_edtGraviton.Text := IntToStr(skilllevel);
              11433: ski_edtHighEnergy.Text := IntToStr(skilllevel);
              11443: ski_edtHydromagnetic.Text := IntToStr(skilllevel);
              11447: ski_edtLaser.Text := IntToStr(skilllevel);
              11452: ski_edtMechanical.Text := IntToStr(skilllevel);
              11445: ski_edtMinmatarStarship.Text := IntToStr(skilllevel);
              11529: ski_edtMolecular.Text := IntToStr(skilllevel);
              11442: ski_edtNanite.Text := IntToStr(skilllevel);
              11451: ski_edtNuclear.Text := IntToStr(skilllevel);
              11441: ski_edtPlasma.Text := IntToStr(skilllevel);
              11455: ski_edtQuantum.Text := IntToStr(skilllevel);
              11449: ski_edtRocket.Text := IntToStr(skilllevel);
              3402: ski_edtScience.Text := IntToStr(skilllevel);
              3380: ski_edtIndustry.Text := IntToStr(skilllevel);
              3388: ski_edtProdEff.Text := IntToStr(skilllevel);
            end;
          end;
        end;
      end;
      XMLDoc.Free;
      if (retrieve) then MessageDlg('Skills succesfully retrieved from api.eve-online.com.', mtConfirmation, [mbOk], 0)
      else MessageDlg('Skills loaded succesfully from cache.', mtConfirmation, [mbOk], 0);
    end;
    // else there was some error in retrieving the skills
  end
  else
    MessageDlg('Enter you user ID and API key, and select a character.', mtError, [mbOk], 0);
end;

// ===========================
// ====== Installations ======
// ===========================

procedure TfrmUserSettings.LoadInstallations;
var
  i: Integer;
  item: TListItem;
  location: TLocation;
begin
  ins_edtCopyInstallationCost.text := FloatToStr(usersettings.Installations.CopyInstallation);
  ins_edtCopyHourlyCost.text := FloatToStr(usersettings.Installations.CopyHourly);
  ins_edtCopyTimeModifier.text := FloatToStr(usersettings.Installations.CopyTimeModifier);
  ins_edtCopyMaterialModifier.text := FloatToStr(usersettings.Installations.CopyMaterialModifier);
  ins_edtInventionInstallationCost.text := FloatToStr(usersettings.Installations.InventionInstallation);
  ins_edtInventionHourlyCost.text := FloatToStr(usersettings.Installations.InventionHourly);
  ins_edtInventionTimeModifier.text := FloatToStr(usersettings.Installations.InventionTimeModifier);
  ins_edtInventionMaterialModifier.text := FloatToStr(usersettings.Installations.InventionMaterialModifier);
  ins_edtManufactureInstallationCost.text := FloatToStr(usersettings.Installations.ManufactureInstallation);
  ins_edtManufactureHourlyCost.text := FloatToStr(usersettings.Installations.ManufactureHourly);
  ins_edtManufactureTimeModifier.text := FloatToStr(usersettings.Installations.ManufactureTimeModifier);
  ins_edtManufactureMaterialModifier.text := FloatToStr(usersettings.Installations.ManufactureMaterialModifier);

  for i := 0 to inventionCalc.Locations.Count -1 do begin
    location := inventionCalc.Locations.GetLocation(i);
    item := lstInstallations.Items.Add;
    item.Caption := location.Name;
    item.SubItems.Add(FloatToStrF(location.TimeModifier, ffNumber, 1, 2));
    item.SubItems.Add(FloatToStrF(location.MaterialModifier, ffNumber, 1, 2));
    item.SubItems.Add(EActivityToString(location.Activity));
    item.Data := location;
  end;
end;

function TfrmUserSettings.CheckInstallations: Boolean;
var
  d: Double;
begin
  result := true;
  ins_edtCopyInstallationCost.Color := clWindow;
  ins_edtCopyHourlyCost.Color := clWindow;
  ins_edtCopyTimeModifier.Color := clWindow;
  ins_edtCopyMaterialModifier.Color := clWindow;
  ins_edtInventionInstallationCost.Color := clWindow;
  ins_edtInventionHourlyCost.Color := clWindow;
  ins_edtInventionTimeModifier.Color := clWindow;
  ins_edtInventionMaterialModifier.Color := clWindow;
  ins_edtManufactureInstallationCost.Color := clWindow;
  ins_edtManufactureHourlyCost.Color := clWindow;
  ins_edtManufactureTimeModifier.Color := clWindow;
  ins_edtManufactureMaterialModifier.Color := clWindow;

  ins_edtCopyInstallationCost.Text := StringReplace(ins_edtCopyInstallationCost.Text, '.', global_ds, [rfReplaceAll]);
  ins_edtCopyHourlyCost.Text := StringReplace(ins_edtCopyHo