
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright  1998-2007 Core Lab. All right reserved.
//  ConnectionEditor Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Dac.inc}

unit DAConnectionEditor;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, ExtCtrls, ComCtrls, Registry, DacVcl, Buttons,
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs, QStdCtrls,
  QMask, QExtCtrls, QComCtrls, DacClx, Qt, QButtons,
{$ENDIF}
{$IFDEF DBTOOLS}
  DBToolsClient,
{$ENDIF}
  CREditor, DBAccess, DADesignUtils;

type
  TDAConnectionEditorForm = class(TCREditorForm)
    PageControl: TPageControl;
    shConnect: TTabSheet;
    Panel: TPanel;
    lbUsername: TLabel;
    lbPassword: TLabel;
    lbServer: TLabel;
    edUsername: TEdit;
    edPassword: TMaskEdit;
    edServer: TComboBox;
    btConnect: TButton;
    btDisconnect: TButton;
    shInfo: TTabSheet;
    shAbout: TTabSheet;
    btClose: TButton;
    meInfo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    lbVersion: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbWeb: TLabel;
    lbMail: TLabel;
    lbIDE: TLabel;
    cbLoginPrompt: TCheckBox;
    shRed: TShape;
    shYellow: TShape;
    shGreen: TShape;
    imPeng: TImage;
    lbEdition: TLabel;
    procedure btDisconnectClick(Sender: TObject);
    procedure lbWebClick(Sender: TObject);
    procedure lbMailClick(Sender: TObject);
    procedure cbLoginPromptClick(Sender: TObject);
    procedure lbWebMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure shAboutMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbMailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure edUsernameChange(Sender: TObject);
    procedure edPasswordChange(Sender: TObject);
    procedure edServerChange(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure edServerDropDown(Sender: TObject); virtual;
    procedure PageControlChange(Sender: TObject);
    procedure edServerKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edServerExit(Sender: TObject);

  protected
    FConnection: TCustomDAConnection;
    FInDoInit: boolean;
  {$IFDEF MSWINDOWS}
    FRegistry: TRegistry;
  {$ENDIF}
  {$IFDEF DBTOOLS}
    FInExistingChange: boolean;

    function GetExistingConnectionComboBox: TComboBox; virtual;
    procedure ChooseExistingConnection;
    function GetConnectionCondition: string; virtual;
  {$ENDIF}

    procedure GetServerList(List: TStrings); virtual;
    procedure AddServerToList; virtual;

    procedure ShowState(Yellow: boolean = False); virtual;
    procedure ConnToControls; virtual;
    procedure ControlsToConn; virtual;
    procedure FillInfo; virtual;

    procedure DoInit; override;

    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;

  public
    constructor Create(Owner: TComponent; DADesignUtilsClass: TDADesignUtilsClass); override;
    destructor Destroy; override;

  end;

implementation

uses
  {$IFDEF MSWINDOWS}ShellAPI, HelpUtils,{$ENDIF}
  {$IFDEF VER6P}Variants, {$ENDIF}
  MemData,
  MemUtils;

{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R DAConnectionEditor.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

{$I DacVer.inc}

{$IFDEF LINUX}
type
  _TMaskEdit = class (TMaskEdit)
  end;
{$ENDIF}

constructor TDAConnectionEditorForm.Create(Owner: TComponent; DADesignUtilsClass: TDADesignUtilsClass);
begin
  inherited;

{$IFDEF MSWINDOWS}
  FRegistry := TRegistry.Create;
  if not FRegistry.OpenKey('\SOFTWARE\CoreLab\' + FDADesignUtilsClass.GetProjectName + '\Connect', True) then
    FreeAndNil(FRegistry);
{$ENDIF}
end;

destructor TDAConnectionEditorForm.Destroy;
begin
{$IFDEF MSWINDOWS}
  FreeAndNil(FRegistry);
{$ENDIF}
  inherited;
end;

procedure TDAConnectionEditorForm.DoInit;
var
  IDE: string;
begin
  FInDoInit := True;
  try
    inherited;

  {$IFDEF D3}
    IDE := 'Delphi 3';
  {$ENDIF}
  {$IFDEF D4}
    IDE := 'Delphi 4';
  {$ENDIF}
  {$IFDEF D5}
    IDE := 'Delphi 5';
  {$ENDIF}
  {$IFDEF D6}
    IDE := 'Delphi 6';
  {$ENDIF}
  {$IFDEF D7}
    IDE := 'Delphi 7';
  {$ENDIF}
  {$IFDEF D8}
    IDE := 'Delphi 8';
  {$ENDIF}
  {$IFDEF D9}
    IDE := 'Delphi 2005';
  {$ENDIF}
  {$IFDEF D10}
    IDE := 'Delphi 2006';
  {$ENDIF}
  {$IFDEF D11}
    IDE := 'Delphi 2007';
  {$ENDIF}
  {$IFDEF CB3}
    IDE := 'C++Builder 3';
  {$ENDIF}
  {$IFDEF CB4}
    IDE := 'C++Builder 4';
  {$ENDIF}
  {$IFDEF CB5}
    IDE := 'C++Builder 5';
  {$ENDIF}
  {$IFDEF CB6}
    IDE := 'C++Builder 6';
  {$ENDIF}
  {$IFDEF LINUX}
    IDE := 'Kylix';
  {$ENDIF}
    lbVersion.Caption := DACVersion + ' ';
    lbIDE.Caption := 'for ' + IDE;
    lbIDE.Left := lbVersion.Left + lbVersion.Width;


  {$IFDEF LINUX}
    imPeng.Visible := True;
    imPeng.BringToFront;
    _TMaskEdit(edPassword).EchoMode := emPassword;
  {$ENDIF}

  {$IFDEF DBTOOLS}
    if DADesignUtilsClass.DBToolsAvailable then begin
      GetDBToolsService(DADesignUtilsClass).GetConnections(GetExistingConnectionComboBox.Items, GetConnectionCondition);
      ChooseExistingConnection;
    end;  
  {$ENDIF}
  
    ConnToControls;

    ShowState;
  finally
    FInDoInit := False;
  end;
end;

function TDAConnectionEditorForm.GetComponent: TComponent;
begin
  Result := FConnection;
end;

procedure TDAConnectionEditorForm.SetComponent(Value: TComponent);
begin
  FConnection := Value as TCustomDAConnection;
end;

{$IFDEF DBTOOLS}
function TDAConnectionEditorForm.GetExistingConnectionComboBox: TComboBox;
begin
  Result := nil;
  Assert(False, 'Must be overriden');
end;

procedure TDAConnectionEditorForm.ChooseExistingConnection;
begin
  if not FInExistingChange and DADesignUtilsClass.DBToolsAvailable then
    with GetExistingConnectionComboBox do
      ItemIndex := Items.IndexOf(GetDBToolsService(DADesignUtilsClass).FindConnectionName(FConnection));
end;

function TDAConnectionEditorForm.GetConnectionCondition: string;
begin
  Result := '';
end;
{$ENDIF}

procedure TDAConnectionEditorForm.ConnToControls;
begin
  edUsername.Text := FConnection.Username;
  edPassword.Text := FConnection.Password;
  edServer.Text := FConnection.Server;
  cbLoginPrompt.Checked := FConnection.LoginPrompt;
end;

procedure TDAConnectionEditorForm.ControlsToConn;
begin
  // all parameters are set in controls OnChange event handlers
end;

procedure TDAConnectionEditorForm.ShowState(Yellow: boolean);
begin
  btDisconnect.Enabled := FConnection.Connected;

  shRed.Brush.Color := clBtnFace;
  shYellow.Brush.Color := clBtnFace;
  shGreen.Brush.Color := clBtnFace;

  if Yellow then begin
    shYellow.Brush.Color := clYellow;
    shYellow.Update;
  end
  else
    if FConnection.Connected then begin
      shGreen.Brush.Color := clGreen;
      shYellow.Update;
    end
    else
      shRed.Brush.Color := clRed;
end;

procedure TDAConnectionEditorForm.lbWebClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  OpenUrl('http://' + lbWeb.Caption);
  lbWeb.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TDAConnectionEditorForm.lbMailClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  MailTo(lbMail.Caption);
  lbMail.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TDAConnectionEditorForm.cbLoginPromptClick(Sender: TObject);
begin
  FConnection.LoginPrompt := cbLoginPrompt.Checked;
end;

procedure TDAConnectionEditorForm.lbWebMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbWeb.Font.Color := $4080FF;
end;

procedure TDAConnectionEditorForm.lbMailMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbMail.Font.Color := $4080FF;
end;

procedure TDAConnectionEditorForm.shAboutMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbWeb.Font.Color := $FF0000;
  lbMail.Font.Color := $FF0000;
end;

procedure TDAConnectionEditorForm.edUsernameChange(Sender: TObject);
begin
  if FInDoInit then
    Exit;

  try
    FConnection.Username := edUsername.Text;
  finally
    ShowState;
  end;
end;

procedure TDAConnectionEditorForm.edPasswordChange(Sender: TObject);
begin
  if FInDoInit then
    Exit;

  try
    FConnection.Password := edPassword.Text;
  finally
    ShowState;
  end;
end;

procedure TDAConnectionEditorForm.edServerChange(Sender: TObject);
begin
  if FInDoInit then
    Exit;

  try
    FConnection.Server := edServer.Text;
  finally
    ShowState;
  end;
end;

procedure TDAConnectionEditorForm.btConnectClick(Sender: TObject);
begin
  ShowState(True);
{$IFDEF DBTOOLS}
  ChooseExistingConnection;
{$ENDIF}
  StartWait;
  try
    ControlsToConn;
    FConnection.PerformConnect;
    if FConnection.Connected then
      AddServerToList;
  finally
    StopWait;
    ShowState;
  end;

  ModalResult := mrOk;
end;

procedure TDAConnectionEditorForm.btDisconnectClick(Sender: TObject);
begin
{$IFDEF DBTOOLS}
  ChooseExistingConnection;
{$ENDIF}
  try
    FConnection.Disconnect;
  finally
    ShowState;
  end;
end;

procedure TDAConnectionEditorForm.edServerDropDown(Sender: TObject);
begin
  StartWait;
  try
    GetServerList(edServer.Items);
  finally
    StopWait;
  end;
end;

procedure TDAConnectionEditorForm.GetServerList(List: TStrings);
{$IFDEF MSWINDOWS}
var
  i: integer;
  s: string;
  ValueNames: TStringList;
{$ENDIF}
begin
  List.Clear;
{$IFDEF MSWINDOWS}
  if FRegistry <> nil then begin
    ValueNames := TStringList.Create;
    try
      FRegistry.GetValueNames(ValueNames);
      ValueNames.Sort;
      for i := 0 to ValueNames.Count - 1 do begin
        s := Trim(FRegistry.ReadString(ValueNames[i]));
        if (s <> '') and (List.IndexOf(s) = -1) then
          List.Add(s);
      end;
    finally
      ValueNames.Free;
    end;
  end;
{$ENDIF}
end;

procedure TDAConnectionEditorForm.AddServerToList;
{$IFDEF MSWINDOWS}
var
  i: integer;
  s: string;
  ValueNames, Values: TStringList;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if FRegistry <> nil then begin
    ValueNames := nil;
    Values := nil;
    try
      ValueNames := TStringList.Create;
      Values := TStringList.Create;

      Values.Add(FConnection.Server); // Add current server at first position

      FRegistry.GetValueNames(ValueNames);
      ValueNames.Sort;

      for i := 0 to ValueNames.Count - 1 do
        if FRegistry.GetDataType(ValueNames[i]) = rdString then begin
          s := Trim(FRegistry.ReadString(ValueNames[i]));
          if (s <> '') and (Values.IndexOf(s) = -1) then
            Values.Add(s);
          FRegistry.DeleteValue(ValueNames[i]); // Clear old list
        end;

      // Store updated list in registry
      for i := 0 to Values.Count - 1 do begin
        s := Format('Server %d', [i]);
        FRegistry.WriteString(s, Values[i]);
      end;

    finally
      ValueNames.Free;
      Values.Free;
    end;
  end;
{$ENDIF}
end;

procedure TDAConnectionEditorForm.FillInfo;
var
  OldLoginPrompt: boolean;

begin
  OldLoginPrompt := FConnection.LoginPrompt;
  try
    FConnection.LoginPrompt := False;

    if not FConnection.Connected then
      try
        ShowState(True);
        FConnection.Connect;
      except
        on E: Exception do
        begin
          // PageControl.ActivePage := shConnect;
          // Application.ShowException(E); - silent exception. Please see CR MyDAC 3443
        end;
      end;
    meInfo.Lines.Clear;
  finally
    FConnection.LoginPrompt := OldLoginPrompt;
    ShowState(False);
  end;
end;

procedure TDAConnectionEditorForm.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = shInfo then
    FillInfo;
end;

procedure TDAConnectionEditorForm.edServerKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key ={$IFDEF MSWINDOWS}VK_RETURN{$ELSE}KEY_RETURN{$ENDIF} then
    edServerChange(Sender);
end;

procedure TDAConnectionEditorForm.edServerExit(Sender: TObject);
begin
{$IFDEF DBTOOLS}
  ChooseExistingConnection;
{$ENDIF}
end;

end.
