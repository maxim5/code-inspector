
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright  1998-2007 Core Lab. All right reserved.
//  DADump Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DADumpEditor;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons, DacVcl,
{$ENDIF}
{$IFDEF LINUX}
  Types, QGraphics, QControls, QForms, QDialogs, QStdCtrls,
  QExtCtrls, QComCtrls, QButtons, DacClx,
{$ENDIF}
{$IFDEF DBTOOLS}
  DBToolsClient,
{$ENDIF}
  SysUtils, Classes, CREditor, DBAccess, DADump, DADesignUtils, DADualListEditor;

type
  TDADumpEditorForm = class(TCREditorForm)
    ClientPanel: TPanel;
    LeftPanel: TPanel;
    meSQL: TMemo;
    gbBackupOptions: TGroupBox;
    cbGenerateHeader: TCheckBox;
    btBackup: TBitBtn;
    btRestore: TBitBtn;
    btImport: TBitBtn;
    btExport: TBitBtn;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    cbTableNames: TComboBox;
    lbTableNames: TLabel;
    procedure btBackupClick(Sender: TObject);
    procedure btRestoreClick(Sender: TObject);
    procedure meSQLExit(Sender: TObject);
    procedure btImportClick(Sender: TObject);
    procedure btExportClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure cbTableNamesChange(Sender: TObject);
    procedure cbTableNamesDropDown(Sender: TObject);
  protected
    FDump, FLocalDump: TDADump;

    procedure EditTableNames(Sender: TObject); virtual;
    procedure DoInit; override;
    procedure DoSave; override;
    procedure DoFinish; override;

    procedure GetButtons; virtual;
    procedure SetButtons; virtual;

    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;
    function GetLocalComponent: TComponent; override;
  public
    property Dump: TDADump read FDump write FDump;
  end;

implementation

uses
  DAConsts, DADumpProgress;

{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R DADumpEditor.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

{ TDADumpEditorForm }

procedure TDADumpEditorForm.DoInit;
{$IFDEF USE_SYNEDIT}
var
  WinControl: TWinControl;
{$ENDIF}
begin
  inherited;

{$IFDEF USE_SYNEDIT}
{$IFDEF DBTOOLS}
  if DADesignUtilsClass.DBToolsAvailable then
    DBTools.ReplaceMemo(meSQL, DADesignUtilsClass, Component)
  else begin
{$ENDIF}
    WinControl := meSQL;
    ReplaceMemo(WinControl, True);
    meSQL := TMemo(WinControl);
{$IFDEF DBTOOLS}
  end;
{$ENDIF}
{$ENDIF}

  FLocalDump := TComponentClass(Dump.ClassType).Create(nil) as TDADump;
  FLocalDump.Assign(Dump);

  SetMemoText(meSQL, FLocalDump.SQL.Text);

  cbGenerateHeader.Checked := FLocalDump.Options.GenerateHeader;
end;

procedure TDADumpEditorForm.DoSave;
begin
  GetButtons;
  Dump.Assign(FLocalDump);
end;

procedure TDADumpEditorForm.DoFinish;
begin
  FLocalDump.Free;
end;

procedure TDADumpEditorForm.GetButtons;
begin
  FLocalDump.Options.GenerateHeader := cbGenerateHeader.Checked;
end;

procedure TDADumpEditorForm.SetButtons;
begin
  btRestore.Enabled := Length(Trim(GetMemoText(meSQL))) > 0;
end;

function TDADumpEditorForm.GetComponent: TComponent;
begin
  Result := Dump;
end;

function TDADumpEditorForm.GetLocalComponent: TComponent;
begin
  Result := FLocalDump;
end;

procedure TDADumpEditorForm.SetComponent(Value: TComponent);
begin
  Dump := Value as TDADump;
end;

procedure TDADumpEditorForm.btBackupClick(Sender: TObject);
var
  OldChangeCursor: boolean;
begin
  GetButtons;

  OldChangeCursor := ChangeCursor;
  try
    StartWait;
    ChangeCursor := False;
    with TDADumpProgressForm.Create(nil) do
      try
        if Dump.Owner <> nil then
          Caption := Dump.Owner.Name + '.' + Dump.Name
        else
          Caption := Dump.Name;
        DADump := FLocalDump;
        Backup;
      finally
        Free;
      end;
  finally
    SetMemoText(meSQL, FLocalDump.SQL.Text);
    ChangeCursor := OldChangeCursor;
    StopWait;
  end;
  Modified := True;
  SetButtons;
end;

procedure TDADumpEditorForm.btRestoreClick(Sender: TObject);
begin
  if MessageDlg(SAreYouSureRestore, mtConfirmation, [mbYes,mbNo], 0) = mrNo then
    Exit;

  with TDADumpProgressForm.Create(nil) do
    try
      if Dump.Owner <> nil then
        Caption := Dump.Owner.Name + '.' + Dump.Name
      else
        Caption := Dump.Name;
      DADump := FLocalDump;
      Restore;
    finally
      Free;
    end;
end;

procedure TDADumpEditorForm.meSQLExit(Sender: TObject);
begin
  if TrimRight(FLocalDump.SQL.Text) = TrimRight(GetMemoText(meSQL)) then
    Exit;

  Modified := True;
  FLocalDump.SQL.Text := GetMemoText(meSQL);
  SetButtons;
end;

procedure TDADumpEditorForm.btImportClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    LoadFromFile(meSQL, OpenDialog.FileName);
end;

procedure TDADumpEditorForm.btExportClick(Sender: TObject);
begin
  if not SaveDialog.Execute then
    Exit;

  meSQLExit(nil);
  SaveToFile(meSQL, SaveDialog.FileName);
end;

procedure TDADumpEditorForm.SaveClick(Sender: TObject);
begin
  meSQLExit(nil);
  inherited;
end;

procedure TDADumpEditorForm.cbTableNamesChange(Sender: TObject);
begin
  FLocalDump.TableNames := cbTableNames.Text;
end;

procedure TDADumpEditorForm.cbTableNamesDropDown(Sender: TObject);
begin
  EditTableNames(Sender);
end;

procedure TDADumpEditorForm.EditTableNames(Sender: TObject);
begin
//
end;

end.
