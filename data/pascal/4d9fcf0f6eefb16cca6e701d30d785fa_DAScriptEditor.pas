
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright  1998-2007 Core Lab. All right reserved.
//  Script Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAScriptEditor;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons,
{$ENDIF}
{$IFDEF LINUX}
  Types, QGraphics, QControls, QForms, QDialogs, QStdCtrls,
  QExtCtrls, QComCtrls, QButtons,
{$ENDIF}
  SysUtils, DB, Classes,
  DBAccess, MemUtils, DAScript,
  CREditor, CRTabEditor, CRFrame, DASQLFrame, DAMacrosFrame;

type
  TDAScriptEditorForm = class(TCRTabEditorForm)
    shSQL: TTabSheet;
    shMacros: TTabSheet;
    btExecute: TButton;
    cbDebug: TCheckBox;
    btOpen: TButton;
    btSave: TButton;
    procedure btExecuteClick(Sender: TObject);
    procedure cbDebugClick(Sender: TObject);
    procedure btOpenClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
  protected
    FSQLFrame: TDASQLFrame;
    FMacrosFrame: TDAMacrosFrame;

    FComponent: TComponent;
    FLocalScript: TDAScript;
    FOldAfterExecute: TAfterStatementExecuteEvent;
    FOldDebug: boolean;

    procedure AfterExecute(Sender: TObject; SQL: string);
    procedure OnError(Sender: TObject; E: Exception; SQL: string; var Action: TErrorAction);

    procedure DoInit; override;
    procedure DoActivate; override;
    procedure DoFinish; override;
    procedure DoSave; override;

    function GetScript: TDAScript;
    procedure SetScript(Value: TDAScript);
    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;
    function GetLocalComponent: TComponent; override;

    function GetFrameByInitProp: TCRFrame; override;

  public
    property Script: TDAScript read GetScript write SetScript;

    property SQLFrame: TDASQLFrame read FSQLFrame;
    property MacrosFrame: TDAMacrosFrame read FMacrosFrame;

  end;

implementation

uses
  DADesignUtils;

{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R DAScriptEditor.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

const
  SFileFilter = 'All SQL files|*.sql; *.qry|SQL files (*.sql)|*.sql|Query files (*.qry)|*.qry|All files (*.*)|*.*';

procedure TDAScriptEditorForm.DoInit;
begin
  FSQLFrame := AddTab(TDASQLFrame, shSQL) as TDASQLFrame;
  //FParamsFrame := AddTab(TMyParamsFrame, shParameters) as TDAParamsFrame;
  FMacrosFrame := AddTab(TDAMacrosFrame, shMacros) as TDAMacrosFrame;

  inherited;

  FLocalScript := TComponentClass(FComponent.ClassType).Create(nil) as TDAScript;
  FLocalScript.Assign(FComponent);
  FDADesignUtilsClass.SetDesignCreate(FLocalScript, True);
  FLocalScript.OnError := OnError;

  FOldDebug := FLocalScript.Debug;
  cbDebug.Checked := FOldDebug;

  Assert(FSQLFrame <> nil);
  Assert(FMacrosFrame <> nil);

  Modified := False;
end;

procedure TDAScriptEditorForm.DoActivate;
var
  Frame: TCRFrame;
begin
  inherited;

  Frame := GetFrameByInitProp;
  if Frame <> nil then begin
    PageControl.ActivePage := Frame.Page;
    Frame.SetFocus;
    Frame.Activate;
  end;

  if PageControl.ActivePage = FSQLFrame.Page then
    ActiveControl := FSQLFrame.ActiveControl;
end;

procedure TDAScriptEditorForm.DoFinish;
begin
  FLocalScript.Free;
  FLocalScript := nil;
  inherited;
end;

procedure TDAScriptEditorForm.AfterExecute(Sender: TObject; SQL: string);
begin
  btExecute.Enabled := True;
  //FParamsFrame.InitParams;

  FLocalScript.AfterExecute := FOldAfterExecute;
end;

procedure TDAScriptEditorForm.OnError(Sender: TObject; E: Exception; SQL: string; var Action: TErrorAction);
begin
  if MessageDlg(E.Message, mtError, [mbAbort, mbIgnore], 0) <> mrIgnore then
    Action := eaAbort
  else
    Action := eaContinue;
end;

procedure TDAScriptEditorForm.DoSave;
begin
  inherited;
  FLocalScript.Debug := FOldDebug;
  FComponent.Assign(FLocalScript);
end;

procedure TDAScriptEditorForm.btExecuteClick(Sender: TObject);
begin
  SaveControlData;
  CheckConnection(FLocalScript);
  FOldAfterExecute := FLocalScript.AfterExecute;
  FLocalScript.AfterExecute := AfterExecute;
  FLocalScript.Execute;
end;

function TDAScriptEditorForm.GetScript: TDAScript;
begin
  Result := FComponent as TDAScript;
end;

procedure TDAScriptEditorForm.SetScript(Value: TDAScript);
begin
  FComponent := Value;
end;

function TDAScriptEditorForm.GetComponent: TComponent;
begin
  Result := Script;
end;

procedure TDAScriptEditorForm.SetComponent(Value: TComponent);
begin
  Script := Value as TDAScript;
end;

function TDAScriptEditorForm.GetLocalComponent: TComponent;
begin
  Result := FLocalScript;
end;

function TDAScriptEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'SQL' then
    Result := FSQLFrame
  else
{  if InitialProperty = 'Params' then
    Result := FParamsFrame
  else}
  if InitialProperty = 'Macros' then
    Result := FMacrosFrame
  else
    Result := inherited GetFrameByInitProp;
end;

procedure TDAScriptEditorForm.cbDebugClick(Sender: TObject);
begin
  FLocalScript.Debug := cbDebug.Checked;
end;

procedure TDAScriptEditorForm.btOpenClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
    try
      Filter := SFileFilter;
      if Execute then begin
        TDAScript(LocalComponent).SQL.LoadFromFile(FileName);
        if ActiveFrame = SQLFrame then
          SQLFrame.ReActivate
        else
          ActivateFrame(SQLFrame);
        Modified := True;
      end;
    finally
      Free;
    end;
end;

procedure TDAScriptEditorForm.btSaveClick(Sender: TObject);
begin
  SaveControlData;
  with TSaveDialog.Create(nil) do
    try
      Options := Options + [ofOverwritePrompt];
      DefaultExt := 'sql';
      Filter := SFileFilter;
      if Execute then
        TDAScript(LocalComponent).SQL.SaveToFile(FileName);
    finally
      Free;
    end;
end;

end.
