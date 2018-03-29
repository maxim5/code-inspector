
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright  1998-2005 Core Lab. All right reserved.
//  SQL Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DASQLEditor;
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
{$IFDEF DBTOOLS}
  DBToolsClient,
{$ENDIF}
  SysUtils, DB, Classes,
  DBAccess, MemUtils,
  CREditor, CRTabEditor, CRFrame, DASQLFrame, DAParamsFrame, DAMacrosFrame, DASPCallFrame;

type
  TDASQLEditorForm = class(TCRTabEditorForm)
    shSQL: TTabSheet;
    shParameters: TTabSheet;
    shMacros: TTabSheet;
    shGeneratorSPC: TTabSheet;
    btExecute: TButton;
    procedure btExecuteClick(Sender: TObject);
  protected
    FSQLFrame: TDASQLFrame;
    FParamsFrame: TDAParamsFrame;
    FMacrosFrame: TDAMacrosFrame;
    FSPCallFrame: TDASPCallFrame;

    FLocalComponent, FComponent: TComponent;
    FOldAfterExecute: TAfterExecuteEvent;

    procedure AfterExecute(Sender: TObject; Result: boolean);

    procedure DoInit; override;
    procedure DoActivate; override;
    procedure DoFinish; override;
    procedure DoSave; override;

    procedure DoError(E: Exception); virtual;

    function GetSQL: TCustomDASQL;
    procedure SetSQL(Value: TCustomDASQL);
    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;
    function GetLocalComponent: TComponent; override;

    function GetFrameByInitProp: TCRFrame; override;

    property SQL: TCustomDASQL read GetSQL write SetSQL;

  public
    property SQLFrame: TDASQLFrame read FSQLFrame;
    property ParamsFrame: TDAParamsFrame read FParamsFrame;
    property MacrosFrame: TDAMacrosFrame read FMacrosFrame;

  end;

implementation

uses
  DADesignUtils;

{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R DASQLEditor.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

procedure TDASQLEditorForm.DoInit;
begin
  inherited;

  try
    FLocalComponent := TComponentClass(FComponent.ClassType).Create(nil);
    TDBAccessUtils.SetDesigning(FLocalComponent, csDesigning in FComponent.ComponentState);
    FLocalComponent.Assign(FComponent);
    FDADesignUtilsClass.SetDesignCreate(FLocalComponent, True);
  finally

    Assert(FSQLFrame <> nil);
    Assert(FParamsFrame <> nil);
    Assert(FMacrosFrame <> nil);
    Assert(FSPCallFrame <> nil);
    FSPCallFrame.Mode := spSQL;

    Modified := False;
  end;
end;

procedure TDASQLEditorForm.DoActivate;
var
  Frame: TCRFrame;
begin
  inherited;

  Frame := GetFrameByInitProp;
  if Frame <> nil then begin
    PageControl.ActivePage := Frame.Page;
    //Frame.SetFocus; // on Kylix control can be invisible in the form OnShow event    
    Frame.Activate;
  end;

  if PageControl.ActivePage = FSQLFrame.Page then
    ActiveControl := FSQLFrame.ActiveControl;
end;

procedure TDASQLEditorForm.DoFinish;
begin
  FLocalComponent.Free;
  FLocalComponent := nil;
  inherited;
end;

procedure TDASQLEditorForm.AfterExecute(Sender:TObject; Result:boolean);
begin
  btExecute.Enabled := True;
  FDADesignUtilsClass.SetAfterExecute(FLocalComponent, FOldAfterExecute);
end;

procedure TDASQLEditorForm.DoSave;
begin
  inherited;
  FComponent.Assign(FLocalComponent);
end;

procedure TDASQLEditorForm.DoError(E: Exception);
begin
  PageControl.ActivePage := FSQLFrame.Page;
{$IFDEF DBTOOLS}
  if DBTools.HasDACSqlEditorFrame(FSQLFrame.meSQL) then
    ActiveControl := DBTools.GetDACSqlEditorFrame(FSQLFrame.meSQL)
  else
{$ENDIF}
    ActiveControl := FSQLFrame.meSQL;
end;

procedure TDASQLEditorForm.btExecuteClick(Sender: TObject);
begin
  SaveControlData;
  CheckConnection(FLocalComponent);
  FOldAfterExecute := FDADesignUtilsClass.GetAfterExecute(FLocalComponent);
  FDADesignUtilsClass.SetAfterExecute(FLocalComponent, AfterExecute);
  btExecute.Enabled := False;
  try
    FDADesignUtilsClass.Execute(FLocalComponent);
    FParamsFrame.SelectItem;
  except
    on E: Exception do begin
      DoError(E);
      AfterExecute(FLocalComponent, False);
      raise;
    end;
  end;
end;

function TDASQLEditorForm.GetSQL: TCustomDASQL;
begin
  Result := FComponent as TCustomDASQL;
end;

procedure TDASQLEditorForm.SetSQL(Value: TCustomDASQL);
begin
  FComponent := Value;
end;

function TDASQLEditorForm.GetComponent: TComponent;
begin
  Result := SQL;
end;

procedure TDASQLEditorForm.SetComponent(Value: TComponent);
begin
  SQL := Value as TCustomDASQL;
end;

function TDASQLEditorForm.GetLocalComponent: TComponent;
begin
  Result := FLocalComponent;
end;

function TDASQLEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'SQL' then
    Result := FSQLFrame
  else
  if InitialProperty = 'Params' then
    Result := FParamsFrame
  else
  if InitialProperty = 'Macros' then
    Result := FMacrosFrame
  else
    Result := inherited GetFrameByInitProp;
end;

end.
