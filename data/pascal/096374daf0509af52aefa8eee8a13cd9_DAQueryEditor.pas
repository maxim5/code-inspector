
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright  1998-2005 Core Lab. All right reserved.
//  Query Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAQueryEditor;
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
  DBAccess, MemUtils,
  CRFrame, CREditor, CRTabEditor, DASQLFrame, DAParamsFrame, DAMacrosFrame, DASPCallFrame,
  DASQLEditor, DAUpdateSQLFrame, DASQLGeneratorFrame;

type
  TDAQueryEditorForm = class(TDASQLEditorForm)
    shEditSQL: TTabSheet;
    shGenerator: TTabSheet;
    btnDataEditor: TBitBtn;
    procedure btnDataEditorClick(Sender: TObject);
  protected
    FUpdateSQLFrame: TDAUpdateSQLFrame;
    FSQLGeneratorFrame: TDASQLGeneratorFrame;

    procedure DoInit; override;
    procedure DoActivate; override;
    procedure DoSave; override;

    function GetQuery: TCustomDADataSet;
    procedure SetQuery(Value: TCustomDADataSet);
    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;
    function GetFrameByInitProp: TCRFrame; override;

    property Query: TCustomDADataSet read GetQuery write SetQuery;

  public
    property UpdateSQLFrame: TDAUpdateSQLFrame read FUpdateSQLFrame;

  end;

implementation

uses 
  DADesignUtils, DADataEditor;

{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R DAQueryEditor.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

procedure TDAQueryEditorForm.DoInit;
begin
  try
    inherited;
  finally
    Assert(FUpdateSQLFrame <> nil);
    Assert(FSQLGeneratorFrame <> nil);
    FSPCallFrame.Mode := spQuery;
  end;
end;

procedure TDAQueryEditorForm.DoActivate;
begin
  inherited;
  if PageControl.ActivePage = FUpdateSQLFrame.Page then
    ActiveControl := FUpdateSQLFrame.ActiveControl;
end;

procedure TDAQueryEditorForm.DoSave;
var
  OldActive: boolean;
  OldDebug: boolean;
begin
  OldActive := TCustomDADataSet(FComponent).Active;
  OldDebug := TCustomDADataSet(FComponent).Debug;
  try
    // CR-M12021
    TCustomDADataSet(FLocalComponent).MasterSource := TCustomDADataSet(FComponent).MasterSource;
    TCustomDADataSet(FLocalComponent).MasterFields := TCustomDADataSet(FComponent).MasterFields;
    TCustomDADataSet(FLocalComponent).DetailFields := TCustomDADataSet(FComponent).DetailFields;

    inherited;
    TCustomDADataSet(FComponent).Debug := False;
    try
      TCustomDADataSet(FComponent).Active := OldActive;
    except
    end;
  finally
    TCustomDADataSet(FComponent).Debug := OldDebug;
  end;
end;

function TDAQueryEditorForm.GetQuery: TCustomDADataSet;
begin
  Result := FComponent as TCustomDADataSet;
end;

procedure TDAQueryEditorForm.SetQuery(Value: TCustomDADataSet);
begin
  FComponent := Value;
end;

function TDAQueryEditorForm.GetComponent: TComponent;
begin
  Result := Query;
end;

procedure TDAQueryEditorForm.SetComponent(Value: TComponent);
begin
  Query := Value as TCustomDADataSet;
end;

function TDAQueryEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'SQLDelete' then begin
    FUpdateSQLFrame.SetStatementType(stDelete);
    Result := FUpdateSQLFrame;
  end
  else
  if InitialProperty = 'SQLInsert' then begin
    FUpdateSQLFrame.SetStatementType(stInsert);
    Result := FUpdateSQLFrame;
  end
  else
  if InitialProperty = 'SQLRefresh' then begin
    FUpdateSQLFrame.SetStatementType(stRefresh);
    Result := FUpdateSQLFrame;
  end
  else
  if InitialProperty = 'SQLUpdate' then begin
    FUpdateSQLFrame.SetStatementType(stUpdate);  
    Result := FUpdateSQLFrame;
  end
  else
    Result := inherited GetFrameByInitProp;
end;

procedure TDAQueryEditorForm.btnDataEditorClick(Sender: TObject);
begin
  SaveControlData;
  CheckConnection(LocalComponent);
  with TDADataEditorForm.Create(nil, FDADesignUtilsClass) do
    try
      Component := Self.LocalComponent;
      ShowModal;
    finally
      Free;
    end;
end;

end.
