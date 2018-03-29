
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright  1998-2005 Core Lab. All right reserved.
//  UpdateSQL Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAUpdateSQLEditor;
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
  SysUtils, Classes, DBAccess, CREditor, CRFrame, CRTabEditor, DAUpdateSQLFrame,
  DASQLGeneratorFrame;

type
  TDAUpdateSQLEditorForm = class(TCRTabEditorForm)
    shEditSQL: TTabSheet;
    shGenerator: TTabSheet;
  protected
    FUpdateSQLFrame: TDAUpdateSQLFrame;
    FSQLGeneratorFrame: TDASQLGeneratorFrame;
    FLocalComponent, FComponent: TComponent;
    FLocalDataSet: TCustomDADataSet;

    procedure DoInit; override;
    procedure DoActivate; override;
    procedure DoFinish; override;
    procedure DoSave; override;

    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;
    function GetLocalComponent: TComponent; override;

    function GetFrameByInitProp: TCRFrame; override;

    function GetUpdateSQL: TCustomDAUpdateSQL;
    procedure SetUpdateSQL(const Value: TCustomDAUpdateSQL);
  public
    property UpdateSQL: TCustomDAUpdateSQL read GetUpdateSQL write SetUpdateSQL;
    property UpdateSQLFrame: TDAUpdateSQLFrame read FUpdateSQLFrame;
    property LocalDataSet: TCustomDADataSet read FLocalDataSet;
  end;

implementation

{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R DAUpdateSQLEditor.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

{ TDAUpdateSQLEditorForm }

function TDAUpdateSQLEditorForm.GetComponent: TComponent;
begin
  Result := UpdateSQL;
end;

procedure TDAUpdateSQLEditorForm.SetComponent(Value: TComponent);
begin
  UpdateSQL := Value as TCustomDAUpdateSQL;
end;

function TDAUpdateSQLEditorForm.GetLocalComponent: TComponent;
begin
  Result := FLocalComponent;
end;

function TDAUpdateSQLEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'DeleteSQL' then begin
    FUpdateSQLFrame.SetStatementType(stDelete);
    Result := FUpdateSQLFrame;
  end
  else
  if InitialProperty = 'InsertSQL' then begin
    FUpdateSQLFrame.SetStatementType(stInsert);
    Result := FUpdateSQLFrame;
  end
  else
  if InitialProperty = 'RefreshSQL' then begin
    FUpdateSQLFrame.SetStatementType(stRefresh);
    Result := FUpdateSQLFrame;
  end
  else
  if InitialProperty = 'ModifySQL' then begin
    FUpdateSQLFrame.SetStatementType(stUpdate);  
    Result := FUpdateSQLFrame;
  end
  else
    Result := inherited GetFrameByInitProp;
end;

function TDAUpdateSQLEditorForm.GetUpdateSQL: TCustomDAUpdateSQL;
begin
  Result := FComponent as TCustomDAUpdateSQL;
end;

procedure TDAUpdateSQLEditorForm.SetUpdateSQL(const Value: TCustomDAUpdateSQL);
begin
  FComponent := Value;
end;

procedure TDAUpdateSQLEditorForm.DoInit;
begin
  inherited;

  FLocalComponent := TComponentClass(UpdateSQL.ClassType).Create(nil);
  FLocalComponent.Assign(UpdateSQL);
  FDADesignUtilsClass.SetDesignCreate(FLocalComponent, True);

  if UpdateSQL.DataSet <> nil then begin
    shGenerator.TabVisible := True;
    FLocalDataSet := TCustomDADataSet(TComponentClass(UpdateSQL.DataSet.ClassType).Create(nil));
    FLocalDataSet.Assign(UpdateSQL.DataSet);
    FDADesignUtilsClass.SetDesignCreate(FLocalDataSet, True);
    TDBAccessUtils.SetDesigning(FLocalDataSet, csDesigning in UpdateSQL.DataSet.ComponentState);
  end
  else
    shGenerator.TabVisible := False;

  Assert(FUpdateSQLFrame <> nil);
  Assert(FSQLGeneratorFrame <> nil);

  Modified := False;
end;

procedure TDAUpdateSQLEditorForm.DoActivate;
begin
  inherited;

  GetFrameByInitProp;
end;

procedure TDAUpdateSQLEditorForm.DoFinish;
begin
  FreeAndNil(FLocalDataSet);
  FreeAndNil(FLocalComponent);
  inherited;
end;

procedure TDAUpdateSQLEditorForm.DoSave;
begin
  inherited;
  FComponent.Assign(FLocalComponent);
end;

end.
