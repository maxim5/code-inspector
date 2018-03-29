
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright  1998-2005 Core Lab. All right reserved.
//  Table Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DATableEditor;
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
  DBAccess, MemUtils, CREditor;

type
  TDATableEditorForm = class(TCREditorForm)
    Label2: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    edFilter: TEdit;
    meSQL: TMemo;
    cbTableName: TComboBox;
    edOrderFields: TEdit;
    Panel1: TPanel;
    btnDataEditor: TBitBtn;
    procedure cbTableNameDropDown(Sender: TObject);
    procedure cbTableNameExit(Sender: TObject);
    procedure edOrderFieldsExit(Sender: TObject);
    procedure edFilterExit(Sender: TObject);
    procedure btnDataEditorClick(Sender: TObject);
    
  protected
    FListGot: boolean;
    FLocalTable, FTable: TCustomDADataSet;

    procedure DoInit; override;
    procedure DoFinish; override;
    procedure DoSave; override;

    procedure InitSQL;
    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;
    function GetLocalComponent: TComponent; override;

    procedure GetTableNames(Connection: TCustomDAConnection; Items: TStrings); virtual;
  public
    property Table: TCustomDADataSet read FTable write FTable;

  end;

implementation

uses
  DADesignUtils, DADataEditor;

{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R DATableEditor.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

procedure TDATableEditorForm.DoInit;
begin
  inherited;

  FLocalTable := TComponentClass(Table.ClassType).Create(nil) as TCustomDADataSet;
  FLocalTable.Assign(Table);
  TDBAccessUtils.SetDesignCreate(FLocalTable, True);

  InitSQL;

  FListGot := False;

{$IFDEF LINUX}
  cbTableName.Items.Text := ' '; // bug in TComboBox
  meSQL.Height := meSQL.Parent.ClientHeight - meSQL.Top - cbTableName.Top;
{$ENDIF}

  cbTableName.Text := FDADesignUtilsClass.GetTableName(FLocalTable);
  edFilter.Text := FLocalTable.FilterSQL;
  edOrderFields.Text := FDADesignUtilsClass.GetOrderFields(FLocalTable);
end;

procedure TDATableEditorForm.DoFinish;
begin
  FLocalTable.Free;
  FLocalTable := nil;
  inherited;
end;

procedure TDATableEditorForm.DoSave;
var
  OldActive: boolean;
  OldDebug: boolean;
begin
  OldActive := Table.Active;
  OldDebug := Table.Debug;

  try
    Table.Assign(FLocalTable);
    inherited;
    Table.Debug := False;
    try
      Table.Active := OldActive;
    except
    end;
  finally
    Table.Debug := OldDebug;
  end;
end;

procedure TDATableEditorForm.InitSQL;
begin
  meSQL.Lines.Text := '';
  try
    if FDADesignUtilsClass.GetTableName(FLocalTable) <> '' then begin
      FDADesignUtilsClass.PrepareSQL(FLocalTable);
      meSQL.Lines.Text := FLocalTable.FinalSQL;
    end;
  except
    on E: Exception do
      if not (E is EOutOfResources) then
        raise; // TMemo bug (see TMemoStrings.Insert)
  end;
end;

procedure TDATableEditorForm.GetTableNames(Connection: TCustomDAConnection;
  Items: TStrings);
begin
  Connection.GetTableNames(Items);
end;

procedure TDATableEditorForm.cbTableNameDropDown(Sender: TObject);
var
  UsedConnection: TCustomDAConnection;
begin
  UsedConnection := TDBAccessUtils.UsedConnection(Table);
  if TDBAccessUtils.UsedConnection(Table) = nil then
    Exit;

  try
    if not FListGot then begin
      GetTableNames(UsedConnection, cbTableName.Items);
      FListGot := True;
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TDATableEditorForm.cbTableNameExit(Sender: TObject);
var
  s: string;
begin
  try
    s := TDBAccessUtils.UnQuoteName(FLocalTable, Trim(cbTableName.Text));
    if s <> FDADesignUtilsClass.GetTableName(FLocalTable) then begin
      FDADesignUtilsClass.SetTableName(FLocalTable, cbTableName.Text);
      InitSQL;
      Modified:= True;
    end;
  except
    cbTableName.SetFocus;
    raise;
  end;
end;

procedure TDATableEditorForm.edOrderFieldsExit(Sender: TObject);
begin
  if edOrderFields.Text <> FDADesignUtilsClass.GetOrderFields(FLocalTable) then begin
    FDADesignUtilsClass.SetOrderFields(FLocalTable, edOrderFields.Text);
    InitSQL;
    Modified:= True;
  end;
end;

procedure TDATableEditorForm.edFilterExit(Sender: TObject);
begin
  if edFilter.Text <> FLocalTable.FilterSQL then begin
    FLocalTable.FilterSQL:= edFilter.Text;
    InitSQL;
    Modified:= True;
  end;
end;

function TDATableEditorForm.GetComponent: TComponent;
begin
  Result := Table;
end;

procedure TDATableEditorForm.SetComponent(Value: TComponent);
begin
  Table := Value as TCustomDADataSet;
end;

function TDATableEditorForm.GetLocalComponent: TComponent;
begin
  Result := FLocalTable;
end;

procedure TDATableEditorForm.btnDataEditorClick(Sender: TObject);
begin
  inherited;
  SaveControlData;
  DoSave;
  with TDADataEditorForm.Create(nil, FDADesignUtilsClass) do
    try
      Component := Self.FLocalTable;
      ShowModal;
    finally
      Free;
    end;
end;

end.
