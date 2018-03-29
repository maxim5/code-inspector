
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright  1998-2005 Core Lab. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Odac.inc}

unit OraSPScriptEditor;
{$ENDIF}

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls,
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs, QStdCtrls,
  QExtCtrls, QComCtrls, QButtons,
{$ENDIF}
  DB, CREditor,
  DBAccess, Ora;

type
  TOraSPScriptEditor = class(TCREditorForm)
    meScript: TMemo;
  protected
    FSQL: string;
    FSession: TOraSession;
    function GetSQL: string;
    procedure SetSQL(const Value: string);

    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;
    procedure DoInit; override;
    procedure DoSave; override;
  public
    property SQL: string read GetSQL write SetSQL;
  end;

implementation

uses
  MemData, CRParser,
  {$IFDEF VER6P}
  Variants,
  {$ENDIF}
  DAParamValueEditor, OraScript, OraQueryEditor;

{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R OraSPScriptEditor.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

procedure TOraSPScriptEditor.DoInit;
var
  Component: TComponent;
begin
  Assert(Owner is TOraQueryEditorForm);
  Component := TOraQueryEditorForm(Owner).LocalComponent;
  Assert(Component is TOraDataset);
  FSession := TOraSession(TDBAccessUtils.UsedConnection(TOraDataset(Component)));
  btOk.Enabled := FSession <> nil;
  Modified := False;
  ReplaceMemos;
end;

procedure TOraSPScriptEditor.DoSave;
begin
  Assert(FSession <> nil);
  with TOraScript.Create(nil) do
    try
      Session := FSession;
      SQL.Text := FSQL;
      Execute;
    finally
      Free;
    end;
end;

function TOraSPScriptEditor.GetComponent: TComponent;
begin
  Result := nil;
end;

procedure TOraSPScriptEditor.SetComponent(Value: TComponent);
begin
end;

function TOraSPScriptEditor.GetSQL: string;
begin
  Result := FSQL;
end;

procedure TOraSPScriptEditor.SetSQL(const Value: string);
begin
  FSQL := Value;
  meScript.Lines.Text := Value;
end;

end.

