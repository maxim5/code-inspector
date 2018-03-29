
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright  1998-2005 Core Lab. All right reserved.
//  StoredProc Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAStoredProcEditor;
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
  CREditor, CRTabEditor, DASQLFrame, DAParamsFrame, DAMacrosFrame, DASPCallFrame,
  DASQLEditor, DAUpdateSQLFrame, DASQLGeneratorFrame, DAQueryEditor;

type
  TDAStoredProcEditorForm = class(TDAQueryEditorForm)
  protected
    procedure DoInit; override;

    function GetStoredProc: TCustomDADataSet;
    procedure SetStoredProc(Value: TCustomDADataSet);

    property StoredProc: TCustomDADataSet read GetStoredProc write SetStoredProc;

  end;

implementation

uses 
  DADesignUtils;

{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R DAStoredProcEditor.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

procedure TDAStoredProcEditorForm.DoInit;
begin
  try
    inherited;
  finally
    Assert(FSQLFrame is TDASPCallFrame);
    TDASPCallFrame(FSQLFrame).Mode := spSQLSP;
    FSPCallFrame.Mode := spQuerySP;

    TDASPCallFrame(FSQLFrame).SetSPName(FDADesignUtilsClass.GetStoredProcName(LocalComponent as TCustomDADataSet));
  end;
end;

function TDAStoredProcEditorForm.GetStoredProc: TCustomDADataSet;
begin
  Result := FComponent as TCustomDADataSet;
end;

procedure TDAStoredProcEditorForm.SetStoredProc(Value: TCustomDADataSet);
begin
  FComponent := Value;
end;

end.
