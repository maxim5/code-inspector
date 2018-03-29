
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright  1998-2005 Core Lab. All right reserved.
//  Tab Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRTabEditor;
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
  SysUtils, Classes,
  CREditor, CRFrame, DADesignUtils, MemData;

type
  TCRTabEditorForm = class(TCREditorForm)
    PageControl: TPageControl;
    procedure FormDestroy(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlChanging(Sender: TObject;
      var AllowChange: Boolean);

  protected
    FFramesList: TList;

  {$IFDEF LINUX}
    procedure DoInit; override;
    procedure DoActivate; override;
  {$ENDIF}
  {$IFDEF DBTOOLS}
    procedure ExitActiveControl; override;
  {$ENDIF}
    procedure SaveControlData; override;

    function AddTab(FrameClass: TCRFrameClass; Page: TTabSheet): TCRFrame;

    function GetFrameByInitProp: TCRFrame; virtual;

    function GetModified: boolean; override;
    procedure SetModified(Value: boolean); override;

    function GetActiveFrame: TCRFrame;

    // Avoid Kylix bug 
    procedure DoPageControlChange(Sender: TObject); virtual;
    procedure DoPageControlChanging(Sender: TObject; var AllowChange: Boolean); virtual;
  public
    constructor Create(Owner: TComponent; DADesignUtilsClass: TDADesignUtilsClass); override;
    procedure ActivateFrame(Frame: TCRFrame);

    property ActiveFrame: TCRFrame read GetActiveFrame;    
  end;

implementation

{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R CRTabEditor.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

{ TCRTabEditorForm }

{
procedure TCRTabEditorForm.CreateFrames;
begin
  Assert(False, 'Must be overriden - D8 bug');
end;
}

{$IFDEF LINUX}
procedure TCRTabEditorForm.DoInit;
begin
  inherited;

  PageControl.OnChange := nil;
  PageControl.OnChanging := nil;
  PageControl.ActivePageIndex := 0;
  PageControl.OnChange := PageControlChange;
  PageControl.OnChanging := PageControlChanging;
end;

procedure TCRTabEditorForm.DoActivate;
var
  ActiveFrame: TCRFrame;
begin
  inherited;

  // on Windows frame is activated from PageControlChange method
  ActiveFrame := GetActiveFrame;
  if ActiveFrame <> nil then
    ActiveFrame.Activate;
end;
{$ENDIF}

{$IFDEF DBTOOLS}
procedure TCRTabEditorForm.ExitActiveControl;
begin
  DBTools.CheckDBToolsChanges(GetActiveFrame);
  inherited;
end;
{$ENDIF}

procedure TCRTabEditorForm.SaveControlData;
var
  ActiveFrame: TCRFrame;
begin
  ActiveFrame := GetActiveFrame;
  if ActiveFrame <> nil then
    ActiveFrame.Finish;

  inherited;
end;

function TCRTabEditorForm.AddTab(FrameClass: TCRFrameClass; Page: TTabSheet): TCRFrame;
begin
  Result := FrameClass.Create(Self);
  Result.Parent := Page;
  Result.Align := alClient;
  Result.Name := Page.Name + FrameClass.ClassName;
  Result.Editor := Self;

  FFramesList.Add(Result);
end;

function TCRTabEditorForm.GetModified: boolean;
var
  i :integer;
begin
  Result := inherited GetModified;
  for i := 0 to FFramesList.Count - 1 do
    Result := Result or TCRFrame(FFramesList[i]).Modified;
end;

procedure TCRTabEditorForm.SetModified(Value: boolean);
var
  i :integer;
begin
  inherited;
  for i := 0 to FFramesList.Count - 1 do
    TCRFrame(FFramesList[i]).Modified := Value;
end;

constructor TCRTabEditorForm.Create(Owner: TComponent; DADesignUtilsClass: TDADesignUtilsClass);
begin
  FFramesList := TList.Create;
  inherited;
end;

procedure TCRTabEditorForm.FormDestroy(Sender: TObject);
begin
  inherited;
  FFramesList.Free;
end;

function TCRTabEditorForm.GetActiveFrame: TCRFrame;
var
  i: integer;
begin
  for i := 0 to FFramesList.Count - 1 do
    if TCRFrame(FFramesList[i]).Page = PageControl.ActivePage then begin
      Result := TCRFrame(FFramesList[i]);
      Exit;
    end;
  Result := nil;
end;

procedure TCRTabEditorForm.ActivateFrame(Frame: TCRFrame);
var
  ActiveFrame: TCRFrame;
begin
  ActiveFrame := GetActiveFrame;
  if ActiveFrame <> nil then
    ActiveFrame.Finish;

  if Frame.Page <> PageControl.ActivePage then
    PageControl.ActivePage := Frame.Page;
  Frame.Activate;
end;

procedure TCRTabEditorForm.DoPageControlChange(Sender: TObject); 
var
  ActiveFrame: TCRFrame;
begin
  ActiveFrame := GetActiveFrame;
  if ActiveFrame <> nil then
    ActiveFrame.Activate;
end;

procedure TCRTabEditorForm.PageControlChange(Sender: TObject);
begin
  DoPageControlChange(Sender); 
end;

procedure TCRTabEditorForm.DoPageControlChanging(Sender: TObject; var AllowChange: Boolean); 
var
  ActiveFrame: TCRFrame;
begin
  try
    ActiveFrame := GetActiveFrame;
    if ActiveFrame <> nil then
      ActiveFrame.Finish;
  except
    on E: Exception do begin
      AllowChange := False;
      ApplicationHandleException(E);
    end;
  end;
end;

procedure TCRTabEditorForm.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  DoPageControlChanging(Sender, AllowChange); 
end;

function TCRTabEditorForm.GetFrameByInitProp: TCRFrame;
var
  i :integer;
begin
  Result := nil;
  if InitialProperty <> '' then begin
    for i := 0 to FFramesList.Count - 1 do
      if TCRFrame(FFramesList[i]).Page.Caption = InitialProperty then begin
        Result := TCRFrame(FFramesList[i]);
        Break;
      end;
    Assert(Result <> nil, 'Unknown frame ' + InitialProperty);
  end;
end;

end.
