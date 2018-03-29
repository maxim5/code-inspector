{ Implementation of Output toolbox }

unit uTbxOutput;                                                                               

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, ComCtrls, Menus,  ExtCtrls, uCustomToolbox,
  ActnList, StdCtrls, CoreEditorBasedControls;

type
  // Output form class 
  TtbxOutput = class(TCustomToolBox)
    PageControl: TPageControl;
    popOutput: TPopupMenu;
    ActionList1: TActionList;
    actClear: TAction;
    actCopy: TAction;
    actSave: TAction;
    Clear1: TMenuItem;
    CopytoClipboard1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    GotoEditor1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
  private
    fOutputLines: TStrings;
    fMemo: TOutputConsoleMemo;
  public
    { Clear output contents } 
    procedure Clear;

    { Search output starting from the end for an error output, when found locate in editor }
    procedure TryGotoLastError;

    procedure GotoNextTraceback(aNext: boolean = true);

    { The actual output control }
    property Memo: TOutputConsoleMemo read fMemo;
    property OutputLines: TStrings read fOutputLines; // easier access to lines
  end;

implementation

uses
  mukDebugUnit, UMainDataModule, uEditorForm, MiscUtils, uSvcGUI,
  CoreServices, Math, uSvcEditorService, Clipbrd, uGUIThemes;

{$R *.dfm}

const
  ERROR_BACKGR_COLOR: TColor = $00000084;
  ERROR_FORE_COLOR: TColor = clRED;

procedure TtbxOutput.FormCreate(Sender: TObject);
begin
  inherited;
  MainDataModule.MainImageList.GetIcon(MainDataModule.actViewOutput.ImageIndex, Icon);

  fMemo := TOutputConsoleMemo.Create(self);
  fMemo.Align := alClient;
  fMemo.Parent := self;
  fMemo.PopupMenu := popOutput;
  fOutputLines := fMemo.Lines;

  ide.options.Themes.RegisterEventObserver(
    procedure(NewTheme: TIdeTheme)
    var style: TThemeStyle;
    begin
      if NewTheme <> nil then
      begin
        style := NewTheme.StyleByName['Toolwindow.Content'];
        if style <> nil then
        begin
          fMemo.Color := style.BgColor;
          fMemo.Font.Color := style.Font.Color;
        end;
      end;
    end
  );
end;

procedure TtbxOutput.actClearExecute(Sender: TObject);
begin
  Clear;
end;

procedure TtbxOutput.actCopyExecute(Sender: TObject);
begin
  if fMemo.SelAvail then Clipboard.AsText := fMemo.SelText
  else Clipboard.AsText := fMemo.Text;
end;

procedure TtbxOutput.actSaveExecute(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
    try
      Title := 'Save Contents to';
      FileName := 'output.txt';
      Filter := '*.txt|*.txt';
      DefaultExt := 'txt';
      if Execute then
      try
        fMemo.Lines.SaveToFile(FileName);
      except
        ide.gui.ExecuteErrorDialog('Can not save to file ' + QuotedStr(FileName));
      end;
    finally
      Free;
    end;
end;

procedure TtbxOutput.Clear;
begin
  fMemo.Clear;
end;

procedure TtbxOutput.TryGotoLastError;
//var i, line: integer;
//    f: string;
begin
//  for i := fOutputLines.Count-1 downto 0 do
//  begin
//    if ParseErrorLine(fOutputLines[i], f, line) then
//    begin
//      if ide.Editors.Open(f, [ooDontShowErrors]) then
//      begin
//        vt.FocusedNode := miscutils.GetVTNodeByIndex(vt, i);
//        vt.Selected[vt.FocusedNode] := true;
//        vt.TopNode := vt.FocusedNode;
//        ide.Editors.ActiveBuffer.Memo.GotoXY(1, line, true);
//      end;
//      BREAK;
//    end;
//  end;
end;

procedure TtbxOutput.GotoNextTraceback(aNext: boolean);
//var cid: integer;
//    i: integer;
//    node: PVirtualNode;
begin
//  cid := IfThen(Assigned(vt.FocusedNode), vt.FocusedNode.Index, 0);
//  i := IfThen(aNext, cid+1, cid-1);
//  node := vt.FocusedNode;
//  while (i < vt.RootNodeCount) and (i >= 0) do begin
//    if aNext then node := vt.GetNext(node) else node := vt.GetPrevious(node);
//    if isErrorLine(self.fOutputLines[i]) then begin
//      vt.FocusedNode := node;
//      vt.Selected[node] := true;
//      vtDblClick(nil);
//      BREAK;
//    end;
//    i := IfThen(aNext, i+1, i-1);
//  end;
end;

end.
