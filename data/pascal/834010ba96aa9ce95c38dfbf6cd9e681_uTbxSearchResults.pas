{ implementation of Search Results toolbox }

unit UTbxSearchResults;                                

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SearchFilesBackEnd, Menus, Buttons,
  ComCtrls, ExtCtrls, Tabs, uCustomToolbox, ActnList,
  UMainDataModule, CoreEditorBasedControls, ToolWin;

type

  TSearchResultPage = class(TTabSheet)
  private
    fListControl: TSearchResultsMemo;
    fPanel: TPanel;
    fResults: TSearchResults;
    fProgressBar: TProgressBar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ListControl: TSearchResultsMemo read fListControl;
    property Panel: TPanel read FPanel;
    property ProgressBar: TProgressBar read fProgressBar;
    property Results: TSearchResults read FResults write FResults;
  end;

  { Search Results toolbox form }
  TtbxSearchResults = class(TCustomToolbox)
    PageControl1: TPageControl;
    TabSet: TTabSet;
    actJumpToSource: TAction;
    actRepeatSearch: TAction;
    actCloseTab: TAction;
    actStopSearch: TAction;
    actGotoNext: TAction;
    actGotoPrev: TAction;
    actCloseAll: TAction;
    actStop: TAction;
    pmpopup1: TPopupMenu;
    JumptoSource2: TMenuItem;
    RepeatThisSearch2: TMenuItem;
    N3: TMenuItem;
    CloseTab2: TMenuItem;
    CloseAllTabs2: TMenuItem;
    N4: TMenuItem;
    Close2: TMenuItem;
    GotoEditor2: TMenuItem;
    actExpandAll: TAction;
    actCollapseAll: TAction;
    N1: TMenuItem;
    CollapseAll1: TMenuItem;
    ExpandAll1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure TabSetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actJumpToSourceExecute(Sender: TObject);
    procedure actRepeatSearchExecute(Sender: TObject);
    procedure actCloseTabExecute(Sender: TObject);
    procedure ActionsUpdate(Sender: TObject);
    procedure actGotoXXXExecute(Sender: TObject);
    procedure actCloseAllExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
    procedure actCollapseAllExecute(Sender: TObject);
  private
    function GetActiveResults: TSearchResults;
    function GetActivePage: TSearchResultPage;
    procedure CloseTab(TabIndex: integer);
    function FocusedResult: TSearchResult;
    procedure DoListControl(ts: TSearchResultPage);
    procedure MemoDblClick(Sender: TObject);
    procedure HandleOnSearchProgress(Sender: TSearchResults; const Msg: string; PercentDone: integer);
  public
    procedure AddResultsPage(aResults: TSearchResults);
    procedure ResultsUpdated(const aNotification: TSearchResultNotification);
    procedure UpdateLineNumbers(const aFileName: string; aFirstLine, aCount: Integer; isDeleted: Boolean);
  end;

implementation

uses
  Types, CoreServices, uDlgSearchInfiles, Miscutils, uSvcEditorService;

resourcestring
  MS_TABTEXT = '%s ';

{$R *.dfm}

{----------------------------------------------------------------------------------------------------------------------}
{ TtbxSearchResults }

procedure TtbxSearchResults.FormCreate(Sender: TObject);
begin
  inherited;
  MainDataModule.MainImageList.GetIcon(MainDataModule.actViewSearchResults.ImageIndex, Icon);
end;

procedure TtbxSearchResults.AddResultsPage(aResults: TSearchResults);
var
  i: integer;
  resultsPage: TSearchResultPage;
begin
  { associate gui&logic all here: }
  resultsPage := TSearchResultPage.Create(PageControl1);
  resultsPage.PageControl := PageControl1;
  resultsPage.Results := aResults;
  resultsPage.Results.GUILink := resultsPage;
  resultsPage.Results.OnSearchProgress := HandleOnSearchProgress;

  DoListControl(resultsPage);

  i := TabSet.Tabs.Add(Format(MS_TABTEXT, [AResults.Name]));
  TabSet.Tabs.Objects[i] := AResults;
  TabSet.TabIndex := i;
end;

procedure TtbxSearchResults.DoListControl(ts: TSearchResultPage);
begin
  { bind events to dummy's }
  ts.ListControl.OnDblClick := MemoDblClick;
  ts.ListControl.PopupMenu := pmpopup1;
  ts.ListControl.Results := ts.Results;
end;

procedure TtbxSearchResults.MemoDblClick(Sender: TObject);
begin
  actJumpToSourceExecute(nil);
  if FocusedResult <> nil then
    Abort;
end;

procedure TtbxSearchResults.ResultsUpdated(const aNotification: TSearchResultNotification);
var
  page: TSearchResultPage;
begin
  page := aNotification.Results.GUILink as TSearchResultPage;
  page.ListControl.ResultsUpdated(aNotification);
end;

function TtbxSearchResults.GetActivePage: TSearchResultPage;
begin
  Result := nil;
  if TabSet.TabIndex > -1 then // occurs at first time.
    Result := PageControl1.ActivePage as TSearchResultPage;
end;

function TtbxSearchResults.GetActiveResults: TSearchResults;
begin
  if GetActivePage <> nil then
    Result := GetActivePage.Results
  else
    Result := nil
end;

procedure TtbxSearchResults.HandleOnSearchProgress(Sender: TSearchResults;
    const Msg: string; PercentDone: integer);
begin
  TSearchResultPage(Sender.GUILink).fProgressBar.Position := PercentDone;
  TSearchResultPage(Sender.GUILink).fProgressBar.Visible := PercentDone < 99;
end;

procedure TtbxSearchResults.TabSetClick(Sender: TObject);
begin
  PageControl1.ActivePageIndex := TabSet.TabIndex;
end;

function  TtbxSearchResults.FocusedResult: TSearchResult;
begin
  Result := GetActivePage.ListControl.FocusedResult;
end;

procedure TtbxSearchResults.actJumpToSourceExecute(Sender: TObject);
begin
  if FocusedResult <> nil then
    with FocusedResult do
      if ide.Editors.Locate(FileName, LineNo, Offset) then
        ide.ActiveBuffer.Memo.SetFocus;
end;

procedure TtbxSearchResults.actRepeatSearchExecute(Sender: TObject);
begin
  { copy settings to the dialog }
  with SearchInFilesForm do
  begin
    cbCaseSensFiles.Checked := GetActiveResults.Settings.MatchCase;
    cbRecursive.Checked := GetActiveResults.Settings.Recursive;
    cbRegExp.Checked := GetActiveResults.Settings.UseRegularExpr;
    cbSearchText.Text := GetActiveResults.Settings.SearchStr;
    cbWholeWordsFile.Checked := GetActiveResults.Settings.WholeWords;
    case GetActiveResults.Settings.SearchLocation of
      slInEditors: rbAllOpenFiles.Checked := true;
      slInProject: rbCurProjFiles.Checked := true;
      slInDirectories: rbDirectory.Checked := true;
    end;
    cbxPath.Text := GetActiveResults.Settings.Path;
    cbxMasks.Text := GetActiveResults.Settings.Masks;

    Execute;
  end;
end;

procedure TtbxSearchResults.actStopExecute(Sender: TObject);
begin
  with GetActivePage do begin
    Results.StopSearching;
    ProgressBar.Visible := False;
  end;
end;

procedure TtbxSearchResults.actCloseAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for I := TabSet.Tabs.Count - 1 downto 0 do
    CloseTab(i);
end;

procedure TtbxSearchResults.CloseTab(TabIndex: integer);
begin
  PageControl1.Pages[TabIndex].Free;
  TabSet.Tabs.Delete(TabIndex);
end;

procedure TtbxSearchResults.actCloseTabExecute(Sender: TObject);
begin
  CloseTab(TabSet.TabIndex);
end;

procedure TtbxSearchResults.actCollapseAllExecute(Sender: TObject);
begin
  GetActivePage.ListControl.FullCollapse;
end;

procedure TtbxSearchResults.actExpandAllExecute(Sender: TObject);
begin
  GetActivePage.ListControl.FullExpand;
end;

procedure TtbxSearchResults.ActionsUpdate(Sender: TObject);
var enb: boolean;
begin
  actStop.Enabled := (TabSet.Tabs.Count > 0) and (GetActiveResults <> nil) and (GetActiveResults.IsSearchInProgress);
  actCloseTab.Enabled := (TabSet.Tabs.Count > 0) and (not actStop.Enabled); // not enabled while search in progress
  actCloseAll.Enabled := actCloseTab.Enabled;
  enb := GetActiveResults <> nil;
  actJumpToSource.Enabled := enb and (FocusedResult <> nil);
  EnableActions([actRepeatSearch, actGotoNext, actGotoPrev], enb);
end;

procedure TtbxSearchResults.actGotoXXXExecute(Sender: TObject);
var
  res: TSearchResult;
begin
  res := (GetActiveResults.GUILink as TSearchResultPage).ListControl.FocusNextResult(Sender <> actGotoNext);
  if assigned(res) then
    ide.Editors.Locate(res.FileName, res.LineNo, res.Offset)
  else
    Beep;
end;

//todo: DISABLED
procedure TtbxSearchResults.UpdateLineNumbers(const aFileName: string;
                                              aFirstLine, aCount: Integer; isDeleted: Boolean);
var i, k: integer;
    ress: TSearchResults;
    r: TSearchResult;
begin
  for i := 0 to PageControl1.PageCount - 1 do
  begin
    assert( PageControl1.Pages[i] is TSearchResultPage );

    ress := TSearchResultPage(PageControl1.Pages[i]).Results;
    for k := 0 to ress.Count - 1 do
    begin
      r := ress.Result[k];
      if SameFileName(aFileName, r.FileName) then
      begin
        if isDeleted then
        begin
          if r.LineNo > aFirstLine + aCount - 1 then
            dec(r.LineNo, aCount)
          else
          if (r.LineNo >= aFirstLine) and (r.LineNo < aFirstLine + aCount) then
            r.LineNo := 0; // that line is gone
        end
        else
        begin
          if r.LineNo >= aFirstLine then
            inc(r.LineNo, aCount);
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------------------------------------------------}
{ TSearchResultPage }

constructor TSearchResultPage.Create(AOwner: TComponent);
begin
  inherited;
  TabVisible := false;

  fListControl := TSearchResultsMemo.Create(self);
  with fListControl do
  begin
    Align := alClient;
    Parent := self;
  end;

  FPanel := TPanel.Create(self);
  with FPanel do
  begin
    Visible := false;
    Parent := self;
    Height := 20;
    BevelOuter := bvNone;
    Align := alBottom;
    Caption := '';
  end;

  fProgressBar := TProgressBar.Create(Self);
  with fProgressBar do
  begin
    Parent := Self;
    Left := 1;
    Top := 1;
    Width := 100;
    Height := 6;
    Anchors := [akTop, akRight];
    Smooth := True;
    Step := 1;
  end;
end;

destructor TSearchResultPage.Destroy;
begin
  inherited;
end;

end.
