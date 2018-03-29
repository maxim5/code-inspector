{ Implementation of New Python Profile dialog }

unit uDlgEditPythonInterpreter;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, uSvcPythonEnvironments, Mask, JvToolEdit,
  JvComponent, JvCreateProcess, ComCtrls, JvExMask, JvComponentBase;

type
  TdlgEditPythonInterpreter = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    cpr: TJvCreateProcess;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label2: TLabel;
    Label3: TLabel;
    editName: TEdit;
    editExecutablePath: TJvFilenameEdit;
    Label7: TLabel;
    GroupBox2: TGroupBox;
    lbPaths: TListBox;
    editPath: TEdit;
    Button3: TButton;
    btnDelpath: TButton;
    btnAddPath: TButton;
    btnTrySysPath: TButton;
    Bevel1: TBevel;
    GroupBox3: TGroupBox;
    btnGetBuiltins: TButton;
    TabSheet4: TTabSheet;
    gbDebugger: TGroupBox;
    rbNoDebug: TRadioButton;
    rbDebug26: TRadioButton;
    TabSheet5: TTabSheet;
    Label4: TLabel;
    editHelpPath: TJvFilenameEdit;
    Label1: TLabel;
    Bevel2: TBevel;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    editPyShellPath: TJvFilenameEdit;
    Label6: TLabel;
    editPyShellPath2: TJvFilenameEdit;
    Label8: TLabel;
    editPyShellPath3: TJvFilenameEdit;
    rbDebug25: TRadioButton;
    procedure editNameChange(Sender: TObject);
    procedure lbPathsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnDelpathClick(Sender: TObject);
    procedure btnTrySysPathClick(Sender: TObject);
    procedure cprTerminate(Sender: TObject; ExitCode: Cardinal);
    procedure FormCreate(Sender: TObject);
    procedure editExecutablePathChange(Sender: TObject);
    procedure lbPathsClick(Sender: TObject);
    procedure editPathKeyPress(Sender: TObject; var Key: Char);
    procedure Button3Click(Sender: TObject);
    procedure btnAddPathClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    fProfileManager: TPythonInterpreterService; // operate on this fake manager
    fProfile: TPythonEnvironment;

    // For new profiles XMLText and such object properties aren't created
    fBuiltinsText: string;
    procedure SaveToProfile(aProfile: TPythonEnvironment);
    // pass nil to indicate new profile. creation
    procedure LoadFromProfile(aProfile: TPythonEnvironment);
  public
    // pass nil to aProfile indicate new profile. creation
    class function Execute(aProfileManager: TPythonInterpreterService; aProfile: TPythonEnvironment): TModalResult;
  end;

  EInternalScriptError = class(Exception)
  end;

implementation

uses UMainDataModule, JclFileUtils, FileCtrl, MiscUtils, CoreServices,
  uTaskDialogs;

{$R *.dfm}

procedure TdlgEditPythonInterpreter.FormCreate(Sender: TObject);
begin
//  cpr.CurrentDirectory := ExtractFilePath(Application.ExeName);
//  PageControl1.ActivePageIndex := 0;
//  editExecutablePath.Dialog.Filter := 'Executables (*.exe)|*.exe';
//  editPyShellPath.Dialog.Filter := editExecutablePath.Dialog.Filter;
//  editPyShellPath2.Dialog.Filter := editExecutablePath.Dialog.Filter;
//  editHelpPath.Dialog.Filter := 'Help files (*.html;*.htm;*.chm)|*.html;*.htm;*.chm';
end;

procedure TdlgEditPythonInterpreter.LoadFromProfile;
begin
//  fProfile := aProfile;
//  if assigned(fProfile) then begin
//    with fProfile do begin
//      editName.Text := Name;
//      editHelpPath.Text := HelpPath;
//      lbPaths.Items.Assign(Paths);
//      editExecutablePath.Text := ExecutablePath;
//      editPyShellPath.Text := PythonShell;
//      editPyShellPath2.Text := PythonShell2;
//      editPyShellPath3.Text := PythonShell3;
//      case DebuggerType of
//        dtNone: rbNoDebug.Checked := True;
//        dtPython25: rbDebug25.Checked := True;
//        dtPython26: rbDebug26.Checked := True;
//      end;
//      if FileExists(GetDirectoryName + 'builtins.xml') then
//      fBuiltinsText := FileToString(GetDirectoryName + 'builtins.xml');
//    end;
//  end;
end;

procedure TdlgEditPythonInterpreter.lbPathsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    btnDelpathClick(self);
end;

procedure TdlgEditPythonInterpreter.btnDelpathClick(Sender: TObject);
var
  i: integer;
begin
  for i := lbPaths.Items.Count - 1 downto 0 do
    if lbPaths.Selected[i] then
      lbPaths.Items.Delete(i);
end;

procedure TdlgEditPythonInterpreter.btnTrySysPathClick(Sender: TObject);
begin
  // Scripts output to files in temporary path and results are read from those files:
  if Sender = btnTrySysPath then
  begin
    Tag := 1; // flag for cprTerminate event 
    cpr.CommandLine := Format('%s %sdumpsyspath.py %ssyspath.txt',
      [editExecutablePath.Text, ExtractFilePath(Application.ExeName), IncludeTrailingPathDelimiter(PathGetTempPath)]);
  end
  else
    if Sender = btnGetBuiltins then
  begin
    Tag := 2;
    cpr.CommandLine := Format('%s %sdumpbuiltins.py %sbuiltins.txt',
      [editExecutablePath.Text, ExtractFilePath(Application.ExeName), IncludeTrailingPathDelimiter(PathGetTempPath)]);
  end;
  EnableControls([btnOK, btnGetBuiltins, btnTrySysPath], false);
  try
    cpr.Run;
    ResumeThread(cpr.ProcessInfo.hThread);
    Sleep(100); // !!!output misses without this
  except on e: Exception do
    begin
      MessageDlg(Format('Can not execute ''%s''' + CRLF +
                        'Check if ''Executable path'' box contains a valid path!',
                 [cpr.CommandLine]), mtError, [mbOK], 0);
      EnableControls([btnOK, btnGetBuiltins, btnTrySysPath], true);
    end;
  end;
end;

procedure TdlgEditPythonInterpreter.cprTerminate(Sender: TObject; ExitCode: Cardinal);
var list: TStringList;
begin
  try
    if ExitCode <> 0 then // error in scripts
    begin                               
      raise EInternalScriptError.Create(cpr.ConsoleOutput.Text);
    end else
    if Tag = 1 then // get sys.path
    begin
      list := TStringList.Create;
      try
        list.LoadFromFile(Format('%ssyspath.txt', [IncludeTrailingPathDelimiter(PathGetTempPath)]));
        lbPaths.Items.AddStrings(list);
      finally
        list.Free;
      end;
    end
    else
    if Tag = 2 then // get builtins
    begin
      fBuiltinsText := FileToString(Format('%sbuiltins.txt', [IncludeTrailingPathDelimiter(PathGetTempPath)]));
    end;
  finally
    EnableControls([btnOK, btnGetBuiltins, btnTrySysPath], true);
  end;
end;

procedure TdlgEditPythonInterpreter.editNameChange(Sender: TObject);
begin
  btnOK.Enabled := editName.Text <> '';
end;

procedure TdlgEditPythonInterpreter.editExecutablePathChange(Sender: TObject);
begin
  btnTrysysPath.Enabled := Trim(editExecutablePath.Text) <> '';
  btnGetBuiltins.Enabled := btnTrysysPath.Enabled;
end;

procedure TdlgEditPythonInterpreter.lbPathsClick(Sender: TObject);
begin
  if lbPaths.ItemIndex > -1 then
    editPath.Text := lbPaths.Items[lbPaths.ItemIndex];
end;

procedure TdlgEditPythonInterpreter.editPathKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    btnAddPathClick(nil);
    Key := #0;
  end;
end;

procedure TdlgEditPythonInterpreter.btnAddPathClick(Sender: TObject);
begin
  if Trim(editPath.Text) <> '' then
    lbPaths.Items.Append(editPath.Text);
end;

procedure TdlgEditPythonInterpreter.Button3Click(Sender: TObject);
var
  s: string;
begin
  if SelectDirectory('Select directory', editPath.Text, s, [sdNewUI]) then
    editPath.Text := s;
end;

procedure TdlgEditPythonInterpreter.SaveToProfile(aProfile: TPythonEnvironment);
begin
//  with aProfile do
//  begin
//    Name := EditName.Text;
//    HelpPath := editHelpPath.Text;
//    Paths.Assign(lbPaths.Items);
//    ExecutablePath := editExecutablePath.Text;
//    PythonShell := editPyShellPath.Text;
//    PythonShell2 := editPyShellPath2.Text;
//    PythonShell3 := editPyShellPath2.Text;
//    if rbNoDebug.Checked then DebuggerType := dtNone;
//    if rbDebug25.Checked then DebuggerType := dtPython25;
//    if rbDebug26.Checked then DebuggerType := dtPython26;
//    fProfileManager.SaveProfile(aProfile);
//    StringToFile(fBuiltinsText, GetDirectoryName + 'builtins.xml');
//  end;
end;

class function TdlgEditPythonInterpreter.Execute(aProfileManager: TPythonInterpreterService; aProfile: TPythonEnvironment): TModalResult;
begin
//  with TdlgEditPythonInterpreter.Create(nil) do
//    try
//      fProfileManager := aProfileManager;
//      if aProfile <> nil then
//        LoadFromProfile(aProfile);
//      Result := ShowModal;
//    finally
//      Free;
//    end;
end;

procedure TdlgEditPythonInterpreter.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var prf: TPythonEnvironment;
begin
//  if ModalResult = mrOK then begin
//    try
//      if fProfile = nil then // new prof.
//        prf := fProfileManager.AddProfile(editName.Text)
//      else // edited prof.
//      begin
//        prf := fProfile;
//        if not SameText(prf.Name, Trim(editName.Text)) then // Name changed
//          fProfileManager.RenameProfile(prf.Name, editName.Text);
//      end;
//    except on err: Exception do // dont let go.
//      begin
//        ExecuteErrorDialog(err.Message);
//        ModalResult := mrNone;
//        EXIT;
//      end;
//    end;
//    SaveToProfile(prf);
//  end
//  else
//  begin
//    if cpr.State <> psReady then
//      cpr.Terminate;
//  end;
end;

end.
