{ Implementation of Parameters dialog }

unit uDlgParameters;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TdlgParameters = class(TForm)
    GroupBox1: TGroupBox;
    cbxParams: TComboBox;
    GroupBox2: TGroupBox;
    cbxDirs: TComboBox;
    btnSelDir: TButton;
    Panel1: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    procedure btnSelDirClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

implementation

uses
  FileCtrl, MiscUtils, UMainDataModule, Jclinifiles, CoreServices;

{$R *.dfm}

procedure TdlgParameters.FormCreate(Sender: TObject);
begin
//
end;

procedure TdlgParameters.FormShow(Sender: TObject);
begin
  IniReadStrings(AppOptions.HistoryIni, 'TdlgParameters.cbxParams.Items', cbxParams.Items);
  IniReadStrings(AppOptions.HistoryIni, 'TdlgParameters.cbxDirs.Items', cbxDirs.Items);
end;

procedure TdlgParameters.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOK then
  begin
    ArrangeComboMRU(cbxParams);
    ArrangeComboMRU(cbxDirs);
    IniWriteStrings(AppOptions.HistoryIni, 'TdlgParameters.cbxParams.Items', cbxParams.Items);
    IniWriteStrings(AppOptions.HistoryIni, 'TdlgParameters.cbxDirs.Items', cbxDirs.Items);
  end;
end;

procedure TdlgParameters.btnSelDirClick(Sender: TObject);
var
  s: string;
begin
  if SelectDirectory('Select directory', cbxDirs.Text, s) then
    cbxDirs.Text := s;
end;

end.
