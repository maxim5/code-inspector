{ Implementation of Project Options dialog }

unit uDlgProjectOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, uSvcProjects, editlist, uldgEditTasks;

type
  { Project options form }
  TdlgProjectOptions = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    PageControl1: TPageControl;
    tsGeneral: TTabSheet;
    tsTasks: TTabSheet;
    Memo1: TMemo;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    edtProjectName: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    fProject: TIDEProject;
    dlgEditTasks: TdlgEditTasks;
  public
    class procedure Execute;
  end;

implementation

uses
  UMainDataModule, CoreServices, Miscutils;

{$R *.dfm}

procedure TdlgProjectOptions.FormCreate(Sender: TObject);
begin
  fProject := ProjectService.ActiveProject;
  assert(assigned(fProject));
end;

procedure TdlgProjectOptions.FormShow(Sender: TObject);
begin
  edtProjectName.Text := fProject.Name;
  Memo1.Lines.Assign(fProject.IgnoreList);
  dlgEditTasks := TdlgEditTasks.Create(self);
  with dlgEditTasks do
  begin
    Tasks := fProject.ProjectTasks;

    btnOk.Visible := False;
    btnCancel.Visible := False;
    BorderStyle := bsNone;
    Align := alClient;
    Parent := tsTasks;
    Visible := True;
  end;
end;

class procedure TdlgProjectOptions.Execute;
begin
  with TdlgProjectOptions.Create(nil) do
    try
      if ShowModal = mrOK then
        try
          Screen.Cursor := crHourGlass;
          fProject.Name := edtProjectName.Text;
          ide.projects.DoProjectEvent(fProject, peProjectOptionsChanged, nil);
        finally
          Screen.Cursor := crDefault;
        end;
    finally
      Free;
    end;
end;

procedure TdlgProjectOptions.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOK then
  begin
    fProject.IgnoreList.Assign(Memo1.Lines);
    fProject.ProjectTasks.Assign(dlgEditTasks.Tasks);
  end;
end;

end.
