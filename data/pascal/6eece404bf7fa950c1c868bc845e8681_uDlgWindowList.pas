{ Implementation of window list dialog}
unit uDlgWindowList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  // Window list dialog 
  TdlgWindowList = class(TForm)
    List: TListBox;
    Panel1: TPanel;
    btnCancel: TButton;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
  public
    class procedure Run;
  end;

implementation

uses
  uMainDataModule, CoreServices;

{$R *.dfm}

{ -------------------------------------------------------------------------------------------------------------------- }
{ TdlgWindowList }
procedure TdlgWindowList.FormShow(Sender: TObject);
var i: integer;
begin
  with WindowService do
  begin
    RefreshWindowList;
    for i := 0 to WindowCount - 1 do
      List.Items.Add(WindowCaptions[i]);
  end; 
end;

procedure TdlgWindowList.ListDblClick(Sender: TObject);
begin
  if list.ItemIndex > -1 then
    ModalResult := mrOK;
end;

class procedure TdlgWindowList.Run;
begin
  with TDlgWindowList.Create(nil) do
    try
      if (ShowModal = mrOK) and (List.ItemIndex > -1) then
        WindowService.ActivateWindow(List.ItemIndex);
    finally
      Free;
    end;
end;

end.
