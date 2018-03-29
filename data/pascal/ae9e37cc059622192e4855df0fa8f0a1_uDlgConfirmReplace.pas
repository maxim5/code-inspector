{implementation of Confirm Replace dialog}

unit uDlgConfirmReplace;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TdlgConfirmReplace = class(TForm)
    btnCancel: TButton;
    btnReplace: TButton;
    btnReplaceAll: TButton;
    btnSkip: TButton;
    Image1: TImage;
    lblConfirmation: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    procedure PrepareShow(AEditorRect: TRect; X, Y1, Y2: integer; AReplaceText:
      string);
  end;

var
  dlgConfirmReplace: TdlgConfirmReplace;

implementation

{$R *.DFM}

resourcestring
  SAskReplaceText = 'Replace this occurence of ''%s''?';

{ TdlgConfirmReplace }

procedure TdlgConfirmReplace.FormCreate(Sender: TObject);
begin
  Image1.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
end;

procedure TdlgConfirmReplace.FormDestroy(Sender: TObject);
begin
  dlgConfirmReplace := nil;
end;

procedure TdlgConfirmReplace.PrepareShow(AEditorRect: TRect; X, Y1, Y2:
  integer; AReplaceText: string);
var
  nW, nH: Integer;
begin
  lblConfirmation.Caption := Format(SAskReplaceText, [AReplaceText]);
  nW := AEditorRect.Right - AEditorRect.Left;
  nH := AEditorRect.Bottom - AEditorRect.Top;

  if nW <= Width then
    X := AEditorRect.Left - (Width - nW) div 2
  else
  begin
    if X + Width > AEditorRect.Right then
      X := AEditorRect.Right - Width;
  end;
  if Y2 > AEditorRect.Top + MulDiv(nH, 2, 3) then
    Y2 := Y1 - Height - 4
  else
    Inc(Y2, 4);
  SetBounds(X, Y2, Width, Height);
end;

end.
