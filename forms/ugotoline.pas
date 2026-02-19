unit UGoToLine;

{$Mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, Forms,
  Controls, Graphics, Dialogs,
  StdCtrls, Spin, LCLType;

type

  { TGoToLine }

  TGoToLine = class(TForm)
    AcceptButton: TButton;
    CancelButton: TButton;
    LineNumberLabel: TLabel;
    LineNumberEdit: TSpinEdit;
    procedure AcceptButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CancelButtonClick(Sender: TObject);
    procedure CancelButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure LineNumberEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public
    procedure SetMaximum(n: longint);
  end;

implementation

{$R *.lfm}

{ TGoToLine }

procedure TGoToLine.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := false;
  hide
end;

procedure TGoToLine.AcceptButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then close;
end;

procedure TGoToLine.CancelButtonClick(Sender: TObject);
begin
  close
end;

procedure TGoToLine.CancelButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then close;
end;

procedure TGoToLine.LineNumberEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then close;
end;

procedure TGoToLine.SetMaximum(n: longint);
begin
  LineNumberLabel.Caption := format('Line number (1 to %d):', [n]);
  LineNumberEdit.MaxValue := n
end;

end.

