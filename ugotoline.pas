unit UGoToLine;

{$Mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, Forms,
  Controls, Graphics, Dialogs, StdCtrls, Spin;

type

  { TGoToLine }

  TGoToLine = class(TForm)
    AcceptButton: TButton;
    CancelButton: TButton;
    LineNumberLabel: TLabel;
    LineNumberEdit: TSpinEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private

  public

  end;

implementation

{$R *.lfm}

{ TGoToLine }

procedure TGoToLine.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := false;
  hide
end;

end.

