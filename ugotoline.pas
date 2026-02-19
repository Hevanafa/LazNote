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
  private

  public

  end;

implementation

{$R *.lfm}

end.

