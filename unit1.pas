unit Unit1;

{$Mode ObjFPC}
{$H+}

interface

uses
  Classes, SysUtils, Forms,
  Controls, Graphics, Dialogs, Menus, StdCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu: TMainMenu;
    ContentMemo: TMemo;
    MainStatusBar: TStatusBar;
    FileMenu: TMenuItem;
    ExitFileMenu: TMenuItem;
    SaveAsFileMenu: TMenuItem;
    SaveFileMenu: TMenuItem;
    NewFileMenu: TMenuItem;
    OpenFileMenu: TMenuItem;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  ContentMemo.clear
end;

end.

