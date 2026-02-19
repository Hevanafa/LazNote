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

    procedure ContentMemoChange(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);
    procedure NewFileMenuClick(Sender: TObject);
    procedure ExitFileMenuClick(Sender: TObject);

  private
    dirtyEditor, lastDirtyEditor: boolean;

    procedure LoadTextFile(const filename: string);
    procedure UpdateCaption;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.UpdateCaption;
begin
  Caption := 'LazNote';

  if dirtyEditor then
    Caption := Caption + ' (*)';
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  ContentMemo.clear;
  dirtyEditor := false;
  UpdateCaption
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  LoadTextFile(filenames[0])
end;

procedure TForm1.ContentMemoChange(Sender: TObject);
begin
  dirtyEditor := true;

  if lastDirtyEditor <> dirtyEditor then begin
    lastDirtyEditor := dirtyEditor;
    UpdateCaption
  end;
end;

procedure TForm1.ExitFileMenuClick(Sender: TObject);
begin
  close
end;

procedure TForm1.LoadTextFile(const filename: string);
var
  f: text;
  line: string;
  lines: TStringList;
begin
  if not FileExists(filename) then begin
    MessageDlg(format('Could not find %s!', [filename]), mtError, [mbOK], 0);
    exit
  end;

  AssignFile(f, filename);
  {$I-} reset(f); {$I+}

  ContentMemo.clear;
  lines := TStringList.create;

  if IOResult = 0 then
    while not eof(f) do begin
      readln(f, line);
      lines.add(line);
    end;

  CloseFile(f);

  ContentMemo.Lines.AddStrings(lines);
  lines.free;

  dirtyEditor := false;
  UpdateCaption
end;

procedure TForm1.NewFileMenuClick(Sender: TObject);
begin
  ContentMemo.clear;
end;

end.

