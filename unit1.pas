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
    Separator1: TMenuItem;
    Separator2: TMenuItem;

    procedure ContentMemoChange(Sender: TObject);

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);

    procedure NewFileMenuClick(Sender: TObject);
    procedure OpenFileMenuClick(Sender: TObject);
    procedure SaveFileMenuClick(Sender: TObject);
    procedure SaveAsFileMenuClick(Sender: TObject);
    procedure ExitFileMenuClick(Sender: TObject);

  private
    dirtyEditor: boolean;
    activeFilepath: string;

    procedure LoadTextFile(const filename: string);
    procedure SaveTextFile(const filename: string);
    function CheckFileSize(const filename: string): TModalResult;
    function CheckDirtyEditor: TModalResult;
    procedure UpdateCaption;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.UpdateCaption;
var
  prefix: string;
begin
  if dirtyEditor then
    prefix := '* '
  else prefix := '';

  if activeFilepath = '' then
    caption := prefix + 'Untitled'
  else
    caption := prefix + activeFilepath;

  Caption := caption + ' - LazNote';
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  NewFileMenuClick(nil)
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  LoadTextFile(filenames[0])
end;

procedure TForm1.ContentMemoChange(Sender: TObject);
begin
  dirtyEditor := true;
  UpdateCaption
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  case CheckDirtyEditor of
    mrYes:
      SaveFileMenuClick(self);
    mrNo:
      CanClose := true;
    mrCancel:
      CanClose := false;
  end;
end;

{ Used to check if the user wants to save the file or not }
function TForm1.CheckDirtyEditor: TModalResult;
var
  interpreted: string;
begin
  if not dirtyEditor then
    CheckDirtyEditor := mrNo
  else begin
    if activeFilepath = '' then
      interpreted := '[Untitled]'
    else
      interpreted := activeFilepath;

    CheckDirtyEditor := MessageDlg(
      'Save',
      format('Save file %s?', [interpreted]),
      mtConfirmation, mbYesNoCancel, 0);
  end;
end;


procedure TForm1.NewFileMenuClick(Sender: TObject);
begin
  case CheckDirtyEditor of
    mrYes: SaveFileMenuClick(NewFileMenu);
    mrNo: ;
    mrCancel: exit;
  end;

  activeFilepath := '';
  ContentMemo.clear;
  dirtyEditor := false;
  UpdateCaption
end;

procedure TForm1.OpenFileMenuClick(Sender: TObject);
var
  od: TOpenDialog;
begin
  case CheckDirtyEditor of
    mrYes: SaveFileMenuClick(OpenFileMenu);
    mrNo: ;
    mrCancel: exit;
  end;

  od := TOpenDialog.create(self);

  try
    od.Filter := 'Text files|*.txt|All files|*.*';
    od.DefaultExt := 'txt';

    if od.Execute then
      LoadTextFile(od.filename);
  finally
    od.free
  end;
end;

procedure TForm1.SaveFileMenuClick(Sender: TObject);
begin
  if activeFilepath = '' then SaveAsFileMenuClick(sender);

  SaveTextFile(activeFilepath)
end;

procedure TForm1.SaveAsFileMenuClick(Sender: TObject);
var
  sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(Self);

  try
    sd.filter := 'Text files|*.txt|All files|*.*';
    sd.DefaultExt := 'txt';

    if sd.Execute then begin
      activeFilepath := sd.FileName;
      SaveTextFile(sd.filename)
    end;
  finally
    sd.free
  end;
end;

procedure TForm1.ExitFileMenuClick(Sender: TObject);
begin
  close
end;

function TForm1.CheckFileSize(const filename: string): TModalResult;
var
  f: file of byte;
begin
  assignfile(f, filename);
  reset(f);

  { MessageDlg(format('File size: %d', [filesize(f)]), mtInformation, [mbOK], 0); }

  if filesize(f) < 100 * 1024 then begin
    CheckFileSize := mrYes;
    closefile(f);
    exit
  end;

  closefile(f);

  CheckFileSize := MessageDlg('Open this large file (> 100KB)?', mtConfirmation, mbYesNo, 0)
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

  if CheckFileSize(filename) = mrNo then exit;

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

  activeFilepath := filename;
  dirtyEditor := false;
  UpdateCaption
end;

procedure TForm1.SaveTextFile(const filename: string);
begin
  ContentMemo.Lines.SaveToFile(filename);
  dirtyEditor := false;
  UpdateCaption
end;

end.

