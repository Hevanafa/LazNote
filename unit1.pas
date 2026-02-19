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
    Separator1: TMenuItem;
    Separator3: TMenuItem;
    ExitFileMenu: TMenuItem;
    SaveAsFileMenu: TMenuItem;
    SaveFileMenu: TMenuItem;
    NewFileMenu: TMenuItem;
    OpenFileMenu: TMenuItem;
    Separator2: TMenuItem;

    EditMenu: TMenuItem;
    UndoEditMenu: TMenuItem;
    CutEditMenu: TMenuItem;
    CopyEditMenu: TMenuItem;
    PasteEditMenu: TMenuItem;
    DeleteEditMenu: TMenuItem;
    TimeDateEditMenu: TMenuItem;

    FormatMenu: TMenuItem;
    ViewMenu: TMenuItem;
    StatusBarViewMenu: TMenuItem;
    HelpMenu: TMenuItem;

    procedure ContentMemoChange(Sender: TObject);
    procedure ContentMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ContentMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ContentMemoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);

    procedure NewFileMenuClick(Sender: TObject);
    procedure OpenFileMenuClick(Sender: TObject);
    procedure SaveFileMenuClick(Sender: TObject);
    procedure SaveAsFileMenuClick(Sender: TObject);
    procedure ExitFileMenuClick(Sender: TObject);

    procedure CopyEditMenuClick(Sender: TObject);
    procedure CutEditMenuClick(Sender: TObject);
    procedure DeleteEditMenuClick(Sender: TObject);
    procedure UndoEditMenuClick(Sender: TObject);
    procedure PasteEditMenuClick(Sender: TObject);

    procedure StatusBarViewMenuClick(Sender: TObject);

  private
    dirtyEditor: boolean;
    activeFilepath: string;

    procedure LoadTextFile(const filename: string);
    procedure SaveTextFile(const filename: string);
    function CheckFileSize(const filename: string): TModalResult;
    function CheckDirtyEditor: TModalResult;
    procedure UpdateCaption;
    procedure UpdatePositionText;

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

procedure TForm1.ContentMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  UpdatePositionText
end;

procedure TForm1.ContentMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  UpdatePositionText
end;

procedure TForm1.UpdatePositionText;
begin
  MainStatusBar.Panels[1].Text := format('Char %d', [ContentMemo.SelStart])
end;

procedure TForm1.ContentMemoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdatePositionText
end;

procedure TForm1.CopyEditMenuClick(Sender: TObject);
begin
  ContentMemo.CopyToClipboard
end;

procedure TForm1.CutEditMenuClick(Sender: TObject);
begin
  ContentMemo.CutToClipboard
end;

procedure TForm1.DeleteEditMenuClick(Sender: TObject);
begin
  ContentMemo.SelText := ''
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

procedure TForm1.PasteEditMenuClick(Sender: TObject);
begin
  ContentMemo.PasteFromClipboard
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

procedure TForm1.StatusBarViewMenuClick(Sender: TObject);
begin
  MainStatusBar.Visible := not MainStatusBar.Visible;
  StatusBarViewMenu.Checked := MainStatusBar.Visible;
end;

procedure TForm1.UndoEditMenuClick(Sender: TObject);
begin
  ContentMemo.Undo
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

