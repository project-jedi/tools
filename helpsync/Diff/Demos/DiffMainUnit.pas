unit DiffMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormMain = class(TForm)
    MemoLeft: TMemo;
    MemoRight: TMemo;
    ButtonDiff: TButton;
    MemoDiff: TMemo;
    ButtonLoadLeft: TButton;
    ButtonLoadRight: TButton;
    ButtonSaveDiff: TButton;
    LabelLeft: TLabel;
    LabelRight: TLabel;
    LabelDiff: TLabel;
    procedure ButtonDiffClick(Sender: TObject);
    procedure ButtonLoadLeftClick(Sender: TObject);
    procedure ButtonLoadRightClick(Sender: TObject);
    procedure ButtonSaveDiffClick(Sender: TObject);
  private
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  SimpleDiff;

procedure TFormMain.ButtonDiffClick(Sender: TObject);
var
  StringsDiff: TStringsSimpleDiff;
  StringDiff: TStringSimpleDiff;
  I: Integer;
begin
  MemoDiff.Lines.Clear;
  StringsDiff := TStringsSimpleDiff.Create;
  try
    StringsDiff.Diff(MemoLeft.Lines, MemoRight.Lines);
    for I := 0 to StringsDiff.Count - 1 do
    begin
      StringDiff := StringsDiff.StringDiffs[I];
      if IsModify(StringDiff.Flags) then
      begin
        MemoDiff.Lines.Add(Format('(%d)M-%s', [StringDiff.LeftIndex + 1, StringDiff.LeftValue]));
        MemoDiff.Lines.Add(Format('(%d)M+%s', [StringDiff.RightIndex + 1, StringDiff.RightValue]));
      end
      else
      if IsInsert(StringDiff.Flags) then
      begin
        MemoDiff.Lines.Add(Format('(%d)+%s', [StringDiff.RightIndex + 1, StringDiff.RightValue]));
      end
      else
      if IsDelete(StringDiff.Flags) then
      begin
        MemoDiff.Lines.Add(Format('(%d)-%s', [StringDiff.LeftIndex + 1, StringDiff.LeftValue]));
      end;
    end;
  finally
    StringsDiff.Free;
  end;
end;

procedure TFormMain.ButtonLoadLeftClick(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the left file...') then
    MemoLeft.Lines.LoadFromFile(AFileName);
end;

procedure TFormMain.ButtonLoadRightClick(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the right file...') then
    MemoRight.Lines.LoadFromFile(AFileName);
end;

procedure TFormMain.ButtonSaveDiffClick(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the right file...', '', True) then
    MemoDiff.Lines.SaveToFile(AFileName);
end;

end.

