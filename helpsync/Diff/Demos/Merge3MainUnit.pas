unit Merge3MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SimpleDiff;

type
  TFormMain = class(TForm)
    MemoLeft: TMemo;
    MemoRight1: TMemo;
    ButtonDiff1: TButton;
    MemoMerge: TMemo;
    ButtonLoadLeft: TButton;
    ButtonLoadRight1: TButton;
    ButtonSaveMerge: TButton;
    ButtonMergeInPlace: TButton;
    ButtonMergeOutOfPlace: TButton;
    MemoDiff1: TMemo;
    ButtonSaveDiff1: TButton;
    MemoRight2: TMemo;
    ButtonLoadRight2: TButton;
    ButtonDiff2: TButton;
    MemoDiff2: TMemo;
    ButtonSaveDiff2: TButton;
    ButtonDiffAll: TButton;
    procedure ButtonDiff1Click(Sender: TObject);
    procedure ButtonLoadLeftClick(Sender: TObject);
    procedure ButtonLoadRight1Click(Sender: TObject);
    procedure ButtonSaveMergeClick(Sender: TObject);
    procedure ButtonMergeInPlaceClick(Sender: TObject);
    procedure ButtonMergeOutOfPlaceClick(Sender: TObject);
    procedure ButtonSaveDiff1Click(Sender: TObject);
    procedure ButtonLoadRight2Click(Sender: TObject);
    procedure ButtonDiff2Click(Sender: TObject);
    procedure ButtonSaveDiff2Click(Sender: TObject);
    procedure ButtonDiffAllClick(Sender: TObject);
  private
    procedure Diff(StringsDiff: TStringsSimpleDiff; MemoDiff: TMemo);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.Diff(StringsDiff: TStringsSimpleDiff; MemoDiff: TMemo);
var
  StringDiff: TStringSimpleDiff;
  I: Integer;
begin
  MemoDiff.Lines.Clear;
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
      MemoDiff.Lines.Add(Format('(%d-%d)+%s', [StringDiff.LeftIndex + 1, StringDiff.RightIndex + 1, StringDiff.RightValue]));
    end
    else
    if IsDelete(StringDiff.Flags) then
    begin
      MemoDiff.Lines.Add(Format('(%d-%d)-%s', [StringDiff.LeftIndex + 1, StringDiff.RightIndex + 1, StringDiff.LeftValue]));
    end;
  end;
end;

procedure TFormMain.ButtonDiff1Click(Sender: TObject);
var
  StringsDiff: TStringsSimpleDiff;
begin
  StringsDiff := TStringsSimpleDiff.Create;
  try
    StringsDiff.Diff(MemoLeft.Lines, MemoRight1.Lines);
    Diff(StringsDiff, MemoDiff1);
  finally
    StringsDiff.Free;
  end;
end;

procedure TFormMain.ButtonDiff2Click(Sender: TObject);
var
  StringsDiff: TStringsSimpleDiff;
begin
  StringsDiff := TStringsSimpleDiff.Create;
  try
    StringsDiff.Diff(MemoLeft.Lines, MemoRight2.Lines);
    Diff(StringsDiff, MemoDiff2);
  finally
    StringsDiff.Free;
  end;
end;

procedure TFormMain.ButtonDiffAllClick(Sender: TObject);
var
  StringsDiff1, StringsDiff2: TStringsSimpleDiff;
begin
  StringsDiff1 := TStringsSimpleDiff.Create;
  StringsDiff2 := TStringsSimpleDiff.Create;
  try
    StringsDiff1.Diff(MemoLeft.Lines, MemoRight1.Lines);
    StringsDiff2.Diff(MemoLeft.Lines, MemoRight2.Lines);

    StringsDiff1.DefaultConflictResolution := crAutomatic;
    StringsDiff1.Add(StringsDiff2, mpSame);

    Diff(StringsDiff1, MemoMerge);
  finally
    StringsDiff1.Free;
    StringsDiff2.Free;
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

procedure TFormMain.ButtonLoadRight1Click(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the right file 1...') then
    MemoRight1.Lines.LoadFromFile(AFileName);
end;

procedure TFormMain.ButtonLoadRight2Click(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the right file 2...') then
    MemoRight2.Lines.LoadFromFile(AFileName);
end;

procedure TFormMain.ButtonMergeInPlaceClick(Sender: TObject);
var
  StringsDiff1, StringsDiff2: TStringsSimpleDiff;
begin
  StringsDiff1 := TStringsSimpleDiff.Create;
  StringsDiff2 := TStringsSimpleDiff.Create;
  try
    StringsDiff1.Diff(MemoLeft.Lines, MemoRight1.Lines);
    StringsDiff2.Diff(MemoLeft.Lines, MemoRight2.Lines);

    StringsDiff1.DefaultConflictResolution := crAutomatic;
    StringsDiff1.Add(StringsDiff2, mpSame);

    MemoMerge.Lines.Assign(MemoLeft.Lines);
    StringsDiff1.Check(MemoMerge.Lines);
    StringsDiff1.Merge(MemoMerge.Lines);
  finally
    StringsDiff1.Free;
    StringsDiff2.Free;
  end;
end;

procedure TFormMain.ButtonMergeOutOfPlaceClick(Sender: TObject);
var
  StringsDiff1, StringsDiff2: TStringsSimpleDiff;
begin
  StringsDiff1 := TStringsSimpleDiff.Create;
  StringsDiff2 := TStringsSimpleDiff.Create;
  try
    StringsDiff1.Diff(MemoLeft.Lines, MemoRight1.Lines);
    StringsDiff2.Diff(MemoLeft.Lines, MemoRight2.Lines);

    StringsDiff1.DefaultConflictResolution := crAutomatic;
    StringsDiff1.Add(StringsDiff2, mpSame);

    StringsDiff1.Check(MemoLeft.Lines);
    StringsDiff1.Merge(MemoLeft.Lines, MemoMerge.Lines);
  finally
    StringsDiff1.Free;
    StringsDiff2.Free;
  end;
end;

procedure TFormMain.ButtonSaveDiff1Click(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the diff file 1...', '', True) then
    MemoDiff1.Lines.SaveToFile(AFileName);
end;

procedure TFormMain.ButtonSaveDiff2Click(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the diff file 2...', '', True) then
    MemoDiff2.Lines.SaveToFile(AFileName);
end;

procedure TFormMain.ButtonSaveMergeClick(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the merged file...', '', True) then
    MemoMerge.Lines.SaveToFile(AFileName);
end;

end.

