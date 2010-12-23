unit MergeBeforeMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SimpleDiff;

type
  TFormMain = class(TForm)
    MemoLeft: TMemo;
    MemoMiddle: TMemo;
    ButtonDiffMiddle: TButton;
    MemoMerge: TMemo;
    ButtonLoadLeft: TButton;
    ButtonLoadMiddle: TButton;
    ButtonSaveMerge: TButton;
    ButtonMergeInPlace: TButton;
    ButtonMergeOutOfPlace: TButton;
    MemoDiffMiddle: TMemo;
    ButtonSaveDiffMiddle: TButton;
    MemoRight: TMemo;
    ButtonLoadRight: TButton;
    ButtonDiffRight: TButton;
    MemoDiffRight: TMemo;
    ButtonSaveDiffRight: TButton;
    ButtonDiffAll: TButton;
    LabelLeft: TLabel;
    LabelMiddle: TLabel;
    LabelRight: TLabel;
    LabelLeftRight: TLabel;
    LabelLeftMiddle: TLabel;
    LabelMiddleRight: TLabel;
    procedure ButtonDiffMiddleClick(Sender: TObject);
    procedure ButtonLoadLeftClick(Sender: TObject);
    procedure ButtonLoadMiddleClick(Sender: TObject);
    procedure ButtonSaveMergeClick(Sender: TObject);
    procedure ButtonMergeInPlaceClick(Sender: TObject);
    procedure ButtonMergeOutOfPlaceClick(Sender: TObject);
    procedure ButtonSaveDiffMiddleClick(Sender: TObject);
    procedure ButtonLoadRightClick(Sender: TObject);
    procedure ButtonDiffRightClick(Sender: TObject);
    procedure ButtonSaveDiffRightClick(Sender: TObject);
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

procedure TFormMain.ButtonDiffMiddleClick(Sender: TObject);
var
  StringsDiff: TStringsSimpleDiff;
begin
  StringsDiff := TStringsSimpleDiff.Create;
  try
    StringsDiff.Diff(MemoLeft.Lines, MemoRight.Lines);
    Diff(StringsDiff, MemoDiffMiddle);
  finally
    StringsDiff.Free;
  end;
end;

procedure TFormMain.ButtonDiffRightClick(Sender: TObject);
var
  StringsDiff: TStringsSimpleDiff;
begin
  StringsDiff := TStringsSimpleDiff.Create;
  try
    StringsDiff.Diff(MemoMiddle.Lines, MemoRight.Lines);
    Diff(StringsDiff, MemoDiffRight);
  finally
    StringsDiff.Free;
  end;
end;

procedure TFormMain.ButtonDiffAllClick(Sender: TObject);
var
  StringsDiff1, StringsDiff2: TStringsSimpleDiff;
begin
  raise Exception.Create('not supported yet');
  StringsDiff1 := TStringsSimpleDiff.Create;
  StringsDiff2 := TStringsSimpleDiff.Create;
  try
    StringsDiff1.Diff(MemoLeft.Lines, MemoMiddle.Lines);
    StringsDiff2.Diff(MemoMiddle.Lines, MemoRight.Lines);

    StringsDiff2.DefaultConflictResolution := crAutomatic;
    //StringsDiff2.Add(StringsDiff1, mpBefore);

    Diff(StringsDiff2, MemoMerge);
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

procedure TFormMain.ButtonLoadMiddleClick(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the right file 1...') then
    MemoMiddle.Lines.LoadFromFile(AFileName);
end;

procedure TFormMain.ButtonLoadRightClick(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the right file 2...') then
    MemoRight.Lines.LoadFromFile(AFileName);
end;

procedure TFormMain.ButtonMergeInPlaceClick(Sender: TObject);
var
  StringsDiff1, StringsDiff2: TStringsSimpleDiff;
begin
  raise Exception.Create('not supported yet');
  StringsDiff1 := TStringsSimpleDiff.Create;
  StringsDiff2 := TStringsSimpleDiff.Create;
  try
    StringsDiff1.Diff(MemoLeft.Lines, MemoMiddle.Lines);
    StringsDiff2.Diff(MemoMiddle.Lines, MemoRight.Lines);

    StringsDiff2.DefaultConflictResolution := crAutomatic;
    //StringsDiff2.Add(StringsDiff1, mpBefore);

    MemoMerge.Lines.Assign(MemoLeft.Lines);
    StringsDiff2.Check(MemoMerge.Lines);
    StringsDiff2.Merge(MemoMerge.Lines);
  finally
    StringsDiff1.Free;
    StringsDiff2.Free;
  end;
end;

procedure TFormMain.ButtonMergeOutOfPlaceClick(Sender: TObject);
var
  StringsDiff1, StringsDiff2: TStringsSimpleDiff;
begin
  raise Exception.Create('not supported yet');
  StringsDiff1 := TStringsSimpleDiff.Create;
  StringsDiff2 := TStringsSimpleDiff.Create;
  try
    StringsDiff1.Diff(MemoLeft.Lines, MemoMiddle.Lines);
    StringsDiff2.Diff(MemoMiddle.Lines, MemoRight.Lines);

    StringsDiff2.DefaultConflictResolution := crAutomatic;
    //StringsDiff2.Add(StringsDiff1, mpBefore);

    StringsDiff2.Check(MemoLeft.Lines);
    StringsDiff2.Merge(MemoLeft.Lines, MemoMerge.Lines);
  finally
    StringsDiff1.Free;
    StringsDiff2.Free;
  end;
end;

procedure TFormMain.ButtonSaveDiffMiddleClick(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the diff file 1...', '', True) then
    MemoDiffMiddle.Lines.SaveToFile(AFileName);
end;

procedure TFormMain.ButtonSaveDiffRightClick(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the diff file 2...', '', True) then
    MemoDiffRight.Lines.SaveToFile(AFileName);
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

