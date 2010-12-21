unit MergeMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormMain = class(TForm)
    MemoLeft: TMemo;
    MemoRight: TMemo;
    ButtonDiff: TButton;
    MemoMerge: TMemo;
    ButtonLoadLeft: TButton;
    ButtonLoadRight: TButton;
    ButtonSaveMerge: TButton;
    ButtonMergeInPlace: TButton;
    ButtonMergeOutOfPlace: TButton;
    MemoDiff: TMemo;
    ButtonSaveDiff: TButton;
    procedure ButtonDiffClick(Sender: TObject);
    procedure ButtonLoadLeftClick(Sender: TObject);
    procedure ButtonLoadRightClick(Sender: TObject);
    procedure ButtonSaveMergeClick(Sender: TObject);
    procedure ButtonMergeInPlaceClick(Sender: TObject);
    procedure ButtonMergeOutOfPlaceClick(Sender: TObject);
    procedure ButtonSaveDiffClick(Sender: TObject);
  private
    procedure Diff(MemoRight, MemoDiff: TMemo);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  SimpleDiff;

procedure TFormMain.Diff(MemoRight, MemoDiff: TMemo);
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

procedure TFormMain.ButtonDiffClick(Sender: TObject);
begin
  Diff(MemoRight, MemoDiff);
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

procedure TFormMain.ButtonMergeInPlaceClick(Sender: TObject);
var
  StringsDiff: TStringsSimpleDiff;
begin
  StringsDiff := TStringsSimpleDiff.Create;
  try
    StringsDiff.Diff(MemoLeft.Lines, MemoRight.Lines);

    MemoMerge.Lines.Assign(MemoLeft.Lines);
    StringsDiff.Check(MemoMerge.Lines);
    StringsDiff.Merge(MemoMerge.Lines);
  finally
    StringsDiff.Free;
  end;
end;

procedure TFormMain.ButtonMergeOutOfPlaceClick(Sender: TObject);
var
  StringsDiff: TStringsSimpleDiff;
begin
  StringsDiff := TStringsSimpleDiff.Create;
  try
    StringsDiff.Diff(MemoLeft.Lines, MemoRight.Lines);

    StringsDiff.Check(MemoLeft.Lines);
    StringsDiff.Merge(MemoLeft.Lines, MemoMerge.Lines);
  finally
    StringsDiff.Free;
  end;
end;

procedure TFormMain.ButtonSaveDiffClick(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the diff file...', '', True) then
    MemoDiff.Lines.SaveToFile(AFileName);
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

