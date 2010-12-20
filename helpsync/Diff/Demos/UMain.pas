unit UMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    MemoLeft: TMemo;
    MemoRight: TMemo;
    ButtonDiff: TButton;
    MemoDiff: TMemo;
    ButtonLoadLeft: TButton;
    ButtonLoadRight: TButton;
    ButtonSaveDiff: TButton;
    procedure ButtonDiffClick(Sender: TObject);
    procedure ButtonLoadLeftClick(Sender: TObject);
    procedure ButtonLoadRightClick(Sender: TObject);
    procedure ButtonSaveDiffClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  SimpleDiff;

procedure TForm1.ButtonDiffClick(Sender: TObject);
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
      case StringDiff.Kind of
        dkModify:
          begin
            MemoDiff.Lines.Add(Format('(%d)M-%s', [StringDiff.LeftIndex + 1, StringDiff.LeftValue]));
            MemoDiff.Lines.Add(Format('(%d)M+%s', [StringDiff.RightIndex + 1, StringDiff.RightValue]));
          end;
        dkInsert:
          begin
            MemoDiff.Lines.Add(Format('(%d)+%s', [StringDiff.RightIndex + 1, StringDiff.RightValue]));
          end;
        dkDelete:
          begin
            MemoDiff.Lines.Add(Format('(%d)-%s', [StringDiff.LeftIndex + 1, StringDiff.LeftValue]));
          end;
      end;
    end;
  finally
    StringsDiff.Free;
  end;
end;

procedure TForm1.ButtonLoadLeftClick(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the left file...') then
    MemoLeft.Lines.LoadFromFile(AFileName);
end;

procedure TForm1.ButtonLoadRightClick(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the right file...') then
    MemoRight.Lines.LoadFromFile(AFileName);
end;

procedure TForm1.ButtonSaveDiffClick(Sender: TObject);
var
  AFileName: string;
begin
  AFileName := '';
  if PromptForFileName(AFileName, '', '', 'Choose the right file...', '', True) then
    MemoDiff.Lines.SaveToFile(AFileName);
end;

end.

