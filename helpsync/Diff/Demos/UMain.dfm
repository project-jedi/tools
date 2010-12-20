object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 535
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    467
    535)
  PixelsPerInch = 96
  TextHeight = 13
  object MemoLeft: TMemo
    Left = 8
    Top = 8
    Width = 217
    Height = 257
    Anchors = [akTop]
    Lines.Strings = (
      'The text begins here'
      'This is some other text'
      'Some lines are deleted'
      'Some text continues here'
      'Some text will be replaced'
      'The text is not ended yet'
      'Some text ends here')
    TabOrder = 0
    WordWrap = False
  end
  object MemoRight: TMemo
    Left = 242
    Top = 8
    Width = 217
    Height = 257
    Anchors = [akTop]
    Lines.Strings = (
      'The text begins here'
      'This text is inserted'
      'This is some other text'
      'Some text continues here'
      'Some text replaces the previous one'
      'The text is not ended yet'
      'Some text ends here')
    TabOrder = 1
    WordWrap = False
  end
  object ButtonDiff: TButton
    Left = 200
    Top = 279
    Width = 75
    Height = 25
    Caption = 'Diff'
    TabOrder = 2
    OnClick = ButtonDiffClick
  end
  object MemoDiff: TMemo
    Left = 8
    Top = 312
    Width = 449
    Height = 185
    TabOrder = 3
    WordWrap = False
  end
  object ButtonLoadLeft: TButton
    Left = 8
    Top = 279
    Width = 75
    Height = 25
    Caption = 'Load...'
    TabOrder = 4
    OnClick = ButtonLoadLeftClick
  end
  object ButtonLoadRight: TButton
    Left = 384
    Top = 279
    Width = 75
    Height = 25
    Caption = 'Load...'
    TabOrder = 5
    OnClick = ButtonLoadRightClick
  end
  object ButtonSaveDiff: TButton
    Left = 200
    Top = 503
    Width = 75
    Height = 25
    Caption = 'Save...'
    TabOrder = 6
    OnClick = ButtonSaveDiffClick
  end
end
