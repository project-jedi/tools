object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'FormMain'
  ClientHeight = 533
  ClientWidth = 679
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelLeft: TLabel
    Left = 8
    Top = 20
    Width = 23
    Height = 13
    Caption = '&Left:'
    FocusControl = MemoLeft
  end
  object LabelMiddle: TLabel
    Left = 231
    Top = 20
    Width = 34
    Height = 13
    Caption = '&Middle:'
    FocusControl = MemoMiddle
  end
  object LabelRight: TLabel
    Left = 454
    Top = 20
    Width = 29
    Height = 13
    Caption = '&Right:'
    FocusControl = MemoRight
  end
  object LabelLeftRight: TLabel
    Left = 8
    Top = 314
    Width = 64
    Height = 13
    Caption = '&Left to Right:'
    FocusControl = MemoMerge
  end
  object LabelLeftMiddle: TLabel
    Left = 231
    Top = 314
    Width = 69
    Height = 13
    Caption = 'Left to &Middle:'
    FocusControl = MemoDiffMiddle
  end
  object LabelMiddleRight: TLabel
    Left = 454
    Top = 314
    Width = 75
    Height = 13
    Caption = 'Middle to &Right:'
    FocusControl = MemoDiffRight
  end
  object MemoLeft: TMemo
    Left = 8
    Top = 39
    Width = 217
    Height = 257
    Lines.Strings = (
      'The text begins here'
      'The text continues here'
      'This line 1 will be deleted'
      'This is a text marker'
      'This line 2 will be deleted'
      'This is an other text marker'
      'This line 3 will be deleted'
      'The text is still here'
      'This line 1 will be replaced'
      'This line 2 will be replaced'
      'This line 3 will be replaced'
      'This is the end of the text')
    TabOrder = 0
    WordWrap = False
  end
  object MemoMiddle: TMemo
    Left = 231
    Top = 39
    Width = 217
    Height = 257
    Lines.Strings = (
      'The text begins here'
      'This line 1 is just added'
      'This line 2 is just added and will be replaced'
      'This line 3 is just added and will be deleted'
      'The text continues here'
      'This is a text marker'
      'This is an other text marker'
      'The text is still here'
      'This line 1 is replaced'
      'This line 2 is replaced and will be replaced again'
      'This line 3 is replaced and will be deleted'
      'This is the end of the text')
    TabOrder = 1
    WordWrap = False
  end
  object ButtonDiffMiddle: TButton
    Left = 302
    Top = 302
    Width = 75
    Height = 25
    Caption = 'Diff'
    TabOrder = 2
    OnClick = ButtonDiffMiddleClick
  end
  object MemoMerge: TMemo
    Left = 8
    Top = 333
    Width = 217
    Height = 161
    TabOrder = 3
    WordWrap = False
  end
  object ButtonLoadLeft: TButton
    Left = 79
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load...'
    TabOrder = 4
    OnClick = ButtonLoadLeftClick
  end
  object ButtonLoadMiddle: TButton
    Left = 302
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load...'
    TabOrder = 5
    OnClick = ButtonLoadMiddleClick
  end
  object ButtonSaveMerge: TButton
    Left = 79
    Top = 500
    Width = 75
    Height = 25
    Caption = 'Save...'
    TabOrder = 6
    OnClick = ButtonSaveMergeClick
  end
  object ButtonMergeInPlace: TButton
    Left = 150
    Top = 302
    Width = 75
    Height = 25
    Caption = 'Merge in-place'
    TabOrder = 7
    OnClick = ButtonMergeInPlaceClick
  end
  object ButtonMergeOutOfPlace: TButton
    Left = 104
    Top = 302
    Width = 40
    Height = 25
    Caption = 'Merge'
    TabOrder = 8
    OnClick = ButtonMergeOutOfPlaceClick
  end
  object MemoDiffMiddle: TMemo
    Left = 231
    Top = 333
    Width = 217
    Height = 161
    TabOrder = 9
    WordWrap = False
  end
  object ButtonSaveDiffMiddle: TButton
    Left = 302
    Top = 500
    Width = 75
    Height = 25
    Caption = 'Save...'
    TabOrder = 10
    OnClick = ButtonSaveDiffMiddleClick
  end
  object MemoRight: TMemo
    Left = 454
    Top = 39
    Width = 217
    Height = 257
    Lines.Strings = (
      'The text begins here'
      'This line 0 is just added'
      'This line 1 is just added'
      'This line 2 is replaced'
      'The text continues here'
      'This line was deleted and is added again'
      'This is a text marker'
      'This is an other text marker'
      'The text is still here'
      'This line 0 is added'
      'This line 1 is replaced'
      'This line 2 is replaced again'
      'This is the end of the text')
    TabOrder = 11
    WordWrap = False
  end
  object ButtonLoadRight: TButton
    Left = 525
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load...'
    TabOrder = 12
    OnClick = ButtonLoadRightClick
  end
  object ButtonDiffRight: TButton
    Left = 525
    Top = 302
    Width = 75
    Height = 25
    Caption = 'Diff'
    TabOrder = 13
    OnClick = ButtonDiffRightClick
  end
  object MemoDiffRight: TMemo
    Left = 454
    Top = 333
    Width = 217
    Height = 161
    TabOrder = 14
    WordWrap = False
  end
  object ButtonSaveDiffRight: TButton
    Left = 525
    Top = 500
    Width = 75
    Height = 25
    Caption = 'Save...'
    TabOrder = 15
    OnClick = ButtonSaveDiffRightClick
  end
  object ButtonDiffAll: TButton
    Left = 72
    Top = 302
    Width = 26
    Height = 25
    Caption = 'Diff'
    TabOrder = 16
    OnClick = ButtonDiffAllClick
  end
end
