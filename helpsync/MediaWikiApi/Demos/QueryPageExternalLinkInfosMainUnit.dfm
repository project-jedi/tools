object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 294
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelMaxLinks: TLabel
    Left = 335
    Top = 100
    Width = 50
    Height = 13
    Caption = '&Max Links:'
    FocusControl = EditMaxLinks
  end
  object LabelPage: TLabel
    Left = 335
    Top = 73
    Width = 28
    Height = 13
    Caption = '&Page:'
  end
  object LabelStartLink: TLabel
    Left = 335
    Top = 127
    Width = 28
    Height = 13
    Caption = '&Start:'
    FocusControl = EditStartLink
  end
  object MemoResult: TMemo
    Left = 16
    Top = 8
    Width = 297
    Height = 273
    ReadOnly = True
    TabOrder = 0
  end
  object ButtonQueryAsync: TButton
    Left = 336
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Async Query'
    TabOrder = 1
    OnClick = ButtonQueryAsyncClick
  end
  object ButtonQuerySync: TButton
    Left = 336
    Top = 39
    Width = 97
    Height = 25
    Caption = 'Sync Query'
    TabOrder = 2
    OnClick = ButtonQuerySyncClick
  end
  object EditMaxLinks: TEdit
    Left = 400
    Top = 97
    Width = 33
    Height = 21
    TabOrder = 3
    Text = '10'
  end
  object EditPage: TEdit
    Left = 376
    Top = 70
    Width = 57
    Height = 21
    TabOrder = 4
    Text = 'JEDI_Code_Library'
  end
  object EditStartLink: TEdit
    Left = 376
    Top = 124
    Width = 57
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnFace
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 5
  end
end
