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
  object LabelSummary: TLabel
    Left = 335
    Top = 100
    Width = 48
    Height = 13
    Caption = '&Summary:'
    FocusControl = EditSummary
  end
  object LabelEditToken: TLabel
    Left = 335
    Top = 127
    Width = 33
    Height = 13
    Caption = '&Token:'
  end
  object LabelPage: TLabel
    Left = 335
    Top = 73
    Width = 28
    Height = 13
    Caption = '&Page:'
  end
  object LabelRevID: TLabel
    Left = 335
    Top = 154
    Width = 34
    Height = 13
    Caption = '&RevID:'
    FocusControl = EditRevID
  end
  object MemoResult: TMemo
    Left = 16
    Top = 8
    Width = 297
    Height = 273
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
  object EditSummary: TEdit
    Left = 389
    Top = 97
    Width = 44
    Height = 21
    TabOrder = 3
    Text = 'Test edit'
  end
  object EditToken: TEdit
    Left = 376
    Top = 124
    Width = 57
    Height = 21
    TabOrder = 4
  end
  object EditPage: TEdit
    Left = 369
    Top = 70
    Width = 64
    Height = 21
    TabOrder = 5
  end
  object EditRevID: TEdit
    Left = 376
    Top = 151
    Width = 57
    Height = 21
    TabOrder = 6
  end
  object ButtonEditAsync: TButton
    Left = 335
    Top = 178
    Width = 97
    Height = 25
    Caption = 'Async Edit'
    TabOrder = 7
    OnClick = ButtonEditAsyncClick
  end
  object ButtonEditSync: TButton
    Left = 335
    Top = 209
    Width = 97
    Height = 25
    Caption = 'Sync Edit'
    TabOrder = 8
    OnClick = ButtonEditSyncClick
  end
end
