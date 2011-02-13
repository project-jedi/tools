object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 331
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
  object LabelToken: TLabel
    Left = 334
    Top = 100
    Width = 33
    Height = 13
    Caption = '&Token:'
  end
  object LabelPage: TLabel
    Left = 334
    Top = 73
    Width = 28
    Height = 13
    Caption = '&Page:'
    FocusControl = EditPage
  end
  object LabelReason: TLabel
    Left = 335
    Top = 243
    Width = 40
    Height = 13
    Caption = '&Reason:'
    FocusControl = EditReason
  end
  object LabelRevision: TLabel
    Left = 334
    Top = 189
    Width = 23
    Height = 13
    Caption = '&Rev:'
    FocusControl = EditRevision
  end
  object LabelNextRevision: TLabel
    Left = 334
    Top = 216
    Width = 27
    Height = 13
    Caption = '&Next:'
    FocusControl = EditNextRevision
  end
  object MemoResult: TMemo
    Left = 8
    Top = 8
    Width = 320
    Height = 315
    TabOrder = 0
  end
  object ButtonQueryPageAsync: TButton
    Left = 336
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Async Page Query'
    TabOrder = 1
    OnClick = ButtonQueryPageAsyncClick
  end
  object ButtonQueryPageSync: TButton
    Left = 336
    Top = 39
    Width = 97
    Height = 25
    Caption = 'Sync Page Query'
    TabOrder = 2
    OnClick = ButtonQueryPageSyncClick
  end
  object EditToken: TEdit
    Left = 375
    Top = 97
    Width = 57
    Height = 21
    TabOrder = 3
  end
  object EditPage: TEdit
    Left = 368
    Top = 70
    Width = 64
    Height = 21
    TabOrder = 4
  end
  object EditReason: TEdit
    Left = 376
    Top = 240
    Width = 57
    Height = 21
    TabOrder = 5
    Text = 'Some reason'
  end
  object ButtonDeleteRevisionAsync: TButton
    Left = 335
    Top = 267
    Width = 97
    Height = 25
    Caption = 'Async Delete'
    TabOrder = 6
    OnClick = ButtonDeleteRevisionAsyncClick
  end
  object ButtonDeleteRevisionSync: TButton
    Left = 335
    Top = 298
    Width = 97
    Height = 25
    Caption = 'Sync Delete'
    TabOrder = 7
    OnClick = ButtonDeleteRevisionSyncClick
  end
  object EditRevision: TEdit
    Left = 375
    Top = 186
    Width = 57
    Height = 21
    TabOrder = 8
  end
  object EditNextRevision: TEdit
    Left = 375
    Top = 213
    Width = 57
    Height = 21
    ReadOnly = True
    TabOrder = 9
  end
  object ButtonQueryRevisionAsync: TButton
    Left = 336
    Top = 124
    Width = 97
    Height = 25
    Caption = 'Async Rev Query'
    TabOrder = 10
    OnClick = ButtonQueryRevisionAsyncClick
  end
  object ButtonQueryRevisionSync: TButton
    Left = 336
    Top = 155
    Width = 97
    Height = 25
    Caption = 'Sync Rev Query'
    TabOrder = 11
    OnClick = ButtonQueryRevisionSyncClick
  end
end
