object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 294
  ClientWidth = 466
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
  object LabelPage: TLabel
    Left = 335
    Top = 73
    Width = 16
    Height = 13
    Caption = '&To:'
    FocusControl = EditToUser
  end
  object ButtonQueryAsync: TButton
    Left = 336
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Async Query'
    TabOrder = 0
    OnClick = ButtonQueryAsyncClick
  end
  object ButtonQuerySync: TButton
    Left = 336
    Top = 39
    Width = 97
    Height = 25
    Caption = 'Sync Query'
    TabOrder = 1
    OnClick = ButtonQuerySyncClick
  end
  object EditToUser: TEdit
    Left = 369
    Top = 70
    Width = 64
    Height = 21
    TabOrder = 2
  end
  object ButtonMergeAsync: TButton
    Left = 336
    Top = 122
    Width = 97
    Height = 25
    Caption = 'Async Merge'
    TabOrder = 3
    OnClick = ButtonMergeAsyncClick
  end
  object ButtonMergeSync: TButton
    Left = 336
    Top = 153
    Width = 97
    Height = 25
    Caption = 'Sync Merge'
    TabOrder = 4
    OnClick = ButtonMergeSyncClick
  end
  object ListBoxUserList: TListBox
    Left = 16
    Top = 8
    Width = 297
    Height = 278
    ItemHeight = 13
    TabOrder = 5
  end
  object CheckBoxDeleteUser: TCheckBox
    Left = 336
    Top = 99
    Width = 97
    Height = 17
    Caption = 'Delete User'
    TabOrder = 6
  end
end
