object DDatabases: TDDatabases
  Left = 878
  Top = 561
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DDatabases'
  ClientHeight = 241
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    257
    241)
  PixelsPerInch = 106
  TextHeight = 13
  object PSQLWait: TPanel
    Left = 8
    Top = 8
    Width = 242
    Height = 186
    Cursor = crHourGlass
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'PSQLWait'
    TabOrder = 0
    Visible = False
  end
  object GroupBox: TGroupBox_Ext
    Left = 8
    Top = 8
    Width = 242
    Height = 186
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'GroupBox'
    TabOrder = 1
    DesignSize = (
      242
      186)
    object FDatabases: TListView
      Left = 8
      Top = 16
      Width = 226
      Height = 154
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelKind = bkTile
      BorderStyle = bsNone
      Columns = <
        item
          AutoSize = True
        end>
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = FDatabasesChange
      OnDblClick = FDatabasesDblClick
    end
  end
  object FBOk: TButton
    Left = 85
    Top = 209
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object FBCancel: TButton
    Left = 175
    Top = 209
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 3
  end
end
