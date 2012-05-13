object DColumns: TDColumns
  Left = 516
  Top = 351
  HelpContext = 1086
  BorderStyle = bsDialog
  Caption = 'DColumns'
  ClientHeight = 441
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object FBOk: TButton
    Left = 128
    Top = 408
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object FBCancel: TButton
    Left = 216
    Top = 408
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 3
  end
  object GroupBox: TGroupBox_Ext
    Left = 8
    Top = 8
    Width = 283
    Height = 377
    Caption = 'GroupBox'
    TabOrder = 0
    object FLWidth: TLabel
      Left = 8
      Top = 347
      Width = 40
      Height = 13
      Caption = 'FLWidth'
      FocusControl = FWidth
    end
    object FColumns: TListView
      Left = 8
      Top = 16
      Width = 217
      Height = 317
      Checkboxes = True
      Columns = <
        item
          Caption = 'Name'
          Width = 213
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = FColumnsChange
      OnResize = FColumnsResize
      OnSelectItem = FColumnsSelectItem
    end
    object FUDWidth: TUpDown
      Left = 257
      Top = 344
      Width = 15
      Height = 21
      Associate = FWidth
      Min = 10
      Max = 0
      Increment = 10
      Position = 10
      TabOrder = 4
    end
    object FWidth: TEdit
      Left = 224
      Top = 344
      Width = 33
      Height = 21
      TabOrder = 3
      Text = '10'
      OnChange = FWidthChange
    end
    object ToolBar1: TToolBar
      Left = 242
      Top = 24
      Width = 25
      Height = 29
      Align = alNone
      TabOrder = 1
      object tbUp: TToolButton
        Left = 0
        Top = 0
        Caption = 'tbUp'
        ImageIndex = 49
        OnClick = tbUpDownClick
      end
    end
    object ToolBar2: TToolBar
      Left = 242
      Top = 60
      Width = 25
      Height = 29
      Align = alNone
      TabOrder = 2
      object tbDown: TToolButton
        Left = 0
        Top = 0
        Caption = 'tbDown'
        ImageIndex = 50
        OnClick = tbUpDownClick
      end
    end
  end
  object FBHelp: TButton
    Left = 8
    Top = 408
    Width = 75
    Height = 25
    Caption = 'FBHelp'
    TabOrder = 1
    OnClick = FBHelpClick
  end
end
