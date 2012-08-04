object DHost: TDHost
  Left = 836
  Top = 285
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DHost'
  ClientHeight = 294
  ClientWidth = 337
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
  DesignSize = (
    337
    294)
  PixelsPerInch = 106
  TextHeight = 13
  object PSQLWait: TPanel
    Left = 8
    Top = 8
    Width = 321
    Height = 241
    Cursor = crHourGlass
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'PSQLWait'
    TabOrder = 0
    Visible = False
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 321
    Height = 241
    ActivePage = TSSource
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object TSBasics: TTabSheet
      Caption = 'TSBasics'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        213)
      object GBasics: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 297
        Height = 82
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GBasics'
        TabOrder = 0
        DesignSize = (
          297
          82)
        object FAll: TRadioButton
          Left = 8
          Top = 22
          Width = 273
          Height = 17
          Caption = 'FAll'
          TabOrder = 0
          OnClick = FBOkCheckEnabled
        end
        object FHost: TRadioButton
          Left = 8
          Top = 50
          Width = 73
          Height = 17
          Caption = 'FHost'
          TabOrder = 1
          OnClick = FBOkCheckEnabled
        end
        object FHosts: TEdit
          Left = 80
          Top = 48
          Width = 205
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 60
          TabOrder = 2
          Text = 'FHosts'
          OnChange = FHostsChange
        end
      end
    end
    object TSDatabases: TTabSheet
      Caption = 'TSDatabases'
      OnShow = TSDatabasesShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        213)
      object FDatabases: TListView
        Left = 8
        Top = 8
        Width = 189
        Height = 187
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            AutoSize = True
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = FDatabasesDblClick
        OnSelectItem = FDatabasesSelectItem
      end
      object FBNew: TButton
        Left = 208
        Top = 8
        Width = 95
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'FBNew'
        TabOrder = 1
        OnClick = FBNewClick
      end
      object FBDelete: TButton
        Left = 208
        Top = 40
        Width = 95
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'FBDelete'
        TabOrder = 2
        OnClick = FBDeleteClick
      end
      object FBEdit: TButton
        Left = 208
        Top = 72
        Width = 95
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'FBEdit'
        TabOrder = 3
        OnClick = FBEditClick
      end
    end
    object TSSource: TTabSheet
      Caption = 'TSSource'
      OnShow = TSSourceShow
      DesignSize = (
        313
        213)
      object FSource: TSynMemo
        Left = 8
        Top = 8
        Width = 297
        Height = 187
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = MSource
        TabOrder = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 0
        MaxScrollWidth = 65535
        Options = [eoAutoIndent, eoGroupUndo, eoHideShowScrollbars, eoNoCaret, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
        ReadOnly = True
        RightEdge = 2000
        RightEdgeColor = clWindow
        ScrollHintFormat = shfTopToBottom
        WantReturns = False
      end
    end
  end
  object FBHelp: TButton
    Left = 8
    Top = 260
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'FBHelp'
    TabOrder = 2
    OnClick = FBHelpClick
  end
  object FBOk: TButton
    Left = 167
    Top = 260
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object FBCancel: TButton
    Left = 255
    Top = 260
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 4
  end
  object MSource: TPopupMenu
    Left = 88
    Top = 256
    object msCopy: TMenuItem
      Caption = 'aECopy'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object msSelectAll: TMenuItem
      Caption = 'aESelectAll'
    end
  end
end
