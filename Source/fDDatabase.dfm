object DDatabase: TDDatabase
  Left = 839
  Top = 666
  HelpContext = 1044
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DDatabase'
  ClientHeight = 248
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
    248)
  PixelsPerInch = 106
  TextHeight = 13
  object FBOk: TButton
    Left = 167
    Top = 215
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object FBCancel: TButton
    Left = 255
    Top = 215
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 3
  end
  object FBHelp: TButton
    Left = 8
    Top = 215
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'FBHelp'
    TabOrder = 0
    OnClick = FBHelpClick
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 321
    Height = 196
    ActivePage = TSBasics
    Anchors = [akLeft, akTop, akRight, akBottom]
    HotTrack = True
    TabOrder = 2
    object TSBasics: TTabSheet
      Caption = 'TSBasics'
      DesignSize = (
        313
        168)
      object GBasics: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 297
        Height = 114
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GBasics'
        TabOrder = 0
        DesignSize = (
          297
          114)
        object FLName: TLabel
          Left = 8
          Top = 23
          Width = 40
          Height = 13
          Caption = 'FLName'
          FocusControl = FName
        end
        object FLDefaultCharset: TLabel
          Left = 8
          Top = 59
          Width = 82
          Height = 13
          Caption = 'FLDefaultCharset'
          FocusControl = FDefaultCharset
        end
        object FLCollation: TLabel
          Left = 8
          Top = 85
          Width = 52
          Height = 13
          Caption = 'FLCollation'
          FocusControl = FCollation
        end
        object FName: TEdit
          Left = 120
          Top = 20
          Width = 145
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 64
          TabOrder = 0
          Text = 'FName'
          OnChange = FNameChange
        end
        object FDefaultCharset: TComboBox_Ext
          Left = 120
          Top = 56
          Width = 89
          Height = 21
          Style = csDropDownList
          TabOrder = 1
          OnChange = FDefaultCharsetChange
          OnExit = FDefaultCharsetExit
        end
        object FCollation: TComboBox_Ext
          Left = 120
          Top = 82
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 2
          OnChange = FCollationChange
          OnDropDown = FCollationDropDown
        end
      end
    end
    object TSInformations: TTabSheet
      Caption = 'TSInformations'
      OnShow = TSInformationsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        168)
      object GDates: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 297
        Height = 73
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GDates'
        TabOrder = 0
        DesignSize = (
          297
          73)
        object FLCreated: TLabel
          Left = 8
          Top = 20
          Width = 49
          Height = 13
          Caption = 'FLCreated'
        end
        object FLUpdated: TLabel
          Left = 8
          Top = 44
          Width = 53
          Height = 13
          Caption = 'FLUpdated'
        end
        object FCreated: TLabel
          Left = 246
          Top = 20
          Width = 43
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FCreated'
        end
        object FUpdated: TLabel
          Left = 242
          Top = 44
          Width = 47
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FUpdated'
        end
      end
      object GSize: TGroupBox_Ext
        Left = 8
        Top = 88
        Width = 297
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GSize'
        TabOrder = 1
        DesignSize = (
          297
          49)
        object FLSize: TLabel
          Left = 8
          Top = 20
          Width = 32
          Height = 13
          Caption = 'FLSize'
        end
        object FSize: TLabel
          Left = 263
          Top = 20
          Width = 26
          Height = 13
          Alignment = taRightJustify
          Anchors = [akLeft, akTop, akRight]
          Caption = 'FSize'
        end
      end
    end
    object TSSource: TTabSheet
      Caption = 'TSSource'
      OnShow = TSSourceShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        168)
      object FSource: TSynMemo
        Left = 8
        Top = 8
        Width = 297
        Height = 141
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
        RightEdge = 0
        RightEdgeColor = clWindow
        ScrollHintFormat = shfTopToBottom
        WantReturns = False
        OnChange = FSourceChange
      end
    end
  end
  object MSource: TPopupMenu
    Left = 88
    Top = 208
    object msCopy: TMenuItem
      Caption = 'msCopy'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object msSelectAll: TMenuItem
      Caption = 'aESelectAll'
    end
  end
end
