object DRoutine: TDRoutine
  Left = 836
  Top = 189
  HelpContext = 1099
  BorderIcons = [biSystemMenu]
  Caption = 'DRoutine'
  ClientHeight = 377
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
    377)
  PixelsPerInch = 106
  TextHeight = 13
  object PSQLWait: TPanel
    Left = 8
    Top = 8
    Width = 321
    Height = 325
    Cursor = crSQLWait
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'PSQLWait'
    TabOrder = 0
    Visible = False
  end
  object FBOk: TButton
    Left = 167
    Top = 344
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
    Top = 344
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 4
  end
  object FBHelp: TButton
    Left = 8
    Top = 344
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'FBHelp'
    TabOrder = 2
    OnClick = FBHelpClick
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 321
    Height = 325
    ActivePage = TSBasics
    Anchors = [akLeft, akTop, akRight, akBottom]
    HotTrack = True
    TabOrder = 1
    object TSBasics: TTabSheet
      Caption = 'TSBasics'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        297)
      object GBasics: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 297
        Height = 153
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GBasics'
        TabOrder = 0
        DesignSize = (
          297
          153)
        object FLName: TLabel
          Left = 8
          Top = 23
          Width = 40
          Height = 13
          Caption = 'FLName'
          FocusControl = FName
        end
        object FLComment: TLabel
          Left = 8
          Top = 115
          Width = 56
          Height = 13
          Caption = 'FLComment'
          FocusControl = FComment
        end
        object FLSecurity: TLabel
          Left = 8
          Top = 57
          Width = 50
          Height = 13
          Caption = 'FLSecurity'
        end
        object FName: TEdit
          Left = 120
          Top = 20
          Width = 145
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Enabled = False
          MaxLength = 64
          TabOrder = 0
          Text = 'FName'
          OnChange = FNameChange
        end
        object FComment: TEdit
          Left = 120
          Top = 112
          Width = 165
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
          Text = 'FComment'
          OnChange = FCommentChange
        end
        object FSecurityDefiner: TRadioButton
          Left = 120
          Top = 56
          Width = 169
          Height = 17
          Caption = 'FSecurityDefiner'
          TabOrder = 1
          OnClick = FSecurityClick
          OnKeyPress = FSecurityKeyPress
        end
        object FSecurityInvoker: TRadioButton
          Left = 120
          Top = 80
          Width = 169
          Height = 17
          Caption = 'FSecurityInvoker'
          TabOrder = 2
          OnClick = FSecurityClick
          OnKeyPress = FSecurityKeyPress
        end
      end
    end
    object TSInformations: TTabSheet
      Caption = 'TSInformations'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        297)
      object GDates: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 297
        Height = 75
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GDates'
        TabOrder = 0
        DesignSize = (
          297
          75)
        object FLCreated: TLabel
          Left = 8
          Top = 20
          Width = 49
          Height = 13
          Caption = 'FLCreated'
        end
        object FCreated: TLabel
          Left = 245
          Top = 20
          Width = 43
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FCreated'
        end
        object FLUpdated: TLabel
          Left = 8
          Top = 48
          Width = 53
          Height = 13
          Caption = 'FLUpdated'
        end
        object FUpdated: TLabel
          Left = 242
          Top = 48
          Width = 47
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FUpdated'
        end
      end
      object GDefiner: TGroupBox_Ext
        Left = 8
        Top = 92
        Width = 297
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GDefiner'
        TabOrder = 1
        DesignSize = (
          297
          49)
        object FLDefiner: TLabel
          Left = 8
          Top = 20
          Width = 46
          Height = 13
          Caption = 'FLDefiner'
        end
        object FDefiner: TLabel
          Left = 249
          Top = 20
          Width = 40
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FDefiner'
        end
      end
      object GSize: TGroupBox_Ext
        Left = 8
        Top = 152
        Width = 297
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GSize'
        TabOrder = 2
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
          Anchors = [akTop, akRight]
          Caption = 'FSize'
        end
      end
    end
    object TSSource: TTabSheet
      Caption = 'TSSource'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        297)
      object FSource: TSynMemo
        Left = 8
        Top = 8
        Width = 297
        Height = 273
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = MSource
        TabOrder = 0
        Gutter.AutoSize = True
        Gutter.DigitCount = 2
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 2
        Gutter.ShowLineNumbers = True
        MaxScrollWidth = 65535
        Options = [eoDragDropEditing, eoGroupUndo, eoHideShowScrollbars, eoShowScrollHint, eoSmartTabDelete, eoTabIndent, eoTabsToSpaces]
        ScrollHintFormat = shfTopToBottom
        OnChange = FSourceChange
      end
    end
  end
  object MSource: TPopupMenu
    Left = 96
    Top = 336
    object msUndo: TMenuItem
      Caption = 'aEUndo'
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object msCut: TMenuItem
      Caption = 'aECut'
    end
    object msCopy: TMenuItem
      Caption = 'aECopy'
    end
    object msPaste: TMenuItem
      Caption = 'aEPaste'
    end
    object msDelete: TMenuItem
      Caption = 'aEDelete'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object msSelectAll: TMenuItem
      Caption = 'aESelectAll'
    end
  end
end
