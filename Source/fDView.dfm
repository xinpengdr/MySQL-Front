object DView: TDView
  Left = 720
  Top = 154
  HelpContext = 1098
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DView'
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
        Top = 4
        Width = 297
        Height = 277
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'GBasics'
        TabOrder = 0
        DesignSize = (
          297
          277)
        object FLAlgorithm: TLabel
          Left = 8
          Top = 67
          Width = 55
          Height = 13
          Caption = 'FLAlgorithm'
          FocusControl = FAlgorithm
        end
        object FLCheckOption: TLabel
          Left = 8
          Top = 145
          Width = 74
          Height = 13
          Caption = 'FLCheckOption'
        end
        object FLStmt: TLabel
          Left = 8
          Top = 208
          Width = 33
          Height = 13
          Caption = 'FLStmt'
          FocusControl = FStmt
        end
        object FLName: TLabel
          Left = 8
          Top = 27
          Width = 40
          Height = 13
          Caption = 'FLName'
        end
        object FLSecurity: TLabel
          Left = 8
          Top = 97
          Width = 50
          Height = 13
          Caption = 'FLSecurity'
        end
        object FAlgorithm: TComboBox
          Left = 120
          Top = 64
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 1
          OnSelect = FAlgorithmSelect
        end
        object FStmt: TSynMemo
          Left = 8
          Top = 224
          Width = 281
          Height = 45
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          PopupMenu = MSource
          TabOrder = 7
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Width = 0
          Options = [eoAutoIndent, eoDragDropEditing, eoGroupUndo, eoHideShowScrollbars, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
          RightEdge = 0
          RightEdgeColor = clWindow
          ScrollHintFormat = shfTopToBottom
          OnChange = FStmtChange
        end
        object FCheckOptionCascade: TCheckBox
          Left = 136
          Top = 164
          Width = 153
          Height = 17
          Caption = 'FCheckOptionCascade'
          TabOrder = 5
          OnClick = FCheckOptionCascadeClick
          OnKeyPress = FCheckOptionCascadeKeyPress
        end
        object FCheckOptionLocal: TCheckBox
          Left = 136
          Top = 184
          Width = 153
          Height = 17
          Caption = 'FCheckOptionLocal'
          TabOrder = 6
          OnClick = FCheckOptionLocalClick
          OnKeyPress = FCheckOptionLocalKeyPress
        end
        object FCheckOption: TCheckBox
          Left = 120
          Top = 142
          Width = 169
          Height = 17
          Caption = 'FCheckOption'
          TabOrder = 4
          OnClick = FCheckOptionClick
          OnKeyPress = FCheckOptionKeyPress
        end
        object FName: TEdit
          Left = 120
          Top = 24
          Width = 145
          Height = 21
          TabOrder = 0
          Text = 'FName'
          OnChange = FNameChange
        end
        object FSecurityDefiner: TRadioButton
          Left = 120
          Top = 96
          Width = 169
          Height = 17
          Caption = 'FSecurityDefiner'
          TabOrder = 2
          OnClick = FSecurityClick
          OnKeyPress = FSecurityKeyPress
        end
        object FSecurityInvoker: TRadioButton
          Left = 120
          Top = 116
          Width = 169
          Height = 17
          Caption = 'FSecurityInvoker'
          TabOrder = 3
          OnClick = FSecurityClick
          OnKeyPress = FSecurityKeyPress
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
        297)
      object GDefiner: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 297
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GDefiner'
        TabOrder = 0
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
      object GRecordCount: TGroupBox_Ext
        Left = 8
        Top = 64
        Width = 297
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GRecordCount'
        TabOrder = 1
        DesignSize = (
          297
          49)
        object FLRecordCount: TLabel
          Left = 8
          Top = 20
          Width = 75
          Height = 13
          Caption = 'FLRecordCount'
        end
        object FRecordCount: TLabel
          Left = 220
          Top = 20
          Width = 69
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FRecordCount'
        end
      end
    end
    object TSFields: TTabSheet
      Caption = 'TSFields'
      object FFields: TListView
        Left = 8
        Top = 8
        Width = 297
        Height = 273
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end
          item
            AutoSize = True
            Caption = 'Type'
          end
          item
            AutoSize = True
            Caption = 'NULL'
          end
          item
            AutoSize = True
            Caption = 'Default'
          end
          item
            AutoSize = True
            Caption = 'Extras'
          end
          item
            AutoSize = True
            Caption = 'Comment'
          end>
        ColumnClick = False
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
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
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 0
        Options = [eoAutoIndent, eoGroupUndo, eoHideShowScrollbars, eoNoCaret, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
        ReadOnly = True
        RightEdge = 0
        RightEdgeColor = clWindow
        ScrollHintFormat = shfTopToBottom
        WantReturns = False
        OnChange = FSourceChange
        OnStatusChange = FSourceStatusChange
      end
    end
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
