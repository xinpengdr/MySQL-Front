object DSearch: TDSearch
  Left = 901
  Top = 143
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DSearch'
  ClientHeight = 331
  ClientWidth = 341
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
    341
    331)
  PixelsPerInch = 106
  TextHeight = 13
  object FBBack: TButton
    Left = 96
    Top = 296
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBBack'
    TabOrder = 2
    OnClick = FBBackClick
  end
  object FBForward: TButton
    Left = 171
    Top = 296
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBForward'
    Default = True
    TabOrder = 3
    OnClick = FBForwardClick
  end
  object FBCancel: TButton
    Left = 257
    Top = 296
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 4
    OnClick = FBCancelClick
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 341
    Height = 289
    ActivePage = TSExecute
    Anchors = [akLeft, akTop, akRight, akBottom]
    Style = tsButtons
    TabOrder = 0
    TabStop = False
    object TSROptions: TTabSheet
      Caption = 'TSROptions'
      ImageIndex = 1
      TabVisible = False
      OnShow = TSROptionsShow
      DesignSize = (
        333
        279)
      object GRWhat: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GRWhat'
        TabOrder = 0
        DesignSize = (
          325
          81)
        object FLRFindText: TLabel
          Left = 8
          Top = 19
          Width = 61
          Height = 13
          Caption = 'FLRFindText'
          FocusControl = FRFindText
        end
        object FLReplaceText: TLabel
          Left = 8
          Top = 51
          Width = 73
          Height = 13
          Caption = 'FLReplaceText'
          FocusControl = FReplaceText
        end
        object FRFindText: TComboBox_Ext
          Left = 128
          Top = 16
          Width = 189
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'FRFindText'
          OnChange = FRFindTextChange
        end
        object FReplaceText: TComboBox_Ext
          Left = 128
          Top = 48
          Width = 189
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          Text = 'FReplaceText'
          OnChange = FRFindTextChange
        end
      end
      object GROptions: TGroupBox_Ext
        Left = 4
        Top = 88
        Width = 325
        Height = 91
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GROptions'
        TabOrder = 1
        DesignSize = (
          325
          91)
        object FLRSearchOptions: TLabel
          Left = 8
          Top = 17
          Width = 90
          Height = 13
          Caption = 'FLRSearchOptions'
        end
        object FRMatchCase: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'FRMatchCase'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object FRWholeValue: TCheckBox
          Left = 128
          Top = 40
          Width = 193
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'FRWholeValue'
          TabOrder = 1
        end
        object FRRegExpr: TCheckBox
          Left = 128
          Top = 64
          Width = 193
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'FRRegExpr'
          TabOrder = 2
          OnClick = FRRegExprClick
          OnKeyPress = FRRegExprKeyPress
        end
      end
      object GBackup: TGroupBox_Ext
        Left = 5
        Top = 187
        Width = 325
        Height = 45
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GBackup'
        TabOrder = 2
        DesignSize = (
          325
          45)
        object FLBackup: TLabel
          Left = 8
          Top = 17
          Width = 49
          Height = 13
          Caption = 'FLBackup'
          FocusControl = FBackup
        end
        object FBackup: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'FBackup'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
      end
    end
    object TSExecute: TTabSheet
      Caption = 'TSExecute'
      ImageIndex = 2
      TabVisible = False
      OnShow = TSExecuteShow
      DesignSize = (
        333
        279)
      object GProgress: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 169
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GProgress'
        TabOrder = 0
        DesignSize = (
          325
          169)
        object FLProgressTables: TLabel
          Left = 8
          Top = 40
          Width = 85
          Height = 13
          Caption = 'FLProgressTables'
        end
        object FLProgressRecords: TLabel
          Left = 8
          Top = 64
          Width = 93
          Height = 13
          Caption = 'FLProgressRecords'
        end
        object FEntieredTables: TLabel
          Left = 158
          Top = 40
          Width = 77
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FEntieredTables'
        end
        object FEntieredRecords: TLabel
          Left = 150
          Top = 64
          Width = 85
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FEntieredRecords'
        end
        object FLProgressTime: TLabel
          Left = 8
          Top = 88
          Width = 76
          Height = 13
          Caption = 'FLProgressTime'
        end
        object FEntieredTime: TLabel
          Left = 167
          Top = 88
          Width = 68
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FEntieredTime'
        end
        object FLEntiered: TLabel
          Left = 184
          Top = 16
          Width = 51
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FLEntiered'
        end
        object FLDone: TLabel
          Left = 278
          Top = 16
          Width = 38
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FLDone'
        end
        object FDoneTables: TLabel
          Left = 253
          Top = 40
          Width = 64
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FDoneTables'
        end
        object FDoneRecords: TLabel
          Left = 244
          Top = 64
          Width = 72
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FDoneRecords'
        end
        object FDoneTime: TLabel
          Left = 261
          Top = 88
          Width = 55
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FDoneTime'
        end
        object FLErrors: TLabel
          Left = 8
          Top = 148
          Width = 39
          Height = 13
          Caption = 'FLErrors'
        end
        object FErrors: TLabel
          Left = 283
          Top = 148
          Width = 33
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FErrors'
        end
        object FProgressBar: TProgressBar
          Left = 8
          Top = 120
          Width = 308
          Height = 16
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
      end
      object GMessages: TGroupBox_Ext
        Left = 4
        Top = 176
        Width = 325
        Height = 89
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'GMessages'
        TabOrder = 1
        DesignSize = (
          325
          89)
        object PErrorMessages: TPanel_Ext
          Left = 8
          Top = 16
          Width = 308
          Height = 65
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvRaised
          BevelOuter = bvLowered
          ParentBackground = False
          TabOrder = 0
          object FErrorMessages: TRichEdit
            Left = 2
            Top = 2
            Width = 304
            Height = 61
            TabStop = False
            Align = alClient
            BorderStyle = bsNone
            Ctl3D = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Lines.Strings = (
              'FErrorMessages')
            ParentCtl3D = False
            ParentFont = False
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 1
            WordWrap = False
          end
          object FTables: TListView_Ext
            Left = 2
            Top = 2
            Width = 304
            Height = 61
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                AutoSize = True
                Caption = 'Table'
              end>
            MultiSelect = True
            ReadOnly = True
            RowSelect = True
            PopupMenu = MTables
            ShowColumnHeaders = False
            TabOrder = 0
            TabStop = False
            ViewStyle = vsReport
            OnDblClick = FTablesDblClick
          end
        end
      end
    end
    object TSSelect: TTabSheet
      Caption = 'TSSelect'
      TabVisible = False
      OnShow = TSSelectShow
      DesignSize = (
        333
        279)
      object GSelect: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 271
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'GSelect'
        TabOrder = 0
        DesignSize = (
          325
          271)
        object PSelect: TPanel_Ext
          Left = 8
          Top = 16
          Width = 308
          Height = 246
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvRaised
          BevelOuter = bvLowered
          Caption = 'PSelect'
          ParentBackground = False
          TabOrder = 0
          object FSelect: TTreeView_Ext
            Left = 2
            Top = 2
            Width = 304
            Height = 242
            Align = alClient
            BorderStyle = bsNone
            HideSelection = False
            Indent = 19
            MultiSelect = True
            MultiSelectStyle = [msControlSelect, msShiftSelect, msSiblingOnly]
            ReadOnly = True
            ShowLines = False
            TabOrder = 0
            OnChange = FSelectChange
            OnDblClick = FSelectDblClick
            OnExpanding = FSelectExpanding
            OnGetImageIndex = FSelectGetImageIndex
          end
        end
      end
    end
    object TSFOptions: TTabSheet
      Caption = 'TSFOptions'
      TabVisible = False
      OnShow = TSFOptionsShow
      DesignSize = (
        333
        279)
      object GFWhat: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GFWhat'
        TabOrder = 0
        DesignSize = (
          325
          49)
        object FLFFindText: TLabel
          Left = 8
          Top = 19
          Width = 59
          Height = 13
          Caption = 'FLFFindText'
          FocusControl = FFFindText
        end
        object FFFindText: TComboBox_Ext
          Left = 128
          Top = 16
          Width = 189
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'FFFindText'
          OnChange = FFFindTextChange
        end
      end
      object GFOptions: TGroupBox_Ext
        Left = 4
        Top = 56
        Width = 325
        Height = 93
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GFOptions'
        TabOrder = 1
        DesignSize = (
          325
          93)
        object FLFSearchOptions: TLabel
          Left = 8
          Top = 17
          Width = 88
          Height = 13
          Caption = 'FLFSearchOptions'
        end
        object FFMatchCase: TCheckBox
          Left = 128
          Top = 16
          Width = 189
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'FFMatchCase'
          TabOrder = 0
        end
        object FFWholeValue: TCheckBox
          Left = 128
          Top = 40
          Width = 189
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'FFWholeValue'
          TabOrder = 1
        end
        object FFRegExpr: TCheckBox
          Left = 128
          Top = 64
          Width = 189
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'FFRegExpr'
          TabOrder = 2
          OnClick = FFRegExprClick
          OnKeyPress = FFRegExprKeyPress
        end
      end
    end
  end
  object FBHelp: TButton
    Left = 8
    Top = 296
    Width = 75
    Height = 25
    HelpContext = 1090
    Anchors = [akLeft, akBottom]
    Caption = 'FBHelp'
    TabOrder = 1
    OnClick = FBHelpClick
  end
  object MTables: TPopupMenu
    Left = 88
    Top = 304
    object mTCopy: TMenuItem
      Caption = 'aECopy'
      OnClick = mTCopyClick
    end
  end
end
