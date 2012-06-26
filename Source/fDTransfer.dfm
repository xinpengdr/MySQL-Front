object DTransfer: TDTransfer
  Left = 689
  Top = 239
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DTransfer'
  ClientHeight = 331
  ClientWidth = 445
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
    445
    331)
  PixelsPerInch = 106
  TextHeight = 13
  object FBForward: TButton
    Left = 275
    Top = 296
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBForward'
    Default = True
    TabOrder = 3
    OnClick = FBForwardClick
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 444
    Height = 289
    ActivePage = TSSelect
    Anchors = [akLeft, akTop, akRight, akBottom]
    Style = tsFlatButtons
    TabOrder = 0
    TabStop = False
    OnResize = PageControlResize
    object TSSelect: TTabSheet
      Caption = 'TSSelect'
      TabVisible = False
      OnShow = TSSelectShow
      DesignSize = (
        436
        279)
      object GMaster: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 209
        Height = 265
        Anchors = [akLeft, akTop, akBottom]
        Caption = 'GMaster'
        TabOrder = 0
        DesignSize = (
          209
          265)
        object PMaster: TPanel_Ext
          Left = 8
          Top = 16
          Width = 193
          Height = 241
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvRaised
          BevelOuter = bvLowered
          Caption = 'PMaster'
          ParentBackground = False
          TabOrder = 0
          object FMaster: TTreeView_Ext
            Left = 2
            Top = 2
            Width = 189
            Height = 237
            Align = alClient
            BorderStyle = bsNone
            HideSelection = False
            Indent = 19
            MultiSelect = True
            MultiSelectStyle = [msControlSelect, msShiftSelect, msSiblingOnly]
            PopupMenu = MMaster
            ReadOnly = True
            ShowLines = False
            TabOrder = 0
            OnChange = TreeViewChange
            OnExpanding = TreeViewExpanding
            OnGetSelectedIndex = TreeViewGetSelectedIndex
            OnMouseDown = TreeViewMouseDown
          end
        end
      end
      object GSlave: TGroupBox_Ext
        Left = 224
        Top = 0
        Width = 209
        Height = 265
        Anchors = [akTop, akBottom]
        Caption = 'GSlave'
        TabOrder = 1
        DesignSize = (
          209
          265)
        object PSlave: TPanel_Ext
          Left = 8
          Top = 16
          Width = 193
          Height = 241
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvRaised
          BevelOuter = bvLowered
          ParentBackground = False
          TabOrder = 0
          object FSlave: TTreeView_Ext
            Left = 2
            Top = 2
            Width = 189
            Height = 237
            Align = alClient
            BorderStyle = bsNone
            HideSelection = False
            Indent = 19
            ReadOnly = True
            ShowLines = False
            TabOrder = 0
            OnChange = TreeViewChange
            OnExpanding = TreeViewExpanding
            OnGetSelectedIndex = TreeViewGetSelectedIndex
            OnMouseDown = TreeViewMouseDown
          end
        end
      end
    end
    object TSExecute: TTabSheet
      Caption = 'TSExecute'
      TabVisible = False
      OnShow = TSExecuteShow
      DesignSize = (
        436
        279)
      object GProgress: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 429
        Height = 169
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GProgress'
        TabOrder = 0
        DesignSize = (
          429
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
        object FDoneTables: TLabel
          Left = 275
          Top = 40
          Width = 64
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FDoneTables'
        end
        object FDoneRecords: TLabel
          Left = 267
          Top = 64
          Width = 72
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FDoneRecords'
        end
        object FLProgressTime: TLabel
          Left = 8
          Top = 88
          Width = 76
          Height = 13
          Caption = 'FLProgressTime'
        end
        object FDoneTime: TLabel
          Left = 284
          Top = 88
          Width = 55
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FDoneTime'
        end
        object FLDone: TLabel
          Left = 302
          Top = 16
          Width = 38
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FLDone'
        end
        object FLEntiered: TLabel
          Left = 368
          Top = 16
          Width = 51
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FLEntiered'
        end
        object FEntieredTables: TLabel
          Left = 342
          Top = 40
          Width = 77
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FEntieredTables'
        end
        object FEntieredRecords: TLabel
          Left = 334
          Top = 64
          Width = 85
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FEntieredRecords'
        end
        object FEntieredTime: TLabel
          Left = 351
          Top = 88
          Width = 68
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FEntieredTime'
        end
        object FLErrors: TLabel
          Left = 8
          Top = 148
          Width = 39
          Height = 13
          Caption = 'FLErrors'
        end
        object FErrors: TLabel
          Left = 387
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
          Width = 413
          Height = 16
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
      end
      object GErrorMessages: TGroupBox_Ext
        Left = 4
        Top = 176
        Width = 429
        Height = 89
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'GErrorMessages'
        TabOrder = 1
        DesignSize = (
          429
          89)
        object PErrorMessages: TPanel_Ext
          Left = 8
          Top = 16
          Width = 413
          Height = 65
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvRaised
          BevelOuter = bvLowered
          ParentBackground = False
          TabOrder = 0
          object FErrorMessages: TRichEdit
            Left = 2
            Top = 2
            Width = 409
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
            TabOrder = 0
            WordWrap = False
          end
        end
      end
    end
    object TSTransferOptions: TTabSheet
      Caption = 'TSTransferOptions'
      TabVisible = False
      OnShow = TSTransferOptionsShow
      DesignSize = (
        436
        279)
      object GTransferWhat: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 429
        Height = 69
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GTransferWhat'
        TabOrder = 0
        DesignSize = (
          429
          69)
        object FLTransferWhat: TLabel
          Left = 8
          Top = 17
          Width = 77
          Height = 13
          Caption = 'FLTransferWhat'
        end
        object FTransferStructure: TCheckBox
          Left = 200
          Top = 16
          Width = 225
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'FTransferStructure'
          Enabled = False
          TabOrder = 0
          OnClick = FTransferStructureClick
          OnKeyPress = FTransferStructureKeyPress
        end
        object FTransferData: TCheckBox
          Left = 200
          Top = 38
          Width = 225
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'FTransferData'
          TabOrder = 1
          OnClick = FTransferDataClick
          OnKeyPress = FTransferDataKeyPress
        end
      end
      object GTransferOptions: TGroupBox_Ext
        Left = 4
        Top = 76
        Width = 429
        Height = 45
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GTransferOptions'
        TabOrder = 1
        DesignSize = (
          429
          45)
        object FLTransferGeneral: TLabel
          Left = 8
          Top = 16
          Width = 88
          Height = 13
          Caption = 'FLTransferGeneral'
        end
        object FTransferDisableKeys: TCheckBox
          Left = 200
          Top = 16
          Width = 225
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'FTransferDisableKeys'
          TabOrder = 0
        end
      end
    end
  end
  object FBHelp: TButton
    Left = 8
    Top = 296
    Width = 75
    Height = 25
    HelpContext = 1089
    Anchors = [akLeft, akBottom]
    Caption = 'FBHelp'
    TabOrder = 1
    OnClick = FBHelpClick
  end
  object FBBack: TButton
    Left = 200
    Top = 296
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBBack'
    TabOrder = 2
    OnClick = FBBackClick
  end
  object FBCancel: TButton
    Left = 361
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
  object MMaster: TPopupMenu
    OnPopup = MMasterPopup
    Left = 112
    Top = 296
    object miSelectAll: TMenuItem
      Caption = 'miSelectAll'
      OnClick = miSelectAllClick
    end
  end
end
