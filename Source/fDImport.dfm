object DImport: TDImport
  Left = 734
  Top = 156
  BorderStyle = bsDialog
  Caption = 'DImport'
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
  OnClose = FormClose
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object FBForward: TButton
    Left = 171
    Top = 296
    Width = 75
    Height = 25
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
    Height = 281
    ActivePage = TSCSVOptions
    Style = tsButtons
    TabOrder = 0
    TabStop = False
    object TSTables: TTabSheet
      Caption = 'TSTables'
      TabVisible = False
      OnHide = TSTablesHide
      OnShow = TSTablesShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GTable: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 265
        Caption = 'GTable'
        TabOrder = 0
        object PTables: TPanel_Ext
          Left = 8
          Top = 16
          Width = 310
          Height = 241
          BevelInner = bvRaised
          BevelOuter = bvLowered
          ParentBackground = False
          TabOrder = 0
          object FTables: TListView
            Left = 2
            Top = 2
            Width = 306
            Height = 237
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Width = 304
              end>
            HideSelection = False
            MultiSelect = True
            ReadOnly = True
            ShowColumnHeaders = False
            TabOrder = 0
            ViewStyle = vsList
            OnDblClick = FTablesDblClick
            OnSelectItem = FTablesSelectItem
          end
        end
      end
    end
    object TSCSVOptions: TTabSheet
      Caption = 'TSCSVOptions'
      TabVisible = False
      OnHide = TSCSVOptionsHide
      OnShow = TSCSVOptionsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GCSVHow: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 137
        Caption = 'GCSVHow'
        TabOrder = 1
        object FLCSVHeadline: TLabel
          Left = 8
          Top = 17
          Width = 75
          Height = 13
          Caption = 'FLCSVHeadline'
        end
        object FCSVHeadline: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FCSVHeadline'
          TabOrder = 0
          OnClick = FCSVPreviewUpdate
          OnKeyPress = FCSVKeyPress
        end
        object PDelimiter: TPanel_Ext
          Left = 4
          Top = 36
          Width = 317
          Height = 45
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 1
          object FLDelimiter: TLabel
            Left = 4
            Top = 6
            Width = 52
            Height = 13
            Caption = 'FLDelimiter'
          end
          object FDelimiterTab: TRadioButton
            Left = 124
            Top = 6
            Width = 193
            Height = 17
            Caption = 'FDelimiterTab'
            TabOrder = 0
            OnClick = FDelimiterClick
            OnKeyPress = FDelimiterKeyPress
          end
          object FDelimiterChar: TRadioButton
            Left = 124
            Top = 26
            Width = 81
            Height = 17
            Caption = 'FDelimiterChar'
            TabOrder = 1
            OnClick = FDelimiterClick
            OnKeyPress = FDelimiterKeyPress
          end
          object FDelimiter: TEdit
            Left = 204
            Top = 24
            Width = 17
            Height = 21
            MaxLength = 1
            TabOrder = 2
            Text = 'FDelimiter'
            OnChange = FCSVPreviewUpdate
          end
        end
        object PQuoting: TPanel_Ext
          Left = 4
          Top = 83
          Width = 317
          Height = 45
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 2
          object FLQuoteValues: TLabel
            Left = 4
            Top = 6
            Width = 73
            Height = 13
            Caption = 'FLQuoteValues'
          end
          object FStringQuote: TRadioButton
            Left = 124
            Top = 26
            Width = 193
            Height = 17
            Caption = 'FStringQuote'
            TabOrder = 0
            OnClick = FQuoteClick
            OnKeyPress = FQuoteKeyPress
          end
          object FQuoteChar: TEdit
            Left = 209
            Top = 24
            Width = 17
            Height = 21
            MaxLength = 1
            TabOrder = 1
            Text = 'FQuoteChar'
            OnChange = FCSVPreviewUpdate
          end
          object FNoQuote: TRadioButton
            Left = 125
            Top = 6
            Width = 193
            Height = 17
            Caption = 'FNoQuote'
            TabOrder = 2
            OnClick = FQuoteClick
            OnKeyPress = FQuoteKeyPress
          end
        end
        object Sizer: TRadioButton
          Left = 100
          Top = 16
          Width = 17
          Height = 17
          TabOrder = 3
          Visible = False
        end
      end
      object GCSVPreview: TGroupBox_Ext
        Left = 3
        Top = 143
        Width = 325
        Height = 125
        Caption = 'GCSVPreview'
        TabOrder = 0
        object PCSVPreview: TPanel_Ext
          Left = 8
          Top = 16
          Width = 308
          Height = 99
          BevelInner = bvRaised
          BevelOuter = bvLowered
          ParentBackground = False
          TabOrder = 0
          object FCSVPreview: TListView
            Left = 2
            Top = 2
            Width = 304
            Height = 95
            Align = alClient
            BorderStyle = bsNone
            Columns = <>
            ColumnClick = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
    end
    object TSODBCOptions: TTabSheet
      Caption = 'TSODBCOptions'
      TabVisible = False
      OnHide = TSODBCOptionsHide
      OnShow = TSODBCOptionsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GODBCDatabaseHow: TGroupBox_Ext
        Left = 4
        Top = 76
        Width = 325
        Height = 157
        Caption = 'GODBCDatabaseHow'
        TabOrder = 1
        object FLEngine: TLabel
          Left = 8
          Top = 23
          Width = 45
          Height = 13
          Caption = 'FLEngine'
          FocusControl = FEngine
        end
        object FLDefaultCharset: TLabel
          Left = 8
          Top = 61
          Width = 82
          Height = 13
          Caption = 'FLDefaultCharset'
          FocusControl = FDefaultCharset
        end
        object FLCollation: TLabel
          Left = 8
          Top = 87
          Width = 52
          Height = 13
          Caption = 'FLCollation'
          FocusControl = FCollation
        end
        object FLRowFormat: TLabel
          Left = 8
          Top = 125
          Width = 66
          Height = 13
          Caption = 'FLRowFormat'
          FocusControl = FRowFormat
        end
        object FEngine: TComboBox_Ext
          Left = 128
          Top = 20
          Width = 89
          Height = 21
          TabOrder = 0
          Text = 'FEngine'
        end
        object FDefaultCharset: TComboBox_Ext
          Left = 128
          Top = 58
          Width = 89
          Height = 21
          TabOrder = 1
          Text = 'FDefaultCharset'
          OnChange = FDefaultCharsetChange
        end
        object FCollation: TComboBox_Ext
          Left = 128
          Top = 84
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 2
        end
        object FRowFormat: TComboBox_Ext
          Left = 128
          Top = 122
          Width = 121
          Height = 21
          TabOrder = 3
          Items.Strings = (
            ''
            'Fixed'
            'Dynamic'
            'Compressed'
            'Redundant'
            'Compact')
        end
      end
      object GODBCWhat: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 71
        Caption = 'GODBCWhat'
        TabOrder = 0
        object FLWhat: TLabel
          Left = 8
          Top = 17
          Width = 38
          Height = 13
          Caption = 'FLWhat'
        end
        object FObjects: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FObjects'
          TabOrder = 0
          OnClick = FObjectsClick
          OnKeyPress = FObjectsKeyPress
        end
        object FData: TCheckBox
          Left = 128
          Top = 42
          Width = 193
          Height = 17
          Caption = 'FData'
          TabOrder = 1
          OnClick = FDataClick
          OnKeyPress = FDataKeyPress
        end
      end
    end
    object TSXMLOptions: TTabSheet
      Caption = 'TSXMLOptions'
      TabVisible = False
      OnHide = TSXMLOptionsHide
      OnShow = TSXMLOptionsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GXMLHow: TGroupBox_Ext
        Left = 4
        Top = 8
        Width = 325
        Height = 57
        Caption = 'GXMLHow'
        TabOrder = 0
        object FLRecordTag: TLabel
          Left = 8
          Top = 25
          Width = 66
          Height = 13
          Caption = 'FLRecordTag'
          FocusControl = FRecordTag
        end
        object FL2RecordTag: TLabel
          Left = 128
          Top = 25
          Width = 6
          Height = 13
          Caption = '<'
        end
        object FL3RecordTag: TLabel
          Left = 191
          Top = 25
          Width = 6
          Height = 13
          Caption = '>'
        end
        object FRecordTag: TEdit
          Left = 134
          Top = 22
          Width = 57
          Height = 21
          CharCase = ecLowerCase
          TabOrder = 0
          Text = 'row'
          OnChange = TSXMLOptionChange
        end
      end
    end
    object TSFields: TTabSheet
      Caption = 'TSFields'
      TabVisible = False
      OnShow = TSFieldsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GFields: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 265
        Caption = 'GFields'
        TabOrder = 0
        object FLSourceFields: TLabel
          Left = 8
          Top = 24
          Width = 73
          Height = 13
          Caption = 'FLSourceFields'
        end
        object FLFields: TLabel
          Left = 156
          Top = 24
          Width = 39
          Height = 13
          Caption = 'FLFields'
        end
        object ScrollBox: TScrollBox
          Left = 4
          Top = 40
          Width = 309
          Height = 217
          BorderStyle = bsNone
          TabOrder = 0
          object FLReferrer1: TLabel
            Left = 131
            Top = 11
            Width = 9
            Height = 13
            Alignment = taCenter
            AutoSize = False
            Caption = '->'
            Visible = False
          end
          object FField1: TComboBox_Ext
            Left = 150
            Top = 8
            Width = 135
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            Visible = False
            OnChange = TSFieldsChange
            OnExit = FFieldExit
          end
          object FSourceField1: TEdit
            Left = 4
            Top = 8
            Width = 117
            Height = 21
            TabOrder = 1
            Text = 'FSourceField1'
            Visible = False
            OnChange = TSFieldsChange
          end
          object FSourceField2: TEdit
            Left = 4
            Top = 40
            Width = 117
            Height = 21
            TabOrder = 2
            Text = 'FSourceField2'
            Visible = False
          end
        end
      end
    end
    object TSImportType: TTabSheet
      Caption = 'TSImportType'
      TabVisible = False
      OnShow = TSImportTypeShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GImportType: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 121
        Caption = 'GImportType'
        TabOrder = 0
        object FLImportType: TLabel
          Left = 8
          Top = 24
          Width = 65
          Height = 13
          Caption = 'FLImportType'
        end
        object FInsert: TRadioButton
          Left = 128
          Top = 24
          Width = 193
          Height = 17
          Caption = 'FInsert'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = FImportTypeClick
          OnKeyPress = FImportTypeKeyPress
        end
        object FReplace: TRadioButton
          Left = 128
          Top = 56
          Width = 193
          Height = 17
          Caption = 'FReplace'
          TabOrder = 1
          OnClick = FImportTypeClick
          OnKeyPress = FImportTypeKeyPress
        end
        object FUpdate: TRadioButton
          Left = 128
          Top = 88
          Width = 193
          Height = 17
          Caption = 'FUpdate'
          TabOrder = 2
          OnClick = FImportTypeClick
          OnKeyPress = FImportTypeKeyPress
        end
      end
    end
    object TSExecute: TTabSheet
      Caption = 'TSExecute'
      TabVisible = False
      OnShow = TSExecuteShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GErrorMessages: TGroupBox_Ext
        Left = 4
        Top = 176
        Width = 325
        Height = 89
        Caption = 'GErrorMessages'
        TabOrder = 1
        object PErrorMessages: TPanel_Ext
          Left = 8
          Top = 16
          Width = 308
          Height = 65
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
            TabOrder = 0
            WordWrap = False
          end
        end
      end
      object GProgress: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 169
        Caption = 'GProgress'
        TabOrder = 0
        object FLProgressTime: TLabel
          Left = 8
          Top = 88
          Width = 76
          Height = 13
          Caption = 'FLProgressTime'
        end
        object FLDone: TLabel
          Left = 197
          Top = 16
          Width = 38
          Height = 13
          Alignment = taRightJustify
          Caption = 'FLDone'
        end
        object FLEntiered: TLabel
          Left = 265
          Top = 16
          Width = 51
          Height = 13
          Alignment = taRightJustify
          Caption = 'FLEntiered'
        end
        object FLProgressRecords: TLabel
          Left = 8
          Top = 64
          Width = 93
          Height = 13
          Caption = 'FLProgressRecords'
        end
        object FDoneRecords: TLabel
          Left = 163
          Top = 64
          Width = 72
          Height = 13
          Alignment = taRightJustify
          Caption = 'FDoneRecords'
        end
        object FEntieredRecords: TLabel
          Left = 232
          Top = 64
          Width = 85
          Height = 13
          Alignment = taRightJustify
          Caption = 'FEntieredRecords'
        end
        object FDoneTime: TLabel
          Left = 180
          Top = 88
          Width = 55
          Height = 13
          Alignment = taRightJustify
          Caption = 'FDoneTime'
        end
        object FEntieredTime: TLabel
          Left = 249
          Top = 88
          Width = 68
          Height = 13
          Alignment = taRightJustify
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
          Left = 283
          Top = 148
          Width = 33
          Height = 13
          Alignment = taRightJustify
          Caption = 'FErrors'
        end
        object FLProgressTables: TLabel
          Left = 8
          Top = 40
          Width = 85
          Height = 13
          Caption = 'FLProgressTables'
        end
        object FDoneTables: TLabel
          Left = 171
          Top = 40
          Width = 64
          Height = 13
          Alignment = taRightJustify
          Caption = 'FDoneTables'
        end
        object FEntieredTables: TLabel
          Left = 240
          Top = 40
          Width = 77
          Height = 13
          Alignment = taRightJustify
          Caption = 'FEntieredTables'
        end
        object FProgressBar: TProgressBar
          Left = 8
          Top = 120
          Width = 308
          Height = 16
          TabOrder = 0
        end
      end
    end
  end
  object FBBack: TButton
    Left = 96
    Top = 296
    Width = 75
    Height = 25
    Caption = 'FBBack'
    TabOrder = 2
    OnClick = FBBackClick
  end
  object FBHelp: TButton
    Left = 8
    Top = 296
    Width = 75
    Height = 25
    Caption = 'FBHelp'
    TabOrder = 1
    OnClick = FBHelpClick
  end
  object OpenDialog: TOpenDialog_Ext
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofNoNetworkButton, ofEnableSizing]
    EncodingIndex = -1
    EncodingLabel = '&Encoding:'
    Left = 296
    Top = 176
  end
end
