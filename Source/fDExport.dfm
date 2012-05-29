object DExport: TDExport
  Left = 486
  Top = 233
  BorderStyle = bsDialog
  Caption = 'DExport'
  ClientHeight = 331
  ClientWidth = 341
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 341
    Height = 281
    ActivePage = TSExecute
    HotTrack = True
    Style = tsFlatButtons
    TabOrder = 0
    TabStop = False
    object TSODBCSelect: TTabSheet
      Caption = 'TSODBCSelect'
      TabVisible = False
      OnShow = TSODBCSelectShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GODBCSelect: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 265
        Caption = 'GODBCSelect'
        TabOrder = 0
        object PODBCSelect: TPanel_Ext
          Left = 8
          Top = 16
          Width = 310
          Height = 241
          BevelInner = bvRaised
          BevelOuter = bvLowered
          ParentBackground = False
          TabOrder = 0
          object FODBCSelect: TListView_Ext
            Left = 2
            Top = 2
            Width = 306
            Height = 237
            Align = alClient
            BorderStyle = bsNone
            Columns = <>
            HideSelection = False
            TabOrder = 0
            ViewStyle = vsList
            OnChange = FODBCSelectChange
            OnDblClick = FODBCSelectDblClick
          end
        end
      end
    end
    object TSSQLOptions: TTabSheet
      Caption = 'TSSQLOptions'
      TabVisible = False
      OnHide = TSOptionsHide
      OnShow = TSSQLOptionsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GSQLWhat: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 63
        Caption = 'GSQLWhat'
        TabOrder = 0
        object FLSQLWhat: TLabel
          Left = 8
          Top = 17
          Width = 59
          Height = 13
          Caption = 'FLSQLWhat'
        end
        object FSQLStructure: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FSQLStructure'
          TabOrder = 0
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
        object FSQLData: TCheckBox
          Left = 128
          Top = 36
          Width = 193
          Height = 17
          Caption = 'FSQLData'
          TabOrder = 1
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
      end
      object GSQLOptions: TGroupBox_Ext
        Left = 4
        Top = 70
        Width = 325
        Height = 143
        Caption = 'GSQLOptions'
        TabOrder = 1
        object FLGeneral: TLabel
          Left = 9
          Top = 117
          Width = 49
          Height = 13
          Caption = 'FLGeneral'
        end
        object FLDrop: TLabel
          Left = 8
          Top = 67
          Width = 35
          Height = 13
          Caption = 'FLDrop'
        end
        object FLDatabaseHandling: TLabel
          Left = 8
          Top = 17
          Width = 100
          Height = 13
          Caption = 'FLDatabaseHandling'
        end
        object FDrop: TCheckBox
          Left = 128
          Top = 66
          Width = 193
          Height = 17
          Caption = 'FDrop'
          TabOrder = 2
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
        object FUseDatabase: TCheckBox
          Left = 128
          Top = 36
          Width = 193
          Height = 17
          Caption = 'FUseDatabase'
          TabOrder = 1
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
        object FCreateDatabase: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FCreateDatabase'
          TabOrder = 0
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
        object FDisableKeys: TCheckBox
          Left = 129
          Top = 116
          Width = 193
          Height = 17
          Caption = 'FDisableKeys'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
        object FReplaceData: TCheckBox
          Left = 128
          Top = 86
          Width = 193
          Height = 17
          Caption = 'FReplaceData'
          TabOrder = 3
          OnClick = FSQLOptionClick
          OnKeyPress = FSQLOptionKeyPress
        end
      end
    end
    object TSCSVOptions: TTabSheet
      Caption = 'TSCSVOptions'
      TabVisible = False
      OnHide = TSOptionsHide
      OnShow = TSCSVOptionsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GCSVOptions: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 197
        Caption = 'GCSVOptions'
        TabOrder = 0
        object FLCSVHeadline: TLabel
          Left = 8
          Top = 17
          Width = 75
          Height = 13
          Caption = 'FLCSVHeadline'
        end
        object PQuote: TPanel_Ext
          Left = 4
          Top = 98
          Width = 317
          Height = 89
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 2
          object FLQuoteValues: TLabel
            Left = 4
            Top = 4
            Width = 73
            Height = 13
            Caption = 'FLQuoteValues'
          end
          object FLQuoteChar: TLabel
            Left = 4
            Top = 67
            Width = 63
            Height = 13
            Caption = 'FLQuoteChar'
            FocusControl = FQuoteChar
          end
          object FQuoteChar: TEdit
            Left = 124
            Top = 64
            Width = 17
            Height = 21
            MaxLength = 1
            TabOrder = 3
            Text = 'FQuoteChar'
            OnExit = FQuoteCharExit
          end
          object FNoQuote: TRadioButton
            Left = 124
            Top = 3
            Width = 193
            Height = 17
            Caption = 'FNoQuote'
            TabOrder = 2
            OnClick = FQuoteClick
          end
          object FStringQuote: TRadioButton
            Left = 124
            Top = 23
            Width = 193
            Height = 17
            Caption = 'FStringQuote'
            TabOrder = 1
            OnClick = FQuoteClick
          end
          object FAllQuote: TRadioButton
            Left = 124
            Top = 43
            Width = 193
            Height = 17
            Caption = 'FAllQuote'
            TabOrder = 0
            OnClick = FQuoteClick
            OnKeyPress = FQuoteKeyPress
          end
        end
        object FCSVHeadline: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FCSVHeadline'
          TabOrder = 0
        end
        object PSeparator: TPanel_Ext
          Left = 4
          Top = 44
          Width = 317
          Height = 53
          BevelOuter = bvNone
          Ctl3D = True
          ParentBackground = False
          ParentCtl3D = False
          TabOrder = 1
          object FLSeparator: TLabel
            Left = 4
            Top = 6
            Width = 58
            Height = 13
            Caption = 'FLSeparator'
          end
          object FSeparatorChar: TRadioButton
            Left = 124
            Top = 26
            Width = 77
            Height = 17
            Caption = 'FSeparatorChar'
            TabOrder = 1
            OnClick = FSeparatorClick
            OnKeyPress = FSeparatorKeyPress
          end
          object FSeparator: TEdit
            Left = 207
            Top = 24
            Width = 21
            Height = 21
            TabOrder = 2
            Text = 'FSeparatorChar'
          end
          object FSeparatorTab: TRadioButton
            Left = 124
            Top = 5
            Width = 193
            Height = 17
            Caption = 'FSeparatorTab'
            TabOrder = 0
            OnClick = FSeparatorClick
            OnKeyPress = FSeparatorKeyPress
          end
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
        Top = 0
        Width = 325
        Height = 265
        Caption = 'GXMLHow'
        TabOrder = 0
        object FLRootTag: TLabel
          Left = 8
          Top = 19
          Width = 54
          Height = 13
          Caption = 'FLRootTag'
          FocusControl = FRootTag
        end
        object FL2RootTag: TLabel
          Left = 128
          Top = 19
          Width = 6
          Height = 13
          Caption = '<'
        end
        object FL3RootTag: TLabel
          Left = 191
          Top = 19
          Width = 6
          Height = 13
          Caption = '>'
        end
        object FLRecordTag: TLabel
          Left = 8
          Top = 185
          Width = 66
          Height = 13
          Caption = 'FLRecordTag'
          FocusControl = FRecordTag
        end
        object FL2RecordTag: TLabel
          Left = 128
          Top = 185
          Width = 6
          Height = 13
          Caption = '<'
        end
        object FL3RecordTag: TLabel
          Left = 191
          Top = 185
          Width = 6
          Height = 13
          Caption = '>'
        end
        object FRootTag: TEdit
          Left = 134
          Top = 16
          Width = 57
          Height = 21
          CharCase = ecLowerCase
          TabOrder = 0
          Text = 'mysql'
          OnChange = TSXMLOptionChange
        end
        object PDatabaseTag: TPanel_Ext
          Left = 4
          Top = 48
          Width = 317
          Height = 57
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 4
          object FLDatabaseTag: TLabel
            Left = 4
            Top = 1
            Width = 77
            Height = 13
            Caption = 'FLDatabaseTag'
          end
          object FL1DatabaseTagFree: TLabel
            Left = 124
            Top = 37
            Width = 6
            Height = 13
            Caption = '<'
          end
          object FL2DatabaseTagFree: TLabel
            Left = 231
            Top = 37
            Width = 66
            Height = 13
            Caption = '="db_name">'
          end
          object FDatabaseTagFree: TRadioButton
            Left = 106
            Top = 36
            Width = 17
            Height = 17
            TabOrder = 2
            OnClick = FDatabaseTagClick
            OnKeyPress = FDatabaseTagKeyPress
          end
          object FDatabaseTag: TEdit
            Left = 130
            Top = 34
            Width = 57
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 3
            Text = 'database'
            OnChange = TSXMLOptionChange
          end
          object FDatabaseTagDisabled: TRadioButton
            Left = 106
            Top = 0
            Width = 185
            Height = 17
            Caption = 'FDatabaseTagDisabled'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = FDatabaseTagClick
            OnKeyPress = FDatabaseTagKeyPress
          end
          object FDatabaseTagName: TRadioButton
            Left = 106
            Top = 18
            Width = 185
            Height = 17
            Caption = '<db_name>'
            TabOrder = 1
            OnClick = FDatabaseTagClick
            OnKeyPress = FDatabaseTagKeyPress
          end
          object FDatabaseAttribute: TEdit
            Left = 194
            Top = 34
            Width = 37
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 4
            Text = 'name'
            OnChange = TSXMLOptionChange
          end
        end
        object PTableTag: TPanel_Ext
          Left = 4
          Top = 112
          Width = 317
          Height = 57
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 2
          object FLTableTag: TLabel
            Left = 4
            Top = 1
            Width = 58
            Height = 13
            Caption = 'FLTableTag'
          end
          object FL1TableTagFree: TLabel
            Left = 124
            Top = 37
            Width = 6
            Height = 13
            Caption = '<'
          end
          object FL2TableTagFree: TLabel
            Left = 231
            Top = 37
            Width = 65
            Height = 13
            Caption = '="tbl_name">'
          end
          object FTableTagFree: TRadioButton
            Left = 106
            Top = 36
            Width = 17
            Height = 17
            TabOrder = 2
            OnClick = FTableTagClick
            OnKeyPress = FTableTagKeyPress
          end
          object FTableTag: TEdit
            Left = 130
            Top = 34
            Width = 57
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 3
            Text = 'table'
            OnChange = TSXMLOptionChange
          end
          object FTableTagDisabled: TRadioButton
            Left = 106
            Top = 0
            Width = 185
            Height = 17
            Caption = 'FTableTagDisabled'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = FTableTagClick
            OnKeyPress = FTableTagKeyPress
          end
          object FTableTagName: TRadioButton
            Left = 106
            Top = 18
            Width = 185
            Height = 17
            Caption = '<tbl_name>'
            TabOrder = 1
            OnClick = FTableTagClick
            OnKeyPress = FTableTagKeyPress
          end
          object FTableAttribute: TEdit
            Left = 194
            Top = 34
            Width = 37
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 4
            Text = 'name'
            OnChange = TSXMLOptionChange
          end
        end
        object FRecordTag: TEdit
          Left = 134
          Top = 182
          Width = 57
          Height = 21
          CharCase = ecLowerCase
          TabOrder = 3
          Text = 'row'
          OnChange = TSXMLOptionChange
        end
        object PFieldTag: TPanel_Ext
          Left = 4
          Top = 216
          Width = 317
          Height = 41
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 1
          object FLFieldTag: TLabel
            Left = 4
            Top = 1
            Width = 53
            Height = 13
            Caption = 'FLFieldTag'
          end
          object FL1FieldTagFree: TLabel
            Left = 124
            Top = 19
            Width = 6
            Height = 13
            Caption = '<'
          end
          object FL2FieldTagFree: TLabel
            Left = 231
            Top = 19
            Width = 65
            Height = 13
            Caption = '="fld_name">'
          end
          object FFieldTagFree: TRadioButton
            Left = 106
            Top = 18
            Width = 17
            Height = 17
            Checked = True
            TabOrder = 1
            TabStop = True
            OnClick = FFieldTagClick
            OnKeyPress = FFieldTagKeyPress
          end
          object FFieldTag: TEdit
            Left = 130
            Top = 16
            Width = 57
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 2
            Text = 'field'
            OnChange = TSXMLOptionChange
          end
          object FFieldTagName: TRadioButton
            Left = 106
            Top = 0
            Width = 185
            Height = 17
            Caption = '<fld_name>'
            TabOrder = 0
            OnClick = FFieldTagClick
            OnKeyPress = FFieldTagKeyPress
          end
          object FFieldAttribute: TEdit
            Left = 194
            Top = 16
            Width = 37
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 3
            Text = 'name'
            OnChange = TSXMLOptionChange
          end
        end
      end
    end
    object TSHTMLOptions: TTabSheet
      Caption = 'TSHTMLOptions'
      TabVisible = False
      OnHide = TSOptionsHide
      OnShow = TSHTMLOptionsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GHTMLWhat: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 69
        Caption = 'GHTMLWhat'
        TabOrder = 0
        object FLHTMLWhat: TLabel
          Left = 8
          Top = 17
          Width = 68
          Height = 13
          Caption = 'FLHTMLWhat'
          FocusControl = FHTMLStructure
        end
        object FHTMLStructure: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FHTMLStructure'
          TabOrder = 0
          OnClick = FHTMLStructureClick
          OnKeyPress = FHTMLStructureKeyPress
        end
        object FHTMLData: TCheckBox
          Left = 128
          Top = 40
          Width = 193
          Height = 17
          Caption = 'FHTMLData'
          TabOrder = 1
          OnClick = FHTMLDataClick
          OnKeyPress = FHTMLDataKeyPress
        end
      end
      object GHTMLOptions: TGroupBox_Ext
        Left = 4
        Top = 76
        Width = 325
        Height = 133
        Caption = 'GHTMLOptions'
        TabOrder = 1
        object FLHTMLNullValues: TLabel
          Left = 8
          Top = 17
          Width = 92
          Height = 13
          Caption = 'FLHTMLNullValues'
          FocusControl = FHTMLNullText
        end
        object FLHTMLViewDatas: TLabel
          Left = 8
          Top = 49
          Width = 93
          Height = 13
          Caption = 'FLHTMLViewDatas'
        end
        object FLHTMLBGColorEnabled: TLabel
          Left = 9
          Top = 81
          Width = 120
          Height = 13
          Caption = 'FLHTMLBGColorEnabled'
        end
        object FHTMLNullText: TCheckBox
          Left = 128
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FHTMLNullText'
          TabOrder = 0
        end
        object FHTMLShowMemoContent: TCheckBox
          Left = 128
          Top = 48
          Width = 193
          Height = 17
          Caption = 'FHTMLShowMemoContent'
          TabOrder = 1
        end
        object FHTMLRowBGColorEnabled: TCheckBox
          Left = 129
          Top = 80
          Width = 193
          Height = 17
          Caption = 'FHTMLRowBGColorEnabled'
          TabOrder = 2
        end
        object FHTMLIndexBGColorEnabled: TCheckBox
          Left = 129
          Top = 104
          Width = 193
          Height = 17
          Caption = 'FHTMLIndexBGColorEnabled'
          TabOrder = 3
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
        object FLFields: TLabel
          Left = 8
          Top = 24
          Width = 39
          Height = 13
          Caption = 'FLFields'
        end
        object FLDestFields: TLabel
          Left = 172
          Top = 24
          Width = 61
          Height = 13
          Caption = 'FLDestFields'
        end
        object ScrollBox: TScrollBox
          Left = 4
          Top = 40
          Width = 309
          Height = 217
          BevelInner = bvNone
          BorderStyle = bsNone
          TabOrder = 0
          object FLReferrer1: TLabel
            Left = 149
            Top = 11
            Width = 9
            Height = 13
            Caption = '->'
            Visible = False
          end
          object FField1: TComboBox_Ext
            Left = 4
            Top = 8
            Width = 135
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            TabStop = False
            Visible = False
            OnChange = FField1Change
            OnExit = FField1Exit
          end
          object FField2: TComboBox_Ext
            Left = 4
            Top = 40
            Width = 135
            Height = 21
            Style = csDropDownList
            TabOrder = 1
            TabStop = False
            Visible = False
          end
          object FDestField1: TEdit
            Left = 168
            Top = 8
            Width = 117
            Height = 21
            TabStop = False
            TabOrder = 2
            Text = 'FDestField1'
            Visible = False
            OnChange = FDestField1Change
          end
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
      object GProgress: TGroupBox_Ext
        Left = 4
        Top = 0
        Width = 325
        Height = 169
        Caption = 'GProgress'
        TabOrder = 0
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
          Left = 171
          Top = 40
          Width = 64
          Height = 13
          Alignment = taRightJustify
          Caption = 'FDoneTables'
        end
        object FDoneRecords: TLabel
          Left = 163
          Top = 64
          Width = 72
          Height = 13
          Alignment = taRightJustify
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
          Left = 180
          Top = 88
          Width = 55
          Height = 13
          Alignment = taRightJustify
          Caption = 'FDoneTime'
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
        object FEntieredTables: TLabel
          Left = 240
          Top = 40
          Width = 77
          Height = 13
          Alignment = taRightJustify
          Caption = 'FEntieredTables'
        end
        object FEntieredRecords: TLabel
          Left = 232
          Top = 64
          Width = 85
          Height = 13
          Alignment = taRightJustify
          Caption = 'FEntieredRecords'
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
        object FProgressBar: TProgressBar
          Left = 8
          Top = 120
          Width = 308
          Height = 16
          TabOrder = 0
        end
      end
      object GErrors: TGroupBox_Ext
        Left = 4
        Top = 176
        Width = 325
        Height = 89
        Caption = 'GErrors'
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
    end
  end
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
end
