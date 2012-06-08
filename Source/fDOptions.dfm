object DOptions: TDOptions
  Left = 694
  Top = 207
  HelpContext = 1066
  BorderStyle = bsDialog
  Caption = 'DOptions'
  ClientHeight = 410
  ClientWidth = 377
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
  object FBOk: TButton
    Left = 207
    Top = 376
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object FBCancel: TButton
    Left = 295
    Top = 376
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 3
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 362
    Height = 345
    ActivePage = TSLog
    HotTrack = True
    MultiLine = True
    TabOrder = 0
    object TSView: TTabSheet
      Caption = 'TSView'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GProgram: TGroupBox_Ext
        Left = 4
        Top = 4
        Width = 345
        Height = 81
        Caption = 'GProgram'
        TabOrder = 0
        object FLLanguage: TLabel
          Left = 8
          Top = 19
          Width = 97
          Height = 13
          AutoSize = False
          Caption = 'FLLanguage'
          FocusControl = FLanguage
        end
        object FLSkin: TLabel
          Left = 8
          Top = 51
          Width = 33
          Height = 13
          Caption = 'FLSkin'
          FocusControl = FSkin
        end
        object FLanguage: TComboBox_Ext
          Left = 144
          Top = 16
          Width = 137
          Height = 21
          Style = csDropDownList
          DropDownCount = 15
          Sorted = True
          TabOrder = 0
        end
        object FSkin: TComboBox_Ext
          Left = 144
          Top = 48
          Width = 137
          Height = 21
          Style = csDropDownList
          DropDownCount = 15
          Sorted = True
          TabOrder = 1
        end
      end
      object GTabs: TGroupBox_Ext
        Left = 4
        Top = 96
        Width = 345
        Height = 45
        Caption = 'GTabs'
        TabOrder = 1
        object FLTabsVisible: TLabel
          Left = 8
          Top = 19
          Width = 66
          Height = 13
          Caption = 'FLTabsVisible'
        end
        object FTabsVisible: TCheckBox
          Left = 144
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FTabsVisible'
          TabOrder = 0
        end
      end
    end
    object TSBrowser: TTabSheet
      Caption = 'TSBrowser'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GGrid: TGroupBox_Ext
        Left = 4
        Top = 4
        Width = 345
        Height = 241
        Caption = 'GGrid'
        TabOrder = 0
        object FLGridFont: TLabel
          Left = 8
          Top = 19
          Width = 52
          Height = 13
          Caption = 'FLGridFont'
          FocusControl = FGridFont
        end
        object FLMaxColumnWidth: TLabel
          Left = 8
          Top = 53
          Width = 95
          Height = 13
          Caption = 'FLMaxColumnWidth'
          FocusControl = FMaxColumnWidth
        end
        object FLGridNullValues: TLabel
          Left = 8
          Top = 121
          Width = 81
          Height = 13
          Caption = 'FLGridNullValues'
          FocusControl = PGridNullBGColor
        end
        object FLViewDatas: TLabel
          Left = 8
          Top = 87
          Width = 63
          Height = 13
          Caption = 'FLViewDatas'
        end
        object FLGridBGColorEnabled: TLabel
          Left = 8
          Top = 175
          Width = 109
          Height = 13
          Caption = 'FLGridBGColorEnabled'
        end
        object FLGridCurrRowBGColor: TLabel
          Left = 8
          Top = 208
          Width = 111
          Height = 13
          Caption = 'FLGridCurrRowBGColor'
        end
        object FLMaxColumnWidthCharacters: TLabel
          Left = 200
          Top = 53
          Width = 146
          Height = 13
          Caption = 'FLMaxColumnWidthCharacters'
        end
        object FMaxColumnWidth: TEdit
          Left = 144
          Top = 50
          Width = 33
          Height = 21
          TabOrder = 2
          Text = '100'
        end
        object FUDMaxColumnWidth: TUpDown
          Left = 177
          Top = 50
          Width = 15
          Height = 21
          Associate = FMaxColumnWidth
          Min = 10
          Max = 1024
          Increment = 10
          Position = 100
          TabOrder = 3
        end
        object FGridFont: TEdit
          Left = 144
          Top = 16
          Width = 121
          Height = 21
          ReadOnly = True
          TabOrder = 0
          Text = 'FGridFont'
          OnClick = FBGridFontClick
          OnKeyPress = FGridFontKeyPress
        end
        object PGridFont: TPanel_Ext
          Left = 96
          Top = 16
          Width = 21
          Height = 21
          Caption = 'PGridFont'
          Enabled = False
          ParentBackground = False
          TabOrder = 1
          Visible = False
        end
        object FGridNullText: TCheckBox
          Left = 162
          Top = 146
          Width = 169
          Height = 17
          Caption = 'FGridNullText'
          TabOrder = 7
        end
        object FGridShowMemoContent: TCheckBox
          Left = 144
          Top = 86
          Width = 169
          Height = 17
          Caption = 'FGridShowMemoContent'
          TabOrder = 4
        end
        object PGridNullBGColorEnabled: TCheckBox
          Left = 144
          Top = 120
          Width = 30
          Height = 17
          TabOrder = 5
        end
        object PGridNullBGColor: TPanel_Ext
          Left = 162
          Top = 118
          Width = 49
          Height = 21
          BevelOuter = bvLowered
          ParentBackground = False
          TabOrder = 6
          OnClick = PGridNullBGColorClick
        end
        object FGridRowBGColorEnabled: TCheckBox
          Left = 144
          Top = 174
          Width = 169
          Height = 17
          Caption = 'FGridRowBGColorEnabled'
          TabOrder = 8
        end
        object FGridCurrRowBGColorEnabled: TCheckBox
          Left = 144
          Top = 208
          Width = 30
          Height = 17
          TabOrder = 9
        end
        object PGridCurrRowBGColor: TPanel_Ext
          Left = 162
          Top = 206
          Width = 49
          Height = 21
          BevelOuter = bvLowered
          ParentBackground = False
          TabOrder = 10
          OnClick = PGridCurrRowBGColorClick
        end
      end
    end
    object TSEditor: TTabSheet
      Caption = 'TSEditor'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GEditor: TGroupBox_Ext
        Left = 4
        Top = 4
        Width = 345
        Height = 281
        Caption = 'GEditor'
        TabOrder = 0
        object FLEditorFont: TLabel
          Left = 8
          Top = 19
          Width = 60
          Height = 13
          Caption = 'FLEditorFont'
          FocusControl = FEditorFont
        end
        object FLEditorLinenumbers: TLabel
          Left = 8
          Top = 53
          Width = 99
          Height = 13
          Caption = 'FLEditorLinenumbers'
          FocusControl = FEditorLinenumbers
        end
        object FLEditorTabWidth: TLabel
          Left = 8
          Top = 159
          Width = 86
          Height = 13
          Caption = 'FLEditorTabWidth'
          FocusControl = FEditorTabWidth
        end
        object FLEditorRightEdge: TLabel
          Left = 8
          Top = 193
          Width = 89
          Height = 13
          Caption = 'FLEditorRightEdge'
          FocusControl = FEditorRightEdge
        end
        object FLEditorCompletition: TLabel
          Left = 8
          Top = 224
          Width = 96
          Height = 13
          Caption = 'FLEditorCompletition'
          FocusControl = FEditorCompletitionEnabled
        end
        object FLEditorCurrRowBGColor: TLabel
          Left = 8
          Top = 252
          Width = 119
          Height = 13
          Caption = 'FLEditorCurrRowBGColor'
        end
        object FLEditorCompletitionTime: TLabel
          Left = 224
          Top = 224
          Width = 119
          Height = 13
          Caption = 'FLEditorCompletitionTime'
        end
        object FLEditorAutoIndent: TLabel
          Left = 8
          Top = 81
          Width = 91
          Height = 13
          Caption = 'FLEditorAutoIndent'
        end
        object FEditorTabWidthCharacters: TLabel
          Left = 191
          Top = 159
          Width = 131
          Height = 13
          Caption = 'FEditorTabWidthCharacters'
        end
        object FLEditorRightEdgeCharacters: TLabel
          Left = 206
          Top = 193
          Width = 140
          Height = 13
          Caption = 'FLEditorRightEdgeCharacters'
        end
        object FEditorCurrRowBGColorEnabled: TCheckBox
          Left = 144
          Top = 251
          Width = 19
          Height = 17
          TabOrder = 12
        end
        object FEditorFont: TEdit
          Left = 144
          Top = 16
          Width = 121
          Height = 21
          ReadOnly = True
          TabOrder = 1
          Text = 'FEditorFont'
          OnClick = FBEditorFontClick
          OnKeyPress = FEditorFontKeyPress
        end
        object PEditorFont: TPanel_Ext
          Left = 112
          Top = 16
          Width = 21
          Height = 21
          Caption = 'PEditorFont'
          Enabled = False
          ParentBackground = False
          TabOrder = 0
          Visible = False
        end
        object FEditorLinenumbers: TCheckBox
          Left = 144
          Top = 52
          Width = 193
          Height = 17
          Caption = 'FEditorLinenumbers'
          TabOrder = 2
        end
        object FEditorTabWidth: TEdit
          Left = 144
          Top = 156
          Width = 25
          Height = 21
          TabOrder = 6
          Text = '4'
        end
        object FEditorAutoIndent: TCheckBox
          Left = 144
          Top = 80
          Width = 193
          Height = 17
          Caption = 'FEditorAutoIndent'
          TabOrder = 3
        end
        object FEditorRightEdge: TEdit
          Left = 144
          Top = 190
          Width = 41
          Height = 21
          TabOrder = 8
          Text = '80'
        end
        object FUDEditorRightEdge: TUpDown
          Left = 185
          Top = 190
          Width = 15
          Height = 21
          Associate = FEditorRightEdge
          Max = 1024
          Position = 80
          TabOrder = 9
        end
        object FUDEditorTabWidth: TUpDown
          Left = 169
          Top = 156
          Width = 15
          Height = 21
          Associate = FEditorTabWidth
          Min = 1
          Position = 4
          TabOrder = 7
        end
        object FEditorTabToSpaces: TCheckBox
          Left = 144
          Top = 128
          Width = 193
          Height = 17
          Caption = 'FEditorTabToSpaces'
          TabOrder = 5
        end
        object FEditorCompletitionEnabled: TCheckBox
          Left = 144
          Top = 223
          Width = 19
          Height = 17
          TabOrder = 10
        end
        object FEditorTabAccepted: TCheckBox
          Left = 144
          Top = 104
          Width = 193
          Height = 17
          Caption = 'FEditorTabAccepted'
          TabOrder = 4
        end
        object PEditorCurrRowBGColor: TPanel_Ext
          Left = 162
          Top = 250
          Width = 49
          Height = 21
          BevelOuter = bvLowered
          ParentBackground = False
          TabOrder = 13
          OnClick = PEditorCurrRowBGColorClick
        end
        object FEditorCompletitionTime: TEdit
          Left = 162
          Top = 221
          Width = 41
          Height = 21
          TabOrder = 11
          Text = '1000'
        end
        object FUDEditorCompletitionTime: TUpDown
          Left = 203
          Top = 221
          Width = 15
          Height = 21
          Associate = FEditorCompletitionTime
          Max = 5000
          Increment = 100
          Position = 1000
          TabOrder = 14
        end
      end
    end
    object TSLog: TTabSheet
      Caption = 'TSLog'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GLog: TGroupBox_Ext
        Left = 4
        Top = 4
        Width = 345
        Height = 141
        Caption = 'GLog'
        TabOrder = 0
        object FLLogFont: TLabel
          Left = 8
          Top = 19
          Width = 51
          Height = 13
          Caption = 'FLLogFont'
          FocusControl = FLogFont
        end
        object FLLogLinenumbers: TLabel
          Left = 8
          Top = 53
          Width = 90
          Height = 13
          Caption = 'FLLogLinenumbers'
        end
        object FLLogSize: TLabel
          Left = 8
          Top = 111
          Width = 50
          Height = 13
          Caption = 'FLLogSize'
          FocusControl = FLogSize
        end
        object FL2LogSize: TLabel
          Left = 206
          Top = 112
          Width = 14
          Height = 13
          Caption = 'KB'
        end
        object PLogFont: TPanel_Ext
          Left = 112
          Top = 16
          Width = 21
          Height = 21
          Caption = 'PLogFont'
          Enabled = False
          ParentBackground = False
          TabOrder = 0
          Visible = False
        end
        object FLogFont: TEdit
          Left = 144
          Top = 16
          Width = 121
          Height = 21
          ReadOnly = True
          TabOrder = 1
          Text = 'FLogFont'
          OnClick = FBLogFontClick
          OnKeyPress = FLogFontKeyPress
        end
        object FLogSize: TEdit
          Left = 144
          Top = 108
          Width = 43
          Height = 21
          TabOrder = 4
          Text = '100'
        end
        object FLogTime: TCheckBox
          Left = 144
          Top = 52
          Width = 198
          Height = 17
          Caption = 'FLogTime'
          TabOrder = 2
        end
        object FLogResult: TCheckBox
          Left = 144
          Top = 76
          Width = 198
          Height = 17
          Caption = 'FLogResult'
          TabOrder = 3
        end
        object FUDLogSize: TUpDown
          Left = 187
          Top = 108
          Width = 15
          Height = 21
          Associate = FLogSize
          Max = 1000
          Increment = 100
          Position = 100
          TabOrder = 5
        end
      end
    end
    object TSHighlighter: TTabSheet
      Caption = 'TSHighlighter'
      ImageIndex = 4
      OnShow = TSHighlighterShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GColors: TGroupBox_Ext
        Left = 4
        Top = 4
        Width = 345
        Height = 289
        Caption = 'GColors'
        TabOrder = 0
        object FBackground: TCheckBox
          Left = 144
          Top = 119
          Width = 89
          Height = 17
          Caption = 'FBackground'
          TabOrder = 6
          OnClick = FBackgroundClick
          OnKeyPress = FBackgroundKeyPress
        end
        object PQuery: TPanel_Ext
          Left = 8
          Top = 172
          Width = 329
          Height = 109
          BevelOuter = bvLowered
          Caption = 'PQuery'
          ParentBackground = False
          TabOrder = 8
          object FPreview: TSynMemo
            Left = 1
            Top = 1
            Width = 327
            Height = 107
            Align = alClient
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 0
            TabStop = False
            BorderStyle = bsNone
            Gutter.AutoSize = True
            Gutter.DigitCount = 2
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.LeftOffset = 0
            Gutter.ShowLineNumbers = True
            Gutter.Width = 0
            Highlighter = Highlighter
            Lines.Strings = (
              '# Create Table'
              'CREATE TABLE `NewTable` ('
              '  `name` char(64) binary NULL'
              ') TYPE=MyISAM COMMENT='#39'Test'#39';'
              '/*!40100 SET NAMES latin1;*/')
            Options = [eoAutoIndent, eoDragDropEditing, eoGroupUndo, eoHideShowScrollbars, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
            ReadOnly = True
            RightEdge = -1
            ScrollHintFormat = shfTopToBottom
          end
        end
        object FStyles: TListView
          Left = 8
          Top = 16
          Width = 125
          Height = 145
          Columns = <
            item
              Width = -1
              WidthType = (
                -1)
            end>
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          TabOrder = 0
          ViewStyle = vsReport
          OnSelectItem = FStylesSelectItem
        end
        object FBForeground: TButton
          Left = 233
          Top = 88
          Width = 21
          Height = 21
          Caption = #183#183#183
          TabOrder = 5
          OnClick = FBForegroundClick
        end
        object FBBackground: TButton
          Left = 233
          Top = 116
          Width = 21
          Height = 21
          Caption = #183#183#183
          TabOrder = 7
          OnClick = FBBackgroundClick
        end
        object FBold: TCheckBox
          Left = 144
          Top = 16
          Width = 177
          Height = 17
          Caption = 'FBold'
          TabOrder = 1
          OnClick = FBoldClick
        end
        object FItalic: TCheckBox
          Left = 144
          Top = 36
          Width = 177
          Height = 17
          Caption = 'FItalic'
          TabOrder = 2
          OnClick = FItalicClick
        end
        object FUnderline: TCheckBox
          Left = 144
          Top = 56
          Width = 177
          Height = 17
          Caption = 'FUnderline'
          TabOrder = 3
          OnClick = FUnderlineClick
        end
        object FForeground: TCheckBox
          Left = 144
          Top = 91
          Width = 89
          Height = 17
          Caption = 'FForeground'
          TabOrder = 4
          OnClick = FForegroundClick
          OnKeyPress = FForegroundKeyPress
        end
        object Sizer: TCheckBox
          Left = 144
          Top = 144
          Width = 17
          Height = 17
          TabOrder = 9
          Visible = False
        end
      end
    end
    object TSExtras: TTabSheet
      Caption = 'TSExtras'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GAssociate: TGroupBox_Ext
        Left = 4
        Top = 4
        Width = 345
        Height = 45
        Caption = 'GAssociate'
        TabOrder = 0
        object FLAssociate: TLabel
          Left = 8
          Top = 17
          Width = 58
          Height = 13
          Caption = 'FLAssociate'
        end
        object FAssociateSQL: TCheckBox
          Left = 144
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FAssociateSQL'
          TabOrder = 0
        end
      end
    end
    object TSUpdates: TTabSheet
      Caption = 'TSUpdates'
      ImageIndex = 7
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GUpdates: TGroupBox_Ext
        Left = 4
        Top = 4
        Width = 345
        Height = 93
        Caption = 'GUpdates'
        TabOrder = 0
        object FLUpdateCheck: TLabel
          Left = 8
          Top = 17
          Width = 78
          Height = 13
          Caption = 'FLUpdateCheck'
        end
        object FUpdateCheckNever: TRadioButton
          Left = 144
          Top = 16
          Width = 193
          Height = 17
          Caption = 'FUpdateCheckNever'
          TabOrder = 0
        end
        object FUpdateCheckStartup: TRadioButton
          Left = 144
          Top = 40
          Width = 193
          Height = 17
          Caption = 'FUpdateCheckStartup'
          TabOrder = 1
        end
        object FUpdateCheckDaily: TRadioButton
          Left = 144
          Top = 64
          Width = 193
          Height = 17
          Caption = 'FUpdateCheckDaily'
          TabOrder = 2
        end
      end
    end
  end
  object FBHelp: TButton
    Left = 8
    Top = 376
    Width = 75
    Height = 25
    Caption = 'FBHelp'
    TabOrder = 1
    OnClick = FBHelpClick
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdAnsiOnly, fdEffects, fdForceFontExist, fdNoOEMFonts, fdNoSimulations]
    Left = 88
    Top = 368
  end
  object Highlighter: TSynSQLSyn
    SQLDialect = sqlMySQL
    Left = 152
    Top = 368
  end
  object ColorDialog: TColorDialog
    Left = 120
    Top = 368
  end
end
