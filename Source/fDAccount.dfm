object DAccount: TDAccount
  Left = 616
  Top = 338
  HelpContext = 1065
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DAccount'
  ClientHeight = 338
  ClientWidth = 329
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
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object FBOk: TButton
    Left = 158
    Top = 304
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object FBCancel: TButton
    Left = 246
    Top = 304
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
    Width = 313
    Height = 277
    ActivePage = TSLogin
    HotTrack = True
    MultiLine = True
    TabOrder = 0
    object TSBasics: TTabSheet
      Caption = 'TSBasics'
      OnShow = TSBasicsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GBasics: TGroupBox_Ext
        Left = 4
        Top = 4
        Width = 297
        Height = 85
        Caption = 'GBasics'
        TabOrder = 0
        object FLName: TLabel
          Left = 8
          Top = 19
          Width = 40
          Height = 13
          Caption = 'FLName'
        end
        object FLIcon: TLabel
          Left = 8
          Top = 53
          Width = 33
          Height = 13
          Caption = 'FLIcon'
        end
        object FName: TEdit
          Left = 128
          Top = 16
          Width = 157
          Height = 21
          MaxLength = 50
          TabOrder = 0
          Text = 'FName'
          OnChange = FEditChange
        end
        object FBIcon: TButton
          Left = 152
          Top = 50
          Width = 21
          Height = 21
          Caption = #183#183#183
          TabOrder = 1
          OnClick = FBIconClick
        end
        object PIcon: TPanel_Ext
          Left = 128
          Top = 51
          Width = 18
          Height = 18
          ParentBackground = False
          TabOrder = 2
          object FIcon: TImage
            Left = 1
            Top = 1
            Width = 16
            Height = 16
            Align = alClient
            OnClick = FIconClick
          end
        end
      end
    end
    object TSConnection: TTabSheet
      Caption = 'TSConnection'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GServer: TGroupBox_Ext
        Left = 4
        Top = 4
        Width = 297
        Height = 165
        Caption = 'GServer'
        TabOrder = 0
        object FLHost: TLabel
          Left = 8
          Top = 19
          Width = 34
          Height = 13
          Caption = 'FLHost'
          FocusControl = FHost
        end
        object FLPort: TLabel
          Left = 8
          Top = 43
          Width = 31
          Height = 13
          Caption = 'FLPort'
          FocusControl = FPort
        end
        object FLConnectionType: TLabel
          Left = 8
          Top = 75
          Width = 90
          Height = 13
          Caption = 'FLConnectionType'
          FocusControl = FConnectionType
        end
        object FLHTTPTunnelURI: TLabel
          Left = 8
          Top = 99
          Width = 93
          Height = 13
          Caption = 'FLHTTPTunnelURI'
          FocusControl = FHTTPTunnelURI
          Visible = False
        end
        object FLLibraryFilename: TLabel
          Left = 8
          Top = 99
          Width = 85
          Height = 13
          Caption = 'FLLibraryFilename'
          FocusControl = FLibraryFilename
          Visible = False
        end
        object FLCharset: TLabel
          Left = 8
          Top = 135
          Width = 48
          Height = 13
          Caption = 'FLCharset'
          FocusControl = FCharset
        end
        object FHTTPTunnelURI: TEdit
          Left = 128
          Top = 96
          Width = 161
          Height = 21
          TabOrder = 5
          Text = 'FHTTPTunnelURI'
          Visible = False
          OnChange = FHTTPTunnelURIChange
          OnEnter = FHTTPTunnelURIEnter
        end
        object FHost: TEdit
          Left = 128
          Top = 16
          Width = 157
          Height = 21
          MaxLength = 50
          TabOrder = 0
          Text = 'FHost'
          OnChange = FEditChange
          OnExit = FHostExit
        end
        object FPort: TEdit
          Left = 128
          Top = 40
          Width = 41
          Height = 21
          MaxLength = 5
          TabOrder = 1
          Text = '0'
        end
        object FConnectionType: TComboBox_Ext
          Left = 128
          Top = 72
          Width = 129
          Height = 21
          Style = csDropDownList
          TabOrder = 3
          OnChange = FConnectionTypeChange
        end
        object FLibraryFilename: TEdit
          Left = 128
          Top = 96
          Width = 97
          Height = 21
          TabOrder = 4
          Text = 'FLibraryFilename'
          Visible = False
        end
        object FUDPort: TUpDown
          Left = 169
          Top = 40
          Width = 16
          Height = 21
          Associate = FPort
          Max = 65535
          TabOrder = 2
          Thousands = False
        end
        object FCharset: TComboBox_Ext
          Left = 128
          Top = 132
          Width = 97
          Height = 21
          TabOrder = 6
          OnDropDown = FCharsetDropDown
        end
      end
    end
    object TSLogin: TTabSheet
      Caption = 'TSLogin'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GLogin: TGroupBox_Ext
        Left = 4
        Top = 4
        Width = 297
        Height = 125
        Caption = 'GLogin'
        TabOrder = 0
        object FLDatabase: TLabel
          Left = 8
          Top = 95
          Width = 58
          Height = 13
          Caption = 'FLDatabase'
          FocusControl = FDatabase
        end
        object FLUser: TLabel
          Left = 8
          Top = 19
          Width = 34
          Height = 13
          Caption = 'FLUser'
          FocusControl = FUser
        end
        object FLPassword: TLabel
          Left = 8
          Top = 51
          Width = 58
          Height = 13
          Caption = 'FLPassword'
          FocusControl = FPassword
        end
        object FDatabase: TEdit
          Left = 128
          Top = 92
          Width = 137
          Height = 21
          TabOrder = 2
          Text = 'FDatabase'
          OnChange = FEditChange
        end
        object FUser: TEdit
          Left = 128
          Top = 16
          Width = 89
          Height = 21
          MaxLength = 50
          TabOrder = 0
          Text = 'FUser'
          OnChange = FEditChange
        end
        object FPassword: TEdit
          Left = 128
          Top = 48
          Width = 89
          Height = 21
          MaxLength = 50
          TabOrder = 1
          Text = 'FPassword'
          OnChange = FEditChange
        end
        object FBDatabase: TButton
          Left = 266
          Top = 92
          Width = 21
          Height = 21
          Caption = #183#183#183
          TabOrder = 3
          OnClick = FBDatabaseClick
        end
      end
    end
    object TSStartup: TTabSheet
      Caption = 'TSStartup'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        305
        231)
      object FStartup: TSynMemo
        Left = 4
        Top = 8
        Width = 297
        Height = 217
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
      end
    end
    object TSDataBrowser: TTabSheet
      Caption = 'TSDataBrowser'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GBrowser: TGroupBox_Ext
        Left = 4
        Top = 4
        Width = 297
        Height = 117
        Caption = 'GBrowser'
        TabOrder = 0
        object FLDefaultSorting: TLabel
          Left = 8
          Top = 89
          Width = 79
          Height = 13
          Caption = 'FLDefaultSorting'
          FocusControl = FDefaultSorting
        end
        object FLLimit: TLabel
          Left = 8
          Top = 17
          Width = 33
          Height = 13
          Caption = 'FLLimit'
        end
        object FLimitRemember: TRadioButton
          Left = 128
          Top = 38
          Width = 161
          Height = 17
          Caption = 'FLimitRemember'
          TabOrder = 1
        end
        object FDefaultSorting: TCheckBox
          Left = 128
          Top = 88
          Width = 161
          Height = 17
          Caption = 'FDefaultSorting'
          TabOrder = 3
        end
        object FLimitOff: TRadioButton
          Left = 128
          Top = 16
          Width = 161
          Height = 17
          Caption = 'FLimitOff'
          TabOrder = 0
        end
        object FLimitOn: TRadioButton
          Left = 128
          Top = 60
          Width = 161
          Height = 17
          Caption = 'FLimitOn'
          TabOrder = 2
        end
      end
    end
    object TSDebug: TTabSheet
      Caption = 'TSDebug'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GDebug: TGroupBox_Ext
        Left = 4
        Top = 4
        Width = 297
        Height = 161
        Caption = 'GDebug'
        TabOrder = 0
        object FLCacheSize: TLabel
          Left = 8
          Top = 19
          Width = 63
          Height = 13
          Caption = 'FLCacheSize'
          FocusControl = FCacheSize
        end
        object FL2CacheSize: TLabel
          Left = 188
          Top = 19
          Width = 16
          Height = 13
          Caption = 'MB'
        end
        object FLCompression: TLabel
          Left = 8
          Top = 49
          Width = 72
          Height = 13
          Caption = 'FLCompression'
          FocusControl = FCompression
        end
        object FLMultiStatements: TLabel
          Left = 8
          Top = 69
          Width = 87
          Height = 13
          Caption = 'FLMultiStatements'
          FocusControl = FMultiStatements
          Transparent = False
        end
        object FLAsynchron: TLabel
          Left = 8
          Top = 89
          Width = 62
          Height = 13
          Caption = 'FLAsynchron'
          FocusControl = FAsynchron
        end
        object FLPrefetch: TLabel
          Left = 8
          Top = 133
          Width = 52
          Height = 13
          Caption = 'FLPrefetch'
          FocusControl = FPrefetch
        end
        object FLUseInformationSchema: TLabel
          Left = 8
          Top = 109
          Width = 122
          Height = 13
          Caption = 'FLUseInformationSchema'
          FocusControl = FUseInformationSchema
        end
        object FCacheSize: TEdit
          Left = 128
          Top = 16
          Width = 41
          Height = 21
          TabOrder = 0
          Text = '0'
        end
        object FUDCacheSize: TUpDown
          Left = 169
          Top = 16
          Width = 15
          Height = 21
          Associate = FCacheSize
          Max = 10240
          Increment = 10
          TabOrder = 2
        end
        object FCompression: TCheckBox
          Left = 128
          Top = 48
          Width = 161
          Height = 17
          Caption = 'FCompression'
          TabOrder = 1
        end
        object FMultiStatements: TCheckBox
          Left = 128
          Top = 68
          Width = 161
          Height = 17
          Caption = 'FMultiStatements'
          TabOrder = 3
        end
        object FAsynchron: TCheckBox
          Left = 128
          Top = 88
          Width = 161
          Height = 17
          Caption = 'FAsynchron'
          TabOrder = 4
        end
        object FPrefetch: TComboBox
          Left = 128
          Top = 130
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 6
        end
        object FUseInformationSchema: TCheckBox
          Left = 128
          Top = 108
          Width = 161
          Height = 17
          Caption = 'FUseInformationSchema'
          TabOrder = 5
        end
      end
    end
  end
  object FBHelp: TButton
    Left = 8
    Top = 304
    Width = 75
    Height = 25
    Caption = 'FBHelp'
    TabOrder = 1
    OnClick = FBHelpClick
  end
  object OpenDialog: TOpenDialog_Ext
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    EncodingIndex = -1
    EncodingLabel = '&Encoding:'
    Left = 96
    Top = 304
  end
  object MSource: TPopupMenu
    Left = 128
    Top = 304
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
