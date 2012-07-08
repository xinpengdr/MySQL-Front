object DAccount: TDAccount
  Left = 616
  Top = 338
  HelpContext = 1065
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DAccount'
  ClientHeight = 401
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object FBOk: TButton
    Left = 142
    Top = 368
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object FBCancel: TButton
    Left = 230
    Top = 368
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 3
  end
  object GBasics: TGroupBox_Ext
    Left = 8
    Top = 4
    Width = 297
    Height = 47
    Caption = 'GBasics'
    TabOrder = 4
    object FLName: TLabel
      Left = 8
      Top = 19
      Width = 40
      Height = 13
      Caption = 'FLName'
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
  end
  object GServer: TGroupBox_Ext
    Left = 8
    Top = 55
    Width = 297
    Height = 163
    Caption = 'GServer'
    TabOrder = 5
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
  object GLogin: TGroupBox_Ext
    Left = 8
    Top = 223
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
  object FBHelp: TButton
    Left = 8
    Top = 368
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
    Left = 80
    Top = 368
  end
  object MSource: TPopupMenu
    Left = 112
    Top = 368
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
