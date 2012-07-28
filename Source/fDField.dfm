object DField: TDField
  Left = 764
  Top = 341
  BorderStyle = bsDialog
  Caption = 'DField'
  ClientHeight = 433
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
    433)
  PixelsPerInch = 106
  TextHeight = 13
  object PSQLWait: TPanel
    Left = 8
    Top = 8
    Width = 321
    Height = 373
    Cursor = crHourGlass
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'PSQLWait'
    TabOrder = 0
    Visible = False
  end
  object GBasics: TGroupBox_Ext
    Left = 8
    Top = 8
    Width = 321
    Height = 289
    Anchors = [akLeft, akTop, akRight]
    Caption = 'GBasics'
    TabOrder = 1
    DesignSize = (
      321
      289)
    object FLFormatDecimals: TLabel
      Left = 8
      Top = 116
      Width = 87
      Height = 13
      Caption = 'FLFormatDecimals'
      FocusControl = FFormatSize
    end
    object FLFormat: TLabel
      Left = 8
      Top = 116
      Width = 44
      Height = 13
      Caption = 'FLFormat'
    end
    object FLName: TLabel
      Left = 8
      Top = 52
      Width = 40
      Height = 13
      Caption = 'FLName'
      FocusControl = FName
    end
    object FLType: TLabel
      Left = 8
      Top = 84
      Width = 36
      Height = 13
      Caption = 'FLType'
      FocusControl = FFieldType
    end
    object FLFormatSize: TLabel
      Left = 8
      Top = 116
      Width = 64
      Height = 13
      Caption = 'FLFormatSize'
      FocusControl = FFormatSize
    end
    object FLDefault: TLabel
      Left = 8
      Top = 148
      Width = 46
      Height = 13
      Caption = 'FLDefault'
      FocusControl = FDefault
    end
    object FLPosition: TLabel
      Left = 8
      Top = 20
      Width = 49
      Height = 13
      Caption = 'FLPosition'
      FocusControl = FPosition
    end
    object FLComment: TLabel
      Left = 8
      Top = 259
      Width = 56
      Height = 13
      Caption = 'FLComment'
    end
    object FLCharset: TLabel
      Left = 8
      Top = 201
      Width = 48
      Height = 13
      Caption = 'FLCharset'
      FocusControl = FCharset
    end
    object FLCollation: TLabel
      Left = 8
      Top = 227
      Width = 52
      Height = 13
      Caption = 'FLCollation'
      FocusControl = FCollation
    end
    object FLUpdateTime: TLabel
      Left = 8
      Top = 213
      Width = 70
      Height = 13
      Caption = 'FLUpdateTime'
    end
    object FDefaultEnum: TComboBox_Ext
      Left = 120
      Top = 144
      Width = 189
      Height = 21
      Style = csDropDownList
      TabOrder = 17
      OnChange = FBOkCheckEnabled
    end
    object FDefaultSet: TListBox
      Left = 120
      Top = 144
      Width = 189
      Height = 41
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 18
      OnClick = FBOkCheckEnabled
    end
    object FRDefaultNull: TRadioButton
      Left = 120
      Top = 147
      Width = 188
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'FRDefaultNull'
      TabOrder = 11
      OnClick = FRDefaultClick
    end
    object FFormatTimestamp: TComboBox_Ext
      Left = 120
      Top = 112
      Width = 137
      Height = 21
      Style = csDropDownList
      TabOrder = 8
      OnChange = FFormatTimestampChange
      Items.Strings = (
        'YYYYMMDDHHMMSS'
        'YYMMDDHHMMSS'
        'YYYYMMDD'
        'YYMMDD')
    end
    object FDefault: TEdit
      Left = 135
      Top = 164
      Width = 174
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 13
      Text = 'FDefault'
      OnChange = FDefaultChange
      OnEnter = FDefaultEnter
      OnExit = FDefaultExit
    end
    object FFormatDate: TEdit
      Left = 120
      Top = 112
      Width = 137
      Height = 21
      TabOrder = 7
      Text = 'FFormatDateTime'
    end
    object FFormatUnion: TEdit
      Left = 120
      Top = 112
      Width = 189
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 10
      OnChange = FFormatUnionChange
    end
    object FFormatYear: TComboBox_Ext
      Left = 120
      Top = 112
      Width = 57
      Height = 21
      Style = csDropDownList
      TabOrder = 9
      OnChange = FFormatYearChange
      Items.Strings = (
        'YYYY'
        'YY')
    end
    object FFormatSize: TEdit
      Left = 120
      Top = 112
      Width = 41
      Height = 21
      TabOrder = 3
      Text = '0'
      OnChange = FFormatSizeChange
      OnExit = FFormatSizeExit
    end
    object FUDFormatSize: TUpDown
      Left = 161
      Top = 112
      Width = 16
      Height = 21
      Associate = FFormatSize
      Max = 30
      TabOrder = 4
      Thousands = False
      OnClick = FUDClick
    end
    object FFormatDecimals: TEdit
      Left = 184
      Top = 112
      Width = 33
      Height = 21
      TabOrder = 5
      Text = '0'
      OnChange = FFormatDecimalsChange
    end
    object FUDFormatDecimals: TUpDown
      Left = 217
      Top = 112
      Width = 15
      Height = 21
      Associate = FFormatDecimals
      Max = 5
      TabOrder = 6
      Thousands = False
      OnClick = FUDClick
    end
    object FFieldType: TComboBox_Ext
      Left = 120
      Top = 80
      Width = 129
      Height = 21
      Style = csDropDownList
      DropDownCount = 15
      TabOrder = 2
      OnChange = FFieldTypeChange
      OnDrawItem = FFieldTypeDrawItem
      OnExit = FFieldTypeExit
      Items.Strings = (
        'TinyInt'
        'SmallInt'
        'MediumInt'
        'Int'
        'BigInt'
        'Float'
        'Double'
        'Decimal'
        'Date'
        'DateTime'
        'TimeStamp'
        'Time'
        'Year'
        'Char'
        'VarChar'
        'Binary'
        'VarBinary'
        'TinyText'
        'Text'
        'MediumText'
        'LongText'
        'TinyBlob'
        'Blob'
        'MediumBlob'
        'LongBlob'
        'Enum'
        'Set'
        'Geometry'
        'Point'
        'LineString'
        'Polygon'
        'MultiPoint'
        'MultiLineString'
        'MultiPolygon'
        'GeometryCollection')
    end
    object FName: TEdit
      Left = 120
      Top = 48
      Width = 145
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 64
      TabOrder = 1
      Text = 'FName'
      OnChange = FBOkCheckEnabled
    end
    object FPosition: TComboBox_Ext
      Left = 120
      Top = 16
      Width = 189
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = FBOkCheckEnabled
    end
    object FComment: TEdit
      Left = 120
      Top = 256
      Width = 189
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 256
      TabOrder = 21
      Text = 'FComment'
      OnChange = FBOkCheckEnabled
    end
    object FCharset: TComboBox_Ext
      Left = 120
      Top = 198
      Width = 89
      Height = 21
      Style = csDropDownList
      TabOrder = 19
      OnChange = FCharsetChange
    end
    object FCollation: TComboBox_Ext
      Left = 120
      Top = 224
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 20
      OnChange = FBOkCheckEnabled
      OnDropDown = FCollationDropDown
    end
    object FRDefault: TRadioButton
      Left = 120
      Top = 166
      Width = 14
      Height = 17
      TabOrder = 12
      OnClick = FRDefaultClick
    end
    object FRDefaultInsertTime: TRadioButton
      Left = 120
      Top = 185
      Width = 188
      Height = 17
      Caption = 'FRDefaultInsertTime'
      TabOrder = 14
      OnClick = FRDefaultClick
    end
    object FUpdateTime: TCheckBox
      Left = 120
      Top = 212
      Width = 188
      Height = 17
      Caption = 'FUpdateTime'
      TabOrder = 16
      OnClick = FRDefaultClick
    end
    object FRDefaultAutoIncrement: TRadioButton
      Left = 120
      Top = 185
      Width = 188
      Height = 17
      Caption = 'FRDefaultAutoIncrement'
      TabOrder = 15
      OnClick = FRDefaultClick
      OnKeyDown = FRDefaultNullKeyDown
    end
  end
  object FBOk: TButton
    Left = 168
    Top = 400
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object FBCancel: TButton
    Left = 256
    Top = 400
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 5
  end
  object GAttributes: TGroupBox_Ext
    Left = 8
    Top = 300
    Width = 321
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'GAttributes'
    TabOrder = 2
    object FFlagNational: TCheckBox
      Left = 8
      Top = 36
      Width = 145
      Height = 17
      Caption = 'FFlagNational'
      TabOrder = 4
      OnClick = FBOkCheckEnabled
      OnKeyPress = FFlagNationalKeyPress
    end
    object FFlagNullAllowed: TCheckBox
      Left = 8
      Top = 16
      Width = 145
      Height = 17
      Caption = 'FFlagNullAllowed'
      TabOrder = 0
      OnClick = FFlagNullAllowedClick
      OnKeyPress = FFlagNullAllowedKeyPress
    end
    object FFlagUnsigned: TCheckBox
      Left = 160
      Top = 16
      Width = 145
      Height = 17
      Caption = 'FFlagUnsigned'
      TabOrder = 1
      OnClick = FFlagUnsignedClick
      OnKeyPress = FFlagUnsignedKeyPress
    end
    object FFlagBinary: TCheckBox
      Left = 160
      Top = 16
      Width = 145
      Height = 17
      Caption = 'FFlagBinary'
      TabOrder = 3
      OnClick = FFlagCharClick
      OnKeyPress = FFlagCharPress
    end
    object FFlagZerofill: TCheckBox
      Left = 160
      Top = 36
      Width = 145
      Height = 17
      Caption = 'FFlagZerofill'
      TabOrder = 2
      OnClick = FFlagZerofillClick
      OnKeyPress = FFlagZerofillKeyPress
    end
    object FFlagAscii: TCheckBox
      Left = 160
      Top = 36
      Width = 145
      Height = 17
      Caption = 'FFlagAscii'
      TabOrder = 5
      OnClick = FFlagCharClick
      OnKeyPress = FFlagCharPress
    end
    object FFlagUnicode: TCheckBox
      Left = 160
      Top = 56
      Width = 145
      Height = 17
      Caption = 'FFlagUnicode'
      TabOrder = 6
      OnClick = FFlagCharClick
      OnKeyPress = FFlagCharPress
    end
  end
  object FBHelp: TButton
    Left = 8
    Top = 400
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'FBHelp'
    TabOrder = 3
    OnClick = FBHelpClick
  end
end
