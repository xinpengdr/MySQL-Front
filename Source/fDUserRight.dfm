object DUserRight: TDUserRight
  Left = 543
  Top = 270
  BorderStyle = bsDialog
  Caption = 'DUserRight'
  ClientHeight = 450
  ClientWidth = 585
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object FBCancel: TButton
    Left = 501
    Top = 416
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 3
  end
  object FBOk: TButton
    Left = 413
    Top = 416
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object GWhat: TGroupBox_Ext
    Left = 8
    Top = 8
    Width = 233
    Height = 261
    Caption = 'GWhat'
    TabOrder = 0
    object FDatabase: TRadioButton
      Left = 8
      Top = 60
      Width = 81
      Height = 17
      Caption = 'FDatabase'
      TabOrder = 1
      OnClick = FDatabaseClick
    end
    object FDatabases: TComboBox_Ext
      Left = 96
      Top = 58
      Width = 129
      Height = 21
      TabOrder = 2
      OnChange = FDatabasesChange
      OnDropDown = FDatabasesDropDown
    end
    object FTable: TRadioButton
      Left = 8
      Top = 100
      Width = 81
      Height = 17
      Caption = 'FTable'
      TabOrder = 3
      OnClick = FTableClick
    end
    object FTables: TComboBox_Ext
      Left = 96
      Top = 98
      Width = 129
      Height = 21
      TabOrder = 4
      OnChange = FTablesChange
      OnDropDown = FTablesDropDown
    end
    object FField: TRadioButton
      Left = 8
      Top = 138
      Width = 81
      Height = 17
      Caption = 'FField'
      TabOrder = 5
      OnClick = FFieldClick
    end
    object FAll: TRadioButton
      Left = 8
      Top = 22
      Width = 217
      Height = 17
      Caption = 'FAll'
      TabOrder = 0
      OnClick = FAllClick
    end
    object FFields: TComboBox_Ext
      Left = 96
      Top = 136
      Width = 129
      Height = 21
      TabOrder = 6
      OnChange = FFieldsChange
      OnDropDown = FFieldsDropDown
    end
    object FProcedures: TComboBox
      Left = 96
      Top = 184
      Width = 129
      Height = 21
      TabOrder = 8
      OnChange = FProceduresChange
      OnDropDown = FProceduresDropDown
    end
    object FProcedure: TRadioButton
      Left = 8
      Top = 186
      Width = 81
      Height = 17
      Caption = 'FProcedure'
      TabOrder = 7
      OnClick = FRoutineClick
    end
    object FFunction: TRadioButton
      Left = 8
      Top = 224
      Width = 81
      Height = 17
      Caption = 'FFunction'
      TabOrder = 9
      OnClick = FRoutineClick
    end
    object FFunctions: TComboBox
      Left = 96
      Top = 222
      Width = 129
      Height = 21
      TabOrder = 10
      OnChange = FFunctionsChange
      OnDropDown = FFunctionsDropDown
    end
  end
  object GRights: TGroupBox_Ext
    Left = 248
    Top = 8
    Width = 329
    Height = 385
    Caption = 'GRights'
    TabOrder = 1
    object FSelect: TCheckBox
      Left = 8
      Top = 16
      Width = 97
      Height = 17
      Caption = 'FSelect'
      TabOrder = 0
      OnClick = FBOkCheckEnabled
    end
    object FDelete: TCheckBox
      Left = 8
      Top = 40
      Width = 97
      Height = 17
      Caption = 'FDelete'
      TabOrder = 3
      OnClick = FBOkCheckEnabled
    end
    object FCreate: TCheckBox
      Left = 8
      Top = 72
      Width = 97
      Height = 17
      Caption = 'FCreate'
      TabOrder = 5
      OnClick = FBOkCheckEnabled
    end
    object FReferences: TCheckBox
      Left = 112
      Top = 40
      Width = 97
      Height = 17
      Caption = 'FReferences'
      TabOrder = 4
      OnClick = FBOkCheckEnabled
    end
    object FInsert: TCheckBox
      Left = 112
      Top = 16
      Width = 97
      Height = 17
      Caption = 'FInsert'
      TabOrder = 1
      OnClick = FBOkCheckEnabled
    end
    object FIndex: TCheckBox
      Left = 8
      Top = 96
      Width = 97
      Height = 17
      Caption = 'FIndex'
      TabOrder = 8
      OnClick = FBOkCheckEnabled
    end
    object FDrop: TCheckBox
      Left = 112
      Top = 72
      Width = 97
      Height = 17
      Caption = 'FDrop'
      TabOrder = 6
      OnClick = FBOkCheckEnabled
    end
    object FReload: TCheckBox
      Left = 8
      Top = 288
      Width = 97
      Height = 17
      Caption = 'FReload'
      TabOrder = 20
      OnClick = FBOkCheckEnabled
    end
    object FUpdate: TCheckBox
      Left = 216
      Top = 16
      Width = 97
      Height = 17
      Caption = 'FUpdate'
      TabOrder = 2
      OnClick = FBOkCheckEnabled
    end
    object FAlter: TCheckBox
      Left = 216
      Top = 72
      Width = 97
      Height = 17
      Caption = 'FAlter'
      TabOrder = 7
      OnClick = FBOkCheckEnabled
    end
    object FShutdown: TCheckBox
      Left = 8
      Top = 360
      Width = 97
      Height = 17
      Caption = 'FShutdown'
      TabOrder = 26
      OnClick = FBOkCheckEnabled
    end
    object FProcess: TCheckBox
      Left = 112
      Top = 264
      Width = 97
      Height = 17
      Caption = 'FProcess'
      TabOrder = 19
      OnClick = FBOkCheckEnabled
    end
    object FFile: TCheckBox
      Left = 112
      Top = 288
      Width = 97
      Height = 17
      Caption = 'FFile'
      TabOrder = 21
      OnClick = FBOkCheckEnabled
    end
    object FCreateTempTable: TCheckBox
      Left = 112
      Top = 128
      Width = 97
      Height = 17
      Caption = 'FCreateTempTable'
      TabOrder = 10
      OnClick = FBOkCheckEnabled
    end
    object FExecute: TCheckBox
      Left = 8
      Top = 232
      Width = 97
      Height = 17
      Caption = 'FExecute'
      TabOrder = 17
      OnClick = FBOkCheckEnabled
    end
    object FLockTable: TCheckBox
      Left = 8
      Top = 128
      Width = 97
      Height = 17
      Caption = 'FLockTable'
      TabOrder = 9
      OnClick = FBOkCheckEnabled
    end
    object FReplClient: TCheckBox
      Left = 8
      Top = 312
      Width = 97
      Height = 17
      Caption = 'FReplClient'
      TabOrder = 22
      OnClick = FBOkCheckEnabled
    end
    object FReplSlave: TCheckBox
      Left = 112
      Top = 312
      Width = 97
      Height = 17
      Caption = 'FReplSlave'
      TabOrder = 23
      OnClick = FBOkCheckEnabled
    end
    object FSuper: TCheckBox
      Left = 112
      Top = 360
      Width = 97
      Height = 17
      Caption = 'FSuper'
      TabOrder = 27
      OnClick = FBOkCheckEnabled
    end
    object FShowDatabase: TCheckBox
      Left = 8
      Top = 264
      Width = 97
      Height = 17
      Caption = 'FShowDatabase'
      TabOrder = 18
      OnClick = FBOkCheckEnabled
    end
    object FGrant: TCheckBox
      Left = 8
      Top = 336
      Width = 97
      Height = 17
      Caption = 'FGrant'
      TabOrder = 24
      OnClick = FBOkCheckEnabled
    end
    object FCreateView: TCheckBox
      Left = 8
      Top = 160
      Width = 97
      Height = 17
      Caption = 'FCreateView'
      TabOrder = 11
      OnClick = FBOkCheckEnabled
    end
    object FShowView: TCheckBox
      Left = 112
      Top = 160
      Width = 97
      Height = 17
      Caption = 'FShowView'
      TabOrder = 12
      OnClick = FBOkCheckEnabled
    end
    object FCreateRoutine: TCheckBox
      Left = 8
      Top = 184
      Width = 97
      Height = 17
      Caption = 'FCreateRoutine'
      TabOrder = 13
      OnClick = FBOkCheckEnabled
    end
    object FAlterRoutine: TCheckBox
      Left = 112
      Top = 184
      Width = 97
      Height = 17
      Caption = 'FAlterRoutine'
      TabOrder = 14
      OnClick = FBOkCheckEnabled
    end
    object FCreateUser: TCheckBox
      Left = 112
      Top = 336
      Width = 97
      Height = 17
      Caption = 'FCreateUser'
      TabOrder = 25
      OnClick = FBOkCheckEnabled
    end
    object FEvent: TCheckBox
      Left = 112
      Top = 208
      Width = 97
      Height = 17
      Caption = 'FEvent'
      TabOrder = 16
      OnClick = FBOkCheckEnabled
    end
    object FTrigger: TCheckBox
      Left = 8
      Top = 208
      Width = 97
      Height = 17
      Caption = 'FTrigger'
      TabOrder = 15
      OnClick = FBOkCheckEnabled
    end
  end
end
