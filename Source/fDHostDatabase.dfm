object DHostDatabase: TDHostDatabase
  Left = 616
  Top = 226
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DHostDatabase'
  ClientHeight = 289
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object FBOk: TButton
    Left = 351
    Top = 256
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object FBCancel: TButton
    Left = 439
    Top = 256
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 3
  end
  object GWhat: TGroupBox_Ext
    Left = 8
    Top = 8
    Width = 217
    Height = 86
    Caption = 'GWhat'
    TabOrder = 0
    object FDatabase: TRadioButton
      Left = 8
      Top = 50
      Width = 73
      Height = 17
      Caption = 'FDatabase'
      TabOrder = 1
      OnClick = FDatabaseClick
    end
    object FDatabases: TComboBox_Ext
      Left = 96
      Top = 48
      Width = 113
      Height = 21
      TabOrder = 2
      OnChange = FDatabasesChange
      OnDropDown = FDatabasesDropDown
    end
    object FAll: TRadioButton
      Left = 8
      Top = 22
      Width = 105
      Height = 17
      Caption = 'FAll'
      TabOrder = 0
      OnClick = FAllClick
    end
  end
  object GRights: TGroupBox_Ext
    Left = 232
    Top = 8
    Width = 281
    Height = 225
    Caption = 'GRights'
    TabOrder = 1
    object FSelect: TCheckBox
      Left = 8
      Top = 16
      Width = 81
      Height = 17
      Caption = 'FSelect'
      TabOrder = 0
    end
    object FInsert: TCheckBox
      Left = 96
      Top = 16
      Width = 81
      Height = 17
      Caption = 'FInsert'
      TabOrder = 1
    end
    object FUpdate: TCheckBox
      Left = 184
      Top = 16
      Width = 81
      Height = 17
      Caption = 'FUpdate'
      TabOrder = 2
    end
    object FDelete: TCheckBox
      Left = 8
      Top = 40
      Width = 81
      Height = 17
      Caption = 'FDelete'
      TabOrder = 3
    end
    object FReferences: TCheckBox
      Left = 96
      Top = 40
      Width = 81
      Height = 17
      Caption = 'FReferences'
      TabOrder = 4
    end
    object FCreate: TCheckBox
      Left = 8
      Top = 72
      Width = 81
      Height = 17
      Caption = 'FCreate'
      TabOrder = 5
    end
    object FDrop: TCheckBox
      Left = 96
      Top = 72
      Width = 81
      Height = 17
      Caption = 'FDrop'
      TabOrder = 6
    end
    object FAlter: TCheckBox
      Left = 184
      Top = 72
      Width = 81
      Height = 17
      Caption = 'FAlter'
      TabOrder = 7
    end
    object FIndex: TCheckBox
      Left = 8
      Top = 104
      Width = 81
      Height = 17
      Caption = 'FIndex'
      TabOrder = 8
    end
    object FLockTable: TCheckBox
      Left = 8
      Top = 136
      Width = 81
      Height = 17
      Caption = 'FLockTable'
      TabOrder = 9
    end
    object FTmpTable: TCheckBox
      Left = 96
      Top = 136
      Width = 81
      Height = 17
      Caption = 'FTmpTable'
      TabOrder = 10
    end
    object FGrant: TCheckBox
      Left = 8
      Top = 200
      Width = 81
      Height = 17
      Caption = 'FGrant'
      TabOrder = 13
    end
    object FCreateView: TCheckBox
      Left = 8
      Top = 168
      Width = 81
      Height = 17
      Caption = 'FCreateView'
      TabOrder = 11
    end
    object FShowView: TCheckBox
      Left = 96
      Top = 168
      Width = 81
      Height = 17
      Caption = 'FShowView'
      TabOrder = 12
    end
  end
end
