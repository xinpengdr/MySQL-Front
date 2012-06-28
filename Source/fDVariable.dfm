object DVariable: TDVariable
  Left = 433
  Top = 187
  HelpContext = 1060
  BorderStyle = bsDialog
  Caption = 'DVariable'
  ClientHeight = 153
  ClientWidth = 249
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
  DesignSize = (
    249
    153)
  PixelsPerInch = 106
  TextHeight = 13
  object PSQLWait: TPanel
    Left = 8
    Top = 8
    Width = 233
    Height = 89
    Cursor = crSQLWait
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'PSQLWait'
    TabOrder = 0
    Visible = False
  end
  object FBOk: TButton
    Left = 80
    Top = 120
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object FBCancel: TButton
    Left = 168
    Top = 120
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 3
  end
  object GroupBox: TGroupBox_Ext
    Left = 8
    Top = 8
    Width = 233
    Height = 89
    Caption = 'GroupBox'
    TabOrder = 1
    object FLValue: TLabel
      Left = 8
      Top = 16
      Width = 39
      Height = 13
      Caption = 'FLValue'
      FocusControl = FValue
    end
    object FLModify: TLabel
      Left = 8
      Top = 65
      Width = 43
      Height = 13
      Caption = 'FLModify'
    end
    object FGlobal: TRadioButton
      Left = 64
      Top = 64
      Width = 73
      Height = 17
      Caption = 'FGlobal'
      TabOrder = 1
      OnClick = FGlobalClick
    end
    object FSession: TRadioButton
      Left = 144
      Top = 64
      Width = 81
      Height = 17
      Caption = 'FSession'
      TabOrder = 2
      OnClick = FSessionClick
    end
    object FValue: TEdit
      Left = 8
      Top = 32
      Width = 217
      Height = 21
      TabOrder = 0
      Text = 'FValue'
      OnChange = FBOkButtonEnable
    end
  end
end
