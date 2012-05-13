object DPartition: TDPartition
  Left = 738
  Top = 232
  BorderStyle = bsDialog
  Caption = 'DPartition'
  ClientHeight = 265
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object FBOk: TButton
    Left = 168
    Top = 232
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object FBCancel: TButton
    Left = 256
    Top = 232
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GBasics: TGroupBox_Ext
    Left = 8
    Top = 8
    Width = 321
    Height = 201
    Caption = 'GBasics'
    TabOrder = 2
    DesignSize = (
      321
      201)
    object FLName: TLabel
      Left = 8
      Top = 19
      Width = 40
      Height = 13
      Caption = 'FLName'
      FocusControl = FName
    end
    object FLExpression: TLabel
      Left = 8
      Top = 83
      Width = 63
      Height = 13
      Caption = 'FLExpression'
      FocusControl = FExpression
    end
    object FLMinRows: TLabel
      Left = 8
      Top = 115
      Width = 56
      Height = 13
      Caption = 'FLMinRows'
      FocusControl = FMinRows
    end
    object FLMaxRows: TLabel
      Left = 8
      Top = 139
      Width = 59
      Height = 13
      Caption = 'FLMaxRows'
      FocusControl = FMaxRows
    end
    object FLComment: TLabel
      Left = 8
      Top = 171
      Width = 56
      Height = 13
      Caption = 'FLComment'
      FocusControl = FComment
    end
    object FLEngine: TLabel
      Left = 8
      Top = 51
      Width = 45
      Height = 13
      Caption = 'FLEngine'
      Enabled = False
    end
    object FName: TEdit
      Left = 120
      Top = 16
      Width = 145
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 64
      TabOrder = 0
      Text = 'FName'
      OnChange = FBOkCheckEnabled
    end
    object FExpression: TEdit
      Left = 120
      Top = 80
      Width = 193
      Height = 21
      TabOrder = 2
      Text = 'FExpression'
      OnChange = FBOkCheckEnabled
    end
    object FMinRows: TEdit
      Left = 120
      Top = 112
      Width = 97
      Height = 21
      TabOrder = 3
      Text = '0'
      OnChange = FBOkCheckEnabled
    end
    object FMaxRows: TEdit
      Left = 120
      Top = 136
      Width = 97
      Height = 21
      TabOrder = 4
      Text = '0'
      OnChange = FBOkCheckEnabled
    end
    object FComment: TEdit
      Left = 120
      Top = 168
      Width = 193
      Height = 21
      TabOrder = 5
      Text = 'FComment'
      OnChange = FBOkCheckEnabled
    end
    object FUDMinRows: TUpDown
      Left = 217
      Top = 112
      Width = 15
      Height = 21
      Associate = FMinRows
      Max = 2147483647
      TabOrder = 6
    end
    object FUDMaxRows: TUpDown
      Left = 217
      Top = 136
      Width = 15
      Height = 21
      Associate = FMaxRows
      Max = 2147483647
      TabOrder = 7
    end
    object FEngine: TComboBox_Ext
      Left = 120
      Top = 48
      Width = 113
      Height = 21
      Enabled = False
      TabOrder = 1
      Text = 'FEngine'
    end
  end
end
