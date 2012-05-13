object DBookmark: TDBookmark
  Left = 692
  Top = 359
  HelpContext = 1088
  BorderStyle = bsDialog
  Caption = 'DBookmark'
  ClientHeight = 159
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object GBasics: TGroupBox_Ext
    Left = 8
    Top = 8
    Width = 393
    Height = 93
    Caption = 'GBasics'
    TabOrder = 0
    object FLCaption: TLabel
      Left = 8
      Top = 27
      Width = 48
      Height = 13
      Caption = 'FLCaption'
      FocusControl = FCaption
    end
    object FLAddress: TLabel
      Left = 8
      Top = 59
      Width = 50
      Height = 13
      Caption = 'FLAddress'
      FocusControl = FAddress
    end
    object FCaption: TEdit
      Left = 88
      Top = 24
      Width = 185
      Height = 21
      TabOrder = 0
      Text = 'FCaption'
      OnChange = FBOkCheckEnabled
    end
    object FAddress: TEdit
      Left = 88
      Top = 56
      Width = 293
      Height = 21
      TabOrder = 1
      Text = 'FAddress'
      OnChange = FBOkCheckEnabled
    end
  end
  object FBOk: TButton
    Left = 238
    Top = 124
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object FBCancel: TButton
    Left = 326
    Top = 124
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 3
  end
  object FBHelp: TButton
    Left = 8
    Top = 124
    Width = 75
    Height = 25
    Caption = 'FBHelp'
    TabOrder = 1
    OnClick = FBHelpClick
  end
end
