object DInfo: TDInfo
  Left = 633
  Top = 357
  HelpContext = 1075
  BorderStyle = bsDialog
  Caption = 'DInfo'
  ClientHeight = 354
  ClientWidth = 498
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object Image: TImage
    Left = 0
    Top = 0
    Width = 500
    Height = 300
    HelpContext = 1075
    AutoSize = True
  end
  object FVersion: TLabel
    Left = 344
    Top = 80
    Width = 73
    Height = 20
    Alignment = taRightJustify
    Caption = 'FVersion'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object FURI: TLabel
    Left = 440
    Top = 112
    Width = 25
    Height = 13
    Cursor = crHandPoint
    Alignment = taRightJustify
    Caption = 'FURI'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    Transparent = True
    OnClick = FURIClick
  end
  object FBuild: TLabel
    Left = 428
    Top = 83
    Width = 38
    Height = 16
    Alignment = taRightJustify
    Caption = 'FBuild'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object FBOk: TButton
    Left = 416
    Top = 320
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object FBHelp: TButton
    Left = 8
    Top = 320
    Width = 75
    Height = 25
    Caption = 'FBHelp'
    TabOrder = 1
    OnClick = FBHelpClick
  end
  object FLine: TPanel_Ext
    Left = 0
    Top = 300
    Width = 500
    Height = 2
    BevelOuter = bvLowered
    ParentBackground = False
    TabOrder = 0
  end
end
