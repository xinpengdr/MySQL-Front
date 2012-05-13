object DGoto: TDGoto
  Left = 364
  Top = 363
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'DGoto'
  ClientHeight = 107
  ClientWidth = 283
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object FLPrimary: TLabel
    Left = 8
    Top = 12
    Width = 46
    Height = 13
    Caption = 'FLPrimary'
    FocusControl = FPrimary
    Visible = False
  end
  object FLSecondary: TLabel
    Left = 8
    Top = 60
    Width = 63
    Height = 13
    Caption = 'FLSecondary'
    FocusControl = FSecondary
    Visible = False
  end
  object FBOk: TButton
    Left = 200
    Top = 8
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object FBCancel: TButton
    Left = 200
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 1
  end
  object FPrimary: TEdit
    Left = 8
    Top = 28
    Width = 177
    Height = 21
    TabOrder = 2
    Text = 'FPrimary'
    Visible = False
    OnChange = FPrimaryChange
  end
  object FSecondary: TEdit
    Left = 8
    Top = 76
    Width = 177
    Height = 21
    TabOrder = 3
    Text = 'FSecondary'
    Visible = False
  end
end
