object DQuickFilter: TDQuickFilter
  Left = 307
  Top = 223
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'DQuickFilter'
  ClientHeight = 130
  ClientWidth = 195
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object FLValue: TLabel
    Left = 16
    Top = 16
    Width = 39
    Height = 13
    Caption = 'FLValue'
    FocusControl = FValue
  end
  object FBOk: TButton
    Left = 16
    Top = 88
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object FBCancel: TButton
    Left = 104
    Top = 88
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 1
  end
  object FValue: TEdit
    Left = 16
    Top = 40
    Width = 161
    Height = 21
    TabOrder = 2
    Text = 'FValue'
  end
end
