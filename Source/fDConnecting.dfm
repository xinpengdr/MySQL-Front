object DConnecting: TDConnecting
  Left = 831
  Top = 449
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'DConnecting'
  ClientHeight = 93
  ClientWidth = 250
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object FInfo: TLabel
    Left = 16
    Top = 16
    Width = 217
    Height = 25
    AutoSize = False
    Caption = 'FInfo'
  end
  object FBCancel: TButton
    Left = 88
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    TabOrder = 0
    OnClick = FBCancelClick
  end
end
