object DTableService: TDTableService
  Left = 369
  Top = 242
  ActiveControl = FBCancel
  BorderStyle = bsDialog
  Caption = 'DTableServiceSmall'
  ClientHeight = 82
  ClientWidth = 246
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
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object FInformation: TLabel
    Left = 8
    Top = 8
    Width = 58
    Height = 13
    Caption = 'FInformation'
  end
  object FBCancel: TButton
    Left = 88
    Top = 48
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    Default = True
    TabOrder = 0
    OnClick = FBCancelClick
  end
end
