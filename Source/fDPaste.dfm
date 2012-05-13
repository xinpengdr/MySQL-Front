object DPaste: TDPaste
  Left = 655
  Top = 409
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'DPaste'
  ClientHeight = 106
  ClientWidth = 177
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object FBOk: TButton
    Left = 8
    Top = 72
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object FBCancel: TButton
    Left = 96
    Top = 72
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 1
  end
  object FStructure: TCheckBox
    Left = 8
    Top = 16
    Width = 169
    Height = 17
    Caption = 'FStructure'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 2
  end
  object FData: TCheckBox
    Left = 8
    Top = 40
    Width = 169
    Height = 17
    Caption = 'FData'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
