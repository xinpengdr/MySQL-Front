object frmEncoding: TfrmEncoding
  Left = 521
  Top = 304
  BorderStyle = bsNone
  Caption = 'Open Dialog Extension'
  ClientHeight = 37
  ClientWidth = 364
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblEncoding: TTntLabel
    Left = 9
    Top = 10
    Width = 47
    Height = 13
    Caption = '&Encoding:'
    FocusControl = cbEncodings
  end
  object cbEncodings: TTntComboBox
    Left = 105
    Top = 7
    Width = 246
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
end
