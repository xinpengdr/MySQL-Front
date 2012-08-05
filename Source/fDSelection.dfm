object DSelection: TDSelection
  Left = 773
  Top = 174
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'DSelection'
  ClientHeight = 313
  ClientWidth = 249
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
  DesignSize = (
    249
    313)
  PixelsPerInch = 106
  TextHeight = 13
  object FBOk: TButton
    Left = 80
    Top = 280
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object FBCancel: TButton
    Left = 168
    Top = 280
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 2
  end
  object FSelection: TListView
    Left = 8
    Top = 8
    Width = 233
    Height = 213
    Columns = <
      item
        AutoSize = True
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    SortType = stText
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = FSelectionChange
    OnCompare = FSelectionCompare
    OnDblClick = FSelectionDblClick
  end
  object FManual: TEdit
    Left = 8
    Top = 240
    Width = 233
    Height = 21
    TabOrder = 3
    Text = 'FManual'
  end
end
