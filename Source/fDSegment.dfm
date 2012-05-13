object DSegment: TDSegment
  Left = 927
  Top = 205
  BorderStyle = bsDialog
  Caption = 'DSegment'
  ClientHeight = 145
  ClientWidth = 337
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
  object GBasics: TGroupBox_Ext
    Left = 8
    Top = 8
    Width = 321
    Height = 89
    Caption = 'GBasics'
    TabOrder = 0
    object FLColor: TLabel
      Left = 8
      Top = 56
      Width = 36
      Height = 13
      Caption = 'FLColor'
    end
    object FLName: TLabel
      Left = 8
      Top = 24
      Width = 40
      Height = 13
      Caption = 'FLName'
    end
    object PColor: TPanel_Ext
      Left = 128
      Top = 56
      Width = 49
      Height = 21
      BevelOuter = bvLowered
      ParentBackground = False
      TabOrder = 1
      OnClick = PColorClick
    end
    object FColor: TButton
      Left = 177
      Top = 56
      Width = 21
      Height = 21
      Caption = #183#183#183
      TabOrder = 2
      OnClick = FColorClick
    end
    object FName: TEdit
      Left = 128
      Top = 24
      Width = 185
      Height = 21
      TabOrder = 0
      Text = 'FName'
    end
  end
  object FBOk: TButton
    Left = 167
    Top = 111
    Width = 75
    Height = 25
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object FBCancel: TButton
    Left = 255
    Top = 111
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ColorDialog: TColorDialog
    Left = 112
    Top = 112
  end
end
