object WMain: TWMain
  Left = 358
  Top = 196
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'WMain'
  ClientHeight = 121
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object GVersionInfo: TGroupBox
    Left = 8
    Top = 8
    Width = 297
    Height = 65
    Caption = 'Version Informationen'
    TabOrder = 2
    object FLVersion: TLabel
      Left = 8
      Top = 27
      Width = 38
      Height = 13
      Caption = '&Version:'
      FocusControl = FMajor
    end
    object FMajor: TEdit
      Left = 104
      Top = 24
      Width = 25
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = FMajorChange
    end
    object FUDMajor: TUpDown
      Left = 129
      Top = 24
      Width = 15
      Height = 21
      Associate = FMajor
      TabOrder = 1
    end
    object FMinor: TEdit
      Left = 152
      Top = 24
      Width = 25
      Height = 21
      TabOrder = 2
      Text = '0'
      OnChange = FMinorChange
    end
    object FPatch: TEdit
      Left = 200
      Top = 24
      Width = 25
      Height = 21
      TabOrder = 4
      Text = '0'
      OnChange = FPatchChange
    end
    object FBuild: TEdit
      Left = 248
      Top = 24
      Width = 25
      Height = 21
      TabOrder = 6
      Text = '0'
    end
    object FUDMinor: TUpDown
      Left = 177
      Top = 24
      Width = 15
      Height = 21
      Associate = FMinor
      TabOrder = 3
    end
    object FUDPatch: TUpDown
      Left = 225
      Top = 24
      Width = 15
      Height = 21
      Associate = FPatch
      TabOrder = 5
    end
    object FUDBuild: TUpDown
      Left = 273
      Top = 24
      Width = 15
      Height = 21
      Associate = FBuild
      Max = 9999
      TabOrder = 7
    end
  end
  object FBOk: TButton
    Left = 142
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = FBOkClick
  end
  object FBCancel: TButton
    Left = 230
    Top = 88
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = FBCancelClick
  end
  object FBeta: TCheckBox
    Left = 8
    Top = 92
    Width = 128
    Height = 17
    Caption = 'Beta'
    TabOrder = 3
  end
end
