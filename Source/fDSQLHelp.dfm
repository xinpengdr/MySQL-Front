object DSQLHelp: TDSQLHelp
  Left = 603
  Top = 315
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DSQLHelp'
  ClientHeight = 569
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnHide = FormHide
  OnKeyPress = FormKeyPress
  PixelsPerInch = 106
  TextHeight = 13
  object Panel: TPanel_Ext
    Left = 0
    Top = 0
    Width = 425
    Height = 25
    Align = alTop
    BevelOuter = bvLowered
    ParentBackground = False
    TabOrder = 0
    object FBDescription: TButton
      Left = 1
      Top = 1
      Width = 79
      Height = 25
      Caption = 'FBDescription'
      TabOrder = 0
      OnClick = FBDescriptionClick
    end
    object FBExample: TButton
      Left = 80
      Top = 1
      Width = 75
      Height = 25
      Caption = 'FBExample'
      TabOrder = 1
      OnClick = FBExampleClick
    end
    object FBManual: TButton
      Tag = 2
      Left = 156
      Top = 1
      Width = 75
      Height = 25
      Caption = 'FBManual'
      TabOrder = 2
      OnClick = FBManualClick
    end
  end
  object FDescription: TRichEdit
    Left = 0
    Top = 25
    Width = 425
    Height = 544
    TabStop = False
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      'FDescription')
    ParentFont = False
    PopupMenu = MSource
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    OnKeyPress = FormKeyPress
  end
  object FExample: TRichEdit
    Left = 0
    Top = 25
    Width = 425
    Height = 544
    TabStop = False
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Constraints.MinHeight = 100
    Constraints.MinWidth = 100
    Lines.Strings = (
      'FExample')
    ParentFont = False
    PopupMenu = MSource
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
    OnKeyPress = FormKeyPress
  end
  object MSource: TPopupMenu
    Left = 40
    Top = 80
    object msCopy: TMenuItem
      Caption = 'aECopy'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object msSelectAll: TMenuItem
      Caption = 'aESelectAll'
    end
  end
end
