object DSQLHelp: TDSQLHelp
  Left = 603
  Top = 315
  BorderIcons = [biSystemMenu]
  Caption = 'DSQLHelp'
  ClientHeight = 567
  ClientWidth = 500
  Color = clWindow
  Constraints.MinHeight = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnHide = FormHide
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object Panel: TPanel_Ext
    Left = 0
    Top = 0
    Width = 500
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      500
      25)
    object FBContent: TButton
      Left = 1
      Top = 1
      Width = 79
      Height = 25
      Caption = 'FBContent'
      TabOrder = 0
      OnClick = FBContentClick
    end
    object FBDescription: TButton
      Left = 80
      Top = 1
      Width = 75
      Height = 25
      Caption = 'FBDescription'
      TabOrder = 1
      OnClick = FBDescriptionClick
    end
    object FBExample: TButton
      Tag = 2
      Left = 156
      Top = 1
      Width = 75
      Height = 25
      Caption = 'FBExample'
      TabOrder = 2
      OnClick = FBExampleClick
    end
    object FBManual: TButton
      Left = 232
      Top = 1
      Width = 75
      Height = 25
      Caption = 'FBManual'
      TabOrder = 3
      OnClick = FBManualClick
    end
    object FQuickSearch: TEdit
      Left = 339
      Top = 1
      Width = 136
      Height = 21
      Anchors = [akTop, akRight]
      AutoSize = False
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 4
      OnKeyPress = FQuickSearchKeyPress
    end
    object TBQuickSearchEnabled: TToolBar
      Left = 475
      Top = 1
      Width = 23
      Height = 22
      Align = alNone
      Anchors = [akTop, akRight]
      AutoSize = True
      TabOrder = 5
      Transparent = False
      object FQuickSearchEnabled: TToolButton
        Left = 0
        Top = 0
        Caption = ' '
        ImageIndex = 89
        OnClick = FQuickSearchEnabledClick
      end
    end
  end
  object FDescription: TRichEdit
    Left = 0
    Top = 25
    Width = 500
    Height = 542
    TabStop = False
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    Ctl3D = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    HideSelection = False
    Lines.Strings = (
      'FDescription')
    ParentCtl3D = False
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
    Width = 500
    Height = 542
    TabStop = False
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Ctl3D = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    HideSelection = False
    Constraints.MinHeight = 100
    Constraints.MinWidth = 100
    Lines.Strings = (
      'FExample')
    ParentCtl3D = False
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
