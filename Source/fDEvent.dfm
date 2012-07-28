object DEvent: TDEvent
  Left = 617
  Top = 209
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DEvent'
  ClientHeight = 535
  ClientWidth = 397
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    397
    535)
  PixelsPerInch = 106
  TextHeight = 13
  object FBOk: TButton
    Left = 227
    Top = 502
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object FBCancel: TButton
    Left = 315
    Top = 502
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 4
  end
  object FBHelp: TButton
    Left = 8
    Top = 502
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'FBHelp'
    TabOrder = 2
    OnClick = FBHelpClick
  end
  object PSQLWait: TPanel
    Left = 8
    Top = 8
    Width = 381
    Height = 483
    Cursor = crHourGlass
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'PSQLWait'
    TabOrder = 0
    Visible = False
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 381
    Height = 483
    ActivePage = TSBasics
    Anchors = [akLeft, akTop, akRight, akBottom]
    HotTrack = True
    TabOrder = 1
    object TSBasics: TTabSheet
      Caption = 'TSBasics'
      DesignSize = (
        373
        455)
      object GBasics: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 357
        Height = 431
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'GBasics'
        TabOrder = 0
        DesignSize = (
          357
          431)
        object FLName: TLabel
          Left = 8
          Top = 23
          Width = 40
          Height = 13
          Caption = 'FLName'
          FocusControl = FName
        end
        object FLStatement: TLabel
          Left = 8
          Top = 361
          Width = 60
          Height = 13
          Caption = 'FLStatement'
          FocusControl = FStatement
        end
        object FLEnabled: TLabel
          Left = 8
          Top = 269
          Width = 51
          Height = 13
          Caption = 'FLEnabled'
          FocusControl = FEnabled
        end
        object FLComment: TLabel
          Left = 8
          Top = 331
          Width = 56
          Height = 13
          Caption = 'FLComment'
          FocusControl = FComment
        end
        object FLPreserve: TLabel
          Left = 8
          Top = 297
          Width = 54
          Height = 13
          Caption = 'FLPreserve'
          FocusControl = FPreserve
        end
        object FLExecuteDateTime: TLabel
          Left = 16
          Top = 80
          Width = 97
          Height = 13
          Caption = 'FLExecuteDateTime'
          FocusControl = FExecuteDate
        end
        object FLStartDateTime: TLabel
          Left = 16
          Top = 212
          Width = 80
          Height = 13
          Caption = 'FLStartDateTime'
          FocusControl = FStartDate
        end
        object FLEndDateTime: TLabel
          Left = 16
          Top = 236
          Width = 77
          Height = 13
          Caption = 'FLEndDateTime'
          FocusControl = FEndDate
        end
        object FLSingleExecution: TLabel
          Left = 8
          Top = 57
          Width = 88
          Height = 13
          Caption = 'FLSingleExecution'
        end
        object FLMultipleExecution: TLabel
          Left = 8
          Top = 109
          Width = 95
          Height = 13
          Caption = 'FLMultipleExecution'
        end
        object FLIntervalDate: TLabel
          Left = 16
          Top = 131
          Width = 70
          Height = 13
          Caption = 'FLIntervalDate'
        end
        object FLIntervalWeeks: TLabel
          Left = 16
          Top = 155
          Width = 81
          Height = 13
          Caption = 'FLIntervalWeeks'
        end
        object FLIntervalTime: TLabel
          Left = 16
          Top = 179
          Width = 70
          Height = 13
          Caption = 'FLIntervalTime'
        end
        object FName: TEdit
          Left = 152
          Top = 20
          Width = 145
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 64
          TabOrder = 0
          Text = 'FName'
          OnChange = FBOkCheckEnabled
        end
        object FStatement: TSynMemo
          Left = 8
          Top = 377
          Width = 341
          Height = 45
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          PopupMenu = MSource
          TabOrder = 30
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Width = 0
          Options = [eoAutoIndent, eoDragDropEditing, eoGroupUndo, eoHideShowScrollbars, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
          RightEdgeColor = clWindow
          ScrollHintFormat = shfTopToBottom
          OnChange = FBOkCheckEnabled
        end
        object FEnabled: TCheckBox
          Left = 152
          Top = 268
          Width = 189
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'FEnabled'
          TabOrder = 27
          OnClick = FBOkCheckEnabled
        end
        object FComment: TEdit
          Left = 152
          Top = 328
          Width = 193
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 29
          Text = 'FComment'
          OnChange = FBOkCheckEnabled
        end
        object FPreserve: TCheckBox
          Left = 152
          Top = 296
          Width = 191
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'FPreserve'
          TabOrder = 28
          OnClick = FBOkCheckEnabled
        end
        object FSingleExecution: TRadioButton
          Left = 152
          Top = 56
          Width = 169
          Height = 17
          Caption = 'FSingleExecution'
          TabOrder = 1
          OnClick = FExecutionClick
          OnKeyPress = FExecutionKeyPress
        end
        object FMultipleExecution: TRadioButton
          Left = 152
          Top = 108
          Width = 169
          Height = 17
          Caption = 'FMultipleExecution'
          TabOrder = 4
          OnClick = FExecutionClick
          OnKeyPress = FExecutionKeyPress
        end
        object FExecuteDate: TDateTimePicker
          Left = 176
          Top = 76
          Width = 81
          Height = 21
          Date = 2.500000000000000000
          Time = 2.500000000000000000
          TabOrder = 2
          OnChange = FBOkCheckEnabled
        end
        object FExecuteTime: TDateTimePicker
          Left = 262
          Top = 76
          Width = 69
          Height = 21
          Date = 1.000000000000000000
          Time = 1.000000000000000000
          Kind = dtkTime
          TabOrder = 3
          OnChange = FBOkCheckEnabled
        end
        object FStartEnabled: TCheckBox
          Left = 176
          Top = 210
          Width = 17
          Height = 17
          TabOrder = 21
          OnClick = FStartEnabledClick
          OnKeyPress = FStartEnabledKeyPress
        end
        object FStartDate: TDateTimePicker
          Left = 194
          Top = 208
          Width = 81
          Height = 21
          Date = 2.500000000000000000
          Time = 2.500000000000000000
          TabOrder = 22
          OnChange = FBOkCheckEnabled
        end
        object FStartTime: TDateTimePicker
          Left = 280
          Top = 208
          Width = 69
          Height = 21
          Date = 1.000000000000000000
          Time = 1.000000000000000000
          Kind = dtkTime
          TabOrder = 23
          OnChange = FBOkCheckEnabled
        end
        object FEndDate: TDateTimePicker
          Left = 194
          Top = 232
          Width = 81
          Height = 21
          Date = 2.500000000000000000
          Time = 2.500000000000000000
          TabOrder = 25
          OnChange = FBOkCheckEnabled
        end
        object FEndTime: TDateTimePicker
          Left = 280
          Top = 232
          Width = 69
          Height = 21
          Date = 1.000000000000000000
          Time = 1.000000000000000000
          Kind = dtkTime
          TabOrder = 26
          OnChange = FBOkCheckEnabled
        end
        object FEndEnabled: TCheckBox
          Left = 176
          Top = 234
          Width = 17
          Height = 17
          TabOrder = 24
          OnClick = FEndEnabledClick
          OnKeyPress = FEndEnabledKeyPress
        end
        object FIntervalQuarter: TEdit
          Left = 176
          Top = 152
          Width = 23
          Height = 21
          TabOrder = 10
          Text = '0'
          OnChange = FBOkCheckEnabled
        end
        object FIntervalWeek: TEdit
          Left = 224
          Top = 152
          Width = 23
          Height = 21
          TabOrder = 13
          Text = '0'
          OnChange = FBOkCheckEnabled
        end
        object FUDIntervalQuarter: TUpDown
          Left = 199
          Top = 152
          Width = 15
          Height = 21
          Associate = FIntervalQuarter
          Max = 99
          TabOrder = 12
          OnExit = FBOkCheckEnabled
        end
        object FUDIntervalWeek: TUpDown
          Left = 247
          Top = 152
          Width = 15
          Height = 21
          Associate = FIntervalWeek
          Max = 99
          TabOrder = 14
          OnExit = FBOkCheckEnabled
        end
        object FIntervalYear: TEdit
          Left = 176
          Top = 128
          Width = 23
          Height = 21
          TabOrder = 5
          Text = '0'
          OnChange = FBOkCheckEnabled
        end
        object FIntervalMonth: TEdit
          Left = 224
          Top = 128
          Width = 23
          Height = 21
          TabOrder = 7
          Text = '0'
          OnChange = FBOkCheckEnabled
        end
        object FIntervalDay: TEdit
          Left = 272
          Top = 128
          Width = 23
          Height = 21
          TabOrder = 9
          Text = '0'
          OnChange = FBOkCheckEnabled
        end
        object FUDIntervalYear: TUpDown
          Left = 199
          Top = 128
          Width = 15
          Height = 21
          Associate = FIntervalYear
          Max = 99
          TabOrder = 6
          OnExit = FBOkCheckEnabled
        end
        object FUDIntervalMonth: TUpDown
          Left = 247
          Top = 128
          Width = 15
          Height = 21
          Associate = FIntervalMonth
          Max = 99
          TabOrder = 8
          OnExit = FBOkCheckEnabled
        end
        object FUDIntervalDay: TUpDown
          Left = 295
          Top = 128
          Width = 15
          Height = 21
          Associate = FIntervalDay
          Max = 99
          TabOrder = 11
          OnExit = FBOkCheckEnabled
        end
        object FUDIntervalHour: TUpDown
          Left = 199
          Top = 176
          Width = 15
          Height = 21
          Associate = FIntervalHour
          Max = 99
          TabOrder = 16
          OnExit = FBOkCheckEnabled
        end
        object FUDIntervalMinute: TUpDown
          Left = 247
          Top = 176
          Width = 15
          Height = 21
          Associate = FIntervalMinute
          Max = 99
          TabOrder = 17
          OnExit = FBOkCheckEnabled
        end
        object FUDIntervalSecond: TUpDown
          Left = 295
          Top = 176
          Width = 15
          Height = 21
          Associate = FIntervalSecond
          Max = 99
          TabOrder = 19
          OnExit = FBOkCheckEnabled
        end
        object FIntervalHour: TEdit
          Left = 176
          Top = 176
          Width = 23
          Height = 21
          TabOrder = 15
          Text = '0'
          OnChange = FBOkCheckEnabled
        end
        object FIntervalMinute: TEdit
          Left = 224
          Top = 176
          Width = 23
          Height = 21
          TabOrder = 18
          Text = '0'
          OnChange = FBOkCheckEnabled
        end
        object FIntervalSecond: TEdit
          Left = 272
          Top = 176
          Width = 23
          Height = 21
          TabOrder = 20
          Text = '0'
          OnChange = FBOkCheckEnabled
        end
      end
    end
    object TSInformations: TTabSheet
      Caption = 'TSInformations'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        373
        455)
      object GDefiner: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 357
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GDefiner'
        TabOrder = 0
        DesignSize = (
          357
          49)
        object FDefiner: TLabel
          Left = 309
          Top = 20
          Width = 40
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FDefiner'
        end
        object FLDefiner: TLabel
          Left = 8
          Top = 20
          Width = 46
          Height = 13
          Caption = 'FLDefiner'
        end
      end
      object GDates: TGroupBox_Ext
        Left = 8
        Top = 67
        Width = 357
        Height = 79
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GDates'
        TabOrder = 1
        DesignSize = (
          357
          79)
        object FLCreated: TLabel
          Left = 8
          Top = 20
          Width = 49
          Height = 13
          Caption = 'FLCreated'
        end
        object FLUpdated: TLabel
          Left = 8
          Top = 48
          Width = 53
          Height = 13
          Caption = 'FLUpdated'
        end
        object FCreated: TLabel
          Left = 306
          Top = 20
          Width = 43
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FCreated'
        end
        object FUpdated: TLabel
          Left = 302
          Top = 48
          Width = 47
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'FUpdated'
        end
      end
    end
    object TSSource: TTabSheet
      Caption = 'TSSource'
      OnShow = TSSourceShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        373
        455)
      object FSource: TSynMemo
        Left = 8
        Top = 8
        Width = 357
        Height = 431
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = MSource
        TabOrder = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Width = 0
        Options = [eoAutoIndent, eoGroupUndo, eoHideShowScrollbars, eoNoCaret, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
        ReadOnly = True
        RightEdge = 0
        RightEdgeColor = clWindow
        ScrollHintFormat = shfTopToBottom
        WantReturns = False
      end
    end
  end
  object MSource: TPopupMenu
    Left = 96
    Top = 496
    object msUndo: TMenuItem
      Caption = 'aEUndo'
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object msCut: TMenuItem
      Caption = 'aECut'
    end
    object msCopy: TMenuItem
      Caption = 'aECopy'
    end
    object msPaste: TMenuItem
      Caption = 'aEPaste'
    end
    object msDelete: TMenuItem
      Caption = 'aEDelete'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object msSelectAll: TMenuItem
      Caption = 'aESelectAll'
    end
  end
end
