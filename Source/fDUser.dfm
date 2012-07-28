object DUser: TDUser
  Left = 831
  Top = 273
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DUser'
  ClientHeight = 294
  ClientWidth = 337
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
    337
    294)
  PixelsPerInch = 106
  TextHeight = 13
  object PSQLWait: TPanel
    Left = 8
    Top = 8
    Width = 321
    Height = 241
    Cursor = crHourGlass
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'PSQLWait'
    TabOrder = 0
    Visible = False
  end
  object FBCancel: TButton
    Left = 255
    Top = 260
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'FBCancel'
    ModalResult = 2
    TabOrder = 4
  end
  object FBOk: TButton
    Left = 167
    Top = 260
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'FBOk'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 321
    Height = 241
    ActivePage = TSBasics
    Anchors = [akLeft, akTop, akRight, akBottom]
    HotTrack = True
    MultiLine = True
    TabOrder = 1
    object TSBasics: TTabSheet
      Caption = 'TSBasics'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        195)
      object GBasics: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 297
        Height = 117
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GBasics'
        TabOrder = 0
        object FLUser: TLabel
          Left = 8
          Top = 23
          Width = 34
          Height = 13
          Caption = 'FLUser'
          FocusControl = FName
        end
        object FLPassword: TLabel
          Left = 8
          Top = 87
          Width = 58
          Height = 13
          Caption = 'FLPassword'
          FocusControl = FPassword
        end
        object FLHost: TLabel
          Left = 8
          Top = 55
          Width = 34
          Height = 13
          Caption = 'FLHost'
          FocusControl = FHost
        end
        object FPassword: TEdit
          Left = 128
          Top = 84
          Width = 97
          Height = 21
          MaxLength = 40
          TabOrder = 2
          Text = 'FPassword'
          OnChange = FBOkCheckEnabled
        end
        object FName: TEdit
          Left = 128
          Top = 20
          Width = 81
          Height = 21
          MaxLength = 16
          TabOrder = 0
          Text = 'FName'
          OnChange = FBOkCheckEnabled
        end
        object FHost: TEdit
          Left = 128
          Top = 52
          Width = 145
          Height = 21
          TabOrder = 1
          Text = 'FHost'
          OnChange = FBOkCheckEnabled
          OnExit = FHostExit
        end
      end
    end
    object TSRights: TTabSheet
      Caption = 'TSRights'
      OnShow = TSRightsShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        195)
      object FRights: TListView
        Left = 8
        Top = 8
        Width = 189
        Height = 169
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            AutoSize = True
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = FRightsDblClick
        OnSelectItem = FRightsSelectItem
      end
      object FBRightsNew: TButton
        Left = 208
        Top = 8
        Width = 95
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'FBRightsNew'
        TabOrder = 1
        OnClick = FBRightsNewClick
      end
      object FBRightsEdit: TButton
        Left = 208
        Top = 72
        Width = 95
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'FBRightsEdit'
        TabOrder = 3
        OnClick = FBRightsEditClick
      end
      object FBRightsDelete: TButton
        Left = 208
        Top = 40
        Width = 95
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'FBRightsDelete'
        TabOrder = 2
        OnClick = FBRightsDeleteClick
      end
    end
    object TSLimits: TTabSheet
      Caption = 'TSLimits'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        195)
      object GLimits: TGroupBox_Ext
        Left = 8
        Top = 8
        Width = 297
        Height = 145
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GLimits'
        TabOrder = 0
        object FLQueriesPerHour: TLabel
          Left = 8
          Top = 55
          Width = 87
          Height = 13
          Caption = 'FLQueriesPerHour'
          FocusControl = FQueriesPerHour
        end
        object FLUpdatesPerHour: TLabel
          Left = 8
          Top = 87
          Width = 91
          Height = 13
          Caption = 'FLUpdatesPerHour'
          FocusControl = FUpdatesPerHour
        end
        object FLConnectionsPerHour: TLabel
          Left = 8
          Top = 23
          Width = 110
          Height = 13
          Caption = 'FLConnectionsPerHour'
          FocusControl = FConnectionsPerHour
        end
        object FLUserConnections: TLabel
          Left = 8
          Top = 115
          Width = 93
          Height = 13
          Caption = 'FLUserConnections'
        end
        object FQueriesPerHour: TEdit
          Left = 232
          Top = 52
          Width = 41
          Height = 21
          TabOrder = 2
          Text = '0'
          OnChange = FBOkCheckEnabled
        end
        object FUpdatesPerHour: TEdit
          Left = 232
          Top = 84
          Width = 41
          Height = 21
          TabOrder = 4
          Text = '0'
          OnChange = FBOkCheckEnabled
        end
        object FConnectionsPerHour: TEdit
          Left = 232
          Top = 20
          Width = 41
          Height = 21
          TabOrder = 0
          Text = '0'
          OnChange = FBOkCheckEnabled
        end
        object FUDQueriesPerHour: TUpDown
          Left = 273
          Top = 52
          Width = 15
          Height = 21
          Associate = FQueriesPerHour
          TabOrder = 3
        end
        object FUDUpdatesPerHour: TUpDown
          Left = 273
          Top = 84
          Width = 15
          Height = 21
          Associate = FUpdatesPerHour
          TabOrder = 5
        end
        object FUDConnectionsPerHour: TUpDown
          Left = 273
          Top = 20
          Width = 15
          Height = 21
          Associate = FConnectionsPerHour
          TabOrder = 1
        end
        object FUserConnections: TEdit
          Left = 232
          Top = 112
          Width = 41
          Height = 21
          TabOrder = 6
          Text = '0'
        end
        object FUDUserConnections: TUpDown
          Left = 273
          Top = 112
          Width = 15
          Height = 21
          Associate = FUserConnections
          TabOrder = 7
        end
      end
    end
    object TSSQLLog: TTabSheet
      Caption = 'TSSQLLog'
      OnShow = TSSQLLogShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        195)
      object FSQLLog: TSynMemo
        Left = 8
        Top = 8
        Width = 297
        Height = 169
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
        MaxScrollWidth = 65535
        Options = [eoAutoIndent, eoGroupUndo, eoHideShowScrollbars, eoNoCaret, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
        ReadOnly = True
        RightEdgeColor = clWindow
        ScrollHintFormat = shfTopToBottom
        WantReturns = False
      end
    end
    object TSSlowSQLLog: TTabSheet
      Caption = 'TSSlowSQLLog'
      OnShow = TSSlowSQLLogShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        313
        195)
      object FSlowSQLLog: TSynMemo
        Left = 8
        Top = 8
        Width = 297
        Height = 169
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
        MaxScrollWidth = 65535
        Options = [eoAutoIndent, eoGroupUndo, eoHideShowScrollbars, eoNoCaret, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
        ReadOnly = True
        RightEdgeColor = clWindow
        ScrollHintFormat = shfTopToBottom
        WantReturns = False
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
        313
        195)
      object FSource: TSynMemo
        Left = 8
        Top = 8
        Width = 297
        Height = 169
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
        MaxScrollWidth = 65535
        Options = [eoAutoIndent, eoGroupUndo, eoHideShowScrollbars, eoNoCaret, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
        ReadOnly = True
        RightEdge = 2000
        RightEdgeColor = clWindow
        ScrollHintFormat = shfTopToBottom
        WantReturns = False
      end
    end
  end
  object FBHelp: TButton
    Left = 8
    Top = 260
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'FBHelp'
    TabOrder = 2
    OnClick = FBHelpClick
  end
  object MSource: TPopupMenu
    Left = 88
    Top = 256
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
