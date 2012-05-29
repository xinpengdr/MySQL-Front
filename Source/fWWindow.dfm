object WWindow: TWWindow
  Left = 387
  Top = 161
  Caption = 'WWindow'
  ClientHeight = 460
  ClientWidth = 691
  Color = clBtnFace
  Constraints.MinHeight = 485
  Constraints.MinWidth = 560
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDefault
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 14
  object PWorkSpace: TPanel_Ext
    Left = 0
    Top = 124
    Width = 691
    Height = 315
    Align = alClient
    BevelOuter = bvNone
    Color = clAppWorkSpace
    ParentBackground = False
    TabOrder = 5
    ExplicitTop = 121
    ExplicitHeight = 318
  end
  object TabControl: TTabControl
    Left = 0
    Top = 96
    Width = 691
    Height = 28
    Align = alTop
    OwnerDraw = True
    ParentShowHint = False
    PopupMenu = MTabControl
    ShowHint = True
    TabHeight = 23
    TabOrder = 2
    TabStop = False
    Visible = False
    OnChange = TabControlChange
    OnChanging = TabControlChanging
    OnContextPopup = TabControlContextPopup
    OnDragDrop = TabControlDragDrop
    OnDragOver = TabControlDragOver
    OnDrawTab = TabControlDrawTab
    OnEndDrag = TabControlEndDrag
    OnGetImageIndex = TabControlGetImageIndex
    OnMouseDown = TabControlMouseDown
    OnMouseMove = TabControlMouseMove
    OnMouseUp = TabControlMouseUp
    OnResize = TabControlResize
    OnStartDrag = TabControlStartDrag
    ExplicitTop = 93
  end
  object PAddressBar: TPanel_Ext
    Left = 0
    Top = 64
    Width = 691
    Height = 32
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    Visible = False
    object TBAddressBar: TToolBar
      Left = 0
      Top = 0
      Width = 691
      AutoSize = True
      BorderWidth = 2
      EdgeBorders = [ebBottom]
      TabOrder = 0
      Transparent = False
      OnResize = TBAddressBarResize
      DesignSize = (
        683
        22)
      object tbPrev: TToolButton
        Left = 0
        Top = 0
        Action = aVPrev
        DropdownMenu = MPrev
        Enabled = False
        PopupMenu = MPrev
        Style = tbsDropDown
      end
      object tbNext: TToolButton
        Left = 38
        Top = 0
        Action = aVNext
        DropdownMenu = MNext
        Enabled = False
        PopupMenu = MNext
        Style = tbsDropDown
      end
      object ToolButton26: TToolButton
        Left = 76
        Top = 0
        Width = 8
        Caption = 'ToolButton26'
        Style = tbsSeparator
      end
      object FAddress: TComboBox_Ext
        Left = 84
        Top = 0
        Width = 457
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Constraints.MinWidth = 300
        TabOrder = 0
        OnDropDown = FAddressDropDown
        OnKeyPress = FAddressKeyPress
        OnSelect = FAddressSelect
      end
      object FAddressApply: TToolButton
        Left = 541
        Top = 0
        Action = aVAddress
        ImageIndex = 21
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 439
    Width = 691
    Height = 21
    AutoHint = True
    Panels = <
      item
        Text = 'sbMessage'
        Width = 200
      end
      item
        Text = 'sbItem'
        Width = 50
      end
      item
        Text = 'sbSummarize'
        Width = 50
      end
      item
        Text = 'sbConnected'
        Width = 50
      end
      item
        Bevel = pbNone
        Text = 'sbEdge'
        Width = 23
      end>
    ParentFont = True
    UseSystemFont = False
  end
  object PToolBar: TPanel_Ext
    Left = 0
    Top = 0
    Width = 691
    Height = 64
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    ParentBackground = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object ToolBar: TToolBar
      Left = 0
      Top = 0
      Width = 691
      Height = 64
      AutoSize = True
      BorderWidth = 2
      DoubleBuffered = True
      EdgeBorders = [ebTop, ebBottom]
      ParentDoubleBuffered = False
      TabOrder = 0
      object tbVRefresh: TToolButton
        Left = 0
        Top = 0
        Action = aVRefresh
      end
      object tbDCancel: TToolButton
        Left = 23
        Top = 0
        Action = aDCancel
      end
      object ToolButton30: TToolButton
        Left = 46
        Top = 0
        Width = 8
        Caption = 'ToolButton30'
        Enabled = False
        Style = tbsSeparator
      end
      object tbECut: TToolButton
        Left = 54
        Top = 0
        Action = aECut
      end
      object tbECopy: TToolButton
        Left = 77
        Top = 0
        Action = aECopy
      end
      object tbEPaste: TToolButton
        Left = 100
        Top = 0
        Action = aEPaste
      end
      object tbEDelete: TToolButton
        Left = 123
        Top = 0
        Action = aEDelete
      end
      object ToolButton11: TToolButton
        Left = 146
        Top = 0
        Width = 8
        Caption = 'ToolButton11'
        Enabled = False
        Style = tbsSeparator
      end
      object tbCreateDatabase: TToolButton
        Left = 154
        Top = 0
        Action = aDCreateDatabase
      end
      object tbDeleteDatabase: TToolButton
        Left = 177
        Top = 0
        Action = aDDeleteDatabase
      end
      object ToolButton14: TToolButton
        Left = 200
        Top = 0
        Width = 8
        Caption = 'ToolButton14'
        Enabled = False
        Style = tbsSeparator
      end
      object tbCreateTable: TToolButton
        Left = 208
        Top = 0
        Action = aDCreateTable
      end
      object tbDeleteTable: TToolButton
        Left = 231
        Top = 0
        Action = aDDeleteTable
      end
      object ToolButton12: TToolButton
        Left = 254
        Top = 0
        Width = 8
        Caption = 'ToolButton12'
        Enabled = False
        Style = tbsSeparator
      end
      object tbCreateIndex: TToolButton
        Left = 262
        Top = 0
        Action = aDCreateIndex
      end
      object tbDeleteIndex: TToolButton
        Left = 285
        Top = 0
        Action = aDDeleteIndex
      end
      object ToolButton15: TToolButton
        Left = 308
        Top = 0
        Width = 8
        Caption = 'ToolButton15'
        Enabled = False
        Style = tbsSeparator
      end
      object tbCreateField: TToolButton
        Left = 316
        Top = 0
        Action = aDCreateField
      end
      object tbDeleteField: TToolButton
        Left = 339
        Top = 0
        Action = aDDeleteField
      end
      object ToolButton16: TToolButton
        Left = 362
        Top = 0
        Width = 8
        Caption = 'ToolButton16'
        Enabled = False
        Style = tbsSeparator
      end
      object tbCreateForeignKey: TToolButton
        Left = 370
        Top = 0
        Action = aDCreateForeignKey
      end
      object tbDeleteForeignKey: TToolButton
        Left = 393
        Top = 0
        Action = aDDeleteForeignKey
      end
      object ToolButton5: TToolButton
        Left = 416
        Top = 0
        Width = 8
        Caption = 'ToolButton5'
        Enabled = False
        Style = tbsSeparator
      end
      object tbProperties: TToolButton
        Left = 424
        Top = 0
        Enabled = False
        ImageIndex = 11
        OnClick = tbPropertiesClick
      end
      object ToolButton2: TToolButton
        Left = 0
        Top = 0
        Width = 8
        Caption = 'ToolButton2'
        Enabled = False
        Wrap = True
        Style = tbsSeparator
      end
      object tbOpen: TToolButton
        Left = 0
        Top = 30
        Action = aFOpen
      end
      object tbSave: TToolButton
        Left = 23
        Top = 30
        Action = aFSave
      end
      object ToolButton4: TToolButton
        Left = 46
        Top = 30
        Width = 8
        Caption = 'ToolButton4'
        Enabled = False
        Style = tbsSeparator
      end
      object tbUndo: TToolButton
        Left = 54
        Top = 30
        Action = aEUndo
        Enabled = False
      end
      object tbRedo: TToolButton
        Left = 77
        Top = 30
        Action = aERedo
        Enabled = False
      end
      object ToolButton7: TToolButton
        Left = 100
        Top = 30
        Width = 8
        Caption = 'ToolButton7'
        Enabled = False
        Style = tbsSeparator
      end
      object tbSearchFind: TToolButton
        Left = 108
        Top = 30
        Action = aSSearchFind
      end
      object tbSearchReplace: TToolButton
        Left = 131
        Top = 30
        Action = aSSearchReplace
      end
      object ToolButton10: TToolButton
        Left = 154
        Top = 30
        Width = 8
        Caption = 'ToolButton10'
        Enabled = False
        Style = tbsSeparator
      end
      object tbRun: TToolButton
        Left = 162
        Top = 30
        Action = aDRun
      end
      object tbRunSelection: TToolButton
        Left = 185
        Top = 30
        Action = aDRunSelection
      end
      object tbPostObject: TToolButton
        Left = 208
        Top = 30
        Action = aDPostObject
      end
      object ToolButton13: TToolButton
        Left = 231
        Top = 30
        Width = 8
        Caption = 'ToolButton13'
        Enabled = False
        Style = tbsSeparator
      end
      object tbDBFirst: TToolButton
        Left = 239
        Top = 30
        Enabled = False
      end
      object tbDBPrev: TToolButton
        Left = 262
        Top = 30
        Enabled = False
      end
      object tbDBNext: TToolButton
        Left = 285
        Top = 30
        Enabled = False
      end
      object tbDBLast: TToolButton
        Left = 308
        Top = 30
        Enabled = False
      end
      object tbDInsertRecord: TToolButton
        Left = 331
        Top = 30
        Action = aDInsertRecord
      end
      object tbDDeleteRecord: TToolButton
        Left = 354
        Top = 30
        Action = aDDeleteRecord
      end
      object ToolButton23: TToolButton
        Left = 377
        Top = 30
        Width = 8
        Caption = 'ToolButton23'
        Style = tbsSeparator
      end
      object tbPostRecord: TToolButton
        Left = 385
        Top = 30
        Enabled = False
      end
      object tbCancelRecord: TToolButton
        Left = 408
        Top = 30
        Enabled = False
      end
      object ToolButton1: TToolButton
        Left = 431
        Top = 30
        Width = 8
        Caption = 'ToolButton1'
        Enabled = False
        Style = tbsSeparator
      end
      object tbDEmpty: TToolButton
        Left = 439
        Top = 30
        Action = aDEmpty
      end
    end
  end
  object TBTabControl: TToolBar
    Left = 144
    Top = 98
    Width = 561
    Height = 22
    Align = alNone
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Visible = False
    object tcOpenAccount: TToolButton
      Left = 0
      Top = 0
      Action = aFOpenAccount
    end
  end
  object MPrev: TPopupMenu
    OnPopup = MPrevPopup
    Left = 16
    Top = 147
  end
  object MNext: TPopupMenu
    OnPopup = MNextPopup
    Left = 64
    Top = 147
  end
  object ActionList: TActionList
    Left = 80
    Top = 240
    object aVObjectBrowser: TAction
      Category = 'View'
      Caption = 'aVObjectBrowser'
      Enabled = False
      HelpContext = 1035
      HelpType = htContext
      ImageIndex = 1
      ShortCut = 117
    end
    object aVDataBrowser: TAction
      Category = 'View'
      Caption = 'aVBrowser'
      Enabled = False
      HelpContext = 1036
      HelpType = htContext
      ImageIndex = 2
      ShortCut = 118
    end
    object aVObjectIDE: TAction
      Category = 'View'
      Caption = 'aVObjectIDE'
      Enabled = False
      HelpContext = 1121
      HelpType = htContext
      ImageIndex = 98
    end
    object aVQueryBuilder: TAction
      Category = 'View'
      Caption = 'aVQueryBuilder'
      Enabled = False
      HelpContext = 1120
      HelpType = htContext
      ImageIndex = 100
      ShortCut = 16503
    end
    object aVSQLEditor: TAction
      Category = 'View'
      Caption = 'aVPlainSQL'
      Enabled = False
      HelpContext = 1037
      HelpType = htContext
      ImageIndex = 3
      ShortCut = 119
    end
    object aVDiagram: TAction
      Category = 'View'
      Caption = 'aVDiagram'
      Enabled = False
      HelpContext = 1125
      ImageIndex = 99
    end
    object aVAddressBar: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'aVAddressBar'
      HelpContext = 1081
      HelpType = htContext
      OnExecute = aVAddressBarExecute
    end
    object aVNavigator: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'aVNavigator'
      Enabled = False
      HelpContext = 1038
      HelpType = htContext
      ImageIndex = 94
      ShortCut = 122
    end
    object aVBookmarks: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'aVBookmarks'
      Enabled = False
      HelpContext = 1082
      HelpType = htContext
      ImageIndex = 73
      ShortCut = 32890
    end
    object aVSQLHistory: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'aVSQLHistory'
      Enabled = False
      HelpContext = 1112
      HelpType = htContext
      ImageIndex = 95
    end
    object aVSQLLog: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'aVSQLLog'
      Enabled = False
      HelpContext = 1039
      HelpType = htContext
      ShortCut = 16506
    end
    object aVDetails: TAction
      Category = 'View'
      Caption = 'aVDetails'
      Enabled = False
      HelpContext = 1086
      HelpType = htContext
    end
    object aVAddress: TAction
      Category = 'View'
      Caption = 'aVAddress'
      OnExecute = aVAddressExecute
    end
    object aVRefresh: TAction
      Category = 'View'
      Caption = 'aVRefresh'
      Enabled = False
      ImageIndex = 22
      ShortCut = 116
    end
    object aVRefreshAll: TAction
      Category = 'View'
      Caption = 'aVRefreshAll'
      Enabled = False
      ImageIndex = 22
      ShortCut = 8308
    end
    object aVPrev: TAction
      Category = 'View'
      Caption = 'aVPrev'
      ImageIndex = 92
      ShortCut = 32805
      OnExecute = aVPrevExecute
    end
    object aVNext: TAction
      Category = 'View'
      Caption = 'aVNext'
      ImageIndex = 93
      ShortCut = 32807
      OnExecute = aVNextExecute
    end
    object aFOpenAccount: TAction
      Category = 'File'
      Caption = 'aFOpenAccount'
      HelpContext = 1001
      HelpType = htContext
      ImageIndex = 40
      ShortCut = 123
      OnExecute = aFOpenAccountExecute
    end
    object aFOpen: TAction
      Category = 'File'
      Caption = 'aFOpen'
      Enabled = False
      HelpContext = 1006
      HelpType = htContext
      ImageIndex = 5
      ShortCut = 16463
    end
    object aFSave: TAction
      Category = 'File'
      Caption = 'aFSave'
      Enabled = False
      HelpContext = 1007
      HelpType = htContext
      ImageIndex = 6
      ShortCut = 16467
    end
    object aFSaveAs: TAction
      Category = 'File'
      Caption = 'aFSaveAs'
      Enabled = False
      HelpContext = 1008
      HelpType = htContext
    end
    object aFImportSQL: TAction
      Category = 'File'
      Caption = 'aFImportSQL'
      Enabled = False
      HelpContext = 1010
      HelpType = htContext
    end
    object aFImportText: TAction
      Category = 'File'
      Caption = 'aFImportText'
      Enabled = False
      HelpContext = 1133
      HelpType = htContext
    end
    object aFImportExcel: TAction
      Category = 'File'
      Caption = 'aFImportExcel'
      Enabled = False
      HelpContext = 1106
      HelpType = htContext
    end
    object aFImportAccess: TAction
      Category = 'File'
      Caption = 'aFImportAccess'
      Enabled = False
      HelpContext = 1013
      HelpType = htContext
    end
    object aFImportSQLite: TAction
      Category = 'File'
      Caption = 'aFImportSQLite'
      Enabled = False
      HelpContext = 1127
      HelpType = htContext
    end
    object aFImportODBC: TAction
      Category = 'File'
      Caption = 'aFImportODBC'
      Enabled = False
      HelpContext = 1012
      HelpType = htContext
    end
    object aFImportXML: TAction
      Category = 'File'
      Caption = 'aFImportXML'
      Enabled = False
      HelpContext = 1132
      HelpType = htContext
    end
    object aFExportSQL: TAction
      Category = 'File'
      Caption = 'aFExportSQL'
      Enabled = False
      HelpContext = 1014
      HelpType = htContext
    end
    object aFExportText: TAction
      Category = 'File'
      Caption = 'aFExportText'
      Enabled = False
      HelpContext = 1134
      HelpType = htContext
    end
    object aFExportExcel: TAction
      Category = 'File'
      Caption = 'aFExportExcel'
      Enabled = False
      HelpContext = 1107
      HelpType = htContext
    end
    object aFExportAccess: TAction
      Category = 'File'
      Caption = 'aFExportAccess'
      Enabled = False
      HelpContext = 1129
      HelpType = htContext
    end
    object aFExportSQLite: TAction
      Category = 'File'
      Caption = 'aFExportSQLite'
      Enabled = False
      HelpContext = 1128
      HelpType = htContext
    end
    object aFExportODBC: TAction
      Category = 'File'
      Caption = 'aFExportODBC'
      Enabled = False
    end
    object aFExportXML: TAction
      Category = 'File'
      Caption = 'aFExportXML'
      Enabled = False
      HelpContext = 1017
      HelpType = htContext
    end
    object aFExportHTML: TAction
      Category = 'File'
      Caption = 'aFExportHTML'
      Enabled = False
      HelpContext = 1016
      HelpType = htContext
    end
    object aFExportBitmap: TAction
      Category = 'File'
      Caption = 'aFExportBitmap'
      Enabled = False
    end
    object aFPrint: TAction
      Category = 'File'
      Caption = 'aFPrint'
      Enabled = False
      HelpContext = 1019
      HelpType = htContext
      ShortCut = 16464
    end
    object aESelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'aESelectAll'
      HelpContext = 1028
      HelpType = htContext
    end
    object aEPaste: TEditPaste
      Category = 'Edit'
      Caption = 'aEPaste'
      Enabled = False
      HelpContext = 1026
      HelpType = htContext
      ImageIndex = 9
    end
    object aSSearchFind: TSearchFind_Ext
      Category = 'Search'
      Caption = 'aSSearchFind'
      Enabled = False
      HelpContext = 1032
      HelpType = htContext
      ImageIndex = 12
      ShortCut = 16454
      OnNotFound = aSSearchFindNotFound
    end
    object aSSearchReplace: TSearchReplace_Ext
      Category = 'Search'
      Caption = 'aSSearchReplace'
      Enabled = False
      HelpContext = 1033
      HelpType = htContext
      ImageIndex = 29
      ShortCut = 16466
    end
    object aSSearchNext: TSearchFindNext
      Category = 'Search'
      Caption = 'aSSearchFindNext'
      Enabled = False
      HelpContext = 1034
      SearchFind = aSSearchFind
      ShortCut = 114
    end
    object aSGoto: TAction
      Category = 'Search'
      Caption = 'aSGoto'
      Enabled = False
      ShortCut = 16455
    end
    object aSAddress: TAction
      Category = 'Search'
      Caption = 'aSAddress'
      Enabled = False
      ShortCut = 16460
      OnExecute = aSAddressExecute
    end
    object aBAdd: TAction
      Category = 'Bookmark'
      Caption = 'aBAdd'
      Enabled = False
      HelpContext = 1087
      HelpType = htContext
    end
    object aBDelete: TAction
      Category = 'Bookmark'
      Caption = 'aBDelete'
      Enabled = False
    end
    object aBEdit: TAction
      Category = 'Bookmark'
      Caption = 'aBEdit'
      Enabled = False
      HelpContext = 1088
      HelpType = htContext
    end
    object aDCreateDatabase: TAction
      Category = 'Database'
      Caption = 'aDCreateDatabase'
      Enabled = False
      HelpContext = 1044
      HelpType = htContext
      ImageIndex = 59
    end
    object aDCreateTable: TAction
      Category = 'Database'
      Caption = 'aDCreateTable'
      Enabled = False
      HelpContext = 1045
      HelpType = htContext
      ImageIndex = 61
    end
    object aDCreateView: TAction
      Category = 'Database'
      Caption = 'aDCreateView'
      Enabled = False
      HelpContext = 1096
      HelpType = htContext
    end
    object aDCreateProcedure: TAction
      Category = 'Database'
      Caption = 'aDCreateProcedure'
      Enabled = False
      HelpContext = 1097
      HelpType = htContext
    end
    object aDCreateFunction: TAction
      Category = 'Database'
      Caption = 'aDCreateFunction'
      Enabled = False
      HelpContext = 1097
      HelpType = htContext
    end
    object aDCreateEvent: TAction
      Category = 'Database'
      Caption = 'aDCreateEvent'
      Enabled = False
      HelpContext = 1114
      HelpType = htContext
    end
    object aDCreateTrigger: TAction
      Category = 'Database'
      Caption = 'aDCreateTrigger'
      Enabled = False
      HelpContext = 1102
      HelpType = htContext
    end
    object aDCreateIndex: TAction
      Category = 'Database'
      Caption = 'aDCreateIndex'
      Enabled = False
      HelpContext = 1046
      HelpType = htContext
      ImageIndex = 63
    end
    object aDCreateField: TAction
      Category = 'Database'
      Caption = 'aDCreateField'
      Enabled = False
      HelpContext = 1047
      HelpType = htContext
      ImageIndex = 65
    end
    object aDCreateForeignKey: TAction
      Category = 'Database'
      Caption = 'aDCreateForeignKey'
      Enabled = False
      HelpContext = 1048
      HelpType = htContext
      ImageIndex = 67
    end
    object aDCreateHost: TAction
      Category = 'Database'
      Caption = 'aDCreateHost'
      Enabled = False
      HelpContext = 1076
      HelpType = htContext
    end
    object aDCreateUser: TAction
      Category = 'Database'
      Caption = 'aDCreateUser'
      Enabled = False
      HelpContext = 1077
      HelpType = htContext
    end
    object aDDeleteDatabase: TAction
      Category = 'Database'
      Caption = 'aDDeleteDatabase'
      Enabled = False
      HelpContext = 1049
      HelpType = htContext
      ImageIndex = 60
    end
    object aDDeleteTable: TAction
      Category = 'Database'
      Caption = 'aDDeleteTable'
      Enabled = False
      HelpContext = 1051
      HelpType = htContext
      ImageIndex = 62
    end
    object aDDeleteView: TAction
      Category = 'Database'
      Caption = 'aDDeleteView'
      Enabled = False
      HelpContext = 1100
      HelpType = htContext
    end
    object aDDeleteRoutine: TAction
      Category = 'Database'
      Caption = 'aDDeleteRoutine'
      Enabled = False
      HelpContext = 1101
      HelpType = htContext
    end
    object aDDeleteEvent: TAction
      Category = 'Database'
      Caption = 'aDDeleteEvent'
      Enabled = False
      HelpContext = 1115
      HelpType = htContext
    end
    object aDDeleteTrigger: TAction
      Category = 'Database'
      Caption = 'aDDeleteTrigger'
      Enabled = False
    end
    object aDDeleteIndex: TAction
      Category = 'Database'
      Caption = 'aDDeleteIndex'
      Enabled = False
      HelpContext = 1052
      HelpType = htContext
      ImageIndex = 64
    end
    object aDDeleteField: TAction
      Category = 'Database'
      Caption = 'aDDeleteField'
      Enabled = False
      HelpContext = 1053
      HelpType = htContext
      ImageIndex = 66
    end
    object aDDeleteForeignKey: TAction
      Category = 'Database'
      Caption = 'aDDeleteForeignKey'
      Enabled = False
      HelpContext = 1053
      HelpType = htContext
      ImageIndex = 72
    end
    object aDDeleteHost: TAction
      Category = 'Database'
      Caption = 'aDDeleteHost'
      Enabled = False
      HelpContext = 1079
      HelpType = htContext
    end
    object aDDeleteProcess: TAction
      Category = 'Database'
      Caption = 'aDDeleteProcesss'
      Enabled = False
      HelpContext = 1078
      HelpType = htContext
    end
    object aDDeleteUser: TAction
      Category = 'Database'
      Caption = 'aDDeleteUser'
      Enabled = False
      HelpContext = 1080
      HelpType = htContext
    end
    object aDEditServer: TAction
      Category = 'Database'
      Caption = 'aDEditServer'
      Enabled = False
      HelpContext = 1091
      HelpType = htContext
    end
    object aDEditDatabase: TAction
      Category = 'Database'
      Caption = 'aDEditDatabase'
      Enabled = False
    end
    object aDEditTable: TAction
      Category = 'Database'
      Caption = 'aDEditTable'
      Enabled = False
      HelpContext = 1054
      HelpType = htContext
    end
    object aDEditView: TAction
      Category = 'Database'
      Caption = 'aDEditView'
      Enabled = False
      HelpContext = 1098
      HelpType = htContext
    end
    object aDEditRoutine: TAction
      Category = 'Database'
      Caption = 'aDEditRoutine'
      Enabled = False
      HelpContext = 1099
      HelpType = htContext
    end
    object aDEditEvent: TAction
      Category = 'Database'
      Caption = 'aDEditEvent'
      Enabled = False
      HelpContext = 1113
      HelpType = htContext
    end
    object aDEditTrigger: TAction
      Category = 'Database'
      Caption = 'aDEditTrigger'
      Enabled = False
    end
    object aDEditIndex: TAction
      Category = 'Database'
      Caption = 'aDEditIndex'
      Enabled = False
      HelpContext = 1055
      HelpType = htContext
    end
    object aDEditField: TAction
      Category = 'Database'
      Caption = 'aDEditField'
      Enabled = False
      HelpContext = 1056
      HelpType = htContext
    end
    object aDEditForeignKey: TAction
      Category = 'Database'
      Caption = 'aDEditForeignKey'
      Enabled = False
      HelpContext = 1057
      HelpType = htContext
    end
    object aDEditHost: TAction
      Category = 'Database'
      Caption = 'aDEditHost'
      Enabled = False
      HelpContext = 1058
      HelpType = htContext
    end
    object aDEditProcess: TAction
      Category = 'Database'
      Caption = 'aDEditProcess'
      Enabled = False
    end
    object aDEditUser: TAction
      Category = 'Database'
      Caption = 'aDEditUser'
      Enabled = False
      HelpContext = 1059
      HelpType = htContext
    end
    object aDEditVariable: TAction
      Category = 'Database'
      Caption = 'aDEditVariable'
      Enabled = False
      HelpContext = 1060
      HelpType = htContext
    end
    object aDInsertRecord: TAction
      Category = 'Database'
      Caption = 'aDInsertRecord'
      Enabled = False
      HelpContext = 1061
      HelpType = htContext
      ImageIndex = 19
    end
    object aDDeleteRecord: TAction
      Category = 'Database'
      Caption = 'aDDeleteRecord'
      Enabled = False
      HelpContext = 1062
      HelpType = htContext
      ImageIndex = 20
    end
    object aDEditRecord: TAction
      Category = 'Database'
      Caption = 'aDEdit'
      Enabled = False
      HelpContext = 1063
      HelpType = htContext
    end
    object aDPostRecord: TDataSetPost
      Category = 'Database'
      Caption = 'aDPostRecord'
      Enabled = False
      ShortCut = 16397
    end
    object aDCancelRecord: TDataSetCancel
      Category = 'Database'
      Caption = 'aDCancelRecord'
      Enabled = False
    end
    object aDCancel: TAction
      Category = 'Database'
      Enabled = False
      ImageIndex = 68
    end
    object aDRun: TAction
      Category = 'Database'
      Caption = 'aDRun'
      Enabled = False
      HelpContext = 1042
      HelpType = htContext
      ImageIndex = 18
      ShortCut = 120
    end
    object aDRunSelection: TAction
      Category = 'Database'
      Caption = 'aDRunSelection'
      Enabled = False
      HelpContext = 1043
      HelpType = htContext
      ImageIndex = 30
      ShortCut = 16504
    end
    object aDPostObject: TAction
      Category = 'Database'
      Caption = 'aDPostObject'
      Enabled = False
      ImageIndex = 97
      ShortCut = 8312
      Visible = False
    end
    object aDEmpty: TAction
      Category = 'Database'
      Caption = 'aDEmpty'
      Enabled = False
      HelpContext = 1064
      HelpType = htContext
      ImageIndex = 17
    end
    object aDAutoCommit: TAction
      Category = 'Database'
      Caption = 'aDAutoCommit'
      Enabled = False
    end
    object aDCommit: TAction
      Category = 'Database'
      Caption = 'aDCommit'
      Enabled = False
    end
    object aDRollback: TAction
      Category = 'Database'
      Caption = 'aDRollback'
      Enabled = False
    end
    object aOGlobals: TAction
      Category = 'Options'
      Caption = 'aOGlobalSettings'
      HelpContext = 1066
      HelpType = htContext
      OnExecute = aOGlobalsExecute
    end
    object aEFind: TAction
      Category = 'Extras'
      Caption = 'aEFind'
      OnExecute = aEFindExecute
    end
    object aEReplace: TAction
      Category = 'Extras'
      Caption = 'aEReplace'
      HelpContext = 1090
      HelpType = htContext
      OnExecute = aEReplaceExecute
    end
    object aEUndo: TEditUndo
      Category = 'Edit'
      Caption = 'aEUndo'
      HelpContext = 1023
      ImageIndex = 37
      ShortCut = 16474
    end
    object aERedo: TAction
      Category = 'Edit'
      Caption = 'aERedo'
      HelpContext = 1092
      ImageIndex = 38
      ShortCut = 16473
    end
    object aECut: TEditCut
      Category = 'Edit'
      Caption = 'aECut'
      Enabled = False
      HelpContext = 1024
      HelpType = htContext
      ImageIndex = 7
    end
    object aOAccounts: TAction
      Category = 'Options'
      Caption = 'aOAccounts'
      HelpContext = 1065
      HelpType = htContext
      OnExecute = aOAccountsExecute
    end
    object aEDelete: TEditDelete
      Category = 'Edit'
      Caption = 'aEDelete'
      Enabled = False
      HelpContext = 1027
      HelpType = htContext
      ImageIndex = 10
    end
    object aECopy: TEditCopy
      Category = 'Edit'
      Caption = 'aECopy'
      Enabled = False
      HelpContext = 1025
      HelpType = htContext
      ImageIndex = 8
    end
    object aECopyToFile: TAction
      Category = 'Edit'
      Caption = 'aECopyToFile'
      Enabled = False
      HelpContext = 1029
      HelpType = htContext
    end
    object aEPasteFromFile: TAction
      Category = 'Edit'
      Caption = 'aEPasteFromFile'
      Enabled = False
      HelpContext = 1030
      HelpType = htContext
    end
    object aFClose: TAction
      Category = 'File'
      Caption = 'aFClose'
      Enabled = False
      HelpContext = 1020
      HelpType = htContext
      ImageIndex = 45
      ShortCut = 16499
      OnExecute = aFCloseExecute
    end
    object aFCloseAll: TAction
      Category = 'File'
      Caption = 'aFCloseAll'
      Enabled = False
      HelpContext = 1021
      HelpType = htContext
      OnExecute = aFCloseAllExecute
    end
    object aFExit: TAction
      Category = 'File'
      Caption = 'aFExit'
      HelpContext = 1022
      HelpType = htContext
      ShortCut = 32883
      OnExecute = aFExitExecute
    end
    object aHIndex: TAction
      Category = 'Help'
      Caption = 'aHIndex'
      HelpContext = 1069
      HelpType = htContext
      ShortCut = 112
      OnExecute = aHIndexExecute
    end
    object aHManual: TAction
      Category = 'Help'
      Caption = 'aHManual'
      Enabled = False
      HelpContext = 1119
      HelpType = htContext
      OnExecute = aHManualExecute
    end
    object aHUpdate: TAction
      Category = 'Help'
      Caption = 'aHUpdate'
      HelpContext = 1073
      HelpType = htContext
      OnExecute = aHUpdateExecute
    end
    object aHInfo: TAction
      Category = 'Help'
      Caption = 'aHInfo'
      HelpContext = 1075
      HelpType = htContext
      OnExecute = aHInfoExecute
    end
    object aERename: TAction
      Category = 'Edit'
      Caption = 'aERename'
      HelpContext = 1031
      HelpType = htContext
      ShortCut = 113
    end
    object aETransfer: TAction
      Category = 'Extras'
      Caption = 'aETransfer'
      HelpType = htContext
      OnExecute = aETransferExecute
    end
    object aBookmark: TAction
      Category = 'Bookmark'
      Caption = 'aBookmark'
    end
    object aHFeedback: TAction
      Category = 'Help'
      Caption = 'Send Feedback...'
      OnExecute = aHFeedbackExecute
    end
  end
  object OpenDialog: TOpenDialog_Ext
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    EncodingIndex = -1
    EncodingLabel = '&Encoding:'
    Left = 16
    Top = 288
  end
  object SaveDialog: TSaveDialog_Ext
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    EncodingIndex = -1
    EncodingLabel = '&Encoding:'
    Left = 80
    Top = 288
  end
  object MainMenu: TMainMenu
    Left = 16
    Top = 240
    object miFile: TMenuItem
      Caption = 'miFile'
      object miFConnect: TMenuItem
        Action = aFOpenAccount
      end
      object N8: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object miFOpen: TMenuItem
        Action = aFOpen
        GroupIndex = 1
      end
      object miFSave: TMenuItem
        Action = aFSave
        GroupIndex = 1
      end
      object miFSaveAs: TMenuItem
        Action = aFSaveAs
        GroupIndex = 1
      end
      object N18: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object miFImport: TMenuItem
        Caption = 'miFImport'
        GroupIndex = 1
        object miFImportSQL: TMenuItem
          Action = aFImportSQL
        end
        object miFImportText: TMenuItem
          Action = aFImportText
        end
        object miFImportExcel: TMenuItem
          Action = aFImportExcel
        end
        object miFImportAccess: TMenuItem
          Action = aFImportAccess
        end
        object miFImportSQLite1: TMenuItem
          Action = aFImportSQLite
        end
        object miFImportODBC: TMenuItem
          Action = aFImportODBC
        end
        object miFImportXML: TMenuItem
          Action = aFImportXML
        end
      end
      object miFExport: TMenuItem
        Caption = 'miFExport'
        GroupIndex = 1
        object miFExportSQL: TMenuItem
          Action = aFExportSQL
        end
        object miFExportText: TMenuItem
          Action = aFExportText
        end
        object miFExportExcel: TMenuItem
          Action = aFExportExcel
        end
        object miFExportAccess: TMenuItem
          Action = aFExportAccess
        end
        object miFExportSQLite: TMenuItem
          Action = aFExportSQLite
        end
        object aFExportODBC1: TMenuItem
          Action = aFExportODBC
        end
        object miFExportXML: TMenuItem
          Action = aFExportXML
        end
        object miFExportHTML: TMenuItem
          Action = aFExportHTML
        end
        object miFExportBitmap: TMenuItem
          Action = aFExportBitmap
        end
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object miFPrint: TMenuItem
        Action = aFPrint
        GroupIndex = 1
      end
      object N17: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object miFClose: TMenuItem
        Action = aFClose
        GroupIndex = 1
      end
      object aFCloseAll1: TMenuItem
        Action = aFCloseAll
        GroupIndex = 1
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object miFExit: TMenuItem
        Action = aFExit
        GroupIndex = 1
      end
    end
    object miEdit: TMenuItem
      Caption = 'miEdit'
      object miEUndo: TMenuItem
        Action = aEUndo
      end
      object miERedo: TMenuItem
        Action = aERedo
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object miECut: TMenuItem
        Action = aECut
      end
      object miECopy: TMenuItem
        Action = aECopy
      end
      object miEPaste: TMenuItem
        Action = aEPaste
      end
      object miEDelete: TMenuItem
        Action = aEDelete
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object miECopyToFile: TMenuItem
        Action = aECopyToFile
      end
      object aEPasteFrom1: TMenuItem
        Action = aEPasteFromFile
      end
      object N21: TMenuItem
        Caption = '-'
      end
      object miESelectAll: TMenuItem
        Action = aESelectAll
      end
      object N25: TMenuItem
        Caption = '-'
      end
      object miERename: TMenuItem
        Action = aERename
      end
    end
    object miSearch: TMenuItem
      Caption = 'miSearch'
      object miSSearchFind: TMenuItem
        Action = aSSearchFind
      end
      object miSSearchReplace: TMenuItem
        Action = aSSearchReplace
      end
      object miSSearchNext: TMenuItem
        Action = aSSearchNext
      end
      object N24: TMenuItem
        Caption = '-'
      end
      object miSGoto: TMenuItem
        Action = aSGoto
      end
      object N26: TMenuItem
        Caption = '-'
      end
      object miSAddress: TMenuItem
        Action = aSAddress
      end
    end
    object miView: TMenuItem
      Caption = 'miView'
      GroupIndex = 3
      object miVObjectBrowser: TMenuItem
        Action = aVObjectBrowser
        RadioItem = True
      end
      object miVBrowser: TMenuItem
        Action = aVDataBrowser
        RadioItem = True
      end
      object miVObjectIDE: TMenuItem
        Action = aVObjectIDE
      end
      object miVQueryBuilder: TMenuItem
        Action = aVQueryBuilder
      end
      object miVSQLEditor: TMenuItem
        Action = aVSQLEditor
        RadioItem = True
      end
      object miVDiagram: TMenuItem
        Action = aVDiagram
        RadioItem = True
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object miVAddressBar: TMenuItem
        Action = aVAddressBar
        AutoCheck = True
      end
      object miVSidebar: TMenuItem
        Caption = 'miVSidebar'
        object miVNavigator: TMenuItem
          Action = aVNavigator
          AutoCheck = True
        end
        object miVBookmarks: TMenuItem
          Action = aVBookmarks
          AutoCheck = True
        end
        object aVSQLHistory1: TMenuItem
          Action = aVSQLHistory
          AutoCheck = True
        end
      end
      object miVSQLLog: TMenuItem
        Action = aVSQLLog
        AutoCheck = True
      end
      object N22: TMenuItem
        Caption = '-'
      end
      object aVDetails1: TMenuItem
        Action = aVDetails
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object miVRefresh: TMenuItem
        Action = aVRefresh
        HelpContext = 1040
      end
      object miVRefreshAll: TMenuItem
        Action = aVRefreshAll
        HelpContext = 1041
      end
    end
    object miBookmarks: TMenuItem
      Caption = 'miBookmarks'
      GroupIndex = 3
      object miBAdd: TMenuItem
        Action = aBAdd
      end
      object miBDelete: TMenuItem
        Action = aBDelete
      end
      object miBEdit: TMenuItem
        Action = aBEdit
      end
      object miBSeparator: TMenuItem
        Caption = '-'
      end
    end
    object miDatabase: TMenuItem
      Caption = 'miDatabase'
      GroupIndex = 3
      object miDCreate: TMenuItem
        Caption = 'miDCreate'
        object miDCreateDatabase: TMenuItem
          Action = aDCreateDatabase
        end
        object miDCreateTable: TMenuItem
          Action = aDCreateTable
        end
        object miDCreateView: TMenuItem
          Action = aDCreateView
        end
        object miDCreateRoutine: TMenuItem
          Action = aDCreateProcedure
        end
        object aDCreateFunction1: TMenuItem
          Action = aDCreateFunction
        end
        object miDCreateEvent: TMenuItem
          Action = aDCreateEvent
        end
        object miDCreateIndex: TMenuItem
          Action = aDCreateIndex
        end
        object miDCreateField: TMenuItem
          Action = aDCreateField
        end
        object miDCreateForeignKey: TMenuItem
          Action = aDCreateForeignKey
        end
        object miDCreateTrigger: TMenuItem
          Action = aDCreateTrigger
        end
        object miDCreateHost: TMenuItem
          Action = aDCreateHost
        end
        object miDCreateUser: TMenuItem
          Action = aDCreateUser
        end
      end
      object miDDelete: TMenuItem
        Caption = 'miDDelete'
        object miDDeleteDatabase: TMenuItem
          Action = aDDeleteDatabase
        end
        object miDDeleteTable: TMenuItem
          Action = aDDeleteTable
        end
        object miDDeleteView: TMenuItem
          Action = aDDeleteView
        end
        object miDDeleteRoutine: TMenuItem
          Action = aDDeleteRoutine
        end
        object miDDeleteEvent: TMenuItem
          Action = aDDeleteEvent
        end
        object miDDeleteIndex: TMenuItem
          Action = aDDeleteIndex
        end
        object miDDeleteField: TMenuItem
          Action = aDDeleteField
        end
        object miDDeleteForeignKey: TMenuItem
          Action = aDDeleteForeignKey
        end
        object miDDeleteTrigger: TMenuItem
          Action = aDDeleteTrigger
        end
        object miDDeleteHost: TMenuItem
          Action = aDDeleteHost
        end
        object miDDeleteProcess: TMenuItem
          Action = aDDeleteProcess
        end
        object miDDeleteUser: TMenuItem
          Action = aDDeleteUser
        end
      end
      object miDProperties: TMenuItem
        Caption = 'miDProperties'
        object miDEditServer: TMenuItem
          Action = aDEditServer
        end
        object miDEditDatabase: TMenuItem
          Action = aDEditDatabase
        end
        object miDEditTable: TMenuItem
          Action = aDEditTable
        end
        object miDEditRoutine: TMenuItem
          Action = aDEditRoutine
        end
        object miDEditEvent: TMenuItem
          Action = aDEditEvent
        end
        object miDEditView: TMenuItem
          Action = aDEditView
        end
        object miDEditIndex: TMenuItem
          Action = aDEditIndex
        end
        object miDEditField: TMenuItem
          Action = aDEditField
        end
        object miDEditForeignKey: TMenuItem
          Action = aDEditForeignKey
        end
        object miDEditTrigger: TMenuItem
          Action = aDEditTrigger
        end
        object miDEditHost: TMenuItem
          Action = aDEditHost
        end
        object miDEditProcess: TMenuItem
          Action = aDEditProcess
        end
        object miDEditUser: TMenuItem
          Action = aDEditUser
        end
        object miDEditVariable: TMenuItem
          Action = aDEditVariable
        end
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object miDInsertRecord: TMenuItem
        Action = aDInsertRecord
      end
      object miDDeleteRecord: TMenuItem
        Action = aDDeleteRecord
      end
      object miDEditRecord: TMenuItem
        Action = aDEditRecord
      end
      object miDPostRecord: TMenuItem
        Action = aDPostRecord
      end
      object miDCancelRecord: TMenuItem
        Action = aDCancelRecord
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object miDRun: TMenuItem
        Action = aDRun
      end
      object miDRunSelection: TMenuItem
        Action = aDRunSelection
      end
      object aDPostObject1: TMenuItem
        Action = aDPostObject
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object miDEmpty: TMenuItem
        Action = aDEmpty
      end
      object N20: TMenuItem
        Caption = '-'
      end
      object miDAutoCommit: TMenuItem
        Action = aDAutoCommit
      end
      object miDCommit: TMenuItem
        Action = aDCommit
      end
      object miDRollback: TMenuItem
        Action = aDRollback
      end
    end
    object miExtras: TMenuItem
      Caption = 'miExtras'
      GroupIndex = 3
      object miEFind: TMenuItem
        Action = aEFind
      end
      object miEReplace: TMenuItem
        Action = aEReplace
      end
      object N27: TMenuItem
        Caption = '-'
      end
      object miETransfer: TMenuItem
        Action = aETransfer
      end
    end
    object miOptions: TMenuItem
      Caption = 'miOptions'
      GroupIndex = 3
      object miOGlobals: TMenuItem
        Action = aOGlobals
      end
      object miOAccounts: TMenuItem
        Action = aOAccounts
      end
    end
    object miHelp: TMenuItem
      Caption = 'miHelp'
      GroupIndex = 3
      object miHIndex: TMenuItem
        Action = aHIndex
      end
      object miHManual: TMenuItem
        Action = aHManual
      end
      object N16: TMenuItem
        Caption = '-'
      end
      object miHUpdate: TMenuItem
        Action = aHUpdate
      end
      object miHFeedback: TMenuItem
        Action = aHFeedback
      end
      object N19: TMenuItem
        Caption = '-'
      end
      object miHInfo: TMenuItem
        Action = aHInfo
      end
    end
  end
  object Highlighter: TSynSQLSyn
    SQLDialect = sqlMySQL
    Left = 144
    Top = 288
  end
  object MTabControl: TPopupMenu
    Left = 113
    Top = 112
    object mtFClose: TMenuItem
      Caption = 'mtFClose'
    end
    object mtFCloseAll: TMenuItem
      Action = aFCloseAll
    end
    object N30: TMenuItem
      Caption = '-'
    end
    object mtVRefresh: TMenuItem
      Caption = 'mtVRefresh'
    end
    object mtVRefreshAll: TMenuItem
      Caption = 'mtVRefreshAll'
    end
    object N31: TMenuItem
      Caption = '-'
    end
    object mtDCommit: TMenuItem
      Caption = 'mtDCommit'
    end
    object mtDRollback: TMenuItem
      Caption = 'mtDRollback'
    end
    object N32: TMenuItem
      Caption = '-'
    end
    object mtFOpenAccount: TMenuItem
      Action = aFOpenAccount
    end
    object mtTabs: TMenuItem
      Caption = 'mtTabs'
    end
  end
end
