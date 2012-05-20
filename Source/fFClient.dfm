object FClient: TFClient
  Left = 0
  Top = 0
  Width = 591
  Height = 532
  Align = alClient
  Constraints.MinHeight = 300
  Constraints.MinWidth = 200
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentColor = False
  ParentFont = False
  TabOrder = 0
  Visible = False
  OnResize = FormResize
  ExplicitHeight = 304
  object SLog: TSplitter_Ext
    Left = 0
    Top = 484
    Width = 591
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    Color = clBtnFace
    ParentColor = False
    ResizeStyle = rsUpdate
    OnCanResize = SLogCanResize
    OnMoved = SLogMoved
    ActiveBorder = alTop
    ExplicitTop = 252
    ExplicitWidth = 443
  end
  object SSideBar: TSplitter_Ext
    Left = 120
    Top = 27
    Width = 4
    Height = 457
    AutoSnap = False
    Color = clBtnFace
    ParentColor = False
    ResizeStyle = rsUpdate
    OnCanResize = SSideBarCanResize
    OnMoved = SSideBarMoved
    ActiveBorder = alRight
    ExplicitTop = 0
    ExplicitHeight = 252
  end
  object PSideBar: TPanel_Ext
    Left = 0
    Top = 27
    Width = 120
    Height = 457
    Align = alLeft
    BevelOuter = bvNone
    Color = clWindow
    Constraints.MinWidth = 120
    ParentBackground = False
    TabOrder = 0
    OnResize = PSideBarResize
    ExplicitHeight = 229
    object PSQLHistory: TPanel_Ext
      Left = 0
      Top = 0
      Width = 120
      Height = 457
      Align = alClient
      BevelInner = bvRaised
      BevelOuter = bvLowered
      ParentBackground = False
      TabOrder = 2
      Visible = False
      ExplicitHeight = 229
      object FSQLHistory: TTreeView_Ext
        Left = 2
        Top = 2
        Width = 116
        Height = 453
        HelpContext = 1112
        Align = alClient
        BorderStyle = bsNone
        DragMode = dmAutomatic
        HideSelection = False
        HotTrack = True
        Indent = 19
        PopupMenu = MSQLHistory
        ReadOnly = True
        RightClickSelect = True
        ShowLines = False
        TabOrder = 0
        OnChange = FSQLHistoryChange
        OnCollapsed = TreeViewCollapsed
        OnCollapsing = TreeViewCollapsing
        OnDblClick = FSQLHistoryDblClick
        OnEndDrag = TreeViewEndDrag
        OnEnter = FSQLHistoryEnter
        OnExit = FSQLHistoryExit
        OnExpanded = TreeViewExpanded
        OnKeyDown = FSQLHistoryKeyDown
        OnKeyPress = FSQLHistoryKeyPress
        OnMouseDown = TreeViewMouseDown
        OnMouseUp = TreeViewMouseUp
        ExplicitHeight = 225
      end
    end
    object PBookmarks: TPanel_Ext
      Left = 0
      Top = 0
      Width = 120
      Height = 457
      Align = alClient
      BevelInner = bvRaised
      BevelOuter = bvLowered
      ParentBackground = False
      TabOrder = 1
      Visible = False
      ExplicitHeight = 229
      object FBookmarks: TListView
        Left = 2
        Top = 2
        Width = 116
        Height = 453
        HelpContext = 1082
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
          end>
        ColumnClick = False
        DragMode = dmAutomatic
        ReadOnly = True
        RowSelect = True
        PopupMenu = MBookmarks
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = FBookmarksChange
        OnDblClick = ListViewDblClick
        OnEnter = FBookmarksEnter
        OnExit = FBookmarksExit
        OnDragDrop = FBookmarksDragDrop
        OnDragOver = FBookmarksDragOver
        ExplicitHeight = 225
      end
    end
    object PNavigator: TPanel_Ext
      Left = 0
      Top = 0
      Width = 120
      Height = 457
      Align = alClient
      BevelInner = bvRaised
      BevelOuter = bvLowered
      Caption = 'PNavigator'
      ParentBackground = False
      TabOrder = 0
      Visible = False
      ExplicitHeight = 229
      object FNavigator: TTreeView_Ext
        Left = 2
        Top = 2
        Width = 116
        Height = 453
        HelpContext = 1038
        Align = alClient
        BorderStyle = bsNone
        ChangeDelay = 50
        DragMode = dmAutomatic
        HideSelection = False
        HotTrack = True
        Indent = 19
        ParentShowHint = False
        PopupMenu = MNavigator
        RightClickSelect = True
        RowSelect = True
        ShowHint = True
        ShowLines = False
        ShowRoot = False
        TabOrder = 0
        OnChange = FNavigatorChange
        OnChanging = FNavigatorChanging
        OnCollapsed = TreeViewCollapsed
        OnCollapsing = TreeViewCollapsing
        OnDragDrop = FNavigatorDragDrop
        OnDragOver = FNavigatorDragOver
        OnEdited = FNavigatorEdited
        OnEditing = FNavigatorEditing
        OnEndDrag = TreeViewEndDrag
        OnEnter = FNavigatorEnter
        OnExit = FNavigatorExit
        OnExpanding = FNavigatorExpanding
        OnExpanded = TreeViewExpanded
        OnGetSelectedIndex = TreeViewGetSelectedIndex
        OnKeyDown = FNavigatorKeyDown
        OnKeyPress = FNavigatorKeyPress
        OnMouseDown = TreeViewMouseDown
        OnMouseUp = TreeViewMouseUp
        ExplicitHeight = 225
      end
    end
  end
  object PLog: TPanel_Ext
    Left = 0
    Top = 488
    Width = 591
    Height = 44
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentBackground = False
    TabOrder = 2
    OnResize = PLogResize
    ExplicitTop = 260
    object FLog: TRichEdit
      Left = 19
      Top = 2
      Width = 570
      Height = 40
      HelpContext = 1039
      TabStop = False
      Align = alClient
      BorderStyle = bsNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      HideSelection = False
      Constraints.MinHeight = 35
      ParentFont = False
      PopupMenu = MLog
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 1
      WantReturns = False
      WordWrap = False
      OnEnter = FLogEnter
      OnExit = FLogExit
      OnSelectionChange = FLogSelectionChange
    end
    object PLogHeader: TPanel_Ext
      Left = 2
      Top = 2
      Width = 17
      Height = 40
      Align = alLeft
      BevelOuter = bvNone
      BorderWidth = 2
      ParentBackground = False
      TabOrder = 0
      OnMouseDown = PanelMouseDown
      OnMouseMove = PanelMouseMove
      OnMouseUp = PanelMouseUp
      OnPaint = PanelPaint
    end
  end
  object PContent: TPanel_Ext
    Left = 124
    Top = 27
    Width = 467
    Height = 457
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    Constraints.MinHeight = 200
    Constraints.MinWidth = 200
    ParentBackground = False
    TabOrder = 1
    OnResize = PContentResize
    ExplicitHeight = 229
    object SResult: TSplitter_Ext
      Left = 0
      Top = 241
      Width = 467
      Height = 4
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      Color = clBtnFace
      Constraints.MinHeight = 4
      ParentColor = False
      ResizeStyle = rsUpdate
      Visible = False
      OnCanResize = SplitterCanResize
      OnMoved = SResultMoved
      ActiveBorder = alBottom
      ExplicitTop = 36
      ExplicitWidth = 319
    end
    object SBlob: TSplitter_Ext
      Left = 0
      Top = 394
      Width = 467
      Height = 4
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      Color = clBtnFace
      Constraints.MinHeight = 4
      ParentColor = False
      ResizeStyle = rsUpdate
      Visible = False
      OnCanResize = SplitterCanResize
      ActiveBorder = alTop
      ExplicitTop = 189
      ExplicitWidth = 319
    end
    object PList: TPanel_Ext
      Left = 0
      Top = 0
      Width = 467
      Height = 56
      HelpContext = 1035
      Align = alTop
      BevelInner = bvRaised
      BevelOuter = bvLowered
      Color = clWindow
      Constraints.MinHeight = 20
      ParentBackground = False
      TabOrder = 3
      Visible = False
      object FList: TListView_Ext
        Left = 2
        Top = 2
        Width = 463
        Height = 52
        HelpContext = 1035
        Align = alClient
        BorderStyle = bsNone
        Columns = <>
        DragMode = dmAutomatic
        HideSelection = False
        MultiSelect = True
        GroupView = True
        PopupMenu = MList
        TabOrder = 0
        ViewStyle = vsReport
        OnChanging = FListChanging
        OnColumnClick = FListColumnClick
        OnCompare = FListCompare
        OnData = FListData
        OnDblClick = ListViewDblClick
        OnEdited = FListEdited
        OnEditing = FListEditing
        OnEnter = FListEnter
        OnExit = FListExit
        OnDragDrop = FNavigatorDragDrop
        OnDragOver = FListDragOver
        OnKeyDown = ListViewKeyDown
        OnSelectItem = FListSelectItem
      end
    end
    object PSQLEditor: TPanel_Ext
      Left = 0
      Top = 310
      Width = 467
      Height = 50
      Align = alTop
      BevelInner = bvRaised
      BevelOuter = bvLowered
      Color = clWindow
      Constraints.MinHeight = 50
      ParentBackground = False
      TabOrder = 2
      Visible = False
      object FSQLEditor: TSynMemo
        Left = 2
        Top = 2
        Width = 463
        Height = 46
        HelpContext = 1037
        OnSearchNotFound = SearchNotFound
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = MSQLEditor
        TabOrder = 0
        OnDragDrop = SynMemoDragDrop
        OnDragOver = SynMemoDragOver
        OnEnter = FSQLEditorEnter
        OnExit = FSQLEditorExit
        BorderStyle = bsNone
        Gutter.AutoSize = True
        Gutter.DigitCount = 2
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 2
        Gutter.ShowLineNumbers = True
        MaxScrollWidth = 1048576
        Options = [eoAutoIndent, eoDragDropEditing, eoGroupUndo, eoHideShowScrollbars, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoTabIndent, eoTabsToSpaces]
        RightEdge = 0
        ScrollHintFormat = shfTopToBottom
        SearchEngine = FSQLEditorSearch
        WantTabs = True
        OnStatusChange = FSQLEditorStatusChange
        RemovedKeystrokes = <
          item
            Command = ecContextHelp
            ShortCut = 112
          end>
        AddedKeystrokes = <
          item
            Command = ecContextHelp
            ShortCut = 16496
          end>
      end
    end
    object PResult: TPanel_Ext
      Left = 0
      Top = 264
      Width = 467
      Height = 130
      Align = alBottom
      BevelOuter = bvNone
      Constraints.MinHeight = 130
      ParentBackground = False
      TabOrder = 5
      Visible = False
      OnResize = PSideBarResize
      ExplicitTop = 36
      object PResultHeader: TPanel_Ext
        Left = 0
        Top = 0
        Width = 17
        Height = 130
        Align = alLeft
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 0
        OnMouseDown = PanelMouseDown
        OnMouseMove = PanelMouseMove
        OnMouseUp = PanelMouseUp
        OnPaint = PanelPaint
      end
      object PResult_2: TPanel_Ext
        Left = 17
        Top = 0
        Width = 450
        Height = 130
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 1
        object PGrid: TPanel_Ext
          Left = 0
          Top = 26
          Width = 450
          Height = 104
          Align = alClient
          BevelInner = bvRaised
          BevelOuter = bvLowered
          Color = clWindow
          Constraints.MinHeight = 50
          ParentBackground = False
          TabOrder = 0
          OnResize = PGridResize
          object FGrid: TMySQLDBGrid
            Left = 2
            Top = 2
            Width = 446
            Height = 100
            HelpContext = 1036
            Align = alClient
            BorderStyle = bsNone
            Constraints.MinHeight = 30
            DataSource = FGridDataSource
            DefaultDrawing = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgMultiSelect, dgTitleClick, dgTitleHotTrack]
            ParentFont = False
            PopupMenu = MGrid
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
            OnCellClick = FGridCellClick
            OnColEnter = DBGridColEnter
            OnColExit = DBGridColExit
            OnColumnMoved = FGridColumnMoved
            OnDrawColumnCell = DBGridDrawColumnCell
            OnDblClick = FGridDblClick
            OnEditButtonClick = FGridEditButtonClick
            OnEnter = DBGridEnter
            OnExit = DBGridExit
            OnKeyDown = FGridKeyDown
            OnMouseMove = DBGridMouseMove
            OnTitleClick = FGridTitleClick
          end
        end
        object TCResult: TTabControl
          Left = 0
          Top = 0
          Width = 450
          Height = 26
          Align = alTop
          ParentShowHint = False
          ShowHint = True
          TabHeight = 23
          TabOrder = 1
          Tabs.Strings = (
            'Query 1'
            'Query 2')
          TabIndex = 0
          Visible = False
          OnChange = TCResultChange
          OnChanging = TCResultChanging
        end
      end
    end
    object PBuilder: TPanel_Ext
      Left = 0
      Top = 160
      Width = 467
      Height = 150
      HelpContext = 1120
      Align = alTop
      BevelOuter = bvNone
      Constraints.MinHeight = 150
      ParentBackground = False
      TabOrder = 1
      Visible = False
      object SBuilderQuery: TSplitter_Ext
        Left = 0
        Top = 96
        Width = 467
        Height = 4
        Cursor = crVSplit
        Align = alBottom
        AutoSnap = False
        ResizeStyle = rsUpdate
        OnCanResize = SplitterCanResize
        ExplicitWidth = 319
      end
      object PBuilderQuery: TPanel_Ext
        Left = 0
        Top = 100
        Width = 467
        Height = 50
        Align = alBottom
        BevelInner = bvRaised
        BevelOuter = bvLowered
        Color = clWindow
        Constraints.MinHeight = 50
        ParentBackground = False
        TabOrder = 0
        object FBuilderEditor: TSynMemo
          Left = 2
          Top = 2
          Width = 463
          Height = 46
          HelpContext = 1120
          OnSearchNotFound = SearchNotFound
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          PopupMenu = MSQLEditor
          TabOrder = 0
          OnDragDrop = SynMemoDragDrop
          OnDragOver = SynMemoDragOver
          OnEnter = FBuilderEditorEnter
          OnExit = FBuilderEditorExit
          BorderStyle = bsNone
          Gutter.AutoSize = True
          Gutter.DigitCount = 2
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.LeftOffset = 2
          Gutter.ShowLineNumbers = True
          MaxScrollWidth = 65535
          Options = [eoAutoIndent, eoDragDropEditing, eoGroupUndo, eoHideShowScrollbars, eoScrollHintFollows, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoTabIndent, eoTabsToSpaces]
          ScrollHintFormat = shfTopToBottom
          SearchEngine = FSQLEditorSearch
          WantTabs = True
          OnChange = FBuilderEditorChange
          OnStatusChange = FBuilderEditorStatusChange
        end
      end
      object FBuilder: TacQueryBuilder
        Left = 0
        Top = 0
        Width = 467
        Height = 96
        SyntaxProvider = SyntaxProvider
        MetadataProvider = MetadataProvider
        MetadataFilter = <>
        BorderStyle = qbbsFlat
        SplitterHeight = 4
        LinkPainterClassName = 'TacQueryBuilderLinkPainterAccess'
        TableFont.Charset = DEFAULT_CHARSET
        TableFont.Color = clWindowText
        TableFont.Height = -11
        TableFont.Name = 'MS Sans Serif'
        TableFont.Style = []
        BkImagePos = bkipTopLeft
        BkImageDarkness = bgidLight
        BkColor = clAppWorkSpace
        OnValidatePopupMenu = FBuilderValidatePopupMenu
        TreeFont.Charset = DEFAULT_CHARSET
        TreeFont.Color = clWindowText
        TreeFont.Height = -11
        TreeFont.Name = 'MS Sans Serif'
        TreeFont.Style = []
        GridFont.Charset = DEFAULT_CHARSET
        GridFont.Color = clWindowText
        GridFont.Height = -11
        GridFont.Name = 'MS Sans Serif'
        GridFont.Style = []
        AddObjectFormOptions.Constraints.MinHeight = 150
        AddObjectFormOptions.Constraints.MinWidth = 150
        TreeOptions.TreeVisible = False
        TreeOptions.TreeWidth = 100
        TreeOptions.UnionsNodeText = 'UNIONS'
        TreeOptions.FieldsNodeText = 'FIELDS'
        TreeOptions.FromNodeText = 'FROM'
        TreeOptionsMetadata.TreeVisible = False
        TreeOptionsMetadata.TreeWidth = 100
        TreeOptionsMetadata.TablesNodeName = 'Tables'
        TreeOptionsMetadata.ViewsNodeName = 'Views'
        TreeOptionsMetadata.ProceduresNodeName = 'Procedures'
        TreeOptionsMetadata.SynonymsNodeName = 'Synonyms'
        QuoteAllIdentifiers = True
        FieldsListOptions.MarkColumnOptions.PKIcon.Data = {
          07544269746D617076010000424D760100000000000036000000280000000A00
          00000A0000000100180000000000400100000000000000000000000000000000
          0000FF808055A7D8519AD1FF8080FF8080FF8080FF8080FF8080FF8080FF8080
          00006EBFE886E9F94DD9F54397CEFF8080FF8080FF8080FF8080FF8080FF8080
          00006ABAE6A1E6F838D2F247D6F64298CEFF8080FF8080FF8080FF8080FF8080
          0000FF80805FB0E399E2F653DCF546D9F63F94CE478ED3488FD6FF8080FF8080
          0000FF8080FF80806CBBE766C1E85ED9F24EDBF65BDDF755D8F53483CEFF8080
          0000FF8080FF8080FF808094C9EB89DDF46AE0F673E2F75FDFF655DAF64185CF
          0000FF8080FF8080FF808078BEE7A9EEF97EE6F89AE8F87ED1F080E2F64A9EDB
          0000FF8080FF8080FF8080ABD5EF5EC1EAA3F0FB80D4F07EC7EC56A6DEFF8080
          0000FF8080FF8080FF8080FF8080A1CEED6FC9ECC9F3FB62BEE8FF8080FF8080
          0000FF8080FF8080FF8080FF8080FF808090C8EB6BBCE7FF8080FF8080FF8080
          0000}
        FieldsListOptions.TypeColumnOptions.FontColor = clGrayText
        FieldsListOptions.DescriptionColumnOptions.FontColor = clGrayText
        Align = alClient
        Color = clBtnFace
        Constraints.MinHeight = 96
        Ctl3D = True
        ParentColor = False
        ParentCtl3D = False
        TabOrder = 1
        OnDragDrop = FBuilderDragDrop
        OnDragOver = FBuilderDragOver
        OnEnter = FBuilderEnter
        OnExit = FBuilderExit
        OnResize = FBuilderResize
      end
    end
    object PDataBrowser: TPanel_Ext
      Left = 0
      Top = 56
      Width = 467
      Height = 25
      HelpContext = 1036
      Align = alTop
      BevelOuter = bvNone
      Constraints.MinWidth = 467
      ParentBackground = False
      TabOrder = 0
      Visible = False
      DesignSize = (
        467
        25)
      object PDataBrowserSpacer: TPanel_Ext
        Left = 0
        Top = 23
        Width = 467
        Height = 2
        Align = alBottom
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 9
      end
      object FOffset: TEdit
        Left = 0
        Top = 1
        Width = 43
        Height = 21
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 0
        Text = '0'
        OnChange = FOffsetChange
        OnKeyPress = FOffsetKeyPress
      end
      object FUDOffset: TUpDown
        Left = 43
        Top = 1
        Width = 15
        Height = 21
        Associate = FOffset
        Max = 2147483647
        TabOrder = 1
        Thousands = False
      end
      object FLimit: TEdit
        Left = 59
        Top = 1
        Width = 36
        Height = 21
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 2
        Text = '100'
        OnChange = FLimitChange
        OnKeyPress = FOffsetKeyPress
      end
      object FUDLimit: TUpDown
        Left = 95
        Top = 1
        Width = 15
        Height = 21
        Associate = FLimit
        Min = 1
        Max = 2147483647
        Increment = 10
        Position = 100
        TabOrder = 3
        Thousands = False
      end
      object ToolBarLimitEnabled: TToolBar
        Left = 111
        Top = 0
        Width = 31
        Height = 23
        Align = alNone
        Caption = 'ToolBarLimitEnabled'
        TabOrder = 4
        object FLimitEnabled: TToolButton
          Left = 0
          Top = 0
          Caption = 'FLimitEnabled'
          ImageIndex = 87
          Style = tbsCheck
          OnClick = FLimitEnabledClick
        end
      end
      object FFilter: TComboBox_Ext
        Left = 142
        Top = 1
        Width = 442
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Constraints.MinWidth = 123
        TabOrder = 5
        OnChange = FFilterChange
        OnDropDown = FFilterDropDown
        OnEnter = FFilterEnter
        OnKeyPress = FFilterKeyPress
      end
      object ToolBarFilterEnabled: TToolBar
        Left = 585
        Top = 0
        Width = 31
        Height = 23
        Align = alNone
        Anchors = [akTop, akRight]
        Caption = 'ToolBarFilterEnabled'
        TabOrder = 6
        object FFilterEnabled: TToolButton
          Left = 0
          Top = 0
          Enabled = False
          ImageIndex = 88
          Style = tbsCheck
          OnClick = FFilterEnabledClick
        end
      end
      object FQuickSearch: TEdit
        Left = 616
        Top = 1
        Width = 136
        Height = 21
        Anchors = [akTop, akRight]
        AutoSize = False
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 7
        OnChange = FQuickSearchChange
        OnKeyPress = FQuickSearchKeyPress
      end
      object ToolBarQuickSearchEnabled: TToolBar
        Left = 753
        Top = 0
        Width = 23
        Height = 22
        Align = alNone
        Anchors = [akTop, akRight]
        AutoSize = True
        Caption = 'ToolBarQuickSearchEnabled'
        TabOrder = 8
        object FQuickSearchEnabled: TToolButton
          Left = 0
          Top = 0
          Caption = ' '
          Enabled = False
          ImageIndex = 89
          Style = tbsCheck
          OnClick = FQuickSearchEnabledClick
        end
      end
    end
    object PObjectIDE: TPanel_Ext
      Left = 0
      Top = 81
      Width = 467
      Height = 79
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 4
      Visible = False
      OnResize = PObjectIDEResize
      object PObjectIDESpacer: TPanel_Ext
        Left = 0
        Top = 36
        Width = 467
        Height = 2
        Align = alBottom
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 0
      end
      object FObjectIDEGrid: TMySQLDBGrid
        Left = 0
        Top = 0
        Width = 467
        Height = 36
        HelpContext = 1122
        Align = alClient
        BorderStyle = bsNone
        Ctl3D = True
        DataSource = PObjectIDEGridDataSource
        DefaultDrawing = False
        Options = [dgTitles, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete]
        ParentCtl3D = False
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        OnColEnter = DBGridColEnter
        OnColExit = DBGridColExit
        OnDrawColumnCell = DBGridDrawColumnCell
        OnEnter = DBGridEnter
        OnExit = DBGridExit
        OnMouseMove = DBGridMouseMove
      end
      object PObjectIDETrigger: TPanel_Ext
        Left = 0
        Top = 38
        Width = 467
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 2
        object BINSERT: TButton
          Left = 8
          Top = 8
          Width = 89
          Height = 25
          Caption = 'INSERT'
          TabOrder = 0
          OnClick = BObjectIDEClick
        end
        object BREPLACE: TButton
          Left = 112
          Top = 8
          Width = 89
          Height = 25
          Caption = 'REPLACE'
          TabOrder = 1
          OnClick = BObjectIDEClick
        end
        object BUPDATE: TButton
          Left = 216
          Top = 8
          Width = 89
          Height = 25
          Caption = 'UPDATE'
          TabOrder = 2
          OnClick = BObjectIDEClick
        end
        object BDELETE: TButton
          Left = 320
          Top = 8
          Width = 89
          Height = 25
          Caption = 'DELETE'
          TabOrder = 3
          OnClick = BObjectIDEClick
        end
      end
    end
    object PBlob: TPanel_Ext
      Left = 0
      Top = 398
      Width = 467
      Height = 59
      Align = alBottom
      BevelInner = bvRaised
      BevelOuter = bvLowered
      Constraints.MinHeight = 50
      ParentBackground = False
      TabOrder = 6
      Visible = False
      ExplicitTop = 170
      object FImage: TImage
        Left = 2
        Top = 27
        Width = 463
        Height = 30
        Align = alClient
        Center = True
        PopupMenu = MText
        Proportional = True
        ExplicitWidth = 315
      end
      object ToolBarBlob: TToolBar
        Left = 2
        Top = 2
        Width = 463
        Height = 23
        ButtonHeight = 21
        ButtonWidth = 86
        Color = clBtnFace
        ParentColor = False
        ParentShowHint = False
        ShowCaptions = True
        ShowHint = True
        TabOrder = 0
        Wrapable = False
        OnResize = ToolBarBlobResize
        object tbBlobText: TToolButton
          Left = 0
          Top = 0
          Action = aVBlobText
          AutoSize = True
          Grouped = True
          ParentShowHint = False
          ShowHint = False
          Style = tbsCheck
        end
        object tbBlobRTF: TToolButton
          Left = 65
          Top = 0
          Action = aVBlobRTF
          AutoSize = True
          Grouped = True
          ParentShowHint = False
          ShowHint = False
          Style = tbsCheck
        end
        object tbBlobHTML: TToolButton
          Left = 127
          Top = 0
          Action = aVBlobHTML
          AutoSize = True
          Grouped = True
          ParentShowHint = False
          ShowHint = False
          Style = tbsCheck
        end
        object tbBlobImage: TToolButton
          Left = 189
          Top = 0
          Action = aVBlobImage
          AutoSize = True
          Grouped = True
          ParentShowHint = False
          ShowHint = False
          Style = tbsCheck
        end
        object tbBlobHexEditor: TToolButton
          Left = 262
          Top = 0
          Action = aVBlobHexEditor
          AutoSize = True
          Grouped = True
          ParentShowHint = False
          ShowHint = False
          Style = tbsCheck
        end
        object tbBlobSpacer: TPanel_Ext
          Left = 352
          Top = 0
          Width = 49
          Height = 21
          BevelOuter = bvNone
          ParentBackground = False
          ParentShowHint = False
          ShowHint = False
          TabOrder = 1
        end
        object FBlobSearch: TEdit
          Left = 401
          Top = 0
          Width = 136
          Height = 21
          AutoSize = False
          TabOrder = 0
          OnChange = FBlobSearchChange
          OnKeyPress = FBlobSearchKeyPress
        end
      end
      object FText: TMemo_Ext
        Left = 2
        Top = 27
        Width = 463
        Height = 30
        Align = alClient
        BorderStyle = bsNone
        HideSelection = False
        PopupMenu = MText
        ScrollBars = ssVertical
        TabOrder = 1
        WantTabs = True
        OnChange = FTextChange
        OnEnter = FTextEnter
        OnExit = FTextExit
        OnKeyPress = FTextKeyPress
      end
      object FRTF: TRichEdit
        Left = 2
        Top = 27
        Width = 463
        Height = 30
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          'FRTF')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 2
        WordWrap = False
        OnChange = FRTFChange
        OnEnter = FRTFEnter
        OnExit = FRTFExit
      end
      object TSXML: TTabSheet
        Caption = 'TSXML'
      end
      object FHexEditor: TMPHexEditorEx
        Left = 2
        Top = 27
        Width = 463
        Height = 30
        Cursor = crIBeam
        BackupExtension = '.bak'
        PrintOptions.MarginLeft = 20
        PrintOptions.MarginTop = 15
        PrintOptions.MarginRight = 25
        PrintOptions.MarginBottom = 25
        PrintOptions.Flags = [pfSelectionBold, pfMonochrome]
        PrintFont.Charset = DEFAULT_CHARSET
        PrintFont.Color = clWindowText
        PrintFont.Height = -15
        PrintFont.Name = 'Courier New'
        PrintFont.Style = []
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        OnEnter = FHexEditorEnter
        OnKeyPress = FHexEditorKeyPress
        ParentFont = False
        PopupMenu = MHexEditor
        TabOrder = 4
        BytesPerRow = 16
        BytesPerColumn = 1
        Translation = tkAsIs
        OffsetFormat = '-!10:|'
        Colors.Background = clWindow
        Colors.ChangedBackground = 11075583
        Colors.ChangedText = clMaroon
        Colors.CursorFrame = clNavy
        Colors.Offset = clBlack
        Colors.OddColumn = clWindowText
        Colors.EvenColumn = clWindowText
        Colors.CurrentOffsetBackground = clBtnShadow
        Colors.OffsetBackground = clBtnFace
        Colors.CurrentOffset = clBtnHighlight
        Colors.Grid = clBtnFace
        Colors.NonFocusCursorFrame = clAqua
        Colors.ActiveFieldBackground = clWindow
        FocusFrame = True
        DrawGridLines = False
        GraySelectionIfNotFocused = True
        Version = 'september 30, 2007; '#169' markus stephany, vcl[at]mirkes[dot]de'
        OnChange = FHexEditorChange
        BytesPerBlock = 8
        SeparateBlocksInCharField = False
      end
      object PBlobSpacer: TPanel_Ext
        Left = 2
        Top = 25
        Width = 463
        Height = 2
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 5
      end
    end
    object PWorkbench: TPanel_Ext
      Left = 0
      Top = 360
      Width = 467
      Height = 50
      HelpContext = 1125
      Align = alTop
      BevelInner = bvRaised
      BevelOuter = bvLowered
      Color = clWindow
      Constraints.MinHeight = 20
      ParentBackground = False
      TabOrder = 7
      Visible = False
    end
    object SBResult: TStatusBar
      Left = 0
      Top = 245
      Width = 467
      Height = 19
      Panels = <
        item
          Width = 50
        end
        item
          Width = 50
        end
        item
          Width = 50
        end
        item
          Width = 50
        end
        item
          Width = 50
        end
        item
          Width = 50
        end>
      ParentFont = True
      UseSystemFont = False
      Visible = False
      ExplicitTop = 17
    end
  end
  object PHeader: TPanel_Ext
    Left = 0
    Top = 0
    Width = 591
    Height = 27
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 3
    object PSideBarHeader: TPanel_Ext
      Left = 0
      Top = 0
      Width = 120
      Height = 27
      BevelOuter = bvNone
      ParentBackground = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      OnMouseDown = PanelMouseDown
      OnMouseMove = PanelMouseMove
      OnMouseUp = PanelMouseUp
      OnPaint = PanelPaint
      object TBSideBar: TToolBar
        Left = 0
        Top = 0
        Width = 77
        Height = 30
        Align = alNone
        AutoSize = True
        BorderWidth = 2
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Wrapable = False
        object tbNavigator: TToolButton
          Left = 0
          Top = 0
          Caption = 'tbNavigator'
          Grouped = True
          Style = tbsCheck
        end
        object tbBookmarks: TToolButton
          Left = 23
          Top = 0
          Caption = 'tbBookmarks'
          Grouped = True
          Style = tbsCheck
        end
        object tbSQLHistory: TToolButton
          Left = 46
          Top = 0
          Caption = 'tbSQLHistory'
          Grouped = True
          Style = tbsCheck
        end
      end
    end
    object PToolBar: TPanel_Ext
      Left = 126
      Top = 0
      Width = 465
      Height = 27
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 1
      object ToolBar: TToolBar
        Left = 0
        Top = 0
        Width = 465
        Height = 27
        Align = alClient
        AutoSize = True
        BorderWidth = 2
        ButtonHeight = 19
        ButtonWidth = 93
        Color = clBtnFace
        List = True
        ParentColor = False
        PopupMenu = MToolBar
        ShowCaptions = True
        TabOrder = 0
        Wrapable = False
        object tbObjectBrowser: TToolButton
          Left = 0
          Top = 0
          AutoSize = True
          Caption = 'tbObjectBrowser'
          ImageIndex = 1
          PopupMenu = MToolBar
          Style = tbsCheck
        end
        object tbDataBrowser: TToolButton
          Left = 97
          Top = 0
          AutoSize = True
          Caption = 'tbDataBrowser'
          ImageIndex = 2
          PopupMenu = MToolBar
          Style = tbsCheck
        end
        object tbObjectIDE: TToolButton
          Left = 185
          Top = 0
          AutoSize = True
          Caption = 'tbObjectIDE'
          PopupMenu = MToolBar
          Style = tbsCheck
          Visible = False
        end
        object tbQueryBuilder: TToolButton
          Left = 255
          Top = 0
          AutoSize = True
          Caption = 'tbQueryBuilder'
          ImageIndex = 0
          PopupMenu = MToolBar
          Style = tbsCheck
        end
        object tbSQLEditor: TToolButton
          Left = 343
          Top = 0
          AutoSize = True
          Caption = 'tbSQLEditor'
          ImageIndex = 3
          PopupMenu = MToolBar
          Style = tbsCheck
        end
        object tbDiagram: TToolButton
          Left = 416
          Top = 0
          AutoSize = True
          Caption = 'tbDiagram'
          PopupMenu = MToolBar
          Style = tbsCheck
        end
      end
    end
  end
  object MList: TPopupMenu
    OnPopup = MListPopup
    Left = 144
    Top = 48
    object mlOpen: TMenuItem
      Caption = 'mlOpen'
      OnClick = mlOpenClick
    end
    object mlOpenInNewWinodow: TMenuItem
      Action = aPOpenInNewWindow
    end
    object mlOpenInNewTab: TMenuItem
      Action = aPOpenInNewTab
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object mlFImport: TMenuItem
      Caption = 'mlFImport'
      object mlFImportSQL: TMenuItem
        Caption = 'aFImportSQL'
      end
      object mlFImportText: TMenuItem
        Caption = 'aFImportText'
      end
      object mlFImportExcel: TMenuItem
        Caption = 'aFImportExcel'
      end
      object mlFImportAccess: TMenuItem
        Caption = 'aFImportAccess'
      end
      object mlFImportSQLite: TMenuItem
        Caption = 'aFImportSQLite'
      end
      object mlFImportODBC: TMenuItem
        Caption = 'aFImportODBC'
      end
      object mlFImportXML: TMenuItem
        Caption = 'aFImportXML'
      end
    end
    object mlFExport: TMenuItem
      Caption = 'mlFExport'
      object mlFExportSQL: TMenuItem
        Caption = 'aFExportSQL'
      end
      object mlFExportText: TMenuItem
        Caption = 'aFExportText'
      end
      object mlFExportExcel: TMenuItem
        Caption = 'aFExportExcel'
      end
      object mlFExportAccess: TMenuItem
        Caption = 'aFExportAccess'
      end
      object mlFExportSQLite: TMenuItem
        Caption = 'aFExportSQLite'
      end
      object mlFExportODBC: TMenuItem
        Caption = 'aFExportODBC'
      end
      object mlFExportXML: TMenuItem
        Caption = 'aFExportXML'
      end
      object mlFExportHTML: TMenuItem
        Caption = 'aFExportHTML'
      end
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object mlECopy: TMenuItem
      Caption = 'aECopy'
    end
    object mlEPaste: TMenuItem
      Caption = 'aEPaste'
    end
    object N17: TMenuItem
      Caption = '-'
    end
    object mlDCreate: TMenuItem
      Caption = 'mlDCreate'
      object mlDCreateDatabase: TMenuItem
        Caption = 'aDCreateDatabase'
      end
      object mlDCreateTable: TMenuItem
        Caption = 'aDCreateTable'
      end
      object mlDCreateView: TMenuItem
        Caption = 'aDCreateView'
      end
      object mlDCreateProcedure: TMenuItem
        Caption = 'aDCreateProcedure'
      end
      object mlDCreateFunction: TMenuItem
        Caption = 'aDCreateFunction'
      end
      object mlDCreateEvent: TMenuItem
        Caption = 'aDCreateEvent'
      end
      object mlDCreateIndex: TMenuItem
        Caption = 'aDCreateIndex'
      end
      object mlDCreateField: TMenuItem
        Caption = 'aDCreateField'
      end
      object mlDCreateForeignKey: TMenuItem
        Caption = 'aDCreateForeignKey'
      end
      object mlDCreateTrigger: TMenuItem
        Caption = 'aDCreateTrigger'
      end
      object mlDCreateHost: TMenuItem
        Caption = 'aDCreateHost'
      end
      object mlDCreateUser: TMenuItem
        Caption = 'aDCreateUser'
      end
    end
    object mlDDelete: TMenuItem
      Action = aDDelete
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object mlDEmpty: TMenuItem
      Caption = 'aDEmpty'
    end
    object N16: TMenuItem
      Caption = '-'
    end
    object mlERename: TMenuItem
      Caption = 'aERename'
      ShortCut = 113
    end
    object mlEProperties: TMenuItem
      Caption = 'mlEProperties'
      Visible = False
    end
  end
  object ActionList: TActionList
    Left = 8
    Top = 112
    object aTBOffset: TAction
      Category = 'ToolBar'
      Caption = 'aTBOffset'
      ShortCut = 24655
      OnExecute = aTBOffsetExecute
    end
    object aTBLimit: TAction
      Category = 'ToolBar'
      Caption = 'aTBLimit'
      ShortCut = 24652
      OnExecute = aTBLimitExecute
    end
    object aTBFilter: TAction
      Category = 'ToolBar'
      Caption = 'aTBFilter'
      ShortCut = 24646
      OnExecute = aTBFilterExecute
    end
    object aEClearAll: TAction
      Category = 'Edit'
      Caption = 'aEClearAll'
      ShortCut = 16471
      OnExecute = aEClearAllExecute
    end
    object aPCollapse: TAction
      Caption = 'aPCollapse'
      OnExecute = aPCollapseExecute
    end
    object aDCreate: TAction
      Category = 'Database'
      Caption = 'aDCreate'
      OnExecute = aDCreateExecute
    end
    object aDDelete: TAction
      Category = 'Database'
      Caption = 'aDDelete'
      OnExecute = aDDeleteExecute
    end
    object aDInsertRecord: TDataSetInsert
      Category = 'Database'
      Caption = 'aDInsertRecord'
      ImageIndex = 4
      DataSource = FGridDataSource
    end
    object aDDeleteRecord: TDataSetDelete
      Category = 'Database'
      Caption = 'aDDeleteRecord'
      ImageIndex = 5
      DataSource = FGridDataSource
    end
    object aDNext: TAction
      Category = 'Database'
      Enabled = False
      ImageIndex = 58
      OnExecute = aDNextExecute
    end
    object aPExpand: TAction
      Caption = 'aPExpand'
      OnExecute = aPExpandExecute
    end
    object aDPrev: TAction
      Category = 'Database'
      Enabled = False
      ImageIndex = 57
      OnExecute = aDPrevExecute
    end
    object DataSetDelete: TDataSetDelete
      Category = 'Database'
      ImageIndex = 5
    end
    object DataSetFirst: TDataSetFirst
      Category = 'Database'
      ImageIndex = 53
    end
    object DataSetLast: TDataSetLast
      Category = 'Database'
      ImageIndex = 54
    end
    object DataSetPost: TDataSetPost
      Category = 'Database'
      ImageIndex = 55
    end
    object DataSetCancel: TDataSetCancel
      Category = 'Database'
      ImageIndex = 56
    end
    object aPOpenInNewWindow: TAction
      Caption = 'aPOpenInNewWinodw'
      OnExecute = aPOpenInNewWindowExecute
    end
    object aPOpenInNewTab: TAction
      Caption = 'aPOpenInNewTab'
      ShortCut = 16462
      OnExecute = aPOpenInNewTabExecute
    end
    object aPObjectBrowserTable: TAction
      Caption = 'aPObjectBrowserTable'
      ShortCut = 16501
      OnExecute = aPObjectBrowserTableExecute
    end
    object aTBQuickSearch: TAction
      Category = 'ToolBar'
      Caption = 'aTBQuickSearch'
      ShortCut = 24659
      OnExecute = aTBQuickSearchExecute
    end
    object aPResult: TAction
      Caption = 'aPResult'
      OnExecute = aPResultExecute
    end
    object aVBlobText: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'aVBlobText'
      GroupIndex = 1
      OnExecute = aVBlobExecute
    end
    object aVBlobRTF: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'aVBlobRTF'
      GroupIndex = 1
      OnExecute = aVBlobExecute
    end
    object aVBlobHTML: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'aVBlobXML'
      GroupIndex = 1
      OnExecute = aVBlobExecute
    end
    object aVBlobImage: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'aVBlobImage'
      GroupIndex = 1
      OnExecute = aVBlobExecute
    end
    object aVBlobHexEditor: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'aVBlobHexEditor'
      GroupIndex = 1
      OnExecute = aVBlobExecute
    end
    object aHRun: TAction
      Caption = 'aHRun'
      OnExecute = aHRunExecute
    end
  end
  object MNavigator: TPopupMenu
    OnPopup = MNavigatorPopup
    Left = 8
    Top = 152
    object miNExpand: TMenuItem
      Action = aPExpand
      Visible = False
    end
    object miNCollapse: TMenuItem
      Action = aPCollapse
      Visible = False
    end
    object miNOpenInNewWinodow: TMenuItem
      Action = aPOpenInNewWindow
    end
    object miNOpenInNewTab: TMenuItem
      Action = aPOpenInNewTab
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object miNImport: TMenuItem
      Caption = 'miNImport'
      object miNImportSQL: TMenuItem
        Caption = 'aFImportSQL'
      end
      object miNImportText: TMenuItem
        Caption = 'aFImportText'
      end
      object miNImportExcel: TMenuItem
        Caption = 'aFImportExcel'
      end
      object miNImportAccess: TMenuItem
        Caption = 'aFImportAccess'
      end
      object miNImportSQLite: TMenuItem
        Caption = 'aFImportSQLite'
      end
      object miNImportODBC: TMenuItem
        Caption = 'aFImportODBC'
      end
      object miNImportXML: TMenuItem
        Caption = 'aFImportMXL'
      end
    end
    object miNExport: TMenuItem
      Caption = 'miNExport'
      object miNExportSQL: TMenuItem
        Caption = 'aFExportSQL'
      end
      object miNExportText: TMenuItem
        Caption = 'aFExportText'
      end
      object miNExportExcel: TMenuItem
        Caption = 'aFExportExcel'
      end
      object miNExportAccess: TMenuItem
        Caption = 'aFExportAccess'
      end
      object miNExportSQLite: TMenuItem
        Caption = 'aFExportSQLite'
      end
      object miNExportODBC: TMenuItem
        Caption = 'aFExportODBC'
      end
      object miNExportXML: TMenuItem
        Caption = 'aFExportXML'
      end
      object miNExportHTML: TMenuItem
        Caption = 'aFExportHTML'
      end
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object miNCopy: TMenuItem
      Caption = 'aECopy'
    end
    object miNPaste: TMenuItem
      Caption = 'aEPaste'
    end
    object N18: TMenuItem
      Caption = '-'
    end
    object miNCreate: TMenuItem
      Caption = 'miNCreate'
      object miNCreateDatabase: TMenuItem
        Caption = 'aDCreateDatabase'
      end
      object miNCreateTable: TMenuItem
        Caption = 'aDCreateTable'
      end
      object miNCreateView: TMenuItem
        Caption = 'aDCreateView'
      end
      object miNCreateProcedure: TMenuItem
        Caption = 'aDCreateProcedure'
      end
      object miNCreateFunction: TMenuItem
        Caption = 'aDCreateRoutine'
      end
      object miNCreateEvent: TMenuItem
        Caption = 'aDCreateEvent'
      end
      object miNCreateIndex: TMenuItem
        Caption = 'aDCreateIndex'
      end
      object miNCreateField: TMenuItem
        Caption = 'aDCreateField'
      end
      object miNCreateForeignKey: TMenuItem
        Caption = 'aDCreateForeignKey'
      end
      object miNCreateTrigger: TMenuItem
        Caption = 'aDCreateTrigger'
      end
      object miNCreateHost: TMenuItem
        Caption = 'aDCreateHost'
      end
      object miNCreateUser: TMenuItem
        Caption = 'aDCreateUser'
      end
    end
    object miNDelete: TMenuItem
      Action = aDDelete
    end
    object N15: TMenuItem
      Caption = '-'
    end
    object miNEmpty: TMenuItem
      Caption = 'aDEmpty'
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object miNRename: TMenuItem
      Caption = 'aERename'
      ShortCut = 113
    end
    object miNProperties: TMenuItem
      Caption = 'miNProperties'
      Visible = False
    end
  end
  object MLog: TPopupMenu
    Left = 72
    Top = 616
    object smECopy: TMenuItem
      Caption = 'aECopy'
    end
    object smEEmpty: TMenuItem
      Caption = 'smEEmpty'
      OnClick = smEEmptyClick
    end
    object smN1: TMenuItem
      Caption = '-'
    end
    object smECopyToFile: TMenuItem
      Caption = 'aECopyToFile'
    end
    object N13: TMenuItem
      Caption = '-'
    end
    object smESelectAll: TMenuItem
      Caption = 'aESelectAll'
    end
  end
  object FGridDataSource: TDataSource
    OnDataChange = FGridDataSourceDataChange
    Left = 136
    Top = 472
  end
  object MGrid: TPopupMenu
    OnPopup = MGridPopup
    Left = 184
    Top = 472
    object gmDInsertRecord: TMenuItem
      Caption = 'aDInsertRecord'
    end
    object gmDDeleteRecord: TMenuItem
      Caption = 'aDDeleteRecord'
    end
    object gmDEditRecord: TMenuItem
      Caption = 'aDEditRecord'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object gmECut: TMenuItem
      Caption = 'aECut'
      ImageIndex = 0
      ShortCut = 16472
    end
    object gmECopy: TMenuItem
      Caption = 'aECopy'
      ImageIndex = 1
      ShortCut = 16451
    end
    object gmEPaste: TMenuItem
      Caption = 'aEPaste'
      ImageIndex = 2
      ShortCut = 16470
    end
    object gmEDelete: TMenuItem
      Caption = 'aEDelete'
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object gmECopyToFile: TMenuItem
      Caption = 'aCopyToFile'
      OnClick = FGridCopyToExecute
    end
    object gmEPasteFromFile: TMenuItem
      Caption = 'aPasteFromFile'
      OnClick = aEPasteFromFileExecute
    end
    object N12: TMenuItem
      Caption = '-'
    end
    object gmFExport: TMenuItem
      Caption = 'aFExport'
      object gmFExportSQL: TMenuItem
        Caption = 'aFExportSQL'
      end
      object gmFExportText: TMenuItem
        Caption = 'aFExportText'
      end
      object gmFExportExcel: TMenuItem
        Caption = 'aFExportExcel'
      end
      object gmFExportXML: TMenuItem
        Caption = 'aFExportXML'
      end
      object gmFExportHTML: TMenuItem
        Caption = 'aFExportHTML'
      end
    end
    object N14: TMenuItem
      Caption = '-'
    end
    object gmFilter: TMenuItem
      Caption = 'gmFilter'
    end
  end
  object MSQLEditor: TPopupMenu
    OnPopup = MSQLEditorPopup
    Left = 152
    Top = 208
    object mpDRun: TMenuItem
      Caption = 'aDRun'
    end
    object mpDRunSelection: TMenuItem
      Caption = 'aDRunSelection'
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object mpECut: TMenuItem
      Caption = 'mpECut'
    end
    object mpECopy: TMenuItem
      Caption = 'mpECopy'
    end
    object mpEPaste: TMenuItem
      Caption = 'mpEPaste'
    end
    object mpEDelete: TMenuItem
      Caption = 'mpEDelete'
    end
    object N11: TMenuItem
      Caption = '-'
    end
    object mpECopyToFile: TMenuItem
      Caption = 'mpECopyToFile'
    end
    object mpEPasteFromFile: TMenuItem
      Caption = 'mpEPasteFromFile'
    end
    object N29: TMenuItem
      Caption = '-'
    end
    object mpESelectAll: TMenuItem
      Caption = 'mpESelectAll'
    end
  end
  object MText: TPopupMenu
    OnPopup = MTextPopup
    Left = 144
    Top = 544
    object tmECut: TMenuItem
      Caption = 'aECut'
    end
    object tmECopy: TMenuItem
      Caption = 'aECopy'
    end
    object tmEPaste: TMenuItem
      Caption = 'aEPaste'
    end
    object tmEDelete: TMenuItem
      Caption = 'aEDelete'
    end
    object N28: TMenuItem
      Caption = '-'
    end
    object tmESelectAll: TMenuItem
      Caption = 'aESelectAll'
    end
  end
  object FSQLEditorSearch: TSynEditSearch
    Left = 192
    Top = 208
  end
  object FSQLEditorPrint: TSynEditPrint
    Copies = 1
    Header.FrameTypes = []
    Header.DefaultFont.Charset = DEFAULT_CHARSET
    Header.DefaultFont.Color = clBlack
    Header.DefaultFont.Height = -13
    Header.DefaultFont.Name = 'Times New Roman'
    Header.DefaultFont.Style = []
    Footer.FrameTypes = []
    Footer.DefaultFont.Charset = ANSI_CHARSET
    Footer.DefaultFont.Color = clBlack
    Footer.DefaultFont.Height = -13
    Footer.DefaultFont.Name = 'Times New Roman'
    Footer.DefaultFont.Style = []
    Margins.Left = 25.000000000000000000
    Margins.Right = 15.000000000000000000
    Margins.Top = 30.000000000000000000
    Margins.Bottom = 35.000000000000000000
    Margins.Header = 15.000000000000000000
    Margins.Footer = 20.000000000000000000
    Margins.LeftHFTextIndent = 2.000000000000000000
    Margins.RightHFTextIndent = 2.000000000000000000
    Margins.HFInternalMargin = 0.500000000000000000
    Margins.MirrorMargins = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Colors = True
    LineNumbers = True
    TabWidth = 8
    Color = clWhite
    Left = 225
    Top = 208
  end
  object FSQLEditorCompletion: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    EndOfTokenChr = '()[]. '
    TriggerChars = '._abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    OnClose = FSQLEditorCompletionClose
    OnExecute = FSQLEditorCompletionExecute
    OnPaintItem = FSQLEditorCompletionPaintItem
    OnShow = FSQLEditorCompletionShow
    ShortCut = 16416
    Editor = FSQLEditor
    Left = 265
    Top = 208
  end
  object MGridHeader: TPopupMenu
    OnPopup = MGridHeaderPopup
    Left = 217
    Top = 472
  end
  object MBookmarks: TPopupMenu
    OnPopup = MBookmarksPopup
    Left = 8
    Top = 182
    object mbBOpen: TMenuItem
      Caption = 'miBOpen'
      Default = True
      OnClick = mbBOpenClick
    end
    object mbBOpenInNewWindow: TMenuItem
      Action = aPOpenInNewWindow
    end
    object mbBOpenInNewTab: TMenuItem
      Action = aPOpenInNewTab
    end
    object N33: TMenuItem
      Caption = '-'
    end
    object mbBAdd: TMenuItem
      Caption = 'aBAdd'
    end
    object mbBDelete: TMenuItem
      Caption = 'aBDelete'
    end
    object mbBEdit: TMenuItem
      Caption = 'aBEdit'
    end
  end
  object MSideBar: TPopupMenu
    Left = 8
    Top = 73
    object miSNavigator: TMenuItem
      Caption = 'miSNavigator'
    end
    object miSBookmarks: TMenuItem
      Caption = 'miSBookmarks'
    end
    object miSSQLHistory: TMenuItem
      Caption = 'miSSQLHistory'
    end
  end
  object PrintDialog: TPrintDialog_Ext
    FromPage = 1
    MinPage = 1
    MaxPage = 3
    ToPage = 2
    Left = 369
    Top = 208
  end
  object MSQLHistory: TPopupMenu
    OnPopup = MSQLHistoryPopup
    Left = 8
    Top = 244
    object miHStatementIntoSQLEditor: TMenuItem
      Caption = 'miHStatementIntoSQLEditor'
      OnClick = miHStatementIntoSQLEditorClick
    end
    object N30: TMenuItem
      Caption = '-'
    end
    object miHExpand: TMenuItem
      Action = aPExpand
    end
    object miHCollapse: TMenuItem
      Action = aPCollapse
    end
    object N27: TMenuItem
      Caption = '-'
    end
    object miHOpen: TMenuItem
      Caption = 'miHOpen'
      OnClick = miHOpenClick
    end
    object miHSaveAs: TMenuItem
      Caption = 'miHSaveAs'
      OnClick = miHSaveAsClick
    end
    object N23: TMenuItem
      Caption = '-'
    end
    object miHRun: TMenuItem
      Action = aHRun
    end
    object N26: TMenuItem
      Caption = '-'
    end
    object miHCopy: TMenuItem
      Caption = 'aECopy'
    end
    object N24: TMenuItem
      Caption = '-'
    end
    object miHProperties: TMenuItem
      Caption = 'miHProperties'
      OnClick = miHPropertiesClick
    end
  end
  object SyntaxProvider: TacMYSQLSyntaxProvider
    Left = 176
    Top = 120
  end
  object SQLBuilder: TacSQLBuilderPlainText
    OnSQLUpdated = SQLBuilderSQLUpdated
    QueryBuilder = FBuilder
    KeywordFormat = akfUpperCase
    QuoteAllIdentifiers = True
    MainQueryFormat.NewLineAfterPartKeywords = True
    MainQueryFormat.WhereFormat.NewLineAfter = nlAllLogical
    ExpressionsSubQueryFormat.MainPartsFromNewLine = False
    ExpressionsSubQueryFormat.FromClauseFormat.NewLineAfterDatasource = False
    Left = 240
    Top = 120
  end
  object MetadataProvider: TacEventMetadataProvider
    OnGetSQLFieldNames = MetadataProviderGetSQLFieldNames
    Left = 208
    Top = 120
  end
  object PObjectIDEGridDataSource: TDataSource
    Left = 152
    Top = 360
  end
  object MWorkbench: TPopupMenu
    OnPopup = MWorkbenchPopup
    Left = 152
    Top = 320
    object mwPOpenInNewWinodw: TMenuItem
      Action = aPOpenInNewWindow
    end
    object mwPOpenInNewTab: TMenuItem
      Action = aPOpenInNewTab
    end
    object N36: TMenuItem
      Caption = '-'
    end
    object mwFImport: TMenuItem
      Caption = 'mwFImport'
      object mwFImportSQL: TMenuItem
        Caption = 'aFImportSQL'
      end
      object mwFImportText: TMenuItem
        Caption = 'aFImportText'
      end
      object mwFImportExcel: TMenuItem
        Caption = 'aFImportExcel'
      end
      object mwFImportAccess: TMenuItem
        Caption = 'aFImportAccess'
      end
      object mwFImportSQLite: TMenuItem
        Caption = 'aFImportSQLite'
      end
      object mwFImportODBC: TMenuItem
        Caption = 'aFImportODBC'
      end
      object mwFImportXML: TMenuItem
        Caption = 'aFImportXML'
      end
    end
    object mwFExport: TMenuItem
      Caption = 'mwFExport'
      object mwFExportSQL: TMenuItem
        Caption = 'aFExportSQL'
      end
      object mwFExportText: TMenuItem
        Caption = 'aFExportText'
      end
      object mwFExportExcel: TMenuItem
        Caption = 'aFExportExcel'
      end
      object mwFExportAccess: TMenuItem
        Caption = 'aFExportAccess'
      end
      object mwFExportSQLite: TMenuItem
        Caption = 'aFExportSQLite'
      end
      object mwFExportODBC: TMenuItem
        Caption = 'aFExportODBC'
      end
      object mwFExportXML: TMenuItem
        Caption = 'aFExportXML'
      end
      object mwFExportHTML: TMenuItem
        Caption = 'aFExportHTML'
      end
      object mwFExportBitmap: TMenuItem
        Caption = 'aFExportBitmap'
      end
    end
    object N32: TMenuItem
      Caption = '-'
    end
    object mwAddTable: TMenuItem
      Caption = 'mwAddTable'
    end
    object mwECopy: TMenuItem
      Caption = 'mwECopy'
    end
    object mwEPaste: TMenuItem
      Caption = 'mwEPaste'
      OnClick = mwEPasteClick
    end
    object mwEDelete: TMenuItem
      Caption = 'aEDelete'
    end
    object N34: TMenuItem
      Caption = '-'
    end
    object mwDCreate: TMenuItem
      Caption = 'mwDCreate'
      object mwDCreateTable: TMenuItem
        Caption = 'aDCreateTable'
      end
      object mwDCreateField: TMenuItem
        Caption = 'aDCreateField'
      end
      object mwDCreateForeignKey: TMenuItem
        Caption = 'aDCreateForeignKey'
      end
      object mwCreateSection: TMenuItem
        Caption = 'mwCreateSection'
        Enabled = False
        OnClick = mwCreateSectionClick
      end
      object mwCreateLine: TMenuItem
        Caption = 'mwCreateLine'
        Enabled = False
        OnClick = mwCreateLineExecute
      end
    end
    object mwDDelete: TMenuItem
      Action = aDDelete
    end
    object N31: TMenuItem
      Caption = '-'
    end
    object mwDEmpty: TMenuItem
      Caption = 'aDEmpty'
    end
    object N35: TMenuItem
      Caption = '-'
    end
    object mwDProperties: TMenuItem
      Caption = 'mwDProperties'
      Default = True
      OnClick = aDPropertiesExecute
    end
  end
  object MToolBar: TPopupMenu
    OnPopup = MToolBarPopup
    Left = 160
    Top = 8
    object mtDataBrowser: TMenuItem
      Caption = 'mtDataBrowser'
      OnClick = ToolBarTabsClick
    end
    object mtObjectIDE: TMenuItem
      Caption = 'mtObjectIDE'
      OnClick = ToolBarTabsClick
    end
    object mtQueryBuilder: TMenuItem
      Caption = 'mtQueryBuilder'
      OnClick = ToolBarTabsClick
    end
    object mtSQLEditor: TMenuItem
      Caption = 'mtSQLEditor'
      OnClick = ToolBarTabsClick
    end
    object mtDiagram: TMenuItem
      Caption = 'mtDiagram'
      OnClick = ToolBarTabsClick
    end
  end
  object SaveDialog: TSaveDialog_Ext
    Options = [ofOverwritePrompt, ofHideReadOnly, ofExtensionDifferent, ofCreatePrompt, ofNoReadOnlyReturn, ofNoNetworkButton, ofEnableSizing]
    EncodingIndex = -1
    EncodingLabel = '&Encoding:'
    Left = 64
    Top = 112
  end
  object OpenDialog: TOpenDialog_Ext
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofNoNetworkButton, ofEnableSizing]
    EncodingIndex = -1
    EncodingLabel = '&Encoding:'
    Left = 64
    Top = 72
  end
  object MHexEditor: TPopupMenu
    Left = 184
    Top = 544
    object hmECut: TMenuItem
      Caption = 'aECut'
    end
    object hmECopy: TMenuItem
      Caption = 'aECopy'
    end
    object hmEPaste: TMenuItem
      Caption = 'aEPaste'
    end
    object hmEDelete: TMenuItem
      Caption = 'aEDelete'
    end
    object TMenuItem
      Caption = '-'
    end
    object hmESelectAll: TMenuItem
      Caption = 'aESelectAll'
    end
  end
end
