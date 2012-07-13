unit fFClient;

interface {********************************************************************}

uses
  Forms, Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Dialogs, ActnList, ComCtrls, ExtCtrls, Menus, StdCtrls, DB, DBGrids, Grids,
  DBCtrls, DBActns, StdActns, ImgList, XMLIntf,
  ShDocVw, CommCtrl, PNGImage, GIFImg, Jpeg, ToolWin,
  MPHexEditor, MPHexEditorEx,
  SynEdit, SynEditHighlighter, SynMemo, SynEditMiscClasses, SynEditSearch,
  SynCompletionProposal, SynEditPrint,
  acSQL92SynProvider, acQBBase, acAST, acQBEventMetaProvider, acMYSQLSynProvider, acSQLBuilderPlainText,
  ShellControls, JAMControls, ShellLink,
  ComCtrls_Ext, StdCtrls_Ext, Dialogs_Ext, Forms_Ext, ExtCtrls_Ext,
  MySQLDB, MySQLDBGrid,
  fDExport, fDImport, fClient, fAccount, fBase, fPreferences, fTools,
  fCWorkbench;

const
  CM_ACTIVATE_DBGRID = WM_USER + 500;
  CM_ACTIVATEFTEXT = WM_USER + 501;
  CM_POSTSCROLL = WM_USER + 502;
  CM_POST_BUILDER_QUERY_CHANGE = WM_USER + 503;
  CM_POST_MONITOR = WM_USER + 504;
  CM_WANTED_SYNCHRONIZE = WM_USER + 505;

const
  sbMessage = 0;
  sbItem = 1;
  sbSummarize = 2;
  sbConnected = 3;

type
  TSynMemoBeforeDrag = record SelStart: Integer; SelLength: Integer; end;
  TListViewSortRec = record Kind: TADesktop.TListViewKind; Index: Integer; Order: Integer; end;
  TListViewSortData = array [lkServer .. lkVariables] of TListViewSortRec;

type
  TFClient = class (TFrame)
    ActionList: TActionList;
    aDCreate: TAction;
    aDDelete: TAction;
    aDDeleteRecord: TDataSetDelete;
    aDInsertRecord: TDataSetInsert;
    aDNext: TAction;
    aDPrev: TAction;
    aEClearAll: TAction;
    aPCollapse: TAction;
    aPExpand: TAction;
    aPObjectBrowserTable: TAction;
    aPOpenInNewTab: TAction;
    aPOpenInNewWindow: TAction;
    aPResult: TAction;
    aTBFilter: TAction;
    aTBLimit: TAction;
    aTBOffset: TAction;
    aTBQuickSearch: TAction;
    aVBlobHexEditor: TAction;
    aVBlobHTML: TAction;
    aVBlobImage: TAction;
    aVBlobRTF: TAction;
    aVBlobText: TAction;
    BDELETE: TButton;
    BINSERT: TButton;
    BREPLACE: TButton;
    BUPDATE: TButton;
    DataSetCancel: TDataSetCancel;
    DataSetDelete: TDataSetDelete;
    DataSetFirst: TDataSetFirst;
    DataSetLast: TDataSetLast;
    DataSetPost: TDataSetPost;
    FBlobSearch: TEdit;
    FBookmarks: TListView;
    FBuilder: TacQueryBuilder;
    FBuilderSynMemo: TSynMemo;
    FFilter: TComboBox_Ext;
    FFilterEnabled: TToolButton;
    FGridDataSource: TDataSource;
    FHexEditor: TMPHexEditorEx;
    FImage: TImage;
    FLimit: TEdit;
    FLimitEnabled: TToolButton;
    FServerListView: TListView_Ext;
    FLog: TRichEdit;
    FNavigator: TTreeView_Ext;
    FObjectIDEGrid: TMySQLDBGrid;
    FOffset: TEdit;
    FQuickSearch: TEdit;
    FQuickSearchEnabled: TToolButton;
    FRTF: TRichEdit;
    FSQLEditorSynMemo: TSynMemo;
    FSQLEditorCompletion: TSynCompletionProposal;
    FSQLEditorPrint: TSynEditPrint;
    FSQLEditorSearch: TSynEditSearch;
    FSQLHistory: TTreeView_Ext;
    FText: TMemo_Ext;
    FUDLimit: TUpDown;
    FUDOffset: TUpDown;
    gmDDeleteRecord: TMenuItem;
    gmDEditRecord: TMenuItem;
    gmDInsertRecord: TMenuItem;
    gmECopy: TMenuItem;
    gmECopyToFile: TMenuItem;
    gmECut: TMenuItem;
    gmEDelete: TMenuItem;
    gmEPaste: TMenuItem;
    gmEPasteFromFile: TMenuItem;
    gmFExport: TMenuItem;
    gmFExportExcel: TMenuItem;
    gmFExportHTML: TMenuItem;
    gmFExportSQL: TMenuItem;
    gmFExportText: TMenuItem;
    gmFExportXML: TMenuItem;
    gmFilter: TMenuItem;
    mbBAdd: TMenuItem;
    mbBDelete: TMenuItem;
    mbBEdit: TMenuItem;
    mbBOpen: TMenuItem;
    mbBOpenInNewTab: TMenuItem;
    mbBOpenInNewWindow: TMenuItem;
    MBookmarks: TPopupMenu;
    MetadataProvider: TacEventMetadataProvider;
    MFiles: TPopupMenu;
    mfOpen: TMenuItem;
    mfOpenInNewWindow: TMenuItem;
    mfOpenInNewTab: TMenuItem;
    mfFilter: TMenuItem;
    mfFilterAccess: TMenuItem;
    mfFilterClear: TMenuItem;
    mfFilterExcel: TMenuItem;
    mfFilterHTML: TMenuItem;
    mfFilterSQL: TMenuItem;
    mfFilterSQLite: TMenuItem;
    mfFilterText: TMenuItem;
    mfFilterXML: TMenuItem;
    mfDelete: TMenuItem;
    mfRename: TMenuItem;
    mfProperties: TMenuItem;
    MGrid: TPopupMenu;
    MGridHeader: TPopupMenu;
    miHCollapse: TMenuItem;
    miHCopy: TMenuItem;
    miHExpand: TMenuItem;
    miHOpen: TMenuItem;
    miHProperties: TMenuItem;
    miHRun: TMenuItem;
    miHSaveAs: TMenuItem;
    miHStatementIntoSQLEditor: TMenuItem;
    miNCollapse: TMenuItem;
    miNCopy: TMenuItem;
    miNCreate: TMenuItem;
    miNCreateDatabase: TMenuItem;
    miNCreateEvent: TMenuItem;
    miNCreateField: TMenuItem;
    miNCreateForeignKey: TMenuItem;
    miNCreateFunction: TMenuItem;
    miNCreateHost: TMenuItem;
    miNCreateIndex: TMenuItem;
    miNCreateProcedure: TMenuItem;
    miNCreateTable: TMenuItem;
    miNCreateTrigger: TMenuItem;
    miNCreateUser: TMenuItem;
    miNCreateView: TMenuItem;
    miNDelete: TMenuItem;
    miNEmpty: TMenuItem;
    miNExpand: TMenuItem;
    miNExport: TMenuItem;
    miNExportAccess: TMenuItem;
    miNExportExcel: TMenuItem;
    miNExportHTML: TMenuItem;
    miNExportODBC: TMenuItem;
    miNExportSQL: TMenuItem;
    miNExportSQLite: TMenuItem;
    miNExportText: TMenuItem;
    miNExportXML: TMenuItem;
    miNImport: TMenuItem;
    miNImportAccess: TMenuItem;
    miNImportExcel: TMenuItem;
    miNImportODBC: TMenuItem;
    miNImportSQL: TMenuItem;
    miNImportSQLite: TMenuItem;
    miNImportText: TMenuItem;
    miNImportXML: TMenuItem;
    miNOpenInNewTab: TMenuItem;
    miNOpenInNewWinodow: TMenuItem;
    miNPaste: TMenuItem;
    miNProperties: TMenuItem;
    miNRename: TMenuItem;
    miSBookmarks: TMenuItem;
    miSExplorer: TMenuItem;
    miSNavigator: TMenuItem;
    miSSQLHistory: TMenuItem;
    mlDCreate: TMenuItem;
    mlDCreateDatabase: TMenuItem;
    mlDCreateEvent: TMenuItem;
    mlDCreateField: TMenuItem;
    mlDCreateForeignKey: TMenuItem;
    mlDCreateFunction: TMenuItem;
    mlDCreateHost: TMenuItem;
    mlDCreateIndex: TMenuItem;
    mlDCreateProcedure: TMenuItem;
    mlDCreateTable: TMenuItem;
    mlDCreateTrigger: TMenuItem;
    mlDCreateUser: TMenuItem;
    mlDCreateView: TMenuItem;
    mlDDelete: TMenuItem;
    mlDEmpty: TMenuItem;
    mlECopy: TMenuItem;
    mlEPaste: TMenuItem;
    mlEProperties: TMenuItem;
    mlERename: TMenuItem;
    mlFExport: TMenuItem;
    mlFExportAccess: TMenuItem;
    mlFExportExcel: TMenuItem;
    mlFExportHTML: TMenuItem;
    mlFExportODBC: TMenuItem;
    mlFExportSQL: TMenuItem;
    mlFExportSQLite: TMenuItem;
    mlFExportText: TMenuItem;
    mlFExportXML: TMenuItem;
    mlFImport: TMenuItem;
    mlFImportAccess: TMenuItem;
    mlFImportExcel: TMenuItem;
    mlFImportODBC: TMenuItem;
    mlFImportSQL: TMenuItem;
    mlFImportSQLite: TMenuItem;
    mlFImportText: TMenuItem;
    mlFImportXML: TMenuItem;
    MList: TPopupMenu;
    MLog: TPopupMenu;
    mlOpen: TMenuItem;
    mlOpenInNewTab: TMenuItem;
    mlOpenInNewWinodow: TMenuItem;
    MNavigator: TPopupMenu;
    mpDRun: TMenuItem;
    mpDRunSelection: TMenuItem;
    mpECopy: TMenuItem;
    mpECopyToFile: TMenuItem;
    mpECut: TMenuItem;
    mpEDelete: TMenuItem;
    mpEPaste: TMenuItem;
    mpEPasteFromFile: TMenuItem;
    mpESelectAll: TMenuItem;
    MSideBar: TPopupMenu;
    MSQLEditor: TPopupMenu;
    MSQLHistory: TPopupMenu;
    mtBrowser: TMenuItem;
    mtDiagram: TMenuItem;
    mtObjects: TMenuItem;
    MText: TPopupMenu;
    mtIDE: TMenuItem;
    MToolBar: TPopupMenu;
    mtBuilder: TMenuItem;
    mtEditor: TMenuItem;
    mwAddTable: TMenuItem;
    mwCreateLink: TMenuItem;
    mwCreateSection: TMenuItem;
    mwDCreate: TMenuItem;
    mwDCreateField: TMenuItem;
    mwDCreateForeignKey: TMenuItem;
    mwDCreateTable: TMenuItem;
    mwDDelete: TMenuItem;
    mwDEmpty: TMenuItem;
    mwDProperties: TMenuItem;
    mwECopy: TMenuItem;
    mwEDelete: TMenuItem;
    mwEPaste: TMenuItem;
    mwFExport: TMenuItem;
    mwFExportAccess: TMenuItem;
    mwFExportBitmap: TMenuItem;
    mwFExportExcel: TMenuItem;
    mwFExportHTML: TMenuItem;
    mwFExportODBC: TMenuItem;
    mwFExportSQL: TMenuItem;
    mwFExportSQLite: TMenuItem;
    mwFExportText: TMenuItem;
    mwFExportXML: TMenuItem;
    mwFImport: TMenuItem;
    mwFImportAccess: TMenuItem;
    mwFImportExcel: TMenuItem;
    mwFImportODBC: TMenuItem;
    mwFImportSQL: TMenuItem;
    mwFImportSQLite: TMenuItem;
    mwFImportText: TMenuItem;
    mwFImportXML: TMenuItem;
    MWorkbench: TPopupMenu;
    mwPOpenInNewTab: TMenuItem;
    mwPOpenInNewWinodw: TMenuItem;
    N01: TMenuItem;
    N02: TMenuItem;
    N03: TMenuItem;
    N04: TMenuItem;
    N05: TMenuItem;
    N06: TMenuItem;
    N07: TMenuItem;
    N08: TMenuItem;
    N09: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    N20: TMenuItem;
    N21: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    N26: TMenuItem;
    N27: TMenuItem;
    N28: TMenuItem;
    N29: TMenuItem;
    N30: TMenuItem;
    N31: TMenuItem;
    N32: TMenuItem;
    N33: TMenuItem;
    N34: TMenuItem;
    OpenDialog: TOpenDialog_Ext;
    PBlob: TPanel_Ext;
    PBlobSpacer: TPanel_Ext;
    PBookmarks: TPanel_Ext;
    PBuilder: TPanel_Ext;
    PBuilderQuery: TPanel_Ext;
    PContent: TPanel_Ext;
    PDataBrowser: TPanel_Ext;
    PDataBrowserSpacer: TPanel_Ext;
    PDBGrid: TPanel_Ext;
    PExplorer: TPanel_Ext;
    PFiles: TPanel_Ext;
    PFolders: TPanel_Ext;
    PHeader: TPanel_Ext;
    PListView: TPanel_Ext;
    PLog: TPanel_Ext;
    PLogHeader: TPanel_Ext;
    PNavigator: TPanel_Ext;
    PObjectIDE: TPanel_Ext;
    PObjectIDEGridDataSource: TDataSource;
    PObjectIDESpacer: TPanel_Ext;
    PObjectIDETrigger: TPanel_Ext;
    PResult: TPanel_Ext;
    PResultHeader: TPanel_Ext;
    PrintDialog: TPrintDialog_Ext;
    PSideBar: TPanel_Ext;
    PSideBarHeader: TPanel_Ext;
    PSynMemo: TPanel_Ext;
    PSQLHistory: TPanel_Ext;
    PToolBar: TPanel_Ext;
    PToolBarBlob: TPanel_Ext;
    PWorkbench: TPanel_Ext;
    SaveDialog: TSaveDialog_Ext;
    SBlob: TSplitter_Ext;
    SBResult: TStatusBar;
    SBuilderQuery: TSplitter_Ext;
    SLog: TSplitter_Ext;
    smECopy: TMenuItem;
    smECopyToFile: TMenuItem;
    smEEmpty: TMenuItem;
    smESelectAll: TMenuItem;
    SExplorer: TSplitter_Ext;
    SQLBuilder: TacSQLBuilderPlainText;
    SResult: TSplitter_Ext;
    SSideBar: TSplitter_Ext;
    SyntaxProvider: TacMYSQLSyntaxProvider;
    tbBlobHexEditor: TToolButton;
    tbBlobHTML: TToolButton;
    tbBlobImage: TToolButton;
    tbBlobRTF: TToolButton;
    tbBlobSpacer: TPanel_Ext;
    tbBlobText: TToolButton;
    tbBookmarks: TToolButton;
    tbBrowser: TToolButton;
    tbDiagram: TToolButton;
    tbExplorer: TToolButton;
    tbNavigator: TToolButton;
    tbObjects: TToolButton;
    tbIDE: TToolButton;
    tbBuilder: TToolButton;
    TBSideBar: TToolBar;
    tbEditor: TToolButton;
    tbSQLHistory: TToolButton;
    tmECopy: TMenuItem;
    tmECut: TMenuItem;
    tmEDelete: TMenuItem;
    tmEPaste: TMenuItem;
    tmESelectAll: TMenuItem;
    ToolBar: TToolBar;
    ToolBarBlob: TToolBar;
    TBFilterEnabled: TToolBar;
    TBLimitEnabled: TToolBar;
    TBQuickSearchEnabled: TToolBar;
    procedure aBAddExecute(Sender: TObject);
    procedure aBDeleteExecute(Sender: TObject);
    procedure aBEditExecute(Sender: TObject);
    procedure aDCreateDatabaseExecute(Sender: TObject);
    procedure aDCreateEventExecute(Sender: TObject);
    procedure aDCreateExecute(Sender: TObject);
    procedure aDCreateFieldExecute(Sender: TObject);
    procedure aDCreateForeignKeyExecute(Sender: TObject);
    procedure aDCreateHostExecute(Sender: TObject);
    procedure aDCreateIndexExecute(Sender: TObject);
    procedure aDCreateRoutineExecute(Sender: TObject);
    procedure aDCreateTableExecute(Sender: TObject);
    procedure aDCreateTriggerExecute(Sender: TObject);
    procedure aDCreateUserExecute(Sender: TObject);
    procedure aDCreateViewExecute(Sender: TObject);
    procedure aDDeleteExecute(Sender: TObject);
    procedure aDDeleteHostExecute(Sender: TObject);
    procedure aDDeleteRecordExecute(Sender: TObject);
    procedure aDDeleteUserExecute(Sender: TObject);
    procedure aDInsertRecordExecute(Sender: TObject);
    procedure aDNextExecute(Sender: TObject);
    procedure aDPrevExecute(Sender: TObject);
    procedure aDPropertiesExecute(Sender: TObject);
    procedure aEClearAllExecute(Sender: TObject);
    procedure aEPasteFromFileExecute(Sender: TObject);
    procedure aFExportAccessExecute(Sender: TObject);
    procedure aFExportBitmapExecute(Sender: TObject);
    procedure aFExportExcelExecute(Sender: TObject);
    procedure aFExportHTMLExecute(Sender: TObject);
    procedure aFExportODBCExecute(Sender: TObject);
    procedure aFExportSQLExecute(Sender: TObject);
    procedure aFExportSQLiteExecute(Sender: TObject);
    procedure aFExportTextExecute(Sender: TObject);
    procedure aFExportXMLExecute(Sender: TObject);
    procedure aFImportAccessExecute(Sender: TObject);
    procedure aFImportExcelExecute(Sender: TObject);
    procedure aFImportODBCExecute(Sender: TObject);
    procedure aFImportSQLExecute(Sender: TObject);
    procedure aFImportSQLiteExecute(Sender: TObject);
    procedure aFImportTextExecute(Sender: TObject);
    procedure aFImportXMLExecute(Sender: TObject);
    procedure aHRunClick(Sender: TObject);
    procedure aHRunExecute(Sender: TObject);
    procedure aPCollapseExecute(Sender: TObject);
    procedure aPExpandExecute(Sender: TObject);
    procedure aPObjectBrowserTableExecute(Sender: TObject);
    procedure aPOpenInNewTabExecute(Sender: TObject);
    procedure aPOpenInNewWindowExecute(Sender: TObject);
    procedure aPResultExecute(Sender: TObject);
    procedure aSSearchFindNotFound(Sender: TObject);
    procedure aTBFilterExecute(Sender: TObject);
    procedure aTBLimitExecute(Sender: TObject);
    procedure aTBOffsetExecute(Sender: TObject);
    procedure aTBQuickSearchExecute(Sender: TObject);
    procedure aVBlobExecute(Sender: TObject);
    procedure aVDetailsExecute(Sender: TObject);
    procedure aVSortAscExecute(Sender: TObject);
    procedure aVSortDescExecute(Sender: TObject);
    procedure BObjectIDEClick(Sender: TObject);
    procedure DataSetAfterCancel(DataSet: TDataSet);
    procedure DataSetAfterClose(DataSet: TDataSet);
    procedure DataSetAfterOpen(DataSet: TDataSet);
    procedure DataSetAfterPost(DataSet: TDataSet);
    procedure DataSetAfterScroll(DataSet: TDataSet);
    procedure DataSetBeforeCancel(DataSet: TDataSet);
    procedure DataSetBeforePost(DataSet: TDataSet);
    procedure DBGridColEnter(Sender: TObject);
    procedure DBGridColExit(Sender: TObject);
    procedure DBGridColumnMoved(Sender: TObject; FromIndex: Integer;
      ToIndex: Integer);
    procedure DBGridCopyToExecute(Sender: TObject);
    procedure DBGridDataSourceDataChange(Sender: TObject; Field: TField);
    procedure DBGridDblClick(Sender: TObject);
    procedure DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGridEditExecute(Sender: TObject);
    procedure DBGridEmptyExecute(Sender: TObject);
    procedure DBGridEnter(Sender: TObject);
    procedure DBGridExit(Sender: TObject);
    procedure DBGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DBGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DBGridTitleClick(Column: TColumn);
    procedure FBlobResize(Sender: TObject);
    procedure FBlobSearchChange(Sender: TObject);
    procedure FBlobSearchKeyPress(Sender: TObject; var Key: Char);
    procedure FBookmarksChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FBookmarksDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FBookmarksDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FBookmarksEnter(Sender: TObject);
    procedure FBookmarksExit(Sender: TObject);
    procedure FBuilderDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure FBuilderDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FBuilderEditorChange(Sender: TObject);
    procedure FBuilderEditorEnter(Sender: TObject);
    procedure FBuilderEditorExit(Sender: TObject);
    procedure FBuilderEditorStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure FBuilderEnter(Sender: TObject);
    procedure FBuilderExit(Sender: TObject);
    procedure FBuilderResize(Sender: TObject);
    procedure FBuilderValidatePopupMenu(Sender: TacQueryBuilder;
      AControlOwner: TacQueryBuilderControlOwner; AForControl: TControl;
      APopupMenu: TPopupMenu);
    procedure PDataBrowserResize(Sender: TObject);
    procedure FFilesEnter(Sender: TObject);
    procedure FFilterChange(Sender: TObject);
    procedure FFilterDropDown(Sender: TObject);
    procedure FFilterEnabledClick(Sender: TObject);
    procedure FFilterEnter(Sender: TObject);
    procedure FFilterKeyPress(Sender: TObject; var Key: Char);
    procedure FFoldersChange(Sender: TObject; Node: TTreeNode);
    procedure FHexEditorChange(Sender: TObject);
    procedure FHexEditorEnter(Sender: TObject);
    procedure FHexEditorKeyPress(Sender: TObject; var Key: Char);
    procedure FLimitChange(Sender: TObject);
    procedure FLimitEnabledClick(Sender: TObject);
    procedure FLogEnter(Sender: TObject);
    procedure FLogExit(Sender: TObject);
    procedure FLogSelectionChange(Sender: TObject);
    procedure FNavigatorAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure FNavigatorChange(Sender: TObject; Node: TTreeNode);
    procedure FNavigatorChange2(Sender: TObject; Node: TTreeNode);
    procedure FNavigatorChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure FNavigatorDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure FNavigatorDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FNavigatorEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure FNavigatorEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure FNavigatorEnter(Sender: TObject);
    procedure FNavigatorExit(Sender: TObject);
    procedure FNavigatorExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FNavigatorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FNavigatorKeyPress(Sender: TObject; var Key: Char);
    procedure FNavigatorSetMenuItems(Sender: TObject; const Node: TTreeNode);
    procedure FOffsetChange(Sender: TObject);
    procedure FOffsetKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure FQuickSearchChange(Sender: TObject);
    procedure FQuickSearchEnabledClick(Sender: TObject);
    procedure FQuickSearchKeyPress(Sender: TObject; var Key: Char);
    procedure FRTFChange(Sender: TObject);
    procedure FRTFEnter(Sender: TObject);
    procedure FRTFExit(Sender: TObject);
    procedure FSQLEditorCompletionClose(Sender: TObject);
    procedure FSQLEditorCompletionExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: string; var x, y: Integer;
      var CanExecute: Boolean);
    procedure FSQLEditorCompletionPaintItem(Sender: TObject;
      Index: Integer; TargetCanvas: TCanvas; ItemRect: TRect;
      var CustomDraw: Boolean);
    procedure FSQLEditorCompletionShow(Sender: TObject);
    procedure FSQLEditorCompletionTimerTimer(Sender: TObject);
    procedure FSQLHistoryChange(Sender: TObject; Node: TTreeNode);
    procedure FSQLHistoryChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure FSQLHistoryDblClick(Sender: TObject);
    procedure FSQLHistoryEnter(Sender: TObject);
    procedure FSQLHistoryExit(Sender: TObject);
    procedure FSQLHistoryHint(Sender: TObject; const Node: TTreeNode;
      var Hint: string);
    procedure FSQLHistoryKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FSQLHistoryKeyPress(Sender: TObject; var Key: Char);
    procedure FTextChange(Sender: TObject);
    procedure FTextEnter(Sender: TObject);
    procedure FTextExit(Sender: TObject);
    procedure FTextKeyPress(Sender: TObject; var Key: Char);
    procedure ListViewAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure ListViewAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewCompare(Sender: TObject; Item1: TListItem;
      Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListViewEdited(Sender: TObject; Item: TListItem;
      var S: string);
    procedure ListViewEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure ListViewEnter(Sender: TObject);
    procedure ListViewExit(Sender: TObject);
    procedure ListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure mbBOpenClick(Sender: TObject);
    procedure MBookmarksPopup(Sender: TObject);
    procedure MetadataProviderGetSQLFieldNames(Sender: TacBaseMetadataProvider;
      const ASQL: WideString; AFields: TacFieldsList);
    procedure mfOpenClick(Sender: TObject);
    procedure mfFilterClearClick(Sender: TObject);
    procedure mfFilterSQLClick(Sender: TObject);
    procedure mfFilterTextClick(Sender: TObject);
    procedure mfFilterHTMLClick(Sender: TObject);
    procedure mfFilterXMLClick(Sender: TObject);
    procedure mfFilterAccessClick(Sender: TObject);
    procedure mfFilterExcelClick(Sender: TObject);
    procedure mfFilterSQLiteClick(Sender: TObject);
    procedure mfDeleteClick(Sender: TObject);
    procedure mfRenameClick(Sender: TObject);
    procedure mfPropertiesClick(Sender: TObject);
    procedure MFilesPopup(Sender: TObject);
    procedure MGridHeaderPopup(Sender: TObject);
    procedure MGridPopup(Sender: TObject);
    procedure miHOpenClick(Sender: TObject);
    procedure miHPropertiesClick(Sender: TObject);
    procedure miHSaveAsClick(Sender: TObject);
    procedure miHStatementIntoSQLEditorClick(Sender: TObject);
    procedure MListPopup(Sender: TObject);
    procedure mlOpenClick(Sender: TObject);
    procedure MNavigatorPopup(Sender: TObject);
    procedure MSQLEditorPopup(Sender: TObject);
    procedure MSQLHistoryPopup(Sender: TObject);
    procedure MTextPopup(Sender: TObject);
    procedure MToolBarPopup(Sender: TObject);
    procedure mwCreateLinkExecute(Sender: TObject);
    procedure mwCreateSectionClick(Sender: TObject);
    procedure mwDCreateForeignKeyClick(Sender: TObject);
    procedure mwDCreateTableClick(Sender: TObject);
    procedure mwEPasteClick(Sender: TObject);
    procedure mwERemoveClick(Sender: TObject);
    procedure MWorkbenchPopup(Sender: TObject);
    procedure PanelMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelPaint(Sender: TObject);
    procedure PanelResize(Sender: TObject);
    procedure PContentResize(Sender: TObject);
    procedure PGridResize(Sender: TObject);
    procedure PLogResize(Sender: TObject);
    procedure PObjectIDEResize(Sender: TObject);
    procedure PSideBarResize(Sender: TObject);
    procedure SearchNotFound(Sender: TObject; FindText: string);
    procedure SLogCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure SLogMoved(Sender: TObject);
    procedure smEEmptyClick(Sender: TObject);
    procedure SplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure FBuilderSQLUpdated(Sender: TObject);
    procedure SResultMoved(Sender: TObject);
    procedure SSideBarCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure SSideBarMoved(Sender: TObject);
    procedure SynMemoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SynMemoDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynMemoEnter(Sender: TObject);
    procedure SynMemoExit(Sender: TObject);
    procedure SynMemoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure ToolBarBlobResize(Sender: TObject);
    procedure ToolBarTabsClick(Sender: TObject);
    procedure ToolButtonStyleClick(Sender: TObject);
    procedure TreeViewCollapsed(Sender: TObject; Node: TTreeNode);
    procedure TreeViewCollapsing(Sender: TObject;
      Node: TTreeNode; var AllowCollapse: Boolean);
    procedure TreeViewEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure TreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolBarResize(Sender: TObject);
  type
    TNewLineFormat = (nlWindows, nlUnix, nlMacintosh);
    TTabState = set of (tsLoading, tsActive);
    TView = (vObjects, vBrowser, vIDE, vBuilder, vEditor, vDiagram);
    TToolBarData = record
      Address: string;
      Addresses: TStringList;
      AddressMRU: TMRUList;
      Caption: string;
      CurrentAddress: Integer;
      tbPropertiesAction: TBasicAction;
      View: TView;
    end;

    TSQLEditor = class(TObject)
    type
      TResult = record
        DataSet: TMySQLDataSet;
        DataSource: TDataSource;
        DBGrid: TMySQLDBGrid;
      end;
    private
      FClient: TFClient;
      Results: TList;
      TCResult: TTabControl;
      function GetActiveDBGrid(): TMySQLDBGrid;
      procedure TCResultChange(Sender: TObject);
    public
      Filename: TFileName;
      FileCodePage: Cardinal;
      procedure CloseResult();
      constructor Create(const AFClient: TFClient);
      destructor Destroy(); override;
      function ResultEvent(const Connection: TMySQLConnection; const Data: Boolean): Boolean;
      property ActiveDBGrid: TMySQLDBGrid read GetActiveDBGrid;
    end;

    TCObjectDesktop = class(TCObject.TDesktop)
    private
      FFClient: TFClient;
    protected
      property FClient: TFClient read FFClient;
    public
      constructor Create(const AFClient: TFClient; const ACObject: TCObject);
    end;

    TDatabaseDesktop = class(TCObjectDesktop)
    private
      DataSet: TMySQLDataSet;
      DataSource: TDataSource;
      FDBGrid: TMySQLDBGrid;
      PDBGrid: TPanel_Ext;
      FXML: IXMLNode;
      function GetDatabase(): TCDatabase; inline;
      function GetXML(): IXMLNode;
    protected
      FWorkbench: TWWorkbench;
    public
      ListView: TListView;
      function BuilderResultEvent(const Connection: TMySQLConnection; const Data: Boolean): Boolean;
      procedure CloseBuilderResult();
      procedure CloseQuery(Sender: TObject; var CanClose: Boolean);
      constructor Create(const AFClient: TFClient; const ADatabase: TCDatabase);
      function CreateListView(): TListView; virtual;
      function CreateWorkbench(): TWWorkbench; virtual;
      destructor Destroy(); override;
      procedure SaveToXML(); override;
      property DBGrid: TMySQLDBGrid read FDBGrid;
      property Database: TCDatabase read GetDatabase;
      property Workbench: TWWorkbench read FWorkbench;
      property XML: IXMLNode read GetXML;
    end;

    TTableDesktop = class(TCObjectDesktop)
    private
      DataSource: TDataSource;
      PDBGrid: TPanel_Ext;
      FXML: IXMLNode;
      function GetFilter(Index: Integer): string;
      function GetFilterCount(): Integer;
      function GetGridXML(FieldName: string): IXMLNode;
      function GetLimit(): Integer;
      function GetLimited(): Boolean;
      function GetTable(): TCTable; inline;
      function GetXML(): IXMLNode;
      procedure SetLimit(const Limit: Integer);
      procedure SetLimited(const ALimited: Boolean);
    protected
      procedure SaveToXML(); override;
      property GridXML[FieldName: string]: IXMLNode read GetGridXML;
    public
      DBGrid: TMySQLDBGrid;
      ListView: TListView;
      procedure AddFilter(const AFilter: string);
      constructor Create(const AFClient: TFClient; const ATable: TCTable);
      function CreateDBGrid(): TMySQLDBGrid; virtual;
      function CreateListView(): TListView; virtual;
      function DataSetOpenEvent(const Connection: TMySQLConnection; const Data: Boolean): Boolean;
      destructor Destroy(); override;
      property Filters[Index: Integer]: string read GetFilter;
      property FilterCount: Integer read GetFilterCount;
      property Limit: Integer read GetLimit write SetLimit;
      property Limited: Boolean read GetLimited write SetLimited;
      property Table: TCTable read GetTable;
      property XML: IXMLNode read GetXML;
    end;

    TViewDesktop = class(TTableDesktop)
    public
      SynMemo: TSynMemo;
      constructor Create(const AFClient: TFClient; const AView: TCView);
      function CreateSynMemo(): TSynMemo; virtual;
      destructor Destroy(); override;
    end;

    TRoutineDesktop = class(TCObjectDesktop)
    type
      TResult = record
        DataSet: TMySQLDataSet;
        DataSource: TDataSource;
        DBGrid: TMySQLDBGrid;
      end;
    private
      PDBGrid: TPanel_Ext;
      Results: TList;
      TCResult: TTabControl;
      function GetActiveDBGrid(): TMySQLDBGrid;
      procedure TCResultChange(Sender: TObject);
    public
      SynMemo: TSynMemo;
      procedure CloseIDEResult();
      constructor Create(const AFClient: TFClient; const ARoutine: TCRoutine);
      function CreateSynMemo(): TSynMemo; virtual;
      destructor Destroy(); override;
      function IDEResultEvent(const Connection: TMySQLConnection; const Data: Boolean): Boolean;
      property ActiveDBGrid: TMySQLDBGrid read GetActiveDBGrid;
    end;

    TEventDesktop = class(TCObjectDesktop)
    public
      SynMemo: TSynMemo;
      constructor Create(const AFClient: TFClient; const AEvent: TCEvent);
      function CreateSynMemo(): TSynMemo; virtual;
      destructor Destroy(); override;
    end;

    TTriggerDesktop = class(TCObjectDesktop)
    public
      SynMemo: TSynMemo;
      constructor Create(const AFClient: TFClient; const ATrigger: TCTrigger);
      function CreateSynMemo(): TSynMemo; virtual;
      destructor Destroy(); override;
    end;

    TWanted = class
    private
      FAction: TAction;
      FAddress: string;
      FClient: TFClient;
      FUpdate: TCClient.TUpdate;
      procedure Clear();
      function GetNothing(): Boolean;
      procedure SetAction(const AAction: TAction);
      procedure SetAddress(const AAddress: string);
      procedure SetUpdate(const AUpdate: TCClient.TUpdate);
    protected
      procedure Synchronize();
    public
      constructor Create(const AFClient: TFClient);
      procedure Execute();
      property Action: TAction read FAction write SetAction;
      property Address: string read FAddress write SetAddress;
      property Update: TCClient.TUpdate read FUpdate write SetUpdate;
      property Nothing: Boolean read GetNothing;
    end;

  private
    ActiveControlOnDeactivate: TWinControl;
    ActiveIDEInputDataSet: TDataSet;
    ActiveListView: TListView;
    ActiveSynMemo: TSynMemo;
    ActiveWorkbench: TWWorkbench;
    aDRunExecuteSelStart: Integer;
    CloseButton: TPicture;
    EditorField: TField;
    FAddress: string;
    FFiles: TJamShellList;
    FFolders: TJamShellTree;
    FHTML: TWebBrowser;
    FilterMRU: TMRUList;
    FNavigatorMenuNode: TTreeNode;
    FNavigatorNodeToExpand: TTreeNode;
    FrameState: TTabState;
    FSQLEditorCompletionTimerCounter: Integer;
    FSQLHistoryMenuNode: TTreeNode;
    GIFImage: TGIFImage;
    HostsListView: TListView;
    IgnoreFGridTitleClick: Boolean;
    JPEGImage: TJPEGImage;
    LastFNavigatorSelected: TTreeNode;
    LastObjectIDEAddress: string;
    LastSelectedDatabase: string;
    LastSelectedTable: string;
    LastTableView: TView;
    LeftMousePressed: Boolean;
    ListViewSortData: TListViewSortData;
    MouseDownNode: TTreeNode;
    MovingToAddress: Boolean;
    NewLineFormat: TNewLineFormat;
    NMListView: PNMListView;
    OldFListOrderIndex: Integer;
    PanelMouseDownPoint: TPoint;
    Param: string;
    PasteMode: Boolean;
    PNGImage: TPNGImage;
    PResultHeight: Integer;
    ProcessesListView: TListView;
    ShellLink: TJamShellLink;
    SQLEditor: TSQLEditor;
    StatiListView: TListView;
    SynMemoBeforeDrag: TSynMemoBeforeDrag;
    NavigatorElapse: Integer;
    UsersListView: TListView;
    VariablesListView: TListView;
    Wanted: TWanted;
    procedure aBookmarkExecute(Sender: TObject);
    function ViewToParam(const AView: TView): Variant;
    procedure aDAutoCommitExecute(Sender: TObject);
    procedure aDCancelExecute(Sender: TObject);
    procedure aDCommitExecute(Sender: TObject);
    procedure aDCommitRefresh(Sender: TObject);
    procedure AddressChanged(Sender: TObject);
    procedure AddressChanging(const Sender: TObject; const NewAddress: String; var AllowChange: Boolean);
    procedure aDPostObjectExecute(Sender: TObject);
    procedure aDRollbackExecute(Sender: TObject);
    procedure aDRunExecute(Sender: TObject);
    procedure aDRunSelectionExecute(Sender: TObject);
    procedure aECopyExecute(Sender: TObject);
    procedure aEPasteExecute(Sender: TObject);
    procedure aEPasteFromExecute(Sender: TObject);
    procedure aERedoExecute(Sender: TObject);
    procedure aERenameExecute(Sender: TObject);
    procedure aFExportExecute(const Sender: TObject; const ExportType: TExportType);
    procedure aFImportExecute(const Sender: TObject; const ImportType: TImportType);
    procedure aFOpenExecute(Sender: TObject);
    procedure aFPrintExecute(Sender: TObject);
    procedure aFSaveAsExecute(Sender: TObject);
    procedure aFSaveExecute(Sender: TObject);
    procedure AfterConnect(Sender: TObject);
    procedure AfterExecuteSQL(Sender: TObject);
    procedure aHManualExecute(Sender: TObject);
    procedure aHSQLExecute(Sender: TObject);
    procedure aViewExecute(Sender: TObject);
    procedure aVRefreshAllExecute(Sender: TObject);
    procedure aVRefreshExecute(Sender: TObject);
    procedure aVSideBarExecute(Sender: TObject);
    procedure aVSQLLogExecute(Sender: TObject);
    procedure BeforeConnect(Sender: TObject);
    procedure BeforeExecuteSQL(Sender: TObject);
    procedure BeginEditLabel(Sender: TObject);
    procedure ClientUpdate(const Event: TCClient.TEvent);
    function ColumnWidthKindFromImageIndex(const AImageIndex: Integer): TADesktop.TListViewKind;
    function CreateDesktop(const CObject: TCObject): TCObject.TDesktop;
    procedure CreateExplorer();
    function CreateDBGrid(const PDBGrid: TPanel_Ext; const DataSource: TDataSource): TMySQLDBGrid;
    function CreateListView(const Data: TCustomData): TListView;
    function CreatePDBGrid(): TPanel_Ext;
    function CreateSynMemo(): TSynMemo;
    function CreateTCResult(const PDBGrid: TPanel_Ext): TTabControl;
    function CreateWorkbench(const ADatabase: TCDatabase): TWWorkbench;
    procedure DBGridGotoExecute(Sender: TObject);
    procedure DBGridInitialize(const DBGrid: TMySQLDBGrid);
    function Desktop(const Database: TCDatabase): TDatabaseDesktop; overload; inline;
    function Desktop(const Event: TCEvent): TEventDesktop; overload; inline;
    function Desktop(const Routine: TCRoutine): TRoutineDesktop; overload; inline;
    function Desktop(const Table: TCTable): TTableDesktop; overload; inline;
    function Desktop(const Trigger: TCTrigger): TTriggerDesktop; overload; inline;
    function Desktop(const View: TCView): TViewDesktop; overload; inline;
    function Dragging(const Sender: TObject): Boolean;
    procedure EndEditLabel(Sender: TObject);
    function FBuilderActiveSelectList(): TacQueryBuilderSelectListControl;
    function FBuilderActiveWorkArea(): TacQueryBuilderWorkArea;
    procedure FBuilderAddTable(Sender: TObject);
    function FBuilderEditorPageControl(): TacPageControl;
    procedure FBuilderEditorPageControlChange(Sender: TObject);
    procedure FBuilderEditorPageControlCheckStyle();
    procedure FBuilderEditorTabSheetEnter(Sender: TObject);
    procedure FHexEditorShow(Sender: TObject);
    procedure FHTMLHide(Sender: TObject);
    procedure FHTMLShow(Sender: TObject);
    procedure FieldSetText(Sender: TField; const Text: string);
    procedure FImageShow(Sender: TObject);
    procedure FNavigatorEmptyExecute(Sender: TObject);
    procedure FNavigatorInitialize(Sender: TObject);
    function FNavigatorNodeByAddress(const Address: string): TTreeNode;
    procedure FNavigatorUpdate(const ClientEvent: TCClient.TEvent);
    procedure FormClientEvent(const Event: TCClient.TEvent);
    procedure FormAccountEvent(const ClassType: TClass);
    procedure FreeDBGrid(const DBGrid: TMySQLDBGrid);
    procedure FreeListView(const ListView: TListView);
    procedure FRTFShow(Sender: TObject);
    procedure FSQLHistoryRefresh(Sender: TObject);
    procedure FTextShow(Sender: TObject);
    function GetActiveDBGrid(): TMySQLDBGrid;
    function GetActiveIDEInputDataSet(): TDataSet;
    function GetActiveListView(): TListView;
    function GetActiveSynMemo(): TSynMemo;
    function GetActiveWorkbench(): TWWorkbench;
    function GetFocusedCItem(): TCItem;
    function GetFocusedDatabaseNames(): string;
    function GetFocusedTableName(): string;
    function GetPath(): TFileName; inline;
    function GetMenuDatabase(): TCDatabase;
    function GetSelectedDatabase(): string;
    function GetSelectedImageIndex(): Integer;
    function GetView(): TView;
    function GetWindow(): TForm_Ext;
    procedure gmFilterClearClick(Sender: TObject);
    procedure gmFilterIntoFilterClick(Sender: TObject);
    function ImageIndexByData(const Data: TObject): Integer;
    procedure ImportError(const Sender: TObject; const Error: TTools.TError; const Item: TTools.TItem; var Success: TDataAction);
    procedure ListViewEmpty(Sender: TObject);
    procedure ListViewInitialize(const ListView: TListView);
    procedure ListViewUpdate(const ClientEvent: TCClient.TEvent; const ListView: TListView; const Data: TCustomData = nil);
    procedure MGridHeaderMenuOrderClick(Sender: TObject);
    procedure MGridTitleMenuVisibleClick(Sender: TObject);
    function NavigatorNodeToAddress(const Node: TTreeNode): string;
    procedure OnConvertError(Sender: TObject; Text: string);
    procedure OpenInNewTabExecute(const DatabaseName, TableName: string; const OpenNewWindow: Boolean = False; const Filename: TFileName = '');
    procedure PasteExecute(const Node: TTreeNode; const Objects: string);
    procedure PContentChange(Sender: TObject);
    function PostObject(Sender: TObject): Boolean;
    procedure PropertiesServerExecute(Sender: TObject);
    procedure PSQLEditorUpdate();
    function RenameCItem(const CItem: TCItem; const NewName: string): Boolean;
    procedure SaveSQLFile(Sender: TObject);
    procedure SBResultRefresh(const DataSet: TMySQLDataSet);
    procedure SendQuery(Sender: TObject; const SQL: string);
    procedure SetView(const AView: TView);
    procedure SetAddress(const AAddress: string);
    procedure SetPath(const APath: TFileName);
    procedure SQLError(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
    procedure SynMemoApllyPreferences(const SynMemo: TSynMemo);
    procedure SynMemoPrintExecute(Sender: TObject);
    procedure TableOpen(Sender: TObject);
    procedure TSymMemoGotoExecute(Sender: TObject);
    function UpdateAfterAddressChanged(): Boolean; virtual;
    procedure WorkbenchAddTable(Sender: TObject);
    procedure WorkbenchChange(Sender: TObject; Control: TWControl);
    procedure WorkbenchCursorMove(Sender: TObject; X, Y: Integer);
    procedure WorkbenchDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure WorkbenchDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure WorkbenchEmptyExecute(Sender: TObject);
    procedure WorkbenchEnter(Sender: TObject);
    procedure WorkbenchExit(Sender: TObject);
    procedure WorkbenchPasteExecute(Sender: TObject);
    procedure WorkbenchPrintExecute(Sender: TObject);
    function WorkbenchValidateControl(Sender: TObject; Control: TWControl): Boolean;
    procedure CMActivateDBGrid(var Message: TMessage); message CM_ACTIVATE_DBGRID;
    procedure CMActivateFText(var Message: TMessage); message CM_ACTIVATEFTEXT;
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMCloseTabQuery(var Message: TMessage); message CM_CLOSE_TAB_QUERY;
    procedure CMExecute(var Message: TMessage); message CM_EXECUTE;
    procedure CMFrameActivate(var Message: TMessage); message CM_ACTIVATEFRAME;
    procedure CMFrameDeactivate(var Message: TMessage); message CM_DEACTIVATEFRAME;
    procedure CMPostBuilderQueryChange(var Message: TMessage); message CM_POST_BUILDER_QUERY_CHANGE;
    procedure CMPostMonitor(var Message: TMessage); message CM_POST_MONITOR;
    procedure CMPostScroll(var Message: TMessage); message CM_POSTSCROLL;
    procedure CMPostShow(var Message: TMessage); message CM_POSTSHOW;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CMWantedSynchronize(var Message: TWMTimer); message CM_WANTED_SYNCHRONIZE;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    property FocusedCItem: TCItem read GetFocusedCItem;
    property FocusedDatabaseNames: string read GetFocusedDatabaseNames;
    property FocusedTableNames: string read GetFocusedTableName;
    property MenuDatabase: TCDatabase read GetMenuDatabase;
    property SelectedImageIndex: Integer read GetSelectedImageIndex;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    ActiveDBGrid: TMySQLDBGrid;
    Client: TCClient;
    StatusBar: TStatusBar;
    ToolBarData: TToolBarData;
    constructor Create(const AOwner: TComponent; const AParent: TWinControl; const AClient: TCClient; const AParam: string); reintroduce;
    destructor Destroy(); override;
    function AddressToCaption(const AAddress: string): string;
    procedure aEFindExecute(Sender: TObject);
    procedure aEReplaceExecute(Sender: TObject);
    procedure aETransferExecute(Sender: TObject);
    procedure CrashRescue();
    procedure MoveToAddress(const ADiff: Integer);
    procedure OpenSQLFile(const AFilename: TFileName; const CodePage: Cardinal = 0; const Insert: Boolean = False);
    procedure StatusBarRefresh(const Immediately: Boolean = False);
    property Address: string read FAddress write SetAddress;
    property Path: TFileName read GetPath write SetPath;
    property SelectedDatabase: string read GetSelectedDatabase;
    property View: TView read GetView write SetView;
    property Window: TForm_Ext read GetWindow;
  end;

implementation {***************************************************************}

{$R *.dfm}

uses
  MMSystem, Math, DBConsts, Clipbrd, DBCommon, ShellAPI, Variants,
  XMLDoc, Themes, StrUtils, UxTheme, FileCtrl, SysConst, RichEdit,
  ShLwApi,
  SynHighlighterSQL,
  acQBLocalizer, acQBStrings,
  CommCtrl_Ext, StdActns_Ext,
  MySQLConsts, SQLUtils,
  fDField, fDKey, fDTable, fDVariable, fDDatabase, fDForeignKey,
  fDHost, fDUser, fDQuickFilter, fDGoto, fDSQLHelp, fDTransfer,
  fDSearch, fDServer, fDBookmark, fURI, fDView, fDRoutine,
  fDTrigger, fDStatement, fDEvent, fDColumns,
  fDPaste, fDSegment, fDConnecting;

const
  nlHost = 0;
  nlDatabase = 1;
  nlTable = 2;

const
  tiNavigator = 1;
  tiStatusBar = 2;
  tiCodeCompletion = 3;

const
  giDatabases = 1;
  giSystemTools = 2;
  giTables = 3;
  giRoutines = 4;
  giEvents = 5;
  giKeys = 6;
  giFields = 7;
  giForeignKeys = 8;
  giTriggers = 9;
  giHosts = 10;
  giProcesses = 11;
  giStati = 12;
  giUsers = 13;
  giVariables = 14;

const
  Filters: array[0 .. 12 - 1] of
    record Text: PChar; ValueType: Integer end = (
      (Text: '%s IS NULL'; ValueType: 0),
      (Text: '%s IS NOT NULL'; ValueType: 0),
      (Text: '%s = %s'; ValueType: 1),
      (Text: '%s <> %s'; ValueType: 1),
      (Text: '%s < %s'; ValueType: 1),
      (Text: '%s > %s'; ValueType: 1),
      (Text: '%s LIKE %s'; ValueType: 2),
      (Text: '%s = %s'; ValueType: 3),
      (Text: '%s <> %s'; ValueType: 3),
      (Text: '%s < %s'; ValueType: 3),
      (Text: '%s > %s'; ValueType: 3),
      (Text: '%s LIKE %s'; ValueType: 4)
    );

function IsRTF(const Value: string): Boolean;
var
  S: string;
begin
  S := Value;
  while (Copy(S, 2, 1) = '{') do Delete(S, 2, 1);
  Result := Copy(S, 1, 5) = '{\rtf';
end;

function IsHTML(const Value: string): Boolean;
begin
  Result := Copy(Trim(Value), 1, 2) = '<!';
end;

function FindChildByClassType(const Control: TWinControl; ClassType: TClass): TWinControl;
var
  I: Integer;
begin
  Result := nil;

  if (Assigned(Control)) then
    for I := 0 to Control.ControlCount - 1 do
      if (not Assigned(Result) and (Control.Controls[I] is TWinControl)) then
        if (Control.Controls[I].ClassType = ClassType) then
          Result := TWinControl(Control.Controls[I])
        else
          Result := FindChildByClassType(TWinControl(Control.Controls[I]), ClassType);
end;

function CopyName(const Name: string; const Items: TCItems): string;
var
  I: Integer;
begin
  Result := Name;
  I := 1;
  while (Items.IndexByName(Result) >= 0) do
  begin
    if (I = 1) then
      Result := Preferences.LoadStr(680, Name)
    else
      Result := Preferences.LoadStr(681, Name, IntToStr(I));
    Result := ReplaceStr(Result, ' ', '_');
    Inc(I);
  end;
end;

{ TFClient.TSQLEditorDesktop **************************************************}

procedure TFClient.TSQLEditor.CloseResult();
var
  I: Integer;
begin
  if (Assigned(Results)) then
  begin
    if (Assigned(TCResult)) then
      FreeAndNil(TCResult);

    for I := 0 to Results.Count - 1 do
    begin
      FClient.FreeDBGrid(TResult(Results[I]^).DBGrid);
      TResult(Results[I]^).DataSource.Free();
      FreeMem(Results[I]);
    end;
    Results.Clear();
  end;
end;

constructor TFClient.TSQLEditor.Create(const AFClient: TFClient);
begin
  inherited Create();

  FClient := AFClient;

  Filename := '';
  FileCodePage := CP_ACP;
  TCResult := nil;
  Results := nil;
end;

destructor TFClient.TSQLEditor.Destroy();
begin
  CloseResult();
  if (Assigned(Results)) then
    Results.Free();

  inherited;
end;

function TFClient.TSQLEditor.GetActiveDBGrid(): TMySQLDBGrid;
begin
  if (not Assigned(Results) or (Results.Count = 0)) then
    Result := nil
  else if (not Assigned(TCResult)) then
    Result := TResult(Results[0]^).DBGrid
  else
    Result := TResult(Results[TCResult.TabIndex]^).DBGrid;
end;

function TFClient.TSQLEditor.ResultEvent(const Connection: TMySQLConnection; const Data: Boolean): Boolean;
var
  EndingCommentLength: Integer;
  Item: ^TResult;
  Len: Integer;
  StartingCommentLength: Integer;
  URI: TUURI;
  XML: IXMLNode;
begin
  if (not Assigned(Results)) then
    Results := TList.Create();

  if ((Results.Count < 5) and Assigned(FClient.Client.Account.HistoryXML)) then
  begin
    XML := FClient.Client.Account.HistoryXML.AddChild('sql');
    if (not Data) then
      XML.Attributes['type'] := 'statement'
    else
      XML.Attributes['type'] := 'query';
    XML.AddChild('database').Text := Connection.DatabaseName;
    XML.AddChild('datetime').Text := FloatToStr(Connection.DateTime, FileFormatSettings);
    if (not Data and (Connection.RowsAffected >= 0)) then
      XML.AddChild('rows_affected').Text := IntToStr(Connection.RowsAffected);
    XML.AddChild('sql').Text := Connection.CommandText;
    if (Connection.Info <> '') then
      XML.AddChild('info').Text := Connection.Info;
    XML.AddChild('execution_time').Text := FloatToStr(Connection.ExecutionTime, FileFormatSettings);
    if (Connection.Connected and (Connection.InsertId > 0)) then
      XML.AddChild('insert_id').Text := IntToStr(Connection.InsertId);

    while (FClient.Client.Account.HistoryXML.ChildNodes.Count > 100) do
      FClient.Client.Account.HistoryXML.ChildNodes.Delete(0);
    FClient.FSQLHistoryRefresh(nil);
  end;

  if (Connection.ErrorCode > 0) then
  begin
    if ((Connection.CommandText <> '') and (Length(FClient.FSQLEditorSynMemo.Text) > Length(Connection.CommandText) + 5)) then
    begin
      Len := SQLStmtLength(Connection.CommandText);
      SQLTrimStmt(Connection.CommandText, 1, Len, StartingCommentLength, EndingCommentLength);
      FClient.FSQLEditorSynMemo.SelStart := FClient.aDRunExecuteSelStart + Connection.ExecutedSQLLength + StartingCommentLength;
      FClient.FSQLEditorSynMemo.SelLength := Len - StartingCommentLength - EndingCommentLength;
    end
  end
  else if (not Data) then
  begin
    if (FClient.Client.Databases.NameCmp(Connection.DatabaseName, FClient.SelectedDatabase) <> 0) then
    begin
      URI := TUURI.Create(FClient.Address);
      URI.Database := Connection.DatabaseName;
      FClient.Address := URI.Address;
      URI.Free();
    end;
  end
  else
  begin
    if (Results.Count = 1) then
    begin
      TCResult := FClient.CreateTCResult(FClient.PDBGrid);
      TCResult.Tabs.Add(Preferences.LoadStr(861, IntToStr(Results.Count)));
      TCResult.OnChange := TCResultChange;
    end;

    GetMem(Item, SizeOf(TResult));
    TResult(Item^).DataSet := TMySQLDataSet.Create(FClient.Owner);
    TResult(Item^).DataSet.Connection := Connection;
    TResult(Item^).DataSet.AfterOpen := FClient.DataSetAfterOpen;
    TResult(Item^).DataSource := TDataSource.Create(FClient.Owner);
    TResult(Item^).DataSource.Enabled := False;
    TResult(Item^).DataSource.DataSet := TResult(Item^).DataSet;
    TResult(Item^).DBGrid := FClient.CreateDBGrid(FClient.PDBGrid, TResult(Item^).DataSource);
    TResult(Item^).DBGrid.Tag := Results.Count;
    Results.Add(Item);

    if (Results.Count > 1) then
    begin
      TCResult.Tabs.Add(Preferences.LoadStr(861, IntToStr(Results.Count)));
      TCResult.TabIndex := Results.Count - 1;
      TCResultChange(nil);
    end;

    FClient.ActiveDBGrid := TResult(Item^).DBGrid;
    TResult(Item^).DataSet.Open();

    FClient.SBResultRefresh(TResult(Item^).DataSet);
  end;

  Result := False;
end;

procedure TFClient.TSQLEditor.TCResultChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FClient.PDBGrid.ControlCount - 1 do
    if (FClient.PDBGrid.Controls[I] is TMySQLDBGrid) then
      FClient.PDBGrid.Controls[I].Visible := TMySQLDBGrid(FClient.PDBGrid.Controls[I]).Tag = TCResult.TabIndex;
end;

{ TFClient.TCObjectDesktop ****************************************************}

constructor TFClient.TCObjectDesktop.Create(const AFClient: TFClient; const ACObject: TCObject);
begin
  FFClient := AFClient;

  inherited Create(ACObject);
end;

{ TFClient.TDatabaseDesktop ***************************************************}

function TFClient.TDatabaseDesktop.BuilderResultEvent(const Connection: TMySQLConnection; const Data: Boolean): Boolean;
begin
  if ((Connection.ErrorCode = 0) and Data) then
  begin
    DataSet := TMySQLDataSet.Create(FClient.Owner);
    DataSet.Connection := Connection;
    DataSet.AfterOpen := FClient.DataSetAfterOpen;

    if (not Assigned(PDBGrid)) then
      PDBGrid := FClient.CreatePDBGrid();
    if (not Assigned(DataSource)) then
    begin
      DataSource := TDataSource.Create(FClient.Owner);
      DataSource.Enabled := False;
    end;
    DataSource.DataSet := DataSet;
    if (not Assigned(FDBGrid)) then
      FDBGrid := FClient.CreateDBGrid(PDBGrid, DataSource);

    FClient.ActiveDBGrid := FDBGrid;
    DataSet.Open();
  end;

  Result := False;
end;

procedure TFClient.TDatabaseDesktop.CloseBuilderResult();
begin
  if (Assigned(FDBGrid)) then
    FreeAndNil(FDBGrid);
  if (Assigned(DataSet)) then
    FreeAndNil(DataSet);
end;

procedure TFClient.TDatabaseDesktop.CloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (Assigned(Workbench) and Workbench.Modified) then
    if (Workbench.ObjectCount > 0) then
      try
        SysUtils.ForceDirectories(ExtractFilePath(FClient.Client.Account.DataPath + Database.Name));
        FWorkbench.SaveToFile(FClient.Client.Account.DataPath + Database.Name + PathDelim + 'Diagram.xml');
      except
        raise EInOutError.Create(SysErrorMessage(GetLastError()) + '  (' + FClient.Client.Account.DataPath + Database.Name + ')');
      end
    else if (FileExists(FClient.Client.Account.DataPath + Database.Name + PathDelim + 'Diagram.xml')) then
      DeleteFile(FClient.Client.Account.DataPath + Database.Name + PathDelim + 'Diagram.xml');
end;

constructor TFClient.TDatabaseDesktop.Create(const AFClient: TFClient; const ADatabase: TCDatabase);
begin
  inherited Create(AFClient, ADatabase);

  DataSet := nil;
  DataSource := nil;
  FDBGrid := nil;
  PDBGrid := nil;
  FWorkbench := nil;
  FXML := nil;
end;

function TFClient.TDatabaseDesktop.CreateListView(): TListView;
begin
  if (not Assigned(ListView)) then
  begin
    ListView := FClient.CreateListView(Database);
    if (Database.Valid) then
      Database.PushBuildEvents();
  end;

  Result := ListView;
end;

function TFClient.TDatabaseDesktop.CreateWorkbench(): TWWorkbench;
begin
  if (not Assigned(FWorkbench)) then
    FWorkbench := FClient.CreateWorkbench(Database);

  Result := FWorkbench;
end;

destructor TFClient.TDatabaseDesktop.Destroy();
begin
  CloseBuilderResult();
  if (Assigned(PDBGrid)) then
    PDBGrid.Free();
  if (Assigned(DataSource)) then
    DataSource.Free();
  if (Assigned(ListView)) then
    FClient.FreeListView(ListView);
  if (Assigned(FWorkbench)) then
    FWorkbench.Free();

  inherited;
end;

function TFClient.TDatabaseDesktop.GetDatabase(): TCDatabase;
begin
  Result := TCDatabase(CObject);
end;

function TFClient.TDatabaseDesktop.GetXML(): IXMLNode;
var
  I: Integer;
  Node: IXMLNode;
begin
  if (not Assigned(FXML) and Assigned(FClient.Client.Account.DesktopXML)) then
  begin
    Node := XMLNode(FClient.Client.Account.DesktopXML, 'browser/databases', True);

    FXML := nil;
    for I := 0 to Node.ChildNodes.Count - 1 do
      if ((Node.ChildNodes[I].NodeName = 'database') and (FClient.Client.Databases.NameCmp(Node.ChildNodes[I].Attributes['name'], Database.Name) = 0)) then
        FXML := Node.ChildNodes[I];

    if (not Assigned(FXML)) then
    begin
      FXML := Node.AddChild('database');
      try
        FXML.Attributes['name'] := Database.Name;
      except
        Node.ChildNodes.Delete(Node.ChildNodes.IndexOf(FXML));
        FXML := nil;
      end;
    end;
  end;

  Result := FXML;
end;

procedure TFClient.TDatabaseDesktop.SaveToXML();
var
  TablesXML: IXMLNode;
  I: Integer;
begin
  TablesXML := XMLNode(XML, 'tables');
  if (Assigned(TablesXML)) then
    for I := TablesXML.ChildNodes.Count - 1 downto 0 do
      if ((TablesXML.ChildNodes[I].NodeName = 'table') and not Assigned(Database.TableByName(TablesXML.ChildNodes[I].Attributes['name']))) then
        TablesXML.ChildNodes.Delete(I);
end;

{ TFClient.TTableDesktop ******************************************************}

procedure TFClient.TTableDesktop.AddFilter(const AFilter: string);
var
  FiltersXML: IXMLNode;
  I: Integer;
begin
  if (AFilter <> '') then
  begin
    FiltersXML := XMLNode(XML, 'filters', True);
    for I := FiltersXML.ChildNodes.Count - 1 downto 0 do
      if (FiltersXML.ChildNodes[I].Text = AFilter) then
        FiltersXML.ChildNodes.Delete(I);
    try
      FiltersXML.AddChild('filter').NodeValue := AFilter;
    except
      // Some characters are not storable inside XML - so just ignore them
    end;
  end;

  while (FiltersXML.ChildNodes.Count > 10) do
    FiltersXML.ChildNodes.Delete(0);
end;

constructor TFClient.TTableDesktop.Create(const AFClient: TFClient; const ATable: TCTable);
begin
  inherited Create(AFClient, ATable);

  PDBGrid := nil;
  FXML := nil;
end;

function TFClient.TTableDesktop.CreateDBGrid(): TMySQLDBGrid;
begin
  if (not Assigned(DBGrid)) then
  begin
    if (not Assigned(PDBGrid)) then
      PDBGrid := FClient.CreatePDBGrid();
    if (not Assigned(DataSource)) then
    begin
      DataSource := TDataSource.Create(FClient.Owner);
      DataSource.Enabled := False;
    end;
    DataSource.DataSet := Table.DataSet;
    DBGrid := FClient.CreateDBGrid(PDBGrid, DataSource);
  end;

  Result := DBGrid;
end;

function TFClient.TTableDesktop.CreateListView(): TListView;
begin
  if (not Assigned(ListView)) then
  begin
    ListView := FClient.CreateListView(Table);
    if (Table.Valid) then
      Table.Tables.PushBuildEvent(Table);
  end;

  Result := ListView;
end;

function TFClient.TTableDesktop.DataSetOpenEvent(const Connection: TMySQLConnection; const Data: Boolean): Boolean;
var
  Child: IXMLNode;
  FieldInfo: TFieldInfo;
  I: Integer;
  Width: Integer;
begin
  if (Connection.ErrorCode = 0) then
  begin
    Table.DataSet.AfterOpen := FClient.DataSetAfterOpen;
    Table.DataSet.Open();

    DBGrid.ReadOnly := Table is TCSystemView;
    for I := 0 to DBGrid.Columns.Count - 1 do
      if (GetFieldInfo(DBGrid.Columns[I].Field.Origin, FieldInfo)) then
      begin
        Child := XMLNode(GridXML[FieldInfo.OriginalFieldName], 'width');
        if (Assigned(Child) and TryStrToInt(Child.Text, Width) and (Width > 10)) then
        begin
          if ((Width > Preferences.GridMaxColumnWidth) and not (DBGrid.Columns[I].Field.DataType in [ftSmallint, ftInteger, ftLargeint, ftWord, ftFloat, ftDate, ftDateTime, ftTime, ftCurrency])) then
            Width := Preferences.GridMaxColumnWidth;
          DBGrid.Columns[I].Width := Width;
        end;
      end;

    if (Table.DataSet.FilterSQL <> '') then
      AddFilter(Table.DataSet.FilterSQL);
    if (((Table.DataSet.Limit > 0) <> Limited) or (Limit <> Table.DataSet.Limit)) then
    begin
      Limited := Table.DataSet.Limit > 0;
      Limit := Table.DataSet.Limit;
    end;

    FClient.FUDOffset.Position := Table.DataSet.Offset;
    FClient.FLimitEnabled.Down := Table.DataSet.Limit > 0;
    if (FClient.FLimitEnabled.Down) then
      FClient.FUDLimit.Position := Table.DataSet.Limit;

    FClient.FFilterEnabled.Down := Table.DataSet.FilterSQL <> '';
    FClient.FFilterEnabled.Enabled := FClient.FFilter.Text <> '';
    FClient.FilterMRU.Clear();
    if (FClient.FFilterEnabled.Down) then
      FClient.FFilter.Text := Table.DataSet.FilterSQL;
    for I := 0 to FilterCount - 1 do
      FClient.FilterMRU.Add(Filters[I]);
    FClient.gmFilter.Clear();

    FClient.FQuickSearchEnabled.Down := Table.DataSet.QuickSearch <> '';

    FClient.AddressChanged(nil);
  end;

  Result := False;
end;

destructor TFClient.TTableDesktop.Destroy();
begin
  if (Assigned(ListView)) then
    FClient.FreeListView(ListView);
  if (Assigned(DBGrid)) then
    FClient.FreeDBGrid(DBGrid);
  if (Assigned(DataSource)) then
    DataSource.Free();
  if (Assigned(PDBGrid)) then
    PDBGrid.Free();

  inherited;
end;

function TFClient.TTableDesktop.GetFilter(Index: Integer): string;
var
  FiltersXML: IXMLNode;
begin
  Result := '';

  if (Assigned(XML)) then
  begin
    FiltersXML := XMLNode(XML, 'filters', True);
    if (Assigned(FiltersXML) and (Index < FiltersXML.ChildNodes.Count)) then
      Result := XMLNode(XML, 'filters').ChildNodes[FiltersXML.ChildNodes.Count - Index - 1].Text;
  end;
end;

function TFClient.TTableDesktop.GetFilterCount(): Integer;
begin
  Result := 0;

  if (Assigned(XML)) then
    if (Assigned(XMLNode(XML, 'filters'))) then
      Result := XMLNode(XML, 'filters').ChildNodes.Count;
end;

function TFClient.TTableDesktop.GetGridXML(FieldName: string): IXMLNode;
var
  Node: IXMLNode;
  I: Integer;
begin
  Node := XMLNode(XML, 'grid', True);
  Result := nil;
  if (Assigned(Node)) then
  begin
    for I := 0 to Node.ChildNodes.Count - 1 do
      if ((Node.ChildNodes[I].NodeName = 'field') and (lstrcmpi(PChar(string(Node.ChildNodes[I].Attributes['name'])), PChar(FieldName)) = 0)) then
        Result := Node.ChildNodes[I];
    if (not Assigned(Result)) then
    begin
      Result := Node.AddChild('field');
      Result.Attributes['name'] := Table.Name;
    end;
  end;
end;

function TFClient.TTableDesktop.GetLimit(): Integer;
begin
  if ((Table is TCBaseTable) and (TCBaseTable(Table).AvgRowLength > 0)) then
  begin
    Result := DefaultLimitSize div TCBaseTable(Table).AvgRowLength;
    if (Result < 2 * DefaultLimit) then
      Result := DefaultLimit
    else
      Result := Result div DefaultLimit * DefaultLimit;
  end
  else if (Assigned(XML) and Assigned(XMLNode(XML, 'limit')) and
    TryStrToInt(XMLNode(XML, 'limit').Text, Result)) then
  else
    Result := DefaultLimit;

  if (Result < 1) then
    Result := DefaultLimit;
end;

function TFClient.TTableDesktop.GetLimited(): Boolean;
begin
  Result := True;
  if (Assigned(XML) and Assigned(XMLNode(XML, 'limit'))) then
    TryStrToBool(XMLNode(XML, 'limit').Attributes['used'], Result);
end;

function TFClient.TTableDesktop.GetXML(): IXMLNode;
var
  I: Integer;
  Node: IXMLNode;
begin
  if (not Assigned(FXML) and Assigned(FClient.Desktop(Table.Database).XML)) then
  begin
    Node := XMLNode(FClient.Desktop(Table.Database).XML, 'tables', True);

    for I := 0 to Node.ChildNodes.Count - 1 do
      if ((Node.ChildNodes[I].NodeName = 'table') and (lstrcmpi(PChar(string(Node.ChildNodes[I].Attributes['name'])), PChar(Table.Name)) = 0)) then
        FXML := Node.ChildNodes[I];

    if (not Assigned(FXML)) then
    begin
      FXML := Node.AddChild('table');
      FXML.Attributes['name'] := Table.Name;
    end;
  end;

  Result := FXML;
end;

function TFClient.TTableDesktop.GetTable(): TCTable;
begin
  Result := TCTable(CObject);
end;

procedure TFClient.TTableDesktop.SaveToXML();
var
  Child: IXMLNode;
  FieldInfo: TFieldInfo;
  FieldName: string;
  I: Integer;
  J: Integer;
  Node: IXMLNode;
begin
  Node := XMLNode(XML, 'grid', True);
  if (Assigned(Node)) then
  begin
    for I := Node.ChildNodes.Count - 1 downto 0 do
      if ((Node.ChildNodes[I].NodeName = 'field') and not Assigned(Table.FieldByName(Node.ChildNodes[I].Attributes['name']))) then
        Node.ChildNodes.Delete(I);

    if (Assigned(DBGrid) and DBGrid.DataSource.DataSet.Active) then
      for I := 0 to DBGrid.Columns.Count - 1 do
        if (Assigned(DBGrid.Columns[I].Field) and GetFieldInfo(DBGrid.Columns[I].Field.Origin, FieldInfo)) then
        begin
          FieldName := FieldInfo.OriginalFieldName;
          Child := nil;
          for J := 0 to Node.ChildNodes.Count - 1 do
            if ((Node.ChildNodes[J].NodeName = 'field') and (lstrcmpi(PChar(string(Node.ChildNodes[J].Attributes['name'])), PChar(FieldName)) = 0)) then
              Child := Node.ChildNodes[J];
          if (not Assigned(Child)) then
          begin
            Child := XMLNode(XML, 'grid', True).AddChild('field');
            Child.Attributes['name'] := FieldName;
          end;
          XMLNode(Child, 'width', True).Text := IntToStr(DBGrid.Columns[I].Width);
        end;
  end;
end;

procedure TFClient.TTableDesktop.SetLimit(const Limit: Integer);
begin
  if (Limit > 0) then
    XMLNode(XML, 'limit', True).Text := IntToStr(Limit);
end;

procedure TFClient.TTableDesktop.SetLimited(const ALimited: Boolean);
begin
  XMLNode(XML, 'limit', True).Attributes['used'] := BoolToStr(ALimited, True);
end;

{ TFClient.TViewDesktop *******************************************************}

constructor TFClient.TViewDesktop.Create(const AFClient: TFClient; const AView: TCView);
begin
  inherited Create(AFClient, AView);

  SynMemo := nil;
end;

function TFClient.TViewDesktop.CreateSynMemo(): TSynMemo;
begin
  if (not Assigned(SynMemo)) then
  begin
    SynMemo := FClient.CreateSynMemo();
    if (Table.Valid) then
      Table.PushBuildEvent();
  end;

  Result := SynMemo;
end;

destructor TFClient.TViewDesktop.Destroy();
begin
  if (Assigned(SynMemo)) then
    SynMemo.Free();

  inherited;
end;

{ TFClient.TRoutineDesktop *******************************************************}

procedure TFClient.TRoutineDesktop.CloseIDEResult();
var
  I: Integer;
begin
  if (Assigned(Results)) then
  begin
    if (Assigned(TCResult)) then
      FreeAndNil(TCResult);

    for I := 0 to Results.Count - 1 do
    begin
      FClient.FreeDBGrid(TResult(Results[I]^).DBGrid);
      TResult(Results[I]^).DataSource.Free();
      FreeMem(Results[I]);
    end;
    Results.Clear();
  end;
end;

constructor TFClient.TRoutineDesktop.Create(const AFClient: TFClient; const ARoutine: TCRoutine);
begin
  inherited Create(AFClient, ARoutine);

  Results := nil;
  SynMemo := nil;
end;

function TFClient.TRoutineDesktop.CreateSynMemo(): TSynMemo;
begin
  if (not Assigned(SynMemo)) then
    SynMemo := FClient.CreateSynMemo();

  Result := SynMemo;
end;

destructor TFClient.TRoutineDesktop.Destroy();
begin
  CloseIDEResult();
  if (Assigned(PDBGrid)) then
    PDBGrid.Free();
  if (Assigned(SynMemo)) then
    SynMemo.Free();
  if (Assigned(Results)) then
    Results.Free();

  inherited;
end;

function TFClient.TRoutineDesktop.GetActiveDBGrid(): TMySQLDBGrid;
begin
  if (not Assigned(Results) or (Results.Count = 0)) then
    Result := nil
  else if (not Assigned(TCResult)) then
    Result := TResult(Results[0]^).DBGrid
  else
    Result := TResult(Results[TCResult.TabIndex]^).DBGrid;
end;

function TFClient.TRoutineDesktop.IDEResultEvent(const Connection: TMySQLConnection; const Data: Boolean): Boolean;
var
  Item: ^TResult;
begin
  if ((Connection.ErrorCode = 0) and Data) then
  begin
    if (not Assigned(PDBGrid)) then
      PDBGrid := FClient.CreatePDBGrid();
    if (not Assigned(Results)) then
      Results := TList.Create();

    if (Results.Count = 1) then
    begin
      TCResult := FClient.CreateTCResult(PDBGrid);
      TCResult.Tabs.Add(Preferences.LoadStr(861, IntToStr(Results.Count)));
      TCResult.OnChange := TCResultChange;
    end;

    GetMem(Item, SizeOf(TResult));
    TResult(Item^).DataSet := TMySQLDataSet.Create(FClient.Owner);
    TResult(Item^).DataSet.Connection := Connection;
    TResult(Item^).DataSet.AfterOpen := FClient.DataSetAfterOpen;
    TResult(Item^).DataSource := TDataSource.Create(FClient.Owner);
    TResult(Item^).DataSource.Enabled := False;
    TResult(Item^).DataSource.DataSet := TResult(Item^).DataSet;
    TResult(Item^).DBGrid := FClient.CreateDBGrid(PDBGrid, TResult(Item^).DataSource);
    TResult(Item^).DBGrid.Tag := Results.Count;
    Results.Add(Item);

    if (Results.Count > 1) then
    begin
      TCResult.Tabs.Add(Preferences.LoadStr(861, IntToStr(Results.Count)));
      TCResult.TabIndex := Results.Count - 1;
      TCResultChange(nil);
    end;

    FClient.ActiveDBGrid := TResult(Item^).DBGrid;
    TResult(Item^).DataSet.Open();
  end;

  Result := False;
end;

procedure TFClient.TRoutineDesktop.TCResultChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to PDBGrid.ControlCount - 1 do
    if (PDBGrid.Controls[I] is TMySQLDBGrid) then
      PDBGrid.Controls[I].Visible := TMySQLDBGrid(PDBGrid.Controls[I]).Tag = TCResult.TabIndex;
end;

{ TFClient.TEventDesktop ******************************************************}

constructor TFClient.TEventDesktop.Create(const AFClient: TFClient; const AEvent: TCEvent);
begin
  SynMemo := nil;

  inherited Create(AFClient, AEvent);
end;

function TFClient.TEventDesktop.CreateSynMemo(): TSynMemo;
begin
  if (not Assigned(SynMemo)) then
    SynMemo := FClient.CreateSynMemo();

  Result := SynMemo;
end;

destructor TFClient.TEventDesktop.Destroy();
begin
  inherited;

  if (Assigned(SynMemo)) then
    SynMemo.Free();
end;

{ TFClient.TTriggerDesktop *******************************************************}

constructor TFClient.TTriggerDesktop.Create(const AFClient: TFClient; const ATrigger: TCTrigger);
begin
  SynMemo := nil;

  inherited Create(AFClient, ATrigger);
end;

function TFClient.TTriggerDesktop.CreateSynMemo(): TSynMemo;
begin
  if (not Assigned(SynMemo)) then
    SynMemo := FClient.CreateSynMemo();

  Result := SynMemo;
end;

destructor TFClient.TTriggerDesktop.Destroy();
begin
  inherited;

  if (Assigned(SynMemo)) then
    SynMemo.Free();
end;

{ TFClient.TWanted ************************************************************}

procedure TFClient.TWanted.Clear();
begin
  FAction := nil;
  FAddress := '';
  FUpdate := nil;
end;

constructor TFClient.TWanted.Create(const AFClient: TFClient);
begin
  FClient := AFClient;

  Clear();
end;

procedure TFClient.TWanted.Execute();
begin
  if (not FClient.Client.Asynchron) then
    FClient.Perform(CM_WANTED_SYNCHRONIZE, 0, 0)
  else
    PostMessage(FClient.Handle, CM_WANTED_SYNCHRONIZE, 0, 0);
end;

function TFClient.TWanted.GetNothing(): Boolean;
begin
  Result := not Assigned(Action) and (Address = '') and not Assigned(Update);
end;

procedure TFClient.TWanted.SetAction(const AAction: TAction);
begin
  if (AAction <> FAction) then
  begin
    Clear();
    FAction := AAction;
  end;
end;

procedure TFClient.TWanted.SetAddress(const AAddress: string);
begin
  if (AAddress <> FAddress) then
  begin
    Clear();
    FAddress := AAddress;
  end;
end;

procedure TFClient.TWanted.SetUpdate(const AUpdate: TCClient.TUpdate);
begin
  Clear();
  if (not FClient.Client.InUse) then
    AUpdate()
  else
    FUpdate := AUpdate;
end;

procedure TFClient.TWanted.Synchronize();
var
  TempAction: TAction;
  TempAddress: string;
  TempUpdate: TCClient.TUpdate;
begin
  if (Assigned(Action)) then
  begin
    TempAction := Action;
    Clear();
    TempAction.Execute();
  end
  else if (Address <> '') then
  begin
    TempAddress := Address;
    Clear();
    FClient.Address := TempAddress;
  end
  else if (Assigned(Update)) then
  begin
    TempUpdate := Update;
    Clear();
    TempUpdate();
  end;
end;

{ TFClient ********************************************************************}

procedure TFClient.aBAddExecute(Sender: TObject);
begin
  Wanted.Clear();

  DBookmark.Bookmarks := Client.Account.Desktop.Bookmarks;
  DBookmark.Bookmark := nil;
  DBookmark.NewCaption := AddressToCaption(Address);
  DBookmark.NewURI := Address;
  if (DBookmark.Execute()) then
    FBookmarks.Selected := FBookmarks.Items[FBookmarks.Items.Count - 1];
end;

procedure TFClient.aBDeleteExecute(Sender: TObject);
begin
  Wanted.Clear();

  Client.Account.Desktop.Bookmarks.DeleteBookmark(Client.Account.Desktop.Bookmarks.ByCaption(FBookmarks.Selected.Caption));
end;

procedure TFClient.aBEditExecute(Sender: TObject);
begin
  Wanted.Clear();

  DBookmark.Bookmarks := Client.Account.Desktop.Bookmarks;
  DBookmark.Bookmark := Client.Account.Desktop.Bookmarks.ByCaption(FBookmarks.Selected.Caption);
  DBookmark.Execute();
end;

procedure TFClient.aBookmarkExecute(Sender: TObject);
begin
  Wanted.Clear();

  Address := Client.Account.Desktop.Bookmarks.ByCaption(TMenuItem(Sender).Caption).URI;
end;

function TFClient.ViewToParam(const AView: TView): Variant;
begin
  case (AView) of
    vBrowser: Result := 'browser';
    vIDE: Result := 'ide';
    vBuilder: Result := 'builder';
    vEditor: Result := 'editor';
    vDiagram: Result := 'diagram';
    else Result := Null;
  end;
end;

procedure TFClient.aDAutoCommitExecute(Sender: TObject);
begin
  Wanted.Clear();

  Client.AutoCommit := not Client.AutoCommit;

  MainAction('aDAutoCommit').Checked := Client.AutoCommit;

  aDCommitRefresh(Sender);
end;

procedure TFClient.aDCancelExecute(Sender: TObject);
begin
  Wanted.Clear();

  Client.Terminate();

  MainAction('aDCancel').Enabled := Client.InUse();
end;

procedure TFClient.aDCommitExecute(Sender: TObject);
begin
  Wanted.Clear();

  Client.CommitTransaction();

  aDCommitRefresh(Sender);
end;

procedure TFClient.aDCommitRefresh(Sender: TObject);
begin
  MainAction('aDAutoCommit').Enabled := (Client.ServerVersion >= 40002) and (Client.Lib.LibraryType <> ltHTTP);
  MainAction('aDAutoCommit').Checked := Client.AutoCommit;
  MainAction('aDCommit').Enabled := not MainAction('aDAutoCommit').Checked and (Client.ServerVersion >= 40002) and (Client.Lib.LibraryType <> ltHTTP);
  MainAction('aDRollback').Enabled := not MainAction('aDAutoCommit').Checked and (Client.ServerVersion >= 40002) and (Client.Lib.LibraryType <> ltHTTP);
end;

procedure TFClient.aDCreateDatabaseExecute(Sender: TObject);
begin
  Wanted.Clear();

  DDatabase.Client := Client;
  DDatabase.Database := nil;
  if (DDatabase.Execute()) then
    Client.Update();
end;

procedure TFClient.aDCreateEventExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (FocusedCItem is TCDatabase) then
  begin
    DEvent.Database := TCDatabase(FocusedCItem);
    DEvent.Event := nil;
    if (DEvent.Execute()) then
      Client.Update();
  end;
end;

procedure TFClient.aDCreateExecute(Sender: TObject);
begin
  Wanted.Clear();

  if ((Window.ActiveControl = FNavigator) or (Window.ActiveControl = ActiveListView)) then
  begin
    if (MainAction('aDCreateDatabase').Enabled) then MainAction('aDCreateDatabase').Execute()
    else if (MainAction('aDCreateDatabase').Enabled) then MainAction('aDCreateDatabase').Execute()
    else if (MainAction('aDCreateTable').Enabled) then MainAction('aDCreateTable').Execute()
    else if (MainAction('aDCreateField').Enabled) then MainAction('aDCreateField').Execute()
    else if (MainAction('aDCreateHost').Enabled) then MainAction('aDCreateHost').Execute()
    else if (MainAction('aDCreateUser').Enabled) then MainAction('aDCreateUser').Execute();
  end
  else if (Window.ActiveControl = FSQLEditorSynMemo) then
    FSQLEditorSynMemo.InsertMode := not FSQLEditorSynMemo.InsertMode
  else if (Window.ActiveControl = ActiveDBGrid) and (not ActiveDBGrid.EditorMode) then
    MainAction('aDInsertRecord').Execute()
  else if (Window.ActiveControl = FText) then
    FText.InsertMode := not FText.InsertMode;
end;

procedure TFClient.aDCreateFieldExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (FocusedCItem is TCBaseTable) then
  begin
    DField.Table := TCBaseTable(FocusedCItem);
    DField.Database := DField.Table.Database;
    DField.Field := nil;
    if (DField.Execute()) then
      Client.Update();
  end;
end;

procedure TFClient.aDCreateForeignKeyExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (FocusedCItem is TCBaseTable) then
  begin
    DForeignKey.Table := TCBaseTable(FocusedCItem);
    DForeignKey.Database := DForeignKey.Table.Database;
    DForeignKey.ParentTable := nil;
    DForeignKey.ForeignKey := nil;
    if (DForeignKey.Execute()) then
      Client.Update();
  end;
end;

procedure TFClient.aDCreateHostExecute(Sender: TObject);
begin
  Wanted.Clear();

  DHost.Client := Client;
  DHost.Host := nil;
  if (DHost.Execute()) then
    Client.Update();
end;

procedure TFClient.aDCreateIndexExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (FocusedCItem is TCBaseTable) then
  begin
    DIndex.Table := TCBaseTable(FocusedCItem);
    DIndex.Database := DIndex.Table.Database;
    DIndex.Key := nil;
    if (DIndex.Execute()) then
      Client.Update();
  end;
end;

procedure TFClient.aDCreateRoutineExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (FocusedCItem is TCDatabase) then
  begin
    DRoutine.Database := TCDatabase(FocusedCItem);
    if (Sender = MainAction('aDCreateProcedure')) then
      DRoutine.RoutineType := rtProcedure
    else if (Sender = MainAction('aDCreateFunction')) then
      DRoutine.RoutineType := rtFunction
    else
      DRoutine.RoutineType := rtUnknown;
    DRoutine.Routine := nil;
    if (DRoutine.Execute()) then
      Client.Update();
  end;
end;

procedure TFClient.aDCreateTableExecute(Sender: TObject);
begin
  Wanted.Clear();

  if ((Window.ActiveControl = ActiveWorkbench) and Assigned(ActiveWorkbench) and (FNavigator.Selected.ImageIndex = iiDatabase) and (View = vDiagram)) then
    ActiveWorkbench.CreateNewTable(0, 0)
  else if (FocusedCItem is TCDatabase) then
  begin
    DTable.Database := TCDatabase(FocusedCItem);
    DTable.Table := nil;
    if (DTable.Execute()) then
      Client.Update();
  end;
end;

procedure TFClient.aDCreateTriggerExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (FocusedCItem is TCBaseTable) then
  begin
    DTrigger.Table := TCBaseTable(FocusedCItem);
    DTrigger.Trigger := nil;
    if (DTrigger.Execute()) then
      Client.Update();
  end;
end;

procedure TFClient.aDCreateUserExecute(Sender: TObject);
begin
  Wanted.Clear();

  DUser.Client := Client;
  DUser.User := nil;
  if (DUser.Execute()) then
    Client.Update();
end;

procedure TFClient.aDCreateViewExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (FocusedCItem is TCDatabase) then
  begin
    DView.Database := TCDatabase(FocusedCItem);
    DView.View := nil;
    if (DView.Execute()) then
      Client.Update();
  end;
end;

procedure TFClient.aDDeleteExecute(Sender: TObject);
var
  CItems: TList;
  Database: TCDatabase;
  I: Integer;
  List: TList;
  Msg: string;
  NewTable: TCBaseTable;
  Success: Boolean;
  Table: TCBaseTable;
begin
  Wanted.Clear();

  CItems := TList.Create();

  if ((Window.ActiveControl = ActiveListView) and (ActiveListView.SelCount > 1)) then
  begin
    for I := 0 to ActiveListView.Items.Count - 1 do
      if (ActiveListView.Items[I].Selected) then
        CItems.Add(ActiveListView.Items[I].Data);
  end
  else if ((Window.ActiveControl = ActiveWorkbench) and (ActiveWorkbench.SelCount > 1)) then
  begin
    for I := 0 to ActiveWorkbench.ControlCount - 1 do
      if ((ActiveWorkbench.Controls[I] is TWTable) and (TWTable(ActiveWorkbench.Controls[I]).Selected)) then
        CItems.Add(TWTable(ActiveWorkbench.Controls[I]).BaseTable)
      else if ((ActiveWorkbench.Controls[I] is TWForeignKey) and (TWForeignKey(ActiveWorkbench.Controls[I]).Selected)) then
        CItems.Add(TWForeignKey(ActiveWorkbench.Controls[I]).BaseForeignKey);
  end
  else if (Assigned(FocusedCItem)) then
    CItems.Add(FocusedCItem);

  if (CItems.Count > 1) then
    Msg := Preferences.LoadStr(413)
  else if (CItems.Count = 1) then
  begin
    if (TCItem(CItems[0]) is TCDatabase) then Msg := Preferences.LoadStr(146, TCItem(CItems[0]).Caption)
    else if (TCItem(CItems[0]) is TCBaseTable) then Msg := Preferences.LoadStr(113, TCItem(CItems[0]).Caption)
    else if (TCItem(CItems[0]) is TCView) then Msg := Preferences.LoadStr(748, TCItem(CItems[0]).Caption)
    else if (TCItem(CItems[0]) is TCProcedure) then Msg := Preferences.LoadStr(772, TCItem(CItems[0]).Caption)
    else if (TCItem(CItems[0]) is TCFunction) then Msg := Preferences.LoadStr(773, TCItem(CItems[0]).Caption)
    else if (TCItem(CItems[0]) is TCEvent) then Msg := Preferences.LoadStr(813, TCItem(CItems[0]).Caption)
    else if (TCItem(CItems[0]) is TCTrigger) then Msg := Preferences.LoadStr(787, TCItem(CItems[0]).Caption)
    else if (TCItem(CItems[0]) is TCKey) then Msg := Preferences.LoadStr(162, TCItem(CItems[0]).Caption)
    else if (TCItem(CItems[0]) is TCField) then Msg := Preferences.LoadStr(100, TCItem(CItems[0]).Caption)
    else if (TCItem(CItems[0]) is TCForeignKey) then Msg := Preferences.LoadStr(692, TCItem(CItems[0]).Caption)
    else if (TCItem(CItems[0]) is TCHost) then Msg := Preferences.LoadStr(429, TCItem(CItems[0]).Caption)
    else if (TCItem(CItems[0]) is TCUser) then Msg := Preferences.LoadStr(428, TCItem(CItems[0]).Caption)
    else if (TCItem(CItems[0]) is TCProcess) then Msg := Preferences.LoadStr(534, TCItem(CItems[0]).Caption);
  end
  else
    Msg := '';

  if ((Msg <> '') and (MsgBox(Msg, Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES)) then
  begin
    List := TList.Create();

    Success := True;

    List.Clear();
    for I := 0 to CItems.Count - 1 do
      if (TCItem(CItems[I]) is TCDatabase) then
      begin
        List.Add(TCDatabase(CItems[I]));
        CItems[I] := nil;
      end;
    if (Success and (List.Count > 0)) then
      Success := Client.DeleteDatabases(List);

    Database := nil;
    List.Clear();
    for I := 0 to CItems.Count - 1 do
      if (TCItem(CItems[I]) is TCDBObject) then
      begin
        Database := TCDBObject(CItems[I]).Database;
        List.Add(TCDBObject(CItems[I]));
        CItems[I] := nil;
      end;
    if (Success and (List.Count > 0)) then
      Success := Database.DeleteObjects(List);

    Table := nil;
    for I := 0 to CItems.Count - 1 do
      if (TCItem(CItems[I]) is TCKey) then
        Table := TCKey(CItems[I]).Table
      else if (TCItem(CItems[I]) is TCBaseTableField) then
        Table := TCBaseTableField(CItems[I]).Table
      else if (TCItem(CItems[I]) is TCForeignKey) then
        Table := TCForeignKey(CItems[I]).Table;
    if (Success and Assigned(Table)) then
    begin
      NewTable := TCBaseTable.Create(Table.Database.Tables);
      NewTable.Assign(Table);

      for I := CItems.Count - 1 downto 0 do
        if ((TCItem(CItems[I]) is TCKey) and (TCKey(CItems[I]).Table = Table)) then
        begin
          NewTable.Keys.DeleteKey(NewTable.Keys[TCKey(CItems[I]).Index]);
          CItems[I] := nil;
        end
        else if ((TCItem(CItems[I]) is TCBaseTableField) and (TCBaseTableField(CItems[I]).Table = Table)) then
        begin
          NewTable.Fields.DeleteField(NewTable.Fields[TCBaseTableField(CItems[I]).Index]);
          CItems[I] := nil;
        end
        else if ((TCItem(CItems[I]) is TCForeignKey) and (TCForeignKey(CItems[I]).Table = Table)) then
        begin
          NewTable.ForeignKeys.DeleteForeignKey(NewTable.ForeignKeys[TCForeignKey(CItems[I]).Index]);
          CItems[I] := nil;
        end;

      if (NewTable.Fields.Count = 0) then
        Success := Table.Database.DeleteObject(NewTable)
      else
        Success := Table.Database.UpdateTable(Table, NewTable);

      NewTable.Free();
    end;

    List.Clear();
    for I := 0 to CItems.Count - 1 do
      if (TCItem(CItems[I]) is TCHost) then
      begin
        List.Add(TCHost(CItems[I]));
        CItems[I] := nil;
      end;
    if (Success and (List.Count > 0)) then
      Success := Client.DeleteHosts(List);

    List.Clear();
    for I := 0 to CItems.Count - 1 do
      if (TCItem(CItems[I]) is TCUser) then
      begin
        List.Add(TCUser(CItems[I]));
        CItems[I] := nil;
      end;
    if (Success and (List.Count > 0)) then
      Success := Client.DeleteUsers(List);

    List.Clear();
    for I := 0 to CItems.Count - 1 do
      if (TCItem(CItems[I]) is TCProcess) then
      begin
        List.Add(TCProcess(CItems[I]));
        CItems[I] := nil;
      end;
    if (Success and (List.Count > 0)) then
      Client.DeleteProcesses(List);

    List.Free();
  end;

  CItems.Free();
end;

procedure TFClient.aDDeleteHostExecute(Sender: TObject);
begin
  Wanted.Clear();

;
end;

procedure TFClient.aDDeleteRecordExecute(Sender: TObject);
var
  Bookmarks: array of TBookmark;
  I: Integer;
begin
  Wanted.Clear();

  if (MsgBox(Preferences.LoadStr(176), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = ID_YES) then
  begin
    if (ActiveDBGrid.SelectedRows.Count = 0) then
      aDDeleteRecord.Execute()
    else if (ActiveDBGrid.DataSource.DataSet is TMySQLDataSet) then
    begin
      SetLength(Bookmarks, ActiveDBGrid.SelectedRows.Count);
      for I := 0 to Length(Bookmarks) - 1 do
        Bookmarks[I] := ActiveDBGrid.SelectedRows.Items[I];
      ActiveDBGrid.SelectedRows.Clear();
      TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).Delete(Bookmarks);
      SetLength(Bookmarks, 0);
    end;
  end;
end;

procedure TFClient.aDDeleteUserExecute(Sender: TObject);
begin
  Wanted.Clear();
end;

procedure TFClient.AddressChanged(Sender: TObject);
var
  Control: TWinControl;
  DDLStmt: TSQLDDLStmt;
  Empty: Boolean;
  I: Integer;
  NewActiveControl: TWinControl;
  OldControl: TWinControl;
  Parse: TSQLParse;
  Sibling: TTreeNode;
  SQL: string;
  Table: TCTable;
begin
  if (not (csDestroying in ComponentState)) then
  begin
    tbObjects.Down := MainAction('aVObjectBrowser').Checked;
    tbBrowser.Down := MainAction('aVDataBrowser').Checked;
    tbIDE.Down := MainAction('aVObjectIDE').Checked;
    tbBuilder.Down := MainAction('aVQueryBuilder').Checked;
    tbEditor.Down := MainAction('aVSQLEditor').Checked;
    tbDiagram.Down := MainAction('aVDiagram').Checked;


    LeftMousePressed := False;

    OldControl := Window.ActiveControl;

    PContentChange(Sender);


    while (Assigned(OldControl) and OldControl.Visible and OldControl.Enabled and Assigned(OldControl.Parent)) do
      OldControl := OldControl.Parent;

    case (View) of
      vObjects: NewActiveControl := ActiveListView;
      vBrowser: NewActiveControl := ActiveDBGrid;
      vIDE: NewActiveControl := ActiveSynMemo;
      vBuilder: NewActiveControl := FBuilderActiveWorkArea();
      vEditor: NewActiveControl := FSQLEditorSynMemo;
      vDiagram: NewActiveControl := ActiveWorkbench;
      else NewActiveControl := nil;
    end;

    Control := NewActiveControl;
    while (Assigned(Control) and Control.Visible and Control.Enabled and Assigned(Control.Parent)) do
      Control := Control.Parent;
    if ((not Assigned(OldControl) or not OldControl.Visible or not OldControl.Enabled)
      and Assigned(Control) and Control.Visible and Control.Enabled) then
      Window.ActiveControl := NewActiveControl;


    case (View) of
      vObjects: if (not (ttObjects in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttObjects); PostMessage(Window.Handle, CM_CHANGEPREFERENCES, 0, 0); end;
      vBrowser: if (not (ttBrowser in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttBrowser); PostMessage(Window.Handle, CM_CHANGEPREFERENCES, 0, 0); end;
      vIDE: if (not (ttIDE in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttIDE); PostMessage(Window.Handle, CM_CHANGEPREFERENCES, 0, 0); end;
      vBuilder: if (not (ttBuilder in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttBuilder); PostMessage(Window.Handle, CM_CHANGEPREFERENCES, 0, 0); end;
      vEditor: if (not (ttEditor in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttEditor); PostMessage(Window.Handle, CM_CHANGEPREFERENCES, 0, 0); end;
      vDiagram: if (not (ttDiagram in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttDiagram); PostMessage(Window.Handle, CM_CHANGEPREFERENCES, 0, 0); end;
    end;

    ToolBarData.Caption := AddressToCaption(Address);

    ToolBarData.View := View;

    if (Address <> ToolBarData.Address) then
    begin
      ToolBarData.Address := Address;

      if (not MovingToAddress) then
      begin
        while (ToolBarData.Addresses.Count > ToolBarData.CurrentAddress + 1) do
          ToolBarData.Addresses.Delete(ToolBarData.CurrentAddress + 1);
        ToolBarData.Addresses.Add(Address);

        while (ToolBarData.Addresses.Count > 30) do
          ToolBarData.Addresses.Delete(0);

        ToolBarData.CurrentAddress := ToolBarData.Addresses.Count - 1;
      end;

      Window.Perform(CM_UPDATETOOLBAR, 0, LPARAM(Self));
    end;

    if (Assigned(FNavigator.Selected) and (FNavigator.Selected <> LastFNavigatorSelected)) then
    begin
      if (FNavigator.AutoExpand and Assigned(FNavigator.Selected)) then
      begin
        if ((FNavigator.Selected.ImageIndex in [iiBaseTable, iiSystemView, iiView]) and not Dragging(FNavigator)) then
          FNavigator.Selected.Expand(False);

        if (Assigned(FNavigator.Selected.Parent)) then
        begin
          Sibling := FNavigator.Selected.Parent.getFirstChild();
          while (Assigned(Sibling)) do
          begin
            if (Sibling <> FNavigator.Selected) then
              Sibling.Collapse(False);
            Sibling := FNavigator.Selected.Parent.getNextChild(Sibling);
          end;
        end;
      end;

      if ((tsActive in FrameState) and (ToolBarData.CurrentAddress > 0) and Wanted.Nothing) then
        PlaySound(PChar(Preferences.SoundFileNavigating), Handle, SND_FILENAME or SND_ASYNC);

      if ((View = vBrowser) and (FNavigator.Selected.ImageIndex in [iiBaseTable, iiSystemView, iiView])) then
      begin
        Table := TCTable(FNavigator.Selected.Data);

        if (not (Table.DataSet is TMySQLTable) or not Assigned(Table.DataSet) or not Table.DataSet.Active or (TMySQLTable(Table.DataSet).Limit < 1)) then
        begin
          FUDOffset.Position := 0;
          FUDLimit.Position := Desktop(Table).Limit;
          FLimitEnabled.Down := Desktop(Table).Limited;
        end
        else
        begin
          FUDOffset.Position := TMySQLTable(Table.DataSet).Offset;
          FUDLimit.Position := TMySQLTable(Table.DataSet).Limit;
          FLimitEnabled.Down := TMySQLTable(Table.DataSet).Limit > 1;
        end;
        FFilterEnabled.Down := (Table.DataSet is TMySQLTable) and Assigned(Table.DataSet) and Table.DataSet.Active and (Table.DataSet.FilterSQL <> '');
        if (not FFilterEnabled.Down) then
          FFilter.Text := ''
        else
          FFilter.Text := TMySQLTable(Table.DataSet).FilterSQL;
        FilterMRU.Clear();
        for I := 0 to Desktop(Table).FilterCount - 1 do
          FilterMRU.Add(Desktop(Table).Filters[I]);
        FFilterEnabled.Enabled := FFilter.Text <> '';
      end;

      if (Window.ActiveControl = FNavigator) then
        FNavigatorSetMenuItems(FNavigator, FNavigator.Selected);

      FNavigator.AutoExpand := not (FNavigator.Selected.ImageIndex in [iiBaseTable, iiSystemView, iiView]) and not CheckWin32Version(6);

      TreeViewExpanded(FNavigator, FNavigator.Selected);
    end;

    LastFNavigatorSelected := FNavigator.Selected;
    if (SelectedDatabase <> '') then
      LastSelectedDatabase := SelectedDatabase;
    if (SelectedImageIndex in [iiBaseTable, iiSystemView, iiView]) then
    begin
      LastSelectedTable := FNavigator.Selected.Text;
      LastTableView := View;
    end
    else if (SelectedImageIndex = iiTrigger) then
      LastSelectedTable := FNavigator.Selected.Parent.Text;
    if (View = vIDE) then
      LastObjectIDEAddress := Address;


    Empty := not Assigned(ActiveSynMemo) or (ActiveSynMemo.Lines.Count <= 1) and (ActiveSynMemo.Text = ''); // Takes a lot of time
    if (not Empty and (View = vIDE)) then SQL := ActiveSynMemo.Text else SQL := '';

    MainAction('aFOpen').Enabled := View = vEditor;
    MainAction('aFSave').Enabled := (View = vEditor) and not Empty and ((SQLEditor.Filename = '') or ActiveSynMemo.Modified);
    MainAction('aFSaveAs').Enabled := (View = vEditor) and not Empty;
    MainAction('aVObjectBrowser').Enabled := True;
    MainAction('aVDataBrowser').Enabled := (SelectedImageIndex in [iiBaseTable, iiSystemView, iiView, iiTrigger]) or ((LastSelectedDatabase <> '') and (LastSelectedDatabase = SelectedDatabase) and (LastSelectedTable <> ''));
    MainAction('aVObjectIDE').Enabled := (SelectedImageIndex in [iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]) or (LastObjectIDEAddress <> '');
    MainAction('aVQueryBuilder').Enabled := (LastSelectedDatabase <> '');
    MainAction('aVSQLEditor').Enabled := True;
    MainAction('aVDiagram').Enabled := (LastSelectedDatabase <> '');
    MainAction('aDRun').Enabled :=
      ((View = vEditor)
      or ((View  = vBuilder) and FBuilder.Visible)
      or ((View = vIDE) and SQLSingleStmt(SQL) and (SelectedImageIndex in [iiView, iiProcedure, iiFunction, iiEvent]))) and not Empty;
    MainAction('aDRunSelection').Enabled := (((ActiveSynMemo = FSQLEditorSynMemo) and not Empty) or Assigned(ActiveSynMemo) and (ActiveSynMemo.SelText <> ''));
    MainAction('aDPostObject').Enabled := (View = vIDE) and Assigned(ActiveSynMemo) and ActiveSynMemo.Modified and SQLSingleStmt(SQL)
      and ((SelectedImageIndex in [iiView]) and SQLCreateParse(Parse, PChar(SQL), Length(SQL),Client.ServerVersion) and (SQLParseKeyword(Parse, 'SELECT'))
        or (SelectedImageIndex in [iiProcedure, iiFunction]) and SQLParseDDLStmt(DDLStmt, PChar(SQL), Length(SQL), Client.ServerVersion) and (DDLStmt.DefinitionType = dtCreate) and (DDLStmt.ObjectType in [otProcedure, otFunction])
        or (SelectedImageIndex in [iiEvent, iiTrigger]));

    StatusBarRefresh();


    if (tsLoading in FrameState) then
    begin
      if (PSideBar.Visible) then
      begin
        if (PNavigator.Visible) then Window.ActiveControl := FNavigator
        else if (PBookmarks.Visible) then Window.ActiveControl := FBookmarks
        else if (PListView.Visible) then Window.ActiveControl := ActiveListView
        else if (PBuilder.Visible) then Window.ActiveControl := FBuilder
        else if (PSynMemo.Visible) then Window.ActiveControl := FSQLEditorSynMemo
        else if (PResult.Visible) then Window.ActiveControl := ActiveDBGrid;
      end
      else
        case (View) of
          vObjects: if (PListView.Visible) then Window.ActiveControl := ActiveListView;
          vBrowser: if (PResult.Visible) then Window.ActiveControl := ActiveDBGrid;
          vIDE: if (PSynMemo.Visible and Assigned(ActiveSynMemo)) then Window.ActiveControl := ActiveSynMemo;
          vBuilder: if (PBuilder.Visible and Assigned(FBuilderActiveWorkArea())) then Window.ActiveControl := FBuilderActiveWorkArea();
          vEditor: if (PSynMemo.Visible) then Window.ActiveControl := FSQLEditorSynMemo;
          vDiagram: if (PWorkbench.Visible) then Window.ActiveControl := ActiveWorkbench;
        end;
      Exclude(FrameState, tsLoading);
    end;

    Wanted.Update := UpdateAfterAddressChanged;
  end;
end;

procedure TFClient.AddressChanging(const Sender: TObject; const NewAddress: String; var AllowChange: Boolean);
var
  NotFound: Boolean;
  Database: TCDatabase;
  DBObject: TCDBObject;
  S: string;
  URI: TUURI;
begin
  URI := TUURI.Create(NewAddress); NotFound := False;

  if (URI.Scheme <> 'mysql') then
    AllowChange := False
  else if ((lstrcmpi(PChar(URI.Host), PChar(Client.Account.Connection.Host)) <> 0) and ((lstrcmpi(PChar(URI.Host), LOCAL_HOST) <> 0))) then
    AllowChange := False
  else if (URI.Port <> Client.Account.Connection.Port) then
    AllowChange := False
  else if ((URI.Username <> '') and (lstrcmpi(PChar(URI.Username), PChar(Client.Account.Connection.User)) <> 0)) then
    AllowChange := False
  else if ((URI.Password <> '') and (URI.Password <> Client.Account.Connection.Password)) then
    AllowChange := False
  else
  begin
    S := URI.Path;
    if (URI.Database <> '') then
      Delete(S, 1, 1 + Length(EscapeURL(URI.Database)));
    if (URI.Table <> '') then
      Delete(S, 1, 1 + Length(EscapeURL(URI.Table)));
    if ((S <> '') and (S <> '/')) then
      AllowChange := False;
  end;

  if (AllowChange) then
  begin
    if (not Client.Update() and ((URI.Database <> '') or (URI.Param['system'] <> Null))) then
      AllowChange := False
    else if (URI.Param['system'] = 'hosts') then
      Client.Hosts.Update()
    else if (URI.Param['system'] = 'processes') then
      Client.Processes.Update()
    else if (URI.Param['system'] = 'stati') then
      Client.Stati.Update()
    else if (URI.Param['system'] = 'users') then
      Client.Users.Update()
    else if (URI.Param['system'] = 'variables') then
      Client.Variables.Update()
    else if (URI.Database <> '') then
    begin
      Database := Client.DatabaseByName(URI.Database);
      if (not Assigned(Database)) then
        NotFound := True
      else if (not Database.Update((URI.Table = '') and (URI.Param['object'] = Null) and (URI.Param['view'] = NULL)) and ((URI.Table <> '') or (URI.Param['object'] <> Null))) then
        AllowChange := False
      else if ((URI.Table <> '') or (URI.Param['object'] <> Null)) then
      begin
        if (URI.Table <> '') then
          DBObject := Database.TableByName(URI.Table)
        else if ((URI.Param['objecttype'] = 'procedure') and (URI.Param['object'] <> Null)) then
          DBObject := Database.ProcedureByName(URI.Param['object'])
        else if ((URI.Param['objecttype'] = 'function') and (URI.Param['object'] <> Null)) then
          DBObject := Database.FunctionByName(URI.Param['object'])
        else if ((URI.Param['objecttype'] = 'trigger') and (URI.Param['object'] <> Null)) then
          DBObject := Database.TriggerByName(URI.Param['object'])
        else if ((URI.Param['objecttype'] = 'event') and (URI.Param['object'] <> Null)) then
          DBObject := Database.EventByName(URI.Param['object'])
        else
          DBObject := nil;

        if (not Assigned(DBObject)) then
          NotFound := True
        else if (not DBObject.Update()) then
          AllowChange := False
        else if ((URI.Param['objecttype'] = 'trigger') and (URI.Param['object'] <> Null) and not Assigned(Database.TriggerByName(URI.Param['object']))) then
          NotFound := True
      end;
    end;

    if (NotFound) then
    begin
      AllowChange := False;
      Wanted.Clear();
    end
    else if (not AllowChange and Client.Asynchron) then
      Wanted.Address := NewAddress;
  end;

  URI.Free();

  if (AllowChange and Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and ActiveDBGrid.DataSource.DataSet.Active) then
    try
      if ((Window.ActiveControl = FText) or (Window.ActiveControl = FRTF) or (Window.ActiveControl = FHexEditor)) then
        Window.ActiveControl := ActiveDBGrid;
      ActiveDBGrid.DataSource.DataSet.CheckBrowseMode();
    except
      AllowChange := False;
    end;

  if (AllowChange) then
    if (Assigned(Window.ActiveControl) and IsChild(PContent.Handle, Window.ActiveControl.Handle)) then
      Window.ActiveControl := nil;
end;

function TFClient.AddressToCaption(const AAddress: string): string;
var
  URI: TUURI;
begin
  URI := TUURI.Create(AAddress); if (URI.Scheme <> 'mysql') then FreeAndNil(URI);

  if (not Assigned(URI)) then
    Result := ''
  else if (URI.Database <> '') then
  begin
    Result := URI.Database;
    if (URI.Table <> '') then
      Result := Result + '.' + URI.Table;
    if (((URI.Table = '') or (URI.Param['objecttype'] = 'trigger')) and (URI.Param['object'] <> Null)) then
      Result := Result + '.' + URI.Param['object'];
  end
  else if ((URI.Database = '') and (URI.Param['system'] = 'hosts')) then
    Result := ReplaceStr(Preferences.LoadStr(335), '&', '')
  else if ((URI.Database = '') and (URI.Param['system'] = 'processes')) then
    Result := ReplaceStr(Preferences.LoadStr(24), '&', '')
  else if ((URI.Database = '') and (URI.Param['system'] = 'stati')) then
    Result := ReplaceStr(Preferences.LoadStr(23), '&', '')
  else if ((URI.Database = '') and (URI.Param['system'] = 'users')) then
    Result := ReplaceStr(Preferences.LoadStr(561), '&', '')
  else if ((URI.Database = '') and (URI.Param['system'] = 'variables')) then
    Result := ReplaceStr(Preferences.LoadStr(22), '&', '');

  if ((URI.Param['view'] = 'editor') and (URI.Param['file'] <> Null)) then
    Result := Result + ' - ' + EscapeURL(URI.Param['file']);

  Result := UnescapeURL(Result);

  URI.Free();
end;

procedure TFClient.aDInsertRecordExecute(Sender: TObject);
begin
  Wanted.Clear();

  aDInsertRecord.Execute();
end;

procedure TFClient.aDNextExecute(Sender: TObject);
begin
  Wanted.Clear();

  ActiveDBGrid.DataSource.DataSet.MoveBy(ActiveDBGrid.RowCount - 1);
end;

procedure TFClient.aDPostObjectExecute(Sender: TObject);
begin
  Wanted.Clear();

  PostObject(Sender)
end;

procedure TFClient.aDPrevExecute(Sender: TObject);
begin
  Wanted.Clear();

  ActiveDBGrid.DataSource.DataSet.MoveBy(- (ActiveDBGrid.RowCount - 1));
end;

procedure TFClient.aDPropertiesExecute(Sender: TObject);
type
  TExecute = function(): Boolean of Object;
var
  CItem: TCItem;
  Execute: TExecute;
  I: Integer;
  Process: TCProcess;
begin
  Wanted.Clear();

  if ((Window.ActiveControl = ActiveListView) and (SelectedImageIndex in [iiDatabase, iiSystemDatabase]) and (ActiveListView.SelCount > 1)) then
  begin
    DTable.Tables := TList.Create();
    for I := 0 to ActiveListView.Items.Count - 1 do
      if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex = iiBaseTable)) then
        DTable.Tables.Add(ActiveListView.Items[I].Data);

    DTable.Database := TCDatabase(FNavigator.Selected.Data);
    DTable.Table := nil;
    if (DTable.Execute()) then
      Client.Update();
  end
  else if ((Window.ActiveControl = ActiveWorkbench) and (ActiveWorkbench.Selected is TWSection)) then
  begin
    DSegment.Section := TWSection(ActiveWorkbench.Selected);
    DSegment.Execute();
    ActiveWorkbench.Selected := nil;
  end
  else
  begin
    CItem := FocusedCItem;

    if (CItem is TCDatabase) then
    begin
      DDatabase.Client := Client;
      DDatabase.Database := TCDatabase(CItem);
      Execute := DDatabase.Execute;
    end
    else if (CItem is TCBaseTable) then
    begin
      DTable.Database := TCBaseTable(CItem).Database;
      DTable.Table := TCBaseTable(CItem);
      Execute := DTable.Execute;
    end
    else if (CItem is TCView) then
    begin
      DView.Database := TCView(CItem).Database;
      DView.View := TCView(CItem);
      Execute := DView.Execute;
    end
    else if (CItem is TCProcedure) then
    begin
      DRoutine.Database := TCRoutine(CItem).Database;
      DRoutine.Routine := TCRoutine(CItem);
      Execute := DRoutine.Execute;
    end
    else if (CItem is TCFunction) then
    begin
      DRoutine.Database := TCRoutine(CItem).Database;
      DRoutine.Routine := TCRoutine(CItem);
      Execute := DRoutine.Execute;
    end
    else if (CItem is TCTrigger) then
    begin
      DTrigger.Table := TCTrigger(CItem).Table;
      DTrigger.Trigger := TCTrigger(CItem);
      Execute := DTrigger.Execute;
    end
    else if (CItem is TCEvent) then
    begin
      DEvent.Database := TCEvent(CItem).Database;
      DEvent.Event := TCEvent(CItem);
      Execute := DEvent.Execute;
    end
    else if (CItem is TCKey) then
    begin
      DIndex.Database := TCKey(CItem).Table.Database;
      DIndex.Table := TCKey(CItem).Table;
      DIndex.Key := TCKey(CItem);
      Execute := DIndex.Execute;
    end
    else if (CItem is TCBaseTableField) then
    begin
      DField.Database := TCBaseTableField(CItem).Table.Database;
      DField.Table := TCBaseTable(TCBaseTableField(CItem).Table);
      DField.Field := TCBaseTableField(CItem);
      Execute := DField.Execute;
    end
    else if (CItem is TCForeignKey) then
    begin
      DForeignKey.Database := TCForeignKey(CItem).Table.Database;
      DForeignKey.Table := TCForeignKey(CItem).Table;
      DForeignKey.ParentTable := nil;
      DForeignKey.ForeignKey := TCForeignKey(CItem);
      Execute := DForeignKey.Execute;
    end
    else if (CItem is TCHost) then
    begin
      DHost.Client := Client;
      DHost.Host := TCHost(CItem);
      Execute := DHost.Execute;
    end
    else if (CItem is TCProcess) then
    begin
      Process := Client.ProcessById(SysUtils.StrToInt(ActiveListView.Selected.Caption));

      DStatement.DatabaseName := Process.DatabaseName;
      DStatement.DateTime := Client.DateTime - Process.Time;
      DStatement.Host := Process.Host;
      DStatement.Id := Process.Id;
      DStatement.StatementTime := Process.Time;
      if (UpperCase(Process.Command) <> 'QUERY') then
        DStatement.SQL := ''
      else
        DStatement.SQL := Process.SQL + ';';
      DStatement.UserName := Process.UserName;
      DStatement.ViewType := vtProcess;

      Execute := DStatement.Execute;
    end
    else if (CItem is TCUser) then
    begin
      DUser.Client := Client;
      DUser.User := TCUser(CItem);
      Execute := DUser.Execute;
    end
    else if (CItem is TCVariable) then
    begin
      DVariable.Client := Client;
      DVariable.Variable := TCVariable(CItem);
      Execute := DVariable.Execute;
    end
    else
      Execute := nil;

    if (Assigned(Execute) and Execute()) then
      Client.Update();
  end;
end;

procedure TFClient.aDRollbackExecute(Sender: TObject);
begin
  Wanted.Clear();

  Client.RollbackTransaction();

  aDCommitRefresh(Sender);
end;

procedure TFClient.aDRunExecute(Sender: TObject);
var
  SQL: string;
begin
  Wanted.Clear();

  if (Window.ActiveControl is TMySQLDBGrid) then
    TMySQLDBGrid(Window.ActiveControl).EditorMode := False;

  FSQLEditorCompletion.CancelCompletion();
  if (Assigned(FBuilderActiveSelectList())) then
    FBuilderActiveSelectList().EditorMode := False;

  SQL := '';
  if (View in [vBuilder, vEditor]) then
    SQL := Trim(ActiveSynMemo.Text)
  else if ((View = vIDE) and (SelectedImageIndex = iiView)) then
  begin
    if (not ActiveSynMemo.Modified or PostObject(Sender)) then
      View := vBrowser;
  end
  else if ((View = vIDE) and (SelectedImageIndex = iiProcedure)) then
  begin
    if (Assigned(FObjectIDEGrid.DataSource.DataSet)) then
      FObjectIDEGrid.DataSource.DataSet.CheckBrowseMode();
    if (not ActiveSynMemo.Modified or PostObject(Sender)) then
      SQL := TCProcedure(FNavigator.Selected.Data).SQLRun();
  end
  else if ((View = vIDE) and (SelectedImageIndex = iiFunction)) then
  begin
    if (Assigned(FObjectIDEGrid.DataSource.DataSet)) then
      FObjectIDEGrid.DataSource.DataSet.CheckBrowseMode();
    if (not ActiveSynMemo.Modified or PostObject(Sender)) then
      SQL := TCFunction(FNavigator.Selected.Data).SQLRun();
  end
  else if ((View = vIDE) and (SelectedImageIndex = iiEvent)) then
  begin
    if (not ActiveSynMemo.Modified or PostObject(Sender)) then
      SQL := TCEvent(FNavigator.Selected.Data).SQLRun();
  end;

  if (SQL <> '') then
  begin
    if ((SelectedDatabase <> Client.DatabaseName) and (SelectedDatabase <> '')) then
      Client.ExecuteSQL(Client.SQLUse(SelectedDatabase));

    aDRunExecuteSelStart := 0;
    SendQuery(Sender, SQL);
  end;
end;

procedure TFClient.aDRunSelectionExecute(Sender: TObject);
var
  Index: Integer;
  Len: Integer;
  SQL: string;
begin
  Wanted.Clear();

  FSQLEditorCompletion.CancelCompletion();

  aDRunExecuteSelStart := ActiveSynMemo.SelStart;
  if (ActiveSynMemo.SelText = '') then
  begin
    SQL := ActiveSynMemo.Text;
    Index := 1; Len := 0;
    while (Index < aDRunExecuteSelStart + 1) do
    begin
      Len := SQLStmtLength(SQL, Index);
      Inc(Index, Len);
    end;
    Dec(Index, Len);
    SQL := Copy(SQL, Index, Len);
    aDRunExecuteSelStart := Index - 1;
  end
  else
    SQL := ActiveSynMemo.SelText;

  if ((SQL <> '') and ((SelectedDatabase = '') or (SelectedDatabase = Client.DatabaseName) or Client.ExecuteSQL(Client.SQLUse(SelectedDatabase)))) then
    SendQuery(Sender, SQL);
end;

procedure TFClient.aEClearAllExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (Window.ActiveControl = FText) then
    FText.Text := ''
  else if (Window.ActiveControl = FSQLEditorSynMemo) then
  begin
    // ClearAll kann nicht mit Undo Rückgängig gemacht werden.
    FSQLEditorSynMemo.BeginUpdate();
    FSQLEditorSynMemo.SelectAll();
    MainAction('aEDelete').Execute();
    FSQLEditorSynMemo.EndUpdate();
  end;
end;

procedure TFClient.aECopyExecute(Sender: TObject);
var
  ClipboardData: HGLOBAL;
  Data: string;
  I: Integer;
  ImageIndex: Integer;
  S: string;
  StringList: TStringList;
begin
  Data := '';

  if (Window.ActiveControl = FNavigator) then
  begin
    if (not Assigned(FNavigatorMenuNode.Parent)) then
      ImageIndex := -1
    else
    begin
      ImageIndex := FNavigatorMenuNode.Parent.ImageIndex;
      case (FNavigatorMenuNode.ImageIndex) of
        iiDatabase,
        iiSystemView: Data := Data + 'Database='    + FNavigatorMenuNode.Text + #13#10;
        iiBaseTable:  Data := Data + 'Table='       + FNavigatorMenuNode.Text + #13#10;
        iiView:       Data := Data + 'View='        + FNavigatorMenuNode.Text + #13#10;
        iiProcedure:  Data := Data + 'Procedure='   + FNavigatorMenuNode.Text + #13#10;
        iiFunction:   Data := Data + 'Function='    + FNavigatorMenuNode.Text + #13#10;
        iiEvent:      Data := Data + 'Event='       + FNavigatorMenuNode.Text + #13#10;
        iiKey:      Data := Data + 'Index='       + FNavigatorMenuNode.Text + #13#10;
        iiSystemViewField,
        iiField,
        iiViewField:  Data := Data + 'Field='       + FNavigatorMenuNode.Text + #13#10;
        iiForeignKey: Data := Data + 'ForeignKey='  + FNavigatorMenuNode.Text + #13#10;
        iiTrigger:    Data := Data + 'Trigger='     + FNavigatorMenuNode.Text + #13#10;
        iiHost:       Data := Data + 'Host='        + FNavigatorMenuNode.Text + #13#10;
        iiUser:       Data := Data + 'User='        + FNavigatorMenuNode.Text + #13#10;
      end;
      if (Data <> '') then
        Data := 'Address=' + NavigatorNodeToAddress(FNavigatorMenuNode.Parent) + #13#10 + Data;
    end;
  end
  else if (Window.ActiveControl = ActiveListView) then
  begin
    ImageIndex := SelectedImageIndex;
    for I := 0 to ActiveListView.Items.Count - 1 do
      if (ActiveListView.Items[I].Selected) then
        case (ActiveListView.Items[I].ImageIndex) of
          iiDatabase,
          iiSystemView: Data := Data + 'Database='   + ActiveListView.Items[I].Caption + #13#10;
          iiBaseTable:  Data := Data + 'Table='      + ActiveListView.Items[I].Caption + #13#10;
          iiView:       Data := Data + 'View='       + ActiveListView.Items[I].Caption + #13#10;
          iiProcedure:  Data := Data + 'Procedure='  + ActiveListView.Items[I].Caption + #13#10;
          iiFunction:   Data := Data + 'Function='   + ActiveListView.Items[I].Caption + #13#10;
          iiEvent:      Data := Data + 'Event='      + ActiveListView.Items[I].Caption + #13#10;
          iiKey:        Data := Data + 'Key='      + TCKey(ActiveListView.Items[I].Data).Name + #13#10;
          iiField,
          iiViewField:  Data := Data + 'Field='      + ActiveListView.Items[I].Caption + #13#10;
          iiForeignKey: Data := Data + 'ForeignKey=' + ActiveListView.Items[I].Caption + #13#10;
          iiTrigger:    Data := Data + 'Trigger='    + ActiveListView.Items[I].Caption + #13#10;
          iiHost:       Data := Data + 'Host='       + TCHost(ActiveListView.Items[I].Data).Name + #13#10;
          iiUser:       Data := Data + 'User='       + ActiveListView.Items[I].Caption + #13#10;
        end;
    if (Data <> '') then
      Data := 'Address=' + NavigatorNodeToAddress(FNavigator.Selected) + #13#10 + Data;
  end
  else if (Window.ActiveControl = ActiveWorkbench) then
  begin
    if ((ActiveWorkbench.Selected is TWSection) and OpenClipboard(Handle)) then
    begin
      EmptyClipboard();

      S := TWSection(ActiveWorkbench.Selected).Caption;
      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SizeOf(S[1]) * (Length(S) + 1));
      StrPCopy(GlobalLock(ClipboardData), S);
      SetClipboardData(CF_UNICODETEXT, ClipboardData);
      GlobalUnlock(ClipboardData);

      CloseClipboard();

      Exit;
    end
    else
    begin
      ImageIndex := SelectedImageIndex;
      if (Assigned(ActiveWorkbench)) then
      begin
        Data := 'Address=' + NavigatorNodeToAddress(FNavigator.Selected);
        if (not Assigned(ActiveWorkbench.Selected)) then
          Data := Data + 'Database='   + ActiveWorkbench.Database.Name + #13#10
        else if (ActiveWorkbench.Selected is TWTable) then
          Data := Data + 'Table='      + TWTable(ActiveWorkbench.Selected).BaseTable.Name + #13#10
        else if (ActiveWorkbench.Selected is TWForeignKey) then
          Data := Data + 'ForeignKey=' + TWForeignKey(ActiveWorkbench.Selected).BaseForeignKey.Name + #13#10;
        if (Data <> '') then
          Data := 'Address=' + NavigatorNodeToAddress(FNavigator.Selected) + #13#10 + Data;
      end;
    end;
  end
  else if (Window.ActiveControl = ActiveDBGrid) then
  begin
    ActiveDBGrid.CopyToClipboard();
    Exit;
  end
  else if (Window.ActiveControl = FSQLHistory) then
  begin
    if (Assigned(FSQLHistory.Selected) and OpenClipboard(Handle)) then
    begin
      EmptyClipboard();

      S := XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'sql').Text;
      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SizeOf(S[1]) * (Length(S) + 1));
      StrPCopy(GlobalLock(ClipboardData), S);
      SetClipboardData(CF_UNICODETEXT, ClipboardData);
      GlobalUnlock(ClipboardData);

      CloseClipboard();
    end;
    Exit;
  end
  else if (Window.ActiveControl = FHexEditor) then
  begin
    FHexEditor.ExecuteAction(MainAction('aECopy'));
    Exit;
  end
  else
  begin
    if (Assigned(Window.ActiveControl)) then
      SendMessage(Window.ActiveControl.Handle, WM_COPY, 0, 0);
    Exit;
  end;

  if ((Data <> '') and OpenClipboard(Handle)) then
  begin
    EmptyClipboard();

    ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SizeOf(Char) * (Length(Data) + 1));
    StrPCopy(GlobalLock(ClipboardData), Data);
    case (ImageIndex) of
      iiServer: SetClipboardData(CF_MYSQLSERVER, ClipboardData);
      iiDatabase,
      iiSystemDatabase: SetClipboardData(CF_MYSQLDATABASE, ClipboardData);
      iiTable,
      iiBaseTable,
      iiSystemView: SetClipboardData(CF_MYSQLTABLE, ClipboardData);
      iiView: SetClipboardData(CF_MYSQLVIEW, ClipboardData);
      iiUsers: SetClipboardData(CF_MYSQLUSERS, ClipboardData);
      iiHosts: SetClipboardData(CF_MYSQLHOSTS, ClipboardData);
    end;
    GlobalUnlock(ClipboardData);

    StringList := TStringList.Create();
    StringList.Text := Trim(Data);
    for I := 1 to StringList.Count - 1 do
      if (StringList.ValueFromIndex[I] <> '') then
      begin
        if (S <> '') then S := S + ',';
        S := S + StringList.ValueFromIndex[I];
      end;
    StringList.Free();

    ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SizeOf(S[1]) * (Length(S) + 1));
    StrPCopy(GlobalLock(ClipboardData), S);
    SetClipboardData(CF_UNICODETEXT, ClipboardData);
    GlobalUnlock(ClipboardData);

    CloseClipboard();
  end;
end;

procedure TFClient.aEFindExecute(Sender: TObject);
var
  I: Integer;
begin
  Wanted.Clear();

  DSearch.Client := Client;
  if ((SelectedImageIndex in [iiBaseTable, iiSystemView, iiView]) and (Window.ActiveControl = ActiveListView)) then
  begin
    DSearch.DatabaseName := FNavigator.Selected.Parent.Text;
    DSearch.TableName := FNavigator.Selected.Text;
    DSearch.FieldName := '';
    for I := 0 to ActiveListView.Items.Count - 1 do
      if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex in [iiField, iiSystemViewField, iiViewField])) then
      begin
        if (DSearch.FieldName <> '') then
          DSearch.FieldName := DSearch.FieldName + ',';
        DSearch.FieldName := DSearch.FieldName + ActiveListView.Items[I].Caption;
      end;
  end
  else
  begin
    DSearch.DatabaseName := FocusedDatabaseNames;
    if (DSearch.DatabaseName = '') then
    begin
      DSearch.TableName := '';
      DSearch.FieldName := '';
    end
    else
    begin
      DSearch.DatabaseName := SelectedDatabase;
      DSearch.TableName := FocusedTableNames;
      DSearch.FieldName := '';
    end;
  end;
  DSearch.SearchOnly := True;
  DSearch.Frame := Self;
  DSearch.Execute();
end;

procedure TFClient.aEPasteExecute(Sender: TObject);
var
  B: Boolean;
  ClipboardData: HGLOBAL;
  I: Integer;
  Node: TTreeNode;
  S: string;
begin
  if (Client.InUse()) then
    MessageBeep(MB_ICONERROR)
  else if (Assigned(ActiveDBGrid) and (Window.ActiveControl = ActiveDBGrid)) then
  begin
    if (not Assigned(EditorField)) then
      ActiveDBGrid.PasteFromClipboard()
    else if (FText.Visible) then
      FText.PasteFromClipboard()
    else if (FRTF.Visible) then
      FRTF.PasteFromClipboard();
  end
  else if (Assigned(ActiveDBGrid) and (Window.ActiveControl = ActiveDBGrid.InplaceEditor)) then
  begin
    ActiveDBGrid.DataSource.DataSet.Edit();
    ActiveDBGrid.InplaceEditor.PasteFromClipboard()
  end
  else if (Assigned(ActiveListView) and (Window.ActiveControl = FNavigator) or (Window.ActiveControl = ActiveListView)) then
  begin
    if (Window.ActiveControl = FNavigator) then
      Node := FNavigatorMenuNode
    else if ((Window.ActiveControl = ActiveListView) and Assigned(ActiveListView.Selected)) then
    begin
      Node := nil;
      FNavigatorExpanding(Sender, FNavigator.Selected, B);
      for I := 0 to FNavigator.Selected.Count - 1 do
        if ((FNavigator.Selected[I].ImageIndex = ActiveListView.Selected.ImageIndex)
          and (FNavigator.Selected[I].Text = ActiveListView.Selected.Caption)) then
          Node := FNavigator.Selected[I];
    end
    else
      Node := FNavigator.Selected;

    if (not Assigned(Node)) then
      MessageBeep(MB_ICONERROR)
    else if ((Node.ImageIndex > 0) and OpenClipboard(Handle)) then
    begin
      case (Node.ImageIndex) of
        iiServer: ClipboardData := GetClipboardData(CF_MYSQLSERVER);
        iiDatabase: ClipboardData := GetClipboardData(CF_MYSQLDATABASE);
        iiBaseTable: ClipboardData := GetClipboardData(CF_MYSQLTABLE);
        iiView: ClipboardData := GetClipboardData(CF_MYSQLVIEW);
        iiHosts: ClipboardData := GetClipboardData(CF_MYSQLHOSTS);
        iiUsers: ClipboardData := GetClipboardData(CF_MYSQLUSERS);
        else ClipboardData := 0;
      end;

      if (ClipboardData = 0) then
      begin
        CloseClipboard();
        MessageBeep(MB_ICONERROR);
      end
      else
      begin
        S := Trim(PChar(GlobalLock(ClipboardData)));;
        GlobalUnlock(ClipboardData);
        CloseClipboard();

        PasteExecute(Node, S)
      end;
    end;
  end
  else if (Window.ActiveControl = FSQLEditorSynMemo) then
    FSQLEditorSynMemo.PasteFromClipboard()
  else if (Assigned(ActiveWorkbench) and (Window.ActiveControl = ActiveWorkbench)) then
    WorkbenchPasteExecute(Sender)
  else
    MessageBeep(MB_ICONERROR);
end;

procedure TFClient.aEPasteFromExecute(Sender: TObject);
begin
  Wanted.Clear();

  OpenSQLFile('', 0, True);
end;

procedure TFClient.aEPasteFromFileExecute(Sender: TObject);
var
  Encoding: TEncoding;
  Reader: TStreamReader;
begin
  Wanted.Clear();

  OpenDialog.Title := ReplaceStr(Preferences.LoadStr(581), '&', '');
  OpenDialog.InitialDir := Path;
  OpenDialog.FileName := '';
  if (ActiveDBGrid.SelectedField.DataType = ftWideMemo) then
  begin
    OpenDialog.DefaultExt := 'txt';
    OpenDialog.Filter := FilterDescription('txt') + ' (*.txt)|*.txt|' + FilterDescription('*') + ' (*.*)|*.*';
    OpenDialog.Encodings.Text := EncodingCaptions();
    OpenDialog.EncodingIndex := OpenDialog.Encodings.IndexOf(CodePageToEncoding(Client.CodePage));
  end
  else if (ActiveDBGrid.SelectedField.DataType = ftBlob) then
  begin
    OpenDialog.DefaultExt := '';
    OpenDialog.Filter := FilterDescription('*') + ' (*.*)|*.*';
    OpenDialog.Encodings.Clear();
    OpenDialog.EncodingIndex := -1;
  end;

  if (OpenDialog.Execute()) then
  begin
    Path := ExtractFilePath(OpenDialog.FileName);

    ActiveDBGrid.SelectedField.DataSet.Edit();
    if (ActiveDBGrid.SelectedField.DataType = ftWideMemo) then
    begin
      case (EncodingToCodePage(OpenDialog.Encodings[OpenDialog.EncodingIndex])) of
        CP_UTF8: Encoding := TUTF8Encoding.Create();
        CP_UNICODE: Encoding := TUnicodeEncoding.Create();
        else Encoding := TMBCSEncoding.Create(EncodingToCodePage(OpenDialog.Encodings[OpenDialog.EncodingIndex]))
      end;
      try
        Reader := TStreamReader.Create(OpenDialog.Filename, Encoding, True);
        try
          TMySQLWideMemoField(ActiveDBGrid.SelectedField).LoadFromStream(Reader.BaseStream);
        finally
          Reader.Free();
        end;
      except
        on E: EFileStreamError do
          MsgBox(SysErrorMessage(GetLastError), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
      end;
      Encoding.Free();
    end
    else if (ActiveDBGrid.SelectedField.DataType = ftBlob) then
      TBlobField(ActiveDBGrid.SelectedField).LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure TFClient.aERedoExecute(Sender: TObject);
begin
  FSQLEditorSynMemo.Redo();
  SynMemoStatusChange(Sender, [scAll]);
end;

procedure TFClient.aERenameExecute(Sender: TObject);
begin
  if (Window.ActiveControl = FNavigator) then
    FNavigatorMenuNode.EditText()
  else if (Window.ActiveControl = ActiveListView) then
    ActiveListView.Selected.EditCaption();
end;

procedure TFClient.aEReplaceExecute(Sender: TObject);
var
  I: Integer;
begin
  Wanted.Clear();

  DSearch.Client := Client;
  if ((SelectedImageIndex in [iiBaseTable, iiSystemView, iiView]) and (Window.ActiveControl = ActiveListView)) then
  begin
    DSearch.DatabaseName := FNavigator.Selected.Parent.Text;
    DSearch.TableName := FNavigator.Selected.Text;
    DSearch.FieldName := '';
    for I := 0 to ActiveListView.Items.Count - 1 do
      if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex in [iiField, iiSystemViewField, iiViewField])) then
      begin
        if (DSearch.FieldName <> '') then
          DSearch.FieldName := DSearch.FieldName + ',';
        DSearch.FieldName := DSearch.FieldName + ActiveListView.Items[I].Caption;
      end;
  end
  else
  begin
    DSearch.DatabaseName := FocusedDatabaseNames;
    if (DSearch.DatabaseName = '') then
    begin
      DSearch.TableName := '';
      DSearch.FieldName := '';
    end
    else
    begin
      DSearch.DatabaseName := SelectedDatabase;
      DSearch.TableName := FocusedTableNames;
      DSearch.FieldName := '';
    end;
  end;
  DSearch.SearchOnly := False;
  DSearch.Frame := Self;
  DSearch.Execute();
end;

procedure TFClient.aETransferExecute(Sender: TObject);
begin
  Wanted.Clear();

  DTransfer.SourceClient := Client;
  DTransfer.SourceDatabaseName := FocusedDatabaseNames;
  if (DTransfer.SourceDatabaseName = '') then
    DTransfer.SourceTableName := ''
  else
  begin
    DTransfer.SourceDatabaseName := SelectedDatabase;
    DTransfer.SourceTableName := FocusedTableNames;
  end;
  DTransfer.DestinationClient := nil;
  DTransfer.Execute();
end;

procedure TFClient.aFExportAccessExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etAccessFile);
end;

procedure TFClient.aFExportBitmapExecute(Sender: TObject);
begin
  Wanted.Clear();

  SaveDialog.Title := ReplaceStr(Preferences.LoadStr(582), '&', '');
  SaveDialog.InitialDir := Path;
  SaveDialog.FileName := SelectedDatabase + '.bmp';
  SaveDialog.DefaultExt := 'bmp';
  SaveDialog.Filter := FilterDescription('bmp') + ' (*.bmp)|*.bmp';
  SaveDialog.Encodings.Clear();
  SaveDialog.EncodingIndex := -1;
  if (SaveDialog.Execute()) then
  begin
    ActiveWorkbench.SaveToBMP(SaveDialog.FileName);

    Path := ExtractFilePath(SaveDialog.FileName);
  end;
end;

procedure TFClient.aFExportExcelExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etExcelFile);
end;

procedure TFClient.aFExportExecute(const Sender: TObject; const ExportType: TExportType);
var
  CodePage: Cardinal;
  Database: TCDatabase;
  FolderName: string;
  I: Integer;
  Updated: Boolean;
  J: Integer;
  Table: TCBaseTable;
  TableNames: array of string;
begin
  Database := nil;
  DExport.Client := Client;
  DExport.DBGrid := nil;
  DExport.DBObjects.Clear();
  DExport.ExportType := ExportType;
  DExport.Window := Window;

  if (Window.ActiveControl = ActiveDBGrid) then
    DExport.DBGrid := ActiveDBGrid
  else if (Window.ActiveControl = ActiveWorkbench) then
  begin
    Database := TCDatabase(FNavigator.Selected.Data);
    if (not Database.Tables.Update()) then
      Wanted.Action := TAction(Sender)
    else
      for I := 0 to ActiveWorkbench.Tables.Count - 1 do
        if (not Assigned(ActiveWorkbench.Selected) or ActiveWorkbench.Tables[I].Selected) then
          DExport.DBObjects.Add(ActiveWorkbench.Tables[I].BaseTable);
  end
  else if ((Window.ActiveControl = ActiveListView) and (ActiveListView.SelCount > 0)) then
  begin
    case (SelectedImageIndex) of
      iiServer:
        for I := 0 to ActiveListView.Items.Count - 1 do
          if (ActiveListView.Items[I].Selected) then
          begin
            Database := TCDatabase(ActiveListView.Items[I].Data);
            if (not Database.Update()) then
              Wanted.Action := TAction(Sender)
            else if ((Client.TableNameCmp(Database.Name, 'mysql') <> 0) and not (Database is TCSystemDatabase)) then
            begin
              for J := 0 to Database.Tables.Count - 1 do
                DExport.DBObjects.Add(Database.Tables[J]);
              if (Assigned(Database.Routines)) then
                for J := 0 to Database.Routines.Count - 1 do
                  DExport.DBObjects.Add(Database.Routines[J]);
              if (Assigned(Database.Triggers)) then
                for J := 0 to Database.Triggers.Count - 1 do
                  DExport.DBObjects.Add(Database.Triggers[J]);
            end;
          end;

      iiDatabase:
        begin
          Database := TCDatabase(FNavigator.Selected.Data);
          if (not Database.Update()) then
            Wanted.Action := TAction(Sender)
          else if ((Client.TableNameCmp(Database.Name, 'mysql') <> 0) and not (Database is TCSystemDatabase)) then
          begin
            SetLength(TableNames, 0);
            for I := 0 to ActiveListView.Items.Count - 1 do
              if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex in [iiBaseTable, iiView])) then
              begin
                SetLength(TableNames, Length(TableNames) + 1);
                TableNames[Length(TableNames) - 1] := ActiveListView.Items[I].Caption;
              end;

            if ((Sender is TAction) and not Database.Update()) then
              Wanted.Action := TAction(Sender)
            else
              for I := 0 to ActiveListView.Items.Count - 1 do
                if (ActiveListView.Items[I].Selected) then
                  if (ActiveListView.Items[I].ImageIndex in [iiBaseTable, iiView]) then
                  begin
                    DExport.DBObjects.Add(TCTable(ActiveListView.Items[I].Data));

                    if (Assigned(Database.Triggers)) then
                      for J := 0 to Database.Triggers.Count - 1 do
                        DExport.DBObjects.Add(Database.Triggers[J]);
                  end
                  else if (ActiveListView.Items[I].ImageIndex in [iiProcedure, iiFunction, iiEvent, iiTrigger]) then
                    DExport.DBObjects.Add(TCDBObject(ActiveListView.Items[I].Data));

            SetLength(TableNames, 0);
          end;
        end;
      iiBaseTable:
        begin
          Database := TCDatabase(FNavigator.Selected.Parent.Data);
          if (not Database.Triggers.Update()) then
            Wanted.Action := TAction(Sender)
          else if ((Client.TableNameCmp(Database.Name, 'mysql') <> 0) and not (Database is TCSystemDatabase)) then
            for I := 0 to ActiveListView.Items.Count - 1 do
              if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex = iiTrigger)) then
                DExport.DBObjects.Add(TCDBObject(ActiveListView.Items[I].Data));
        end;
    end
  end
  else if (FocusedCItem is TCDatabase) then
  begin
    Database := TCDatabase(FocusedCItem);
    if (not (Database is TCSystemDatabase)) then
      if (not Database.Update()) then
        Wanted.Action := TAction(Sender)
      else
      begin
        for J := 0 to Database.Tables.Count - 1 do
          DExport.DBObjects.Add(Database.Tables[J]);
        if (Assigned(Database.Routines)) then
          for J := 0 to Database.Routines.Count - 1 do
            DExport.DBObjects.Add(Database.Routines[J]);
        if (Assigned(Database.Triggers)) then
          for J := 0 to Database.Triggers.Count - 1 do
            DExport.DBObjects.Add(Database.Triggers[J]);
      end;
  end
  else if (FocusedCItem is TCBaseTable) then
  begin
    Table := TCBaseTable(FocusedCItem);
    Database := Table.Database;
    if (not Table.Update() or Assigned(Database.Triggers) and not Database.Triggers.Update()) then
      Wanted.Action := TAction(Sender)
    else
    begin
      DExport.DBObjects.Add(Table);

      if (Assigned(Database.Triggers)) then
        for J := 0 to Database.Triggers.Count - 1 do
          if (Database.Triggers[J].Table = Table) then
            DExport.DBObjects.Add(Database.Triggers[J]);
    end;
  end
  else if (FocusedCItem is TCDBObject) then
  begin
    Database := TCDBObject(FocusedCItem).Database;
    if (not Database.Update()) then
      Wanted.Action := TAction(Sender)
    else
      DExport.DBObjects.Add(FocusedCItem);
  end
  else if (Assigned(FocusedCItem) and (FocusedCItem is TCDBObject)) then
  begin
    if (not TCDBObject(FocusedCItem).Update()) then
      Wanted.Action := TAction(Sender)
    else
      DExport.DBObjects.Add(FocusedCItem);
  end
  else
  begin
    Updated := False;
    for I := 0 to DExport.Client.Databases.Count - 1 do
      if ((Client.TableNameCmp(Client.Databases[I].Name, 'mysql') <> 0) and not (Client.Databases[I] is TCSystemDatabase)) then
        Updated := Updated or not Client.Databases[I].Update();

    if (Updated and (Sender is TAction)) then
      Wanted.Action := TAction(Sender)
    else
      for I := 0 to DExport.Client.Databases.Count - 1 do
        if ((Client.TableNameCmp(Client.Databases[I].Name, 'mysql') <> 0) and not (Client.Databases[I] is TCSystemDatabase)) then
        begin
          for J := 0 to Client.Databases[I].Tables.Count - 1 do
            DExport.DBObjects.Add(Client.Databases[I].Tables[J]);
          if (Assigned(Client.Databases[I].Routines)) then
            for J := 0 to Client.Databases[I].Routines.Count - 1 do
              DExport.DBObjects.Add(Client.Databases[I].Routines[J]);
          if (Assigned(Client.Databases[I].Triggers)) then
            for J := 0 to Client.Databases[I].Triggers.Count - 1 do
              DExport.DBObjects.Add(Client.Databases[I].Triggers[J]);
        end;
  end;

  if (Assigned(DExport.DBGrid) or (DExport.DBObjects.Count >= 1)) then
  begin
    if (Assigned(Client) and (Client.Account.Connection.Charset <> '')) then
      CodePage := Client.CharsetToCodePage(Client.Account.Connection.Charset)
    else if ((DExport.DBObjects.Count = 1) and (TObject(DExport.DBObjects[0]) is TCBaseTable)) then
      CodePage := Client.CharsetToCodePage(TCBaseTable(DExport.DBObjects[0]).DefaultCharset)
    else if (Assigned(Database)) then
      CodePage := Client.CharsetToCodePage(Database.DefaultCharset)
    else
      CodePage := Client.CodePage;

    if (ExportType = etPrint) then
    begin
      DExport.ExportType := etPrint;
      DExport.Execute();
    end
    else if (ExportType = etODBC) then
    begin
      DExport.Execute();
    end
    else
    begin
      SaveDialog.Title := ReplaceStr(Preferences.LoadStr(582), '&', '');
      SaveDialog.InitialDir := Path;
      SaveDialog.Filter := '';
      case (ExportType) of
        etSQLFile:
          begin
            SaveDialog.Filter := FilterDescription('sql') + ' (*.sql)|*.sql';
            SaveDialog.DefaultExt := 'sql';
            SaveDialog.Encodings.Text := EncodingCaptions();
          end;
        etTextFile:
          begin
            if (DExport.DBObjects.Count = 1) then
            begin
              SaveDialog.Filter := FilterDescription('txt') + ' (*.txt;*.csv;*.tab;*.asc)|*.txt;*.csv;*.tab;*.asc';
              SaveDialog.DefaultExt := 'csv';
              SaveDialog.Encodings.Text := EncodingCaptions();
            end
            else
            begin
              SaveDialog.Filter := FilterDescription('zip') + ' (*.zip)|*.zip';
              SaveDialog.DefaultExt := 'zip';
              SaveDialog.Encodings.Clear();
            end;
          end;
        etExcelFile:
          begin
            SaveDialog.Filter := FilterDescription('xls') + ' (*.xls)|*.xls';
            SaveDialog.DefaultExt := 'xls';
            SaveDialog.Encodings.Clear();
          end;
        etAccessFile:
          begin
            SaveDialog.Filter := FilterDescription('mdb') + ' (*.mdb)|*.mdb';
            SaveDialog.DefaultExt := 'mdb';
            SaveDialog.Encodings.Clear();
          end;
        etSQLiteFile:
          begin
            SaveDialog.Filter := FilterDescription('sqlite') + ' (*.db3;*.sqlite)|*.db3;*.sqlite';
            SaveDialog.DefaultExt := 'db3';
            SaveDialog.Encodings.Clear();
          end;
        etHTMLFile:
          begin
            SaveDialog.Filter := FilterDescription('html') + ' (*.html;*.htm)|*.html;*.htm';
            SaveDialog.DefaultExt := 'html';
            SaveDialog.Encodings.Text := EncodingCaptions(True);
          end;
        etXMLFile:
          begin
            SaveDialog.Filter := FilterDescription('xml') + ' (*.xml)|*.xml';
            SaveDialog.DefaultExt := 'xml';
            SaveDialog.Encodings.Text := EncodingCaptions(True);
          end;
      end;
      SaveDialog.Filter := SaveDialog.Filter + '|' + FilterDescription('*') + ' (*.*)|*.*';

      if (Assigned(DExport.DBGrid)) then
        SaveDialog.FileName := Preferences.LoadStr(362) + '.' + SaveDialog.DefaultExt
      else if (not Assigned(Database)) then
        SaveDialog.FileName := TCDBObject(DExport.DBObjects[0]).Database.Client.Account.Name + '.' + SaveDialog.DefaultExt
      else if (DExport.DBObjects.Count = 1) then
        SaveDialog.FileName := TCDBObject(DExport.DBObjects[0]).Name + '.' + SaveDialog.DefaultExt
      else
        SaveDialog.FileName := Database.Name + '.' + SaveDialog.DefaultExt;

      if (SaveDialog.Encodings.Count = 0) then
        SaveDialog.EncodingIndex := -1
      else
        SaveDialog.EncodingIndex := SaveDialog.Encodings.IndexOf(CodePageToEncoding(CodePage));

      if (SaveDialog.Execute()) then
      begin
        Path := ExtractFilePath(SaveDialog.FileName);

        if (SaveDialog.EncodingIndex < 0) then
          DExport.CodePage := CP_ACP
        else
          DExport.CodePage := EncodingToCodePage(SaveDialog.Encodings[SaveDialog.EncodingIndex]);
        DExport.Filename := SaveDialog.FileName;
        DExport.Execute();
      end;
    end;
  end;
end;

procedure TFClient.aFExportHTMLExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etHTMLFile);
end;

procedure TFClient.aFExportODBCExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etODBC);
end;

procedure TFClient.aFExportSQLExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etSQLFile);
end;

procedure TFClient.aFExportSQLiteExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etSQLiteFile);
end;

procedure TFClient.aFExportTextExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etTextFile);
end;

procedure TFClient.aFExportXMLExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etXMLFile);
end;

procedure TFClient.aFImportAccessExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFImportExecute(Sender, itAccessFile);
end;

procedure TFClient.aFImportExcelExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFImportExecute(Sender, itExcelFile);
end;

procedure TFClient.aFImportExecute(const Sender: TObject; const ImportType: TImportType);
var
  CItem: TCItem;
begin
  CItem := FocusedCItem;

  if ((Sender is TAction) and ((CItem is TCDatabase) and TCDatabase(CItem).Update()) or ((CItem is TCTable) and TCTable(CItem).Update())) then
    Wanted.Action := TAction(Sender)
  else
  begin
    DImport.Client := Client;
    DImport.Database := nil;
    DImport.Table := nil;
    if (CItem is TCDatabase) then
      DImport.Database := TCDatabase(CItem)
    else if (CItem is TCBaseTable) then
    begin
      DImport.Database := TCBaseTable(CItem).Database;
      DImport.Table := TCBaseTable(CItem);
    end;

    OpenDialog.Title := ReplaceStr(Preferences.LoadStr(581), '&', '');
    OpenDialog.InitialDir := Path;
    OpenDialog.Filter := '';
    case (ImportType) of
      itSQLFile:
        begin
          OpenDialog.Filter := FilterDescription('sql') + ' (*.sql)|*.sql';
          OpenDialog.DefaultExt := 'sql';
          OpenDialog.Encodings.Text := EncodingCaptions();
        end;
      itTextFile:
        begin
          OpenDialog.Filter := FilterDescription('txt') + ' (*.txt;*.csv;*.tab;*.asc)|*.txt;*.csv;*.tab;*.asc';
          OpenDialog.DefaultExt := 'csv';
          OpenDialog.Encodings.Text := EncodingCaptions();
        end;
      itExcelFile:
        begin
          OpenDialog.Filter := FilterDescription('xls') + ' (*.xls)|*.xls';
          OpenDialog.DefaultExt := 'xls';
          OpenDialog.Encodings.Clear();
        end;
      itAccessFile:
        begin
          OpenDialog.Filter := FilterDescription('mdb') + ' (*.mdb)|*.mdb';
          OpenDialog.DefaultExt := 'mdb';
          OpenDialog.Encodings.Clear();
        end;
      itSQLiteFile:
        begin
          OpenDialog.Filter := FilterDescription('sqlite') + ' (*.db3;*.sqlite)|*.db3;*.sqlite';
          OpenDialog.DefaultExt := 'db3';
          OpenDialog.Encodings.Clear();
        end;
      itXMLFile:
        begin
          OpenDialog.Filter := FilterDescription('xml') + ' (*.xml)|*.xml';
          OpenDialog.DefaultExt := 'xml';
          OpenDialog.Encodings.Clear();
        end;
    end;
    OpenDialog.Filter := OpenDialog.Filter + '|' + FilterDescription('*') + ' (*.*)|*.*';

    OpenDialog.FileName := '';
    OpenDialog.EncodingIndex := OpenDialog.Encodings.IndexOf(CodePageToEncoding(CP_ACP));

    if (OpenDialog.Execute()) then
    begin
      Path := ExtractFilePath(OpenDialog.FileName);

      DImport.Window := Window;
      DImport.ImportType := ImportType;
      DImport.Filename := OpenDialog.FileName;
      if (OpenDialog.EncodingIndex < 0) then
        DImport.CodePage := CP_ACP
      else
        DImport.CodePage := EncodingToCodePage(OpenDialog.Encodings[OpenDialog.EncodingIndex]);
      DImport.Execute();
      Client.Update();
    end;
  end;
end;

procedure TFClient.aFImportODBCExecute(Sender: TObject);
begin
  Wanted.Clear();

  DImport.Window := Window;
  DImport.Client := Client;
  if (FocusedCItem is TCDatabase) then
  begin
    DImport.Database := TCDatabase(FocusedCItem);
    DImport.Table := nil;
  end
  else if (FocusedCItem is TCBaseTable) then
  begin
    DImport.Table := TCBaseTable(FocusedCItem);
    DImport.Database := DImport.Table.Database;
  end
  else
  begin
    DImport.Database := nil;
    DImport.Table := nil;
  end;
  DImport.Filename := '';
  DImport.CodePage := CP_ACP;
  DImport.ImportType := itODBC;
  DImport.Execute();
  Client.Update();
end;

procedure TFClient.aFImportSQLExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFImportExecute(Sender, itSQLFile);
end;

procedure TFClient.aFImportSQLiteExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFImportExecute(Sender, itSQLiteFile);
end;

procedure TFClient.aFImportTextExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFImportExecute(Sender, itTextFile);
end;

procedure TFClient.aFImportXMLExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFImportExecute(Sender, itXMLFile);
end;

procedure TFClient.aFOpenExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (Boolean(Perform(CM_CLOSE_TAB_QUERY, 0, 0))) then
    OpenSQLFile('');
end;

procedure TFClient.aFPrintExecute(Sender: TObject);
begin
  Wanted.Clear();

  if ((Window.ActiveControl = FSQLEditorSynMemo) or (Window.ActiveControl = FLog)) then
    SynMemoPrintExecute(Sender)
  else if (Window.ActiveControl = ActiveWorkbench) then
    WorkbenchPrintExecute(Sender)
  else
    aFExportExecute(Sender, etPrint);
end;

procedure TFClient.aFSaveAsExecute(Sender: TObject);
begin
  Wanted.Clear();

  SaveSQLFile(Sender);
end;

procedure TFClient.aFSaveExecute(Sender: TObject);
begin
  Wanted.Clear();

  SaveSQLFile(Sender);
end;

procedure TFClient.AfterConnect(Sender: TObject);
begin
  MainAction('aDCancel').Enabled := False;

  StatusBar.Panels[sbMessage].Text := Preferences.LoadStr(382);
  SetTimer(Handle, tiStatusBar, 5000, nil);
end;

procedure TFClient.AfterExecuteSQL(Sender: TObject);
var
  Hour: Word;
  Minute: Word;
  MSec: Word;
  Msg: string;
  Second: Word;
begin
  MainAction('aDCancel').Enabled := False;

  aDCommitRefresh(Client);     // Maybe we're still in a database transaction...

  if (Client.RowsAffected < 0) then
    Msg := Preferences.LoadStr(382)
  else
    Msg := Preferences.LoadStr(658, IntToStr(Client.RowsAffected));

  DecodeTime(Client.ExecutionTime, Hour, Minute, Second, MSec);
  if (Hour > 0) or (Minute > 0) then
    Msg := Msg + '  (' + Preferences.LoadStr(520) + ': ' + FormatDateTime(FormatSettings.LongTimeFormat, Client.ExecutionTime) + ')'
  else if (Client.ExecutionTime >= 0) then
    Msg := Msg + '  (' + Preferences.LoadStr(520) + ': ' + Format('%2.2f', [Second + MSec / 1000]) + ')';

  StatusBar.Panels[sbMessage].Text := Msg;
  SetTimer(Handle, tiStatusBar, 5000, nil);

  if (not Wanted.Nothing) then
    Wanted.Execute();
end;

procedure TFClient.aHManualExecute(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', PChar(Client.Account.ManualURL), '', '', SW_SHOW);
end;

procedure TFClient.aHRunClick(Sender: TObject);
var
  SQL: string;
begin
  if (Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock])) then
  begin
    Wanted.Clear();

    View := vEditor;
    if (View = vEditor) then
    begin
      SQL := '';

      if ((XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'database').Text <> Client.DatabaseName) and (XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'database').Text <> '')) then
        SQL := SQL + Client.SQLUse(XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'database').Text);

      SQL := SQL + XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'sql').Text;

      SendQuery(Sender, SQL);
    end;
  end;
end;

procedure TFClient.aHRunExecute(Sender: TObject);
var
  SQL: string;
begin
  Wanted.Clear();

  if (Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock])) then
  begin
    View := vEditor;
    if (View = vEditor) then
    begin
      SQL := '';

      if ((XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'database').Text <> Client.DatabaseName) and (XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'database').Text <> '')) then
        SQL := SQL + Client.SQLUse(XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'database').Text);

      SQL := SQL + XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'sql').Text;

      SendQuery(Sender, SQL);
    end;
  end;
end;

procedure TFClient.aHSQLExecute(Sender: TObject);

  function FLogWordAtCursor(): string;
  var
    Index: Integer;
    Len: Integer;
    Line: string;
    LineIndex: Integer;
  begin
    LineIndex := FLog.Perform(EM_EXLINEFROMCHAR, 0, FLog.SelStart);
    Index := FLog.SelStart - FLog.Perform(EM_LINEINDEX, LineIndex, 0);
    Line := FLog.Lines[LineIndex];
    while ((Index > 1) and not CharInSet(Line[Index - 1], [#9, #10, #13, ' ', '"', '`', '.', ',', ')', ']', '=', ';'])) do Dec(Index);
    Len := 0;
    while ((Len <= Length(Line) - Index) and not CharInSet(Line[Index + Len], [#9, #10, #13, ' ', '"', '`', '.', ',', '(', '[', '=', ';'])) do Inc(Len);
    Result := copy(Line, Index, Len);
  end;

begin
  if (Window.ActiveControl = FLog) then
  begin
    if (FLog.SelText <> '') then
      DSQLHelp.Keyword := FLog.SelText
    else
      DSQLHelp.Keyword := FLogWordAtCursor()
  end
  else if (Assigned(ActiveSynMemo) and (Window.ActiveControl = ActiveSynMemo)) then
    if (ActiveSynMemo.SelText <> '') then
      DSQLHelp.Keyword := ActiveSynMemo.SelText
    else if (ActiveSynMemo.WordAtCursor <> '') then
      DSQLHelp.Keyword := ActiveSynMemo.WordAtCursor
    else
      DSQLHelp.Keyword := ActiveSynMemo.Text
  else
    DSQLHelp.Keyword := '';

  DSQLHelp.Client := Client;
  DSQLHelp.Execute();
end;

procedure TFClient.aPCollapseExecute(Sender: TObject);
begin
  if (Window.ActiveControl = FNavigator) then
    FNavigatorMenuNode.Collapse(False)
  else if (Window.ActiveControl = FSQLHistory) then
    FSQLHistoryMenuNode.Collapse(False);
end;

procedure TFClient.aPExpandExecute(Sender: TObject);
begin
  if (Window.ActiveControl = FNavigator) then
    FNavigatorMenuNode.Expand(False)
  else if (Window.ActiveControl = FSQLHistory) then
    FSQLHistoryMenuNode.Expand(False);
end;

procedure TFClient.aPObjectBrowserTableExecute(Sender: TObject);
var
  URI: TUURI;
begin
  Wanted.Clear();

  URI := TUURI.Create(Address);
  URI.Param['view'] := Null;
  if (URI.Database <> '') then
    URI.Table := LastSelectedTable;
  Address := URI.Address;
  URI.Free();
end;

procedure TFClient.aPOpenInNewTabExecute(Sender: TObject);
begin
  if (FocusedCItem is TCDatabase) then
    OpenInNewTabExecute(TCDatabase(FocusedCItem).Name, '')
  else if (FocusedCItem is TCTable) then
    OpenInNewTabExecute(TCTable(FocusedCItem).Database.Name, TCTable(FocusedCItem).Name)
  else if (Window.ActiveControl = FBookmarks) then
  begin
    if (Assigned(FBookmarks.Selected)) then
      Window.Perform(CM_ADDTAB, 0, LPARAM(PChar(string(Client.Account.Desktop.Bookmarks.ByCaption(FBookmarks.Selected.Caption).URI))));
  end
  else if (Window.ActiveControl = FFiles) then
    if (LowerCase(ExtractFileExt(Path + PathDelim + FFiles.Selected.Caption)) = '.sql') then
      OpenInNewTabExecute(SelectedDatabase, '', False, Path + PathDelim + FFiles.Selected.Caption);
end;

procedure TFClient.aPOpenInNewWindowExecute(Sender: TObject);
begin
  if (FocusedCItem is TCDatabase) then
    OpenInNewTabExecute(TCDatabase(FocusedCItem).Name, '', True)
  else if (FocusedCItem is TCTable) then
    OpenInNewTabExecute(TCTable(FocusedCItem).Database.Name, TCTable(FocusedCItem).Name, True)
  else if (Window.ActiveControl = FBookmarks) then
  begin
    if (Assigned(FBookmarks.Selected)) then
      ShellExecute(Application.Handle, 'open', PChar(TFileName(Application.ExeName)), PChar(string(Client.Account.Desktop.Bookmarks.ByCaption(FBookmarks.Selected.Caption).URI)), '', SW_SHOW);
  end
  else if (Window.ActiveControl = FFiles) then
    if (LowerCase(ExtractFileExt(Path + PathDelim + FFiles.Selected.Caption)) = '.sql') then
      OpenInNewTabExecute(SelectedDatabase, '', True, Path + PathDelim + FFiles.Selected.Caption);
end;

procedure TFClient.aPResultExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (PResult.Visible and Assigned(ActiveDBGrid)) then
    Window.ActiveControl := ActiveDBGrid;
end;

procedure TFClient.aSSearchFindNotFound(Sender: TObject);
begin
  MsgBox(Preferences.LoadStr(533, TSearchFind(Sender).Dialog.FindText), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);
end;

procedure TFClient.aTBFilterExecute(Sender: TObject);
begin
  if (View = vBrowser) then
    Window.ActiveControl := FFilter;
end;

procedure TFClient.aTBLimitExecute(Sender: TObject);
begin
  if (View = vBrowser) then
    Window.ActiveControl := FLimit;
end;

procedure TFClient.aTBOffsetExecute(Sender: TObject);
begin
  if (View = vBrowser) then
    Window.ActiveControl := FOffset;
end;

procedure TFClient.aTBQuickSearchExecute(Sender: TObject);
begin
  if (View = vBrowser) then
    Window.ActiveControl := FQuickSearch;
end;

procedure TFClient.aVBlobExecute(Sender: TObject);
begin
  Wanted.Clear();

  FHTMLHide(Sender);

  FText.Visible := (Sender = aVBlobText) or not Assigned(Sender) and aVBlobText.Checked;
  FRTF.Visible := (Sender = aVBlobRTF) or not Assigned(Sender) and aVBlobRTF.Checked;
  if (Assigned(FHTML)) then
    FHTML.Visible := (Sender = aVBlobHTML) or not Assigned(Sender) and aVBlobHTML.Checked;
  FImage.Visible := (Sender = aVBlobImage) or not Assigned(Sender) and aVBlobImage.Checked;
  FHexEditor.Visible := (Sender = aVBlobHexEditor) or not Assigned(Sender) and aVBlobHexEditor.Checked;
  FBlobSearch.Visible := aVBlobText.Checked;
  ToolBarBlobResize(Sender);

  if (FText.Visible) then
    FTextShow(Sender)
  else if (FRTF.Visible) then
    FRTFShow(Sender)
  else if ((Sender = aVBlobHTML) or not Assigned(Sender) and aVBlobHTML.Checked) then
    FHTMLShow(Sender)
  else if (FImage.Visible) then
    FImageShow(Sender)
  else if (FHexEditor.Visible) then
    FHexEditorShow(Sender);

  if (CheckWin32Version(6)) then
    SendMessage(FBlobSearch.Handle, EM_SETCUEBANNER, 0, LParam(PChar(ReplaceStr(Preferences.LoadStr(424), '&', ''))));
end;

procedure TFClient.aVDetailsExecute(Sender: TObject);
begin
  Wanted.Clear();

  DColumns.DBGrid := ActiveDBGrid;
  if (DColumns.Execute()) then
  begin
    Window.ActiveControl := nil;
    Window.ActiveControl := ActiveDBGrid;
  end;
end;

procedure TFClient.aViewExecute(Sender: TObject);
var
  NewView: TView;
  AllowChange: Boolean;
begin
  if (Sender = MainAction('aVObjectBrowser')) then
    NewView := vObjects
  else if (Sender = MainAction('aVDataBrowser')) then
    NewView := vBrowser
  else if (Sender = MainAction('aVObjectIDE')) then
    NewView := vIDE
  else if (Sender = MainAction('aVQueryBuilder')) then
    NewView := vBuilder
  else if (Sender = MainAction('aVSQLEditor')) then
    NewView := vEditor
  else if (Sender = MainAction('aVDiagram')) then
    NewView := vDiagram
  else
    NewView := View;

  AllowChange := True;
  if (AllowChange and Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and ActiveDBGrid.DataSource.DataSet.Active) then
    try
      if ((Window.ActiveControl = FText) or (Window.ActiveControl = FRTF) or (Window.ActiveControl = FHexEditor)) then
        Window.ActiveControl := ActiveDBGrid;
      ActiveDBGrid.DataSource.DataSet.CheckBrowseMode();
    except
      AllowChange := False;
    end;

  tbObjects.Down := MainAction('aVObjectBrowser').Checked;
  tbBrowser.Down := MainAction('aVDataBrowser').Checked;
  tbIDE.Down := MainAction('aVObjectIDE').Checked;
  tbBuilder.Down := MainAction('aVQueryBuilder').Checked;
  tbEditor.Down := MainAction('aVSQLEditor').Checked;
  tbDiagram.Down := MainAction('aVDiagram').Checked;

  if (AllowChange) then
  begin
    View := NewView;

    case (View) of
      vObjects: if (PListView.Visible) then Window.ActiveControl := ActiveListView;
      vBrowser: if (PResult.Visible and Assigned(ActiveDBGrid)) then Window.ActiveControl := ActiveDBGrid;
      vIDE: if (PSynMemo.Visible and Assigned(ActiveSynMemo)) then Window.ActiveControl := ActiveSynMemo;
      vBuilder: if (PBuilder.Visible and Assigned(FBuilderActiveWorkArea())) then Window.ActiveControl := FBuilderActiveWorkArea();
      vEditor: if (PSynMemo.Visible) then Window.ActiveControl := FSQLEditorSynMemo;
      vDiagram: if (PWorkbench.Visible) then Window.ActiveControl := ActiveWorkbench;
    end;
  end;
end;

procedure TFClient.aVRefreshAllExecute(Sender: TObject);
var
  ChangeEvent: TTVChangedEvent;
  ChangingEvent: TTVChangingEvent;
  TempAddress: string;
begin
  KillTimer(Handle, tiStatusBar);
  KillTimer(Handle, tiNavigator);

  Client.Invalidate();

  TempAddress := Address;

  ChangingEvent := FNavigator.OnChanging; FNavigator.OnChanging := nil;
  ChangeEvent := FNavigator.OnChange; FNavigator.OnChange := nil;
  FNavigator.Selected := nil;
  FNavigator.OnChanging := ChangingEvent;
  FNavigator.OnChange := ChangeEvent;

  Address := TempAddress;
end;

procedure TFClient.aVRefreshExecute(Sender: TObject);
var
  AllowRefresh: Boolean;
  List: TList;
begin
  if (GetKeyState(VK_SHIFT) < 0) then
    aVRefreshAllExecute(Sender)
  else
  begin
    KillTimer(Handle, tiNavigator);

    case (View) of
      vObjects,
      vIDE:
        begin
          case (SelectedImageIndex) of
            iiServer: Client.Invalidate();
            iiDatabase,
            iiSystemDatabase: TCDatabase(FNavigator.Selected.Data).Invalidate();
            iiBaseTable,
            iiSystemView:
              begin
                TCTable(FNavigator.Selected.Data).Invalidate();
                if (Assigned(TCDatabase(FNavigator.Selected.Parent.Data).Triggers)) then
                  TCDatabase(FNavigator.Selected.Parent.Data).Triggers.Invalidate();
              end;
            iiView: TCView(FNavigator.Selected.Data).Invalidate();
            iiProcedure: TCProcedure(FNavigator.Selected.Data).Invalidate();
            iiFunction: TCFunction(FNavigator.Selected.Data).Invalidate();
            iiEvent: TCEvent(FNavigator.Selected.Data).Invalidate();
            iiTrigger: TCTrigger(FNavigator.Selected.Data).Invalidate();
            iiHosts: Client.Hosts.Invalidate();
            iiProcesses: Client.Processes.Invalidate();
            iiStati: Client.Stati.Invalidate();
            iiUsers: Client.Users.Invalidate();
            iiVariables: Client.Variables.Invalidate();
          end;
          Address := Address;
        end;
      vBrowser,
      vBuilder,
      vEditor:
        if ((PResult.Visible or (View = vBrowser)) and (ActiveDBGrid.DataSource.DataSet is TMySQLDataSet) and (TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).CommandText <> '')) then
        begin
          AllowRefresh := True;

          if (ActiveDBGrid.DataSource.DataSet.Active) then
          begin
            ActiveDBGrid.EditorMode := False;
            try
              ActiveDBGrid.DataSource.DataSet.CheckBrowseMode();
            except
              AllowRefresh := False;
            end;
          end;

          if (AllowRefresh) then
            if ((View = vBrowser) and (FNavigator.Selected.ImageIndex in [iiBaseTable, iiSystemView, iiView]) and not TCTable(FNavigator.Selected.Data).Valid) then
            begin
              ActiveDBGrid.DataSource.DataSet.Close();
              Client.Update();
            end
            else
              ActiveDBGrid.DataSource.DataSet.Refresh();
        end;
      vDiagram:
        begin
          TCDatabase(FNavigator.Selected.Data).Tables.Invalidate();
          List := TList.Create();
          List.Add(TCDatabase(FNavigator.Selected.Data).Tables);
          Client.Update(List);
          List.Free();
        end;
    end;
  end;
end;

procedure TFClient.aVSideBarExecute(Sender: TObject);
begin
  PSideBar.DisableAlign();

  MainAction('aVNavigator').Checked := (Sender = MainAction('aVNavigator')) and MainAction('aVNavigator').Checked;
  MainAction('aVBookmarks').Checked := (Sender = MainAction('aVBookmarks')) and MainAction('aVBookmarks').Checked;
  MainAction('aVExplorer').Checked := (Sender = MainAction('aVExplorer')) and MainAction('aVExplorer').Checked;
  MainAction('aVSQLHistory').Checked := (Sender = MainAction('aVSQLHistory')) and MainAction('aVSQLHistory').Checked;

  PNavigator.Visible := MainAction('aVNavigator').Checked;
  PBookmarks.Visible := MainAction('aVBookmarks').Checked;
  PExplorer.Visible := MainAction('aVExplorer').Checked;
  PSQLHistory.Visible := MainAction('aVSQLHistory').Checked;

  if (PExplorer.Visible and not Assigned(FFolders)) then
    CreateExplorer();

  SSideBar.Visible := PNavigator.Visible or PBookmarks.Visible or PExplorer.Visible or PSQLHistory.Visible;
  if (SSideBar.Visible) then
  begin
    SSideBar.Left := PNavigator.Width;
    SSideBar.Align := alLeft;
    PSideBar.Visible := SSideBar.Visible;
  end
  else
    PSideBar.Visible := False;
  PSideBarHeader.Visible := PSideBar.Visible; PSideBarResize(Sender);


  PSideBar.EnableAlign();

  if (MainAction('aVNavigator').Checked) then
    Window.ActiveControl := FNavigator
  else if (MainAction('aVBookmarks').Checked) then
    Window.ActiveControl := FBookmarks
  else if (MainAction('aVExplorer').Checked and FFolders.Visible) then
    Window.ActiveControl := FFolders
  else if (MainAction('aVSQLHistory').Checked) then
  begin
    FSQLHistoryRefresh(Sender);
    Window.ActiveControl := FSQLHistory;
  end;
end;

procedure TFClient.aVSortAscExecute(Sender: TObject);
var
  SortDef: TIndexDef;
begin
  Wanted.Clear();

  SortDef := TIndexDef.Create(nil, 'SortDef', ActiveDBGrid.SelectedField.FieldName, []);

  TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).Sort(SortDef);
  ActiveDBGrid.UpdateHeader();

  SortDef.Free();
end;

procedure TFClient.aVSortDescExecute(Sender: TObject);
var
  SortDef: TIndexDef;
begin
  Wanted.Clear();

  SortDef := TIndexDef.Create(nil, 'SortDef', ActiveDBGrid.SelectedField.FieldName, []);
  SortDef.DescFields := ActiveDBGrid.SelectedField.FieldName;

  TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).Sort(SortDef);
  ActiveDBGrid.UpdateHeader();

  SortDef.Free();
end;

procedure TFClient.aVSQLLogExecute(Sender: TObject);
begin
  PLog.Visible := MainAction('aVSQLLog').Checked;
  SLog.Visible := PLog.Visible;

  if (PLog.Visible) then
  begin
    // make sure, SLog is above PLog
    SLog.Align := alNone;
    SLog.Top := 0;
    SLog.Align := alBottom;

    Perform(CM_POST_MONITOR, 0, 0);
  end;

  FormResize(Sender);
end;

procedure TFClient.BeforeConnect(Sender: TObject);
begin
  StatusBar.Panels[sbMessage].Text := Preferences.LoadStr(195) + '...';
  KillTimer(Handle, tiStatusBar);

  MainAction('aDCancel').Enabled := True;
end;

procedure TFClient.BeforeExecuteSQL(Sender: TObject);
begin
  StatusBar.Panels[sbMessage].Text := Preferences.LoadStr(196) + '...';
  KillTimer(Handle, tiStatusBar);

  MainAction('aDCancel').Enabled := True;
end;

procedure TFClient.BeginEditLabel(Sender: TObject);
begin
  KillTimer(Handle, tiNavigator);

  aDCreate.ShortCut := 0;
  aDDelete.ShortCut := 0;
end;

procedure TFClient.BObjectIDEClick(Sender: TObject);
var
  SQL: string;
  Trigger: TCTrigger;
begin
  Wanted.Clear();

  Trigger := nil;
  if (TObject(FNavigator.Selected.Data) is TCTrigger) then
    Trigger := TCTrigger(FNavigator.Selected.Data);

  if (Assigned(Trigger) and (not ActiveSynMemo.Modified or PostObject(Sender))) then
  begin
    FObjectIDEGrid.EditorMode := False;
    if (Sender = BINSERT) then
      SQL := Trigger.SQLInsert()
    else if (Sender = BREPLACE) then
      SQL := Trigger.SQLReplace()
    else if (Sender = BUPDATE) then
      SQL := Trigger.SQLUpdate()
    else if (Sender = BDELETE) then
      SQL := Trigger.SQLDelete();

    if (SelectedDatabase <> Client.DatabaseName) then
      SQL := Client.SQLUse(SelectedDatabase) + SQL;

    Client.ExecuteSQL(SQL);
  end;
end;

procedure TFClient.ClientUpdate(const Event: TCClient.TEvent);
var
  Control: TWinControl;
  I: Integer;
  TempActiveControl: TWinControl;
begin
  LeftMousePressed := False;

  TempActiveControl := Window.ActiveControl;

  if (Assigned(Event)) then
  begin
    if (Event.EventType in [ceItemsValid, ceItemCreated, ceItemAltered, ceItemDropped]) then
      FNavigatorUpdate(Event);

    if (Event.EventType in [ceItemsValid, ceItemValid, ceItemCreated, ceItemAltered, ceItemDropped]) then
    begin
      if (Event.Sender is TCClient) then
        ListViewUpdate(Event, FServerListView);

      if (Event.Sender is TCDatabase) then
      begin
        ListViewUpdate(Event, FServerListView);
        if (not (Event.CItems is TCTriggers)) then
          ListViewUpdate(Event, Desktop(TCDatabase(Event.Sender)).ListView)
        else if (Event.EventType = ceItemDropped) then
          ListViewUpdate(Event, Desktop(TCTrigger(Event.CItem).Table).ListView)
        else
          for I := 0 to TCTriggers(Event.CItems).Count - 1 do
            ListViewUpdate(Event, Desktop(TCTriggers(Event.CItems)[I].Table).ListView);
        if (Assigned(Desktop(TCDatabase(Event.Sender)).Workbench)) then
          Desktop(TCDatabase(Event.Sender)).Workbench.ClientUpdate(Event);
      end
      else if (Event.Sender is TCTable) then
      begin
        ListViewUpdate(Event, Desktop(TCTable(Event.Sender).Database).ListView);
        ListViewUpdate(Event, Desktop(TCTable(Event.Sender)).ListView);
        if ((Event.Sender is TCBaseTable) and Assigned(Desktop(TCBaseTable(Event.Sender).Database).Workbench)) then
          Desktop(TCBaseTable(Event.Sender).Database).Workbench.ClientUpdate(Event);
      end
      else if (Event.CItems is TCHosts) then
        ListViewUpdate(Event, HostsListView)
      else if (Event.CItems is TCProcesses) then
        ListViewUpdate(Event, ProcessesListView)
      else if (Event.CItems is TCStati) then
        ListViewUpdate(Event, StatiListView)
      else if (Event.CItems is TCUsers) then
        ListViewUpdate(Event, UsersListView)
      else if (Event.CItems is TCVariables) then
        ListViewUpdate(Event, VariablesListView);
    end;

    if (Event.EventType in [ceItemValid]) then
      if ((Event.CItem is TCView) and Assigned(Desktop(TCView(Event.CItem)).SynMemo)) then
        Desktop(TCView(Event.CItem)).SynMemo.Text := Trim(SQLWrapStmt(TCView(Event.CItem).Stmt, ['from', 'where', 'group by', 'having', 'order by', 'limit', 'procedure'], 0)) + #13#10
      else if ((Event.CItem is TCRoutine) and Assigned(Desktop(TCRoutine(Event.CItem)).SynMemo)) then
      begin
        Desktop(TCRoutine(Event.CItem)).SynMemo.Text := TCRoutine(Event.CItem).Source + #13#10;
        PContentChange(nil);
      end;

    if ((Event.EventType = ceItemAltered) and (Event.CItem is TCTable)
      and Assigned(Desktop(TCTable(Event.CItem)).DBGrid)) then
      Desktop(TCTable(Event.CItem)).DBGrid.DataSource.DataSet.Close();
  end;

  if (PContent.Visible and Assigned(TempActiveControl) and TempActiveControl.Visible) then
  begin
    Control := TempActiveControl;
    while (Control.Visible and Control.Enabled and Assigned(Control.Parent)) do Control := Control.Parent;
    if (Control.Visible and Control.Enabled) then
      Window.ActiveControl := TempActiveControl;
  end;

  if (Assigned(Event) and ((Event.EventType in [ceItemCreated, ceItemAltered]) or ((Event.EventType in [ceItemValid]) and (Event.CItem is TCObject) and not TCObject(Event.CItem).Valid)) and (Screen.ActiveForm = Window) and Wanted.Nothing) then
    Wanted.Update := Client.Update;
end;

procedure TFClient.CMActivateDBGrid(var Message: TMessage);
begin
  Window.ActiveControl := TWinControl(Message.LParam);
  ActiveDBGrid.EditorMode := False;
end;

procedure TFClient.CMActivateFText(var Message: TMessage);
const
  KEYEVENTF_UNICODE = 4;
var
  Input: TInput;
begin
  Window.ActiveControl := FText;
  if (Message.WParam <> 0) then
  begin
    ZeroMemory(@Input, SizeOf(Input));
    Input.Itype := INPUT_KEYBOARD;
    Input.ki.wVk := Message.WParam;
    Input.ki.dwFlags := KEYEVENTF_UNICODE;
    SendInput(1, Input, SizeOf(Input));
  end;
  FText.SelStart := Length(FText.Text);
end;

procedure TFClient.CMChangePreferences(var Message: TMessage);
var
  I: Integer;
begin
  if (not CheckWin32Version(6) or TStyleManager.Enabled and (TStyleManager.ActiveStyle <> TStyleManager.SystemStyle)) then
  begin
    TBSideBar.BorderWidth := 0;
    ToolBar.BorderWidth := 0;
  end
  else
  begin
    TBSideBar.BorderWidth := 2;
    ToolBar.BorderWidth := 2;
  end;

  Client.SQLMonitor.CacheSize := Preferences.LogSize;
  if (Preferences.LogResult) then
    Client.SQLMonitor.TraceTypes := Client.SQLMonitor.TraceTypes + [ttInfo]
  else
    Client.SQLMonitor.TraceTypes := Client.SQLMonitor.TraceTypes - [ttInfo];
  if (Preferences.LogTime) then
    Client.SQLMonitor.TraceTypes := Client.SQLMonitor.TraceTypes + [ttTime]
  else
    Client.SQLMonitor.TraceTypes := Client.SQLMonitor.TraceTypes - [ttTime];

  aPExpand.Caption := Preferences.LoadStr(150);
  aPCollapse.Caption := Preferences.LoadStr(151);
  aPOpenInNewWindow.Caption := Preferences.LoadStr(760);
  aPOpenInNewTab.Caption := Preferences.LoadStr(850);
  aDDelete.Caption := Preferences.LoadStr(28);
  aDPrev.Caption := Preferences.LoadStr(512);
  aDNext.Caption := Preferences.LoadStr(513);
  DataSetFirst.Caption := Preferences.LoadStr(514);
  DataSetLast.Caption := Preferences.LoadStr(515);
  DataSetPost.Caption := Preferences.LoadStr(516);
  DataSetCancel.Caption := Preferences.LoadStr(517);
  aVBlobText.Caption := Preferences.LoadStr(379);
  aVBlobRTF.Caption := 'RTF';
  aVBlobHTML.Caption := 'HTML';
  aVBlobImage.Caption := Preferences.LoadStr(380);
  aVBlobHexEditor.Caption := Preferences.LoadStr(381);
  mwDCreateTable.Caption := MainAction('aDCreateTable').Caption;
  mwCreateSection.Caption := Preferences.LoadStr(877) + ' ...';
  mwCreateLink.Caption := Preferences.LoadStr(251) + ' ...';

  for I := 0 to ActionList.ActionCount - 1 do
    if (ActionList.Actions[I] is TCustomAction) and (TCustomAction(ActionList.Actions[I]).Hint = '') then
      TCustomAction(ActionList.Actions[I]).Hint := TCustomAction(ActionList.Actions[I]).Caption;

  tbObjects.Caption := ReplaceStr(Preferences.LoadStr(4), '&', '');
  tbBrowser.Caption := ReplaceStr(Preferences.LoadStr(5), '&', '');
  tbIDE.Caption := ReplaceStr(Preferences.LoadStr(865), '&', '');
  tbBuilder.Caption := ReplaceStr(tbBuilder.Caption, '&', '');
  tbEditor.Caption := ReplaceStr(Preferences.LoadStr(6), '&', '');
  tbDiagram.Caption := ReplaceStr(Preferences.LoadStr(800), '&', '');

  mfOpen.Caption := Preferences.LoadStr(581);
  mfOpenInNewWindow.Caption := Preferences.LoadStr(760);
  mfFilter.Caption := Preferences.LoadStr(209);
  mfFilterClear.Caption := FilterDescription('*') + ' (*.*)';
  mfFilterSQL.Caption := FilterDescription('sql') + ' (*.sql)';
  mfFilterText.Caption := FilterDescription('txt') + ' (*.txt,*.csv)';
  mfFilterHTML.Caption := FilterDescription('html') + ' (*.html,*.hmt)';
  mfFilterXML.Caption := FilterDescription('xml') + ' (*.xml)';
  mfFilterAccess.Caption := FilterDescription('mdb') + ' (*.mdb)';
  mfFilterExcel.Caption := FilterDescription('xls') + ' (*.xls)';
  mfDelete.Caption := Preferences.LoadStr(28);
  mfRename.Caption := Preferences.LoadStr(98);
  mfProperties.Caption := Preferences.LoadStr(97) + '...';

  miNImport.Caption := Preferences.LoadStr(371);
  miNExport.Caption := Preferences.LoadStr(200);
  miNCreate.Caption := Preferences.LoadStr(26);
  miNDelete.Caption := Preferences.LoadStr(28);

  mbBOpen.Caption := Preferences.LoadStr(581);

  miHOpen.Caption := Preferences.LoadStr(581);
  miHSaveAs.Caption := MainAction('aFSaveAs').Caption;
  miHStatementIntoSQLEditor.Caption := Preferences.LoadStr(198) + ' -> ' + Preferences.LoadStr(20);
  miHRun.Caption := MainAction('aDRun').Caption;
  miHProperties.Caption := Preferences.LoadStr(684) + '...';

  mlOpen.Caption := Preferences.LoadStr(581);
  mlFImport.Caption := Preferences.LoadStr(371);
  mlFExport.Caption := Preferences.LoadStr(200);
  mlDCreate.Caption := Preferences.LoadStr(26);
  mlDDelete.Caption := Preferences.LoadStr(28);

  mwFImport.Caption := Preferences.LoadStr(371);
  mwFExport.Caption := Preferences.LoadStr(200);
  mwAddTable.Caption := Preferences.LoadStr(383);
  mwEPaste.Caption := MainAction('aEPaste').Caption;
  mwDCreate.Caption := Preferences.LoadStr(26);
  mwDProperties.Caption := Preferences.LoadStr(97) + '...';

  gmFExport.Caption := Preferences.LoadStr(200);
  gmFilter.Caption := Preferences.LoadStr(209);

  mtObjects.Caption := tbObjects.Caption;
  mtBrowser.Caption := tbBrowser.Caption;
  mtIDE.Caption := tbIDE.Caption;
  mtBuilder.Caption := tbBuilder.Caption;
  mtEditor.Caption := tbEditor.Caption;
  mtDiagram.Caption := tbDiagram.Caption;

  tbObjects.Visible := ttObjects in Preferences.ToolbarTabs;
  tbBrowser.Visible := ttBrowser in Preferences.ToolbarTabs;
  tbIDE.Visible := ttIDE in Preferences.ToolbarTabs;
  tbBuilder.Visible := ttBuilder in Preferences.ToolbarTabs;
  tbEditor.Visible := ttEditor in Preferences.ToolbarTabs;
  tbDiagram.Visible := ttDiagram in Preferences.ToolbarTabs;


  if (not (tsLoading in FrameState)) then
    ClientUpdate(nil);

  SQLBuilder.RightMargin := Preferences.Editor.RightEdge;

  FOffset.Hint := ReplaceStr(Preferences.LoadStr(846), '&', '') + ' (' + ShortCutToText(aTBOffset.ShortCut) + ')';
  FUDOffset.Hint := ReplaceStr(Preferences.LoadStr(846), '&', '');
  FLimit.Hint := ReplaceStr(Preferences.LoadStr(197), '&', '') + ' (' + ShortCutToText(aTBLimit.ShortCut) + ')';
  FUDLimit.Hint := ReplaceStr(Preferences.LoadStr(846), '&', '');
  FLimitEnabled.Hint := ReplaceStr(Preferences.LoadStr(197), '&', '');
  FFilter.Hint := ReplaceStr(Preferences.LoadStr(209), '&', '') + ' (' + ShortCutToText(aTBFilter.ShortCut) + ')';
  FFilterEnabled.Hint := ReplaceStr(Preferences.LoadStr(209), '&', '');
  FQuickSearch.Hint := ReplaceStr(Preferences.LoadStr(424), '&', '') + ' (' + ShortCutToText(aTBQuickSearch.ShortCut) + ')';
  FQuickSearchEnabled.Hint := ReplaceStr(Preferences.LoadStr(424), '&', '');
  FBlobSearch.Hint := ReplaceStr(Preferences.LoadStr(424), '&', '');

  if (Preferences.Editor.LineNumbersBackground = clNone) then
    FSQLEditorSynMemo.Gutter.Color := clBtnFace
  else
    FSQLEditorSynMemo.Gutter.Color := Preferences.Editor.LineNumbersBackground;
  FSQLEditorSynMemo.Gutter.Font.Style := Preferences.Editor.LineNumbersStyle;
  FSQLEditorSynMemo.Gutter.Font.Size := FSQLEditorSynMemo.Font.Size;
  FSQLEditorSynMemo.Gutter.Font.Charset := FSQLEditorSynMemo.Font.Charset;
  FSQLEditorSynMemo.Gutter.Visible := Preferences.Editor.LineNumbers;
  FSQLEditorSynMemo.Options := FSQLEditorSynMemo.Options + [eoScrollHintFollows];  // Slow down the performance on large content
  if (Preferences.Editor.AutoIndent) then
    FSQLEditorSynMemo.Options := FSQLEditorSynMemo.Options + [eoAutoIndent, eoSmartTabs]
  else
    FSQLEditorSynMemo.Options := FSQLEditorSynMemo.Options - [eoAutoIndent, eoSmartTabs];
  if (Preferences.Editor.TabToSpaces) then
    FSQLEditorSynMemo.Options := FSQLEditorSynMemo.Options + [eoTabsToSpaces]
  else
    FSQLEditorSynMemo.Options := FSQLEditorSynMemo.Options - [eoTabsToSpaces];
  FSQLEditorSynMemo.TabWidth := Preferences.Editor.TabWidth;
  FSQLEditorSynMemo.RightEdge := Preferences.Editor.RightEdge;
  FSQLEditorSynMemo.WantTabs := Preferences.Editor.TabAccepted;
  if (not Preferences.Editor.CurrRowBGColorEnabled) then
    FSQLEditorSynMemo.ActiveLineColor := clNone
  else
    FSQLEditorSynMemo.ActiveLineColor := Preferences.Editor.CurrRowBGColor;

  for I := 0 to PSynMemo.ControlCount - 1 do
    if (PSynMemo.Controls[I] is TSynMemo) then
      SynMemoApllyPreferences(TSynMemo(PSynMemo.Controls[I]));

  SynMemoApllyPreferences(FBuilderSynMemo);

  FSQLEditorPrint.Font := FSQLEditorSynMemo.Font;

  FSQLEditorCompletion.Font.Name := FSQLEditorSynMemo.Font.Name;
  FSQLEditorCompletion.Font.Style := FSQLEditorSynMemo.Font.Style;
  FSQLEditorCompletion.Font.Color := FSQLEditorSynMemo.Font.Color;
  FSQLEditorCompletion.Font.Size := FSQLEditorSynMemo.Font.Size;
  FSQLEditorCompletion.Font.Charset := FSQLEditorSynMemo.Font.Charset;

  smEEmpty.Caption := Preferences.LoadStr(181);

  FNavigatorInitialize(nil);
  ListViewInitialize(FServerListView);

  BINSERT.Font := FSQLEditorSynMemo.Font;
  BINSERT.Font.Style := [fsBold];
  BREPLACE.Font := BINSERT.Font;
  BUPDATE.Font := BINSERT.Font;
  BDELETE.Font := BINSERT.Font;

  OpenDialog.EncodingLabel := Preferences.LoadStr(682) + ':';
  SaveDialog.EncodingLabel := Preferences.LoadStr(682) + ':';

  FBuilderSQLUpdated(nil);

  FLog.Font.Name := Preferences.LogFontName;
  FLog.Font.Style := Preferences.LogFontStyle;
  FLog.Font.Color := Preferences.LogFontColor;
  FLog.Font.Size := Preferences.LogFontSize;
  FLog.Font.Charset := Preferences.LogFontCharset;

  PasteMode := False;
end;

procedure TFClient.CMCloseTabQuery(var Message: TMessage);
var
  CanClose: Boolean;
  I: Integer;
  Msg: string;
  ServerNode: TTreeNode;
begin
  CanClose := True;

  if (CanClose and Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and ActiveDBGrid.DataSource.DataSet.Active) then
    ActiveDBGrid.DataSource.DataSet.CheckBrowseMode();

  if (CanClose) then
    if (FSQLEditorSynMemo.Modified and (SQLEditor.Filename <> '')) then
    begin
      if (SQLEditor.Filename = '') then
        Msg := Preferences.LoadStr(589)
      else
        Msg := Preferences.LoadStr(584, ExtractFileName(SQLEditor.Filename));
      View := vEditor;
      Window.ActiveControl := FSQLEditorSynMemo;
      case (MsgBox(Msg, Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION)) of
        IDYES: SaveSQLFile(MainAction('aFSave'));
        IDCANCEL: CanClose := False;
      end;
    end;

  ServerNode := FNavigator.Items.getFirstNode();
  if (Assigned(ServerNode)) then
    for I := 0 to ServerNode.Count - 1 do
      if (CanClose and (ServerNode.Item[I].ImageIndex = iiDatabase)) then
        Desktop(TCDatabase(ServerNode.Item[I].Data)).CloseQuery(nil, CanClose);

  if (not CanClose) then
    Message.Result := 0
  else
    Message.Result := 1;
end;

procedure TFClient.CMExecute(var Message: TMessage);
begin
  MainAction('aDRun').Execute();

  Window.Close();
end;

procedure TFClient.CMFrameActivate(var Message: TMessage);
begin
  Include(FrameState, tsActive);

  FormatSettings.ThousandSeparator := Client.FormatSettings.ThousandSeparator;
  FormatSettings.DecimalSeparator := Client.FormatSettings.DecimalSeparator;
  FormatSettings.ShortDateFormat := Client.FormatSettings.ShortDateFormat;
  FormatSettings.LongTimeFormat := Client.FormatSettings.LongTimeFormat;
  FormatSettings.DateSeparator := Client.FormatSettings.DateSeparator;
  FormatSettings.TimeSeparator := Client.FormatSettings.TimeSeparator;

  Client.BeforeConnect := BeforeConnect;
  Client.AfterConnect := AfterConnect;
  Client.OnConvertError := OnConvertError;

  if (Window.ActiveControl is TWinControl) then
    Perform(CM_ENTER, 0, 0);

  if (Assigned(MainActionList)) then
  begin
    MainAction('aVNavigator').Checked := PNavigator.Visible;
    MainAction('aVBookmarks').Checked := PBookmarks.Visible;
    MainAction('aVExplorer').Checked := PExplorer.Visible;
    MainAction('aVSQLHistory').Checked := PSQLHistory.Visible;
    MainAction('aVSQLLog').Checked := PLog.Visible;

    MainAction('aFOpen').OnExecute := aFOpenExecute;
    MainAction('aFSave').OnExecute := aFSaveExecute;
    MainAction('aFSaveAs').OnExecute := aFSaveAsExecute;
    MainAction('aFImportSQL').OnExecute := aFImportSQLExecute;
    MainAction('aFImportText').OnExecute := aFImportTextExecute;
    MainAction('aFImportExcel').OnExecute := aFImportExcelExecute;
    MainAction('aFImportAccess').OnExecute := aFImportAccessExecute;
    MainAction('aFImportSQLite').OnExecute := aFImportSQLiteExecute;
    MainAction('aFImportODBC').OnExecute := aFImportODBCExecute;
    MainAction('aFImportXML').OnExecute := aFImportXMLExecute;
    MainAction('aFExportSQL').OnExecute := aFExportSQLExecute;
    MainAction('aFExportText').OnExecute := aFExportTextExecute;
    MainAction('aFExportExcel').OnExecute := aFExportExcelExecute;
    MainAction('aFExportAccess').OnExecute := aFExportAccessExecute;
    MainAction('aFExportSQLite').OnExecute := aFExportSQLiteExecute;
    MainAction('aFExportODBC').OnExecute := aFExportODBCExecute;
    MainAction('aFExportXML').OnExecute := aFExportXMLExecute;
    MainAction('aFExportHTML').OnExecute := aFExportHTMLExecute;
    MainAction('aFExportBitmap').OnExecute := aFExportBitmapExecute;
    MainAction('aFPrint').OnExecute := aFPrintExecute;
    MainAction('aERedo').OnExecute := aERedoExecute;
    MainAction('aECopy').OnExecute := aECopyExecute;
    MainAction('aEPaste').OnExecute := aEPasteExecute;
    MainAction('aERename').OnExecute := aERenameExecute;
    MainAction('aVObjectBrowser').OnExecute := aViewExecute;
    MainAction('aVDataBrowser').OnExecute := aViewExecute;
    MainAction('aVObjectIDE').OnExecute := aViewExecute;
    MainAction('aVQueryBuilder').OnExecute := aViewExecute;
    MainAction('aVSQLEditor').OnExecute := aViewExecute;
    MainAction('aVDiagram').OnExecute := aViewExecute;
    MainAction('aVNavigator').OnExecute := aVSideBarExecute;
    MainAction('aVBookmarks').OnExecute := aVSideBarExecute;
    MainAction('aVExplorer').OnExecute := aVSideBarExecute;
    MainAction('aVSQLHistory').OnExecute := aVSideBarExecute;
    MainAction('aVSQLLog').OnExecute := aVSQLLogExecute;
    MainAction('aVDetails').OnExecute := aVDetailsExecute;
    MainAction('aVRefresh').OnExecute := aVRefreshExecute;
    MainAction('aVRefreshAll').OnExecute := aVRefreshAllExecute;
    MainAction('aBAdd').OnExecute := aBAddExecute;
    MainAction('aBDelete').OnExecute := aBDeleteExecute;
    MainAction('aBEdit').OnExecute := aBEditExecute;
    MainAction('aBookmark').OnExecute := aBookmarkExecute;
    MainAction('aDCancel').OnExecute := aDCancelExecute;
    MainAction('aDCreateDatabase').OnExecute := aDCreateDatabaseExecute;
    MainAction('aDCreateTable').OnExecute := aDCreateTableExecute;
    MainAction('aDCreateView').OnExecute := aDCreateViewExecute;
    MainAction('aDCreateProcedure').OnExecute := aDCreateRoutineExecute;
    MainAction('aDCreateFunction').OnExecute := aDCreateRoutineExecute;
    MainAction('aDCreateKey').OnExecute := aDCreateIndexExecute;
    MainAction('aDCreateField').OnExecute := aDCreateFieldExecute;
    MainAction('aDCreateForeignKey').OnExecute := aDCreateForeignKeyExecute;
    MainAction('aDCreateTrigger').OnExecute := aDCreateTriggerExecute;
    MainAction('aDCreateEvent').OnExecute := aDCreateEventExecute;
    MainAction('aDCreateHost').OnExecute := aDCreateHostExecute;
    MainAction('aDCreateUser').OnExecute := aDCreateUserExecute;
    MainAction('aDEditServer').OnExecute := PropertiesServerExecute;
    MainAction('aDEditDatabase').OnExecute := aDPropertiesExecute;
    MainAction('aDEditTable').OnExecute := aDPropertiesExecute;
    MainAction('aDEditView').OnExecute := aDPropertiesExecute;
    MainAction('aDEditRoutine').OnExecute := aDPropertiesExecute;
    MainAction('aDEditEvent').OnExecute := aDPropertiesExecute;
    MainAction('aDEditTrigger').OnExecute := aDPropertiesExecute;
    MainAction('aDEditKey').OnExecute := aDPropertiesExecute;
    MainAction('aDEditField').OnExecute := aDPropertiesExecute;
    MainAction('aDEditForeignKey').OnExecute := aDPropertiesExecute;
    MainAction('aDEditHost').OnExecute := aDPropertiesExecute;
    MainAction('aDEditProcess').OnExecute := aDPropertiesExecute;
    MainAction('aDEditUser').OnExecute := aDPropertiesExecute;
    MainAction('aDEditVariable').OnExecute := aDPropertiesExecute;
    MainAction('aDDeleteDatabase').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteTable').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteView').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteRoutine').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteKey').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteField').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteForeignKey').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteTrigger').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteEvent').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteHost').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteUser').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteProcess').OnExecute := aDDeleteExecute;
    MainAction('aDInsertRecord').OnExecute := aDInsertRecordExecute;
    MainAction('aDDeleteRecord').OnExecute := aDDeleteRecordExecute;
    MainAction('aDRun').OnExecute := aDRunExecute;
    MainAction('aDRunSelection').OnExecute := aDRunSelectionExecute;
    MainAction('aDPostObject').OnExecute := aDPostObjectExecute;
    MainAction('aDAutoCommit').OnExecute := aDAutoCommitExecute;
    MainAction('aDCommit').OnExecute := aDCommitExecute;
    MainAction('aDRollback').OnExecute := aDRollbackExecute;
    MainAction('aHSQL').OnExecute := aHSQLExecute;
    MainAction('aHManual').OnExecute := aHManualExecute;


    MainAction('aSAddress').Enabled := True;
    MainAction('aVObjectBrowser').Enabled := True;
    MainAction('aVDataBrowser').Enabled := (SelectedImageIndex in [iiBaseTable, iiSystemView, iiView, iiTrigger]) or ((LastSelectedDatabase <> '') and (LastSelectedDatabase = SelectedDatabase) and (LastSelectedTable <> ''));
    MainAction('aVObjectIDE').Enabled := (SelectedImageIndex in [iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]) or (LastObjectIDEAddress <> '');
    MainAction('aVQueryBuilder').Enabled := (LastSelectedDatabase <> '');
    MainAction('aVSQLEditor').Enabled := True;
    MainAction('aVDiagram').Enabled := (LastSelectedDatabase <> '');
    MainAction('aVNavigator').Enabled := True;
    MainAction('aVBookmarks').Enabled := True;
    MainAction('aVExplorer').Enabled := True;
    MainAction('aVSQLHistory').Enabled := True;
    MainAction('aVSQLLog').Enabled := True;
    MainAction('aVRefresh').Enabled := True;
    MainAction('aVRefreshAll').Enabled := True;
    MainAction('aBAdd').Enabled := True;
    MainAction('aDCancel').Enabled := Client.InUse();
    aDCommitRefresh(nil);
    MainAction('aHSQL').Enabled := Client.ServerVersion >= 40100;
    MainAction('aHManual').Enabled := Client.Account.ManualURL <> '';

    aPResult.ShortCut := ShortCut(VK_F8, [ssAlt]);

    if (Assigned(ActiveControlOnDeactivate) and ActiveControlOnDeactivate.Visible) then
      try Window.FocusControl(ActiveControlOnDeactivate); except end;

    if (Window.ActiveControl = FNavigator) then FNavigatorEnter(nil)
    else if (Window.ActiveControl = ActiveListView) then ListViewEnter(nil)
    else if (Window.ActiveControl = FLog) then FLogEnter(nil)
    else if (Window.ActiveControl is TSynMemo) then SynMemoEnter(nil)
    else if (Window.ActiveControl = ActiveDBGrid) then DBGridEnter(ActiveDBGrid);

    if (Assigned(FFolders) and (Path <> FFolders.SelectedFolder)) then
      FFolders.SelectedFolder := Path;
  end;

  if (Assigned(StatusBar)) then
    StatusBarRefresh(True);
end;

procedure TFClient.CMFrameDeactivate(var Message: TMessage);
begin
  KillTimer(Handle, tiNavigator);
  KillTimer(Handle, tiStatusBar);

  ActiveControlOnDeactivate := Window.ActiveControl;

  MainAction('aSAddress').Enabled := False;
  MainAction('aVObjectBrowser').Enabled := False;
  MainAction('aVDataBrowser').Enabled := False;
  MainAction('aVObjectIDE').Enabled := False;
  MainAction('aVQueryBuilder').Enabled := False;
  MainAction('aVSQLEditor').Enabled := False;
  MainAction('aVDiagram').Enabled := False;
  MainAction('aVNavigator').Enabled := False;
  MainAction('aVBookmarks').Enabled := False;
  MainAction('aVExplorer').Enabled := False;
  MainAction('aVSQLHistory').Enabled := False;
  MainAction('aVSQLLog').Enabled := False;
  MainAction('aVRefresh').Enabled := False;
  MainAction('aVRefreshAll').Enabled := False;
  MainAction('aBAdd').Enabled := False;
  MainAction('aDCancel').Enabled := False;
  MainAction('aDAutoCommit').Enabled := False;
  MainAction('aDCommit').Enabled := False;
  MainAction('aDRollback').Enabled := False;
  MainAction('aHSQL').Enabled := False;
  MainAction('aHManual').Enabled := False;

  MainAction('aECopy').OnExecute := nil;
  MainAction('aEPaste').OnExecute := nil;

  aPResult.ShortCut := 0;

  if (Window.ActiveControl = FNavigator) then FNavigatorExit(Window.ActiveControl)
  else if (Window.ActiveControl = ActiveListView) then ListViewExit(Window.ActiveControl)
  else if (Window.ActiveControl = FLog) then FLogExit(Window.ActiveControl)
  else if (Window.ActiveControl is TSynMemo) then SynMemoExit(Window.ActiveControl)
  else if (Window.ActiveControl = ActiveDBGrid) then DBGridExit(Window.ActiveControl);

  Include(FrameState, tsActive);
end;

procedure TFClient.CMPostBuilderQueryChange(var Message: TMessage);
begin
  FBuilderEditorPageControlCheckStyle();
end;

procedure TFClient.CMPostMonitor(var Message: TMessage);
var
  Text: string;
begin
  if (MainAction('aVSQLLog').Checked) then
  begin
    Text := Client.SQLMonitor.CacheText;
    SendMessage(FLog.Handle, WM_SETTEXT, 0, LPARAM(PChar(Text)));

    PLogResize(nil);
  end;
end;

procedure TFClient.CMPostScroll(var Message: TMessage);
begin
//  StatusBarRefresh();
end;

procedure TFClient.CMPostShow(var Message: TMessage);
var
  URI: TUURI;
begin
  PNavigator.Visible := Client.Account.Desktop.NavigatorVisible;
  PBookmarks.Visible := Client.Account.Desktop.BookmarksVisible;
  PExplorer.Visible := Client.Account.Desktop.ExplorerVisible;
  PSQLHistory.Visible := Client.Account.Desktop.SQLHistoryVisible; if (PSQLHistory.Visible) then FSQLHistoryRefresh(nil);
  PSideBar.Visible := PNavigator.Visible or PBookmarks.Visible or PExplorer.Visible or PSQLHistory.Visible; SSideBar.Visible := PSideBar.Visible;

  if (PExplorer.Visible) then
    CreateExplorer();

  PSideBar.Width := Client.Account.Desktop.SelectorWitdth;
  PFiles.Height := PSideBar.ClientHeight - Client.Account.Desktop.FoldersHeight - SExplorer.Height;

  FSQLEditorSynMemo.Options := FSQLEditorSynMemo.Options + [eoScrollPastEol];  // Speed up the performance
  FSQLEditorSynMemo.Text := Client.Account.Desktop.EditorContent;
  if (Length(FSQLEditorSynMemo.Lines.Text) < LargeSQLScriptSize) then
    FSQLEditorSynMemo.Options := FSQLEditorSynMemo.Options - [eoScrollPastEol]  // Slow down the performance on large content
  else
    FSQLEditorSynMemo.Options := FSQLEditorSynMemo.Options + [eoScrollPastEol];  // Speed up the performance
  PResult.Height := Client.Account.Desktop.DataHeight;
  PResultHeight := PResult.Height;
  PBlob.Height := Client.Account.Desktop.BlobHeight;

  PLog.Height := Client.Account.Desktop.LogHeight;
  PLog.Visible := Client.Account.Desktop.LogVisible; SLog.Visible := PLog.Visible;

  aVBlobText.Checked := True;

  FormResize(nil);

  Visible := True;

  Perform(CM_ACTIVATEFRAME, 0, 0);


  if (Copy(Param, 1, 8) = 'mysql://') then
    Address := Param
  else if (Param <> '') then
  begin
    URI := TUURI.Create(Client.Account.Desktop.Address);
    URI.Param['view'] := 'editor';
    URI.Table := '';
    URI.Param['system'] := Null;
    URI.Param['filter'] := Null;
    URI.Param['offset'] := Null;
    URI.Param['file'] := EscapeURL(Param);
    URI.Param['cp'] := Null;
    Address := URI.Address;
    URI.Free();
  end
  else
    Address := Client.Account.Desktop.Address;
end;

procedure TFClient.CMSysFontChanged(var Message: TMessage);
var
  LogFont: TLogFont;
  NonClientMetrics: TNonClientMetrics;
begin
  inherited;

  if (not StyleServices.Enabled and not CheckWin32Version(6)) then
  begin
    PNavigator.BevelInner := bvRaised; PNavigator.BevelOuter := bvLowered;
    PBookmarks.BevelInner := bvRaised; PBookmarks.BevelOuter := bvLowered;
    PExplorer.BevelInner := bvRaised; PExplorer.BevelOuter := bvLowered;
    PSQLHistory.BevelInner := bvRaised; PSQLHistory.BevelOuter := bvLowered;
    PFolders.BevelInner := bvRaised; PFolders.BevelOuter := bvLowered;
    PFiles.BevelInner := bvRaised; PFiles.BevelOuter := bvLowered;
    PListView.BevelInner := bvRaised; PListView.BevelOuter := bvLowered;
    PBuilderQuery.BevelInner := bvRaised; PBuilderQuery.BevelOuter := bvLowered;
    PSynMemo.BevelInner := bvRaised; PSynMemo.BevelOuter := bvLowered;
    PWorkbench.BevelInner := bvRaised; PWorkbench.BevelOuter := bvLowered;
    PDBGrid.BevelInner := bvRaised; PDBGrid.BevelOuter := bvLowered;
    PBlob.BevelInner := bvRaised; PBlob.BevelOuter := bvLowered;
    PLog.BevelInner := bvRaised; PLog.BevelOuter := bvLowered;

    TBSideBar.BorderWidth := 1;
    ToolBar.BorderWidth := 1;
  end
  else
  begin
    PNavigator.BevelInner := bvNone; PNavigator.BevelOuter := bvNone;
    PBookmarks.BevelInner := bvNone; PBookmarks.BevelOuter := bvNone;
    PExplorer.BevelInner := bvNone; PExplorer.BevelOuter := bvNone;
    PFolders.BevelInner := bvNone; PNavigator.BevelOuter := bvNone;
    PFiles.BevelInner := bvNone; PFiles.BevelOuter := bvNone;
    PSQLHistory.BevelInner := bvNone; PSQLHistory.BevelOuter := bvNone;
    PListView.BevelInner := bvNone; PListView.BevelOuter := bvNone;
    PBuilderQuery.BevelInner := bvNone; PBuilderQuery.BevelOuter := bvNone;
    PSynMemo.BevelInner := bvNone; PSynMemo.BevelOuter := bvNone;
    PWorkbench.BevelInner := bvNone; PWorkbench.BevelOuter := bvNone;
    PDBGrid.BevelInner := bvNone; PDBGrid.BevelOuter := bvNone;
    PBlob.BevelInner := bvNone; PBlob.BevelOuter := bvNone;
    PLog.BevelInner := bvNone; PLog.BevelOuter := bvNone;
  end;

  TBSideBar.Left := 0;
  TBSideBar.Top := 0;
  TBSideBar.ButtonHeight := TBSideBar.Images.Height + 6; TBSideBar.ButtonWidth := TBSideBar.Images.Width + 7;
  PSideBarHeader.Height := PSideBarHeader.Height;

  PToolBar.AutoSize := False;
  ToolBar.ButtonHeight := ToolBar.Images.Height + 6; ToolBar.ButtonWidth := ToolBar.Images.Width + 7;
  PToolBar.AutoSize := True;

  if (CheckWin32Version(6)) then
  begin
    SendMessage(FFilter.Handle, CB_SETCUEBANNER, 0, LParam(PChar(ReplaceStr(Preferences.LoadStr(209), '&', ''))));
    SendMessage(FQuickSearch.Handle, EM_SETCUEBANNER, 0, LParam(PChar(ReplaceStr(Preferences.LoadStr(424), '&', ''))));
    SendMessage(FBlobSearch.Handle, EM_SETCUEBANNER, 0, LParam(PChar(ReplaceStr(Preferences.LoadStr(424), '&', ''))));
  end;

  SetWindowLong(ListView_GetHeader(FServerListView.Handle), GWL_STYLE, GetWindowLong(ListView_GetHeader(FServerListView.Handle), GWL_STYLE) or HDS_DRAGDROP);

  if (not StyleServices.Enabled) then
    PObjectIDESpacer.Color := clBtnFace
  else
    PObjectIDESpacer.Color := clActiveBorder;

  if (Assigned(Client)) then
  begin
    PResultHeader.Width := CloseButton.Bitmap.Width + 2 * GetSystemMetrics(SM_CXEDGE);
    PLogHeader.Width := CloseButton.Bitmap.Width + 2 * GetSystemMetrics(SM_CXEDGE);

    if (Assigned(ActiveDBGrid) and (ActiveDBGrid.DataSource.DataSet is TMySQLDataSet)) then
      SBResultRefresh(TMySQLDataSet(ActiveDBGrid.DataSource.DataSet));

    FormResize(nil);
  end;

  Font := Window.Font;

  FSQLEditorSynMemo.Font.Name := Preferences.SQLFontName;
  FSQLEditorSynMemo.Font.Style := Preferences.SQLFontStyle;
  FSQLEditorSynMemo.Font.Color := Preferences.SQLFontColor;
  FSQLEditorSynMemo.Font.Size := Preferences.SQLFontSize;
  FSQLEditorSynMemo.Font.Charset := Preferences.SQLFontCharset;
  FSQLEditorSynMemo.Gutter.Font := FSQLEditorSynMemo.Font;
  if (Preferences.Editor.LineNumbersForeground = clNone) then
    FSQLEditorSynMemo.Gutter.Font.Color := clWindowText
  else
    FSQLEditorSynMemo.Gutter.Font.Color := Preferences.Editor.LineNumbersForeground;

  FText.Font.Name := Preferences.GridFontName;
  FText.Font.Style := Preferences.GridFontStyle;
  FText.Font.Color := Preferences.GridFontColor;
  FText.Font.Size := Preferences.GridFontSize;
  FText.Font.Charset := Preferences.GridFontCharset;
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)
    and (GetObject(FText.Font.Handle, SizeOf(LogFont), @LogFont) <> 0)) then
  begin
    LogFont.lfQuality  := NonClientMetrics.lfMessageFont.lfQuality;
    FText.Font.Handle := CreateFontIndirect(LogFont);
  end;
  FText.WantTabs := Preferences.Editor.TabAccepted;

  FRTF.Font := FText.Font;

  FHexEditor.Font := FSQLEditorSynMemo.Font;
  if (Preferences.Editor.LineNumbersForeground = clNone) then
    FHexEditor.Colors.Offset := clWindowText
  else
    FHexEditor.Colors.Offset := Preferences.Editor.LineNumbersForeground;
  if (Preferences.Editor.LineNumbersBackground = clNone) then
    FHexEditor.Colors.OffsetBackground := clBtnFace
  else
    FHexEditor.Colors.OffsetBackground := Preferences.Editor.LineNumbersBackground;

  SendMessage(FLog.Handle, EM_SETTEXTMODE, TM_PLAINTEXT, 0);
  SendMessage(FLog.Handle, EM_SETWORDBREAKPROC, 0, LPARAM(@EditWordBreakProc));
end;

procedure TFClient.CMWantedSynchronize(var Message: TWMTimer);
begin
  if (not (csDestroying in ComponentState)) then
    Wanted.Synchronize();
end;

function TFClient.ColumnWidthKindFromImageIndex(const AImageIndex: Integer): TADesktop.TListViewKind;
begin
  case (AImageIndex) of
    iiServer: Result := lkServer;
    iiDatabase,
    iiSystemDatabase: Result := lkDatabase;
    iiBaseTable,
    iiSystemView,
    iiView: Result := lkTable;
    iiHosts: Result := lkHosts;
    iiProcesses: Result := lkProcesses;
    iiUsers: Result := lkUsers;
    iiStati: Result := lkStati;
    iiVariables: Result := lkVariables;
    else raise ERangeError.Create(SRangeError);
  end;
end;

procedure TFClient.CrashRescue();
begin
  if ((SQLEditor.Filename <> '') and not FSQLEditorSynMemo.Modified) then
    Client.Account.Desktop.EditorContent := ''
  else
    Client.Account.Desktop.EditorContent := FSQLEditorSynMemo.Text;

  try
    if (Assigned(ActiveWorkbench)) then
      ActiveWorkbench.SaveToFile(Client.Account.DataPath + ActiveWorkbench.Name + PathDelim + 'Diagram.xml');
  except
  end;
end;

procedure TFClient.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.Style := Params.Style
    and not (WS_THICKFRAME or WS_SYSMENU or WS_DLGFRAME or WS_BORDER);
end;

constructor TFClient.Create(const AOwner: TComponent; const AParent: TWinControl; const AClient: TCClient; const AParam: string);
var
  Kind: TADesktop.TListViewKind;
  Node: TTreeNode;
  NonClientMetrics: TNonClientMetrics;
begin
  inherited Create(AOwner);


  Parent := TWinControl(AParent);

  Width := Window.ClientWidth;
  Height := Window.ClientHeight;

  FrameState := [tsLoading];

  NMListView := nil;
  Client := AClient;
  SQLEditor := TSQLEditor.Create(Self);
  Param := AParam;
  ActiveControlOnDeactivate := nil;
  ActiveDBGrid := nil;
  ActiveIDEInputDataSet := nil;
  ActiveListView := FServerListView;
  ActiveSynMemo := nil;
  ActiveWorkbench := nil;
  HostsListView := nil;
  ProcessesListView := nil;
  StatiListView := nil;
  UsersListView := nil;
  VariablesListView := nil;
  for Kind := lkServer to lkVariables do
  begin
    ListViewSortData[Kind].Kind := Kind;
    ListViewSortData[Kind].Index := 0;
    if (Kind = lkTable) then
      ListViewSortData[Kind].Order := 0
    else
      ListViewSortData[Kind].Order := 1;
  end;
  FNavigatorNodeToExpand := nil;
  PanelMouseDownPoint := Point(-1, -1);


  TBSideBar.Images := Preferences.SmallImages;
  ToolBar.Images := Preferences.SmallImages;
  FNavigator.Images := Preferences.SmallImages;
  FBookmarks.SmallImages := Preferences.SmallImages;
  FSQLHistory.Images := Preferences.SmallImages;
  TBLimitEnabled.Images := Preferences.SmallImages;
  TBQuickSearchEnabled.Images := Preferences.SmallImages;
  TBFilterEnabled.Images := Preferences.SmallImages;
  FServerListView.SmallImages := Preferences.SmallImages;

  Node := FNavigator.Items.Add(nil, Client.Caption);
  Node.Data := Client;
  Node.HasChildren := True;
  Node.ImageIndex := iiServer;

  FUDOffset.HandleNeeded();
  FOffset.HandleNeeded();
  FUDLimit.HandleNeeded();
  FLimit.HandleNeeded();

  tbNavigator.Action := MainAction('aVNavigator');
  tbBookmarks.Action := MainAction('aVBookmarks');
  tbExplorer.Action := MainAction('aVExplorer');
  tbSQLHistory.Action := MainAction('aVSQLHistory');

  tbObjects.Action := MainAction('aVObjectBrowser'); tbObjects.Caption := ReplaceStr(tbObjects.Caption, '&', '');
  tbBrowser.Action := MainAction('aVDataBrowser'); tbBrowser.Caption := ReplaceStr(tbBrowser.Caption, '&', '');
  tbIDE.Action := MainAction('aVObjectIDE'); tbIDE.Caption := ReplaceStr(tbIDE.Caption, '&', '');
  tbBuilder.Action := MainAction('aVQueryBuilder'); tbBuilder.Caption := ReplaceStr(tbBuilder.Caption, '&', '');
  tbEditor.Action := MainAction('aVSQLEditor'); tbEditor.Caption := ReplaceStr(tbEditor.Caption, '&', '');
  tbDiagram.Action := MainAction('aVDiagram'); tbDiagram.Caption := ReplaceStr(tbDiagram.Caption, '&', '');

  miSNavigator.Action := MainAction('aVNavigator');
  miSBookmarks.Action := MainAction('aVBookmarks');
  miSExplorer.Action := MainAction('aVExplorer');
  miSSQLHistory.Action := MainAction('aVSQLHistory');

  miNImportSQL.Action := MainAction('aFImportSQL');
  miNImportText.Action := MainAction('aFImportText');
  miNImportExcel.Action := MainAction('aFImportExcel');
  miNImportAccess.Action := MainAction('aFImportAccess');
  miNImportSQLite.Action := MainAction('aFImportSQLite');
  miNImportODBC.Action := MainAction('aFImportODBC');
  miNImportXML.Action := MainAction('aFImportXML');
  miNExportSQL.Action := MainAction('aFExportSQL');
  miNExportText.Action := MainAction('aFExportText');
  miNExportExcel.Action := MainAction('aFExportExcel');
  miNExportAccess.Action := MainAction('aFExportAccess');
  miNExportSQLite.Action := MainAction('aFExportSQLite');
  miNExportODBC.Action := MainAction('aFExportODBC');
  miNExportXML.Action := MainAction('aFExportXML');
  miNExportHTML.Action := MainAction('aFExportHTML');
  miNCopy.Action := MainAction('aECopy');
  miNPaste.Action := MainAction('aEPaste');
  miNRename.Action := MainAction('aERename');
  miNCreateDatabase.Action := MainAction('aDCreateDatabase');
  miNCreateTable.Action := MainAction('aDCreateTable');
  miNCreateView.Action := MainAction('aDCreateView');
  miNCreateProcedure.Action := MainAction('aDCreateProcedure');
  miNCreateFunction.Action := MainAction('aDCreateFunction');
  miNCreateIndex.Action := MainAction('aDCreateKey');
  miNCreateField.Action := MainAction('aDCreateField');
  miNCreateForeignKey.Action := MainAction('aDCreateForeignKey');
  miNCreateTrigger.Action := MainAction('aDCreateTrigger');
  miNCreateEvent.Action := MainAction('aDCreateEvent');
  miNCreateUser.Action := MainAction('aDCreateUser');
  miNCreateHost.Action := MainAction('aDCreateHost');
  miNEmpty.Action := MainAction('aDEmpty');

  mbBAdd.Action := MainAction('aBAdd');
  mbBDelete.Action := MainAction('aBDelete');
  mbBEdit.Action := MainAction('aBEdit');

  miHCopy.Action := MainAction('aECopy');

  mlFImportSQL.Action := MainAction('aFImportSQL');
  mlFImportText.Action := MainAction('aFImportText');
  mlFImportExcel.Action := MainAction('aFImportExcel');
  mlFImportAccess.Action := MainAction('aFImportAccess');
  mlFImportSQLite.Action := MainAction('aFImportSQLite');
  mlFImportODBC.Action := MainAction('aFImportODBC');
  mlFImportXML.Action := MainAction('aFImportXML');
  mlFExportSQL.Action := MainAction('aFExportSQL');
  mlFExportText.Action := MainAction('aFExportText');
  mlFExportExcel.Action := MainAction('aFExportExcel');
  mlFExportAccess.Action := MainAction('aFExportAccess');
  mlFExportSQLite.Action := MainAction('aFExportSQLite');
  mlFExportODBC.Action := MainAction('aFExportODBC');
  mlFExportXML.Action := MainAction('aFExportXML');
  mlFExportHTML.Action := MainAction('aFExportHTML');
  mlECopy.Action := MainAction('aECopy');
  mlEPaste.Action := MainAction('aEPaste');
  mlERename.Action := MainAction('aERename');
  mlDCreateDatabase.Action := MainAction('aDCreateDatabase');
  mlDCreateTable.Action := MainAction('aDCreateTable');
  mlDCreateView.Action := MainAction('aDCreateView');
  mlDCreateProcedure.Action := MainAction('aDCreateProcedure');
  mlDCreateFunction.Action := MainAction('aDCreateFunction');
  mlDCreateIndex.Action := MainAction('aDCreateKey');
  mlDCreateField.Action := MainAction('aDCreateField');
  mlDCreateForeignKey.Action := MainAction('aDCreateForeignKey');
  mlDCreateTrigger.Action := MainAction('aDCreateTrigger');
  mlDCreateEvent.Action := MainAction('aDCreateEvent');
  mlDCreateUser.Action := MainAction('aDCreateUser');
  mlDCreateHost.Action := MainAction('aDCreateHost');
  mlDEmpty.Action := MainAction('aDEmpty');

  mpDRun.Action := MainAction('aDRun');
  mpDRunSelection.Action := MainAction('aDRunSelection');
  mpECut.Action := MainAction('aECut');
  mpECopy.Action := MainAction('aECopy');
  mpEPaste.Action := MainAction('aEPaste');
  mpEDelete.Action := MainAction('aEDelete');
  mpECopyToFile.Action := MainAction('aECopyToFile');
  mpEPasteFromFile.Action := MainAction('aEPasteFromFile');
  mpESelectAll.Action := MainAction('aESelectAll');

  gmECut.Action := MainAction('aECut');
  gmECopy.Action := MainAction('aECopy');
  gmEPaste.Action := MainAction('aEPaste');
  gmEDelete.Action := MainAction('aEDelete');
  gmECopyToFile.Action := MainAction('aECopyToFile');
  gmEPasteFromFile.Action := MainAction('aEPasteFromFile');
  gmFExportSQL.Action := MainAction('aFExportSQL');
  gmFExportText.Action := MainAction('aFExportText');
  gmFExportExcel.Action := MainAction('aFExportExcel');
  gmFExportXML.Action := MainAction('aFExportXML');
  gmFExportHTML.Action := MainAction('aFExportHTML');
  gmDInsertRecord.Action := MainAction('aDInsertRecord');
  gmDDeleteRecord.Action := MainAction('aDDeleteRecord');
  gmDEditRecord.Action := MainAction('aDEditRecord');

  mwFImportSQL.Action := MainAction('aFImportSQL');
  mwFImportText.Action := MainAction('aFImportText');
  mwFImportExcel.Action := MainAction('aFImportExcel');
  mwFImportAccess.Action := MainAction('aFImportAccess');
  mwFImportSQLite.Action := MainAction('aFImportSQLite');
  mwFImportODBC.Action := MainAction('aFImportODBC');
  mwFImportXML.Action := MainAction('aFImportXML');
  mwFExportSQL.Action := MainAction('aFExportSQL');
  mwFExportText.Action := MainAction('aFExportText');
  mwFExportExcel.Action := MainAction('aFExportExcel');
  mwFExportAccess.Action := MainAction('aFExportAccess');
  mwFExportSQLite.Action := MainAction('aFExportSQLite');
  mwFExportODBC.Action := MainAction('aFExportODBC');
  mwFExportXML.Action := MainAction('aFExportXML');
  mwFExportHTML.Action := MainAction('aFExportHTML');
  mwFExportBitmap.Action := MainAction('aFExportBitmap');
  mwECopy.Action := MainAction('aECopy');
  mwDCreateField.Action := MainAction('aDCreateField');
  mwDCreateForeignKey.Action := MainAction('aDCreateForeignKey');
  mwDEmpty.Action := MainAction('aDEmpty');

  tmECut.Action := MainAction('aECut');
  tmECopy.Action := MainAction('aECopy');
  tmEPaste.Action := MainAction('aEPaste');
  tmEDelete.Action := MainAction('aEDelete');
  tmESelectAll.Action := MainAction('aESelectAll');

  smECopy.Action := MainAction('aECopy');
  smECopyToFile.Action := MainAction('aECopyToFile');
  smESelectAll.Action := MainAction('aESelectAll');

  FNavigator.RowSelect := CheckWin32Version(6, 1);
  FSQLHistory.RowSelect := CheckWin32Version(6, 1);

  PListView.Align := alClient;
  PSynMemo.Align := alClient;
  PBuilder.Align := alClient;

  FServerListView.RowSelect := CheckWin32Version(6);

  FSQLEditorSynMemo.Highlighter := MainHighlighter;
  FBuilderSynMemo.Highlighter := MainHighlighter;
  FSQLEditorPrint.Highlighter := MainHighlighter;

  FText.Modified := False;
  GIFImage := TGIFImage.Create();
  PNGImage := TPNGImage.Create();
  JPEGImage := TJPEGImage.Create();

  NavigatorElapse := 0;
  MovingToAddress := False;

  LastFNavigatorSelected := nil;
  LastSelectedDatabase := '';
  LastSelectedTable := '';
  LastTableView := vObjects;
  LastObjectIDEAddress := '';


  OldFListOrderIndex := -1;
  IgnoreFGridTitleClick := False;

  Client.CreateDesktop := CreateDesktop;
  Client.RegisterEventProc(FormClientEvent);

  Client.Account.RegisterDesktop(Self, FormAccountEvent);

  Wanted := TWanted.Create(Self);

  ToolBarData.Addresses := TStringList.Create();
  ToolBarData.AddressMRU := TMRUList.Create(0);
  ToolBarData.AddressMRU.Assign(Client.Account.Desktop.AddressMRU);
  FilterMRU := TMRUList.Create(100);
  ToolBarData.CurrentAddress := -1;

  SyntaxProvider.ServerVersionInt := Client.ServerVersion;
  if (Client.LowerCaseTableNames <> 0) then
    SyntaxProvider.IdentCaseSens := icsSensitiveLowerCase
  else
    SyntaxProvider.IdentCaseSens := icsNonSensitive;

  FormAccountEvent(Client.Account.Desktop.Bookmarks.ClassType);


  CloseButton := TPicture.Create();
  CloseButton.Bitmap.Height := 11;
  CloseButton.Bitmap.Width := CloseButton.Bitmap.Height + 1;
  DrawCloseBitmap(CloseButton.Bitmap);

  FOffset.Constraints.MaxWidth := FOffset.Width;

  Perform(CM_CHANGEPREFERENCES, 0, 0);
  Window.Perform(CM_SYSFONTCHANGED, 0, 0);

  FBuilderEditorStatusChange(Self, []);

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    Window.ApplyWinAPIUpdates(Self, NonClientMetrics.lfStatusFont);


  PostMessage(Handle, CM_POSTSHOW, 0, 0);
end;

function TFClient.CreateDesktop(const CObject: TCObject): TCObject.TDesktop;
begin
  if (CObject is TCDatabase) then
    Result := TDatabaseDesktop.Create(Self, TCDatabase(CObject))
  else if (CObject is TCBaseTable) then
    Result := TTableDesktop.Create(Self, TCBaseTable(CObject))
  else if (CObject is TCView) then
    Result := TViewDesktop.Create(Self, TCView(CObject))
  else if (CObject is TCRoutine) then
    Result := TRoutineDesktop.Create(Self, TCRoutine(CObject))
  else if (CObject is TCEvent) then
    Result := TEventDesktop.Create(Self, TCEvent(CObject))
  else if (CObject is TCTrigger) then
    Result := TTriggerDesktop.Create(Self, TCTrigger(CObject))
  else
    Result := nil;
end;

procedure TFClient.CreateExplorer();
begin
  if (not Assigned(ShellLink)) then
    ShellLink := TJamShellLink.Create(Owner);

  if (not Assigned(FFolders)) then
  begin
    FFolders := TJamShellTree.Create(Owner);
    FFolders.Parent := PFolders;
    FFolders.Visible := False;
    FFolders.Left := 0;
    FFolders.Top := 0;
    FFolders.Width := PFolders.ClientWidth;
    FFolders.Height := PFolders.ClientHeight;
    FFolders.Align := alClient;
    FFolders.HelpContext := 1108;
    FFolders.HotTrack := True;
    FFolders.ShellLink := ShellLink;
    FFolders.BorderStyle := bsNone;
    FFolders.ParentFont := True;
    FFolders.ShowLines := False;
    FFolders.ShowRecycleBin := False;
    FFolders.OnChange := FFoldersChange;

    if (CheckWin32Version(6, 1)) then
      SetWindowLong(FFolders.Handle, GWL_STYLE, GetWindowLong(FFolders.Handle, GWL_STYLE) or TVS_FULLROWSELECT);
  end;

  if (not Assigned(FFiles)) then
  begin
    FFiles := TJamShellList.Create(Owner);
    FFiles.Parent := PFiles;
    FFiles.Left := 0;
    FFiles.Top := 0;
    FFiles.Width := PFiles.Width;
    FFiles.Height := PFiles.Height;
    FFiles.Align := alClient;
    FFiles.BorderStyle := bsNone;
    FFiles.Filter := Client.Account.Desktop.FilesFilter;
    FFiles.HelpContext := 1108;
    FFiles.HideSelection := False;
    FFiles.IconOptions.AutoArrange := True;
    FFiles.PopupMenu := MFiles;
    FFiles.BackgroundPopupMenu := MFiles;
    FFiles.ParentFont := True;
    FFiles.RowSelect := True;
    FFiles.ShellContextMenu := False;
    FFiles.ShowFolders := False;
    FFiles.ShowRecycleBin := False;
    FFiles.ShellLink := ShellLink;
    FFiles.OnDblClick := ListViewDblClick;
    FFiles.OnKeyDown := ListViewKeyDown;
    FFiles.OnEnter := FFilesEnter;
  end;
end;

function TFClient.CreateDBGrid(const PDBGrid: TPanel_Ext; const DataSource: TDataSource): TMySQLDBGrid;
var
  I: Integer;
  LogFont: TLogFont;
  NonClientMetrics: TNonClientMetrics;
begin
  Result := TMySQLDBGrid.Create(Owner);
  Result.BorderStyle := bsNone;

  Result.Left := 0;
  Result.Top := 0;
  Result.Width := PDBGrid.ClientWidth;
  Result.Height := PDBGrid.ClientHeight;
  Result.Align := alClient;
  Result.Constraints.MinHeight := 30;
  Result.DataSource := FGridDataSource;
  Result.DefaultDrawing := False;
  Result.HelpContext := 1036;
  Result.Options := [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgMultiSelect, dgTitleClick, dgTitleHotTrack];
  Result.ParentFont := False;
  Result.Font.Name := Preferences.GridFontName;
  Result.Font.Style := Preferences.GridFontStyle;
  Result.Font.Color := Preferences.GridFontColor;
  Result.Font.Size := Preferences.GridFontSize;
  Result.Font.Charset := Preferences.GridFontCharset;
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)
    and (GetObject(Result.Font.Handle, SizeOf(LogFont), @LogFont) <> 0)) then
  begin
    LogFont.lfQuality  := NonClientMetrics.lfMessageFont.lfQuality;
    Result.Font.Handle := CreateFontIndirect(LogFont);
  end;
  Result.TitleFont.Name :=  Result.Font.Name;
  Result.TitleFont.Style := Result.Font.Style;
  Result.TitleFont.Color := Result.Font.Color;
  Result.TitleFont.Size := Result.Font.Size;
  Result.TitleFont.Charset := Result.Font.Charset;
  Result.Canvas.Font := Result.Font;
  for I := 0 to Result.Columns.Count - 1 do
    Result.Columns[I].Font := Result.Font;
  Result.PopupMenu := MGrid;
  Result.TabOrder := 0;
  Result.OnCanEditShow := Client.GridCanEditShow;
  Result.OnColEnter := DBGridColEnter;
  Result.OnColExit := DBGridColExit;
  Result.OnColumnMoved := DBGridColumnMoved;
  Result.OnDrawColumnCell := DBGridDrawColumnCell;
  Result.OnDblClick := DBGridDblClick;
  Result.OnEnter := DBGridEnter;
  Result.OnExit := DBGridExit;
  Result.OnKeyDown := DBGridKeyDown;
  Result.OnMouseMove := DBGridMouseMove;
  Result.OnTitleClick := DBGridTitleClick;

  Result.DataSource := DataSource;

  Result.Parent := PDBGrid;

  Result.Perform(CM_PARENTCOLORCHANGED, 0, 0);
  Result.Perform(CM_PARENTFONTCHANGED, 0, 0);
  Result.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
  Result.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  Result.Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  Result.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    Window.ApplyWinAPIUpdates(Result, NonClientMetrics.lfStatusFont);
end;

function TFClient.CreateListView(const Data: TCustomData): TListView;
var
  NonClientMetrics: TNonClientMetrics;
begin
  Result := TListView.Create(FServerListView.Owner);

  Result.Left := 0;
  Result.Top := 0;
  Result.Width := PListView.ClientWidth;
  Result.Height := PListView.ClientHeight;
  Result.Align := alClient;
  Result.BorderStyle := FServerListView.BorderStyle;
  Result.DragMode := FServerListView.DragMode;
  Result.HelpContext := FServerListView.HelpContext;
  Result.HideSelection := FServerListView.HideSelection;
  Result.MultiSelect := FServerListView.MultiSelect;
  Result.GroupView := FServerListView.GroupView;
  Result.PopupMenu := FServerListView.PopupMenu;
  Result.RowSelect := FServerListView.RowSelect;
  Result.SmallImages := Preferences.SmallImages;
  Result.Parent := FServerListView.Parent;
  Result.ViewStyle := FServerListView.ViewStyle;
  Result.Visible := False;
  if (TObject(Data) is TCTable) then
  begin
    Result.OnAdvancedCustomDrawItem := ListViewAdvancedCustomDrawItem;
    Result.OnAdvancedCustomDrawSubItem := ListViewAdvancedCustomDrawSubItem;
  end;
  Result.OnChange := FServerListView.OnChange;
  Result.OnChanging := FServerListView.OnChanging;
  Result.OnColumnClick := FServerListView.OnColumnClick;
  Result.OnCompare := FServerListView.OnCompare;
  Result.OnDblClick := FServerListView.OnDblClick;
  Result.OnEdited := FServerListView.OnEdited;
  Result.OnEditing := FServerListView.OnEditing;
  Result.OnEnter := FServerListView.OnEnter;
  Result.OnExit := FServerListView.OnExit;
  Result.OnDragDrop := FServerListView.OnDragDrop;
  Result.OnDragOver := FServerListView.OnDragOver;
  Result.OnKeyDown := FServerListView.OnKeyDown;
  Result.OnSelectItem := FServerListView.OnSelectItem;

  Result.Tag := NativeInt(Data);

  SetWindowLong(ListView_GetHeader(Result.Handle), GWL_STYLE, GetWindowLong(ListView_GetHeader(FServerListView.Handle), GWL_STYLE) or HDS_DRAGDROP);

  Result.Parent := PListView;

  Result.Perform(CM_PARENTCOLORCHANGED, 0, 0);
  Result.Perform(CM_PARENTFONTCHANGED, 0, 0);
  Result.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
  Result.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  Result.Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  Result.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    Window.ApplyWinAPIUpdates(Result, NonClientMetrics.lfStatusFont);

  ListViewInitialize(Result);
end;

function TFClient.CreatePDBGrid(): TPanel_Ext;
var
  NonClientMetrics: TNonClientMetrics;
begin
  Result := TPanel_Ext.Create(Owner);

  Result.Left := PDBGrid.Left;
  Result.Top := PDBGrid.Top;
  Result.Width := PDBGrid.Width;
  Result.Height := PDBGrid.Height;
  Result.Align := PDBGrid.Align;
  Result.BevelInner := PDBGrid.BevelInner;
  Result.BevelOuter := PDBGrid.BevelOuter;
  Result.Color := PDBGrid.Color;
  Result.Constraints.MinHeight := PDBGrid.Constraints.MinHeight;
  Result.ParentBackground := PDBGrid.ParentBackground;
  Result.OnResize := PDBGrid.OnResize;

  Result.Parent := PDBGrid.Parent;

  Result.Perform(CM_PARENTCOLORCHANGED, 0, 0);
  Result.Perform(CM_PARENTFONTCHANGED, 0, 0);
  Result.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
  Result.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  Result.Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  Result.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    Window.ApplyWinAPIUpdates(Result, NonClientMetrics.lfStatusFont);
end;

function TFClient.CreateSynMemo(): TSynMemo;
var
  NonClientMetrics: TNonClientMetrics;
begin
  Result := TSynMemo.Create(FSQLEditorSynMemo.Owner);

  Result.Left := 0;
  Result.Top := 0;
  Result.Width := PSynMemo.ClientWidth;
  Result.Height := PSynMemo.ClientHeight;
  Result.Align := alClient;
  Result.BorderStyle := FSQLEditorSynMemo.BorderStyle;
  Result.HelpType := FSQLEditorSynMemo.HelpType;
  Result.HelpContext := FSQLEditorSynMemo.HelpContext;
  Result.Highlighter := FSQLEditorSynMemo.Highlighter;
  Result.Gutter.AutoSize := FSQLEditorSynMemo.Gutter.AutoSize;
  Result.Gutter.DigitCount := FSQLEditorSynMemo.Gutter.DigitCount;
  Result.Gutter.LeftOffset := FSQLEditorSynMemo.Gutter.LeftOffset;
  Result.Gutter.ShowLineNumbers := FSQLEditorSynMemo.Gutter.ShowLineNumbers;
  Result.Keystrokes := FSQLEditorSynMemo.Keystrokes;
  Result.MaxScrollWidth := FSQLEditorSynMemo.MaxScrollWidth;
  Result.OnChange := FSQLEditorSynMemo.OnChange;
  Result.OnDragDrop := FSQLEditorSynMemo.OnDragDrop;
  Result.OnDragOver := FSQLEditorSynMemo.OnDragOver;
  Result.OnEnter := FSQLEditorSynMemo.OnEnter;
  Result.OnExit := FSQLEditorSynMemo.OnExit;
  Result.OnSearchNotFound := FSQLEditorSynMemo.OnSearchNotFound;
  Result.OnStatusChange := FSQLEditorSynMemo.OnStatusChange;
  Result.PopupMenu := FSQLEditorSynMemo.PopupMenu;
  Result.RightEdge := FSQLEditorSynMemo.RightEdge;
  Result.ScrollHintFormat := FSQLEditorSynMemo.ScrollHintFormat;
  Result.SearchEngine := FSQLEditorSynMemo.SearchEngine;

  SynMemoApllyPreferences(Result);

  Result.Parent := PSynMemo;

  Result.Perform(CM_PARENTCOLORCHANGED, 0, 0);
  Result.Perform(CM_PARENTFONTCHANGED, 0, 0);
  Result.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
  Result.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  Result.Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  Result.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    Window.ApplyWinAPIUpdates(Result, NonClientMetrics.lfStatusFont);
end;

function TFClient.CreateTCResult(const PDBGrid: TPanel_Ext): TTabControl;
var
  NonClientMetrics: TNonClientMetrics;
begin
  Result := TTabControl.Create(Owner);

  Result.Left := 0;
  Result.Top := 0;
  Result.Width := PDBGrid.ClientWidth;
  Result.Height := 24;
  Result.Align := alTop;

  Result.Parent := PDBGrid;

  Result.TabHeight := 23;

  Result.Perform(CM_PARENTCOLORCHANGED, 0, 0);
  Result.Perform(CM_PARENTFONTCHANGED, 0, 0);
  Result.ParentShowHint := False;
  Result.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  Result.Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  Result.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    Window.ApplyWinAPIUpdates(Result, NonClientMetrics.lfStatusFont);
end;

function TFClient.CreateWorkbench(const ADatabase: TCDatabase): TWWorkbench;
begin
  Result := TWWorkbench.Create(Owner, ADatabase);

  Result.Left := 0;
  Result.Top := 0;
  Result.Width := PWorkbench.ClientWidth;
  Result.Height := PWorkbench.ClientHeight;
  Result.Align := alClient;
  Result.BorderStyle := bsNone;
  Result.HelpContext := 1125;
  Result.HideSelection := True;
  Result.HorzScrollBar.Tracking := True;
  Result.MultiSelect := True;
  Result.OnChange := WorkbenchChange;
  Result.OnDblClick := ListViewDblClick;
  Result.OnDragOver := WorkbenchDragOver;
  Result.OnDragDrop := WorkbenchDragDrop;
  Result.OnEnter := WorkbenchEnter;
  Result.OnExit := WorkbenchExit;
  Result.OnCursorMove := WorkbenchCursorMove;
  Result.OnValidateControl := WorkbenchValidateControl;
  Result.ParentFont := True;
  Result.PopupMenu := MWorkbench;
  Result.VertScrollBar.Tracking := True;

  Result.Parent := PWorkbench;

  Result.Perform(CM_PARENTCOLORCHANGED, 0, 0);
  Result.Perform(CM_PARENTFONTCHANGED, 0, 0);
  Result.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
  Result.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  Result.Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  Result.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);
end;

function TFClient.Desktop(const Database: TCDatabase): TDatabaseDesktop;
begin
  Result := TDatabaseDesktop(Database.Desktop);
end;

function TFClient.Desktop(const Event: TCEvent): TEventDesktop;
begin
  Result := TEventDesktop(Event.Desktop);
end;

function TFClient.Desktop(const Routine: TCRoutine): TRoutineDesktop;
begin
  Result := TRoutineDesktop(Routine.Desktop);
end;

function TFClient.Desktop(const Table: TCTable): TTableDesktop;
begin
  Result := TTableDesktop(Table.Desktop);
end;

function TFClient.Desktop(const Trigger: TCTrigger): TTriggerDesktop;
begin
  Result := TTriggerDesktop(Trigger.Desktop);
end;

function TFClient.Desktop(const View: TCView): TViewDesktop;
begin
  Result := TViewDesktop(View.Desktop);
end;

procedure TFClient.DataSetAfterCancel(DataSet: TDataSet);
begin
  DBGridColEnter(ActiveDBGrid);
end;

procedure TFClient.DataSetAfterOpen(DataSet: TDataSet);
begin
  if (Assigned(ActiveDBGrid) and (ActiveDBGrid.DataSource.DataSet = DataSet)) then
  begin
    ActiveDBGrid.DataSource.Enabled := False;
    DBGridInitialize(ActiveDBGrid);
    PContentChange(nil);
  end;
end;

procedure TFClient.DataSetAfterClose(DataSet: TDataSet);
begin
  PBlob.Visible := False; SBlob.Visible := PBlob.Visible;
  if (PResult.Align = alClient) then
  begin
    PResult.Align := alBottom;
    PResult.Height := PResultHeight;
    if (PBuilder.Visible) then PBuilder.Align := alClient;
    if (PSynMemo.Visible) then PSynMemo.Align := alClient;
  end;

  PResult.Visible := False; SResult.Visible := False;
  PBuilder.Update(); // TSynMemo aktualisiert leider nicht sofort nach Änderung von TSynMemo.Align
  PSynMemo.Update(); // TSynMemo aktualisiert leider nicht sofort nach Änderung von TSynMemo.Align

  aDPrev.Enabled := False;
  aDNext.Enabled := False;

  MainAction('aFExportSQL').Enabled := False;
  MainAction('aFExportText').Enabled := False;
  MainAction('aFExportExcel').Enabled := False;
  MainAction('aFExportSQLite').Enabled := False;
  MainAction('aFExportXML').Enabled := False;
  MainAction('aFExportHTML').Enabled := False;
end;

procedure TFClient.DataSetAfterPost(DataSet: TDataSet);
begin
  DataSetAfterScroll(DataSet);

  StatusBarRefresh();
end;

procedure TFClient.DataSetAfterScroll(DataSet: TDataSet);
var
  InputDataSet: Boolean;
begin
  if (not DataSet.ControlsDisabled) then
  begin
    InputDataSet := (DataSet is TMySQLDataSet) and (TMySQLDataSet(DataSet).CachedUpdates);

    if (((Window.ActiveControl = ActiveDBGrid) or (Window.ActiveControl = FText) or (Window.ActiveControl = FRTF) or (Window.ActiveControl = FHexEditor)) and (TMySQLQuery(DataSet).FieldCount > 0)) then
      DBGridColEnter(ActiveDBGrid)
    else if (FObjectIDEGrid.DataSource.DataSet = DataSet) then
      DBGridColEnter(FObjectIDEGrid);

    aDPrev.Enabled := not DataSet.Bof and not InputDataSet;
    aDNext.Enabled := not DataSet.Eof and not InputDataSet;
    MainAction('aDInsertRecord').Enabled := aDInsertRecord.Enabled and (DataSet.State in [dsBrowse, dsEdit]) and (DataSet.FieldCount > 0) and Assigned(ActiveDBGrid) and (ActiveDBGrid.SelectedRows.Count < 1) and not InputDataSet;
    MainAction('aDDeleteRecord').Enabled := aDDeleteRecord.Enabled and (DataSet.State in [dsBrowse, dsEdit]) and not DataSet.IsEmpty() and not InputDataSet;

    StatusBarRefresh();
//    PostMessage(Handle, CM_POSTSCROLL, 0, 0);
  end;
end;

procedure TFClient.DataSetBeforeCancel(DataSet: TDataSet);
begin
  if (PBlob.Visible) then
  begin
    DBGridColEnter(ActiveDBGrid);
    if (PResult.Visible and Assigned(ActiveDBGrid)) then
      Window.ActiveControl := ActiveDBGrid;
  end;
end;

procedure TFClient.DataSetBeforePost(DataSet: TDataSet);
begin
  if (PBlob.Visible and aVBlobText.Checked) then
    FTextExit(DataSetPost);
end;

procedure TFClient.DBGridColEnter(Sender: TObject);
var
  DBGrid: TMySQLDBGrid;
begin
  if (Sender is TMySQLDBGrid) then
  begin
    DBGrid := TMySQLDBGrid(Sender);

    if ((((Window.ActiveControl = DBGrid) or (Window.ActiveControl = FText) or (Window.ActiveControl = FRTF) or (Window.ActiveControl = FHexEditor)) and Assigned(DBGrid.SelectedField)) or (Sender = DataSetCancel)) then
    begin
      FText.OnChange := nil;

      EditorField := nil;
      if (DBGrid.SelectedField.DataType in [ftWideMemo, ftBlob]) then
        EditorField := DBGrid.SelectedField;
      if (Assigned(EditorField) xor PBlob.Visible) then
      begin
        PostMessage(DBGrid.Handle, WM_SIZE, SIZE_MAXIMIZED, DBGrid.Height shl 16 + DBGrid.Width);
        PContentChange(DBGrid);
      end;

      if (Assigned(EditorField)) then
      begin
        if (EditorField.DataType = ftBlob) then
          FImageShow(Sender);

        aVBlobText.Visible := not GeometryField(EditorField) and ((EditorField.DataType = ftWideMemo) or not Assigned(FImage.Picture.Graphic));
        aVBlobRTF.Visible := aVBlobText.Visible and (EditorField.DataType = ftWideMemo) and not EditorField.IsNull and IsRTF(EditorField.AsString);
        aVBlobHTML.Visible := aVBlobText.Visible and (EditorField.DataType = ftWideMemo) and not EditorField.IsNull and IsHTML(EditorField.AsString);
        aVBlobImage.Visible := (EditorField.DataType = ftBlob) and Assigned(FImage.Picture.Graphic);
        ToolBarBlobResize(Sender);

        if (aVBlobText.Visible and aVBlobText.Checked) then
          aVBlobExecute(nil)
        else if (aVBlobRTF.Visible and aVBlobRTF.Checked) then
          aVBlobExecute(nil)
        else if (aVBlobHTML.Visible and aVBlobHTML.Checked) then
          aVBlobExecute(nil)
        else if (aVBlobImage.Visible and aVBlobImage.Checked) then
          aVBlobExecute(nil)
        else if (aVBlobHexEditor.Visible and aVBlobHexEditor.Checked) then
          aVBlobExecute(nil)
        else if (aVBlobText.Visible) then
          aVBlobText.Execute()
        else if (aVBlobRTF.Visible) then
          aVBlobRTF.Execute()
        else if (aVBlobHTML.Visible) then
          aVBlobHTML.Execute()
        else if (aVBlobImage.Visible) then
          aVBlobImage.Execute()
        else
          aVBlobHexEditor.Execute();
      end;

      if (not Assigned(DBGrid.SelectedField) or (DBGrid.SelectedField.DataType in [ftWideMemo, ftBlob]) or (DBGrid.SelectedField.DataType in [ftUnknown])) then
        DBGrid.Options := DBGrid.Options - [dgEditing]
      else
        DBGrid.Options := DBGrid.Options + [dgEditing];

      FText.OnChange := FTextChange;
    end;

    DBGrid.UpdateAction(MainAction('aEPaste'));
    MainAction('aECopyToFile').Enabled := (DBGrid.SelectedField.DataType in [ftWideMemo, ftBlob]) and (not DBGrid.SelectedField.IsNull) and (DBGrid.SelectedRows.Count <= 1);
    MainAction('aEPasteFromFile').Enabled := (DBGrid.SelectedField.DataType in [ftWideMemo, ftBlob]) and not DBGrid.SelectedField.ReadOnly and (DBGrid.SelectedRows.Count <= 1);
    MainAction('aDCreateField').Enabled := Assigned(DBGrid.SelectedField) and (View = vBrowser);
    MainAction('aDEditRecord').Enabled := Assigned(DBGrid.SelectedField) and (View <> vIDE);
    MainAction('aDEmpty').Enabled := (Assigned(DBGrid.DataSource.DataSet) and DBGrid.DataSource.DataSet.CanModify and Assigned(DBGrid.SelectedField) and not DBGrid.SelectedField.IsNull and not DBGrid.SelectedField.Required and (DBGrid.SelectedRows.Count <= 1));
  end;

  StatusBarRefresh();
end;

procedure TFClient.DBGridColExit(Sender: TObject);
var
  Trigger: TCTrigger;
begin
  MainAction('aECopyToFile').Enabled := False;
  MainAction('aEPasteFromFile').Enabled := False;
  MainAction('aDCreateField').Enabled := False;
  MainAction('aDEditRecord').Enabled := False;
  MainAction('aDEmpty').Enabled := False;

  if ((Sender = FObjectIDEGrid) and (SelectedImageIndex = iiTrigger) and (TMySQLDBGrid(Sender).DataSource.DataSet is TMySQLDataSet)) then
  begin
    Trigger := TCTrigger(FNavigator.Selected.Data);

    FObjectIDEGrid.DataSource.DataSet.CheckBrowseMode();

    BINSERT.Enabled := Trigger.SQLInsert() <> '';
    BREPLACE.Enabled := Trigger.SQLReplace() <> '';
    BUPDATE.Enabled := Trigger.SQLUpdate() <> '';
    BDELETE.Enabled := Trigger.SQLDelete() <> '';
  end;

  gmFilter.Clear();
end;

procedure TFClient.DBGridColumnMoved(Sender: TObject; FromIndex: Integer;
  ToIndex: Integer);
begin
  IgnoreFGridTitleClick := IgnoreFGridTitleClick or (FromIndex <> ToIndex);
end;

procedure TFClient.DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);

  function ColorAdd(const Color1, Color2: TColor): TColor;
  begin
    if (Color2 > 0) then
      Result :=
        Color1 and $ff0000 or Color2 and $ff0000
        + Color1 and $00ff00 or Color2 and $00ff00
        + Color1 and $0000ff or Color2 and $0000ff
    else
      Result :=
        Color1 and $ff0000 and not (-Color2 and $ff0000)
        + Color1 and $00ff00 and not (-Color2 and $00ff00)
        + Color1 and $0000ff and not (-Color2 and $0000ff);
  end;

var
  BGRect: TRect;
  DBGrid: TMySQLDBGrid;
  Text: string;
  TextRect: TRect;
begin
  if (Sender is TMySQLDBGrid) then
  begin
    DBGrid := TMySQLDBGrid(Sender);

    if (not Assigned(Column.Field)) then
      Text := ''
    else if (Column.Field.IsNull) then
      if (not Column.Field.Required and Preferences.GridNullText) then
        Text := '<NULL>'
      else
        Text := ''
    else if (GeometryField(Column.Field)) then
      Text := '<GEO>'
    else if (Column.Field.DataType = ftBlob) then
      Text := '<BLOB>'
    else if (Column.Field.DataType = ftWideMemo) then
      if (not Preferences.GridShowMemoContent) then
        Text := '<MEMO>'
      else
        Text := Copy(TMySQLQuery(Column.Field.DataSet).GetAsString(Column.Field.FieldNo), 1, 1000)
    else
      Text := TMySQLQuery(Column.Field.DataSet).GetAsString(Column.Field.FieldNo);

    TextRect := Rect;
    InflateRect(TextRect, -2, -2);
    if (Column.Alignment = taRightJustify) then
      TextRect.Left := Max(TextRect.Left, TextRect.Right - DBGrid.Canvas.Textwidth(Text));

    if (DBGrid.Focused and DBGrid.SelectedRows.CurrentRowSelected) then
    begin // Row is selected, Grid is focused
      DBGrid.Canvas.Font.Color := clHighlightText;
      DBGrid.Canvas.Brush.Color := clHighlight;
    end
    else if (not DBGrid.Focused and DBGrid.SelectedRows.CurrentRowSelected) then
    begin // Row is selected, Grid is NOT focused
      DBGrid.Canvas.Font.Color := clBtnText;
      DBGrid.Canvas.Brush.Color := clBtnFace;
    end
    else if (gdFocused in State) then
    begin // Cell is focused, Grid is focused
      DBGrid.Canvas.Font.Color := clHighlightText;
      DBGrid.Canvas.Brush.Color := clHighlight;

      DBGrid.Canvas.Pen.Style := psDot;
      DBGrid.Canvas.Pen.Mode := pmNotCopy;
      DBGrid.Canvas.Rectangle(Rect);

      BGRect := Rect; InflateRect(BGRect, -1, -1);
      DBGrid.Canvas.Pen.Style := psSolid;
      DBGrid.Canvas.Pen.Mode := pmCopy;
    end
    else if (not DBGrid.Focused and (Column.Field = DBGrid.SelectedField) and DBGrid.CurrentRow and not DBGrid.Focused and (dgAlwaysShowSelection in DBGrid.Options)) then
    begin // Cell is focused, Grid is NOT focused
      DBGrid.Canvas.Font.Color := clBtnText;
      DBGrid.Canvas.Brush.Color := clBtnFace;
    end
    else if ((DBGrid.Parent <> PObjectIDE) and Preferences.GridCurrRowBGColorEnabled and DBGrid.CurrentRow) then
    begin // Row is focused
      DBGrid.Canvas.Font.Color := clWindowText;
      DBGrid.Canvas.Brush.Color := ColorToRGB(Preferences.GridCurrRowBGColor)
    end
    else if ((DBGrid.Parent <> PObjectIDE) and Preferences.GridNullBGColorEnabled and Assigned(Column.Field) and Column.Field.IsNull and not Column.Field.Required) then
    begin // Cell is NULL
      DBGrid.Canvas.Font.Color := clGrayText;
      DBGrid.Canvas.Brush.Color := Preferences.GridNullBGColor;
    end
    else if (Assigned(Column.Field) and (Column.Field.Tag and ftSortedField <> 0) and (not CheckWin32Version(6) or StyleServices.Enabled) and not CheckWin32Version(6, 1)) then
    begin // Column is sorted
      DBGrid.Canvas.Font.Color := clWindowText;
      if (ColorToRGB(clWindow) >= $800000) then
        DBGrid.Canvas.Brush.Color := ColorAdd(ColorToRGB(clWindow), -$101010)
      else
        DBGrid.Canvas.Brush.Color := ColorAdd(ColorToRGB(clWindow), $101010);
    end
    else if ((DBGrid.Parent <> PObjectIDE) and Preferences.GridRowBGColorEnabled and (DBGrid.DataSource.DataSet.RecNo mod 2 <> 0) and not (dgRowSelect in DBGrid.Options)) then
    begin // Row is even
      DBGrid.Canvas.Font.Color := clWindowText;
      if (ColorToRGB(clWindow) >= $800000) then
        DBGrid.Canvas.Brush.Color := ColorAdd(ColorToRGB(clWindow), -$080808)
      else
        DBGrid.Canvas.Brush.Color := ColorAdd(ColorToRGB(clWindow), $080808);
    end
    else
    begin
      DBGrid.Canvas.Font.Color := clWindowText;
      DBGrid.Canvas.Brush.Color := clWindow;
    end;

    if (Assigned(Column.Field) and Column.Field.IsNull) then
      DBGrid.Canvas.Font.Color := clGrayText;
    DBGrid.Canvas.FillRect(Rect);
    DBGrid.Canvas.TextRect(TextRect, TextRect.Left, TextRect.Top, Text);
  end;
end;

procedure TFClient.DBGridCopyToExecute(Sender: TObject);
const
  ChunkSize = 32768;
var
  BytesRead: DWord;
  BytesToWrite: DWord;
  BytesWritten: DWord;
  CodePage: Cardinal;
  FileBuffer: array[0 .. (ChunkSize - 1) * 3] of AnsiChar;
  Handle: THandle;
  Stream: TStream;
  StreamBuffer: array[0 .. ChunkSize - 1] of Byte;
  Success: Boolean;
begin
  SaveDialog.Title := ReplaceStr(Preferences.LoadStr(582), '&', '');
  SaveDialog.InitialDir := Path;
  if (ActiveDBGrid.SelectedField.DataType = ftWideMemo) then
  begin
    SaveDialog.FileName := ActiveDBGrid.SelectedField.DisplayName + '.txt';
    SaveDialog.DefaultExt := 'txt';
    SaveDialog.Filter := FilterDescription('txt') + ' (*.txt)|*.txt' + '|' + FilterDescription('*') + ' (*.*)|*.*';
    SaveDialog.Encodings.Text := EncodingCaptions();
    SaveDialog.EncodingIndex := SaveDialog.Encodings.IndexOf(CodePageToEncoding(Client.CodePage));
  end
  else if (ActiveDBGrid.SelectedField.DataType in [ftWideMemo, ftBlob]) then
  begin
    SaveDialog.FileName := ActiveDBGrid.SelectedField.DisplayName;
    SaveDialog.DefaultExt := '';
    SaveDialog.Filter := FilterDescription('*') + ' (*.*)|*.*';
    SaveDialog.Encodings.Clear();
    SaveDialog.EncodingIndex := -1;
  end
  else
    Exit;

  if (SaveDialog.Execute()) then
  begin
    Path := ExtractFilePath(SaveDialog.FileName);

    Handle := CreateFile(PChar(SaveDialog.FileName),
                         GENERIC_WRITE,
                         FILE_SHARE_READ,
                         nil,
                         CREATE_ALWAYS, 0, 0);
    if (Handle = INVALID_HANDLE_VALUE) then
      MsgBox(SysErrorMessage(GetLastError()), Preferences.LoadStr(45), MB_OK + MB_ICONERROR)
    else
    begin
      if (ActiveDBGrid.SelectedField.DataType in BinaryDataTypes) then
        CodePage := 0
      else
        CodePage := EncodingToCodePage(SaveDialog.Encodings[SaveDialog.EncodingIndex]);

      Stream := ActiveDBGrid.SelectedField.DataSet.CreateBlobStream(ActiveDBGrid.SelectedField, bmRead);

      if (ActiveDBGrid.SelectedField.DataType = ftWideMemo) then
        case (CodePage) of
          CP_UNICODE: WriteFile(Handle, BOM_UNICODE^, Length(BOM_UNICODE), BytesWritten, nil);
          CP_UTF8: WriteFile(Handle, BOM_UTF8^, Length(BOM_UTF8), BytesWritten, nil);
        end;

      repeat
        BytesRead := Stream.Read(StreamBuffer, SizeOf(StreamBuffer));
        if (BytesRead = 0) then
        begin
          BytesToWrite := 0;
          BytesWritten := 0;
          Success := True;
        end
        else if ((ActiveDBGrid.SelectedField.DataType = ftBlob) or (CodePage = CP_UNICODE)) then
        begin
          BytesToWrite := BytesRead;
          Success := WriteFile(Handle, StreamBuffer, BytesToWrite, BytesWritten, nil);
        end
        else
        begin
          BytesToWrite := WideCharToMultiByte(CodePage, 0, PWideChar(@StreamBuffer), BytesRead div SizeOf(WideChar), nil, 0, nil, nil);
          if (BytesToWrite < 1) or (SizeOf(FileBuffer) < BytesToWrite) then
            raise ERangeError.Create(SRangeError);
          WideCharToMultiByte(CodePage, 0, PWideChar(@StreamBuffer), BytesRead div SizeOf(WideChar), @FileBuffer, SizeOf(FileBuffer), nil, nil);
          Success := WriteFile(Handle, FileBuffer, BytesToWrite, BytesWritten, nil);
        end;
      until ((BytesToWrite = 0) or (BytesWritten <> BytesToWrite));

      Stream.Free();

      if (not Success) then
        MsgBox(SysErrorMessage(GetLastError), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);

      CloseHandle(Handle);
    end;
  end;
end;

procedure TFClient.DBGridDataSourceDataChange(Sender: TObject; Field: TField);
begin
  if ((Window.ActiveControl = ActiveDBGrid) and (Field = ActiveDBGrid.SelectedField) and (ActiveDBGrid.SelectedField.DataType in [ftWideMemo, ftBlob])) then
    aVBlobExecute(nil);
end;

procedure TFClient.DBGridDblClick(Sender: TObject);
begin
  Wanted.Clear();

  if (ActiveDBGrid.DataSource.DataSet.CanModify) then
    if (not (ActiveDBGrid.SelectedField.DataType in [ftWideMemo, ftBlob])) then
      ActiveDBGrid.EditorMode := True
    else if (aVBlobText.Visible) then
    begin
      aVBlobText.Checked := True;
      aVBlobExecute(nil);
      FText.SelStart := FText.SelStart + FText.SelLength;
      SendMessage(FText.Handle, WM_VSCROLL, SB_BOTTOM, 0);
      Window.ActiveControl := FText;
    end
    else
    begin
      aVBlobHexEditor.Checked := True;
      aVBlobExecute(nil);
      FHexEditor.SelStart := FHexEditor.DataSize - 1;
      SendMessage(FHexEditor.Handle, WM_VSCROLL, SB_BOTTOM, 0);
      Window.ActiveControl := FHexEditor;
    end;
end;

procedure TFClient.DBGridEditExecute(Sender: TObject);
begin
  Wanted.Clear();

  DBGridDblClick(Sender);
end;

procedure TFClient.DBGridEmptyExecute(Sender: TObject);
begin
  Wanted.Clear();

  FText.Lines.Clear();
  FHexEditor.DataSize := 0;
  if (Assigned(FImage.Picture.Graphic)) then
    FImage.Picture.Graphic := nil;

  if (not ActiveDBGrid.SelectedField.IsNull) then
  begin
    ActiveDBGrid.DataSource.DataSet.Edit();
    if (ActiveDBGrid.EditorMode) then
      ActiveDBGrid.InplaceEditor.Text := '';
    ActiveDBGrid.SelectedField.Clear();
  end;
  DBGridColEnter(ActiveDBGrid);
end;

procedure TFClient.DBGridEnter(Sender: TObject);
var
  DBGrid: TMySQLDBGrid;
  FieldInfo: TFieldInfo;
  Found: Boolean;
  I: Integer;
  SQL: string;
  Table: TCBaseTable;
begin
  if (Sender is TMySQLDBGrid) then
  begin
    if (View = vIDE) then SQL := SQLTrimStmt(ActiveSynMemo.Text) else SQL := '';

    DBGrid := TMySQLDBGrid(Sender);

    Found := False;
    for I := 0 to DBGrid.DataSource.DataSet.FieldCount - 1 do
      Found := Found or GetFieldInfo(DBGrid.DataSource.DataSet.Fields[I].Origin, FieldInfo) and (FieldInfo.TableName <> '');
    MainAction('aFExportSQL').Enabled := Found;
    MainAction('aFExportText').Enabled := True;
    MainAction('aFExportExcel').Enabled := True;
    MainAction('aFExportXML').Enabled := True;
    MainAction('aFExportHTML').Enabled := True;
    MainAction('aFPrint').Enabled := True;
    MainAction('aECopyToFile').OnExecute := DBGridCopyToExecute;
    MainAction('aEPasteFromFile').OnExecute := aEPasteFromFileExecute;
    MainAction('aSGoto').OnExecute := DBGridGotoExecute;
    MainAction('aDEditRecord').OnExecute := DBGridEditExecute;
    MainAction('aDEmpty').OnExecute := DBGridEmptyExecute;

    MainAction('aVDetails').Enabled := True;

    MainAction('aERename').ShortCut := 0;

    MainAction('aDEditRecord').ShortCut := VK_F2;

    MainAction('aSGoto').Enabled := False;
    if ((View = vBrowser) and (SelectedImageIndex in [iiBaseTable, iiSystemView])) then
    begin
      Table := TCBaseTable(FNavigator.Selected.Data);
      for I := 0 to Table.Keys.Count - 1 do
        if (Table.Keys[I].Unique) then
          MainAction('aSGoto').Enabled := True;
    end;

    DataSetAfterScroll(DBGrid.DataSource.DataSet);
  end;
end;

procedure TFClient.DBGridExit(Sender: TObject);
var
  Cancel: Boolean;
  DBGrid: TMySQLDBGrid;
begin
  if (Sender is TMySQLDBGrid) then
  begin
    DBGrid := TMySQLDBGrid(Sender);

    try
      DBGrid.DataSource.DataSet.CheckBrowseMode();
      Cancel := False;
    except
      Cancel := True;
    end;

    if (not Cancel) then
    begin
      DBGridColExit(Sender);
      DBGrid.Repaint();

      MainAction('aFExportSQL').Enabled := False;
      MainAction('aFExportText').Enabled := False;
      MainAction('aFExportExcel').Enabled := False;
      MainAction('aFExportXML').Enabled := False;
      MainAction('aFExportHTML').Enabled := False;
      MainAction('aFImportText').Enabled := False;
      MainAction('aFImportExcel').Enabled := False;
      MainAction('aFImportAccess').Enabled := False;
      MainAction('aFImportSQLite').Enabled := False;
      MainAction('aFImportODBC').Enabled := False;
      MainAction('aFImportXML').Enabled := False;
      MainAction('aFPrint').Enabled := False;
      MainAction('aECopyToFile').Enabled := False;
      MainAction('aEPasteFromFile').Enabled := False;
      MainAction('aSGoto').Enabled := False;
      MainAction('aVDetails').Enabled := False;
      MainAction('aDInsertRecord').Enabled := False;
      MainAction('aDDeleteRecord').Enabled := False;
      MainAction('aDEditRecord').Enabled := False;
      MainAction('aDPostObject').Enabled := False;
      MainAction('aDEmpty').Enabled := False;

      MainAction('aDEditRecord').ShortCut := 0;

      MainAction('aERename').ShortCut := VK_F2;
    end;
  end;
end;

procedure TFClient.DBGridGotoExecute(Sender: TObject);
var
  I: Integer;
  Key: TCKey;
  Line: Integer;
  Table: TCBaseTable;
begin
  Wanted.Clear();

  Table := TCBaseTable(FNavigator.Selected.Data);
  Key := nil;
  if (Assigned(Table) and (ActiveDBGrid.DataSource.DataSet = Table.DataSet) and (Table.Keys.Count >= 0)) then
    Key := Table.Keys[0];

  if (Assigned(Key) and Key.Unique) then
  begin
    DGoto.Captions := '';
    for I := 0 to Key.Columns.Count - 1 do
    begin
      if (DGoto.Captions <> '') then DGoto.Captions := DGoto.Captions + ';';
      DGoto.Captions := DGoto.Captions + Key.Columns.Column[I].Field.Name;
    end;
    if (DGoto.Execute()) then
      if (not ActiveDBGrid.DataSource.DataSet.Locate(DGoto.Captions, DGoto.Values, [loCaseInsensitive])) then
        MsgBox(Preferences.LoadStr(677), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION)
      else
        for I := ActiveDBGrid.Columns.Count - 1 downto 0 do
          if (ActiveDBGrid.Columns[I].Field.FieldName = Key.Columns.Column[0].Field.Name) then
            ActiveDBGrid.SelectedField := ActiveDBGrid.Columns[I].Field;
  end
  else
  begin
    DGoto.Captions := Preferences.LoadStr(678);
    if (DGoto.Execute()) then
      if (not TryStrToInt(DGoto.Values[0], Line)) then
        MessageBeep(MB_ICONERROR)
      else if (not (Line in [1 .. ActiveDBGrid.DataSource.DataSet.RecordCount])) then
        MessageBeep(MB_ICONERROR)
      else
        ActiveDBGrid.DataSource.DataSet.RecNo := Line - 1;
  end;
end;

procedure TFClient.DBGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_APPS) then
    ActiveDBGrid.PopupMenu := MGrid
  else if ((Key = VK_INSERT) and (Shift = []) and not ActiveDBGrid.EditorMode) then
    MainAction('aDInsertRecord').Execute()
  else if ((Key = VK_DELETE) and (Shift = [ssCtrl]) and not ActiveDBGrid.EditorMode) then
  begin
    MainAction('aDDeleteRecord').Execute();
    Key := 0;
  end
  else if (ActiveDBGrid.SelectedField.DataType in [ftWideMemo, ftBlob]) then
    if ((Key = VK_RETURN) and not aVBlobText.Visible) then
    begin
      aVBlobHexEditor.Checked := True;
      SendMessage(FText.Handle, WM_VSCROLL, SB_BOTTOM, 0);
    end
    else if (not (Key in [VK_F2, VK_TAB, VK_DOWN, VK_UP, VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_PRIOR, VK_NEXT, VK_APPS, VK_SHIFT, VK_CONTROL, VK_MENU])) then
    begin
      aVBlobText.Checked := True;
      if (Key = VK_RETURN) then
      begin
        SendMessage(FText.Handle, WM_VSCROLL, SB_BOTTOM, 0);
        PostMessage(Handle, CM_ACTIVATEFTEXT, 0, 0);
      end
      else if (Key = VK_DELETE) then
        SendMessage(FText.Handle, WM_CLEAR, 0, 0)
      else
        PostMessage(Handle, CM_ACTIVATEFTEXT, Key, 0);

      Key := 0;
    end;
end;

procedure TFClient.DBGridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  GridCoord: TGridCoord;
  DBGrid: TMySQLDBGrid;
begin
  inherited;

  if (Sender is TMySQLDBGrid) then
  begin
    DBGrid := TMySQLDBGrid(Sender);

    if (not (ssLeft in Shift) and DBGrid.Dragging()) then
      DBGrid.EndDrag(False);

    GridCoord := DBGrid.MouseCoord(X, Y);
    if ((GridCoord.X >= 0) and (GridCoord.Y = 0)) then
      DBGrid.PopupMenu := MGridHeader
    else
      DBGrid.PopupMenu := MGrid;
  end;
end;

procedure TFClient.DBGridTitleClick(Column: TColumn);
var
  FieldName: string;
  OldDescending: Boolean;
  Pos: Integer;
  SortDef: TIndexDef;
begin
  if (not IgnoreFGridTitleClick
    and not (ActiveDBGrid.Fields[Column.Index].DataType in [ftUnknown, ftWideMemo, ftBlob])) then
  begin
    SortDef := TIndexDef.Create(nil, 'SortDef', '', []);

    OldDescending := True;

    Pos := 1;
    repeat
      FieldName := ExtractFieldName(TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).SortDef.Fields, Pos);
      if (FieldName <> '') then
        if (FieldName <> ActiveDBGrid.Fields[Column.Index].FieldName) then
        begin
          if (SortDef.Fields <> '') then SortDef.Fields := SortDef.Fields + ';';
          SortDef.Fields := SortDef.Fields + FieldName;
        end
        else
          OldDescending := False;
    until (FieldName = '');

    Pos := 1;
    repeat
      FieldName := ExtractFieldName(TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).SortDef.DescFields, Pos);
      if (FieldName <> '') then
        if (FieldName <> ActiveDBGrid.Fields[Column.Index].FieldName) then
        begin
          if (SortDef.DescFields <> '') then SortDef.DescFields := SortDef.DescFields + ';';
          SortDef.DescFields := SortDef.DescFields + FieldName;
        end
        else
          OldDescending := True;
    until (FieldName = '');

    if (ssShift in ActiveDBGrid.MouseDownShiftState) then
    begin
      if (SortDef.Fields <> '') then SortDef.Fields := ';' + SortDef.Fields;
      SortDef.Fields := ActiveDBGrid.Fields[Column.Index].FieldName + SortDef.Fields;
      if (not OldDescending) then
      begin
        if (SortDef.DescFields <> '') then SortDef.DescFields := ';' + SortDef.DescFields;
        SortDef.DescFields := ActiveDBGrid.Fields[Column.Index].FieldName + SortDef.DescFields;
      end;
    end
    else if (ssCtrl in ActiveDBGrid.MouseDownShiftState) then
    begin
      if (SortDef.Fields <> '') then SortDef.Fields := SortDef.Fields + ';';
      SortDef.Fields := SortDef.Fields + ActiveDBGrid.Fields[Column.Index].FieldName;
      if (not OldDescending) then
      begin
        if (SortDef.DescFields <> '') then SortDef.DescFields := SortDef.DescFields + ';';
        SortDef.DescFields := SortDef.DescFields + ActiveDBGrid.Fields[Column.Index].FieldName;
      end;
    end
    else
    begin
      SortDef.Fields := ActiveDBGrid.Fields[Column.Index].FieldName;
      if (not OldDescending) then
        SortDef.DescFields := ActiveDBGrid.Fields[Column.Index].FieldName;
    end;

    TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).Sort(SortDef);
    ActiveDBGrid.UpdateHeader();

    SortDef.Free();
  end;

  IgnoreFGridTitleClick := False;
end;

procedure TFClient.DBGridInitialize(const DBGrid: TMySQLDBGrid);
var
  I: Integer;
  MenuItem: TMenuItem;
  SortMenuItem: TMenuItem;
  Table: TCTable;
begin
  DBGrid.DataSource.DataSet.AfterClose := DataSetAfterClose;
  DBGrid.DataSource.DataSet.AfterScroll := DataSetAfterScroll;
  DBGrid.DataSource.DataSet.BeforePost := DataSetBeforePost;
  DBGrid.DataSource.DataSet.AfterPost := DataSetAfterPost;
  DBGrid.DataSource.DataSet.BeforeCancel := DataSetBeforeCancel;
  DBGrid.DataSource.DataSet.AfterCancel := DataSetAfterCancel;
  DBGrid.DataSource.DataSet.OnDeleteError := SQLError;
  DBGrid.DataSource.DataSet.OnEditError := SQLError;
  DBGrid.DataSource.DataSet.OnPostError := SQLError;

  MGridHeader.Items.Clear();

  for I := 0 to DBGrid.Columns.Count - 1 do
    if (Assigned(DBGrid.Columns[I].Field)) then
    begin
      MenuItem := TMenuItem.Create(Self);
      MenuItem.Caption := DBGrid.Columns[I].Field.DisplayLabel;
      MenuItem.OnClick := MGridTitleMenuVisibleClick;
      MGridHeader.Items.Add(MenuItem);
    end;

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := '-';
  MenuItem.Tag := -1;
  MGridHeader.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Action := MainAction('aVDetails');
  MenuItem.Tag := -1;
  MGridHeader.Items.Add(MenuItem);

  if ((View = vBrowser) and (SelectedImageIndex in [iiBaseTable, iiSystemView])) then
  begin
    Table := TCBaseTable(FNavigator.Selected.Data);

    if (TCBaseTable(Table).Keys.Count > 0) then
    begin
      MenuItem := TMenuItem.Create(Self);
      MenuItem.Caption := '-';
      MenuItem.Tag := -1;
      MGridHeader.Items.Add(MenuItem);

      SortMenuItem := TMenuItem.Create(Self);
      SortMenuItem.Caption := Preferences.LoadStr(672);
      MenuItem.Tag := -1;
      MGridHeader.Items.Add(SortMenuItem);

      for I := 0 to TCBaseTable(Table).Keys.Count - 1 do
      begin
        MenuItem := TMenuItem.Create(Self);
        MenuItem.Caption := TCBaseTable(Table).Keys[I].Caption;
        MenuItem.Default := TCBaseTable(Table).Keys[I].Primary;
        MenuItem.Tag := I;
        MenuItem.RadioItem := True;
        MenuItem.OnClick := MGridHeaderMenuOrderClick;
        SortMenuItem.Add(MenuItem);
      end;
    end;
  end;

  DBGrid.DataSource.Enabled := True;

  DBGrid.Columns.BeginUpdate();
  for I := 0 to DBGrid.Columns.Count - 1 do
    if (Assigned(DBGrid.Columns[I].Field)) then
    begin
      if (DBGrid.Columns[I].Width > Preferences.GridMaxColumnWidth) then
        DBGrid.Columns[I].Width := Preferences.GridMaxColumnWidth;

      DBGrid.Columns[I].Field.OnSetText := FieldSetText;
    end;
  DBGrid.Columns.EndUpdate();

  DBGridColEnter(DBGrid);

  SResult.Visible := PResult.Visible and (PBuilder.Visible or PSynMemo.Visible);
end;

destructor TFClient.Destroy();
var
  TempB: Boolean;
  URI: TUURI;
begin
  FNavigatorChanging(nil, nil, TempB);

  Window.ActiveControl := nil;

  FNavigator.Items.BeginUpdate();
  FNavigator.Items.Clear();
  FNavigator.Items.EndUpdate();

  try
    if (Assigned(ShellLink)) then ShellLink.Free();
    if (Assigned(FFolders)) then FFolders.Free();
    if (Assigned(FFiles)) then FFiles.Free();
  except
      // There is a bug inside ShellBrowser.pas ver. 7.3 - but it's not interested to get informed
  end;

  FServerListView.OnChanging := nil;
  FServerListView.Items.BeginUpdate();
  FServerListView.Items.Clear();
  FServerListView.Items.EndUpdate();
  if (Assigned(HostsListView)) then FreeListView(HostsListView);
  if (Assigned(ProcessesListView)) then FreeListView(ProcessesListView);
  if (Assigned(StatiListView)) then FreeListView(StatiListView);
  if (Assigned(UsersListView)) then FreeListView(UsersListView);
  if (Assigned(VariablesListView)) then FreeListView(VariablesListView);

  if (Assigned(SQLEditor)) then SQLEditor.Free();


  if ((SQLEditor.Filename <> '') and not FSQLEditorSynMemo.Modified) then
    Client.Account.Desktop.EditorContent := ''
  else
    Client.Account.Desktop.EditorContent := FSQLEditorSynMemo.Text;
  Client.Account.Desktop.FoldersHeight := PFolders.Height;
  if (Assigned(FFiles)) then
    try
      Client.Account.Desktop.FilesFilter := FFiles.Filter;
    except
      // There is a bug inside ShellBrowser.pas ver. 7.3 - but it's not interested to get informed
    end;
  Client.Account.Desktop.NavigatorVisible := PNavigator.Visible;
  Client.Account.Desktop.BookmarksVisible := PBookmarks.Visible;
  Client.Account.Desktop.ExplorerVisible := PExplorer.Visible;
  Client.Account.Desktop.SQLHistoryVisible := PSQLHistory.Visible;
  Client.Account.Desktop.SelectorWitdth := PSideBar.Width;
  Client.Account.Desktop.LogVisible := PLog.Visible;
  Client.Account.Desktop.LogHeight := PLog.Height;
  URI := TUURI.Create(Address);
  URI.Param['file'] := Null;
  URI.Param['cp'] := Null;
  Client.Account.Desktop.Address := URI.Address;
  FreeAndNil(URI);

  if (PResult.Align <> alBottom) then
    Client.Account.Desktop.DataHeight := PResultHeight
  else
    Client.Account.Desktop.DataHeight := PResult.Height;
  Client.Account.Desktop.BlobHeight := PBlob.Height;

  Client.Account.Desktop.AddressMRU.Assign(ToolBarData.AddressMRU);
  Client.Account.UnRegisterDesktop(Self);

  Client.UnRegisterEventProc(FormClientEvent);
  Client.Free();

  FreeAndNil(JPEGImage);
  FreeAndNil(PNGImage);
  FreeAndNil(GIFImage);

  FLog.Lines.Clear();

  FreeAndNil(CloseButton);

  FreeAndNil(FilterMRU);
  FreeAndNil(ToolBarData.AddressMRU);
  FreeAndNil(ToolBarData.Addresses);
  FreeAndNil(Wanted);

  inherited;
end;

function TFClient.Dragging(const Sender: TObject): Boolean;
begin
  Result := LeftMousePressed and (Window.ActiveControl = FNavigator) and ((Window.ActiveControl as TTreeView_Ext).Selected <> MouseDownNode);
end;

procedure TFClient.EndEditLabel(Sender: TObject);
begin
  aDCreate.ShortCut := VK_INSERT;
  aDDelete.ShortCut := VK_DELETE;
end;

procedure TFClient.FBlobResize(Sender: TObject);
begin
  FText.Repaint();
end;

procedure TFClient.FBlobSearchChange(Sender: TObject);
begin
  if (FBlobSearch.Text <> '') then
  begin
    TSearchFind_Ext(MainAction('aSSearchFind')).Control := FText;
    TSearchFind_Ext(MainAction('aSSearchFind')).Dialog.FindText := FBlobSearch.Text;
    TSearchFind_Ext(MainAction('aSSearchFind')).FindFirst := Assigned(Sender);
    TSearchFind_Ext(MainAction('aSSearchFind')).Search(Sender);
  end;
end;

procedure TFClient.FBlobSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if (Ord(Key) = VK_ESCAPE) then
  begin
    FBlobSearch.Text := '';
    Key := #0;
  end
  else if ((Ord(Key) = VK_RETURN) and (FText.Text <> '')) then
  begin
    FBlobSearchChange(nil);
    Key := #0;
  end;
end;

procedure TFClient.FBookmarksChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  mbBOpen.Enabled := Assigned(Item) and Item.Selected;
  aPOpenInNewWindow.Enabled := Assigned(Item) and Item.Selected;
  aPOpenInNewTab.Enabled := Assigned(Item) and Item.Selected;
  MainAction('aBDelete').Enabled := Assigned(Item) and Item.Selected;
  MainAction('aBEdit').Enabled := Assigned(Item) and Item.Selected;

  mbBOpen.Default := mbBOpen.Enabled;
end;

procedure TFClient.FBookmarksDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  TargetItem: TListItem;
begin
  TargetItem := TListView(Sender).GetItemAt(X, Y);

  Client.Account.Desktop.Bookmarks.MoveBookmark(Client.Account.Desktop.Bookmarks.ByCaption(FBookmarks.Selected.Caption), FBookmarks.Items.IndexOf(TargetItem));
end;

procedure TFClient.FBookmarksDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  TargetItem: TListItem;
begin
  TargetItem := TListView(Sender).GetItemAt(X, Y);

  Accept := (Sender = Source) and (TargetItem <> FBookmarks.Selected);
end;

procedure TFClient.FBookmarksEnter(Sender: TObject);
begin
  mbBOpen.ShortCut := VK_RETURN;
  MainAction('aBDelete').ShortCut := VK_DELETE;
  MainAction('aBEdit').ShortCut := ShortCut(VK_RETURN, [ssAlt]);

  FBookmarksChange(Sender, FBookmarks.Selected, ctState);
end;

procedure TFClient.FBookmarksExit(Sender: TObject);
begin
  mbBOpen.ShortCut := 0;
  MainAction('aBDelete').ShortCut := 0;
  MainAction('aBEdit').ShortCut := 0;

  aPOpenInNewWindow.Enabled := False;
  aPOpenInNewTab.Enabled := False;
  MainAction('aBDelete').Enabled := False;
  MainAction('aBEdit').Enabled := False;
end;

function TFClient.FBuilderActiveSelectList(): TacQueryBuilderSelectListControl;
begin
  if (not Assigned(FBuilderEditorPageControl())) then
    Result := nil
  else
  begin
    Result := TacQueryBuilderSelectListControl(FindChildByClassType(FBuilderEditorPageControl().ActivePage, TacQueryBuilderSelectListControl));
    if (not Assigned(Result) and (FBuilderEditorPageControl().PageCount = 1)) then
      Result := TacQueryBuilderSelectListControl(FindChildByClassType(FBuilderEditorPageControl().Pages[0], TacQueryBuilderSelectListControl));
  end;
end;

function TFClient.FBuilderActiveWorkArea(): TacQueryBuilderWorkArea;
var
  PageControl: TPageControl;
begin
  if (not Assigned(FBuilderEditorPageControl())) then
    Result := nil
  else
  begin
    PageControl := FBuilderEditorPageControl();
    if (Assigned(PageControl.ActivePage)) then
      Result := TacQueryBuilderWorkArea(FindChildByClassType(PageControl.ActivePage, TacQueryBuilderWorkArea))
    else if (PageControl.PageCount = 1) then
      Result := TacQueryBuilderWorkArea(FindChildByClassType(PageControl.Pages[0], TacQueryBuilderWorkArea))
    else
      Result := nil;

    if (not Assigned(PageControl.OnChange)) then
    begin
      PageControl.OnChange := FBuilderEditorPageControlChange;
      FBuilderEditorPageControlChange(nil);
    end;
    FBuilderEditorPageControlCheckStyle();
  end;
end;

procedure TFClient.FBuilderAddTable(Sender: TObject);
var
  MenuItem: TMenuItem;
  SQLQualifiedName: TSQLQualifiedName;
  Table: TCTable;
begin
  if (Sender is TMenuItem) then
  begin
    MenuItem := TMenuItem(Sender);

    if ((MenuItem.GetParentMenu() is TPopupMenu) and (TObject(MenuItem.Tag) is TCTable)) then
    begin
      Table := TCTable(TMenuItem(Sender).Tag);

      SQLQualifiedName := TSQLQualifiedName.Create(FBuilder.MetadataContainer.SQLContext);

      SQLQualifiedName.AddName(Table.Name);
      FBuilder.ActiveSubQuery.ActiveUnionSubquery.AddObjectAt(SQLQualifiedName, FBuilderActiveWorkArea().ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint));

      SQLQualifiedName.Free();
    end;
  end;
end;

procedure TFClient.FBuilderDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Database: TCDatabase;
  Node: TTreeNode;
  SQLQualifiedName: TSQLQualifiedName;
begin
  if ((Source = FNavigator) and (MouseDownNode.ImageIndex in [iiBaseTable, iiSystemView, iiView])) then
  begin
    Node := MouseDownNode;

    Database := TCDatabase(Node.Parent.Data);

    SQLQualifiedName := TSQLQualifiedName.Create(FBuilder.MetadataContainer.SQLContext);
    if (Database <> TCDatabase(FNavigator.Selected.Data)) then
      SQLQualifiedName.AddPrefix(Database.Name);

    SQLQualifiedName.AddName(TCTable(Node.Data).Name);
    FBuilder.ActiveSubQuery.ActiveUnionSubquery.AddObjectAt(SQLQualifiedName, Point(X, Y));

    SQLQualifiedName.Free();
  end;
end;

procedure TFClient.FBuilderDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  SourceNode: TTreeNode;
begin
  Accept := False;

  if (Source = FNavigator) then
  begin
    SourceNode := MouseDownNode;

    Accept := SourceNode.ImageIndex in [iiBaseTable, iiSystemView, iiView];
  end
end;

procedure TFClient.FBuilderEditorChange(Sender: TObject);
begin
  FBuilder.Enabled := True;
  try
    FBuilder.SQL := FBuilderSynMemo.Lines.Text;
    PostMessage(Handle, CM_POST_BUILDER_QUERY_CHANGE, 0, 0);

    FBuilderEditorPageControlCheckStyle();

    FBuilder.Visible := True;
  except
    FBuilder.Visible := False;
  end;

  FBuilderEditorStatusChange(Sender, [scModified]);
end;

procedure TFClient.FBuilderEditorEnter(Sender: TObject);
begin
  SQLBuilder.OnSQLUpdated := nil;

  SynMemoEnter(Sender);
end;

procedure TFClient.FBuilderEditorExit(Sender: TObject);
begin
  SynMemoExit(Sender);

  SQLBuilder.OnSQLUpdated := FBuilderSQLUpdated;
end;

function TFClient.FBuilderEditorPageControl(): TacPageControl;
begin
  Result := TacQueryBuilderPageControl(FindChildByClassType(FBuilder, TacQueryBuilderPageControl));
end;

procedure TFClient.FBuilderEditorPageControlChange(Sender: TObject);
begin
  if (not Assigned(FBuilderEditorPageControl().ActivePage.OnEnter)) then
    FBuilderEditorPageControl().ActivePage.OnEnter := FBuilderEditorTabSheetEnter;
end;

procedure TFClient.FBuilderEditorPageControlCheckStyle();
var
  PageControl: TPageControl;
begin
  PageControl := FBuilderEditorPageControl();
  if (PBuilder.Visible and Assigned(PageControl)) then
  begin
    if ((FBuilder.SubQueries.Count = 1) and PageControl.Pages[0].TabVisible) then
    begin
      PageControl.Style := tsFlatButtons;
      PageControl.Pages[0].TabVisible := False;
      PageControl.Pages[0].Visible := True;
    end
    else if ((FBuilder.SubQueries.Count > 1) and not PageControl.Pages[0].TabVisible) then
    begin
      PageControl.Style := tsTabs;
      PageControl.Pages[0].TabVisible := True;
    end;
  end;
end;

procedure TFClient.FBuilderEditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  BevelWidth: Integer;
  LineCount: Integer;
  NewHeight: Integer;
  ScrollBarInfo: TScrollBarInfo;
begin
  FBuilderResize(Sender);

  BevelWidth := 0;
  if (PBuilderQuery.BevelInner in [bvLowered, bvRaised]) then
    Inc(BevelWidth, PBuilderQuery.BevelWidth);
  if (PBuilderQuery.BevelOuter in [bvLowered, bvRaised]) then
    Inc(BevelWidth, PBuilderQuery.BevelWidth);

  ZeroMemory(@ScrollBarInfo, SizeOf(ScrollBarInfo));
  ScrollBarInfo.cbSize := SizeOf(ScrollBarInfo);
  GetScrollBarInfo(FBuilderSynMemo.Handle, Integer(OBJID_HSCROLL), ScrollBarInfo);

  LineCount := FBuilderSynMemo.Lines.Count;
  if (LineCount = 0) then
    LineCount := 1;

  if (ScrollBarInfo.rgstate[0] = STATE_SYSTEM_INVISIBLE) then
    NewHeight := LineCount * (FBuilderSynMemo.Canvas.TextHeight('SELECT') + 1) + 2 * FBuilderSynMemo.Top + 2 * BevelWidth
  else
    NewHeight := LineCount * (FBuilderSynMemo.Canvas.TextHeight('SELECT') + 1) + 2 * FBuilderSynMemo.Top + 2 * BevelWidth + GetSystemMetrics(SM_CYHSCROLL);
  PBuilderQuery.Constraints.MaxHeight := NewHeight;

  if (NewHeight > (PBuilder.Height + PBuilderQuery.Height) div 3) then
    NewHeight := (PBuilder.Height + PBuilderQuery.Height) div 3;

  if ((Sender = FBuilder) or (NewHeight < PBuilderQuery.Height) or (scModified in Changes)) then
    PBuilderQuery.Height := NewHeight;

  SynMemoStatusChange(FBuilderSynMemo, Changes);
end;

procedure TFClient.FBuilderEditorTabSheetEnter(Sender: TObject);
begin
  StatusBarRefresh();
end;

procedure TFClient.FBuilderEnter(Sender: TObject);
begin
  FBuilderSynMemo.OnChange := nil;

  StatusBarRefresh();
end;

procedure TFClient.FBuilderExit(Sender: TObject);
begin
  FBuilderSynMemo.OnChange := FBuilderEditorChange;
end;

procedure TFClient.FBuilderResize(Sender: TObject);
var
  FBuilderEditorSelectList: TacQueryBuilderSelectListControl;
  I: Integer;
  PSQLEditorBuilderSelectList: TacPanel;
  ScrollBarInfo: TScrollBarInfo;
begin
  if (Assigned(FBuilderEditorPageControl())) then
    for I := 0 to FBuilderEditorPageControl().PageCount - 1 do
    begin
      FBuilderEditorSelectList := TacQueryBuilderSelectListControl(FindChildByClassType(FBuilderEditorPageControl().Pages[I], TacQueryBuilderSelectListControl));
      if (Assigned(FBuilderEditorSelectList) and (FBuilderEditorSelectList.Parent is TacPanel)) then
        PSQLEditorBuilderSelectList := TacPanel(FBuilderEditorSelectList.Parent)
      else
        PSQLEditorBuilderSelectList := nil;
      if (Assigned(PSQLEditorBuilderSelectList)) then
      begin
        ZeroMemory(@ScrollBarInfo, SizeOf(ScrollBarInfo));
        ScrollBarInfo.cbSize := SizeOf(ScrollBarInfo);
        GetScrollBarInfo(FBuilderEditorSelectList.Handle, Integer(OBJID_HSCROLL), ScrollBarInfo);

        if (ScrollBarInfo.rgstate[0] = STATE_SYSTEM_INVISIBLE) then
          PSQLEditorBuilderSelectList.Height := (FBuilderEditorSelectList.DefaultRowHeight + 2) + FBuilderEditorSelectList.SelectList.Count * (FBuilderEditorSelectList.DefaultRowHeight + 1) + 3
        else
          PSQLEditorBuilderSelectList.Height := (FBuilderEditorSelectList.DefaultRowHeight + 2) + FBuilderEditorSelectList.SelectList.Count * (FBuilderEditorSelectList.DefaultRowHeight + 1) + 3 + GetSystemMetrics(SM_CYHSCROLL);

        if (PSQLEditorBuilderSelectList.Height > FBuilder.Height div 2) then
          PSQLEditorBuilderSelectList.Height := FBuilder.Height div 2;
      end;
    end;
end;

procedure TFClient.FBuilderSQLUpdated(Sender: TObject);
var
  S: string;
  SQL: string;
begin
  FBuilderEditorPageControlCheckStyle();

  SQL := Trim(SQLBuilder.SQL);
  if (UpperCase(RightStr(SQL, 4)) = 'FROM') then
    SQL := Trim(LeftStr(SQL, Length(SQL) - 4));

  S := Trim(ReplaceStr(ReplaceStr(SQL, #13, ' '), #10, ' '));
  while (Pos('  ', S) > 0) do
    S := ReplaceStr(S, '  ', ' ');
  if (S = 'SELECT *') then
    FBuilderSynMemo.Lines.Text := ''
  else
  begin
    if (Length(SQL) < 80) then SQL := SQLUnwrapStmt(SQL);
    if (SQL = '') then
      FBuilderSynMemo.Lines.Clear()
    else
      FBuilderSynMemo.Lines.Text := SQL + ';';
  end;

  FBuilderEditorStatusChange(FBuilder, [scModified]);
end;

procedure TFClient.FBuilderValidatePopupMenu(Sender: TacQueryBuilder;
  AControlOwner: TacQueryBuilderControlOwner; AForControl: TControl;
  APopupMenu: TPopupMenu);
var
  I: Integer;
  J: Integer;
  MenuItem: TMenuItem;
begin
  if (AForControl.ClassType = TacQueryBuilderWorkArea) then
  begin
    for I := 0 to APopupMenu.Items.Count - 1 do
      if (APopupMenu.Items[I].Caption = QueryBuilderLocalizer.ReadWideString('acAddObject', acAddObject)) then
      begin
        APopupMenu.Items[I].Caption := Preferences.LoadStr(383);
        APopupMenu.Items[I].OnClick := nil;

        for J := 0 to TCDatabase(FNavigator.Selected.Data).Tables.Count - 1 do
        begin
          MenuItem := TMenuItem.Create(Self);
          MenuItem.Caption := TCDatabase(FNavigator.Selected.Data).Tables[J].Name;
          MenuItem.OnClick := FBuilderAddTable;
          MenuItem.Tag := Integer(TCDatabase(FNavigator.Selected.Data).Tables[J]);
          APopupMenu.Items[I].Add(MenuItem);
        end;
      end;
  end;
end;

procedure TFClient.FFilesEnter(Sender: TObject);
begin
  miHOpen.ShortCut := VK_RETURN;
end;

procedure TFClient.FFilterChange(Sender: TObject);
begin
  FFilter.Text := FFilter.Text;

  FFilterEnabled.Enabled := SQLSingleStmt(FFilter.Text);
  FFilterEnabled.Down := FFilterEnabled.Enabled and Assigned(ActiveDBGrid.DataSource.DataSet) and (FFilter.Text = TMySQLTable(ActiveDBGrid.DataSource.DataSet).FilterSQL);
end;

procedure TFClient.FFilterDropDown(Sender: TObject);
var
  I: Integer;
begin
  FFilter.Items.Clear();
  for I := FilterMRU.Count - 1 downto 0 do
    FFilter.Items.Add(FilterMRU.Values[I]);
end;

procedure TFClient.FFilterEnabledClick(Sender: TObject);
begin
  FQuickSearchEnabled.Down := False;
  TableOpen(Sender);
  Window.ActiveControl := FFilter;
end;

procedure TFClient.FFilterEnter(Sender: TObject);
begin
  if (FFilter.Items.Count = 0) then
    FFilterDropDown(Sender);
end;

procedure TFClient.FFilterKeyPress(Sender: TObject; var Key: Char);
begin
  if ((Key = Chr(VK_ESCAPE)) and (TMySQLTable(ActiveDBGrid.DataSource.DataSet).FilterSQL <> '')) then
  begin
    FFilter.Text := TMySQLTable(ActiveDBGrid.DataSource.DataSet).FilterSQL;
    FFilterChange(Sender);

    FFilter.SelStart := 0;
    FFilter.SelLength := Length(FFilter.Text);
    Key := #0;
  end
  else if ((Key = Chr(VK_RETURN)) and not FFilterEnabled.Down) then
  begin
    FFilterEnabled.Down := True;
    FFilterEnabledClick(Sender);

    FFilter.SelStart := 0;
    FFilter.SelLength := Length(FFilter.Text);
    Key := #0;
  end;
end;

procedure TFClient.FFoldersChange(Sender: TObject; Node: TTreeNode);
begin
  if (not (tsLoading in FrameState) and PExplorer.Visible and Visible) then
    if ((Sender is TJamShellTree) and not TJamShellTree(Sender).Visible) then
    begin
      TJamShellTree(Sender).Visible := True;
      Window.ActiveControl := TJamShellTree(Sender);
    end
    else
      PlaySound(PChar(Preferences.SoundFileNavigating), Handle, SND_FILENAME or SND_ASYNC);

  Path := FFolders.SelectedFolder;
end;

procedure TFClient.FHexEditorChange(Sender: TObject);
var
  Stream: TStream;
begin
  if (FHexEditor.Modified) then
  begin
    if (EditorField.DataSet.State <> dsEdit) then
      EditorField.DataSet.Edit();

    if (EditorField.DataType = ftWideMemo) then
      Stream := TStringStream.Create('')
    else if (EditorField.DataType = ftBlob) then
      Stream := EditorField.DataSet.CreateBlobStream(EditorField, bmWrite)
    else
      Stream := nil;

    if (Assigned(Stream)) then
    begin
      FHexEditor.SaveToStream(Stream);

      Stream.Free();
    end;
  end;

  aVBlobRTF.Visible := Assigned(EditorField) and (EditorField.DataType = ftWideMemo) and not EditorField.IsNull and IsRTF(EditorField.AsString);
end;

procedure TFClient.FHexEditorEnter(Sender: TObject);
begin
  StatusBarRefresh();
end;

procedure TFClient.FHexEditorKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  begin
    Window.ActiveControl := ActiveDBGrid;
    ActiveDBGrid.DataSource.DataSet.Cancel();
  end;
end;

procedure TFClient.FHexEditorShow(Sender: TObject);
var
  Stream: TStream;
begin
  FHexEditor.UnicodeChars := False;
  if (not EditorField.IsNull and (EditorField.DataType = ftBlob)) then
  begin
    Stream := EditorField.DataSet.CreateBlobStream(EditorField, bmRead);
    FHexEditor.BytesPerColumn := 1;
  end
  else if (not EditorField.IsNull and (EditorField.DataType = ftWideMemo)) then
  begin
    Stream := EditorField.DataSet.CreateBlobStream(EditorField, bmRead);
    FHexEditor.BytesPerColumn := 2;
  end
  else
    Stream := nil;

  if (not Assigned(Stream)) then
    FHexEditor.DataSize := 0
  else
  begin
    FHexEditor.LoadFromStream(Stream);
    FHexEditor.UnicodeChars := EditorField.DataType = ftWideMemo;
    FHexEditor.AllowInsertMode := True;
    FHexEditor.InsertMode := False;
    FHexEditor.ReadOnlyView := EditorField.ReadOnly or not EditorField.DataSet.CanModify;
    FHexEditor.SelectAll();
    Stream.Free();
  end;
end;

procedure TFClient.FHTMLHide(Sender: TObject);
begin
  if (Assigned(FHTML)) then
    FreeAndNil(FHTML);
end;

procedure TFClient.FHTMLShow(Sender: TObject);
var
  FilenameP: array [0 .. MAX_PATH] of Char;
  FileStream: TFileStream;
  Stream: TStream;
begin
  if (not Assigned(FHTML)) then
  begin
    FHTML := TWebBrowser.Create(Self);
    FHTML.Align := alClient;

    PBlob.InsertControl(FHTML);
  end;

  if (Assigned(FHTML)) then
  begin
    if (not EditorField.IsNull and (EditorField.DataType = ftWideMemo)) then
      Stream := TStringStream.Create(EditorField.AsString)
    else if (not EditorField.IsNull and (EditorField.DataType = ftBlob)) then
      Stream := EditorField.DataSet.CreateBlobStream(EditorField, bmRead)
    else
      Stream := nil;

    if (Assigned(Stream) and (GetTempPath(MAX_PATH, FilenameP) > 0) and (GetTempFileName(@FilenameP, '~MF', 0, @FilenameP) > 0)) then
    begin
      FileStream := TFileStream.Create(FilenameP, fmCreate or fmShareDenyWrite);
      if (Assigned(FileStream)) then
      begin
        FileStream.CopyFrom(Stream, 0);
        FileStream.Free();

        FHTML.Navigate(FilenameP);

        DeleteFile(string(FilenameP));
      end;

      Stream.Free();
    end;
  end;
end;

procedure TFClient.FieldSetText(Sender: TField; const Text: string);
begin
  try
    Sender.AsString := Text;
  except
    OnConvertError(Sender, Text);
    Abort();
  end;
end;

procedure TFClient.FImageShow(Sender: TObject);
var
  Buffer: array [0..9] of AnsiChar;
  Size: Integer;
  Stream: TStream;
begin
  if (EditorField.IsNull or not (EditorField.DataType in [ftWideMemo, ftBlob])) then
    Stream := nil
  else
    Stream := EditorField.DataSet.CreateBlobStream(EditorField, bmRead);

  if (not Assigned(Stream) or (Stream.Size = 0)) then
    Size := 0
  else
    begin Size := Stream.Read(Buffer, Length(Buffer)); Stream.Seek(0, soFromBeginning); end;

  try
    if ((Size > 3) and (Buffer[0] = 'G') and (Buffer[1] = 'I') and (Buffer[2] = 'F')) then
      try GIFImage.LoadFromStream(Stream); FImage.Picture.Graphic := GIFImage; except FImage.Picture.Graphic := nil; end
    else if ((Size >= 10) and (Buffer[6] = 'J') and (Buffer[7] = 'F') and (Buffer[8] = 'I') and (Buffer[9] = 'F')) then
      try JPEGImage.LoadFromStream(Stream); FImage.Picture.Graphic := JPEGImage; except FImage.Picture.Graphic := nil; end
    else if ((Size >= 4) and (Buffer[1] = 'P') and (Buffer[2] = 'N') and (Buffer[3] = 'G')) then
      try PNGImage.LoadFromStream(Stream); FImage.Picture.Graphic := PNGImage; except FImage.Picture.Graphic := nil; end
    else
      FImage.Picture.Graphic := nil;
  except
  end;
  if (Assigned(Stream)) then
    Stream.Free();
end;

procedure TFClient.FLimitChange(Sender: TObject);
begin
  FUDLimit.Position := FUDLimit.Position;

  FUDOffset.Increment := FUDLimit.Position;

  FOffsetChange(Sender);
end;

procedure TFClient.FLimitEnabledClick(Sender: TObject);
begin
  FQuickSearchEnabled.Down := False;

  TableOpen(Sender);

  Window.ActiveControl := FOffset;
end;

procedure TFClient.FLogEnter(Sender: TObject);
begin
  MainAction('aECopyToFile').OnExecute := SaveSQLFile;
  MainAction('aSGoto').OnExecute := TSymMemoGotoExecute;

  MainAction('aFPrint').Enabled := True;
  MainAction('aSSearchReplace').Enabled := False;
  MainAction('aSGoto').Enabled := True;

  MainAction('aHIndex').ShortCut := 0;
  MainAction('aHSQL').ShortCut := ShortCut(VK_F1, []);

  FLogSelectionChange(Sender);
  StatusBarRefresh();
end;

procedure TFClient.FLogExit(Sender: TObject);
begin
  MainAction('aFPrint').Enabled := False;
  MainAction('aECopyToFile').Enabled := False;
  MainAction('aSGoto').Enabled := False;

  MainAction('aHIndex').ShortCut := ShortCut(VK_F1, []);
  MainAction('aHSQL').ShortCut := 0;
end;

procedure TFClient.FLogSelectionChange(Sender: TObject);
begin
  if (PLog.Visible and (Window.ActiveControl = FLog)) then
    MainAction('aECopyToFile').Enabled := FLog.SelText <> '';
end;

procedure TFClient.FNavigatorAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
begin
  if ((Stage = cdPrePaint) and not (csDestroying in ComponentState) and Assigned(Node)
    and ((Node.ImageIndex = iiKey) and TCKey(Node.Data).Primary or (Node.ImageIndex = iiField) and TCTableField(Node.Data).InPrimaryKey)) then
    Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsBold]
  else
    Sender.Canvas.Font.Style := Sender.Canvas.Font.Style - [fsBold];
end;

procedure TFClient.FNavigatorChange(Sender: TObject; Node: TTreeNode);
begin
  if (not (tsLoading in FrameState)) then
  begin
    FNavigatorMenuNode := Node;

    KillTimer(Handle, tiNavigator);
    if (NavigatorElapse = 0) then
      FNavigatorChange2(Sender, Node)
    else
    begin
      SetTimer(Self.Handle, tiNavigator, NavigatorElapse, nil);
      NavigatorElapse := 0;
    end;
  end;
end;

procedure TFClient.FNavigatorChange2(Sender: TObject; Node: TTreeNode);
begin
  KillTimer(Handle, tiNavigator);

  if (Assigned(Node)) then
    Address := NavigatorNodeToAddress(Node)
  else
    Wanted.Address := NavigatorNodeToAddress(Node);
end;

procedure TFClient.FNavigatorChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  AllowChange := AllowChange and not Dragging(Sender) and not (Assigned(Node) and (Node.ImageIndex in [iiKey, iiField, iiSystemViewField, iiViewField, iiForeignKey]));

  if (AllowChange and Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and ActiveDBGrid.DataSource.DataSet.Active) then
    try
      if ((Window.ActiveControl = FText) or (Window.ActiveControl = FRTF) or (Window.ActiveControl = FHexEditor)) then
        Window.ActiveControl := ActiveDBGrid;
      ActiveDBGrid.DataSource.DataSet.CheckBrowseMode();
    except
      AllowChange := False;
    end;
end;

procedure TFClient.FNavigatorDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  I: Integer;
  List: TList;
  Objects: string;
  SourceNode: TTreeNode;
  TargetNode: TTreeNode;
begin
  if (Sender = FNavigator) then
    TargetNode := FNavigator.GetNodeAt(X, Y)
  else if (Sender = ActiveListView) then
    TargetNode := FNavigator.Selected
  else
    TargetNode := nil;

  List := TList.Create();

  if ((Source is TTreeView_Ext) and (TTreeView_Ext(Source).Name = FNavigator.Name)) then
  begin
    SourceNode := TFClient(TTreeView_Ext(Source).Owner).MouseDownNode;

    case (SourceNode.ImageIndex) of
      iiDatabase,
      iiSystemView: Objects := Objects + 'Database='    + SourceNode.Text + #13#10;
      iiBaseTable:  Objects := Objects + 'Table='       + SourceNode.Text + #13#10;
      iiView:       Objects := Objects + 'View='        + SourceNode.Text + #13#10;
      iiProcedure:  Objects := Objects + 'Procedure='   + SourceNode.Text + #13#10;
      iiFunction:   Objects := Objects + 'Function='    + SourceNode.Text + #13#10;
      iiEvent:      Objects := Objects + 'Event='       + SourceNode.Text + #13#10;
      iiKey:        Objects := Objects + 'Index='       + SourceNode.Text + #13#10;
      iiField,
      iiViewField:  Objects := Objects + 'Field='       + SourceNode.Text + #13#10;
      iiForeignKey: Objects := Objects + 'ForeignKey='  + SourceNode.Text + #13#10;
      iiTrigger:    Objects := Objects + 'Trigger='     + SourceNode.Text + #13#10;
      iiHost:       Objects := Objects + 'Host='        + SourceNode.Text + #13#10;
      iiUser:       Objects := Objects + 'User='        + SourceNode.Text + #13#10;
    end;
    if (Objects <> '') then
      Objects := 'Address=' + NavigatorNodeToAddress(SourceNode) + #13#10 + Objects;
  end
  else if ((Source is TListView) and (TListView(Source).Parent.Name = 'PListView')) then
  begin
    SourceNode := TFClient(TComponent(TListView(Source).Owner)).FNavigator.Selected;

    for I := 0 to TListView(Source).Items.Count - 1 do
      if (TListView(Source).Items[I].Selected) then
        case (TListView(Source).Items[I].ImageIndex) of
          iiDatabase,
          iiSystemView: Objects := Objects + 'Database='   + TListView(Source).Items[I].Caption + #13#10;
          iiBaseTable:  Objects := Objects + 'Table='      + TListView(Source).Items[I].Caption + #13#10;
          iiView:       Objects := Objects + 'View='       + TListView(Source).Items[I].Caption + #13#10;
          iiProcedure:  Objects := Objects + 'Procedure='  + TListView(Source).Items[I].Caption + #13#10;
          iiFunction:   Objects := Objects + 'Function='   + TListView(Source).Items[I].Caption + #13#10;
          iiEvent:      Objects := Objects + 'Event='      + TListView(Source).Items[I].Caption + #13#10;
          iiKey:        Objects := Objects + 'Key='      + TCKey(TListView(Source).Items[I].Data).Name + #13#10;
          iiField,
          iiViewField:  Objects := Objects + 'Field='      + TListView(Source).Items[I].Caption + #13#10;
          iiForeignKey: Objects := Objects + 'ForeignKey=' + TListView(Source).Items[I].Caption + #13#10;
          iiTrigger:    Objects := Objects + 'Trigger='    + TListView(Source).Items[I].Caption + #13#10;
          iiHost:       Objects := Objects + 'Host='       + TCHost(TListView(Source).Items[I].Data).Name + #13#10;
          iiUser:       Objects := Objects + 'User='       + TListView(Source).Items[I].Caption + #13#10;
        end;
    if (Objects <> '') then
      Objects := 'Address=' + NavigatorNodeToAddress(SourceNode) + #13#10 + Objects;
  end;

  List.Free();

  if (Objects <> '') then
    PasteExecute(TargetNode, Objects);
end;

procedure TFClient.FNavigatorDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  SourceNode: TTreeNode;
  TargetNode: TTreeNode;
begin
  Accept := False;

  TargetNode := TTreeView_Ext(Sender).GetNodeAt(X, Y);

  if (Source is TTreeView_Ext and (TTreeView_Ext(Source).Name = FNavigator.Name)) then
  begin
    SourceNode := TFClient(TTreeView_Ext(Source).Owner).MouseDownNode;
    if (Assigned(TargetNode) and (TargetNode <> SourceNode)) then
      case (SourceNode.ImageIndex) of
        iiDatabase: Accept := (TargetNode = TTreeView_Ext(Sender).Items.getFirstNode()) and (TargetNode <> SourceNode.Parent);
        iiBaseTable: Accept := (TargetNode.ImageIndex = iiDatabase) and (TargetNode <> SourceNode.Parent) or (TargetNode.ImageIndex = iiBaseTable) and (TargetNode <> SourceNode);
        iiProcedure,
        iiFunction: Accept := (TargetNode.ImageIndex = iiDatabase) and (TargetNode <> SourceNode.Parent);
        iiField: Accept := (TargetNode.ImageIndex = iiBaseTable) and (TargetNode <> SourceNode.Parent);
      end;
  end
  else if ((Source is TListView) and (TListView(Source).Parent.Name = PListView.Name)) then
    Accept := ((TFClient(TListView(Source).Owner).Client <> Client) or (TFClient(TListView(Source).Owner).FNavigator.Selected <> TargetNode))
      and (TFClient(TListView(Source).Owner).FNavigator.Selected.ImageIndex = SelectedImageIndex);
end;

procedure TFClient.FNavigatorEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  if (not RenameCItem(FocusedCItem, S)) then
    S := Node.Text;
end;

procedure TFClient.FNavigatorEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := (Node.ImageIndex = iiDatabase) and (Client.ServerVersion >= 50107) or (Node.ImageIndex = iiForeignKey) and (Client.ServerVersion >= 40013) or (Node.ImageIndex in [iiBaseTable, iiView, iiEvent, iiTrigger, iiField]);
end;

procedure TFClient.FNavigatorEmptyExecute(Sender: TObject);
var
  Database: TCDatabase;
  Field: TCBaseTableField;
  List: TList;
  Table: TCBaseTable;
begin
  Wanted.Clear();

  if ((FocusedCItem is TCDatabase) and (Sender is TAction)) then
  begin
    Database := TCDatabase(FocusedCItem);
    if (not Database.Update()) then
      Wanted.Action := TAction(Sender)
    else if (MsgBox(Preferences.LoadStr(374, Database.Name), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
      Database.EmptyTables();
  end
  else if (FocusedCItem is TCBaseTable) then
  begin
    Table := TCBaseTable(FocusedCItem);
    if (MsgBox(Preferences.LoadStr(375, Table.Name), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
      Table.Empty();
  end
  else if (FocusedCItem is TCBaseTableField) then
  begin
    Field := TCBaseTableField(FocusedCItem);
    Table := Field.Table;
    if (Assigned(Field) and (MsgBox(Preferences.LoadStr(376, Field.Name), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES)) then
    begin
      List := TList.Create();
      List.Add(Field);
      Table.EmptyFields(List);
      List.Free();
    end;
  end;
end;

procedure TFClient.FNavigatorEnter(Sender: TObject);
begin
  MainAction('aDEmpty').OnExecute := FNavigatorEmptyExecute;

  aDDelete.ShortCut := VK_DELETE;

  FNavigatorSetMenuItems(Sender, FNavigator.Selected);
  StatusBarRefresh();
end;

procedure TFClient.FNavigatorExit(Sender: TObject);
begin
  MainAction('aFImportSQL').Enabled := False;
  MainAction('aFImportText').Enabled := False;
  MainAction('aFImportExcel').Enabled := False;
  MainAction('aFImportAccess').Enabled := False;
  MainAction('aFImportSQLite').Enabled := False;
  MainAction('aFImportODBC').Enabled := False;
  MainAction('aFImportXML').Enabled := False;
  MainAction('aFExportSQL').Enabled := False;
  MainAction('aFExportText').Enabled := False;
  MainAction('aFExportExcel').Enabled := False;
  MainAction('aFExportAccess').Enabled := False;
  MainAction('aFExportSQLite').Enabled := False;
  MainAction('aFExportODBC').Enabled := False;
  MainAction('aFExportXML').Enabled := False;
  MainAction('aFExportHTML').Enabled := False;
  MainAction('aFPrint').Enabled := False;
  MainAction('aECopy').Enabled := False;
  MainAction('aEPaste').Enabled := False;
  MainAction('aERename').Enabled := False;
  MainAction('aDCreateDatabase').Enabled := False;
  MainAction('aDCreateTable').Enabled := False;
  MainAction('aDCreateView').Enabled := False;
  MainAction('aDCreateProcedure').Enabled := False;
  MainAction('aDCreateFunction').Enabled := False;
  MainAction('aDCreateEvent').Enabled := False;
  MainAction('aDCreateTrigger').Enabled := False;
  MainAction('aDCreateKey').Enabled := False;
  MainAction('aDCreateField').Enabled := False;
  MainAction('aDCreateForeignKey').Enabled := False;
  MainAction('aDCreateHost').Enabled := False;
  MainAction('aDCreateUser').Enabled := False;
  MainAction('aDDeleteDatabase').Enabled := False;
  MainAction('aDDeleteTable').Enabled := False;
  MainAction('aDDeleteView').Enabled := False;
  MainAction('aDDeleteRoutine').Enabled := False;
  MainAction('aDDeleteEvent').Enabled := False;
  MainAction('aDDeleteKey').Enabled := False;
  MainAction('aDDeleteField').Enabled := False;
  MainAction('aDDeleteForeignKey').Enabled := False;
  MainAction('aDDeleteTrigger').Enabled := False;
  MainAction('aDEditServer').Enabled := False;
  MainAction('aDEditDatabase').Enabled := False;
  MainAction('aDEditTable').Enabled := False;
  MainAction('aDEditView').Enabled := False;
  MainAction('aDEditRoutine').Enabled := False;
  MainAction('aDEditEvent').Enabled := False;
  MainAction('aDEditTrigger').Enabled := False;
  MainAction('aDEditKey').Enabled := False;
  MainAction('aDEditField').Enabled := False;
  MainAction('aDEditForeignKey').Enabled := False;
  MainAction('aDEditHost').Enabled := False;
  MainAction('aDEditProcess').Enabled := False;
  MainAction('aDEditUser').Enabled := False;
  MainAction('aDEditVariable').Enabled := False;
  MainAction('aDEmpty').Enabled := False;

  aDDelete.ShortCut := 0;
end;

procedure TFClient.FNavigatorExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  Database: TCDatabase;
  Table: TCTable;
begin
  if (Node.HasChildren) then
  begin
    Database := nil;
    Table := nil;
    case (Node.ImageIndex) of
      iiDatabase,
      iiSystemDatabase:
        begin
          Database := TCDatabase(Node.Data);
          AllowExpansion := AllowExpansion and Database.Update();
        end;
      iiBaseTable,
      iiSystemView,
      iiView:
        begin
          Table := TCTable(Node.Data);
          AllowExpansion := AllowExpansion and Table.Update();
        end;
    end;

    if (not AllowExpansion and (Sender = FNavigator)) then
      FNavigatorNodeToExpand := Node;
    if (Node.HasChildren and (Node.Count = 0)) then
    begin
      FNavigatorNodeToExpand := Node;
      if (Assigned(Table)) then
        Table.PushBuildEvent()
      else if (Assigned(Database)) then
        Database.PushBuildEvents();
    end;
  end;
end;

procedure TFClient.FNavigatorInitialize(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := FNavigator.Items.getFirstNode().getFirstChild();
  while (Assigned(Node)) do
  begin
    case (Node.ImageIndex) of
      iiHosts: Node.Text := Preferences.LoadStr(335);
      iiProcesses: Node.Text := Preferences.LoadStr(24);
      iiStati: Node.Text := Preferences.LoadStr(23);
      iiUsers: Node.Text := ReplaceStr(Preferences.LoadStr(561), '&', '');
      iiVariables: Node.Text := Preferences.LoadStr(22);
    end;
    Node := Node.getNextSibling();
  end;
end;

procedure TFClient.FNavigatorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (not TTreeView(Sender).IsEditing()) then
    if ((Key = Ord('C')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssCtrl])) then
      begin aECopyExecute(Sender); Key := 0; end
    else if ((Key = Ord('V')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssShift])) then
      begin aEPasteExecute(Sender); Key := 0; end
    else if (not (Key in [VK_SHIFT, VK_CONTROL])) then
      NavigatorElapse := 500;
end;

procedure TFClient.FNavigatorKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #3) then Key := #0; // Why is threre a Beep on <Ctrl+C> without this?
end;

function TFClient.FNavigatorNodeByAddress(const Address: string): TTreeNode;
var
  AllowExpansion: Boolean;
  Child: TTreeNode;
  DatabaseNode: TTreeNode;
  TableName: string;
  TableNode: TTreeNode;
  URI: TUURI;
begin
  URI := TUURI.Create(Address);

  Result := nil;

  if (URI.Param['system'] <> Null) then
  begin
    Child := FNavigator.Items.getFirstNode().getFirstChild();
    while (Assigned(Child) and not Assigned(Result)) do
      if ((URI.Param['system'] = 'hosts') and (Child.ImageIndex = iiHosts)
        or (URI.Param['system'] = 'processes') and (Child.ImageIndex = iiProcesses)
        or (URI.Param['system'] = 'stati') and (Child.ImageIndex = iiStati)
        or (URI.Param['system'] = 'users') and (Child.ImageIndex = iiUsers)
        or (URI.Param['system'] = 'variables') and (Child.ImageIndex = iiVariables)) then
        Result := Child
      else
        Child := Child.getNextSibling();
  end
  else if (URI.Database <> '') then
  begin
    Child := FNavigator.Items.getFirstNode().getFirstChild(); DatabaseNode := nil;
    while (Assigned(Child) and not Assigned(DatabaseNode)) do
      if ((Child.ImageIndex in [iiDatabase, iiSystemDatabase]) and (Client.Databases.NameCmp(URI.Database, Child.Text) = 0)) then
        DatabaseNode := Child
      else
        Child := Child.getNextSibling();

    if ((URI.Table = '') and ((URI.Param['objecttype'] = Null) or (URI.Param['object'] = Null))) then
      Result := DatabaseNode
    else
    begin
      if (DatabaseNode.HasChildren and (DatabaseNode.Count = 0)) then
      begin
        AllowExpansion := True;
        FNavigatorExpanding(nil, DatabaseNode, AllowExpansion);
      end;

      Child := DatabaseNode.getFirstChild();
      if ((URI.Table <> '') or (URI.Param['objecttype'] = 'trigger') and (URI.Param['object'] <> Null)) then
      begin
        if (URI.Table <> '') then
          TableName := URI.Table
        else
          TableName := TCDatabase(DatabaseNode.Data).TriggerByName(URI.Param['object']).TableName;
        TableNode := nil;
        while (Assigned(Child) and not Assigned(TableNode)) do
          if ((Child.ImageIndex in [iiBaseTable, iiSystemView, iiView]) and (TCDatabase(DatabaseNode.Data).Tables.NameCmp(TableName, Child.Text) = 0)) then
            TableNode := Child
          else
            Child := Child.getNextSibling();
        if ((URI.Param['objecttype'] <> 'trigger') or (URI.Param['object'] = Null)) then
          Result := TableNode
        else
        begin
          if (TableNode.HasChildren and (TableNode.Count = 0)) then
          begin
            AllowExpansion := True;
            FNavigatorExpanding(nil, TableNode, AllowExpansion);
          end;
          Child := TableNode.getFirstChild();
          while (Assigned(Child) and not Assigned(Result)) do
            if ((Child.ImageIndex in [iiTrigger]) and (TCDatabase(DatabaseNode.Data).Triggers.NameCmp(URI.Param['object'], Child.Text) = 0)) then
              Result := Child
            else
              Child := Child.getNextSibling();
        end;
      end
      else if (URI.Param['objecttype'] = 'procedure') then
        while (Assigned(Child) and not Assigned(Result)) do
          if ((Child.ImageIndex = iiProcedure) and (TCDatabase(DatabaseNode.Data).Routines.NameCmp(Child.Text, URI.Param['object']) = 0)) then
            Result := Child
          else
            Child := Child.getNextSibling()
      else if (URI.Param['objecttype'] = 'function') then
        while (Assigned(Child) and not Assigned(Result)) do
          if ((Child.ImageIndex = iiFunction) and (TCDatabase(DatabaseNode.Data).Routines.NameCmp(Child.Text, URI.Param['object']) = 0)) then
            Result := Child
          else
            Child := Child.getNextSibling()
      else if (URI.Param['objecttype'] = 'event') then
        while (Assigned(Child) and not Assigned(Result)) do
          if ((Child.ImageIndex = iiEvent) and (TCDatabase(DatabaseNode.Data).Events.NameCmp(Child.Text, URI.Param['object']) = 0)) then
            Result := Child
          else
            Child := Child.getNextSibling()
    end;
  end
  else
    Result := FNavigator.Items.getFirstNode();

  URI.Free();
end;

procedure TFClient.FNavigatorUpdate(const ClientEvent: TCClient.TEvent);

  function GroupIDByImageIndex(const ImageIndex: Integer): Integer;
  begin
    case (ImageIndex) of
      iiDatabase,
      iiSystemDatabase:
        Result := giDatabases;
      iiTable,
      iiBaseTable,
      iiSystemView,
      iiView:
        Result := giTables;
      iiProcedure,
      iiFunction:
        Result := giRoutines;
      iiEvent:
        Result := giEvents;
      iiKey:
        Result := giKeys;
      iiField,
      iiSystemViewField,
      iiViewField:
        Result := giFields;
      iiForeignKey:
        Result := giForeignKeys;
      iiTrigger:
        Result := giTriggers;
      iiHosts,
      iiProcesses,
      iiStati,
      iiUsers,
      iiVariables:
        Result := giSystemTools;
      iiHost:
        Result := giHosts;
      iiProcess:
        Result := giProcesses;
      iiStatus:
        Result := giStati;
      iiUser:
        Result := giUsers;
      iiVariable:
        Result := giVariables;
      else
        Result := 0;
    end;
  end;

  function Compare(const ImageIndex: Integer; const Item1, Item2: TTreeNode): Integer;
  const
    ImageIndexSort = Chr(iiHosts) + Chr(iiProcesses) + Chr(iiStati) + Chr(iiUsers) + Chr(iiVariables);
  begin
    if (GroupIDByImageIndex(Item1.ImageIndex) <> GroupIDByImageIndex(Item2.ImageIndex)) then
      Result := Sign(GroupIDByImageIndex(Item1.ImageIndex) - GroupIDByImageIndex(Item2.ImageIndex))
    else if (GroupIDByImageIndex(Item1.ImageIndex) = giSystemTools) then
      Result := Sign(Pos(Chr(Item1.ImageIndex), ImageIndexSort) - Pos(Chr(Item2.ImageIndex), ImageIndexSort))
    else if ((TObject(Item1.Data) is TCItem) and (TObject(Item2.Data) is TCItem)) then
      Result := Sign(TCItem(Item1.Data).Index - TCItem(Item2.Data).Index)
    else
      raise ERangeError.Create(SRangeError);
  end;

  procedure InsertChild(const Node: TTreeNode; const Data: TObject);
  var
    Added: Boolean;
    C: Integer;
    Child: TTreeNode;
    I: Integer;
    Index: Integer;
    Left: Integer;
    Mid: Integer;
    MidChild: TTreeNode;
    OldMid: Integer;
    Right: Integer;
    Text: string;
    VirtualChild: TTreeNode;
  begin
    Index := 0;
    Child := Node.getFirstChild();
    while (Assigned(Child) and (Child.Data <> Data)) do
    begin
      Child := Child.getNextSibling();
      Inc(Index);
    end;

    if ((Index = Node.Count) and (Node.Count > 0)) then
    begin
      VirtualChild := TTreeNode.Create(Node.Owner);
      VirtualChild.Data := Data;
      VirtualChild.ImageIndex := ImageIndexByData(Data);
      VirtualChild.Text := TCItem(Data).Caption;

      Left := 0; C := 0; OldMid := 0; MidChild := Node.getFirstChild();
      Right := Node.Count - 1;
      while (Left <= Right) do
      begin
        Mid := (Right - Left) div 2 + Left;
        if (Mid < OldMid) then
          for I := OldMid downto Mid + 1 do
            MidChild := MidChild.getPrevSibling()
        else if (Mid > OldMid) then
          for I := OldMid to Mid - 1 do
            MidChild := MidChild.getNextSibling();
        OldMid := Mid;
        C := Compare(Node.ImageIndex, MidChild, VirtualChild);
        case (C) of
          -1: begin Left := Mid + 1; Index := Mid + 1; end;
          0: raise ERangeError.Create(SRangeError);
          1: begin Right := Mid - 1; Index := Mid; end;
        end;
      end;
      if (C = -1) then
        Child := MidChild.getNextSibling()
      else
        Child := MidChild;

      VirtualChild.Free();
    end;

    if (TObject(Data) is TCItem) then
      Text := TCItem(Data).Caption
    else if (TObject(Data) is TCHosts) then
      Text := Preferences.LoadStr(335)
    else if (TObject(Data) is TCProcesses) then
      Text := Preferences.LoadStr(24)
    else if (TObject(Data) is TCStati) then
      Text := Preferences.LoadStr(23)
    else if (TObject(Data) is TCUsers) then
      Text := ReplaceStr(Preferences.LoadStr(561), '&', '')
    else if (TObject(Data) is TCVariables) then
      Text := Preferences.LoadStr(22)
    else
      raise ERangeError.Create(SRangeError);

    if (Index = Node.Count) then
    begin
      Child := FNavigator.Items.AddChild(Node, Text);
      Added := True;
    end
    else if (Child.Data <> Data) then
    begin
      Child := FNavigator.Items.Insert(Child, Text);
      Added := True;
    end
    else
      Added := False;
    Child.Data := Data;
    Child.ImageIndex := ImageIndexByData(Data);
    Child.Text := Text;
    if (Added and (Child.ImageIndex in [iiDatabase, iiSystemDatabase, iiSystemView, iiBaseTable, iiView])) then
      Child.HasChildren := True;
  end;

  procedure AddChild(const Node: TTreeNode; const Data: TObject);
  var
    Child: TTreeNode;
    Text: string;
  begin
    if (TObject(Data) is TCItem) then
      Text := TCItem(Data).Caption
    else if (TObject(Data) is TCHosts) then
      Text := Preferences.LoadStr(335)
    else if (TObject(Data) is TCProcesses) then
      Text := Preferences.LoadStr(24)
    else if (TObject(Data) is TCStati) then
      Text := Preferences.LoadStr(23)
    else if (TObject(Data) is TCUsers) then
      Text := ReplaceStr(Preferences.LoadStr(561), '&', '')
    else if (TObject(Data) is TCVariables) then
      Text := Preferences.LoadStr(22)
    else
      raise ERangeError.Create(SRangeError);

    Child := FNavigator.Items.AddChild(Node, Text);
    Child.Data := Data;
    Child.ImageIndex := ImageIndexByData(Data);
    Child.Text := Text;
    if (Child.ImageIndex in [iiDatabase, iiSystemDatabase, iiSystemView, iiBaseTable, iiView]) then
      Child.HasChildren := True;
  end;

  procedure UpdateGroup(const Node: TTreeNode; const GroupID: Integer; const CItems: TCItems);
  var
    Add: Boolean;
    Child: TTreeNode;
    DeleteChild: TTreeNode;
    Destination: TTreeNode;
    I: Integer;
  begin
    case (ClientEvent.EventType) of
      ceItemsValid:
        begin
          Child := Node.getFirstChild();
          while (Assigned(Child)) do
            if ((GroupIDByImageIndex(Child.ImageIndex) <> GroupID) or (CItems.IndexOf(Child.Data) >= 0)) then
              Child := Child.getNextSibling()
            else
            begin
              if (Child = FNavigatorNodeToExpand) then
                FNavigatorNodeToExpand := nil;
              DeleteChild := Child;
              Child := Child.getNextSibling();
              DeleteChild.Delete();
            end;

          Add := Node.Count = 0;
          for I := 0 to CItems.Count - 1 do
            if (not (CItems is TCTriggers) or (TCTriggers(CItems)[I].Table = ClientEvent.Sender)) then
              if (not Add) then
                InsertChild(Node, CItems[I])
              else
                AddChild(Node, CItems[I]);
        end;
      ceItemCreated:
        InsertChild(Node, ClientEvent.CItem);
      ceItemAltered:
        begin
          Child := Node.getFirstChild();
          while (Assigned(Child) and (Child.Data <> ClientEvent.CItem)) do
            Child := Child.getNextSibling();
          if (Assigned(Child) and (Child.Text <> ClientEvent.CItem.Caption)) then
          begin
            Child.Text := ClientEvent.CItem.Caption;
            Destination := Node.getFirstChild();
            while (Assigned(Destination) and ((Destination = Child) or (Compare(Node.ImageIndex, Destination, Child) < 0))) do
              Destination := Destination.getNextSibling();
            if (Assigned(Destination)) then
              Child.MoveTo(Destination, naInsert)
            else
              Child.MoveTo(Node, naAddChild);
          end;
        end;
      ceItemDropped:
        begin
          Child := Node.getFirstChild();
          while (Assigned(Child)) do
            if (Child.Data <> ClientEvent.CItem) then
              Child := Child.getNextSibling()
            else
            begin
              if (Child = FNavigatorNodeToExpand) then
                FNavigatorNodeToExpand := nil;
              DeleteChild := FNavigator.Selected;
              while (Assigned(DeleteChild)) do
              begin
                if (Child = DeleteChild) then
                  NavigatorElapse := 1; // We're inside a Monitor callback - but the related Address change has to be outside the callback
                DeleteChild := DeleteChild.Parent;
              end;
              DeleteChild := Child;
              Child := Child.getNextSibling();
              DeleteChild.Delete();
              if (not Assigned(FNavigator.Selected)) then
                FNavigator.Selected := Node;
            end;
        end;
    end;
  end;

var
  Child: TTreeNode;
  ChangeEvent: TTVChangedEvent;
  ChangingEvent: TTVChangingEvent;
  Database: TCDatabase;
  Expanded: Boolean;
  ExpandingEvent: TTVExpandingEvent;
  Node: TTreeNode;
  OldSelected: TTreeNode;
  Table: TCTable;
begin
  OldSelected := FNavigator.Selected;

  ChangingEvent := FNavigator.OnChanging; FNavigator.OnChanging := nil;
  ChangeEvent := FNavigator.OnChange; FNavigator.OnChange := nil;
  FNavigator.Items.BeginUpdate();

  if (ClientEvent.Sender is TCClient) then
  begin
    Node := FNavigator.Items.getFirstNode();

    if (Node.Count = 0) then
    begin
      if (Assigned(Client.Hosts)) then
        InsertChild(Node, Client.Hosts);
      if (Assigned(Client.Processes)) then
        InsertChild(Node, Client.Processes);
      InsertChild(Node, Client.Stati);
      if (Assigned(Client.Users)) then
        InsertChild(Node, Client.Users);
      InsertChild(Node, Client.Variables);
      Node.Expand(False);
    end;

    if (ClientEvent.CItems is TCDatabases) then
      UpdateGroup(Node, giDatabases, ClientEvent.CItems);
  end
  else if (ClientEvent.Sender is TCDatabase) then
  begin
    Database := TCDatabase(ClientEvent.Sender);

    Node := FNavigator.Items.getFirstNode().getFirstChild();
    while (Assigned(Node) and (Node.Data <> Database)) do Node := Node.getNextSibling();

    if (Assigned(Node) and (not Node.HasChildren or (Node.Count > 0) or (Node = FNavigatorNodeToExpand))) then
    begin
      if (not (ClientEvent.CItems is TCTriggers)) then
      begin
        UpdateGroup(Node, giTriggers, ClientEvent.CItems);

        Node.HasChildren := (Node.Count > 0)
          or not Database.Tables.Valid or (Database.Tables.Count > 0)
          or (Assigned(Database.Routines) and ((Database.Routines.Count > 0) or not Database.Routines.Valid))
          or (Assigned(Database.Events) and ((Database.Events.Count > 0) or not Database.Events.Valid));
      end
      else
      begin
        Child := Node.getFirstChild();
        while (Assigned(Child)) do
        begin
          if (Child.ImageIndex = iiBaseTable) then
            UpdateGroup(Child, giTables, ClientEvent.CItems);
          Child := Child.getNextSibling();
        end;
      end;
    end;
  end
  else if (ClientEvent.Sender is TCTable) then
  begin
    Table := TCTable(ClientEvent.Sender);

    Node := FNavigator.Items.getFirstNode().getFirstChild();
    while (Assigned(Node) and (Node.Data <> Table.Database)) do Node := Node.getNextSibling();

    if (Assigned(Node) and (not Node.HasChildren or (Node.Count > 0) or (Node = FNavigatorNodeToExpand))) then
    begin
      Node := Node.getFirstChild();
      while (Assigned(Node) and (Node.Data <> Table)) do Node := Node.getNextSibling();

      if (Assigned(Node) and (not Node.HasChildren or (Node.Count > 0) or (Node = FNavigatorNodeToExpand))) then
      begin
        Expanded := Node.Expanded;

        if (Table is TCBaseTable) then
          UpdateGroup(Node, giKeys, TCBaseTable(Table).Keys);
        UpdateGroup(Node, giFields, Table.Fields);
        if ((Table is TCBaseTable) and Assigned(TCBaseTable(Table).ForeignKeys)) then
          UpdateGroup(Node, giForeignKeys, TCBaseTable(Table).ForeignKeys);
        if ((Table is TCBaseTable) and Assigned(Table.Database.Triggers)) then
          UpdateGroup(Node, giTriggers, Table.Database.Triggers);

        Node.HasChildren := Node.Count > 0;
        Node.Expanded := Expanded;
      end;
    end;
  end;

  FNavigator.Items.EndUpdate();
  FNavigator.OnChanging := ChangingEvent;
  FNavigator.OnChange := ChangeEvent;

  if (FNavigator.Selected <> OldSelected) then
    SetTimer(Self.Handle, tiNavigator, 1, nil);

  if (Assigned(FNavigatorNodeToExpand) and (FNavigatorNodeToExpand.Count > 0)) then
  begin
    ExpandingEvent := FNavigator.OnExpanding;
    FNavigator.OnExpanding := nil;
    FNavigatorNodeToExpand.Expand(False);
    FNavigator.OnExpanding := ExpandingEvent;
  end;
end;

procedure TFClient.FNavigatorSetMenuItems(Sender: TObject; const Node: TTreeNode);
begin
  aPExpand.Enabled := Assigned(Node) and (not Assigned(Node) or not Node.Expanded) and (Assigned(Node) and Node.HasChildren);
  aPCollapse.Enabled := Assigned(Node) and (not Assigned(Node) or Node.Expanded) and (Node.ImageIndex <> iiServer);
  aPOpenInNewWindow.Enabled := Assigned(Node) and (Node.
  ImageIndex in [iiServer, iiDatabase, iiSystemDatabase, iiBaseTable, iiSystemView, iiView, iiProcedure, iiFunction]);
  aPOpenInNewTab.Enabled := aPOpenInNewWindow.Enabled;

  MainAction('aFImportSQL').Enabled := Assigned(Node) and (((Node.ImageIndex = iiServer) and (not Assigned(Client.UserRights) or Client.UserRights.RInsert)) or (Node.ImageIndex = iiDatabase));
  MainAction('aFImportText').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable]);
  MainAction('aFImportExcel').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable]);
  MainAction('aFImportAccess').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable]);
  MainAction('aFImportSQLite').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable]);
  MainAction('aFImportODBC').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable]);
  MainAction('aFImportXML').Enabled := Assigned(Node) and (Node.ImageIndex in [iiBaseTable]);
  MainAction('aFExportSQL').Enabled := Assigned(Node) and (Node.ImageIndex in [iiServer, iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]);
  MainAction('aFExportText').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable, iiView]);
  MainAction('aFExportExcel').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable, iiView]);
  MainAction('aFExportAccess').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable]);
  MainAction('aFExportSQLite').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable, iiView]);
  MainAction('aFExportODBC').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable]);
  MainAction('aFExportXML').Enabled := Assigned(Node) and (Node.ImageIndex in [iiServer, iiDatabase, iiBaseTable, iiView]);
  MainAction('aFExportHTML').Enabled := Assigned(Node) and (Node.ImageIndex in [iiServer, iiDatabase, iiBaseTable, iiView]);
  MainAction('aFPrint').Enabled := Assigned(Node) and ((View = vDiagram) or (Node.ImageIndex in [iiServer, iiDatabase, iiBaseTable, iiView]));
  MainAction('aECopy').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger, iiField, iiSystemViewField, iiViewField, iiHost, iiUser]);
  MainAction('aEPaste').Enabled := Assigned(Node) and ((Node.ImageIndex = iiServer) and Clipboard.HasFormat(CF_MYSQLSERVER) or (Node.ImageIndex = iiDatabase) and Clipboard.HasFormat(CF_MYSQLDATABASE) or (Node.ImageIndex = iiBaseTable) and Clipboard.HasFormat(CF_MYSQLTABLE) or (Node.ImageIndex = iiHosts) and Clipboard.HasFormat(CF_MYSQLHOSTS) or (Node.ImageIndex = iiUsers) and Clipboard.HasFormat(CF_MYSQLUSERS));
  MainAction('aERename').Enabled := Assigned(Node) and ((Node.ImageIndex = iiForeignKey) and (Client.ServerVersion >= 40013) or (Node.ImageIndex in [iiBaseTable, iiView, iiEvent, iiTrigger, iiField]));
  MainAction('aDCreateDatabase').Enabled := Assigned(Node) and (Node.ImageIndex in [iiServer]) and (not Assigned(Client.UserRights) or Client.UserRights.RCreate);
  MainAction('aDCreateTable').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase);
  MainAction('aDCreateView').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase) and (Client.ServerVersion >= 50001);
  MainAction('aDCreateProcedure').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase) and (Client.ServerVersion >= 50004);
  MainAction('aDCreateFunction').Enabled := MainAction('aDCreateProcedure').Enabled;
  MainAction('aDCreateEvent').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase) and (Client.ServerVersion >= 50106);
  MainAction('aDCreateTrigger').Enabled := Assigned(Node) and (Node.ImageIndex = iiBaseTable) and Assigned(TCDatabase(Node.Parent.Data).Triggers);
  MainAction('aDCreateKey').Enabled := Assigned(Node) and (Node.ImageIndex = iiBaseTable);
  MainAction('aDCreateField').Enabled := Assigned(Node) and (Node.ImageIndex = iiBaseTable);
  MainAction('aDCreateForeignKey').Enabled := Assigned(Node) and (Node.ImageIndex in [iiBaseTable]) and Assigned(TCBaseTable(Node.Data).Engine) and TCBaseTable(Node.Data).Engine.ForeignKeyAllowed;
  MainAction('aDCreateHost').Enabled := Assigned(Node) and (Node.ImageIndex = iiHosts);
  MainAction('aDCreateUser').Enabled := Assigned(Node) and (Node.ImageIndex = iiUsers);
  MainAction('aDDeleteDatabase').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase);
  MainAction('aDDeleteTable').Enabled := Assigned(Node) and (Node.ImageIndex = iiBaseTable);
  MainAction('aDDeleteView').Enabled := Assigned(Node) and (Node.ImageIndex = iiView);
  MainAction('aDDeleteRoutine').Enabled := Assigned(Node) and (Node.ImageIndex in [iiProcedure, iiFunction]);
  MainAction('aDDeleteEvent').Enabled := Assigned(Node) and (Node.ImageIndex = iiEvent);
  MainAction('aDDeleteKey').Enabled := Assigned(Node) and (Node.ImageIndex = iiKey);
  MainAction('aDDeleteField').Enabled := Assigned(Node) and (Node.ImageIndex = iiField) and (TCTableField(Node.Data).Fields.Count > 1);
  MainAction('aDDeleteForeignKey').Enabled := Assigned(Node) and (Node.ImageIndex = iiForeignKey) and (Client.ServerVersion >= 40013);
  MainAction('aDDeleteTrigger').Enabled := Assigned(Node) and (Node.ImageIndex = iiTrigger);
  MainAction('aDDeleteHost').Enabled := False;
  MainAction('aDDeleteUser').Enabled := False;
  MainAction('aDDeleteProcess').Enabled := False;
  MainAction('aDEditServer').Enabled := Assigned(Node) and (Node.ImageIndex = iiServer);
  MainAction('aDEditDatabase').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase);
  MainAction('aDEditTable').Enabled := Assigned(Node) and (Node.ImageIndex = iiBaseTable);
  MainAction('aDEditView').Enabled := Assigned(Node) and (Node.ImageIndex = iiView);
  MainAction('aDEditRoutine').Enabled := Assigned(Node) and (Node.ImageIndex in [iiProcedure, iiFunction]);
  MainAction('aDEditEvent').Enabled := Assigned(Node) and (Node.ImageIndex = iiEvent);
  MainAction('aDEditKey').Enabled := Assigned(Node) and (Node.ImageIndex = iiKey);
  MainAction('aDEditField').Enabled := Assigned(Node) and (Node.ImageIndex = iiField);
  MainAction('aDEditForeignKey').Enabled := Assigned(Node) and (Node.ImageIndex = iiForeignKey);
  MainAction('aDEditTrigger').Enabled := Assigned(Node) and (Node.ImageIndex = iiTrigger);
  MainAction('aDEmpty').Enabled := Assigned(Node) and ((Node.ImageIndex = iiDatabase) or (Node.ImageIndex = iiBaseTable) or ((Node.ImageIndex = iiField) and TCTableField(Node.Data).NullAllowed));

  miNExpand.Default := aPExpand.Enabled;
  miNCollapse.Default := aPCollapse.Enabled;
  aDDelete.Enabled := MainAction('aDDeleteDatabase').Enabled
    or MainAction('aDDeleteTable').Enabled
    or MainAction('aDDeleteView').Enabled
    or MainAction('aDDeleteRoutine').Enabled
    or MainAction('aDDeleteEvent').Enabled
    or MainAction('aDDeleteKey').Enabled
    or MainAction('aDDeleteField').Enabled
    or MainAction('aDDeleteForeignKey').Enabled
    or MainAction('aDDeleteTrigger').Enabled;
  if (not Assigned(Node)) then
    miNProperties.Action := nil
  else
    case (Node.ImageIndex) of
      iiServer: miNProperties.Action := MainAction('aDEditServer');
      iiDatabase: miNProperties.Action := MainAction('aDEditDatabase');
      iiBaseTable: miNProperties.Action := MainAction('aDEditTable');
      iiView: miNProperties.Action := MainAction('aDEditView');
      iiProcedure,
      iiFunction: miNProperties.Action := MainAction('aDEditRoutine');
      iiEvent: miNProperties.Action := MainAction('aDEditEvent');
      iiTrigger: miNProperties.Action := MainAction('aDEditTrigger');
      iiKey: miNProperties.Action := MainAction('aDEditKey');
      iiField: miNProperties.Action := MainAction('aDEditField');
      iiForeignKey: miNProperties.Action := MainAction('aDEditForeignKey');
      iiHost: miNProperties.Action := MainAction('aDEditHost');
      iiProcess: miNProperties.Action := MainAction('aDEditProcess');
      iiUser: miNProperties.Action := MainAction('aDEditUser');
      iiVariable: miNProperties.Action := MainAction('aDEditVariable');
      else miNProperties.Action := nil;
    end;
  miNProperties.Enabled := Assigned(miNProperties.Action);
  miNProperties.Caption := Preferences.LoadStr(97) + '...';
  miNProperties.ShortCut := ShortCut(VK_RETURN, [ssAlt]);

  ToolBarData.tbPropertiesAction := miNProperties.Action;
  Window.Perform(CM_UPDATETOOLBAR, 0, LPARAM(Self));

  ShowEnabledItems(MNavigator.Items);

  miNExpand.Default := miNExpand.Visible;
  miNCollapse.Default := miNCollapse.Visible;

  FNavigator.ReadOnly := not MainAction('aERename').Enabled;
end;

procedure TFClient.FOffsetChange(Sender: TObject);
begin
  if (Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet)) then
  begin
    FUDOffset.Position := FUDOffset.Position;

    FLimitEnabled.Enabled := FUDLimit.Position > 0;
    FLimitEnabled.Down := (FUDOffset.Position = TMySQLTable(ActiveDBGrid.DataSource.DataSet).Offset) and (FUDLimit.Position = TMySQLTable(ActiveDBGrid.DataSource.DataSet).Limit);
  end;
end;

procedure TFClient.FOffsetKeyPress(Sender: TObject; var Key: Char);
begin
  if ((Key = Chr(VK_ESCAPE)) and (TMySQLTable(ActiveDBGrid.DataSource.DataSet).Limit > 0)) then
  begin
    FUDOffset.Position := FUDOffset.Position;
    FUDLimit.Position := FUDLimit.Position;
    FLimitChange(Sender);

    if (Sender is TEdit) then
    begin
      TEdit(Sender).SelStart := 0;
      TEdit(Sender).SelLength := Length(TEdit(Sender).Text);
    end;
    Key := #0;
  end
  else if ((Key = Chr(VK_RETURN)) and not FLimitEnabled.Down) then
  begin
    FLimitEnabled.Down := True;
    FLimitEnabledClick(Sender);

    if (Sender is TEdit) then
    begin
      TEdit(Sender).SelStart := 0;
      TEdit(Sender).SelLength := Length(TEdit(Sender).Text);
    end;
    Key := #0;
  end;
end;

procedure TFClient.FormClientEvent(const Event: TCClient.TEvent);
begin
  if (not (csDestroying in ComponentState)) then
    case (Event.EventType) of
      ceItemsValid,
      ceItemValid,
      ceItemCreated,
      ceItemAltered,
      ceItemDropped:
        ClientUpdate(Event);
      ceMonitor:
        Perform(CM_POST_MONITOR, 0, 0);
      ceBeforeExecuteSQL:
        BeforeExecuteSQL(Event);
      ceAfterExecuteSQL:
        AfterExecuteSQL(Event);
    end;
end;

procedure TFClient.FormResize(Sender: TObject);
var
  MaxHeight: Integer;
begin
  if (PSideBar.Visible) then
  begin
    PSideBar.Constraints.MaxWidth := ClientWidth - PContent.Constraints.MinWidth - SSideBar.Width;

    MaxHeight := ClientHeight;
    if (PLog.Visible) then Dec(MaxHeight, PLog.Height);
    if (SLog.Visible) then Dec(MaxHeight, SLog.Height);
    PSideBar.Constraints.MaxHeight := MaxHeight;
    PanelResize(PSideBar);
  end;

  if (PLog.Visible) then
  begin
    PLog.Constraints.MaxHeight := ClientHeight - PHeader.Height - PContent.Constraints.MinHeight - SLog.Height;
    PLogResize(Sender);
  end;
end;

procedure TFClient.FormAccountEvent(const ClassType: TClass);
var
  I: Integer;
  NewListItem: TListItem;
begin
  if (ClassType = Client.Account.Desktop.Bookmarks.ClassType) then
  begin
    FBookmarks.Items.Clear();
    for I := 0 to Client.Account.Desktop.Bookmarks.Count - 1 do
    begin
      NewListItem := FBookmarks.Items.Add();
      NewListItem.Caption := Client.Account.Desktop.Bookmarks[I].Caption;
      NewListItem.ImageIndex := 73;
    end;

    Window.Perform(CM_BOOKMARKCHANGED, 0, 0);
  end;
end;

procedure TFClient.FQuickSearchChange(Sender: TObject);
begin
  FQuickSearchEnabled.Enabled := FQuickSearch.Text <> '';
  FQuickSearchEnabled.Down := (FQuickSearch.Text <> '') and (ActiveDBGrid.DataSource.DataSet is TCTableDataSet) and (FQuickSearch.Text = TCTableDataSet(ActiveDBGrid.DataSource.DataSet).QuickSearch);
end;

procedure TFClient.FQuickSearchEnabledClick(Sender: TObject);
begin
  Wanted.Clear();

  TableOpen(Sender);
  Window.ActiveControl := FQuickSearch;
end;

procedure TFClient.FQuickSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(VK_ESCAPE)) then
  begin
    FQuickSearch.Text := '';
    if ((ActiveDBGrid.DataSource.DataSet is TCTableDataSet) and (TCTableDataSet(ActiveDBGrid.DataSource.DataSet).QuickSearch <> '')) then
      FQuickSearchEnabled.Click();

    Key := #0;
  end
  else if ((Key = Chr(VK_RETURN)) and not FQuickSearchEnabled.Down) then
  begin
    FQuickSearchEnabled.Down := True;
    FQuickSearchEnabled.Click();

    FQuickSearch.SelStart := 0;
    FQuickSearch.SelLength := Length(FQuickSearch.Text);
    Key := #0;
  end;
end;

procedure TFClient.FreeDBGrid(const DBGrid: TMySQLDBGrid);
begin
  if (ActiveDBGrid = DBGrid) then
    ActiveDBGrid := nil;
  DBGrid.Free();
end;

procedure TFClient.FreeListView(const ListView: TListView);
begin
  ListView.OnChanging := nil;
  ListView.Items.BeginUpdate();
  ListView.Items.Clear();
  ListView.Items.EndUpdate();
  ListView.Free();
end;

procedure TFClient.FRTFChange(Sender: TObject);
begin
  FRTF.ReadOnly := True;
{
  FRTF.Text ist nicht der RTF SourceCode!
  FRTF.ReadOnly := not Assigned(ActiveDBGrid.DataSource.DataSet) or not ActiveDBGrid.DataSource.DataSet.CanModify;
  if (not FRTF.ReadOnly and (FRTF.Text <> EditorField.OldValue)) then
    ActiveDBGrid.DataSource.DataSet.Edit();
}
end;

procedure TFClient.FRTFEnter(Sender: TObject);
begin
  FRTFChange(nil);

  StatusBarRefresh();
end;

procedure TFClient.FRTFExit(Sender: TObject);
begin
{
  FRTF.Text ist nicht der RTF SourceCode!
  if (FRTF.Modified) then
    EditorField.AsString := FRTF.Text;
}

  SendMessage(FRTF.Handle, WM_VSCROLL, SB_TOP, 0);
end;

procedure TFClient.FRTFShow(Sender: TObject);
var
  TempFRTFOnChange: TNotifyEvent;
begin
  TempFRTFOnChange := FRTF.OnChange; FRTF.OnChange := nil;

  if (not Assigned(EditorField) or EditorField.IsNull) then
  begin
    FRTF.Text := '';
    FRTF.ReadOnly := False;
  end
  else
  begin
    FRTF.Text := EditorField.AsString;
    FRTF.ReadOnly := EditorField.ReadOnly or not EditorField.DataSet.CanModify;
  end;
  FRTF.SelectAll();

  FRTF.OnChange := TempFRTFOnChange;
end;

procedure TFClient.FSQLEditorCompletionClose(Sender: TObject);
begin
  SynMemoEnter(Sender);
end;

procedure TFClient.FSQLEditorCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var
  Attri: TSynHighlighterAttributes;
  Database: TCDatabase;
  DMLStmt: TSQLDMLStmt;
  I: Integer;
  Identifier: string;
  Index: Integer;
  J: Integer;
  Len: Integer;
  Owner: string;
  Parse: TSQLParse;
  QueryBuilder: TacQueryBuilder;
  S: string;
  SQL: string;
  StringList: TStringList;
  Table: TCTable;
  Tables: array of TCTable;
  Token: string;
begin
  SetLength(Tables, 0);

  Index := 1; Len := 0;
  try
    SQL := ActiveSynMemo.Text; // Sometimes this forces in exception SynGetText / GetSelAvail
    repeat
      Len := SQLStmtLength(SQL, Index);
      Inc(Index, Len);
    until (Index >= ActiveSynMemo.SelStart);
    Dec(Index, Len);
    SQL := Copy(SQL, Index, Len);
  except
    SQL := '';
  end;

  if ((Len <> 0) and not Client.InUse() and FSQLEditorSynMemo.GetHighlighterAttriAtRowCol(FSQLEditorSynMemo.WordStart(), Token, Attri)) then
  begin
    Index := ActiveSynMemo.SelStart - Index + 1;
    while ((Index > 0) and (Pos(SQL[Index], FSQLEditorCompletion.EndOfTokenChr) = 0)) do Dec(Index);

    Database := Client.DatabaseByName(SelectedDatabase);
    if (Assigned(Database)) then
    begin
      Client.BeginSynchron();
      if (not Database.Update()) then
        Database := nil;
      Client.EndSynchron();
    end;

    StringList := TStringList.Create();

    if (((Index > 0) and (SQL[Index] = '.')) or (Attri = MainHighlighter.DelimitedIdentifierAttri)) then
    begin
      I := Index - 1;
      Identifier := SQLUnescape(Copy(SQL, I + 1, Index - I - 1));
      if (SQL[Index] <> '.') then
        Owner := ''
      else
      begin
        Dec(I);
        while ((I > 0) and (Pos(SQL[I], FSQLEditorCompletion.EndOfTokenChr) = 0)) do Dec(I);
        Owner := SQLUnescape(Copy(SQL, I + 1, Index - I - 1 - Length(Owner)));
      end;

      if (Owner <> '') then
      begin
        Database := Client.DatabaseByName(Owner);

        if (Assigned(Database)) then
          for I := 0 to Database.Tables.Count - 1 do
            StringList.Add(Database.Tables[I].Name);
      end;

      Table := nil;
      if (Owner <> '') then
      begin
        if (Assigned(Database)) then
          Table := Database.BaseTableByName(Owner)
        else if (SelectedDatabase <> '') then
          Table := Client.DatabaseByName(SelectedDatabase).BaseTableByName(Owner);
        if (Assigned(Table)) then
        begin
          SetLength(Tables, 1);
          Tables[0] := Table;
        end
        else if (SQLCreateParse(Parse, PChar(SQL), Length(SQL),Client.ServerVersion) and SQLParseKeyword(Parse, 'SELECT') and (Owner <> '')) then
        begin
          QueryBuilder := TacQueryBuilder.Create(Window);
          QueryBuilder.Visible := False;
          QueryBuilder.Parent := ActiveSynMemo;
          QueryBuilder.SyntaxProvider := SyntaxProvider;
          QueryBuilder.MetadataProvider := MetadataProvider;
          Insert('*', SQL, Index + 1);
          try
            QueryBuilder.SQL := SQL;
            Application.ProcessMessages();
            for I := 0 to QueryBuilder.QueryStatistics.UsedDatabaseObjects.Count - 1 do
              for J := 0 to QueryBuilder.QueryStatistics.UsedDatabaseObjects[I].Aliases.Count - 1 do
              if ((Client.LowerCaseTableNames = 0) and (lstrcmp(PChar(QueryBuilder.QueryStatistics.UsedDatabaseObjects[I].Aliases[J].Token), PChar(Owner)) = 0)
                or (Client.LowerCaseTableNames > 0) and (lstrcmpi(PChar(QueryBuilder.QueryStatistics.UsedDatabaseObjects[I].Aliases[J].Token), PChar(Owner)) = 0)) then
              begin
                if (QueryBuilder.QueryStatistics.UsedDatabaseObjects[I].Database.QualifiedName = '') then
                  Database := Client.DatabaseByName(SelectedDatabase)
                else
                  Database := Client.DatabaseByName(QueryBuilder.QueryStatistics.UsedDatabaseObjects[I].Database.QualifiedName);
                if (not Assigned(Database)) then
                  Table := nil
                else
                  Table := Database.TableByName(QueryBuilder.QueryStatistics.UsedDatabaseObjects[I].Name.Token);
                if (Assigned(Table)) then
                begin
                  SetLength(Tables, 1);
                  Tables[0] := Table;
                end;
              end;
          except
          end;
          QueryBuilder.Free();
        end;
      end
      else if (SQLParseDMLStmt(DMLStmt, PChar(SQL), Length(SQL), Client.ServerVersion)) then
        for I := 0 to Length(DMLStmt.TableNames) - 1 do
        begin
          Database := Client.DatabaseByName(DMLStmt.DatabaseNames[I]);
          if (Assigned(Database)) then
          begin
            Table := Database.TableByName(DMLStmt.TableNames[I]);
            if (Assigned(Table)) then
            begin
              SetLength(Tables, Length(Tables) + 1);
              Tables[Length(Tables) - 1] := Table;
            end;
          end;
        end;
    end
    else
    begin
      if (SQLCreateParse(Parse, PChar(SQL), Length(SQL),Client.ServerVersion) and SQLParseKeyword(Parse, 'SELECT')) then
      begin
        QueryBuilder := TacQueryBuilder.Create(Window);
        QueryBuilder.Visible := False;
        QueryBuilder.Parent := ActiveSynMemo;
        QueryBuilder.SyntaxProvider := SyntaxProvider;
        QueryBuilder.MetadataProvider := MetadataProvider;
        try
          QueryBuilder.SQL := SQL;
          Application.ProcessMessages();
          for I := 0 to QueryBuilder.QueryStatistics.UsedDatabaseObjects.Count - 1 do
          begin
            if (QueryBuilder.QueryStatistics.UsedDatabaseObjects[I].Database.QualifiedName = '') then
              Database := Client.DatabaseByName(SelectedDatabase)
            else
              Database := Client.DatabaseByName(QueryBuilder.QueryStatistics.UsedDatabaseObjects[I].Database.QualifiedName);
            if (not Assigned(Database)) then
              Table := nil
            else
              Table := Database.TableByName(QueryBuilder.QueryStatistics.UsedDatabaseObjects[I].Name.Token);
            if (Assigned(Table)) then
            begin
              SetLength(Tables, Length(Tables) + 1);
              Tables[Length(Tables) - 1] := Table;
            end;
          end;
        except
        end;
        QueryBuilder.Free();
      end;

      StringList.Text
        := ReplaceStr(MainHighlighter.GetKeywords(Ord(tkKey)), ',', #13#10) + #13#10
        + ReplaceStr(MainHighlighter.GetKeywords(Ord(tkDatatype)), ',', #13#10) + #13#10
        + ReplaceStr(MainHighlighter.GetKeywords(Ord(tkFunction)), ',', #13#10) + #13#10
        + ReplaceStr(MainHighlighter.GetKeywords(Ord(tkPLSQL)), ',', #13#10);

      for I := 0 to Client.Databases.Count - 1 do
        StringList.Add(Client.Databases[I].Name);
      if (Assigned(Database)) then
        for I := 0 to Database.Tables.Count - 1 do
          StringList.Add(Database.Tables[I].Name);
    end;

    try
      for I := 0 to Length(Tables) - 1 do
        if (Tables[I].Fields.Count > 0) then
        begin
          if (Tables[I] is TCBaseTable) then
            for J := 0 to TCBaseTable(Tables[I]).Keys.Count - 1 do
              if (not TCBaseTable(Tables[I]).Keys[J].Primary) then
                StringList.Add(TCBaseTable(Tables[I]).Keys[J].Caption);
          for J := 0 to Tables[I].Fields.Count - 1 do
            StringList.Add(Tables[I].Fields[J].Name);
          if (Tables[I] is TCBaseTable) then
            for J := 0 to TCBaseTable(Tables[I]).ForeignKeys.Count - 1 do
              StringList.Add(TCBaseTable(Tables[I]).ForeignKeys[J].Name);
        end;
    except
    end;

    S := Copy(ActiveSynMemo.Lines.Strings[ActiveSynMemo.WordStart().Line - 1], ActiveSynMemo.WordStart().Char, ActiveSynMemo.CharIndexToRowCol(ActiveSynMemo.SelStart).Char - ActiveSynMemo.WordStart().Char);

    // avoid Popup, if a word has been typed completely
    I := StringList.Count - 1;
    while ((I >= 0) and (StringList.Count > 0)) do
    begin
      if (lstrcmpi(PChar(StringList[I]), PChar(S)) = 0) then
        StringList.Clear();
      Dec(I);
    end;

    StringList.Sort();
    FSQLEditorCompletion.ItemList.Text := StringList.Text;

    StringList.Free();
  end;

  CanExecute := FSQLEditorCompletion.ItemList.Count > 0;
end;

procedure TFClient.FSQLEditorCompletionPaintItem(Sender: TObject;
  Index: Integer; TargetCanvas: TCanvas; ItemRect: TRect;
  var CustomDraw: Boolean);
begin
  FSQLEditorCompletionShow(Sender);
end;

procedure TFClient.FSQLEditorCompletionShow(Sender: TObject);
begin
  MainAction('aDRun').Enabled := True;
  MainAction('aDRunSelection').Enabled := True;

  KillTimer(Handle, tiCodeCompletion);
  SetTimer(Handle, tiCodeCompletion, 1000, nil);
  FSQLEditorCompletionTimerCounter := 0;
end;

procedure TFClient.FSQLEditorCompletionTimerTimer(Sender: TObject);
begin
  Inc(FSQLEditorCompletionTimerCounter);

  if ((FSQLEditorCompletionTimerCounter = 5) or (FSQLEditorCompletion.Form.AssignedList.Count = 0)) then
  begin
    FSQLEditorCompletion.CancelCompletion();
    KillTimer(Handle, tiCodeCompletion);
  end;
end;

procedure TFClient.FSQLHistoryChange(Sender: TObject; Node: TTreeNode);
begin
  FSQLHistoryMenuNode := Node;
end;

procedure TFClient.FSQLHistoryChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  AllowChange := False;
end;

procedure TFClient.FSQLHistoryDblClick(Sender: TObject);
begin
  Wanted.Clear();

  FSQLHistoryMenuNode := FSQLHistory.Selected;


  if (Sender = FSQLHistory) then
  begin
    MSQLHistoryPopup(Sender);
      if (miHStatementIntoSQLEditor.Default) then
        miHStatementIntoSQLEditor.Click();
  end;
end;

procedure TFClient.FSQLHistoryEnter(Sender: TObject);
begin
  miHProperties.ShortCut := ShortCut(VK_RETURN, [ssAlt]);
end;

procedure TFClient.FSQLHistoryExit(Sender: TObject);
begin
  MainAction('aECopy').Enabled := False;

  miHProperties.ShortCut := 0;
end;

procedure TFClient.FSQLHistoryHint(Sender: TObject; const Node: TTreeNode;
  var Hint: string);
begin
  if (Node.ImageIndex in [iiQuery, iiStatement]) then
    Hint := XMLNode(IXMLNode(Node.Data), 'sql').Text
  else
    Hint := '';
end;

procedure TFClient.FSQLHistoryKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (not TTreeView(Sender).IsEditing()) then
    if ((Key = Ord('C')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssCtrl])) then
      begin aECopyExecute(Sender); Key := 0; end
    else if ((Key = Ord('V')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssShift])) then
      begin aEPasteExecute(Sender); Key := 0; end;
end;

procedure TFClient.FSQLHistoryKeyPress(Sender: TObject; var Key: Char);
var
  I: Integer;
  MenuItem: TMenuItem;
  TreeView: TTreeView_Ext;
begin
  TreeView := TTreeView_Ext(Sender);

  if (not TreeView.IsEditing()) then
    if ((Sender = ActiveListView) and (Ord(Key) = VK_BACK)) then
      FNavigator.Selected := FNavigator.Selected.Parent
    else if (Key = #3) then
      Key := #0
    else if (Ord(Key) = VK_RETURN) then
      if (Assigned(TreeView.Selected)) then
      begin
        MenuItem := nil;
        for I := 0 to TreeView.PopupMenu.Items.Count - 1 do
          if (TreeView.PopupMenu.Items[I].Default) then
            MenuItem := TreeView.PopupMenu.Items[I];
        if Assigned(MenuItem) then
          begin MenuItem.Click(); Key := #0; end;
      end;
end;

procedure TFClient.FSQLHistoryRefresh(Sender: TObject);
var
  Date: TDateTime;
  DateNode: TTreeNode;
  I: Integer;
  NewNode: TTreeNode;
  OldNode: TTreeNode;
  TimeNode: TTreeNode;
  XML: IXMLNode;
begin
  if (PSQLHistory.Visible) then
  begin
    DateNode := nil;

    FSQLHistory.Items.BeginUpdate();

    if (FSQLHistory.Items.Count > 0) then
      XML := IXMLNode(FSQLHistory.Items[FSQLHistory.Items.Count - 1].Data)
    else if (Client.Account.HistoryXML.ChildNodes.Count > 0) then
      XML := Client.Account.HistoryXML.ChildNodes[0]
    else
      XML := nil;

    while (Assigned(XML)) do
    begin
      if (XML.NodeName = 'sql') then
      begin
        Date := SysUtils.StrToFloat(XMLNode(XML, 'datetime').Text, FileFormatSettings);
        DateNode := nil;
        for I := 0 to FSQLHistory.Items.Count - 1 do
          if (FSQLHistory.Items[I].Text = SysUtils.DateToStr(Date, LocaleFormatSettings)) then
            DateNode := FSQLHistory.Items[I];
        if (not Assigned(DateNode)) then
        begin
          DateNode := FSQLHistory.Items.Add(nil, SysUtils.DateToStr(Date, LocaleFormatSettings));
          DateNode.HasChildren := True;
          DateNode.ImageIndex := iiCalendar;
        end;

        OldNode := FSQLHistory.Items[FSQLHistory.Items.Count - 1];
        case (OldNode.ImageIndex) of
          iiCalendar: OldNode := nil;
          iiClock: OldNode := OldNode.Parent;
        end;
        if (not Assigned(OldNode) or (OldNode.Parent <> DateNode) or (XMLNode(IXMLNode(OldNode.Data), 'sql').Text <> XMLNode(XML, 'sql').Text)) then
        begin
          NewNode := FSQLHistory.Items.AddChild(DateNode, SQLStmtToCaption(XMLNode(XML, 'sql').Text));
          if (XML.Attributes['type'] <> 'query') then
            NewNode.ImageIndex := iiStatement
          else
            NewNode.ImageIndex := iiQuery;
          NewNode.Data := Pointer(XML);
        end
        else
        begin
          if (not OldNode.HasChildren) then
          begin
            TimeNode := FSQLHistory.Items.AddChild(OldNode, TimeToStr(StrToFloat(XMLNode(IXMLNode(OldNode.Data), 'datetime').Text, FileFormatSettings), LocaleFormatSettings));
            TimeNode.ImageIndex := iiClock;
            TimeNode.Data := OldNode.Data;
          end;

          TimeNode := FSQLHistory.Items.AddChild(OldNode, TimeToStr(StrToFloat(XMLNode(XML, 'datetime').Text, FileFormatSettings), LocaleFormatSettings));
          TimeNode.ImageIndex := iiClock;
          TimeNode.Data := Pointer(XML);
        end;
      end;

      XML := XML.NextSibling();
    end;

    if (Assigned(DateNode)) then
    begin
      DateNode.Expand(False);

      PostMessage(FSQLHistory.Handle, WM_VSCROLL, SB_BOTTOM, 0);
    end;

    FSQLHistory.Items.EndUpdate();

    FSQLHistoryMenuNode := DateNode;
  end;
end;

procedure TFClient.FTextChange(Sender: TObject);
begin
  if (Assigned(EditorField) and FText.Modified) then
    case (NewLineFormat) of
      nlUnix: EditorField.AsString := ReplaceStr(FText.Text, #13#10, #10);
      nlMacintosh: EditorField.AsString := ReplaceStr(FText.Text, #13#10, #13);
      else EditorField.AsString := FText.Text;
    end;

  aVBlobRTF.Visible := Assigned(EditorField) and (EditorField.DataType = ftWideMemo) and not EditorField.IsNull and IsRTF(EditorField.AsString);
end;

procedure TFClient.FTextEnter(Sender: TObject);
begin
  if (Assigned(EditorField) and (EditorField.DataSet.State <> dsEdit)) then
    EditorField.DataSet.Edit();

  StatusBarRefresh();
end;

procedure TFClient.FTextExit(Sender: TObject);
begin
  SendMessage(FText.Handle, WM_VSCROLL, SB_TOP, 0);
end;

procedure TFClient.FTextKeyPress(Sender: TObject; var Key: Char);
begin
  if ((Window.ActiveControl = FText) and (Key = #27)) then
  begin
    Window.ActiveControl := ActiveDBGrid;
    ActiveDBGrid.DataSource.DataSet.Cancel();
  end
end;

procedure TFClient.FTextShow(Sender: TObject);
var
  S: string;
begin
  FText.OnChange := nil;

  if (not Assigned(EditorField) or EditorField.IsNull) then
    FText.Text := ''
  else
  begin
    S := EditorField.AsString;
    if ((Pos(#10, S) > 0) and (Pos(#13, S) > 0)) then
    begin
      if (Pos(#10, S) < Pos(#13, S)) then
        NewLineFormat := nlUnix
      else if ((Length(S) > Pos(#13, S)) and (S[Pos(#13, S) + 1] = #10)) then
        NewLineFormat := nlWindows
      else if ((Length(S) > Pos(#10, S)) and (S[Pos(#10, S) + 1] = #13)) then
        NewLineFormat := nlMacintosh;
    end
    else if (Pos(#13, S) > 0) then
      NewLineFormat := nlMacintosh
    else
      NewLineFormat := nlUnix;

    case (NewLineFormat) of
      nlUnix: FText.Text := ReplaceStr(ReplaceStr(EditorField.AsString, #10, #13#10), #13#10#10, #13#10);
      nlMacintosh: FText.Text := ReplaceStr(ReplaceStr(EditorField.AsString, #13, #13#10), #13#13#10, #13#10);
      else FText.Text := EditorField.AsString;
    end;
    FText.Modified := False;
  end;
  if (FText.Text <> '') then
    FText.SelectAll();

  FText.ReadOnly := not Assigned(EditorField) or EditorField.ReadOnly or not EditorField.DataSet.CanModify;

  FText.OnChange := FTextChange;


  aVBlobRTF.Visible := (EditorField.DataType = ftWideMemo) and not EditorField.IsNull and IsRTF(EditorField.AsString);
end;

function TFClient.GetActiveDBGrid(): TMySQLDBGrid;
var
  I: Integer;
begin
  if (not Assigned(FNavigator.Selected)) then
    Result := nil
  else
    case (View) of
      vBrowser:
        Result := Desktop(TCTable(FNavigator.Selected.Data)).CreateDBGrid();
      vIDE:
        case (FNavigator.Selected.ImageIndex) of
          iiProcedure,
          iiFunction:
            Result := Desktop(TCRoutine(FNavigator.Selected.Data)).ActiveDBGrid;
          else
            Result := nil;
        end;
      vBuilder:
        Result := Desktop(TCDatabase(FNavigator.Selected.Data)).DBGrid;
      vEditor:
        Result := SQLEditor.ActiveDBGrid;
      else
        Result := nil;
    end;

  if (Assigned(Result)) then
  begin
    aDDeleteRecord.DataSource := Result.DataSource;
    aDInsertRecord.DataSource := Result.DataSource;
    Result.DataSource.OnDataChange := DBGridDataSourceDataChange;
  end;

  if (Assigned(Result)) then
    for I := 0 to PResult.ControlCount - 1 do
      PResult.Controls[I].Visible := PResult.Controls[I] = Result.Parent;
end;

function TFClient.GetActiveIDEInputDataSet(): TDataSet;
var
  I: Integer;
  J: Integer;
  Routine: TCRoutine;
begin
  case (SelectedImageIndex) of
    iiProcedure,
    iiFunction:
      begin
        Routine := TCRoutine(FNavigator.Selected.Data);

        FObjectIDEGrid.DataSource.DataSet := Routine.InputDataSet;

        for I := 0 to Routine.ParameterCount - 1 do
          if (Routine.Parameter[I].FieldType = mfEnum) then
            for J := 0 to Length(Routine.Parameter[I].Items) - 1 do
              FObjectIDEGrid.Columns[I].PickList.Add(Routine.Parameter[I].Items[J]);
      end;
    iiTrigger:
      FObjectIDEGrid.DataSource.DataSet := TCTrigger(FNavigator.Selected.Data).InputDataSet;
    else
      FObjectIDEGrid.DataSource.DataSet := nil;
  end;

  if (Assigned(FObjectIDEGrid.DataSource.DataSet) and not FObjectIDEGrid.DataSource.Enabled) then
    FObjectIDEGrid.DataSource.Enabled := True;

  if (Assigned(FObjectIDEGrid.DataSource.DataSet)) then
    for I := 0 to FObjectIDEGrid.DataSource.DataSet.FieldCount - 1 do
      if ((FObjectIDEGrid.Columns[I].Width > Preferences.GridMaxColumnWidth) and not (FObjectIDEGrid.Columns[I].Field.DataType in [ftSmallint, ftInteger, ftLargeint, ftWord, ftFloat, ftDate, ftDateTime, ftTime, ftCurrency])) then
        FObjectIDEGrid.Columns[I].Width := Preferences.GridMaxColumnWidth;

  if (Assigned(FObjectIDEGrid.DataSource.DataSet) and not FObjectIDEGrid.DataSource.Enabled) then
    DBGridColEnter(FObjectIDEGrid);

  PObjectIDETrigger.Visible := (SelectedImageIndex = iiTrigger);
  if (PObjectIDETrigger.Visible) then
    DBGridColExit(FObjectIDEGrid);

  PObjectIDEResize(nil);

  Result := FObjectIDEGrid.DataSource.DataSet;
end;

function TFClient.GetActiveListView(): TListView;
var
  I: Integer;
begin
  if (not Assigned(FNavigator.Selected)) then
    Result := FServerListView
  else
    case (FNavigator.Selected.ImageIndex) of
      iiServer: Result := FServerListView;
      iiDatabase,
      iiSystemDatabase:
        Result := Desktop(TCDatabase(FNavigator.Selected.Data)).CreateListView();
      iiBaseTable,
      iiSystemView,
      iiView:
        Result := Desktop(TCTable(FNavigator.Selected.Data)).CreateListView();
      iiHosts:
        begin
          if (not Assigned(HostsListView)) then
          begin
            HostsListView := CreateListView(Client.Hosts);
            Client.Hosts.PushBuildEvent(Client.Hosts);
          end;
          Result := HostsListView;
        end;
      iiProcesses:
        begin
          if (not Assigned(ProcessesListView)) then
          begin
            ProcessesListView := CreateListView(Client.Processes);
            Client.Processes.PushBuildEvent(Client.Processes);
          end;
          Result := ProcessesListView;
        end;
      iiStati:
        begin
          if (not Assigned(StatiListView)) then
          begin
            StatiListView := CreateListView(Client.Stati);
            Client.Stati.PushBuildEvent(Client.Stati);
          end;
          Result := StatiListView;
        end;
      iiUsers:
        begin
          if (not Assigned(UsersListView)) then
          begin
            UsersListView := CreateListView(Client.Users);
            Client.Users.PushBuildEvent(Client.Users);
          end;
          Result := UsersListView;
        end;
      iiVariables:
        begin
          if (not Assigned(VariablesListView)) then
          begin
            VariablesListView := CreateListView(Client.Variables);
            Client.Variables.PushBuildEvent(Client.Variables);
          end;
          Result := VariablesListView;
        end;
      else
        Result := nil;
    end;

  if (Assigned(Result)) then
    for I := 0 to PListView.ControlCount - 1 do
      PListView.Controls[I].Visible := PListView.Controls[I] = Result;
end;

function TFClient.GetActiveSynMemo(): TSynMemo;
var
  I: Integer;
begin
  case (View) of
    vIDE:
      case (SelectedImageIndex) of
        iiView: Result := Desktop(TCView(FNavigator.Selected.Data)).CreateSynMemo();
        iiProcedure,
        iiFunction: Result := Desktop(TCRoutine(FNavigator.Selected.Data)).CreateSynMemo();
        iiEvent: Result := Desktop(TCEvent(FNavigator.Selected.Data)).CreateSynMemo();
        iiTrigger: Result := Desktop(TCTrigger(FNavigator.Selected.Data)).CreateSynMemo();
        else Result := nil;
      end;
    vBuilder:
      Result := FBuilderSynMemo;
    vEditor:
      Result := FSQLEditorSynMemo;
    else
      Result := nil;
  end;

  if (Assigned(Result) and (Result <> FBuilderSynMemo)) then
    for I := 0 to PSynMemo.ControlCount - 1 do
      PSynMemo.Controls[I].Visible := PSynMemo.Controls[I] = Result;
end;

function TFClient.GetActiveWorkbench(): TWWorkbench;
var
  I: Integer;
begin
  case (View) of
    vDiagram:
      Result := Desktop(TCDatabase(FNavigator.Selected.Data)).Workbench;
    else
      Result := nil;
  end;

  if (Assigned(Result)) then
    for I := 0 to PWorkbench.ControlCount - 1 do
      PWorkbench.Controls[I].Visible := PWorkbench.Controls[I] = Result;
end;

function TFClient.GetFocusedCItem(): TCItem;
begin
  if ((Window.ActiveControl = ActiveListView) and Assigned(ActiveListView.Selected)) then
    if ((ActiveListView.SelCount > 1) or not (TCObject(ActiveListView.Selected.Data) is TCItem)) then
      Result := nil
    else
      Result := TCItem(ActiveListView.Selected.Data)
  else if ((Window.ActiveControl = ActiveWorkbench) and Assigned(ActiveWorkbench) and Assigned(ActiveWorkbench.Selected)) then
    if (ActiveWorkbench.Selected is TWTable) then
      Result := TWTable(ActiveWorkbench.Selected).BaseTable
    else if (ActiveWorkbench.Selected is TWForeignKey) then
      Result := TWForeignKey(ActiveWorkbench.Selected).BaseForeignKey
    else
      Result := TCDatabase(FNavigator.Selected.Data)
  else if ((Window.ActiveControl = ActiveListView) and not Assigned(ActiveListView.Selected) or (Window.ActiveControl = ActiveWorkbench) and (not Assigned(ActiveWorkbench) or not Assigned(ActiveWorkbench.Selected))) then
    Result := TCItem(FNavigator.Selected.Data)
  else if ((Window.ActiveControl = FNavigator) and Assigned(FNavigator.Selected)) then
    Result := TCItem(FNavigator.Selected.Data)
  else if (Assigned(FNavigatorMenuNode)) then
    Result := TCItem(FNavigatorMenuNode.Data)
  else
    Result := nil;
end;

function TFClient.GetFocusedDatabaseNames(): string;
var
  I: Integer;
begin
  if ((Window.ActiveControl = ActiveListView) and (SelectedImageIndex = iiServer)) then
  begin
    Result := '';
    for I := 0 to ActiveListView.Items.Count - 1 do
      if ((ActiveListView.Items[I].Selected) and (ActiveListView.Items[I].ImageIndex in [iiDatabase, iiSystemDatabase])) then
      begin
        if (Result <> '') then
          Result := Result + ',';
        Result := Result + ActiveListView.Items[I].Caption;
      end;
  end
  else
    Result := SelectedDatabase;
end;

function TFClient.GetFocusedTableName(): string;
var
  I: Integer;
  URI: TUURI;
begin
  if ((Window.ActiveControl = ActiveListView) and (SelectedImageIndex in [iiDatabase, iiSystemDatabase])) then
  begin
    Result := '';
    for I := 0 to ActiveListView.Items.Count - 1 do
      if ((ActiveListView.Items[I].Selected) and (ActiveListView.Items[I].ImageIndex in [iiTable, iiBaseTable, iiSystemView, iiView])) then
      begin
        if (Result <> '') then Result := Result + ',';
        Result := Result + ActiveListView.Items[I].Caption;
      end;
  end
  else if ((Window.ActiveControl is TWWorkbench) and Assigned(ActiveWorkbench.Selected) and (ActiveWorkbench.Selected is TWTable)) then
    Result := TWTable(ActiveWorkbench.Selected).Caption
  else
  begin
    URI := TUURI.Create(Address);
    Result := URI.Table;
    URI.Free();
  end;
end;

function TFClient.GetPath(): TFileName;
begin
  Result := ExcludeTrailingPathDelimiter(Preferences.Path);
end;

function TFClient.GetMenuDatabase(): TCDatabase;
var
  Node: TTreeNode;
begin
  if (Window.ActiveControl = FNavigator) then
    Node := FNavigatorMenuNode
  else
    Node := FNavigator.Selected;

  while (Assigned(Node) and Assigned(Node.Parent) and Assigned(Node.Parent.Parent)) do
    Node := Node.Parent;

  if (Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiSystemDatabase])) then
    Result := TCDatabase(Node.Data)
  else
    Result := nil;
end;

function TFClient.GetSelectedDatabase(): string;
var
  URI: TUURI;
begin
  URI := TUURI.Create(Address);
  Result := URI.Database;
  URI.Free();
end;

function TFClient.GetSelectedImageIndex(): Integer;
begin
  if (not Assigned(FNavigator.Selected)) then
    Result := -1
  else
    Result := FNavigator.Selected.ImageIndex;
end;

function TFClient.GetView(): TView;
var
  URI: TUURI;
begin
  URI := TUURI.Create(Address);
  if (URI.Param['view'] = 'browser') then
    Result := vBrowser
  else if (URI.Param['view'] = 'ide') then
    Result := vIDE
  else if (URI.Param['view'] = 'builder') then
    Result := vBuilder
  else if (URI.Param['view'] = 'editor') then
    Result := vEditor
  else if (URI.Param['view'] = 'diagram') then
    Result := vDiagram
  else
    Result := vObjects;
  URI.Free();
end;

function TFClient.GetWindow(): TForm_Ext;
begin
  if (not Assigned(Owner)) then
    raise Exception.Create('Owner not set');

  Result := TForm_Ext(Owner);
end;

procedure TFClient.gmFilterClearClick(Sender: TObject);
begin
  Wanted.Clear();

  if (ActiveDBGrid.DataSource.DataSet.Filtered) then
  begin
    ActiveDBGrid.DataSource.DataSet.Filtered := False;
    ActiveDBGrid.DataSource.DataSet.Filter := '';
    StatusBarRefresh();
  end
  else if (ActiveDBGrid.DataSource.DataSet is TMySQLTable) then
    FFilterEnabledClick(nil);
end;

procedure TFClient.gmFilterIntoFilterClick(Sender: TObject);
var
  FilterIndex: Integer;
  MenuItem: TMenuItem;
  Success: Boolean;
  Value: string;
begin
  Wanted.Clear();

  MenuItem := TMenuItem(Sender); FilterIndex := MenuItem.Tag;

  Success := True;
  case (Filters[FilterIndex].ValueType) of
    0: Value := '';
    1: if (ActiveDBGrid.SelectedField.DataType in UnquotedDataTypes) then
         Value := ActiveDBGrid.SelectedField.DisplayText
       else
         Value := SQLEscape(ActiveDBGrid.SelectedField.DisplayText);
    2: Value := SQLEscape('%' + ActiveDBGrid.SelectedField.DisplayText + '%');
    else
      begin
        if (Filters[FilterIndex].ValueType = 3) then
          DQuickFilter.Data := ActiveDBGrid.SelectedField.DisplayText
        else
          DQuickFilter.Data := '%' + ActiveDBGrid.SelectedField.DisplayText + '%';
        Success := DQuickFilter.Execute();
        if (Success) then
          if (ActiveDBGrid.SelectedField.DataType in UnquotedDataTypes) then
            Value := DQuickFilter.Data
          else
            Value := SQLEscape(DQuickFilter.Data);
      end;
  end;

  if (Success) then
    if (View = vBrowser) then
    begin
      FFilter.Text := Format(Filters[FilterIndex].Text, [Client.EscapeIdentifier(ActiveDBGrid.SelectedField.FieldName), Value]);
      FFilterEnabled.Down := True;
      FFilterEnabledClick(Sender);
    end
    else
    begin
      ActiveDBGrid.DataSource.DataSet.FilterOptions := [foCaseInsensitive];
      ActiveDBGrid.DataSource.DataSet.Filter := Format(Filters[FilterIndex].Text, ['[' + ActiveDBGrid.SelectedField.FieldName + ']', Value]);
      ActiveDBGrid.DataSource.DataSet.Filtered := True;
      StatusBarRefresh();
    end;
end;

function TFClient.ImageIndexByData(const Data: TObject): Integer;
begin
  if (not Assigned(Data)) then
    Result := iiServer
  else if (TObject(Data) is TCSystemDatabase) then
    Result := iiSystemDatabase
  else if (TObject(Data) is TCDatabase) then
    Result := iiDatabase
  else if (TObject(Data) is TCSystemView) then
    Result := iiSystemView
  else if (TObject(Data) is TCBaseTable) then
    Result := iiBaseTable
  else if (TObject(Data) is TCView) then
    Result := iiView
  else if (TObject(Data) is TCProcedure) then
    Result := iiProcedure
  else if (TObject(Data) is TCFunction) then
    Result := iiFunction
  else if (TObject(Data) is TCEvent) then
    Result := iiEvent
  else if (TObject(Data) is TCKey) then
    Result := iiKey
  else if (TObject(Data) is TCTableField) then
    if (TCTableField(Data).Table is TCSystemView) then
      Result := iiSystemViewField
    else if (TCTableField(Data).Table is TCBaseTable) then
      Result := iiField
    else
      Result := iiViewField
  else if (TObject(Data) is TCForeignKey) then
    Result := iiForeignKey
  else if (TObject(Data) is TCTrigger) then
    Result := iiTrigger
  else if (TObject(Data) is TCHosts) then
    Result := iiHosts
  else if (TObject(Data) is TCProcesses) then
    Result := iiProcesses
  else if (TObject(Data) is TCStati) then
    Result := iiStati
  else if (TObject(Data) is TCUsers) then
    Result := iiUsers
  else if (TObject(Data) is TCVariables) then
    Result := iiVariables
  else
    raise ERangeError.Create(SRangeError);
end;

procedure TFClient.ImportError(const Sender: TObject; const Error: TTools.TError; const Item: TTools.TItem; var Success: TDataAction);
begin
  MsgBox(Error.ErrorMessage, Preferences.LoadStr(45), MB_OK + MB_ICONERROR);

  Success := daAbort;
end;

procedure TFClient.ListViewAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if ((Stage = cdPrePaint) and not (csDestroying in ComponentState) and Assigned(Item)
    and ((Item.ImageIndex = iiKey) and TCKey(Item.Data).Primary or (Item.ImageIndex = iiField) and TCTableField(Item.Data).InPrimaryKey)) then
    Sender.Canvas.Font.Style := [fsBold]
  else
    Sender.Canvas.Font.Style := [];
end;

procedure TFClient.ListViewAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  Sender.Canvas.Font.Style := [fsBold]; // Without this the first sub item is Bold (Delphi XE2)
  Sender.Canvas.Font.Style := [];
end;

procedure TFClient.ListViewColumnClick(Sender: TObject; Column: TListColumn);
var
  HDItem: THDItem;
  I: Integer;
  Kind: TADesktop.TListViewKind;
begin
  Kind := ColumnWidthKindFromImageIndex(SelectedImageIndex);

  with (ListViewSortData[Kind]) do
  begin
    ListViewSortData[Kind].Index := Column.Index;
    if (Sender = ActiveListView) then
    begin
      if ((OldFListOrderIndex <> Index) or ((Order = 0) or (Order < 0) and (not (SelectedImageIndex in [iiBaseTable, iiSystemView, iiView]) or (Index > 0)))) then
        ListViewSortData[Kind].Order := 1
      else if (Order = 1) then
        ListViewSortData[Kind].Order := -1
      else
        ListViewSortData[Kind].Order := 0;

      ActiveListView.CustomSort(nil, LPARAM(@ListViewSortData[Kind]));
    end;

    HDItem.Mask := HDI_FORMAT;
    for I := 0 to ActiveListView.Columns.Count - 1 do
      if (BOOL(SendMessage(ListView_GetHeader(ActiveListView.Handle), HDM_GETITEM, I, LParam(@HDItem)))) then
      begin
        if ((Order = 0) or (I <> Index)) then
          HDItem.fmt := HDItem.fmt and not HDF_SORTUP and not HDF_SORTDOWN
        else if (Order > 0) then
          HDItem.fmt := HDItem.fmt and not HDF_SORTDOWN or HDF_SORTUP
        else
          HDItem.fmt := HDItem.fmt and not HDF_SORTUP or HDF_SORTDOWN;
        SendMessage(ListView_GetHeader(ActiveListView.Handle), HDM_SETITEM, I, LParam(@HDItem));
      end;

    if ((ComCtl32MajorVersion >= 6) and not CheckWin32Version(6, 1)) then
      if (Order = 0) then
        SendMessage(ActiveListView.Handle, LVM_SETSELECTEDCOLUMN, -1, 0)
      else
        SendMessage(ActiveListView.Handle, LVM_SETSELECTEDCOLUMN, Index, 0);

    OldFListOrderIndex := Index;
  end;
end;

procedure TFClient.ListViewCompare(Sender: TObject; Item1: TListItem;
  Item2: TListItem; Data: Integer; var Compare: Integer);
const
  ImageIndexSort = Chr(iiHosts) + Chr(iiProcesses) + Chr(iiStati) + Chr(iiUsers) + Chr(iiVariables);
var
  String1: string;
  String2: string;
  SortRec: ^TListViewSortRec;
begin
  SortRec := @TListViewSortRec(Pointer(Data)^);

  Compare := 0;
  if (Item1.GroupID <> Item2.GroupID) then
    Compare := Sign(Item1.GroupID - Item2.GroupID)
  else if (Item1.GroupID = giSystemTools) then
    Compare := Sign(Pos(Chr(Item1.ImageIndex), ImageIndexSort) - Pos(Chr(Item2.ImageIndex), ImageIndexSort))
  else if (SortRec^.Order = 0) then
    Compare := Sign(TCItem(Item1.Data).Index - TCItem(Item2.Data).Index)
  else
  begin
    if ((SortRec^.Index = 0) or (SortRec^.Index > Item1.SubItems.Count) or (SortRec^.Index > Item2.SubItems.Count)) then
    begin
      String1 := Item1.Caption;
      String2 := Item2.Caption;
    end
    else
    begin
      String1 := Item1.SubItems[SortRec^.Index - 1];
      String2 := Item2.SubItems[SortRec^.Index - 1];

      Compare := 0;
      case (SelectedImageIndex) of
        iiServer:
          case (SortRec^.Index) of
            0: if (Client.LowerCaseTableNames = 0) then
                 Compare := Sign(lstrcmp(PChar(String1), PChar(String2)))
               else
                 Compare := Sign(lstrcmpi(PChar(String1), PChar(String2)));
            1: Compare := Sign(SysUtils.StrToInt64(String1) - SysUtils.StrToInt64(String2));
            2:
              begin
                if (String1 = '') then String1 := '0';               if (String2 = '') then String2 := '0';

                String1 := ReplaceStr(String1, '???', '0');         String2 := ReplaceStr(String2, '???', '0');

                String1 := ReplaceStr(String1, ' B', '');           String2 := ReplaceStr(String2, ' B', '');
                String1 := ReplaceStr(String1, ' KB', '000');       String2 := ReplaceStr(String2, ' KB', '000');
                String1 := ReplaceStr(String1, ' MB', '000000');    String2 := ReplaceStr(String2, ' MB', '000000');
                String1 := ReplaceStr(String1, ' GB', '000000000'); String2 := ReplaceStr(String2, ' GB', '000000000');

                String1 := ReplaceStr(String1, LocaleFormatSettings.ThousandSeparator, '');
                String2 := ReplaceStr(String2, LocaleFormatSettings.ThousandSeparator, '');
                Compare := Sign(SysUtils.StrToFloat(String1, LocaleFormatSettings) - SysUtils.StrToFloat(String2, LocaleFormatSettings));
              end;
            3:
              begin
                if (String1 = '') then String1 := SysUtils.DateToStr(1, LocaleFormatSettings);
                if (String2 = '') then String2 := SysUtils.DateToStr(1, LocaleFormatSettings);

                String1 := ReplaceStr(String1, '???', SysUtils.DateToStr(1, LocaleFormatSettings));
                String2 := ReplaceStr(String2, '???', SysUtils.DateToStr(1, LocaleFormatSettings));

                Compare := Sign(SysUtils.StrToDateTime(String1, LocaleFormatSettings) - SysUtils.StrToDateTime(String2, LocaleFormatSettings));
              end;
          end;
        iiDatabase:
          case (SortRec^.Index) of
            0: if (Client.LowerCaseTableNames = 0) then
                 Compare := Sign(lstrcmp(PChar(String1), PChar(String2)))
               else
                 Compare := Sign(lstrcmpi(PChar(String1), PChar(String2)));
            2:
              begin
                if (String1 = '') then String1 := '0';
                if (String2 = '') then String2 := '0';

                String1 := ReplaceStr(String1, '~', '');            String2 := ReplaceStr(String2, '~', '');
                String1 := ReplaceStr(String1, '???', '0');         String2 := ReplaceStr(String2, '???', '0');

                String1 := ReplaceStr(String1, LocaleFormatSettings.ThousandSeparator, '');
                String2 := ReplaceStr(String2, LocaleFormatSettings.ThousandSeparator, '');
                Compare := Sign(SysUtils.StrToFloat(String1, LocaleFormatSettings) - SysUtils.StrToFloat(String2, LocaleFormatSettings));
              end;
            3:
              begin
                if (String1 = '') then String1 := '0';               if (String2 = '') then String2 := '0';

                String1 := ReplaceStr(String1, '???', '0');         String2 := ReplaceStr(String2, '???', '0');
                String1 := ReplaceStr(String1, '???', '0');         String2 := ReplaceStr(String2, '???', '0');
                String1 := ReplaceStr(String1, ' B', '');           String2 := ReplaceStr(String2, ' B', '');
                String1 := ReplaceStr(String1, ' KB', '000');       String2 := ReplaceStr(String2, ' KB', '000');
                String1 := ReplaceStr(String1, ' MB', '000000');    String2 := ReplaceStr(String2, ' MB', '000000');
                String1 := ReplaceStr(String1, ' GB', '000000000'); String2 := ReplaceStr(String2, ' GB', '000000000');

                String1 := ReplaceStr(String1, LocaleFormatSettings.ThousandSeparator, '');
                String2 := ReplaceStr(String2, LocaleFormatSettings.ThousandSeparator, '');
                Compare := Sign(SysUtils.StrToFloat(String1, LocaleFormatSettings) - SysUtils.StrToFloat(String2, LocaleFormatSettings));
              end;
            4:
              begin
                if (String1 = '') then String1 := SysUtils.DateToStr(1, LocaleFormatSettings);
                if (String2 = '') then String2 := SysUtils.DateToStr(1, LocaleFormatSettings);

                String1 := ReplaceStr(String1, '???', SysUtils.DateToStr(1, LocaleFormatSettings));
                String2 := ReplaceStr(String2, '???', SysUtils.DateToStr(1, LocaleFormatSettings));

                Compare := Sign(SysUtils.StrToDateTime(String1, LocaleFormatSettings) - SysUtils.StrToDateTime(String2, LocaleFormatSettings));
              end;
          end;
      end;
    end;

    if (Compare = 0) then
      Compare := Sign(lstrcmpi(PChar(String1), PChar(String2)));
    if (Compare = 0) then
      Compare := Sign(lstrcmpi(PChar(Item1.Caption), PChar(Item2.Caption)));

    Compare := ListViewSortData[SortRec^.Kind].Order * Compare;
  end;
end;

procedure TFClient.ListViewDblClick(Sender: TObject);
var
  I: Integer;
  MenuItem: TMenuItem;
  PopupMenu: TPopupMenu;
begin
  Wanted.Clear();

  MenuItem := nil;

  if ((Sender is TListView) and Assigned(TListView(Sender).OnSelectItem)) then
    TListView(Sender).OnSelectItem(Sender, TListView(Sender).Selected, Assigned(TListView(Sender).Selected));

  if (Sender is TListView) then
    PopupMenu := TListView(Sender).PopupMenu
  else if (Sender is TWWorkbench) then
    PopupMenu := TWWorkbench(Sender).PopupMenu
  else
    PopupMenu := nil;

  if (Assigned(PopupMenu)) then
    for I := 0 to PopupMenu.Items.Count - 1 do
      if (PopupMenu.Items[I].Default and PopupMenu.Items[I].Visible and PopupMenu.Items[I].Enabled) then
        MenuItem := PopupMenu.Items[I];

  if (Assigned(MenuItem)) then MenuItem.Click();
end;

procedure TFClient.ListViewDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  SourceClient: TCClient;
  SourceDatabase: TCDatabase;
  SourceItem: TListItem;
  SourceNode: TTreeNode;
  TargetItem: TListItem;
begin
  Accept := False;

  TargetItem := TListView(Sender).GetItemAt(X, Y);

  if ((Source is TTreeView_Ext) and (TTreeView_Ext(Source).Name = FNavigator.Name)) then
  begin
    SourceNode := TFClient(TTreeView_Ext(Source).Owner).MouseDownNode;

    if (not Assigned(TargetItem)) then
      case (SourceNode.ImageIndex) of
        iiBaseTable,
        iiProcedure,
        iiFunction,
        iiField: Accept := (SourceNode.Parent.ImageIndex = SelectedImageIndex) and (SourceNode.Parent <> FNavigator.Selected);
      end
    else if (((TargetItem.Caption <> SourceNode.Text) or (SourceNode.Parent <> FNavigator.Selected)) and (SourceNode.Parent.Text <> TargetItem.Caption)) then
      case (TargetItem.ImageIndex) of
        iiDatabase: Accept := (SourceNode.ImageIndex in [iiDatabase, iiBaseTable, iiProcedure, iiFunction]);
        iiBaseTable: Accept := SourceNode.ImageIndex in [iiBaseTable, iiField];
      end;
  end
  else if ((Source is TListView) and (TListView(Source).SelCount = 1) and (TListView(Source).Parent.Name = PListView.Name)) then
  begin
    SourceItem := TListView(Source).Selected;
    SourceClient := TFClient(TTreeView_Ext(Source).Owner).Client;
    SourceDatabase := TFClient(TTreeView_Ext(Source).Owner).MenuDatabase;

    if (not Assigned(TargetItem)) then
      case (SourceItem.ImageIndex) of
        iiBaseTable,
        iiProcedure,
        iiFunction: Accept := (SelectedImageIndex = iiDatabase) and (not Assigned(TargetItem) and (Client.DatabaseByName(SelectedDatabase) <> SourceDatabase));
      end
    else if ((TargetItem <> SourceItem) and (TargetItem.ImageIndex = SourceItem.ImageIndex)) then
      case (SourceItem.ImageIndex) of
        iiBaseTable: Accept := (SelectedImageIndex = iiDatabase);
      end
    else if ((TargetItem <> SourceItem) and (SourceClient <> Client)) then
      case (SourceItem.ImageIndex) of
        iiBaseTable: Accept := (SelectedImageIndex = iiServer);
      end;
  end;
end;

procedure TFClient.ListViewEdited(Sender: TObject; Item: TListItem;
  var S: string);
begin
  if (not RenameCItem(FocusedCItem, S)) then
    S := Item.Caption;
end;

procedure TFClient.ListViewEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit := (Item.ImageIndex = iiDatabase) and (Client.ServerVersion >= 50107) or (Item.ImageIndex = iiForeignKey) and (Client.ServerVersion >= 40013) or (Item.ImageIndex in [iiBaseTable, iiView, iiEvent, iiField, iiTrigger, iiHost, iiUser]);
end;

procedure TFClient.ListViewInitialize(const ListView: TListView);

  procedure SetColumnWidths(const ListView: TListView; const Kind: TADesktop.TListViewKind);
  var
    I: Integer;
  begin
    for I := 0 to ListView.Columns.Count - 1 do
    begin
      ListView.Column[I].Width := Client.Account.Desktop.ColumnWidths[Kind, I];
      ListView.Columns[I].MinWidth := 10;
    end;
  end;

var
  Count: Integer;
  I: Integer;
  Update: Boolean;
begin
  Update := ListView.Columns.Count > 0;

  ListView.Groups.BeginUpdate();

  if (not Update) then
  begin
    ListView.Columns.BeginUpdate();
    if (ListView = FServerListView) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkServer);
      ListView.Columns[1].Alignment := taRightJustify;
      ListView.Columns[2].Alignment := taRightJustify;

      ListView.Groups.Add().GroupID := giDatabases;
      ListView.Groups.Add().GroupID := giSystemTools;
    end
    else if (TObject(ListView.Tag) is TCDatabase) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkDatabase);
      if (ListView.Column[1].Width > 2 * Preferences.GridMaxColumnWidth) then
        ListView.Column[1].Width := 2 * Preferences.GridMaxColumnWidth;
      ListView.Columns[2].Alignment := taRightJustify;
      ListView.Columns[3].Alignment := taRightJustify;

      ListView.Groups.Add().GroupID := giTables;
      ListView.Groups.Add().GroupID := giRoutines;
      ListView.Groups.Add().GroupID := giEvents;
    end
    else if (TObject(ListView.Tag) is TCBaseTable) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      if (Client.ServerVersion >= 40100) then
        ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkTable);
      if (ListView.Column[1].Width > 2 * Preferences.GridMaxColumnWidth) then
        ListView.Column[1].Width := 2 * Preferences.GridMaxColumnWidth;

      ListView.Groups.Add().GroupID := giKeys;
      ListView.Groups.Add().GroupID := giFields;
      ListView.Groups.Add().GroupID := giForeignKeys;
      ListView.Groups.Add().GroupID := giTriggers;
    end
    else if (TObject(ListView.Tag) is TCView) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkTable);
      if (ListView.Column[1].Width > 2 * Preferences.GridMaxColumnWidth) then
        ListView.Column[1].Width := 2 * Preferences.GridMaxColumnWidth;

      ListView.Groups.Add().GroupID := giFields;
    end
    else if (TObject(ListView.Tag) is TCHosts) then
    begin
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkHosts);

      ListView.Groups.Add().GroupID := giHosts;
    end
    else if (TObject(ListView.Tag) is TCProcesses) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkProcesses);

      ListView.Groups.Add().GroupID := giProcesses;
    end
    else if (TObject(ListView.Tag) is TCStati) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkStati);

      ListView.Groups.Add().GroupID := giStati;
    end
    else if (TObject(ListView.Tag) is TCUsers) then
    begin
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkUsers);

      ListView.Groups.Add().GroupID := giUsers;
    end
    else if (TObject(ListView.Tag) is TCVariables) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkVariables);

      ListView.Groups.Add().GroupID := giVariables;
    end;
  end;

  if (ListView = FServerListView) then
  begin
    ListView.Columns[0].Caption := ReplaceStr(Preferences.LoadStr(38), '&', '');
    ListView.Columns[1].Caption := Preferences.LoadStr(76);
    ListView.Columns[2].Caption := Preferences.LoadStr(67);
    ListView.Columns[3].Caption := Preferences.LoadStr(77);
    ListView.Columns[4].Caption := ReplaceStr(Preferences.LoadStr(73), '&', '');

    Count := ListView.Items.Count;
    for I := 0 to ListView.Items.Count - 1 do
      case (ListView.Items[I].ImageIndex) of
        iiHosts: ListView.Items[I].Caption := Preferences.LoadStr(335);
        iiProcesses: ListView.Items[I].Caption := Preferences.LoadStr(24);
        iiStati: ListView.Items[I].Caption := Preferences.LoadStr(23);
        iiUsers: ListView.Items[I].Caption := ReplaceStr(Preferences.LoadStr(561), '&', '');
        iiVariables: ListView.Items[I].Caption := Preferences.LoadStr(22);
        else Dec(Count);
      end;

    ListView.Groups[1].Header := ReplaceStr(Preferences.LoadStr(12), '&', '') + ' (' + IntToStr(Count) + ')';
  end
  else if (TObject(ListView.Tag) is TCDatabase) then
  begin
    ListView.Columns[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
    ListView.Columns[1].Caption := Preferences.LoadStr(69);
    ListView.Columns[2].Caption := Preferences.LoadStr(66);
    ListView.Columns[3].Caption := Preferences.LoadStr(67);
    ListView.Columns[4].Caption := Preferences.LoadStr(68);
    ListView.Columns[5].Caption := ReplaceStr(Preferences.LoadStr(73), '&', '');
    ListView.Columns[6].Caption := ReplaceStr(Preferences.LoadStr(111), '&', '');
  end
  else if (TObject(ListView.Tag) is TCBaseTable) then
  begin
    ListView.Columns[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
    ListView.Columns[1].Caption := Preferences.LoadStr(69);
    ListView.Columns[2].Caption := Preferences.LoadStr(71);
    ListView.Columns[3].Caption := Preferences.LoadStr(72);
    ListView.Columns[4].Caption := ReplaceStr(Preferences.LoadStr(73), '&', '');
    if (Client.ServerVersion >= 40100) then
      ListView.Columns[5].Caption := ReplaceStr(Preferences.LoadStr(111), '&', '');
  end
  else if (TObject(ListView.Tag) is TCView) then
  begin
    ListView.Columns[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
    ListView.Columns[1].Caption := Preferences.LoadStr(69);
    ListView.Columns[2].Caption := Preferences.LoadStr(71);
    ListView.Columns[3].Caption := Preferences.LoadStr(72);
    ListView.Columns[4].Caption := ReplaceStr(Preferences.LoadStr(73), '&', '');
  end
  else if (TObject(ListView.Tag) is TCHosts) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(335);
  end
  else if (TObject(ListView.Tag) is TCProcesses) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(269);
    ListView.Columns[1].Caption := ReplaceStr(Preferences.LoadStr(561), '&', '');
    ListView.Columns[2].Caption := Preferences.LoadStr(271);
    ListView.Columns[3].Caption := ReplaceStr(Preferences.LoadStr(38), '&', '');
    ListView.Columns[4].Caption := Preferences.LoadStr(273);
    ListView.Columns[5].Caption := Preferences.LoadStr(274);
    ListView.Columns[6].Caption := ReplaceStr(Preferences.LoadStr(661), '&', '');
    ListView.Columns[7].Caption := Preferences.LoadStr(276);
  end
  else if (TObject(ListView.Tag) is TCStati) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(267);
    ListView.Columns[1].Caption := Preferences.LoadStr(268);
  end
  else if (TObject(ListView.Tag) is TCUsers) then
  begin
    ListView.Columns[0].Caption := ReplaceStr(Preferences.LoadStr(561), '&', '');
  end
  else if (TObject(ListView.Tag) is TCVariables) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(267);
    ListView.Columns[1].Caption := Preferences.LoadStr(268);
  end;

  ListView.Groups.EndUpdate();
end;

procedure TFClient.ListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  if (not TListView(Sender).IsEditing()) then
    if ((Sender = ActiveListView) and (Key = VK_BACK)) then
      FNavigator.Selected := FNavigator.Selected.Parent
    else if ((Key = Ord('A')) and (Shift = [ssCtrl])) then
      MainAction('aESelectAll').Execute()
    else if ((Key = Ord('C')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssCtrl])) then
      begin aECopyExecute(Sender); Key := 0; end
    else if ((Key = Ord('V')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssShift])) then
      begin aEPasteExecute(Sender); Key := 0; end
    else if (Key = VK_RETURN) then
      if (Assigned(TListView(Sender).Selected) and Assigned(TListView(Sender).PopupMenu)) then
      begin
        MenuItem := nil;
        for I := 0 to TListView(Sender).PopupMenu.Items.Count - 1 do
          if (TListView(Sender).PopupMenu.Items[I].Default) then
            MenuItem := TListView(Sender).PopupMenu.Items[I];
        if (Assigned(MenuItem)) then
          begin MenuItem.Click(); Key := 0; end;
      end;
end;

procedure TFClient.ListViewEmpty(Sender: TObject);
var
  I: Integer;
  List: TList;
begin
  Wanted.Clear();

  if (ActiveListView.SelCount <= 1) then
    FNavigatorEmptyExecute(Sender)
  else if (Sender is TAction) then
  begin
    List := TList.Create();
    case (SelectedImageIndex) of
      iiServer:
        begin
          for I := 0 to ActiveListView.Items.Count - 1 do
            if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex in [iiDatabase, iiSystemDatabase])) then
              List.Add(ActiveListView.Items[I].Data);
          if (not Client.Update(List)) then
            Wanted.Action := TAction(Sender)
          else if (MsgBox(Preferences.LoadStr(405), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
            Client.EmptyDatabases(List);
        end;
      iiDatabase:
        begin
          for I := 0 to ActiveListView.Items.Count - 1 do
            if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex in [iiBaseTable])) then
              List.Add(ActiveListView.Items[I].Data);
          if (not Client.Update(List)) then
            Wanted.Action := TAction(Sender)
          else if (MsgBox(Preferences.LoadStr(406), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
            Client.DatabaseByName(SelectedDatabase).EmptyTables(List);
        end;
      iiBaseTable:
        if (MsgBox(Preferences.LoadStr(407), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
        begin
          for I := 0 to ActiveListView.Items.Count - 1 do
            if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex = iiField)) then
              List.Add(ActiveListView.Items[I].Data);
          TCBaseTable(FNavigator.Selected.Data).EmptyFields(List);
        end;
    end;
    List.Free();
  end;
end;

procedure TFClient.ListViewEnter(Sender: TObject);
begin
  MainAction('aDEmpty').OnExecute := ListViewEmpty;

  aDCreate.ShortCut := VK_INSERT;
  aDDelete.ShortCut := VK_DELETE;

  ListViewSelectItem(ActiveListView, ActiveListView.Selected, Assigned(ActiveListView.Selected));
  if (not Assigned(ActiveListView.Selected) and (ActiveListView.Items.Count > 0)) then
    ActiveListView.Items[0].Focused := True;

  StatusBarRefresh();
end;

procedure TFClient.ListViewExit(Sender: TObject);
var
  I: Integer;
  ImageIndex: Integer;
begin
  if (Sender is TListView) then
  begin
    ImageIndex := ImageIndexByData(TObject(TListView(Sender).Tag));
    if (ImageIndex > 0) then
      for I := 0 to ActiveListView.Columns.Count - 1 do
        Client.Account.Desktop.ColumnWidths[ColumnWidthKindFromImageIndex(ImageIndex), I] := ActiveListView.Columns[I].Width;
  end;

  MainAction('aFImportSQL').Enabled := False;
  MainAction('aFImportText').Enabled := False;
  MainAction('aFImportExcel').Enabled := False;
  MainAction('aFImportAccess').Enabled := False;
  MainAction('aFImportSQLite').Enabled := False;
  MainAction('aFImportODBC').Enabled := False;
  MainAction('aFImportXML').Enabled := False;
  MainAction('aFExportSQL').Enabled := False;
  MainAction('aFExportText').Enabled := False;
  MainAction('aFExportExcel').Enabled := False;
  MainAction('aFExportAccess').Enabled := False;
  MainAction('aFExportSQLite').Enabled := False;
  MainAction('aFExportODBC').Enabled := False;
  MainAction('aFExportXML').Enabled := False;
  MainAction('aFExportHTML').Enabled := False;
  MainAction('aFPrint').Enabled := False;
  MainAction('aERename').Enabled := False;
  MainAction('aDCreateDatabase').Enabled := False;
  MainAction('aDCreateTable').Enabled := False;
  MainAction('aDCreateView').Enabled := False;
  MainAction('aDCreateProcedure').Enabled := False;
  MainAction('aDCreateFunction').Enabled := False;
  MainAction('aDCreateEvent').Enabled := False;
  MainAction('aDCreateTrigger').Enabled := False;
  MainAction('aDCreateKey').Enabled := False;
  MainAction('aDCreateField').Enabled := False;
  MainAction('aDCreateForeignKey').Enabled := False;
  MainAction('aDCreateHost').Enabled := False;
  MainAction('aDCreateUser').Enabled := False;
  MainAction('aDDeleteDatabase').Enabled := False;
  MainAction('aDDeleteTable').Enabled := False;
  MainAction('aDDeleteView').Enabled := False;
  MainAction('aDDeleteRoutine').Enabled := False;
  MainAction('aDDeleteEvent').Enabled := False;
  MainAction('aDDeleteKey').Enabled := False;
  MainAction('aDDeleteField').Enabled := False;
  MainAction('aDDeleteForeignKey').Enabled := False;
  MainAction('aDDeleteTrigger').Enabled := False;
  MainAction('aDDeleteHost').Enabled := False;
  MainAction('aDDeleteUser').Enabled := False;
  MainAction('aDEditServer').Enabled := False;
  MainAction('aDEditDatabase').Enabled := False;
  MainAction('aDEditTable').Enabled := False;
  MainAction('aDEditView').Enabled := False;
  MainAction('aDEditRoutine').Enabled := False;
  MainAction('aDEditEvent').Enabled := False;
  MainAction('aDEditKey').Enabled := False;
  MainAction('aDEditField').Enabled := False;
  MainAction('aDEditForeignKey').Enabled := False;
  MainAction('aDEditTrigger').Enabled := False;
  MainAction('aDEditHost').Enabled := False;
  MainAction('aDEditProcess').Enabled := False;
  MainAction('aDEditUser').Enabled := False;
  MainAction('aDEditVariable').Enabled := False;
  MainAction('aDEmpty').Enabled := False;

  aDCreate.ShortCut := 0;
  aDDelete.ShortCut := 0;
  mlEProperties.ShortCut := 0;
end;

procedure TFClient.ListViewUpdate(const ClientEvent: TCClient.TEvent; const ListView: TListView; const Data: TCustomData = nil);

  function Compare(const Kind: TADesktop.TListViewKind; const Item1, Item2: TListItem): Integer;
  begin
    ListViewCompare(nil, Item1, Item2, LPARAM(@ListViewSortData[Kind]), Result);
  end;

  function GroupByGroupID(const GroupID: Integer): TListGroup;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to ListView.Groups.Count - 1 do
      if (ListView.Groups[I].GroupID = GroupID) then
        Result := ListView.Groups[I];
  end;

  procedure UpdateItem(const Item: TListItem; const Data: TObject);
  var
    I: Integer;
    S: string;
    S2: string;
  begin
    Assert(Item.Data = Data);


    Item.SubItems.BeginUpdate();
    Item.SubItems.Clear();

    if (Data is TCDatabase) then
    begin
      Item.GroupID := giDatabases;
      if (Data is TCSystemDatabase) then
        Item.ImageIndex := iiSystemDatabase
      else
        Item.ImageIndex := iiDatabase;
      Item.Caption := TCDatabase(Data).Caption;
      Item.SubItems.Clear();
      if (TCDatabase(Data).Count < 0) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(FormatFloat('#,##0', TCDatabase(Data).Count, LocaleFormatSettings));
      if ((TCDatabase(Data) is TCSystemDatabase) or (TCDatabase(Data).Size < 0)) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SizeToStr(TCDatabase(Data).Size));
      if (TCDatabase(Data) is TCSystemDatabase) then
        Item.SubItems.Add(SysUtils.DateToStr(Client.StartTime, LocaleFormatSettings))
      else if (TCDatabase(Data).Created = 0) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SysUtils.DateToStr(TCDatabase(Data).Created, LocaleFormatSettings));
      if (TCDatabase(Data).DefaultCharset = Client.DefaultCharset) then
        S := ''
      else
        S := TCDatabase(Data).DefaultCharset;
      if ((TCDatabase(Data).Collation <> '') and (TCDatabase(Data).Collation <> Client.Collation)) then
      begin
        if (S <> '') then S := S + ', ';
        S := S + TCDatabase(Data).Collation;
      end;
      if (TCDatabase(Data) is TCSystemDatabase) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(S);
    end
    else if (Data is TCTable) then
    begin
      Item.GroupID := giTables;
      if (Data is TCSystemView) then
        Item.ImageIndex := iiSystemView
      else if (Data is TCBaseTable) then
        Item.ImageIndex := iiBaseTable
      else
        Item.ImageIndex := iiView;
      Item.Caption := TCTable(Data).Caption;
      if ((TCTable(Data) is TCBaseTable) and Assigned(TCBaseTable(Data).Engine)) then
        Item.SubItems.Add(TCBaseTable(Data).Engine.Name)
      else if ((TCTable(Data) is TCView)) then
        Item.SubItems.Add(ReplaceStr(Preferences.LoadStr(738), '&', ''))
      else
        Item.SubItems.Add('');
      if ((TCTable(Data) is TCBaseTable) and not TCBaseTable(TCTable(Data)).ValidStatus) then
        Item.SubItems.Add('')
      else if ((TCTable(Data) is TCBaseTable) and (TCBaseTable(TCTable(Data)).Rows < 0)) then
        Item.SubItems.Add('')
      else if ((TCTable(Data) is TCBaseTable) and Assigned(TCBaseTable(TCTable(Data)).Engine) and (UpperCase(TCBaseTable(TCTable(Data)).Engine.Name) = 'INNODB')) then
        Item.SubItems.Add('~' + FormatFloat('#,##0', TCBaseTable(TCTable(Data)).Rows, LocaleFormatSettings))
      else if ((TCTable(Data) is TCBaseTable)) then
        Item.SubItems.Add(FormatFloat('#,##0', TCBaseTable(TCTable(Data)).Rows, LocaleFormatSettings))
      else
        Item.SubItems.Add('');
      if ((TCTable(Data) is TCBaseTable) and not TCBaseTable(TCTable(Data)).ValidStatus) then
        Item.SubItems.Add('')
      else if (TCTable(Data) is TCBaseTable) then
        if ((TCBaseTable(TCTable(Data)).DataSize + TCBaseTable(TCTable(Data)).IndexSize < 0)) then
          Item.SubItems.Add('')
        else
          Item.SubItems.Add(SizeToStr(TCBaseTable(TCTable(Data)).DataSize + TCBaseTable(TCTable(Data)).IndexSize + 1))
      else
        Item.SubItems.Add(SizeToStr(Length(TCView(TCTable(Data)).Source)));
      if (not (TCTable(Data) is TCBaseTable) or not TCBaseTable(TCTable(Data)).ValidStatus or (TCBaseTable(TCTable(Data)).Updated <= 0)) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SysUtils.DateTimeToStr(TCBaseTable(TCTable(Data)).Updated, LocaleFormatSettings));
      S := '';
      if ((TCTable(Data) is TCBaseTable) and (TCBaseTable(TCTable(Data)).DefaultCharset <> '') and (TCBaseTable(TCTable(Data)).DefaultCharset <> TCBaseTable(TCTable(Data)).Database.DefaultCharset)) then
        S := S + TCBaseTable(TCTable(Data)).DefaultCharset;
      if ((TCTable(Data) is TCBaseTable) and (TCBaseTable(TCTable(Data)).Collation <> '') and (TCBaseTable(TCTable(Data)).Collation <> TCBaseTable(TCTable(Data)).Database.Collation)) then
      begin
        if (S <> '') then S := S + ', ';
        S := S + TCBaseTable(TCTable(Data)).Collation;
      end;
      Item.SubItems.Add(S);
      if (Data is TCBaseTable) then
        Item.SubItems.Add(TCBaseTable(Data).Comment);
    end
    else if (Data is TCRoutine) then
    begin
      Item.GroupID := giRoutines;
      if (Data is TCProcedure) then
        Item.ImageIndex := iiProcedure
      else
        Item.ImageIndex := iiFunction;
      Item.Caption := TCRoutine(Data).Caption;
      case (TCRoutine(Data).RoutineType) of
        rtProcedure: Item.SubItems.Add('Procedure');
        rtFunction: Item.SubItems.Add('Function');
      end;
      Item.SubItems.Add('');
      if (not TCRoutine(Data).Valid) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SizeToStr(Length(TCRoutine(Data).Source)));
      if (TCRoutine(Data).Modified = 0) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SysUtils.DateTimeToStr(TCRoutine(Data).Modified, LocaleFormatSettings));
      if (not Assigned(TCRoutine(Data).FunctionResult) or (TCRoutine(Data).FunctionResult.Charset = '') or (TCRoutine(Data).FunctionResult.Charset = TCRoutine(Data).Database.DefaultCharset)) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(TCRoutine(Data).FunctionResult.Charset);
      Item.SubItems.Add(TCRoutine(Data).Comment);
    end
    else if (Data is TCEvent) then
    begin
      Item.GroupID := giEvents;
      Item.ImageIndex := iiEvent;
      Item.Caption := TCEvent(Data).Caption;
      Item.SubItems.Add('TCEvent(Data)');
      Item.SubItems.Add('');
      if (not TCEvent(Data).Valid) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SizeToStr(Length(TCEvent(Data).Source)));
      if (TCEvent(Data).Updated = 0) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SysUtils.DateTimeToStr(TCEvent(Data).Updated, LocaleFormatSettings));
      Item.SubItems.Add('');
      Item.SubItems.Add(TCEvent(Data).Comment);
    end
    else if (Data is TCKey) then
    begin
      Item.GroupID := giKeys;
      Item.ImageIndex := iiKey;
      Item.Caption := TCKey(Data).Caption;
      S := '';
      for I := 0 to TCKey(Data).Columns.Count - 1 do
      begin
        if (S <> '') then S := S + ',';
        S := S + TCKey(Data).Columns[I].Field.Name;
        if (TCKey(Data).Columns.Column[I].Length > 0) then
          S := S + '(' + IntToStr(TCKey(Data).Columns[I].Length) + ')';
      end;
      Item.SubItems.Add(S);
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      if (TCKey(Data).Unique) then
        Item.SubItems.Add('unique')
      else if (TCKey(Data).Fulltext) then
        Item.SubItems.Add('fulltext')
      else
        Item.SubItems.Add('');
      if (Client.ServerVersion >= 40100) then
        Item.SubItems.Add('');
    end
    else if (Data is TCBaseTableField) then
    begin
      Item.GroupID := giFields;
      if (TCBaseTableField(Data).Table is TCSystemView) then
        Item.ImageIndex := iiSystemViewField
      else
        Item.ImageIndex := iiField;
      Item.Caption := TCBaseTableField(Data).Caption;
      Item.SubItems.Add(TCBaseTableField(Data).DBTypeStr());
      if (TCBaseTableField(Data).NullAllowed) then
        Item.SubItems.Add(Preferences.LoadStr(74))
      else
        Item.SubItems.Add(Preferences.LoadStr(75));
      if (TCBaseTableField(Data).AutoIncrement) then
        Item.SubItems.Add('<auto_increment>')
      else if (TCBaseTableField(Data).Default = 'NULL') then
        Item.SubItems.Add('<' + Preferences.LoadStr(71) + '>')
      else if (TCBaseTableField(Data).Default = 'CURRENT_TIMESTAMP') then
        Item.SubItems.Add('<INSERT-TimeStamp>')
      else
        Item.SubItems.Add(TCBaseTableField(Data).UnescapeValue(TCBaseTableField(Data).Default));
      S := '';
      if (TCBaseTableField(Data).FieldType in TextFieldTypes) then
      begin
        if ((TCBaseTableField(Data).Charset <> '') and (TCBaseTableField(Data).Charset <> TCBaseTableField(Data).Table.DefaultCharset)) then
          S := S + TCBaseTableField(Data).Charset;
        if ((TCBaseTableField(Data).Collation <> '') and (TCBaseTableField(Data).Collation <> TCBaseTableField(Data).Table.Collation)) then
        begin
          if (S <> '') then S := S + ', ';
          S := S + TCBaseTableField(Data).Collation;
        end;
      end;
      Item.SubItems.Add(S);
      if (Client.ServerVersion >= 40100) then
        Item.SubItems.Add(TCBaseTableField(Data).Comment);
    end
    else if (Data is TCForeignKey) then
    begin
      Item.GroupID := giForeignKeys;
      Item.ImageIndex := iiForeignKey;
      Item.Caption := TCForeignKey(Data).Caption;
      Item.SubItems.Add(TCForeignKey(Data).DBTypeStr());
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      S := '';
      if (TCForeignKey(Data).OnDelete = dtCascade) then S := 'cascade on delete';
      if (TCForeignKey(Data).OnDelete = dtSetNull) then S := 'set NULL on delete';
      if (TCForeignKey(Data).OnDelete = dtSetDefault) then S := 'set default on delete';
      if (TCForeignKey(Data).OnDelete = dtNoAction) then S := 'no action on delete';
      S2 := '';
      if (TCForeignKey(Data).OnUpdate = utCascade) then S2 := 'cascade on update';
      if (TCForeignKey(Data).OnUpdate = utSetNull) then S2 := 'set NULL on update';
      if (TCForeignKey(Data).OnUpdate = utSetDefault) then S2 := 'set default on update';
      if (TCForeignKey(Data).OnUpdate = utNoAction) then S2 := 'no action on update';
      if (S <> '') and (S2 <> '') then S := S + ', ';
      S := S + S2;
      Item.SubItems.Add(S);
    end
    else if (Data is TCTrigger) then
    begin
      Item.GroupID := giTriggers;
      Item.ImageIndex := iiTrigger;
      Item.Caption := TCTrigger(Data).Caption;
      S := '';
      case (TCTrigger(Data).Timing) of
        ttBefore: S := S + 'before ';
        ttAfter: S := S + 'after ';
      end;
      case (TCTrigger(Data).Event) of
        teInsert: S := S + 'insert';
        teUpdate: S := S + 'update';
        teDelete: S := S + 'delete';
      end;
      Item.SubItems.Add(S);
    end
    else if (Data is TCViewField) then
    begin
      Item.GroupID := giFields;
      Item.ImageIndex := iiViewField;
      Item.Caption := TCViewField(Data).Caption;
      if (TCViewField(Data).FieldType <> mfUnknown) then
      begin
        Item.SubItems.Add(TCViewField(Data).DBTypeStr());
        if (TCViewField(Data).NullAllowed) then
          Item.SubItems.Add(Preferences.LoadStr(74))
        else
          Item.SubItems.Add(Preferences.LoadStr(75));
        if (TCViewField(Data).AutoIncrement) then
          Item.SubItems.Add('<auto_increment>')
        else
          Item.SubItems.Add(TCViewField(Data).Default);
        if (TCViewField(Data).Charset <> TCViewField(Data).Table.Database.DefaultCharset) then
          Item.SubItems.Add(TCViewField(Data).Charset);
      end;
    end
    else if (Data is TCHosts) then
    begin
      Item.GroupID := giSystemTools;
      Item.ImageIndex := iiHosts;
      Item.SubItems.Add(FormatFloat('#,##0', TCHosts(Data).Count, LocaleFormatSettings));
    end
    else if (Data is TCHost) then
    begin
      Item.GroupID := giHosts;
      Item.ImageIndex := iiHost;
      Item.Caption := TCHost(Data).Caption;
    end
    else if (Data is TCProcesses) then
    begin
      Item.GroupID := giSystemTools;
      Item.ImageIndex := iiProcesses;
      if (TCProcesses(Data).Count > 0) then
        Item.SubItems.Add(FormatFloat('#,##0', TCProcesses(Data).Count, LocaleFormatSettings));
    end
    else if (Data is TCProcess) then
    begin
      Item.GroupID := giProcesses;
      Item.ImageIndex := iiProcess;
      Item.Caption := IntToStr(TCProcess(Data).Id);
      Item.SubItems.Add(TCProcess(Data).UserName);
      Item.SubItems.Add(TCProcess(Data).Host);
      Item.SubItems.Add(TCProcess(Data).DatabaseName);
      Item.SubItems.Add(TCProcess(Data).Command);
      Item.SubItems.Add(SQLStmtToCaption(TCProcess(Data).SQL, 30));
      if (TCProcess(Data).Time = 0) then
        Item.SubItems.Add('???')
      else
        Item.SubItems.Add(ExecutionTimeToStr(TCProcess(Data).Time));
      Item.SubItems.Add(TCProcess(Data).State);
    end
    else if (Data is TCStati) then
    begin
      Item.GroupID := giSystemTools;
      Item.ImageIndex := iiStati;
      Item.SubItems.Add(FormatFloat('#,##0', TCStati(Data).Count, LocaleFormatSettings));
    end
    else if (Data is TCStatus) then
    begin
      Item.GroupID := giStati;
      Item.ImageIndex := iiStatus;
      Item.Caption := TCStatus(Data).Caption;
      Item.SubItems.Add(TCStatus(Data).Value);
    end
    else if (Data is TCUsers) then
    begin
      Item.GroupID := giSystemTools;
      Item.ImageIndex := iiUsers;
      if (Client.Users.Count >= 0) then
        Item.SubItems.Add(FormatFloat('#,##0', Client.Users.Count, LocaleFormatSettings))
      else
        Item.SubItems.Add('???');
    end
    else if (Data is TCUser) then
    begin
      Item.GroupID := giUsers;
      Item.ImageIndex := iiUser;
      Item.Caption := TCUser(Data).Caption;
    end
    else if (Data is TCVariables) then
    begin
      Item.GroupID := giSystemTools;
      Item.ImageIndex := iiVariables;
      Item.SubItems.Add(FormatFloat('#,##0', Client.Variables.Count, LocaleFormatSettings));
    end
    else if (Data is TCVariable) then
    begin
      Item.GroupID := giVariables;
      Item.ImageIndex := iiVariable;
      Item.Caption := TCVariable(Data).Caption;
      Item.SubItems.Add(TCVariable(Data).Value);
    end;

    Item.SubItems.EndUpdate();
  end;

  function InsertItem(const Kind: TADesktop.TListViewKind; const Data: TObject): TListItem;
  var
    GroupID: Integer;
    I: Integer;
    Item: TListItem;
    Index: Integer;
    Left: Integer;
    Mid: Integer;
    ReorderGroup: Boolean;
    Right: Integer;
  begin
    Index := 0;
    while ((Index < ListView.Items.Count) and (ListView.Items[Index].Data <> Data)) do
      Inc(Index);

    if ((Index = ListView.Items.Count) and (ListView.Items.Count > 0)) then
    begin
      Item := TListItem.Create(ListView.Items);
      Item.Data := Data;
      UpdateItem(Item, Data);

      Left := 0;
      Right := ListView.Items.Count - 1;
      while (Left <= Right) do
      begin
        Mid := (Right - Left) div 2 + Left;
        case (Compare(Kind, ListView.Items[Mid], Item)) of
          -1: begin Left := Mid + 1; Index := Mid + 1; end;
          0: raise ERangeError.Create(SRangeError);
          1: begin Right := Mid - 1; Index := Mid; end;
        end;
      end;

      Item.Free();
    end;

    if (Index = ListView.Items.Count) then
    begin
      Result := ListView.Items.Add();
      Result.Data := Data;
      ReorderGroup := False;
    end
    else if (ListView.Items[Index].Data <> Data) then
    begin
      Result := ListView.Items.Insert(Index);
      Result.Data := Data;
      ReorderGroup := True;
    end
    else
    begin
      Result := ListView.Items[Index];
      ReorderGroup := True;
    end;
    UpdateItem(Result, Data);

    if (ReorderGroup and ListView.GroupView) then
    begin
      GroupID := Result.GroupID;
      for I := 0 to ListView.Items.Count - 1 do
        if (ListView.Items[I].GroupID = GroupID) then
        begin
          ListView.Items[I].GroupID := 0;
          ListView.Items[I].GroupID := GroupID; // Why is this needed (in Delphi XE2)???
        end;
    end;
  end;

  function AddItem(const Kind: TADesktop.TListViewKind; const Data: TObject): TListItem;
  begin
    Result := ListView.Items.Add();
    Result.Data := Data;
    UpdateItem(Result, Data);
  end;

  procedure UpdateGroup(const Kind: TADesktop.TListViewKind; const GroupID: Integer; const CItems: TCItems);
  var
    Add: Boolean;
    ColumnWidths: array [0..7] of Integer;
    Count: Integer;
    Header: string;
    I: Integer;
    Index: Integer;
    Item: TListItem;
    J: Integer;
    NewIndex: Integer;
    S: string;
    ItemSelected: Boolean;
    ItemFocused: Boolean;
  begin
    case (ClientEvent.EventType) of
      ceItemsValid:
        begin
          ListView.Columns.BeginUpdate();
          for I := 0 to ListView.Columns.Count - 1 do
          begin
            ColumnWidths[I] := ListView.Columns[I].Width;
            ListView.Columns[I].Width := 50; // Make soure no auto column width will be calculated for each item
          end;

          for I := ListView.Items.Count - 1 downto 0 do
            if ((ListView.Items[I].GroupID = GroupID) and (CItems.IndexOf(ListView.Items[I].Data) < 0)) then
              ListView.Items.Delete(I);

          Add := ListView.Items.Count = 0;
          for I := 0 to CItems.Count - 1 do
            if (not (CItems is TCTriggers) or (TCTriggers(CItems)[I].Table = ClientEvent.Sender)) then
              if (not Add) then
                InsertItem(Kind, CItems[I])
              else
                AddItem(Kind, CItems[I]);

          ListView.Columns.EndUpdate();

          for I := 0 to ListView.Columns.Count - 1 do
            if ((Kind = lkProcesses) and (I = 5)) then
              ListView.Columns[I].Width := Preferences.GridMaxColumnWidth
            else if ((Kind in [lkServer, lkDatabase, lkTable]) or (ListView.Items.Count > 0)) then
              ListView.Columns[I].Width := ColumnWidths[I]
            else if (ListView.Items.Count = 0) then
              ListView.Columns[I].Width := ColumnHeaderWidth
            else
              ListView.Columns[I].Width := ColumnTextWidth;
        end;
      ceItemValid:
        for I := 0 to ListView.Items.Count - 1 do
          if (ListView.Items[I].Data = ClientEvent.CItem) then
            UpdateItem(ListView.Items[I], ClientEvent.CItem);
      ceItemCreated:
        begin
          Item := InsertItem(Kind, ClientEvent.CItem);
          if (not Assigned(ListView.Selected)) then
          begin
            Item.Selected := True;
            Item.Focused := True;
          end;
        end;
      ceItemAltered:
        begin
          Index := 0;
          while ((Index < ListView.Items.Count) and (ListView.Items[Index].Data <> ClientEvent.CItem)) do
            Inc(Index);
          if (Index = ListView.Items.Count) then
            InsertItem(Kind, ClientEvent.CItem)
          else if (ListView.Items[Index].Caption = ClientEvent.CItem.Caption) then
            UpdateItem(ListView.Items[Index], ClientEvent.CItem)
          else
          begin
            ItemSelected := ListView.Items[Index].Selected;
            ItemFocused := ListView.Items[Index].Focused;
            ListView.Items.Delete(Index);
            Item := InsertItem(Kind, ClientEvent.CItem);
            Item.Selected := ItemSelected;
            Item.Focused := ItemFocused;
          end;
        end;
      ceItemDropped:
        for I := ListView.Items.Count - 1 downto 0 do
          if (ListView.Items[I].Data = ClientEvent.CItem) then
            ListView.Items.Delete(I);
    end;

    if (ClientEvent.EventType in [ceItemsValid, ceItemCreated, ceItemDropped]) then
      case (GroupID) of
        giDatabases:
          GroupByGroupID(GroupID).Header := ReplaceStr(Preferences.LoadStr(265) + ' (' + IntToStr(ClientEvent.CItems.Count) + ')', '&', '');
        giTables:
          begin
            Header := ReplaceStr(Preferences.LoadStr(234), '&', '');
            if (Client.ServerVersion >= 50001) then
              Header := Header + ' + ' + ReplaceStr(Preferences.LoadStr(873), '&', '');
            Header := Header + ' (' + IntToStr(ClientEvent.CItems.Count) + ')';
            GroupByGroupID(GroupID).Header := Header;
          end;
        giRoutines:
          GroupByGroupID(GroupID).Header := ReplaceStr(Preferences.LoadStr(874) + ' + ' + Preferences.LoadStr(875), '&', '') + ' (' + IntToStr(ClientEvent.CItems.Count) + ')';
        giEvents:
          GroupByGroupID(GroupID).Header := ReplaceStr(Preferences.LoadStr(876), '&', '') + ' (' + IntToStr(ClientEvent.CItems.Count) + ')';
        giKeys:
          GroupByGroupID(GroupID).Header := ReplaceStr(Preferences.LoadStr(458), '&', '') + ' (' + IntToStr(TCBaseTable(ClientEvent.Sender).Keys.Count) + ')';
        giFields:
          GroupByGroupID(GroupID).Header := ReplaceStr(Preferences.LoadStr(253), '&', '') + ' (' + IntToStr(TCTable(ClientEvent.Sender).Fields.Count) + ')';
        giForeignKeys:
          GroupByGroupID(GroupID).Header := ReplaceStr(Preferences.LoadStr(459), '&', '') + ' (' + IntToStr(TCBaseTable(ClientEvent.Sender).ForeignKeys.Count) + ')';
        giTriggers:
          begin
            Count := 0;
            for I := 0 to TCTriggers(CItems).Count - 1 do
              if (TCTriggers(CItems)[I].Table = TCBaseTable(ClientEvent.Sender)) then
              begin
                InsertItem(Kind, TCTriggers(CItems)[I]);
                Inc(Count);
              end;
            GroupByGroupID(GroupID).Header := ReplaceStr(Preferences.LoadStr(797), '&', '') + ' (' + IntToStr(Count) + ')';
          end;
        giHosts:
          GroupByGroupID(GroupID).Header := ReplaceStr(Preferences.LoadStr(335), '&', '') + ' (' + IntToStr(Client.Hosts.Count) + ')';
        giProcesses:
          GroupByGroupID(GroupID).Header := ReplaceStr(Preferences.LoadStr(24), '&', '') + ' (' + IntToStr(Client.Processes.Count) + ')';
        giStati:
          GroupByGroupID(GroupID).Header := ReplaceStr(Preferences.LoadStr(23), '&', '') + ' (' + IntToStr(Client.Stati.Count) + ')';
        giUsers:
          GroupByGroupID(GroupID).Header := ReplaceStr(Preferences.LoadStr(561), '&', '') + ' (' + IntToStr(Client.Users.Count) + ')';
        giVariables:
          GroupByGroupID(GroupID).Header := ReplaceStr(Preferences.LoadStr(22), '&', '') + ' (' + IntToStr(Client.Variables.Count) + ')';
      end;
  end;

var
  ChangingEvent: TLVChangingEvent;
  I: Integer;
  Kind: TADesktop.TListViewKind;
  Table: TCTable;
begin
  if (Assigned(ListView) and (Assigned(ClientEvent.CItems) or (ClientEvent.Sender is TCTable))) then
  begin
    ChangingEvent := ListView.OnChanging;
    ListView.OnChanging := nil;
    ListView.DisableAlign();
    ListView.Items.BeginUpdate();

    Kind := ColumnWidthKindFromImageIndex(ImageIndexByData(TObject(ListView.Tag)));

    case (Kind) of
      lkServer:
        begin
          if (ListView.Items.Count = 0) then
          begin
            if (Assigned(Client.Hosts)) then
              InsertItem(Kind, Client.Hosts);
            if (Assigned(Client.Processes)) then
              InsertItem(Kind, Client.Processes);
            if (Assigned(Client.Stati)) then
              InsertItem(Kind, Client.Stati);
            if (Assigned(Client.Users)) then
              InsertItem(Kind, Client.Users);
            if (Assigned(Client.Variables)) then
              InsertItem(Kind, Client.Variables);
            ListViewInitialize(ListView);
          end;

          if (ClientEvent.CItems is TCDatabases) then
            UpdateGroup(Kind, giDatabases, ClientEvent.CItems)
          else if ((ClientEvent.CItems is TCHosts) or (ClientEvent.CItems is TCProcesses) or (ClientEvent.CItems is TCStati) or (ClientEvent.CItems is TCUsers) or (ClientEvent.CItems is TCVariables)) then
          begin
            for I := 0 to ListView.Items.Count - 1 do
              if (ListView.Items[I].Data = ClientEvent.CItems) then
                UpdateItem(ListView.Items[I], ClientEvent.CItems);
          end;
        end;
      lkDatabase:
        begin
          if (ClientEvent.CItems is TCTables) then
            UpdateGroup(Kind, giTables, ClientEvent.CItems)
          else if (ClientEvent.CItems is TCRoutines) then
            UpdateGroup(Kind, giRoutines, ClientEvent.CItems)
          else if (ClientEvent.CItems is TCEvents) then
            UpdateGroup(Kind, giEvents, ClientEvent.CItems);
        end;
      lkTable:
        if (ClientEvent.Sender is TCTable) then
        begin
          Table := TCTable(ClientEvent.Sender);

          if (Table is TCBaseTable) then
            UpdateGroup(Kind, giKeys, TCBaseTable(Table).Keys);
          UpdateGroup(Kind, giFields, Table.Fields);
          if ((Table is TCBaseTable) and Assigned(TCBaseTable(Table).ForeignKeys)) then
            UpdateGroup(Kind, giForeignKeys, TCBaseTable(Table).ForeignKeys);
          if ((Table is TCBaseTable) and Assigned(TCBaseTable(Table).Database.Triggers)) then
            UpdateGroup(Kind, giTriggers, TCBaseTable(Table).Database.Triggers);
        end
        else if ((ClientEvent.Sender is TCDatabase) and (ClientEvent.CItem is TCTrigger)) then
          UpdateGroup(Kind, giTriggers, ClientEvent.CItems);
      lkHosts:
        UpdateGroup(Kind, giHosts, ClientEvent.CItems);
      lkProcesses:
        UpdateGroup(Kind, giProcesses, ClientEvent.CItems);
      lkStati:
        UpdateGroup(Kind, giStati, ClientEvent.CItems);
      lkUsers:
        UpdateGroup(Kind, giUsers, ClientEvent.CItems);
      lkVariables:
        UpdateGroup(Kind, giVariables, ClientEvent.CItems);
    end;

    if ((Window.ActiveControl = ListView) and Assigned(ListView.OnSelectItem)) then
      ListView.OnSelectItem(nil, ListView.Selected, Assigned(ListView.Selected));

    ListView.Items.EndUpdate();
    ListView.EnableAlign();
    ListView.OnChanging := ChangingEvent;
  end;
end;

procedure TFClient.ListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  BaseTable: TCBaseTable;
  Database: TCDatabase;
  I: Integer;
  ListView: TListView;
  Msg: TMsg;
begin
  if ((not (PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) and (Msg.Message = WM_MOUSEMOVE) and (Msg.wParam = MK_LBUTTON)) or (ActiveListView.SelCount <= 1)) and (Sender is TListView)) then
  begin
    ListView := TListView(Sender);

    aPOpenInNewWindow.Enabled := False;
    aPOpenInNewTab.Enabled := False;
    MainAction('aFImportSQL').Enabled := False;
    MainAction('aFImportText').Enabled := False;
    MainAction('aFImportExcel').Enabled := False;
    MainAction('aFImportAccess').Enabled := False;
    MainAction('aFImportSQLite').Enabled := False;
    MainAction('aFImportODBC').Enabled := False;
    MainAction('aFImportXML').Enabled := False;
    MainAction('aFExportSQL').Enabled := False;
    MainAction('aFExportText').Enabled := False;
    MainAction('aFExportExcel').Enabled := False;
    MainAction('aFExportAccess').Enabled := False;
    MainAction('aFExportSQLite').Enabled := False;
    MainAction('aFExportODBC').Enabled := False;
    MainAction('aFExportXML').Enabled := False;
    MainAction('aFExportHTML').Enabled := False;
    MainAction('aFPrint').Enabled := False;
    MainAction('aECopy').Enabled := False;
    MainAction('aEPaste').Enabled := False;
    MainAction('aERename').Enabled := False;
    MainAction('aDCreateDatabase').Enabled := False;
    MainAction('aDCreateTable').Enabled := False;
    MainAction('aDCreateView').Enabled := False;
    MainAction('aDCreateProcedure').Enabled := False;
    MainAction('aDCreateFunction').Enabled := False;
    MainAction('aDCreateTrigger').Enabled := False;
    MainAction('aDCreateEvent').Enabled := False;
    MainAction('aDCreateKey').Enabled := False;
    MainAction('aDCreateField').Enabled := False;
    MainAction('aDCreateForeignKey').Enabled := False;
    MainAction('aDCreateHost').Enabled := False;
    MainAction('aDCreateUser').Enabled := False;
    MainAction('aDDeleteDatabase').Enabled := False;
    MainAction('aDDeleteTable').Enabled := False;
    MainAction('aDDeleteView').Enabled := False;
    MainAction('aDDeleteRoutine').Enabled := False;
    MainAction('aDDeleteKey').Enabled := False;
    MainAction('aDDeleteField').Enabled := False;
    MainAction('aDDeleteForeignKey').Enabled := False;
    MainAction('aDDeleteTrigger').Enabled := False;
    MainAction('aDDeleteEvent').Enabled := False;
    MainAction('aDDeleteHost').Enabled := False;
    MainAction('aDDeleteUser').Enabled := False;
    MainAction('aDEditServer').Enabled := False;
    MainAction('aDEditDatabase').Enabled := False;
    MainAction('aDEditTable').Enabled := False;
    MainAction('aDEditView').Enabled := False;
    MainAction('aDEditRoutine').Enabled := False;
    MainAction('aDEditEvent').Enabled := False;
    MainAction('aDEditKey').Enabled := False;
    MainAction('aDEditField').Enabled := False;
    MainAction('aDEditForeignKey').Enabled := False;
    MainAction('aDEditTrigger').Enabled := False;
    MainAction('aDEmpty').Enabled := False;

    mlOpen.Enabled := False;
    aDDelete.Enabled := False;
    mlEProperties.Action := nil;

    if (not Assigned(Item) or (Item is TListItem)) then
      case (SelectedImageIndex) of
        iiServer:
          begin
            aPOpenInNewWindow.Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex in [iiDatabase, iiSystemDatabase]);
            aPOpenInNewTab.Enabled := aPOpenInNewWindow.Enabled;
            MainAction('aFImportSQL').Enabled := (ListView.SelCount <= 1) and (not Assigned(Client.UserRights) or Client.UserRights.RInsert) or Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aFImportExcel').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aFImportAccess').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aFImportSQLite').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aFImportODBC').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aFExportSQL').Enabled := (ListView.SelCount = 0) or Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aFExportText').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aFExportExcel').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aFExportAccess').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aFExportSQLite').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aFExportODBC').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aFExportXML').Enabled := (ListView.SelCount = 0) or Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aFExportHTML').Enabled := (ListView.SelCount = 0) or Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aFPrint').Enabled := (ListView.SelCount = 0) or Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aECopy').Enabled := ListView.SelCount >= 1;
            MainAction('aEPaste').Enabled := (not Assigned(Item) and Clipboard.HasFormat(CF_MYSQLSERVER) or Assigned(Item) and (Item.ImageIndex = iiDatabase) and Clipboard.HasFormat(CF_MYSQLDATABASE));
            MainAction('aDCreateDatabase').Enabled := (ListView.SelCount = 0) and (not Assigned(Client.UserRights) or Client.UserRights.RCreate);
            MainAction('aDCreateTable').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aDCreateView').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase) and (Client.ServerVersion >= 50001);
            MainAction('aDCreateProcedure').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase) and (Client.ServerVersion >= 50004);
            MainAction('aDCreateFunction').Enabled := MainAction('aDCreateProcedure').Enabled;
            MainAction('aDCreateEvent').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase) and (Client.ServerVersion >= 50106);
            MainAction('aDCreateHost').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiHosts);
            MainAction('aDCreateUser').Enabled := (ListView.SelCount = 1) and Assigned(Item) and (Item.ImageIndex = iiUsers);
            MainAction('aDDeleteDatabase').Enabled := (ListView.SelCount >= 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aDEditServer').Enabled := (ListView.SelCount = 0);
            MainAction('aDEditDatabase').Enabled := (ListView.SelCount >= 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase);
            MainAction('aDEmpty').Enabled := (ListView.SelCount >= 1) and Assigned(Item) and (Item.ImageIndex = iiDatabase);
            aDDelete.Enabled := MainAction('aDDeleteDatabase').Enabled;

            for I := 0 to ListView.Items.Count - 1 do
              if (ListView.Items[I].Selected) then
              begin
                MainAction('aFExportSQL').Enabled := MainAction('aFExportSQL').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
                MainAction('aFExportText').Enabled := MainAction('aFExportText').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
                MainAction('aFExportExcel').Enabled := MainAction('aFExportExcel').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
                MainAction('aFExportAccess').Enabled := MainAction('aFExportAccess').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
                MainAction('aFExportSQLite').Enabled := MainAction('aFExportSQLite').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
                MainAction('aFExportODBC').Enabled := MainAction('aFExportODBC').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
                MainAction('aFExportXML').Enabled := MainAction('aFExportXML').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
                MainAction('aFExportHTML').Enabled := MainAction('aFExportHTML').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
                MainAction('aFPrint').Enabled := MainAction('aFPrint').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
                MainAction('aECopy').Enabled := MainAction('aECopy').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
                MainAction('aDDeleteDatabase').Enabled := MainAction('aDDeleteDatabase').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
                MainAction('aDEditDatabase').Enabled := MainAction('aDEditDatabase').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
                MainAction('aDEmpty').Enabled := MainAction('aDEmpty').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
              end;

            mlOpen.Enabled := ListView.SelCount = 1;

            if (not Assigned(Item)) then
              mlEProperties.Action := MainAction('aDEditServer')
            else
              mlEProperties.Action := MainAction('aDEditDatabase');
          end;
        iiDatabase,
        iiSystemDatabase:
          begin
            Database := TCDatabase(FNavigator.Selected.Data);

            aPOpenInNewWindow.Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex in [iiBaseTable, iiSystemView, iiView, iiProcedure, iiFunction]);
            aPOpenInNewTab.Enabled := aPOpenInNewWindow.Enabled;
            MainAction('aFImportSQL').Enabled := (ListView.SelCount = 0) and (SelectedImageIndex = iiDatabase);
            MainAction('aFImportText').Enabled := ((ListView.SelCount = 0) or (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiBaseTable));
            MainAction('aFImportExcel').Enabled := ((ListView.SelCount = 0) or (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiBaseTable));
            MainAction('aFImportAccess').Enabled := ((ListView.SelCount = 0) or (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiBaseTable));
            MainAction('aFImportSQLite').Enabled := ((ListView.SelCount = 0) or (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiBaseTable));
            MainAction('aFImportODBC').Enabled := ((ListView.SelCount = 0) or (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiBaseTable));
            MainAction('aFImportXML').Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiBaseTable);
            MainAction('aFExportSQL').Enabled := ((ListView.SelCount = 0) or Selected and Assigned(Item) and (Item.ImageIndex in [iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger])) and (SelectedImageIndex = iiDatabase);
            MainAction('aFExportText').Enabled := ((ListView.SelCount = 0) or Selected and Assigned(Item) and (Item.ImageIndex in [iiBaseTable, iiView])) and (SelectedImageIndex = iiDatabase);
            MainAction('aFExportExcel').Enabled := ((ListView.SelCount = 0) or Selected and Assigned(Item) and (Item.ImageIndex in [iiBaseTable, iiView])) and (SelectedImageIndex = iiDatabase);
            MainAction('aFExportAccess').Enabled := ((ListView.SelCount = 0) or Selected and Assigned(Item) and (Item.ImageIndex in [iiBaseTable])) and (SelectedImageIndex = iiDatabase);
            MainAction('aFExportSQLite').Enabled := ((ListView.SelCount = 0) or Selected and Assigned(Item) and (Item.ImageIndex in [iiBaseTable])) and (SelectedImageIndex = iiDatabase);
            MainAction('aFExportODBC').Enabled := ((ListView.SelCount = 0) or Selected and Assigned(Item) and (Item.ImageIndex in [iiBaseTable])) and (SelectedImageIndex = iiDatabase);
            MainAction('aFExportXML').Enabled := ((ListView.SelCount = 0) or Selected and Assigned(Item) and (Item.ImageIndex in [iiBaseTable, iiView])) and (SelectedImageIndex = iiDatabase);
            MainAction('aFExportHTML').Enabled := SelectedImageIndex = iiDatabase;
            MainAction('aFPrint').Enabled := SelectedImageIndex = iiDatabase;
            MainAction('aECopy').Enabled := ListView.SelCount >= 1;
            MainAction('aEPaste').Enabled := (not Assigned(Item) and Clipboard.HasFormat(CF_MYSQLDATABASE) or Assigned(Item) and ((Item.ImageIndex = iiBaseTable) and Clipboard.HasFormat(CF_MYSQLTABLE) or (Item.ImageIndex = iiView) and Clipboard.HasFormat(CF_MYSQLVIEW)));
            MainAction('aERename').Enabled := Assigned(Item) and (ListView.SelCount = 1) and (Item.ImageIndex in [iiBaseTable, iiView, iiEvent]);
            MainAction('aDCreateTable').Enabled := (ListView.SelCount = 0) and (SelectedImageIndex = iiDatabase);
            MainAction('aDCreateView').Enabled := (ListView.SelCount = 0) and (Client.ServerVersion >= 50001) and (SelectedImageIndex = iiDatabase);
            MainAction('aDCreateProcedure').Enabled := (ListView.SelCount = 0) and (Client.ServerVersion >= 50004) and (SelectedImageIndex = iiDatabase);
            MainAction('aDCreateFunction').Enabled := MainAction('aDCreateProcedure').Enabled;
            MainAction('aDCreateEvent').Enabled := (ListView.SelCount = 0) and (Client.ServerVersion >= 50106) and (SelectedImageIndex = iiDatabase);
            MainAction('aDCreateKey').Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiBaseTable);
            MainAction('aDCreateField').Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiBaseTable);
            MainAction('aDCreateForeignKey').Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiBaseTable) and Assigned(TCBaseTable(Item.Data).Engine) and TCBaseTable(Item.Data).Engine.ForeignKeyAllowed;
            MainAction('aDCreateTrigger').Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiBaseTable) and Assigned(Database.Triggers);
            MainAction('aDDeleteTable').Enabled := (ListView.SelCount >= 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiBaseTable);
            MainAction('aDDeleteView').Enabled := (ListView.SelCount >= 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiView);
            MainAction('aDDeleteRoutine').Enabled := (ListView.SelCount >= 1) and Selected and Assigned(Item) and (Item.ImageIndex in [iiProcedure, iiFunction]);
            MainAction('aDDeleteEvent').Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiEvent);
            MainAction('aDEditDatabase').Enabled := (ListView.SelCount = 0) and (SelectedImageIndex = iiDatabase);
            MainAction('aDEditTable').Enabled := (ListView.SelCount >= 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiBaseTable);
            MainAction('aDEditView').Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiView);
            MainAction('aDEditRoutine').Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex in [iiProcedure, iiFunction]);
            MainAction('aDEditEvent').Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiEvent);
            MainAction('aDEmpty').Enabled := (ListView.SelCount >= 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiBaseTable);
            aDDelete.Enabled := (ListView.SelCount >= 1);

            for I := 0 to ListView.Items.Count - 1 do
              if (ListView.Items[I].Selected) then
              begin
                MainAction('aFExportSQL').Enabled := MainAction('aFExportSQL').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]);
                MainAction('aFExportText').Enabled := MainAction('aFExportText').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable, iiView]);
                MainAction('aFExportExcel').Enabled := MainAction('aFExportExcel').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable, iiView]);
                MainAction('aFExportAccess').Enabled := MainAction('aFExportAccess').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable]);
                MainAction('aFExportSQLite').Enabled := MainAction('aFExportSQLite').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable]);
                MainAction('aFExportODBC').Enabled := MainAction('aFExportODBC').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable]);
                MainAction('aFExportXML').Enabled := MainAction('aFExportXML').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable, iiView]);
                MainAction('aFExportHTML').Enabled := MainAction('aFExportHTML').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable, iiView]);
                MainAction('aFPrint').Enabled := MainAction('aFPrint').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable, iiView]);
                MainAction('aECopy').Enabled := MainAction('aECopy').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent]);
                MainAction('aDDeleteTable').Enabled := MainAction('aDDeleteTable').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable]);
                MainAction('aDDeleteView').Enabled := MainAction('aDDeleteView').Enabled and (ListView.Items[I].ImageIndex in [iiView]);
                MainAction('aDDeleteRoutine').Enabled := MainAction('aDDeleteRoutine').Enabled and (ListView.Items[I].ImageIndex in [iiProcedure, iiFunction]);
                MainAction('aDDeleteEvent').Enabled := MainAction('aDDeleteEvent').Enabled and (ListView.Items[I].ImageIndex in [iiEvent]);
                MainAction('aDEditTable').Enabled := MainAction('aDEditTable').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable]);
                MainAction('aDEditView').Enabled := MainAction('aDEditView').Enabled and (ListView.Items[I].ImageIndex in [iiView]);
                MainAction('aDEditRoutine').Enabled := MainAction('aDEditRoutine').Enabled and (ListView.Items[I].ImageIndex in [iiProcedure, iiFunction]);
                MainAction('aDEditEvent').Enabled := MainAction('aDEditEvent').Enabled and (ListView.Items[I].ImageIndex in [iiEvent]);
                MainAction('aDEmpty').Enabled := MainAction('aDEmpty').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable]);
                aDDelete.Enabled := aDDelete.Enabled and not (ListView.Items[I].ImageIndex in [iiSystemView]);
              end;

            mlOpen.Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex in [iiBaseTable, iiSystemView, iiView, iiProcedure, iiFunction, iiEvent]);

            if (SelectedImageIndex = iiSystemDatabase) then
              mlEProperties.Action := nil
            else if (not Assigned(Item)) then
              mlEProperties.Action := MainAction('aDEditDatabase')
            else
              case (Item.ImageIndex) of
                iiBaseTable,
                iiSystemView: mlEProperties.Action := MainAction('aDEditTable');
                iiView: mlEProperties.Action := MainAction('aDEditView');
                iiProcedure,
                iiFunction: mlEProperties.Action := MainAction('aDEditRoutine');
                iiEvent: mlEProperties.Action := MainAction('aDEditEvent');
              end;
          end;
        iiBaseTable:
          begin
            BaseTable := TCBaseTable(FNavigator.Selected.Data);

            MainAction('aFImportText').Enabled := (ListView.SelCount = 0);
            MainAction('aFImportExcel').Enabled := (ListView.SelCount = 0);
            MainAction('aFImportAccess').Enabled := (ListView.SelCount = 0);
            MainAction('aFImportSQLite').Enabled := (ListView.SelCount = 0);
            MainAction('aFImportODBC').Enabled := (ListView.SelCount = 0);
            MainAction('aFImportXML').Enabled := (ListView.SelCount = 0);
            MainAction('aFExportSQL').Enabled := (ListView.SelCount = 0) or Selected and Assigned(Item) and (Item.ImageIndex in [iiTrigger]);
            MainAction('aFExportText').Enabled := (ListView.SelCount = 0);
            MainAction('aFExportExcel').Enabled := (ListView.SelCount = 0);
            MainAction('aFExportAccess').Enabled := (ListView.SelCount = 0);
            MainAction('aFExportSQLite').Enabled := (ListView.SelCount = 0);
            MainAction('aFExportODBC').Enabled := (ListView.SelCount = 0);
            MainAction('aFExportXML').Enabled := (ListView.SelCount = 0);
            MainAction('aFExportHTML').Enabled := (ListView.SelCount = 0);
            MainAction('aECopy').Enabled := (ListView.SelCount >= 1);
            MainAction('aEPaste').Enabled := not Assigned(Item) and Clipboard.HasFormat(CF_MYSQLTABLE);
            MainAction('aERename').Enabled := Assigned(Item) and (ListView.SelCount = 1) and ((Item.ImageIndex in [iiField, iiTrigger]) or (Item.ImageIndex = iiForeignKey) and (Client.ServerVersion >= 40013));
            MainAction('aDCreateKey').Enabled := (ListView.SelCount = 0);
            MainAction('aDCreateField').Enabled := (ListView.SelCount = 0);
            MainAction('aDCreateForeignKey').Enabled := (ListView.SelCount = 0) and Assigned(BaseTable.Engine) and BaseTable.Engine.ForeignKeyAllowed;
            MainAction('aDCreateTrigger').Enabled := (ListView.SelCount = 0) and Assigned(BaseTable.Database.Triggers);
            MainAction('aDDeleteKey').Enabled := (ListView.SelCount >= 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiKey);
            MainAction('aDDeleteField').Enabled := (ListView.SelCount >= 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiField) and (BaseTable.Fields.Count > ListView.SelCount);
            MainAction('aDDeleteForeignKey').Enabled := (ListView.SelCount >= 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiForeignKey) and (Client.ServerVersion >= 40013);
            MainAction('aDDeleteTrigger').Enabled := (ListView.SelCount >= 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiTrigger);
            MainAction('aDEditTable').Enabled := (ListView.SelCount = 0);
            MainAction('aDEditKey').Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiKey);
            MainAction('aDEditField').Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiField);
            MainAction('aDEditForeignKey').Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiForeignKey);
            MainAction('aDEditTrigger').Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex = iiTrigger);
            MainAction('aDEmpty').Enabled := (ListView.SelCount = 0) or Selected and Assigned(Item) and (Item.ImageIndex = iiField) and TCBaseTableField(Item.Data).NullAllowed;
            aDDelete.Enabled := (ListView.SelCount >= 1);

            for I := 0 to ListView.Items.Count - 1 do
              if (ListView.Items[I].Selected) then
              begin
                MainAction('aFExportSQL').Enabled := MainAction('aFExportSQL').Enabled and (ListView.Items[I].ImageIndex in [iiTrigger]);
                MainAction('aDDeleteKey').Enabled := MainAction('aDDeleteKey').Enabled and (ListView.Items[I].ImageIndex in [iiKey]);
                MainAction('aDDeleteField').Enabled := MainAction('aDDeleteField').Enabled and (ListView.Items[I].ImageIndex in [iiField]);
                MainAction('aDDeleteForeignKey').Enabled := MainAction('aDDeleteForeignKey').Enabled and (ListView.Items[I].ImageIndex in [iiForeignKey]);
                MainAction('aDEditKey').Enabled := MainAction('aDEditKey').Enabled and (ListView.Items[I].ImageIndex in [iiKey]);
                MainAction('aDEditField').Enabled := MainAction('aDEditField').Enabled and (ListView.Items[I].ImageIndex in [iiField]);
                MainAction('aDEditForeignKey').Enabled := MainAction('aDEditForeignKey').Enabled and (ListView.Items[I].ImageIndex in [iiForeignKey]);
                MainAction('aDEditTrigger').Enabled := MainAction('aDEditTrigger').Enabled and (ListView.Items[I].ImageIndex in [iiTrigger]);
                MainAction('aDEmpty').Enabled := MainAction('aDEmpty').Enabled and (ListView.Items[I].ImageIndex in [iiField]);
              end;

            mlOpen.Enabled := (ListView.SelCount = 1) and Selected and Assigned(Item) and (Item.ImageIndex in [iiForeignKey, iiTrigger]);

            if (not Assigned(Item)) then
              mlEProperties.Action := MainAction('aDEditTable')
            else
              case (Item.ImageIndex) of
                iiKey: mlEProperties.Action := MainAction('aDEditKey');
                iiField: mlEProperties.Action := MainAction('aDEditField');
                iiForeignKey: mlEProperties.Action := MainAction('aDEditForeignKey');
                iiTrigger: mlEProperties.Action := MainAction('aDEditTrigger');
              end;
          end;
        iiSystemView:
          begin
            MainAction('aECopy').Enabled := (ListView.SelCount >= 1);
          end;
        iiView:
          begin
            MainAction('aFExportSQL').Enabled := (ListView.SelCount = 0);
            MainAction('aFExportText').Enabled := (ListView.SelCount = 0);
            MainAction('aFExportExcel').Enabled := (ListView.SelCount = 0);
            MainAction('aFExportXML').Enabled := (ListView.SelCount = 0);
            MainAction('aFExportHTML').Enabled := (ListView.SelCount = 0);
            MainAction('aFExportSQLite').Enabled := (ListView.SelCount = 0);
            MainAction('aECopy').Enabled := (ListView.SelCount >= 1);
            MainAction('aEPaste').Enabled := not Assigned(Item) and Clipboard.HasFormat(CF_MYSQLVIEW);
            MainAction('aDEditView').Enabled := (ListView.SelCount = 0);

            mlEProperties.Action := MainAction('aDEditView');
          end;
        iiHosts:
          begin
            MainAction('aECopy').Enabled := (ListView.SelCount >= 1);
            MainAction('aEPaste').Enabled := not Assigned(Item) and Clipboard.HasFormat(CF_MYSQLHOSTS);
            MainAction('aDCreateHost').Enabled := (ListView.SelCount = 0);
            MainAction('aDDeleteHost').Enabled := (ListView.SelCount >= 1);
            MainAction('aDEditHost').Enabled := (ListView.SelCount = 1);
            aDDelete.Enabled := (ListView.SelCount >= 1);

            mlEProperties.Action := MainAction('aDEditHost');
          end;
        iiProcesses:
          begin
            MainAction('aDDeleteProcess').Enabled := (ListView.SelCount >= 1) and Selected and Assigned(Item) and (Trunc(SysUtils.StrToInt(Item.Caption)) <> Client.ThreadId);
            MainAction('aDEditProcess').Enabled := (ListView.SelCount = 1);
            aDDelete.Enabled := (ListView.SelCount >= 1);

            mlEProperties.Action := MainAction('aDEditProcess');
          end;
        iiUsers:
          begin
            MainAction('aECopy').Enabled := (ListView.SelCount >= 1);
            MainAction('aEPaste').Enabled := not Assigned(Item) and Clipboard.HasFormat(CF_MYSQLUSERS);
            MainAction('aDCreateUser').Enabled := (ListView.SelCount = 0);
            MainAction('aDDeleteUser').Enabled := (ListView.SelCount >= 1);
            MainAction('aDEditUser').Enabled := (ListView.SelCount = 1);
            aDDelete.Enabled := (ListView.SelCount >= 1);

            mlEProperties.Action := MainAction('aDEditUser');
          end;
        iiVariables:
          begin
            MainAction('aDEditVariable').Enabled := (ListView.SelCount = 1);

            mlEProperties.Action := MainAction('aDEditVariable');
          end;
      end;

    mlOpen.Default := mlOpen.Enabled and not (Assigned(Item) and (Item.ImageIndex = iiForeignKey));
    mlEProperties.Default := Assigned(Item) and not mlOpen.Default and mlEProperties.Enabled;
    mlEProperties.Caption := Preferences.LoadStr(97) + '...';
    mlEProperties.ShortCut := ShortCut(VK_RETURN, [ssAlt]);

    ToolBarData.tbPropertiesAction := mlEProperties.Action;
    Window.Perform(CM_UPDATETOOLBAR, 0, LPARAM(Self));

    ShowEnabledItems(MList.Items);

    if (Sender <> MList) then
      StatusBarRefresh();
  end;
end;

procedure TFClient.mbBOpenClick(Sender: TObject);
begin
  Wanted.Clear();

  if (Assigned(FBookmarks.Selected)) then
    Address := Client.Account.Desktop.Bookmarks.ByCaption(FBookmarks.Selected.Caption).URI;
end;

procedure TFClient.MBookmarksPopup(Sender: TObject);
begin
  ShowEnabledItems(MBookmarks.Items);
end;

procedure TFClient.MetadataProviderGetSQLFieldNames(Sender: TacBaseMetadataProvider;
  const ASQL: WideString; AFields: TacFieldsList);
var
  Database: TCDatabase;
  DatabaseName: string;
  I: Integer;
  Parse: TSQLParse;
  Table: TCTable;
  TableName: string;
begin
  if (SQLCreateParse(Parse, PChar(ASQL), Length(ASQL), Client.ServerVersion)
    and SQLParseKeyword(Parse, 'SELECT')) then
  begin
    repeat
      SQLParseValue(Parse);
    until (SQLParseEnd(Parse) or not SQLParseChar(Parse, ','));
    if (SQLParseKeyword(Parse, 'FROM')) then
    begin
      DatabaseName := SelectedDatabase;
      if (SQLParseObjectName(Parse, DatabaseName, TableName)) then
      begin
        Database := Client.DatabaseByName(DatabaseName);
        if (Assigned(Database)) then
        begin
          Table := Database.TableByName(TableName);
          if (Assigned(Table)) then
          begin
            Client.BeginSynchron();
            if (Table.Update()) then
              for I := 0 to Table.Fields.Count - 1 do
                AFields.AddField(Table.Fields[I].Name, Client.LowerCaseTableNames = 0);
            Client.EndSynchron();
          end;
        end;
      end;
    end;
  end;
end;

procedure TFClient.mfDeleteClick(Sender: TObject);
begin
  FFiles.InvokeCommandOnSelected('delete');
end;

procedure TFClient.mfFilterAccessClick(Sender: TObject);
begin
  FFiles.Filter := '*.mdb';
end;

procedure TFClient.mfFilterClearClick(Sender: TObject);
begin
  FFiles.Filter := '*';
  mfFilter.Checked := False;
  mfFilterText.Checked := False;
end;

procedure TFClient.mfFilterExcelClick(Sender: TObject);
begin
  FFiles.Filter := '*.xls';
end;

procedure TFClient.mfFilterHTMLClick(Sender: TObject);
begin
  FFiles.Filter := '*.html;*.htm';
end;

procedure TFClient.mfFilterSQLClick(Sender: TObject);
begin
  FFiles.Filter := '*.sql';
end;

procedure TFClient.mfFilterSQLiteClick(Sender: TObject);
begin
  FFiles.Filter := '*.sqlite';
end;

procedure TFClient.mfFilterTextClick(Sender: TObject);
begin
  FFiles.Filter := '*.txt;*.csv';
end;

procedure TFClient.mfFilterXMLClick(Sender: TObject);
begin
  FFiles.Filter := '*.xml';
end;

procedure TFClient.MFilesPopup(Sender: TObject);
begin
  mfFilterClear.Checked := FFiles.Filter = '*';
  mfFilterSQL.Checked := FFiles.Filter = '*.sql';
  mfFilterText.Checked := FFiles.Filter = '*.txt;*.csv';
  mfFilterAccess.Checked := FFiles.Filter = '*.mdb';
  mfFilterExcel.Checked := FFiles.Filter = '*.xls';
  mfFilterSQLite.Checked := FFiles.Filter = '*.sqlite';
  mfFilterHTML.Checked := FFiles.Filter = '*.html;*.htm';
  mfFilterXML.Checked := FFiles.Filter = '*.xml';

  mfOpen.Enabled := FFiles.SelCount = 1;
  aPOpenInNewWindow.Enabled := (FFiles.SelCount = 1) and (LowerCase(ExtractFileExt(FFolders.SelectedFolder + PathDelim + FFiles.Selected.Caption)) = '.sql');
  aPOpenInNewTab.Enabled := aPOpenInNewWindow.Enabled;
  mfFilter.Enabled := FFiles.SelCount = 0;
  mfDelete.Enabled := FFiles.SelCount = 1;
  mfRename.Enabled := FFiles.SelCount = 1;
  mfProperties.Enabled := FFiles.SelCount = 1;

  ShowEnabledItems(MFiles.Items);
end;

procedure TFClient.mfOpenClick(Sender: TObject);
begin
  if (Assigned(FFiles.Selected) and (LowerCase(ExtractFileExt(FFolders.SelectedFolder + PathDelim + FFiles.Selected.Caption)) = '.sql')) then
  begin
    View := vEditor;

    if (Boolean(Perform(CM_CLOSE_TAB_QUERY, 0, 0))) then
      OpenSQLFile(FFolders.SelectedFolder + PathDelim + FFiles.Selected.Caption);
  end
  else
    FFiles.InvokeCommandOnSelected('open');
end;

procedure TFClient.mfPropertiesClick(Sender: TObject);
begin
  FFiles.InvokeCommandOnSelected('properties');
end;

procedure TFClient.mfRenameClick(Sender: TObject);
begin
  FFiles.Selected.EditCaption();
end;

procedure TFClient.MGridHeaderMenuOrderClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  SortDef: TIndexDef;
begin
  Wanted.Clear();

  if (not IgnoreFGridTitleClick) then
  begin
    MenuItem := TMenuItem(Sender);

    SortDef := TIndexDef.Create(nil, '', '', []);
    if (MenuItem.Checked) then
      MenuItem.Checked := False
    else if ((View = vBrowser) and (SelectedImageIndex in [iiBaseTable, iiSystemView])) then
      TCBaseTable(FNavigator.Selected.Data).Keys[MenuItem.Tag].GetSortDef(SortDef);

    TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).Sort(SortDef);
    ActiveDBGrid.UpdateHeader();
    SortDef.Free();
  end;

  IgnoreFGridTitleClick := False;
end;

procedure TFClient.MGridHeaderPopup(Sender: TObject);
var
  I: Integer;
  Key: TCKey;
  SortMenuItem: TMenuItem;
  Table: TCBaseTable;
begin
  MainAction('aVDetails').Enabled := True;

  if (SelectedImageIndex in [iiBaseTable, iiSystemView]) then
  begin
    Table := TCBaseTable(FNavigator.Selected.Data);

    SortMenuItem := nil;
    for I := 0 to MGridHeader.Items.Count - 1 do
      if (MGridHeader.Items[I].Count > 0) then
        SortMenuItem := TMenuItem(MGridHeader.Items[I]);

    if (Assigned(SortMenuItem) and (ActiveDBGrid.DataSource.DataSet is TCTableDataSet)) then
    begin
      Key := Table.KeyByDataSet(TCTableDataSet(ActiveDBGrid.DataSource.DataSet));
      for I := 0 to SortMenuItem.Count - 1 do
        if (SortMenuItem.Items[I].Tag >= 0) then
          SortMenuItem.Items[I].Checked := Assigned(Key) and (Key.Index = SortMenuItem.Items[I].Tag);
    end;
  end;
end;

procedure TFClient.MGridPopup(Sender: TObject);

  procedure AddFilterMenuItem(const Field: TField; const Value: string; const FilterIndex: Integer);
  var
    NewMenuItem: TMenuItem;
  begin
    NewMenuItem := TMenuItem.Create(Self);
    NewMenuItem.AutoHotkeys := maManual;
    if (FilterIndex < 0) then
      NewMenuItem.Caption := '-'
    else
    begin
      NewMenuItem.Caption := Format(Filters[FilterIndex].Text, [Field.DisplayName, Value]);
      NewMenuItem.OnClick := gmFilterIntoFilterClick;
      NewMenuItem.Tag := FilterIndex;
    end;
    gmFilter.Add(NewMenuItem);
  end;

var
  ClientCoord: TPoint;
  GridCoord: TGridCoord;
  I: Integer;
  NewMenuItem: TMenuItem;
  Value: string;
begin
  ClientCoord := ActiveDBGrid.ScreenToClient(MGrid.PopupPoint);
  GridCoord := ActiveDBGrid.MouseCoord(ClientCoord.X, ClientCoord.Y);

  if (GridCoord.X < 0) then
  begin
    for I := 0 to MGrid.Items.Count - 1 do
      MGrid.Items[I].Visible := False;

    ShowEnabledItems(gmFExport);
    gmFExport.Visible := not ActiveDBGrid.EditorMode;
  end
  else
  begin
    gmFilter.Clear(); gmFilter.Enabled := False;

    if (not ActiveDBGrid.EditorMode and not (ActiveDBGrid.SelectedField.DataType in [ftWideMemo, ftBlob])) then
    begin
      gmFilter.Enabled := True;

      if (((ActiveDBGrid.DataSource.DataSet is TMySQLTable) and (TMySQLTable(ActiveDBGrid.DataSource.DataSet).FilterSQL <> ''))
        or not (ActiveDBGrid.DataSource.DataSet is TMySQLTable) and ActiveDBGrid.DataSource.DataSet.Filtered) then
      begin
        NewMenuItem := TMenuItem.Create(Self);
        NewMenuItem.Caption := Preferences.LoadStr(28);
        NewMenuItem.OnClick := gmFilterClearClick;
        gmFilter.Add(NewMenuItem);

        NewMenuItem := TMenuItem.Create(Self);
        NewMenuItem.Caption := '-';
        gmFilter.Add(NewMenuItem);
      end;

      if (not ActiveDBGrid.SelectedField.Required) then
      begin
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 0);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 1);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, '', -1);
      end;

      if (not ActiveDBGrid.SelectedField.IsNull) then
      begin
        if (ActiveDBGrid.SelectedField.DataType in UnquotedDataTypes) then
          Value := ActiveDBGrid.SelectedField.DisplayText
        else
          Value := SQLEscape(ActiveDBGrid.SelectedField.DisplayText);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 2);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 3);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 4);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 5);
        if (ActiveDBGrid.SelectedField.DataType in [ftString, ftWideString]) then
          AddFilterMenuItem(ActiveDBGrid.SelectedField, SQLEscape('%' + ActiveDBGrid.SelectedField.DisplayText + '%'), 6);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, '', -1);
      end;

      if (ActiveDBGrid.SelectedField.DataType in UnquotedDataTypes) then
        Value := '...'
      else
        Value := SQLEscape('...');
      AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 7);
      AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 8);
      AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 9);
      AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 10);
      if (ActiveDBGrid.SelectedField.DataType in [ftString, ftWideString]) then
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 11);
    end;

    ShowEnabledItems(MGrid.Items);

    gmDInsertRecord.Visible := gmDInsertRecord.Visible and not ActiveDBGrid.EditorMode;
    gmDDeleteRecord.Visible := gmDDeleteRecord.Visible and not ActiveDBGrid.EditorMode;
    gmFExport.Visible := gmFExport.Visible and not ActiveDBGrid.EditorMode;
    gmFilter.Visible := gmFilter.Visible and not ActiveDBGrid.EditorMode;
    gmDEditRecord.Visible := gmDEditRecord.Visible and not ActiveDBGrid.EditorMode;
  end;
end;

procedure TFClient.MGridTitleMenuVisibleClick(Sender: TObject);
var
  I: Integer;
  Index: Integer;
begin
  Wanted.Clear();

  Index := -1;
  for I := 0 to ActiveDBGrid.Columns.Count - 1 do
    if (ActiveDBGrid.Columns[I].Field.FieldNo - 1 = MGridHeader.Items.IndexOf(TMenuItem(Sender))) then
      Index := I;
  if (Index >= 0) then
    ActiveDBGrid.Columns[Index].Visible := not ActiveDBGrid.Columns[Index].Visible;
end;

procedure TFClient.miHOpenClick(Sender: TObject);
begin
  Wanted.Clear();

  if (Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock])
    and Boolean(Perform(CM_CLOSE_TAB_QUERY, 0, 0))) then
  begin
    View := vEditor;
    if (View = vEditor) then
    begin
      FSQLEditorSynMemo.Text := XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'sql').Text;

      Window.ActiveControl := FSQLEditorSynMemo;
    end;
  end;
end;

procedure TFClient.miHPropertiesClick(Sender: TObject);
var
  Node: IXMLNode;
begin
  Node := IXMLNode(FSQLHistoryMenuNode.Data);

  if (not Assigned(XMLNode(Node, 'database'))) then
    DStatement.DatabaseName := ''
  else
    DStatement.DatabaseName := XMLNode(Node, 'database').Text;
  DStatement.DateTime := StrToFloat(XMLNode(Node, 'datetime').Text, FileFormatSettings);
  if (not Assigned(XMLNode(Node, 'info'))) then
    DStatement.Info := ''
  else
    DStatement.Info := XMLNode(Node, 'info').Text;
  if (not Assigned(XMLNode(Node, 'insert_id'))) then
    DStatement.Id := 0
  else
    DStatement.Id := StrToInt64(XMLNode(Node, 'insert_id').Text);
  if (not Assigned(XMLNode(Node, 'rows_affected'))) then
    DStatement.RowsAffected := -1
  else
    DStatement.RowsAffected := SysUtils.StrToInt(XMLNode(Node, 'rows_affected').Text);
  if (not Assigned(XMLNode(Node, 'execution_time'))) then
    DStatement.StatementTime := MySQLZeroDate
  else
    DStatement.StatementTime := StrToFloat(XMLNode(Node, 'execution_time').Text, FileFormatSettings);
  DStatement.SQL := XMLNode(Node, 'sql').Text;
  if (Node.Attributes['type'] = 'query') then
    DStatement.ViewType := vtQuery
  else
    DStatement.ViewType := vtStatement;

  DStatement.Execute();
end;

procedure TFClient.miHSaveAsClick(Sender: TObject);
begin
  Wanted.Clear();

  SaveSQLFile(Sender);
end;

procedure TFClient.miHStatementIntoSQLEditorClick(Sender: TObject);
var
  SelLength: Integer;
  SelStart: Integer;
begin
  Wanted.Clear();

  if (Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock])) then
  begin
    View := vEditor;
    if (View = vEditor) then
    begin
      SelStart := FSQLEditorSynMemo.SelStart;
      FSQLEditorSynMemo.SelText := XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'sql').Text;
      SelLength := FSQLEditorSynMemo.SelStart - SelStart;

      FSQLEditorSynMemo.SelStart := SelStart;
      FSQLEditorSynMemo.SelLength := SelLength;

      Window.ActiveControl := FSQLEditorSynMemo;
    end;
  end;
end;

procedure TFClient.MListPopup(Sender: TObject);
var
  I: Integer;
  Rect: TRect;
begin
  ListViewSelectItem(ActiveListView, ActiveListView.Selected, Assigned(ActiveListView.Selected));

  if (not BOOL(Header_GetItemRect(ListView_GetHeader(ActiveListView.Handle), 0, @Rect)) or (MList.PopupPoint.Y - ActiveListView.ClientOrigin.Y < Rect.Bottom)) then
    for I := 0 to MList.Items.Count - 1 do
      MList.Items[I].Visible := False;
end;

procedure TFClient.mlOpenClick(Sender: TObject);
var
  Child: TTreeNode;
  ForeignKey: TCForeignKey;
  NewNode: TTreeNode;
  URI: TUURI;
begin
  Wanted.Clear();

  case (ActiveListView.Selected.ImageIndex) of
    iiForeignKey:
      begin
        ForeignKey := TCForeignKey(ActiveListView.Selected.Data);
        URI := TUURI.Create(Address);
        URI.Database := ForeignKey.Parent.DatabaseName;
        URI.Table := ForeignKey.Parent.TableName;
        Address := URI.Address;
        URI.Free();
      end;
    else
      begin
        NewNode := nil;
        FNavigator.Selected.Expand(False);
        Child := FNavigator.Selected.getFirstChild();
        while (Assigned(Child)) do
        begin
          if (lstrcmpi(PChar(Child.Text), PChar(ActiveListView.Selected.Caption)) = 0) then
            NewNode := Child;
          Child := FNavigator.Selected.getNextChild(Child);
        end;
        if (Assigned(NewNode)) then
          FNavigator.Selected := NewNode;
      end;
  end;
end;

procedure TFClient.MNavigatorPopup(Sender: TObject);
var
  AllowChange: Boolean;
  P: TPoint;
begin
  KillTimer(Handle, tiStatusBar);
  KillTimer(Handle, tiNavigator);

  AllowChange := True;
  FNavigatorChanging(Sender, FNavigator.Selected, AllowChange);

  if (Sender = FNavigator.PopupMenu) then
  begin
    // Bei einem Click auf den WhiteSpace: FNavigator.Selected zeigt den zuletzt selektierten Node an :-(
    P := GetClientOrigin();
    FNavigatorMenuNode := FNavigator.GetNodeAt(MNavigator.PopupPoint.X - P.x - (PSideBar.Left + PNavigator.Left + FNavigator.Left), MNavigator.PopupPoint.y - P.y - (PSideBar.Top + PNavigator.Top + FNavigator.Top));
  end
  else
    FNavigatorMenuNode := FNavigator.Selected;

  FNavigatorSetMenuItems(Sender, FNavigatorMenuNode);
end;

procedure TFClient.MoveToAddress(const ADiff: Integer);
begin
  if (ADiff <> 0) then
  begin
    MovingToAddress := True;
    ToolBarData.CurrentAddress := ToolBarData.CurrentAddress + ADiff;
    Address := ToolBarData.Addresses[ToolBarData.CurrentAddress];
    MovingToAddress := False;
  end;
end;

procedure TFClient.MSQLEditorPopup(Sender: TObject);
var
  I: Integer;
begin
  if ((View = vBuilder) and not (Window.ActiveControl = FBuilderSynMemo)) then
  begin
    Window.ActiveControl := FBuilderSynMemo;
    SynMemoStatusChange(Sender, []);
  end
  else
  if ((View = vEditor) and not (Window.ActiveControl = FSQLEditorSynMemo)) then
  begin
    Window.ActiveControl := FSQLEditorSynMemo;
    SynMemoStatusChange(Sender, []);
  end;

  ShowEnabledItems(MSQLEditor.Items);

  if (FSQLEditorSynMemo.Gutter.Visible) then
    for I := 0 to MSQLEditor.Items.Count - 1 do
      MSQLEditor.Items[I].Visible := MSQLEditor.Items[I].Visible and (MSQLEditor.PopupPoint.X - FSQLEditorSynMemo.ClientOrigin.X > FSQLEditorSynMemo.Gutter.Width);
end;

procedure TFClient.MSQLHistoryPopup(Sender: TObject);
var
  P: TPoint;
begin
  if (Sender = FSQLHistory.PopupMenu) then
  begin
    // Bei einem Click auf den WhiteSpace: FNavigator.Selected zeigt den zuletzt selektierten Node an :-(
    P := GetClientOrigin();
    FSQLHistoryMenuNode := FSQLHistory.GetNodeAt(MSQLHistory.PopupPoint.x - P.x - (PSideBar.Left + PSQLHistory.Left + FSQLHistory.Left), MSQLHistory.PopupPoint.y - P.y - (PSideBar.Top + PSQLHistory.Top + FSQLHistory.Top));
  end
  else
    FSQLHistoryMenuNode := FSQLHistory.Selected;

  MainAction('aECopy').Enabled := Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery]);

  miHStatementIntoSQLEditor.Enabled := Assigned(FSQLHistoryMenuNode) and (View = vEditor) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock]);
  aPExpand.Enabled := Assigned(FSQLHistoryMenuNode) and not FSQLHistoryMenuNode.Expanded and FSQLHistoryMenuNode.HasChildren;
  aPCollapse.Enabled := Assigned(FSQLHistoryMenuNode) and FSQLHistoryMenuNode.Expanded;
  miHOpen.Enabled := Assigned(FSQLHistoryMenuNode) and (View = vEditor) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock]);
  miHSaveAs.Enabled := Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock]);
  miHRun.Enabled := Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock]);
  miHProperties.Enabled := Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock]) and not FSQLHistoryMenuNode.HasChildren;

  miHExpand.Default := aPExpand.Enabled;
  miHCollapse.Default := aPCollapse.Enabled;
  miHStatementIntoSQLEditor.Default := not aPExpand.Enabled and not aPCollapse.Enabled and miHStatementIntoSQLEditor.Enabled;

  ShowEnabledItems(MSQLHistory.Items);
end;

procedure TFClient.MTextPopup(Sender: TObject);
begin
  ShowEnabledItems(MText.Items);

  tmECut.Visible := True;
  tmECopy.Visible := True;
  tmEPaste.Visible := True;
  tmEDelete.Visible := True;
  tmESelectAll.Visible := True;
end;

procedure TFClient.MToolBarPopup(Sender: TObject);
var
  Checked: Integer;
  I: Integer;
begin
  mtObjects.Checked := ttObjects in Preferences.ToolbarTabs;
  mtBrowser.Checked := ttBrowser in Preferences.ToolbarTabs;
  mtIDE.Checked := ttIDE in Preferences.ToolbarTabs;
  mtBuilder.Checked := ttBuilder in Preferences.ToolbarTabs;
  mtEditor.Checked := ttEditor in Preferences.ToolbarTabs;
  mtDiagram.Checked := ttDiagram in Preferences.ToolbarTabs;

  mtObjects.Enabled := View <> vObjects;
  mtBrowser.Enabled := View <> vBrowser;
  mtIDE.Enabled := View <> vIDE;
  mtBuilder.Enabled := View <> vBuilder;
  mtEditor.Enabled := View <> vEditor;
  mtDiagram.Enabled := View <> vDiagram;

  Checked := 0;
  for I := 0 to MToolBar.Items.Count - 1 do
    if (MToolBar.Items[I].Checked) then
      Inc(Checked);

  for I := 0 to MToolBar.Items.Count - 1 do
    MToolBar.Items[I].Enabled := (Checked > 1) or not MToolBar.Items[I].Checked;
end;

procedure TFClient.mwCreateLinkExecute(Sender: TObject);
var
  MenuItem: TMenuItem;
  P: TPoint;
begin
  Wanted.Clear();

  if (Sender is TMenuItem) then
  begin
    MenuItem := TMenuItem(Sender);

    P := ActiveWorkbench.ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint);
    ActiveWorkbench.CreateNewLink(P.X, P.Y);
  end;
end;

procedure TFClient.mwCreateSectionClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  P: TPoint;
begin
  Wanted.Clear();

  if (Sender is TMenuItem) then
  begin
    MenuItem := TMenuItem(Sender);

    P := ActiveWorkbench.ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint);
    ActiveWorkbench.CreateNewSection(P.X, P.Y);
  end;
end;

procedure TFClient.mwDCreateForeignKeyClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  P: TPoint;
begin
  Wanted.Clear();

  if (Sender is TMenuItem) then
  begin
    MenuItem := TMenuItem(Sender);

    P := ActiveWorkbench.ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint);
    ActiveWorkbench.CreateNewForeignKey(P.X, P.Y);
  end;
end;

procedure TFClient.mwDCreateTableClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  P: TPoint;
begin
  Wanted.Clear();

  if (Sender is TMenuItem) then
  begin
    MenuItem := TMenuItem(Sender);

    P := ActiveWorkbench.ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint);
    ActiveWorkbench.CreateNewTable(P.X, P.Y);
  end;
end;

procedure TFClient.mwERemoveClick(Sender: TObject);
begin
  MainAction('aEDelete').Execute();
end;

procedure TFClient.mwEPasteClick(Sender: TObject);
begin
  WorkbenchPasteExecute(Sender);
end;

procedure TFClient.MWorkbenchPopup(Sender: TObject);
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  mwAddTable.Clear();
  mwEPaste.Enabled := MainAction('aEPaste').Enabled;

  ActiveWorkbench.UpdateAction(MainAction('aEDelete'));
  mwEDelete.Enabled := MainAction('aEDelete').Enabled;
  if ((ActiveWorkbench.Selected is TWTable)) then
    mwEDelete.Caption := Preferences.LoadStr(559)
  else
    mwEDelete.Caption := Preferences.LoadStr(28);


  if (not Assigned(ActiveWorkbench.Selected)) then
    for I := 0 to ActiveWorkbench.Database.Tables.Count - 1 do
      if ((ActiveWorkbench.Database.Tables[I] is TCBaseTable)
        and not Assigned(ActiveWorkbench.TableByBaseTable(TCBaseTable(ActiveWorkbench.Database.Tables[I])))) then
      begin
        MenuItem := TMenuItem.Create(Self);
        MenuItem.Caption := ActiveWorkbench.Database.Tables[I].Name;
        MenuItem.OnClick := WorkbenchAddTable;
        MenuItem.Tag := Integer(ActiveWorkbench.Database.Tables[I]);
        mwAddTable.Add(MenuItem);
      end;
  mwDProperties.Default := mwDProperties.Enabled;

  mwDCreateForeignKey.OnClick := mwDCreateForeignKeyClick;

  ShowEnabledItems(MWorkbench.Items);
end;

function TFClient.NavigatorNodeToAddress(const Node: TTreeNode): string;
var
  URI: TUURI;
begin
  URI := TUURI.Create('');

  URI.Scheme := 'mysql';
  URI.Host := Client.Host;
  if (Client.Port <> MYSQL_PORT) then
    URI.Port := Client.Port;

  if (Assigned(Node)) then
  begin
    if (not (Node.ImageIndex in [iiHosts, iiProcesses, iiStati, iiUsers, iiVariables])) then
      URI.Param['view'] := ViewToParam(View);

    case (Node.ImageIndex) of
      iiServer:
        if ((URI.Param['view'] <> Null) and (URI.Param['view'] <> 'editor')) then URI.Param['view'] := Null;
      iiDatabase,
      iiSystemDatabase:
        begin
          if ((URI.Param['view'] <> Null) and (URI.Param['view'] <> 'editor') and (URI.Param['view'] <> 'builder') and (URI.Param['view'] <> 'diagram')) then URI.Param['view'] := Null;
          URI.Database := Node.Text;
        end;
      iiBaseTable,
      iiSystemView:
        begin
          if ((URI.Param['view'] <> Null) and (URI.Param['view'] <> 'browser')) then URI.Param['view'] := Null;
          URI.Database := Node.Parent.Text;
          URI.Table := Node.Text;
        end;
      iiView:
        begin
          if ((URI.Param['view'] <> Null) and (URI.Param['view'] <> 'browser') and (URI.Param['view'] <> 'ide')) then URI.Param['view'] := ViewToParam(LastTableView);
          URI.Database := Node.Parent.Text;
          URI.Table := Node.Text;
        end;
      iiProcedure:
        begin
          URI.Param['view'] := 'ide';
          URI.Database := Node.Parent.Text;
          URI.Param['objecttype'] := 'procedure';
          URI.Param['object'] := Node.Text;
        end;
      iiFunction:
        begin
          URI.Param['view'] := 'ide';
          URI.Database := Node.Parent.Text;
          URI.Param['objecttype'] := 'function';
          URI.Param['object'] := Node.Text;
        end;
      iiEvent:
        begin
          URI.Param['view'] := 'ide';
          URI.Database := Node.Parent.Text;
          URI.Param['objecttype'] := 'event';
          URI.Param['object'] := Node.Text;
        end;
      iiTrigger:
        begin
          URI.Param['view'] := 'ide';
          URI.Database := Node.Parent.Parent.Text;
          URI.Table := Node.Parent.Text;
          URI.Param['objecttype'] := 'trigger';
          URI.Param['object'] := Node.Text;
        end;
      iiHosts:
        URI.Param['system'] := 'hosts';
      iiProcesses:
        URI.Param['system'] := 'processes';
      iiStati:
        URI.Param['system'] := 'stati';
      iiUsers:
        URI.Param['system'] := 'users';
      iiVariables:
        URI.Param['system'] := 'variables';
    end;

    if (Node = FNavigator.Selected) then
    begin
      if (URI.Param['view'] = 'browser') then
        if (Desktop(TCTable(FNavigator.Selected.Data)).Table.ValidDataSet) then
        begin
           if (Desktop(TCTable(FNavigator.Selected.Data)).Table.DataSet.Offset > 0) then
            URI.Param['offset'] := IntToStr(Desktop(TCTable(FNavigator.Selected.Data)).Table.DataSet.Offset);

          if (Desktop(TCTable(FNavigator.Selected.Data)).Table.DataSet.FilterSQL <> '') then
            URI.Param['filter'] := Desktop(TCTable(FNavigator.Selected.Data)).Table.DataSet.FilterSQL;
        end;
      if (URI.Param['view'] = 'editor') then
      begin
        if (SQLEditor.Filename <> '') then
          URI.Param['file'] := UnescapeURL(SQLEditor.Filename);
        if (SQLEditor.FileCodePage <> 0) then
          URI.Param['cp'] := IntToStr(SQLEditor.FileCodePage);
      end;
    end;
  end;

  Result := URI.Address;

  URI.Free();
end;

procedure TFClient.OnConvertError(Sender: TObject; Text: string);
begin
  fBase.ConvertError(Sender, Text);
end;

procedure TFClient.OpenInNewTabExecute(const DatabaseName, TableName: string; const OpenNewWindow: Boolean = False; const Filename: TFileName = '');
var
  URI: TUURI;
begin
  URI := TUURI.Create('');
  URI.Host := Client.Account.Connection.Host;
  if (Client.Account.Connection.Port <> MYSQL_PORT) then
    URI.Port := Client.Account.Connection.Port;
  URI.Username := Client.Account.Connection.User;
  URI.Password := Client.Account.Connection.Password;
  if (Filename = '') then
  begin
    URI.Database := DatabaseName;
    URI.Table := TableName;
    case (View) of
      vBrowser: if (TableName <> '') then URI.Param['view'] := 'browser';
      vIDE: if (TableName = '') then URI.Param['view'] := 'ide';
      vBuilder: if (TableName = '') then URI.Param['view'] := 'builder';
      vEditor: if (TableName = '') then URI.Param['view'] := 'editor';
      vDiagram: if (TableName = '') then URI.Param['view'] := 'diagram';
    end;
  end
  else
  begin
    URI.Param['view'] := 'editor';
    URI.Database := DatabaseName;
    URI.Param['file'] := EscapeURL(Filename);
  end;

  if (not OpenNewWindow) then
    Window.Perform(CM_ADDTAB, 0, LPARAM(PChar(string(URI.Address))))
  else
    ShellExecute(Application.Handle, 'open', PChar(TFileName(Application.ExeName)), PChar(URI.Address), '', SW_SHOW);

  URI.Free();
end;

procedure TFClient.OpenSQLFile(const AFilename: TFileName; const CodePage: Cardinal = 0; const Insert: Boolean = False);
var
  Answer: Integer;
  FileSize: TLargeInteger;
  Handle: THandle;
  Import: TTImportSQL;
  Text: string;
  URI: TUURI;
begin
  tbEditor.Click();

  OpenDialog.Title := ReplaceStr(Preferences.LoadStr(581), '&', '');
  if (AFilename = '') then
    OpenDialog.InitialDir := Path
  else
    OpenDialog.InitialDir := ExtractFilePath(AFilename);
  OpenDialog.FileName := AFilename;
  OpenDialog.DefaultExt := 'sql';
  OpenDialog.Filter := FilterDescription('sql') + ' (*.sql)|*.sql|' + FilterDescription('*') + ' (*.*)|*.*';
  OpenDialog.Encodings.Text := EncodingCaptions();
  if (CodePage <> 0) then
    OpenDialog.EncodingIndex := OpenDialog.Encodings.IndexOf(CodePageToEncoding(CodePage))
  else
    OpenDialog.EncodingIndex := OpenDialog.Encodings.IndexOf(CodePageToEncoding(Client.CodePage));

  if ((OpenDialog.FileName <> '') or OpenDialog.Execute()) then
  begin
    Path := ExtractFilePath(OpenDialog.FileName);

    Answer := ID_CANCEL;

    Handle := CreateFile(PChar(OpenDialog.FileName),
                         GENERIC_READ,
                         FILE_SHARE_READ,
                         nil,
                         OPEN_EXISTING, 0, 0);

    if (Handle <> INVALID_HANDLE_VALUE) then
      LARGE_INTEGER(FileSize).LowPart := GetFileSize(Handle, @LARGE_INTEGER(FileSize).HighPart);

    if ((Handle = INVALID_HANDLE_VALUE) or (LARGE_INTEGER(FileSize).LowPart = INVALID_FILE_SIZE) and (GetLastError() <> 0)) then
      MsgBox(SysErrorMessage(GetLastError()), Preferences.LoadStr(45), MB_OK + MB_ICONERROR)
    else if ((ActiveSynMemo <> FSQLEditorSynMemo) or (FileSize < TLargeInteger(LargeSQLScriptSize))) then
      Answer := ID_NO
    else
      Answer := MsgBox(Preferences.LoadStr(751), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION);
    CloseHandle(Handle);
    if (Answer = ID_YES) then
    begin
      DImport.ImportType := itSQLFile;
      DImport.Client := Client;
      DImport.Database := Client.DatabaseByName(SelectedDatabase);
      DImport.Table := nil;
      DImport.FileName := OpenDialog.FileName;
      DImport.CodePage := EncodingToCodePage(OpenDialog.Encodings[OpenDialog.EncodingIndex]);
      DImport.Execute();
      Client.Update();
    end
    else if (Answer = ID_NO) then
    begin
      Import := TTImportSQL.Create(OpenDialog.FileName, EncodingToCodePage(OpenDialog.Encodings[OpenDialog.EncodingIndex]), Client, nil);
      Import.OnError := ImportError;
      Import.Text := @Text;
      Import.Execute();

      if (Import.ErrorCount = 0) then
      begin
        FSQLEditorSynMemo.Options := FSQLEditorSynMemo.Options + [eoScrollPastEol];  // Speed up the performance
        if (Insert) then
          FSQLEditorSynMemo.SelText := Text
        else
        begin
          FSQLEditorSynMemo.Text := Text;
          SQLEditor.Filename := Import.Filename;
          SQLEditor.FileCodePage := Import.CodePage;
          URI := TUURI.Create(Address);
          URI.Param['file'] := EscapeURL(SQLEditor.Filename);
          if (SQLEditor.FileCodePage = 0) then
            URI.Param['cp'] := Null
          else
            URI.Param['cp'] := IntToStr(SQLEditor.FileCodePage);
          FAddress := URI.Address;
          AddressChanged(nil);
          URI.Free();
          Client.Account.Desktop.Files.Add(SQLEditor.Filename, SQLEditor.FileCodePage);
          Window.Perform(CM_UPDATETOOLBAR, 0, LPARAM(Self));
        end;
        if (Length(FSQLEditorSynMemo.Lines.Text) < LargeSQLScriptSize) then
          FSQLEditorSynMemo.Options := FSQLEditorSynMemo.Options - [eoScrollPastEol];  // Slow down the performance on large content
        FSQLEditorSynMemo.ClearUndo();
        FSQLEditorSynMemo.Modified := Import.SetCharacterSetApplied;

        Window.Perform(CM_UPDATETOOLBAR, 0, LPARAM(Self));
      end;

      Import.Free();
    end;
  end;
end;

procedure TFClient.PanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    PanelMouseDownPoint := Point(X, Y);
    PanelMouseMove(Sender, Shift, X, Y);
  end;
end;

procedure TFClient.PanelMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Panel: TPanel_Ext;
  Rect: TRect;
begin
  if (Sender is TPanel_Ext) then
  begin
    Panel := TPanel_Ext(Sender);

    Rect.Left := Panel.Width - CloseButton.Bitmap.Width - GetSystemMetrics(SM_CXEDGE) - 1;
    Rect.Top := GetSystemMetrics(SM_CYEDGE) - 1;
    Rect.Right := Rect.Left + CloseButton.Bitmap.Width + 2;
    Rect.Bottom := Rect.Top + CloseButton.Bitmap.Height + 2;

    if (PtInRect(Rect, Point(X, Y))) then
    begin
      SetCapture(Panel.Handle);

      if (PtInRect(Rect, PanelMouseDownPoint)) then
        Frame3D(Panel.Canvas, Rect, clDkGray, clWhite, 1)
      else
        Frame3D(Panel.Canvas, Rect, clWhite, clDkGray, 1);
    end
    else if (ReleaseCapture()) then
      Frame3D(Panel.Canvas, Rect, Panel.Color, Panel.Color, 1);
  end;
end;

procedure TFClient.PanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Panel: TPanel_Ext;
  Rect: TRect;
begin
  if (Button = mbLeft) and (Sender is TPanel_Ext) then
  begin
    Panel := TPanel_Ext(Sender);

    Rect.Left := Panel.Width - CloseButton.Bitmap.Width - GetSystemMetrics(SM_CXEDGE) - 1;
    Rect.Top := GetSystemMetrics(SM_CYEDGE) - 1;
    Rect.Right := Rect.Left + CloseButton.Bitmap.Width + 2;
    Rect.Bottom := Rect.Top + CloseButton.Bitmap.Height + 2;

    if (PtInRect(Rect, Point(X, Y)) and PtInRect(Rect, PanelMouseDownPoint)) then
      if (Sender = PResultHeader) then
      begin
        SResult.Visible := False;
        PResult.Visible := False;
        case (View) of
          vIDE: if (FNavigator.Selected.ImageIndex in [iiProcedure, iiFunction]) then Desktop(TCRoutine(FNavigator.Selected.Data)).CloseIDEResult();
          vBuilder: Desktop(TCDatabase(FNavigator.Selected.Data)).CloseBuilderResult();
          vEditor: SQLEditor.CloseResult();
        end;
        ActiveDBGrid := nil;
      end
      else if (Sender = PLogHeader) then
        MainAction('aVSQLLog').Execute()
      else if (Sender = PSideBarHeader) then
        if (MainAction('aVNavigator').Checked) then
          MainAction('aVNavigator').Execute()
        else if (MainAction('aVBookmarks').Checked) then
          MainAction('aVBookmarks').Execute()
        else if (MainAction('aVExplorer').Checked) then
          MainAction('aVExplorer').Execute()
        else if (MainAction('aVSQLHistory').Checked) then
          MainAction('aVSQLHistory').Execute();
    PanelMouseDownPoint := Point(-1, -1);
  end;
end;

procedure TFClient.PanelPaint(Sender: TObject);
begin
  if ((Sender is TPanel_Ext) and Assigned(CloseButton)) then
    TPanel_Ext(Sender).Canvas.Draw(TPanel_Ext(Sender).Width - CloseButton.Bitmap.Width - GetSystemMetrics(SM_CXEDGE), GetSystemMetrics(SM_CYEDGE), CloseButton.Bitmap)
end;

procedure TFClient.PanelResize(Sender: TObject);
var
  ClientControl: TWinControl;
  Control: TWinControl;
  I: Integer;
  NewHeight: Integer;
  ReduceControl: TWinControl;
  ToReduceHeight: Integer;
begin
  if (Sender is TWinControl) then
  begin
    Control := TWinControl(Sender);

    ClientControl := nil;
    NewHeight := Control.ClientHeight;
    for I := 0 to Control.ControlCount - 1 do
      if (Control.Controls[I].Visible) then
        if ((Control.Controls[I].Align = alClient) and (Control.Controls[I] is TWinControl)) then
          ClientControl := TWinControl(Control.Controls[I])
        else if (Control.Controls[I].Align in [alTop, alBottom]) then
          Dec(NewHeight, Control.Controls[I].Height);

    if (Assigned(ClientControl) and (NewHeight < ClientControl.Constraints.MinHeight)) then
    begin
      ToReduceHeight := ClientControl.Constraints.MinHeight - NewHeight;

      ReduceControl := nil;
      for I := 0 to Control.ControlCount - 1 do
        if (Control.Controls[I].Visible and (Control.Controls[I].Align = alBottom) and (Control.Controls[I].Height > Control.Controls[I].Constraints.MinHeight)) then
          if (not Assigned(ReduceControl) or (Control.Controls[I].Top > ReduceControl.Top) and (Control.Controls[I] <> SBResult) and (Control.Controls[I] <> SResult)) then
            ReduceControl := TWinControl(Control.Controls[I]);
      if (Assigned(ReduceControl)) then
        ReduceControl.Height := ReduceControl.Height - ToReduceHeight;
    end;
  end;
end;

procedure TFClient.PasteExecute(const Node: TTreeNode; const Objects: string);
var
  Database: TCDatabase;
  Found: Boolean;
  I: Integer;
  J: Integer;
  Name: string;
  NewField: TCTableField;
  NewForeignKey: TCForeignKey;
  NewKey: TCKey;
  NewTable: TCBaseTable;
  SourceClient: TCClient;
  SourceDatabase: TCDatabase;
  SourceHost: TCHost;
  SourceRoutine: TCRoutine;
  SourceTable: TCBaseTable;
  SourceURI: TUURI;
  SourceUser: TCUser;
  SourceView: TCView;
  StringList: TStringList;
  Success: Boolean;
  Table: TCBaseTable;
begin
  StringList := TStringList.Create();
  StringList.Text := Objects;

  if (StringList.Count > 0) then
  begin
    SourceURI := TUURI.Create(StringList.Values['Address']);
    SourceClient := Clients.ClientByAccount(Accounts.AccountByURI(SourceURI.Address, Client.Account), SourceURI.Database);
    if (not Assigned(SourceClient) and Assigned(Accounts.AccountByURI(SourceURI.Address))) then
    begin
      SourceClient := TCClient.Create(Clients, Accounts.AccountByURI(SourceURI.Address));
      DConnecting.Client := SourceClient;
      if (not DConnecting.Execute()) then
        FreeAndNil(SourceClient);
    end;

    if (Assigned(SourceClient)) then
    begin
      Success := True;

      case (Node.ImageIndex) of
        iiServer:
          if (SourceClient <> Client) then
          begin
            DTransfer.SourceClient := SourceClient;
            DTransfer.SourceDatabaseName := '';
            for I := 1 to StringList.Count - 1 do
              if (Assigned(SourceClient.DatabaseByName(StringList.ValueFromIndex[I]))) then
              begin
                if (DTransfer.SourceDatabaseName <> '') then
                  DTransfer.SourceDatabaseName := DTransfer.SourceDatabaseName + ',';
                DTransfer.SourceDatabaseName := DTransfer.SourceDatabaseName + StringList.ValueFromIndex[I];
              end;
            if (DTransfer.SourceDatabaseName = '') then
              MessageBeep(MB_ICONERROR)
            else
            begin
              DTransfer.SourceTableName := '';
              DTransfer.DestinationClient := Client;
              DTransfer.DestinationDatabaseName := '';
              DTransfer.DestinationTableName := '';
              DTransfer.Execute();
            end;
          end
          else if (DPaste.Execute()) then
          begin
            DDatabase.Client := Client;
            DDatabase.Database := TCDatabase.Create(Client, Name);
            for I := 1 to StringList.Count - 1 do
            begin
              SourceDatabase := SourceClient.DatabaseByName(StringList.ValueFromIndex[I]);

              if (Success and Assigned(SourceDatabase)) then
              begin
                Name := CopyName(SourceDatabase.Name, Client.Databases);
                if (Client.LowerCaseTableNames = 1) then
                  Name := LowerCase(Name);

                DDatabase.Database.Assign(SourceDatabase);
                DDatabase.Database.Name := Name;
                Success := DDatabase.Execute();
                if (Success) then
                begin
                  SourceDatabase := SourceClient.DatabaseByName(StringList.ValueFromIndex[I]);
                  Success := Client.CloneDatabase(SourceDatabase, Client.DatabaseByName(DDatabase.Name), DPaste.Data);
                  if (Success) then
                    Client.Update();
                end;
              end;
            end;
            DDatabase.Database.Free();
          end;
        iiDatabase:
          begin
            SourceDatabase := SourceClient.DatabaseByName(SourceURI.Database);

            if (not Assigned(SourceDatabase)) then
              MessageBeep(MB_ICONERROR)
            else
            begin
              Database := TCDatabase(Node.Data);

              Found := False;
              for I := 1 to StringList.Count - 1 do
                Found := Found or (StringList.Names[I] = 'Table');

              if (not Assigned(SourceDatabase)) then
                MessageBeep(MB_ICONERROR)
              else if (not Found or DPaste.Execute()) then
              begin
                if (Found and (SourceClient <> Client)) then
                begin
                  DTransfer.SourceClient := SourceClient;
                  DTransfer.SourceDatabaseName := SourceURI.Database;
                  DTransfer.SourceTableName := '';
                  for I := 1 to StringList.Count - 1 do
                    if (Assigned(SourceClient.DatabaseByName(SourceURI.Database).TableByName(StringList.ValueFromIndex[I]))) then
                    begin
                      if (DTransfer.SourceTableName <> '') then
                        DTransfer.SourceTableName := DTransfer.SourceTableName + ',';
                      DTransfer.SourceTableName := DTransfer.SourceTableName + StringList.ValueFromIndex[I];
                    end;
                  if (DTransfer.SourceTableName = '') then
                    MessageBeep(MB_ICONERROR)
                  else
                  begin
                    DTransfer.DestinationClient := Client;
                    DTransfer.DestinationDatabaseName := SelectedDatabase;
                    DTransfer.DestinationTableName := '';
                    DTransfer.Execute();
                  end;
                end
                else
                  for I := 1 to StringList.Count - 1 do
                    if (Success) then
                      if (StringList.Names[I] = 'Table') then
                      begin
                        SourceTable := SourceDatabase.BaseTableByName(StringList.ValueFromIndex[I]);

                        if (not Assigned(SourceTable)) then
                          MessageBeep(MB_ICONERROR)
                        else
                        begin
                          Name := Client.TableName(CopyName(SourceTable.Name, Database.Tables));

                          Success := Database.CloneTable(SourceTable, Name, DPaste.Data);
                        end;
                      end;
                for I := 1 to StringList.Count - 1 do
                  if (Success) then
                    if (StringList.Names[I] = 'View') then
                    begin
                      SourceView := SourceDatabase.ViewByName(StringList.ValueFromIndex[I]);

                      if (not Assigned(SourceView)) then
                        MessageBeep(MB_ICONERROR)
                      else
                      begin
                        Name := CopyName(SourceView.Name, Database.Tables);
                        if (Client.LowerCaseTableNames = 1) then
                          Name := LowerCase(Name);

                        Success := Database.CloneView(SourceView, Name);
                      end;
                    end;
                for I := 1 to StringList.Count - 1 do
                  if (Success) then
                    if (StringList.Names[I] = 'Procedure') then
                    begin
                      SourceRoutine := SourceDatabase.ProcedureByName(StringList.ValueFromIndex[I]);

                      if (not Assigned(SourceRoutine)) then
                        MessageBeep(MB_ICONERROR)
                      else
                      begin
                        Name := SourceRoutine.Name;
                        J := 1;
                        while (Assigned(Database.ProcedureByName(Name))) do
                        begin
                          if (J = 1) then
                            Name := Preferences.LoadStr(680, SourceRoutine.Name)
                          else
                            Name := Preferences.LoadStr(681, SourceRoutine.Name, IntToStr(J));
                          Name := ReplaceStr(Name, ' ', '_');
                          Inc(J);
                        end;

                        Success := Database.CloneRoutine(SourceRoutine, Name);
                      end;
                    end
                    else if (StringList.Names[I] = 'Function') then
                    begin
                      SourceRoutine := SourceDatabase.FunctionByName(StringList.ValueFromIndex[I]);

                      if (not Assigned(SourceRoutine)) then
                        MessageBeep(MB_ICONERROR)
                      else
                      begin
                        Name := SourceRoutine.Name;
                        J := 1;
                        while (Assigned(Database.FunctionByName(Name))) do
                        begin
                          if (J = 1) then
                            Name := Preferences.LoadStr(680, SourceRoutine.Name)
                          else
                            Name := Preferences.LoadStr(681, SourceRoutine.Name, IntToStr(J));
                          Name := ReplaceStr(Name, ' ', '_');
                          Inc(J);
                        end;

                        Success := Database.CloneRoutine(SourceRoutine, Name);
                      end;
                    end;
              end;
            end;
          end;
        iiBaseTable:
          begin
            SourceDatabase := SourceClient.DatabaseByName(SourceURI.Database);
            if (not Assigned(SourceDatabase)) then
              SourceTable := nil
            else
              SourceTable := SourceDatabase.BaseTableByName(SourceURI.Table);

            if (not Assigned(SourceTable)) then
              MessageBeep(MB_ICONERROR)
            else
            begin
              Database := Client.DatabaseByName(Node.Parent.Text);
              Table := Database.BaseTableByName(Node.Text);

              NewTable := TCBaseTable.Create(Database.Tables);
              NewTable.Assign(Table);

              for I := 1 to StringList.Count - 1 do
                if (StringList.Names[I] = 'Field') then
                begin
                  Name := CopyName(StringList.ValueFromIndex[I], NewTable.Fields);

                  NewField := TCBaseTableField.Create(NewTable.Fields);
                  NewField.Assign(SourceTable.FieldByName(StringList.ValueFromIndex[I]));
                  TCBaseTableField(NewField).OriginalName := '';
                  NewField.Name := Name;
                  NewField.FieldBefore := NewTable.Fields[NewTable.Fields.Count - 1];
                  NewTable.Fields.AddField(NewField);
                  NewField.Free();
                end;

              for I := 1 to StringList.Count - 1 do
                if (StringList.Names[I] = 'Key') then
                begin
                  Name := CopyName(StringList.ValueFromIndex[I], NewTable.Keys);

                  NewKey := TCKey.Create(NewTable.Keys);
                  NewKey.Assign(SourceTable.IndexByName(StringList.ValueFromIndex[I]));
                  NewKey.Name := Name;
                  NewTable.Keys.AddKey(NewKey);
                  NewKey.Free();
                end
                else if (StringList.Names[I] = 'ForeignKey') then
                begin
                  Name := CopyName(StringList.ValueFromIndex[I], NewTable.ForeignKeys);

                  NewForeignKey := TCForeignKey.Create(NewTable.ForeignKeys);
                  NewForeignKey.Assign(SourceTable.ForeignKeyByName(StringList.ValueFromIndex[I]));
                  NewForeignKey.Name := Name;
                  NewTable.ForeignKeys.AddForeignKey(NewForeignKey);
                  NewForeignKey.Free();
                end;

              Database.UpdateTable(Table, NewTable);
              NewTable.Free();
            end;
          end;
        iiHosts:
          for I := 1 to StringList.Count - 1 do
            if (Success and (StringList.Names[I] = 'Host')) then
            begin
              SourceHost := SourceClient.HostByName(StringList.ValueFromIndex[I]);

              if (not Assigned(SourceHost)) then
                MessageBeep(MB_ICONERROR)
              else
              begin
                Name := CopyName(SourceHost.Name, Client.Hosts);

                Success := Client.CloneHost(SourceHost, Name);
              end;
            end;
        iiUsers:
          for I := 1 to StringList.Count - 1 do
            if (Success and (StringList.Names[I] = 'User')) then
            begin
              SourceUser := SourceClient.UserByName(StringList.ValueFromIndex[I]);

              if (not Assigned(SourceUser)) then
                MessageBeep(MB_ICONERROR)
              else
              begin
                Name := CopyName(SourceUser.Name, Client.Users);

                Success := Client.CloneUser(SourceUser, Name);
              end;
            end;
      end;
    end;

    SourceURI.Free();
  end;
  StringList.Free();
end;

procedure TFClient.PContentChange(Sender: TObject);

  procedure DisableAligns(const Control: TWinControl);
  var
    I: Integer;
  begin
    with Control do SendMessage(Handle, WM_MOVE, 0, MAKELPARAM(Left, Top));
    Control.DisableAlign();
    for I := 0 to Control.ControlCount - 1 do
      if (Control.Controls[I] is TWinControl) then
        DisableAligns(TWinControl(Control.Controls[I]));
  end;

  procedure EnableAligns(const Control: TWinControl);
  var
    I: Integer;
  begin
    for I := 0 to Control.ControlCount - 1 do
      if (Control.Controls[I] is TWinControl) then
        EnableAligns(TWinControl(Control.Controls[I]));
    if (Control.AlignDisabled) then
      Control.EnableAlign();
  end;

var
  I: Integer;
  NewTop: Integer;
  OldActiveControl: TWinControl;
  PResultVisible: Boolean;
begin
  for I := 0 to Client.Databases.Count - 1 do
    if (Assigned(Desktop(Client.Databases[I]).FWorkbench)) then
      Desktop(Client.Databases[I]).Workbench.Visible := Client.Databases[I].Name = SelectedDatabase;

  if (Sender <> Self) then
  begin
    if (PResult.Align = alBottom) then
      PResultHeight := PResult.Height;

    OldActiveControl := Window.ActiveControl;
    DisableAligns(PContent);

    if (PListView.Align = alClient) then PListView.Align := alNone;
    if (PBuilder.Align = alClient) then PBuilder.Align := alNone;
    if (PSynMemo.Align = alClient) then PSynMemo.Align := alNone;
    if (PResult.Align = alClient) then PResult.Align := alNone;
    PListView.Align := alNone;
    PDataBrowser.Align := alNone;
    PObjectIDE.Align := alNone;
    PBuilder.Align := alNone;
    PSynMemo.Align := alNone;
    SResult.Align := alNone;
    PResult.Align := alNone;
    SBlob.Align := alNone;
    PBlob.Align := alNone;

    PBlob.Visible := False;


    EnableAligns(PContent);

    if (View in [vObjects]) then ActiveListView := GetActiveListView() else ActiveListView := nil;
    if (View in [vIDE]) then ActiveIDEInputDataSet := GetActiveIDEInputDataSet() else ActiveIDEInputDataSet := nil;
    if (View in [vIDE, vBuilder, vEditor]) then ActiveSynMemo := GetActiveSynMemo() else ActiveSynMemo := nil;
    if (View in [vBrowser, vIDE, vBuilder, vEditor]) then ActiveDBGrid := GetActiveDBGrid() else ActiveDBGrid := nil;
    if (View in [vDiagram]) then ActiveWorkbench := GetActiveWorkbench() else ActiveWorkbench := nil;

    if ((View = vBrowser) and Assigned(FNavigator.Selected)) then
    begin
      FUDOffset.Position := 0;
      FUDLimit.Position := Desktop(TCTable(FNavigator.Selected.Data)).Limit;
      FLimitEnabled.Down := Desktop(TCTable(FNavigator.Selected.Data)).Limited;

      PDataBrowser.Top := 0;
      PDataBrowser.Align := alTop;
      PDataBrowser.Visible := True;
    end
    else
      PDataBrowser.Visible := False;

    if (Assigned(ActiveIDEInputDataSet)) then
    begin
      PObjectIDETrigger.Visible := (SelectedImageIndex = iiTrigger);
      PObjectIDEResize(Sender);

      PObjectIDE.Top := 0;
      PObjectIDE.Align := alTop;
      PObjectIDE.Visible := True;
    end
    else
      PObjectIDE.Visible := False;

    case (View) of
      vBrowser: PResultVisible := True;
      vIDE:
        case (FNavigator.Selected.ImageIndex) of
          iiProcedure,
          iiFunction: PResultVisible := Assigned(Desktop(TCRoutine(FNavigator.Selected.Data)).ActiveDBGrid);
          else PResultVisible := False;
        end;
      vBuilder: PResultVisible := Assigned(Desktop(TCDatabase(FNavigator.Selected.Data)).DBGrid);
      vEditor: PResultVisible := Assigned(SQLEditor.ActiveDBGrid);
      else PResultVisible := False;
    end;

    FText.OnChange := nil;
    if (Assigned(OldActiveControl) and (PResultVisible or (OldActiveControl = FObjectIDEGrid)) and (OldActiveControl is TMySQLDBGrid) and (TMySQLDBGrid(OldActiveControl).SelectedField = EditorField)) then
    begin
      if ((OldActiveControl = FObjectIDEGrid) and (PBlob.Parent <> PContent)) then
        PBlob.Parent := PContent
      else if ((OldActiveControl <> FObjectIDEGrid) and (PBlob.Parent <> PResult)) then
        PBlob.Parent := ActiveDBGrid.Parent;
      NewTop := PBlob.Parent.ClientHeight - PBlob.Height;
      for I := 0 to PBlob.Parent.ControlCount - 1 do
        if (PBlob.Parent.Controls[I].Align = alBottom) then
          Dec(NewTop, PBlob.Parent.Controls[I].Height);
      PBlob.Top := NewTop;
      PBlob.Align := alBottom;
      PBlob.Visible := True;
    end
    else
    begin
      PBlob.Visible := False;
      PBlob.Parent := PContent;
    end;
    FText.OnChange := FTextChange;

    if (PBlob.Visible) then
    begin
      SBlob.Parent := PBlob.Parent;
      SBlob.Top := PBlob.Top - SBlob.Height;
      SBlob.Align := alBottom;
      SBlob.Visible := True;
    end
    else
    begin
      SBlob.Visible := False;
      SBlob.Parent := nil;
    end;

    if ((Sender is TMySQLDataSet) and Assigned(ActiveDBGrid) and (ActiveDBGrid = SQLEditor.ActiveDBGrid)) then
    begin
      SBResult.Top := PContent.ClientHeight - SBResult.Height;
      SBResult.Align := alBottom;
      SBResult.Visible := True;
    end
    else
    begin
      SBResult.Align := alNone;
      SBResult.Visible := False;
    end;

    if (PResultVisible and Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and ActiveDBGrid.DataSource.DataSet.Active) then
    begin
      NewTop := PContent.ClientHeight - PResult.Height;
      for I := 0 to PContent.ControlCount - 1 do
        if (PContent.Controls[I].Align = alBottom) then
          Dec(NewTop, PContent.Controls[I].Height);
      PResult.Top := NewTop;
      if (View = vBrowser) then
      begin
        PResult.Align := alClient;
        PResult.Left := 0;
        PResult.Height := PContent.ClientHeight - PResult.Top;
        PResult.Width := PContent.ClientWidth
      end
      else
      begin
        PResult.Align := alBottom;
        PResult.Height := PResultHeight;
      end;
      PResultHeader.Visible := View <> vBrowser;

      PResult.Visible := True;
    end
    else
      PResult.Visible := False;

    if (PResult.Visible and (PResult.Align = alBottom)) then
    begin
      NewTop := PContent.ClientHeight - SResult.Height;
      for I := 0 to PContent.ControlCount - 1 do
        if (PContent.Controls[I].Align = alBottom) then
          Dec(NewTop, PContent.Controls[I].Height);
      SResult.Top := NewTop;
      SResult.Align := alBottom;
      SResult.Visible := True;
    end
    else
      SResult.Visible := False;

    if (View = vDiagram) then
    begin
      PWorkbench.Align := alClient;
      PWorkbench.Visible := True;
      PWorkbench.BringToFront();
    end
    else
      PWorkbench.Visible := False;

    if ((View = vBuilder) and (SelectedImageIndex in [iiServer, iiDatabase, iiSystemDatabase])) then
    begin
      PBuilder.Align := alClient;
      PBuilder.Visible := True;
    end
    else
      PBuilder.Visible := False;

    if ((View = vEditor) and (SelectedImageIndex in [iiServer, iiDatabase, iiSystemDatabase]) or (View = vIDE) and (SelectedImageIndex in [iiView, iiFunction, iiProcedure, iiEvent, iiTrigger])) then
    begin
      PSQLEditorUpdate();
      if (Assigned(ActiveSynMemo)) then ActiveSynMemo.BringToFront();
      PSynMemo.Align := alClient;
      PSynMemo.Visible := True;
    end
    else
      PSynMemo.Visible := False;

    if ((View = vObjects) and not (SelectedImageIndex in [iiKey, iiField, iiForeignKey]) or ((View = vBrowser) and (SelectedImageIndex = iiServer))) then
    begin
      PListView.Align := alClient;
      PListView.Visible := True;
      PListView.BringToFront();
    end
    else
      PListView.Visible := False;
  end;

  PObjectIDEResize(Sender);
  PanelResize(PContent);
  if (Assigned(PResult.OnResize)) then PResult.OnResize(PResult);
end;

procedure TFClient.PContentResize(Sender: TObject);
begin
  PanelResize(Sender);
  PToolBar.Width := PContent.Width;
end;

procedure TFClient.PDataBrowserResize(Sender: TObject);
begin
  TBQuickSearchEnabled.Left := PDataBrowser.ClientWidth - TBQuickSearchEnabled.Width - GetSystemMetrics(SM_CXVSCROLL);
  FQuickSearch.Left := TBQuickSearchEnabled.Left - FQuickSearch.Width;
  TBFilterEnabled.Left := FQuickSearch.Left - TBFilterEnabled.Width;
  FFilter.Width := TBFilterEnabled.Left - FFilter.Left;
end;

procedure TFClient.PGridResize(Sender: TObject);
begin
  if (Assigned(ActiveDBGrid)) then
    ActiveDBGrid.Invalidate();
end;

procedure TFClient.PLogResize(Sender: TObject);
begin
  if (PLog.Visible and (FLog.Lines.Count > 0)) then
    PostMessage(FLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TFClient.PObjectIDEResize(Sender: TObject);
var
  I: Integer;
  NewHeight: Integer;
  ScrollBarInfo: TScrollBarInfo;
begin
  NewHeight := 0;

  Inc(NewHeight, FObjectIDEGrid.DefaultRowHeight);
  if (dgRowLines in FObjectIDEGrid.Options) then
    Inc(NewHeight, FObjectIDEGrid.GridLineWidth);
  Inc(NewHeight, FObjectIDEGrid.DefaultRowHeight);
  
  ZeroMemory(@ScrollBarInfo, SizeOf(ScrollBarInfo));
  ScrollBarInfo.cbSize := SizeOf(ScrollBarInfo);
  GetScrollBarInfo(FObjectIDEGrid.Handle, Integer(OBJID_HSCROLL), ScrollBarInfo);
  if (ScrollBarInfo.rgstate[0] <> STATE_SYSTEM_INVISIBLE) then
    Inc(NewHeight, GetSystemMetrics(SM_CYHSCROLL));

  for I := 0 to PObjectIDE.ControlCount - 1 do
    if (PObjectIDE.Controls[I].Visible and (PObjectIDE.Controls[I].Align in [alTop, alClient, alBottom]) and (PObjectIDE.Controls[I] <> FObjectIDEGrid)) then
      Inc(NewHeight, PObjectIDE.Controls[I].Height);
  PObjectIDE.Height := NewHeight;
end;

function TFClient.PostObject(Sender: TObject): Boolean;
var
  Database: TCDatabase;
  Event: TCEvent;
  NewEvent: TCEvent;
  NewTrigger: TCTrigger;
  NewView: TCView;
  Routine: TCRoutine;
  Trigger: TCTrigger;
  View: TCView;
begin
  Database := Client.DatabaseByName(SelectedDatabase);

  case (SelectedImageIndex) of
    iiView:
      begin
        View := TCView(FNavigator.Selected.Data);

        NewView := TCView.Create(Database.Tables);
        NewView.Assign(View);

        NewView.Stmt := Trim(ActiveSynMemo.Text);

        Database.UpdateView(View, NewView);
        Result := True;

        NewView.Free();
      end;
    iiProcedure,
    iiFunction:
      begin
        Routine := TCRoutine(FNavigator.Selected.Data);

        Result := Database.UpdateRoutine(Routine, Trim(ActiveSynMemo.Text));
      end;
    iiEvent:
      begin
        Event := TCEvent(FNavigator.Selected.Data);

        NewEvent := TCEvent.Create(Database.Events);
        NewEvent.Assign(Event);

        NewEvent.Stmt := Trim(ActiveSynMemo.Text);

        Result := Database.UpdateEvent(Event, NewEvent);

        NewEvent.Free();
      end;
    iiTrigger:
      begin
        Trigger := TCTrigger(FNavigator.Selected.Data);

        NewTrigger := TCTrigger.Create(Database.Triggers);
        NewTrigger.Assign(Trigger);

        NewTrigger.Stmt := Trim(ActiveSynMemo.Text);

        Result := Database.UpdateTrigger(Trigger, NewTrigger);

        NewTrigger.Free();
      end;
    else
      Result := False;
  end;

  if (Result) then
  begin
    GetActiveIDEInputDataSet();
    ActiveSynMemo.Modified := False;
    SynMemoStatusChange(FSQLEditorSynMemo, []);
  end;
end;

procedure TFClient.PropertiesServerExecute(Sender: TObject);
begin
  Wanted.Clear();

  DServer.Client := Client;
  DServer.Tab := Self;
  DServer.Execute();
end;

procedure TFClient.PSideBarResize(Sender: TObject);
begin
  SetWindowLong(FNavigator.Handle, GWL_STYLE, GetWindowLong(FNavigator.Handle, GWL_STYLE) or TVS_NOHSCROLL);
  SetWindowLong(FSQLHistory.Handle, GWL_STYLE, GetWindowLong(FSQLHistory.Handle, GWL_STYLE) or TVS_NOHSCROLL);
  if (Assigned(FFolders)) then
    SetWindowLong(FFolders.Handle, GWL_STYLE, GetWindowLong(FFolders.Handle, GWL_STYLE) or TVS_NOHSCROLL);

  PSideBarHeader.Width := PSideBar.Width;
  PToolBar.Left := PContent.Left;
end;

procedure TFClient.PSQLEditorUpdate();
var
  Event: TCEvent;
  Routine: TCRoutine;
  Trigger: TCTrigger;
begin
  if (Self.View = vIDE) then
    case (SelectedImageIndex) of
      iiProcedure,
      iiFunction:
        begin
          Routine := TCRoutine(FNavigator.Selected.Data);
          Desktop(Routine).SynMemo.Text := Routine.Source + #13#10;
        end;
      iiEvent:
        begin
          Event := TCEvent(FNavigator.Selected.Data);
          Desktop(Event).SynMemo.Text := Event.Stmt + #13#10;
        end;
      iiTrigger:
        begin
          Trigger := TCTrigger(FNavigator.Selected.Data);
          Desktop(Trigger).SynMemo.Text := Trigger.Stmt + #13#10;
        end;
    end;

  if (Assigned(ActiveSynMemo) and Assigned(ActiveSynMemo.OnStatusChange)) then ActiveSynMemo.OnStatusChange(ActiveSynMemo, [scModified]);
end;

function TFClient.RenameCItem(const CItem: TCItem; const NewName: string): Boolean;
var
  BaseTable: TCBaseTable;
  Event: TCEvent;
  Host: TCHost;
  NewBaseTable: TCBaseTable;
  NewEvent: TCEvent;
  NewHost: TCHost;
  NewTrigger: TCTrigger;
  NewUser: TCUser;
  Table: TCTable;
  Trigger: TCTrigger;
  User: TCUser;
begin
  if (CItem is TCTable) then
  begin
    Table := TCTable(CItem);

    Table.Database.RenameTable(Table, NewName);

    Result := False;
  end
  else if (CItem is TCTrigger) then
  begin
    Trigger := TCTrigger(CItem);

    NewTrigger := TCTrigger.Create(Trigger.Database.Triggers);
    NewTrigger.Assign(Trigger);
    NewTrigger.Name := NewName;
    Result := Trigger.Database.UpdateTrigger(Trigger, NewTrigger);
    NewTrigger.Free();
  end
  else if (CItem is TCEvent) then
  begin
    Event := TCEvent(CItem);

    NewEvent := TCEvent.Create(Event.Database.Events);
    NewEvent.Assign(Event);
    NewEvent.Name := NewName;
    Result := Event.Database.UpdateEvent(Event, NewEvent);
    NewEvent.Free();
  end
  else if (CItem is TCBaseTableField) then
  begin
    BaseTable := TCBaseTableField(CItem).Table;

    NewBaseTable := TCBaseTable.Create(BaseTable.Database.Tables);
    NewBaseTable.Assign(BaseTable);
    TCBaseTableField(CItem).Name := NewName;
    Result := BaseTable.Database.UpdateTable(BaseTable, NewBaseTable);
    NewBaseTable.Free();
  end
  else if (CItem is TCForeignKey) then
  begin
    BaseTable := TCForeignKey(CItem).Table;

    NewBaseTable := TCBaseTable.Create(BaseTable.Database.Tables);
    NewBaseTable.Assign(BaseTable);
    TCForeignKey(CItem).Name := NewName;
    Result := BaseTable.Database.UpdateTable(BaseTable, NewBaseTable);
    NewBaseTable.Free();
  end
  else if (CItem is TCHost) then
  begin
    Host := TCHost(CItem);

    NewHost := TCHost.Create(Client.Hosts);
    NewHost.Assign(Host);
    if (NewName = '<' + Preferences.LoadStr(327) + '>') then
      NewHost.Name := ''
    else
      NewHost.Name := NewName;
    Result := Client.UpdateHost(Host, NewHost);
    NewHost.Free();
  end
  else if (CItem is TCUser) then
  begin
    User := TCUser(CItem);

    NewUser := TCUser.Create(Client.Users);
    NewUser.Assign(User);
    if (NewName = '<' + Preferences.LoadStr(287) + '>') then
      NewUser.Name := ''
    else
      NewUser.Name := NewName;
    Result := Client.UpdateUser(User, NewUser);
    NewUser.Free();
  end
  else
    Result := False;

  if (Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and Result) then
    ActiveDBGrid.DataSource.DataSet.Close();
end;

procedure TFClient.SaveSQLFile(Sender: TObject);
var
  BytesWritten: DWord;
  FileBuffer: PAnsiChar;
  Handle: THandle;
  Len: Cardinal;
  Success: Boolean;
  Text: string;
  URI: TUURI;
begin
  SaveDialog.Title := ReplaceStr(Preferences.LoadStr(582), '&', '');
  SaveDialog.InitialDir := Path;
  SaveDialog.Encodings.Text := EncodingCaptions();
  SaveDialog.EncodingIndex := SaveDialog.Encodings.IndexOf(CodePageToEncoding(Client.CodePage));
  if ((Sender = MainAction('aFSave')) or (Sender = MainAction('aFSaveAs'))) then
  begin
    if (SQLEditor.Filename = '') then
      SaveDialog.FileName := ReplaceStr(Preferences.LoadStr(6), '&', '') + '.sql'
    else
    begin
      SaveDialog.FileName := ExtractFileName(SQLEditor.Filename);
      SaveDialog.EncodingIndex := SaveDialog.Encodings.IndexOf(CodePageToEncoding(SQLEditor.FileCodePage));
    end;
    Text := ActiveSynMemo.Text;
  end
  else if (Sender = MainAction('aECopyToFile')) then
  begin
    if (Window.ActiveControl = ActiveSynMemo) then
    begin
      SaveDialog.FileName := '';
      Text := ActiveSynMemo.SelText;
      if (Text = '') then Text := ActiveSynMemo.Text;
    end
    else if (Window.ActiveControl = FLog) then
    begin
      SaveDialog.FileName := ReplaceStr(Preferences.LoadStr(11), '&', '') + '.sql';
      Text := FLog.SelText;
      if (Text = '') then Text := FLog.Text;
    end;
  end
  else if (Sender = miHSaveAs) then
  begin
    SaveDialog.FileName := '';
    Text := XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'sql').Text;
  end
  else
    Exit;
  SaveDialog.DefaultExt := 'sql';
  SaveDialog.Filter := FilterDescription('sql') + ' (*.sql)|*.sql' + '|' + FilterDescription('*') + ' (*.*)|*.*';
  if (((Sender = MainAction('aFSave')) and (SQLEditor.Filename <> '')) or (Text <> '') and SaveDialog.Execute()) then
  begin
    Path := ExtractFilePath(SaveDialog.FileName);

    Handle := CreateFile(PChar(SaveDialog.FileName),
                         GENERIC_WRITE,
                         FILE_SHARE_READ,
                         nil,
                         CREATE_ALWAYS, 0, 0);
    if (Handle = INVALID_HANDLE_VALUE) then
      MsgBox(SysErrorMessage(GetLastError()), Preferences.LoadStr(45), MB_OK + MB_ICONERROR)
    else
    begin
      SQLEditor.Filename := SaveDialog.FileName;
      SQLEditor.FileCodePage := EncodingToCodePage(SaveDialog.Encodings[SaveDialog.EncodingIndex]);

      case (SQLEditor.FileCodePage) of
        CP_UNICODE: Success := WriteFile(Handle, BOM_UNICODE^, Length(BOM_UNICODE), BytesWritten, nil);
        CP_UTF8: Success := WriteFile(Handle, BOM_UTF8^, Length(BOM_UTF8), BytesWritten, nil);
        else Success := True;
      end;

      if (Success) then
        case (SQLEditor.FileCodePage) of
          CP_UNICODE: Success := WriteFile(Handle, Text[1], Length(Text), BytesWritten, nil);
          else
            if (Text <> '') then
            begin
              Len := WideCharToMultiByte(SQLEditor.FileCodePage, 0, PChar(Text), Length(Text), nil, 0, nil, nil);
              if (Len > 0) then
              begin
                GetMem(FileBuffer, Len);
                WideCharToMultiByte(SQLEditor.FileCodePage, 0, PChar(Text), Length(Text), FileBuffer, Len, nil, nil);
                Success := WriteFile(Handle, FileBuffer^, Len, BytesWritten, nil);
                FreeMem(FileBuffer);
              end
              else if (GetLastError() <> 0) then
                RaiseLastOSError();
            end;
        end;

      if (not Success) then
        MsgBox(SysErrorMessage(GetLastError()), Preferences.LoadStr(45), MB_OK + MB_ICONERROR)
      else
      begin
        if ((Sender = MainAction('aFSave')) or (Sender = MainAction('aFSaveAs'))) then
          ActiveSynMemo.Modified := False;
        URI := TUURI.Create(Address);
        URI.Param['file'] := EscapeURL(SQLEditor.Filename);
        if (SQLEditor.FileCodePage = 0) then
          URI.Param['cp'] := Null
        else
          URI.Param['cp'] := IntToStr(SQLEditor.FileCodePage);
        FAddress := URI.Address;
        AddressChanged(nil);
        URI.Free();
        Client.Account.Desktop.Files.Add(SQLEditor.Filename, SQLEditor.FileCodePage);
        Window.Perform(CM_UPDATETOOLBAR, 0, LPARAM(Self));
      end;

      CloseHandle(Handle);
    end;
  end;
end;

procedure TFClient.SBResultRefresh(const DataSet: TMySQLDataSet);
var
  I: Integer;
begin
  SBResult.Panels[0].Text := Preferences.LoadStr(703, IntToStr(DataSet.Connection.ExecutedStmts));
  SBResult.Panels[1].Text := ExecutionTimeToStr(DataSet.Connection.ExecutionTime);
  if (DataSet.Connection.RowsAffected < 0) then
    SBResult.Panels[2].Text := Preferences.LoadStr(658, IntToStr(0))
  else
    SBResult.Panels[2].Text := Preferences.LoadStr(658, IntToStr(DataSet.Connection.RowsAffected));
  if (DataSet.Connection.WarningCount < 0) then
    SBResult.Panels[3].Text := Preferences.LoadStr(704) + ': ???'
  else
    SBResult.Panels[3].Text := Preferences.LoadStr(704) + ': ' + IntToStr(DataSet.Connection.WarningCount);
  if (DataSet.Connection.ExecutedStmts <> 1) then
  begin
    SBResult.Panels[4].Text := '';
    SBResult.Panels[5].Text := '';
  end
  else
  begin
    SBResult.Panels[4].Text := Preferences.LoadStr(124) + ': ' + IntToStr(DataSet.RecordCount);
    SBResult.Panels[5].Text := Preferences.LoadStr(253) + ': ' + IntToStr(DataSet.FieldCount);
  end;

  for I := 0 to SBResult.Panels.Count - 2 do
    SBResult.Panels[I].Width := SBResult.Canvas.TextWidth(SBResult.Panels[I].Text) + 15;
end;

procedure TFClient.SearchNotFound(Sender: TObject; FindText: string);
begin
  MsgBox(Preferences.LoadStr(533, FindText), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);
end;

procedure TFClient.SendQuery(Sender: TObject; const SQL: string);
begin
  Wanted.Action := nil;

  if ((Sender is TAction) and Assigned(Client.DatabaseByName(SelectedDatabase)) and not Client.DatabaseByName(SelectedDatabase).Update()) then
    Wanted.Action := TAction(Sender)
  else
    case (View) of
      vIDE:
        case (FNavigator.Selected.ImageIndex) of
          iiProcedure,
          iiFunction:
            begin
              Desktop(TCRoutine(FNavigator.Selected.Data)).CloseIDEResult();
              PContentChange(Sender);
              Client.SendSQL(SQL, Desktop(TCRoutine(FNavigator.Selected.Data)).IDEResultEvent);
            end;
          iiEvent:
            Client.SendSQL(SQL);
        end;
      vBuilder:
        case (FNavigator.Selected.ImageIndex) of
          iiDatabase,
          iiSystemDatabase:
            begin
              Desktop(TCDatabase(FNavigator.Selected.Data)).CloseBuilderResult();
              PContentChange(Sender);
              Client.SendSQL(SQL, Desktop(TCDatabase(FNavigator.Selected.Data)).BuilderResultEvent);
            end;
        end;
      vEditor:
        begin
          SQLEditor.CloseResult();
          PContentChange(Sender);
          Client.SendSQL(SQL, SQLEditor.ResultEvent);
        end;
    end;
end;

procedure TFClient.SetView(const AView: TView);
var
  URI: TUURI;
begin
  if (AView <> View) then
  begin
    URI := TUURI.Create(Address);

    case (AView) of
      vObjects: URI.Param['view'] := Null;
      vBrowser: URI.Param['view'] := 'browser';
      vIDE: URI.Param['view'] := 'ide';
      vBuilder: URI.Param['view'] := 'builder';
      vEditor:
        begin
          URI.Param['view'] := 'editor';
          if (SQLEditor.Filename <> '') then
            URI.Param['file'] := EscapeURL(SQLEditor.Filename);
          if (SQLEditor.FileCodePage = 0) then
            URI.Param['cp'] := Null
          else
            URI.Param['cp'] := IntToStr(SQLEditor.FileCodePage);
        end;
      vDiagram: URI.Param['view'] := 'diagram';
    end;


    if ((AView = vObjects) and (SelectedImageIndex in [iiProcedure, iiFunction, iiTrigger, iiEvent])) then
    begin
      URI.Param['objecttype'] := Null;
      URI.Param['object'] := Null;
    end
    else if ((AView = vBrowser) and not (SelectedImageIndex in [iiBaseTable, iiSystemView, iiView])) then
    begin
      if (SelectedImageIndex = iiTrigger) then
        URI.Table := TCTrigger(FNavigator.Selected.Data).TableName
      else
        URI.Table := LastSelectedTable;
    end
    else if ((AView = vIDE) and not (SelectedImageIndex in [iiView, iiProcedure, iiFunction, iiEvent, iiTrigger])) then
      URI.Address := LastObjectIDEAddress
    else if ((AView = vBuilder) and not (SelectedImageIndex in [iiDatabase, iiSystemDatabase])
      or (AView = vEditor) and not (SelectedImageIndex in [iiServer, iiDatabase, iiSystemDatabase])) then
    begin
      if (URI.Database = '') then
        URI.Database := Client.DatabaseName;
      URI.Table := '';
      URI.Param['objecttype'] := Null;
      URI.Param['object'] := Null;
      URI.Param['system'] := Null;
      URI.Param['filter'] := Null;
      URI.Param['offset'] := Null;
      URI.Param['file'] := Null;
      URI.Param['cp'] := Null;
    end
    else if ((AView = vDiagram) and not (SelectedImageIndex in [iiDatabase, iiSystemDatabase])) then
    begin
      if (URI.Database = '') then
        URI.Database := LastSelectedDatabase;
      URI.Table := '';
      URI.Param['system'] := Null;
      URI.Param['filter'] := Null;
      URI.Param['offset'] := Null;
      URI.Param['file'] := Null;
      URI.Param['cp'] := Null;
    end;

    Address := URI.Address;
    URI.Free();
  end;
end;

procedure TFClient.SetAddress(const AAddress: string);
var
  AllowChange: Boolean;
  ChangeEvent: TTVChangedEvent;
  ChangingEvent: TTVChangingEvent;
  CodePage: Integer;
  FileName: string;
  NewView: TView;
  NewAddress: string;
  Node: TTreeNode;
  Position: Integer;
  Table: TCTable;
  URI: TUURI;
begin
  AllowChange := True;
  NewAddress := AAddress; // We need this, since in AddressChanging maybe Wanted.Address will be changed, but AAddress is Wanted.Address
  AddressChanging(nil, NewAddress, AllowChange);
  if (not AllowChange and Wanted.Nothing) then
  begin
    NewAddress := Client.Account.ExpandAddress('/');
    AllowChange := True;
    AddressChanging(nil, NewAddress, AllowChange);
  end;
  if (AllowChange) then
  begin
    FAddress := NewAddress;

    Node := FNavigatorNodeByAddress(Address);

    ChangingEvent := FNavigator.OnChanging; FNavigator.OnChanging := nil;
    ChangeEvent := FNavigator.OnChange; FNavigator.OnChange := nil;
    FNavigator.Selected := Node;
    FNavigator.OnChanging := ChangingEvent;
    FNavigator.OnChange := ChangeEvent;

    URI := TUURI.Create(Address);

    if ((URI.Param['view'] = 'browser') and (Node.ImageIndex in [iiBaseTable, iiSystemView, iiView])) then
      NewView := vBrowser
    else if ((URI.Param['view'] = 'ide') and (Node.ImageIndex in [iiView, iiProcedure, iiFunction, iiTrigger, iiEvent])) then
      NewView := vIDE
    else if ((URI.Param['view'] = 'builder') and (Node.ImageIndex in [iiDatabase, iiSystemDatabase])) then
      NewView := vBuilder
    else if (URI.Param['view'] = 'editor') then
      NewView := vEditor
    else if ((URI.Param['view'] = 'diagram') and (Node.ImageIndex in [iiDatabase, iiSystemDatabase])) then
      NewView := vDiagram
    else
      NewView := vObjects;

    MainAction('aVObjectBrowser').Checked := NewView = vObjects;
    MainAction('aVDataBrowser').Checked := NewView = vBrowser;
    MainAction('aVObjectIDE').Checked := NewView = vIDE;
    MainAction('aVQueryBuilder').Checked := NewView = vBuilder;
    MainAction('aVSQLEditor').Checked := NewView = vEditor;
    MainAction('aVDiagram').Checked := NewView = vDiagram;

    case (NewView) of
      vBrowser:
        begin
          Table := Client.DatabaseByName(URI.Database).TableByName(URI.Table);

          FUDOffset.Position := 0;
          FUDLimit.Position := Desktop(Table).Limit;
          FLimitEnabled.Down := Desktop(Table).Limited;

          if (URI.Param['offset'] <> Null) then
          begin
            if (TryStrToInt(URI.Param['offset'], Position)) then FUDOffset.Position := Position else FUDOffset.Position := 0;
            FLimitEnabled.Down := URI.Param['offset'] <> '';
          end;
          if (URI.Param['filter'] <> Null) then
          begin
            FFilter.Text := URI.Param['filter'];
            FFilterEnabled.Down := URI.Param['filter'] <> '';
            FFilterEnabled.Enabled := FFilterEnabled.Down;
          end;
        end;
      vEditor:
        if (URI.Param['file'] <> Null) then
        begin
          FileName := UnescapeURL(URI.Param['file']);
          if (ExtractFilePath(FileName) = '') then
            FileName := ExpandFilename(FileName);
          if ((FileName <> SQLEditor.Filename) and FileExists(FileName)) then
            if ((URI.Param['cp'] = Null) or not TryStrToInt(URI.Param['cp'], CodePage)) then
              OpenSQLFile(FileName)
            else
              OpenSQLFile(FileName, CodePage);
        end;
    end;

    URI.Free();

    AddressChanged(nil);
  end;
end;

procedure TFClient.SetPath(const APath: TFileName);
begin
  if (APath <> Preferences.Path) then
  begin
    Preferences.Path := APath;
    if (Assigned(FFolders) and (APath <> FFolders.SelectedFolder)) then
      FFolders.SelectedFolder := APath;
  end;
end;

procedure TFClient.SLogCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
begin
  Accept := (PLog.Constraints.MinHeight <= NewSize) and (NewSize <= ClientHeight - PToolBar.Height - SLog.Height - PContent.Constraints.MinHeight);
end;

procedure TFClient.SLogMoved(Sender: TObject);
begin
  FormResize(Sender);
end;

procedure TFClient.smEEmptyClick(Sender: TObject);
begin
  Wanted.Clear();

  Client.SQLMonitor.Clear();
  FLog.Lines.Clear();
  PLogResize(nil);
end;

procedure TFClient.SplitterCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
var
  I: Integer;
  MaxHeight: Integer;
  MinHeight: Integer;
  Splitter: TSplitter_Ext;
begin
  if (Sender is TSplitter_Ext) then
  begin
    Splitter := TSplitter_Ext(Sender);

    MaxHeight := Splitter.Parent.ClientHeight;
    MinHeight := 0;
    for I := 0 to Splitter.Parent.ControlCount - 1 do
      if ((Splitter.Parent.Controls[I].Top < Splitter.Top) and (Splitter.Parent.Controls[I].Constraints.MinHeight > 0)) then
        Inc(MinHeight, Splitter.Parent.Controls[I].Constraints.MinHeight)
      else if ((Splitter.Parent.Controls[I].Top > Splitter.Top) and (Splitter.Parent.Controls[I].Constraints.MinHeight > 0)) then
        Dec(MaxHeight, Splitter.Parent.Controls[I].Constraints.MinHeight)
      else if (Splitter.Parent.Controls[I] = Splitter) then
        Dec(MaxHeight, Splitter.Height + 3);

    Accept := (MinHeight <= NewSize) and (NewSize <= MaxHeight);
  end;
end;

procedure TFClient.SQLError(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
var
  Flags: Integer;
  Msg: string;
begin
  if (E is EDatabasePostError) then
    Msg := Preferences.LoadStr(675)
  else if (E is EMySQLError) then
    case (EMySQLError(E).ErrorCode) of
      CR_CONN_HOST_ERROR: if (EMySQLError(E).Connection.Host <> '') then Msg := Preferences.LoadStr(495, EMySQLError(E).Connection.Host) else Msg := Preferences.LoadStr(495);
      CR_UNKNOWN_HOST: if (EMySQLError(E).Connection.Host <> '') then Msg := Preferences.LoadStr(706, EMySQLError(E).Connection.Host) else Msg := Preferences.LoadStr(706);
      else Msg := Preferences.LoadStr(165, IntToStr(EMySQLError(E).ErrorCode), E.Message);
    end
  else
    Msg := E.Message;

  Flags := MB_CANCELTRYCONTINUE + MB_ICONERROR;
  case (MsgBox(Msg, Preferences.LoadStr(45), Flags, Handle)) of
    IDCANCEL,
    IDABORT: begin Action := daAbort; PostMessage(Handle, CM_ACTIVATE_DBGRID, 0, LPARAM(ActiveDBGrid)); end;
    IDRETRY,
    IDTRYAGAIN: Action := daRetry;
    IDCONTINUE,
    IDIGNORE: begin Action := daAbort; DataSet.Cancel(); end;
  end;
end;

procedure TFClient.SResultMoved(Sender: TObject);
begin
  if (SBResult.Visible and (SBResult.Align = alBottom)) then
    SBResult.Top := PContent.Height - SBResult.Height;
end;

procedure TFClient.SSideBarCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
begin
  Accept := NewSize <= ClientWidth - SSideBar.Width - PContent.Constraints.MinWidth;
end;

procedure TFClient.SSideBarMoved(Sender: TObject);
begin
  FormResize(Sender);
end;

procedure TFClient.StatusBarRefresh(const Immediately: Boolean = False);
var
  Count: Integer;
  QueryBuilderWorkArea: TacQueryBuilderWorkArea;
  SelCount: Integer;
  Table: TCBaseTable;
  Text: string;
begin
  if (Assigned(StatusBar) and (Immediately or (tsActive in FrameState)) and not (csDestroying in ComponentState) and Assigned(Window)) then
  begin
    KillTimer(Handle, tiStatusBar);

    Count := -1;
    case (View) of
      vObjects: if (Assigned(ActiveListView)) then Count := ActiveListView.Items.Count;
      vBrowser: if (Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and ActiveDBGrid.DataSource.DataSet.Active) then Count := ActiveDBGrid.DataSource.DataSet.RecordCount;
      vIDE,
      vEditor:
        if (Assigned(Window.ActiveControl) and (Window.ActiveControl = ActiveSynMemo)) then
          Count := ActiveSynMemo.Lines.Count
        else if ((Window.ActiveControl = ActiveDBGrid) and Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet)) then
          Count := ActiveDBGrid.DataSource.DataSet.RecordCount;
      vBuilder:
        begin
          if (Assigned(FBuilderEditorPageControl())) then
          begin
            QueryBuilderWorkArea := FBuilderActiveWorkArea();
            if (Assigned(Window.ActiveControl) and Assigned(QueryBuilderWorkArea) and IsChild(QueryBuilderWorkArea.Handle, Window.ActiveControl.Handle)) then
              Count := FBuilderActiveWorkArea().ControlCount
            else if (Window.ActiveControl = FBuilderSynMemo) then
              Count := FBuilderSynMemo.Lines.Count;
          end;
        end;
    end;

    SelCount := -1;
    if ((Window.ActiveControl = ActiveListView) and Assigned(ActiveListView) and (ActiveListView.SelCount > 1)) then
      SelCount := ActiveListView.SelCount
    else if (Assigned(ActiveSynMemo) and (Window.ActiveControl = ActiveSynMemo)) then
      SelCount := 0
    else if ((Window.ActiveControl = ActiveDBGrid) and Assigned(ActiveDBGrid) and (ActiveDBGrid.SelectedRows.Count > 0)) then
      SelCount := ActiveDBGrid.SelectedRows.Count;

    if ((View = vBrowser) and (SelectedImageIndex in [iiBaseTable, iiSystemView])) then
      Table := TCBaseTable(FNavigator.Selected.Data)
    else
      Table := nil;

    if (SelCount > 0) then
      Text := Preferences.LoadStr(688, IntToStr(SelCount))
    else if ((View = vBrowser) and (SelectedImageIndex in [iiBaseTable, iiSystemView]) and not Client.InUse() and Assigned(Table) and Table.DataSet.LimitedDataReceived and (Table.Rows >= 0)) then
      if (Assigned(TCBaseTable(FNavigator.Selected.Data).Engine) and (UpperCase(TCBaseTable(FNavigator.Selected.Data).Engine.Name) <> 'INNODB')) then
        Text := Preferences.LoadStr(691, IntToStr(Count), IntToStr(TCBaseTable(FNavigator.Selected.Data).Rows))
      else
        Text := Preferences.LoadStr(691, IntToStr(Count), '~' + IntToStr(TCBaseTable(FNavigator.Selected.Data).Rows))
    else if (Assigned(ActiveSynMemo) and (Window.ActiveControl = ActiveSynMemo) and (Count >= 0)) then
      Text := IntToStr(Count) + ' ' + ReplaceStr(Preferences.LoadStr(600), '&', '')
    else if ((View = vBuilder) and (Count >= 0)) then
      if (Window.ActiveControl = FBuilderSynMemo) then
        Text := IntToStr(Count) + ' ' + ReplaceStr(Preferences.LoadStr(600), '&', '')
      else
        Text := Preferences.LoadStr(687, IntToStr(Count))
    else if (Count >= 0) then
      Text := Preferences.LoadStr(687, IntToStr(Count))
    else
      Text := '';
    StatusBar.Panels[sbSummarize].Text := Text;


    if (not Client.Connected) then
      StatusBar.Panels[sbConnected].Text := ''
    else
      StatusBar.Panels[sbConnected].Text := Preferences.LoadStr(519) + ': ' + FormatDateTime(FormatSettings.ShortTimeFormat, Client.LatestConnect);

    if (Assigned(Window.ActiveControl)) then
      if (Window.ActiveControl = ActiveSynMemo) then
        Text := IntToStr(ActiveSynMemo.CaretXY.Line) + ':' + IntToStr(ActiveSynMemo.CaretXY.Char)
      else if ((Window.ActiveControl = ActiveListView) and Assigned(ActiveListView.ItemFocused) and Assigned(ActiveListView.Selected) and (ActiveListView.ItemFocused.ImageIndex = iiKey)) then
        Text := Preferences.LoadStr(377) + ': ' + IntToStr(TCKey(ActiveListView.ItemFocused.Data).Index + 1)
      else if ((Window.ActiveControl = ActiveListView) and (SelectedImageIndex in [iiBaseTable, iiSystemView, iiView]) and Assigned(ActiveListView.ItemFocused) and Assigned(ActiveListView.Selected) and (ActiveListView.ItemFocused.ImageIndex = iiField)) then
        Text := ReplaceStr(Preferences.LoadStr(164), '&', '') + ': ' + IntToStr(TCTableField(ActiveListView.ItemFocused.Data).Index)
      else if ((Window.ActiveControl = ActiveDBGrid) and Assigned(ActiveDBGrid.SelectedField) and (ActiveDBGrid.DataSource.DataSet.RecNo >= 0)) then
        Text := IntToStr(ActiveDBGrid.DataSource.DataSet.RecNo + 1) + ':' + IntToStr(ActiveDBGrid.SelectedField.FieldNo)
      else
        Text := '';
      StatusBar.Panels[sbItem].Text := Text;
  end;
end;

procedure TFClient.SynMemoApllyPreferences(const SynMemo: TSynMemo);
begin
  if (SynMemo <> FSQLEditorSynMemo) then
  begin
    SynMemo.Font.Name := FSQLEditorSynMemo.Font.Name;
    SynMemo.Font.Style := FSQLEditorSynMemo.Font.Style;
    SynMemo.Font.Color := FSQLEditorSynMemo.Font.Color;
    SynMemo.Font.Size := FSQLEditorSynMemo.Font.Size;
    SynMemo.Font.Charset := FSQLEditorSynMemo.Font.Charset;
    SynMemo.Gutter.Font.Name := FSQLEditorSynMemo.Gutter.Font.Name;
    SynMemo.Gutter.Font.Style := FSQLEditorSynMemo.Gutter.Font.Style;
    SynMemo.Gutter.Font.Color := FSQLEditorSynMemo.Gutter.Font.Color;
    SynMemo.Gutter.Font.Size := FSQLEditorSynMemo.Gutter.Font.Size;
    SynMemo.Gutter.Font.Charset := FSQLEditorSynMemo.Gutter.Font.Charset;
    SynMemo.Gutter.Visible := FSQLEditorSynMemo.Gutter.Visible;
    SynMemo.Options := FSQLEditorSynMemo.Options;
    SynMemo.TabWidth := FSQLEditorSynMemo.TabWidth;
    SynMemo.RightEdge := FSQLEditorSynMemo.RightEdge;
    SynMemo.WantTabs := FSQLEditorSynMemo.WantTabs;
    SynMemo.ActiveLineColor := FSQLEditorSynMemo.ActiveLineColor;
    SynMemo.Highlighter := FSQLEditorSynMemo.Highlighter;
  end;
end;

procedure TFClient.SynMemoDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DatabaseName: string;
  S: string;
  SelStart: Integer;
begin
  if ((Source = FNavigator) and (Sender is TSynMemo)) then
  begin
    case (MouseDownNode.ImageIndex) of
      iiKey: S := TCKey(MouseDownNode.Data).Name;
      iiForeignKey: S := TCForeignKey(MouseDownNode.Data).Name;
      else S := MouseDownNode.Text;
    end;
    SelStart := TSynMemo(Sender).SelStart;
    TSynMemo(Sender).SelText := Client.EscapeIdentifier(S);
    TSynMemo(Sender).SelStart := SelStart;
    TSynMemo(Sender).SelLength := Length(Client.EscapeIdentifier(S));
    TSynMemo(Sender).AlwaysShowCaret := False;

    Window.ActiveControl := TSynMemo(Sender);
  end
  else if ((Source = FSQLHistory) and (Sender is TSynMemo)) then
  begin
    S := XMLNode(IXMLNode(MouseDownNode.Data), 'sql').Text;

    DatabaseName := XMLNode(IXMLNode(MouseDownNode.Data), 'database').Text;
    if (DatabaseName <> SelectedDatabase) then
      S := Client.SQLUse(DatabaseName) + S;
    S := ReplaceStr(ReplaceStr(S, #13#10, #10), #10, #13#10);

    SelStart := TSynMemo(Sender).SelStart;
    TSynMemo(Sender).SelText := S;
    TSynMemo(Sender).SelStart := SelStart;
    TSynMemo(Sender).SelLength := Length(S);
    TSynMemo(Sender).AlwaysShowCaret := False;

    Window.ActiveControl := TSynMemo(Sender);
  end
  else if ((Source = ActiveDBGrid) and (Sender = FSQLEditorSynMemo)) then
  begin
    S := ActiveDBGrid.SelectedField.AsString;

    SelStart := TSynMemo(Sender).SelStart;
    TSynMemo(Sender).SelText := S;
    TSynMemo(Sender).SelStart := SelStart;
    TSynMemo(Sender).SelLength := Length(S);
    TSynMemo(Sender).AlwaysShowCaret := False;

    Window.ActiveControl := TSynMemo(Sender);
  end;
end;

procedure TFClient.SynMemoDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if (Source = FNavigator) then
    Accept := MouseDownNode.ImageIndex in [iiDatabase, iiSystemDatabase, iiBaseTable, iiSystemView, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger, iiKey, iiField, iiSystemViewField, iiViewField, iiForeignKey]
  else if (Source = FSQLHistory) then
    Accept := MouseDownNode.ImageIndex in [iiStatement, iiQuery, iiClock]
  else if (Source = ActiveDBGrid) then
    Accept := Assigned(ActiveDBGrid.SelectedField)
      and not ActiveDBGrid.SelectedField.IsNull
      and not (ActiveDBGrid.SelectedField.DataType in [ftWideMemo, ftBlob])
  else
    Accept := False;

  if (Accept) then
  begin
    if ((Sender = ActiveSynMemo) and not ActiveSynMemo.AlwaysShowCaret) then
    begin
      SynMemoBeforeDrag.SelStart := ActiveSynMemo.SelStart;
      SynMemoBeforeDrag.SelLength := ActiveSynMemo.SelLength;
      ActiveSynMemo.AlwaysShowCaret := True;
    end;

    if (not FSQLEditorSynMemo.Gutter.Visible) then
      ActiveSynMemo.CaretX := (X) div ActiveSynMemo.CharWidth + 1
    else
      ActiveSynMemo.CaretX := (X - ActiveSynMemo.Gutter.RealGutterWidth(ActiveSynMemo.CharWidth)) div ActiveSynMemo.CharWidth + 1;
    ActiveSynMemo.CaretY := (Y div ActiveSynMemo.LineHeight) + 1;
  end;
end;

procedure TFClient.SynMemoEnter(Sender: TObject);
begin
  MainAction('aECopyToFile').OnExecute := SaveSQLFile;
  MainAction('aEPasteFromFile').OnExecute := aEPasteFromExecute;
  MainAction('aSGoto').OnExecute := TSymMemoGotoExecute;

  MainAction('aHIndex').ShortCut := 0;
  MainAction('aHSQL').ShortCut := ShortCut(VK_F1, []);

  SynMemoStatusChange(Sender, [scAll]);
  StatusBarRefresh();
end;

procedure TFClient.SynMemoExit(Sender: TObject);
begin
  MainAction('aFImportSQL').Enabled := False;
  MainAction('aFExportSQL').Enabled := False;
  MainAction('aFPrint').Enabled := False;
  MainAction('aERedo').Enabled := False;
  MainAction('aECopyToFile').Enabled := False;
  MainAction('aEPasteFromFile').Enabled := False;
  MainAction('aSGoto').Enabled := False;

  MainAction('aHIndex').ShortCut := ShortCut(VK_F1, []);
  MainAction('aHSQL').ShortCut := 0;
end;

procedure TFClient.SynMemoPrintExecute(Sender: TObject);
var
  I: Integer;
  SynEdit: TSynEdit;
begin
  Wanted.Clear();

  if (Window.ActiveControl is TSynMemo) then
  begin
    SynEdit := TSynMemo(Window.ActiveControl);

    FSQLEditorPrint.SynEdit := SynEdit;
    FSQLEditorPrint.Highlight := Assigned(SynEdit.Highlighter);
    FSQLEditorPrint.LineNumbers := SynEdit.Gutter.Visible;
    FSQLEditorPrint.TabWidth := SynEdit.TabWidth;

    PrintDialog.FromPage := 1;
    PrintDialog.ToPage := FSQLEditorPrint.PageCount;
    PrintDialog.MinPage := 1;
    PrintDialog.MaxPage := FSQLEditorPrint.PageCount;
    if (FSQLEditorPrint.PageCount = 1) then
      PrintDialog.Options := PrintDialog.Options - [poPageNums]
    else
      PrintDialog.Options := PrintDialog.Options + [poPageNums];
    if (SynEdit.SelText = '') then
    begin
      PrintDialog.Options := PrintDialog.Options - [poSelection];
      PrintDialog.PrintRange := prAllPages;
    end
    else
    begin
      PrintDialog.Options := PrintDialog.Options + [poSelection];
      PrintDialog.PrintRange := prSelection;
    end;
    if (PrintDialog.Execute()) then
    begin
      FSQLEditorPrint.Copies := PrintDialog.Copies;
      if (SQLEditor.Filename <> '') then
        FSQLEditorPrint.Title := ExtractFileName(SQLEditor.Filename)
      else
        FSQLEditorPrint.Title := ReplaceStr(Preferences.LoadStr(6), '&', '');
      FSQLEditorPrint.Header.Clear();
      FSQLEditorPrint.Header.Add('$TITLE$', nil, taLeftJustify, 1);
      FSQLEditorPrint.Header.Add('$PAGENUM$ / $PAGECOUNT$', nil, taRightJustify, 1);
      FSQLEditorPrint.Footer.Clear();
      FSQLEditorPrint.Footer.Add(Address, nil, taLeftJustify, 1);
      FSQLEditorPrint.Footer.Add(SysUtils.DateTimeToStr(Now(), LocaleFormatSettings), nil, taRightJustify, 1);
      case (PrintDialog.PrintRange) of
        prAllPages,
        prSelection: begin FSQLEditorPrint.SelectedOnly := PrintDialog.PrintRange = prSelection; FSQLEditorPrint.Print(); end;
        prPageNums:
          for I := 0 to PrintDialog.PageRangesCount - 1 do
            FSQLEditorPrint.PrintRange(PrintDialog.PageRanges[I].FromPage, PrintDialog.PageRanges[I].ToPage);
      end;
    end;
  end;
end;

procedure TFClient.SynMemoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  Attri: TSynHighlighterAttributes;
  DDLStmt: TSQLDDLStmt;
  Empty: Boolean;
  Parse: TSQLParse;
  SelSQL: string;
  SQL: string;
  Token: string;
begin
  if (not (csDestroying in ComponentState)) then
  begin
    if (((scCaretX in Changes) or (scModified in Changes) or (scAll in Changes)) and Assigned(ActiveSynMemo)) then
    begin
      SelSQL := ActiveSynMemo.SelText; // Cache, da Abfrage bei vielen Zeilen Zeit benötigt
      if (View = vIDE) then SQL := ActiveSynMemo.Text;

      Empty := ((ActiveSynMemo.Lines.Count <= 1) and (ActiveSynMemo.Text = '')); // Benötigt bei vielen Zeilen Zeit

      MainAction('aFSave').Enabled := not Empty and ((SQLEditor.Filename = '') or ActiveSynMemo.Modified);
      MainAction('aFSaveAs').Enabled := not Empty;
      MainAction('aFPrint').Enabled := (ActiveSynMemo = FSQLEditorSynMemo) and not Empty;
      MainAction('aERedo').Enabled := ActiveSynMemo.CanRedo;
      MainAction('aECopyToFile').Enabled := (SelSQL <> '');
      MainAction('aEPasteFromFile').Enabled := (ActiveSynMemo = FSQLEditorSynMemo);
      MainAction('aSGoto').Enabled := (Sender = FSQLEditorSynMemo) and not Empty;
      MainAction('aDRun').Enabled :=
        ((View = vEditor)
        or ((View  = vBuilder) and FBuilder.Visible)
        or ((View = vIDE) and SQLSingleStmt(SQL) and (SelectedImageIndex in [iiView, iiProcedure, iiFunction, iiEvent]))) and not Empty;
      MainAction('aDRunSelection').Enabled := (((ActiveSynMemo = FSQLEditorSynMemo) and not Empty) or Assigned(ActiveSynMemo) and (ActiveSynMemo.SelText <> ''));
      MainAction('aDPostObject').Enabled := (View = vIDE) and ActiveSynMemo.Modified and SQLSingleStmt(SQL)
        and ((SelectedImageIndex in [iiView]) and SQLCreateParse(Parse, PChar(SQL), Length(SQL),Client.ServerVersion) and (SQLParseKeyword(Parse, 'SELECT'))
          or (SelectedImageIndex in [iiProcedure, iiFunction]) and SQLParseDDLStmt(DDLStmt, PChar(SQL), Length(SQL), Client.ServerVersion) and (DDLStmt.DefinitionType = dtCreate) and (DDLStmt.ObjectType in [otProcedure, otFunction])
          or (SelectedImageIndex in [iiEvent, iiTrigger]));
    end;

    FSQLEditorCompletion.TimerInterval := 0;
    if ((ActiveSynMemo = FSQLEditorSynMemo) and (FSQLEditorSynMemo.SelStart > 0)
      and FSQLEditorSynMemo.GetHighlighterAttriAtRowCol(FSQLEditorSynMemo.WordStart(), Token, Attri) and (Attri <> MainHighlighter.StringAttri) and (Attri <> MainHighlighter.CommentAttri) and (Attri <> MainHighlighter.VariableAttri)) then
    begin
      FSQLEditorCompletion.Editor := ActiveSynMemo;
      FSQLEditorCompletion.TimerInterval := Preferences.Editor.CodeCompletionTime;
    end;

    StatusBarRefresh();
  end;
end;

procedure TFClient.TableOpen(Sender: TObject);
var
  FilterSQL: string;
  Limit: Integer;
  Offset: Integer;
  QuickSearch: string;
  SortDef: TIndexDef;
  Table: TCTable;
begin
  Table := TCTable(FNavigator.Selected.Data);

  if (Assigned(Table) and Assigned(Table.Fields) and (Table.Fields.Count > 0)) then  // Terminate in Table.Fields.GetCount erkennen und Struktur vor SELECT ermitteln, damit bei UPDATE Charset bekannt ist
  begin
    if (not FLimitEnabled.Down) then
    begin
      Offset := 0;
      Limit := 0;
    end
    else
    begin
      Offset := FUDOffset.Position;
      Limit := FUDLimit.Position;
    end;

    if (not FFilterEnabled.Down) then
      FilterSQL := ''
    else
      FilterSQL := FFilter.Text;

    if (not FQuickSearchEnabled.Down) then
      QuickSearch := ''
    else
      QuickSearch := FQuickSearch.Text;

    SortDef := TIndexDef.Create(nil, '', '', []);
    if (Table.DataSet.Active) then
      SortDef.Assign(Table.DataSet.SortDef)
    else if ((Table is TCBaseTable) and (TCBaseTable(Table).Keys.Count > 0) and (TCBaseTable(Table).Keys[0].Name = '')) then
      TCBaseTable(Table).Keys[0].GetSortDef(SortDef);

    if (not Table.DataSet.Active) then
    begin
      PResult.Visible := False; SResult.Visible := PResult.Visible;
      Table.Open(FilterSQL, QuickSearch, SortDef, Offset, Limit, Desktop(Table).DataSetOpenEvent);
    end
    else
    begin
      Table.DataSet.FilterSQL := FilterSQL;
      Table.DataSet.QuickSearch := QuickSearch;
      Table.DataSet.SortDef.Assign(SortDef);
      Table.DataSet.Offset := Offset;
      Table.DataSet.Limit := Limit;
      Table.DataSet.Refresh();
    end;

    SortDef.Free();
  end;
end;

procedure TFClient.ToolBarBlobResize(Sender: TObject);
var
  I: Integer;
  Widths: Integer;
begin
  Widths := 0;
  for I := 0 to ToolBarBlob.ControlCount - 1 do
    if (ToolBarBlob.Controls[I].Visible and (ToolBarBlob.Controls[I] <> tbBlobSpacer)) then
      Inc(Widths, ToolBarBlob.Controls[I].Width);
  Inc(Widths, PBlobSpacer.Height + GetSystemMetrics(SM_CXVSCROLL));
  tbBlobSpacer.Width := ToolBarBlob.Width - Widths;
end;

procedure TFClient.ToolBarResize(Sender: TObject);
begin
  if ((Sender is TToolBar) and (TToolBar(Sender).Parent is TPanel)) then
    TPanel(TToolBar(Sender).Parent).ClientHeight := TToolBar(Sender).Height + 2 * TToolBar(Sender).Top;
end;

procedure TFClient.ToolBarTabsClick(Sender: TObject);
begin
  Wanted.Clear();

  if (Sender = mtObjects) then
    if (mtObjects.Checked) then
      Exclude(Preferences.ToolbarTabs, ttObjects)
    else
      Include(Preferences.ToolbarTabs, ttObjects);
  if (Sender = mtBrowser) then
    if (mtBrowser.Checked) then
      Exclude(Preferences.ToolbarTabs, ttBrowser)
    else
      Include(Preferences.ToolbarTabs, ttBrowser);
  if (Sender = mtIDE) then
    if (mtIDE.Checked) then
      Exclude(Preferences.ToolbarTabs, ttIDE)
    else
      Include(Preferences.ToolbarTabs, ttIDE);
  if (Sender = mtBuilder) then
    if (mtBuilder.Checked) then
      Exclude(Preferences.ToolbarTabs, ttBuilder)
    else
      Include(Preferences.ToolbarTabs, ttBuilder);
  if (Sender = mtEditor) then
    if (mtEditor.Checked) then
      Exclude(Preferences.ToolbarTabs, ttEditor)
    else
      Include(Preferences.ToolbarTabs, ttEditor);
  if (Sender = mtDiagram) then
    if (mtDiagram.Checked) then
      Exclude(Preferences.ToolbarTabs, ttDiagram)
    else
      Include(Preferences.ToolbarTabs, ttDiagram);

  PostMessage(Window.Handle, CM_CHANGEPREFERENCES, 0, 0);
end;

procedure TFClient.ToolButtonStyleClick(Sender: TObject);
begin
  Wanted.Clear();

  TToolButton(Sender).CheckMenuDropdown();
end;

procedure TFClient.TreeViewCollapsed(Sender: TObject; Node: TTreeNode);
begin
  aPExpand.Enabled := not (Node.ImageIndex in [iiServer]) and Node.HasChildren;
  aPCollapse.Enabled := False;

  if ((Sender is TTreeView_Ext) and Assigned(TTreeView_Ext(Sender).PopupMenu)) then
    ShowEnabledItems(TTreeView_Ext(Sender).PopupMenu.Items);
end;

procedure TFClient.TreeViewCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  if ((Sender is TTreeView_Ext) and Assigned(TTreeView_Ext(Sender).OnChange)) then
  begin
    if ((View = vBrowser) and not (Node.ImageIndex in [iiBaseTable, iiSystemView, iiView]) and (Node = TTreeView_Ext(Sender).Selected.Parent)) then
      TTreeView_Ext(Sender).Selected := Node;

    AllowCollapse := Node <> TTreeView_Ext(Sender).Items.getFirstNode();
  end;
end;

procedure TFClient.TreeViewEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if (Assigned(ActiveSynMemo) and (ActiveSynMemo.AlwaysShowCaret)) then
  begin
    ActiveSynMemo.SelStart := SynMemoBeforeDrag.SelStart;
    ActiveSynMemo.SelLength := SynMemoBeforeDrag.SelLength;
    ActiveSynMemo.AlwaysShowCaret := False;
  end;
end;

procedure TFClient.TreeViewExpanded(Sender: TObject; Node: TTreeNode);
begin
  aPExpand.Enabled := False;
  aPCollapse.Enabled := Node.ImageIndex <> iiServer;

  if ((Sender is TTreeView_Ext) and Assigned(TTreeView_Ext(Sender).PopupMenu)) then
    ShowEnabledItems(TTreeView_Ext(Sender).PopupMenu.Items);
end;

procedure TFClient.TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TFClient.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LeftMousePressed := (Sender is TTreeView_Ext) and (Button = mbLeft);
  if (LeftMousePressed) then
    MouseDownNode := TTreeView_Ext(Sender).GetNodeAt(X, Y);
  Exclude(FrameState, tsLoading);
end;

procedure TFClient.TreeViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LeftMousePressed := False;
end;

procedure TFClient.TSymMemoGotoExecute(Sender: TObject);
var
  Line: Integer;
begin
  Wanted.Clear();

  if (Window.ActiveControl is TSynMemo) then
  begin
    DGoto.Captions := Preferences.LoadStr(678);
    if (DGoto.Execute()) then
      if (not TryStrToInt(DGoto.Values[0], Line) or (Line < 1) or (TSynMemo(Window.ActiveControl).Lines.Count < Line)) then
        MessageBeep(MB_ICONERROR)
      else
        TSynMemo(Window.ActiveControl).GotoLineAndCenter(Line);
  end;
end;

function TFClient.UpdateAfterAddressChanged(): Boolean;
var
  Database: TCDatabase;
  I: Integer;
  List: TList;
begin
  Result := False;

  case (View) of
    vObjects,
    vEditor:
      case (SelectedImageIndex) of
        iiServer:
          begin
            List := TList.Create();

            for I := 0 to Client.Databases.Count - 1 do
              List.Add(Client.Databases[I]);

            Result := not Client.Update(List, View = vObjects);

            List.Free();
          end;
        iiDatabase,
        iiSystemDatabase:
          begin
            Database := TCDatabase(FNavigator.Selected.Data);

            if (Database.Count < 50) then
            begin
              List := TList.Create();

              for I := 0 to Database.Tables.Count - 1 do
                List.Add(Database.Tables[I]);
              if (Assigned(Database.Routines)) then
                for I := 0 to Database.Routines.Count - 1 do
                  List.Add(Database.Routines[I]);
              if (Assigned(Database.Events)) then
                for I := 0 to Database.Routines.Count - 1 do
                  List.Add(Database.Routines[I]);
              if (Assigned(Database.Triggers)) then
                for I := 0 to Database.Triggers.Count - 1 do
                  List.Add(Database.Triggers[I]);

              Result := not Client.Update(List);

              List.Free();
            end;
          end;
      end;
    vBrowser:
      if (not TCTable(FNavigator.Selected.Data).ValidDataSet or not TCTable(FNavigator.Selected.Data).DataSet.Active) then
        TableOpen(nil);
    vDiagram:
      if (not Assigned(ActiveWorkbench)) then
      begin
        Desktop(TCDatabase(FNavigator.Selected.Data)).CreateWorkbench();
        ActiveWorkbench := GetActiveWorkbench();
        if (FileExists(Client.Account.DataPath + ActiveWorkbench.Database.Name + PathDelim + 'Diagram.xml')) then
          ActiveWorkbench.LoadFromFile(Client.Account.DataPath + ActiveWorkbench.Database.Name + PathDelim + 'Diagram.xml');
      end;
  end;
end;

procedure TFClient.WMNotify(var Message: TWMNotify);
begin
  case (Message.NMHdr^.code) of
    TVN_BEGINLABELEDIT,
    LVN_BEGINLABELEDIT: BeginEditLabel(Window.ActiveControl);
    TVN_ENDLABELEDIT,
    LVN_ENDLABELEDIT: EndEditLabel(Window.ActiveControl);
    LVN_ITEMCHANGING: NMListView := PNMListView(Message.NMHdr);
  end;

  inherited;

  NMListView := nil;
end;

procedure TFClient.WMParentNotify(var Message: TWMParentNotify);
var
  ClientPoint: TPoint;
  GridPoint: TPoint;
  GridCoord: TGridCoord;
  ScreenPoint: TPoint;
begin
  ClientPoint := Point(Message.XPos, Message.YPos);
  ScreenPoint := ClientToScreen(ClientPoint);

  if ((Message.Event = WM_RBUTTONDOWN)
    and (ControlAtPos(ClientPoint, False, True) = PContent)
    and (PContent.ControlAtPos(PContent.ScreenToClient(ScreenPoint), False, True) = PResult)
    and (PResult.ControlAtPos(PResult.ScreenToClient(ScreenPoint), False, True) = ActiveDBGrid.Parent)
    and (ActiveDBGrid.Parent.ControlAtPos(ActiveDBGrid.Parent.ScreenToClient(ScreenPoint), False, True) = ActiveDBGrid)) then
  begin
    Window.ActiveControl := ActiveDBGrid;
    GridPoint := ActiveDBGrid.Parent.ScreenToClient(ScreenPoint);
    GridCoord := ActiveDBGrid.MouseCoord(GridPoint.X, GridPoint.Y);
    if ((GridCoord.X >= 0) and (GridCoord.Y = 0)) then
      ActiveDBGrid.PopupMenu := MGridHeader
    else
      ActiveDBGrid.PopupMenu := MGrid;
  end;

  inherited;
end;

procedure TFClient.WMTimer(var Message: TWMTimer);
begin
  case (Message.TimerID) of
    tiNavigator:
      FNavigatorChange2(FNavigator, FNavigator.Selected);
    tiStatusBar:
      begin
        StatusBar.Panels[sbMessage].Text := '';
        StatusBarRefresh();
      end;
  end;
end;

procedure TFClient.WorkbenchAddTable(Sender: TObject);
var
  BaseTable: TCBaseTable;
  MenuItem: TMenuItem;
  Point: TPoint;
begin
  if (Sender is TMenuItem) then
  begin
    MenuItem := TMenuItem(Sender);

    if ((MenuItem.GetParentMenu() is TPopupMenu) and (TObject(MenuItem.Tag) is TCTable)) then
    begin
      Point := ActiveWorkbench.ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint);
      BaseTable := TCBaseTable(TMenuItem(Sender).Tag);

      ActiveWorkbench.AddExistingTable(Point.X, Point.Y, BaseTable);
    end;
  end;
end;

procedure TFClient.WorkbenchChange(Sender: TObject; Control: TWControl);
var
  aEPasteEnabled: Boolean;
  ClipboardData: HGLOBAL;
  Database: TCDatabase;
  DatabaseName: string;
  Index: Integer;
  S: string;
  AccountName: string;
  Table: TCBaseTable;
  TableName: string;
  Values: TStringList;
begin
  if (not Clipboard.HasFormat(CF_MYSQLTABLE) or not OpenClipboard(Handle)) then
    aEPasteEnabled := False
  else
  begin
    ClipboardData := GetClipboardData(CF_MYSQLTABLE);
    S := PChar(GlobalLock(ClipboardData));
    GlobalUnlock(ClipboardData);
    CloseClipboard();

    Values := TStringList.Create();
    Values.Text := ReplaceStr(Trim(S), ',', #13#10);

    aEPasteEnabled := Values.Count = 1;

    if (aEPasteEnabled) then
    begin
      S := Values[0];

      if (Pos('.', S) = 0) then
        AccountName := ''
      else
      begin
        Index := Pos('.', S);
        while ((Index > 1) and (S[Index - 1] = '\')) do
          Inc(Index, Pos('.', Copy(S, Index + 1, Length(S) - Index)));
        AccountName := ReplaceStr(Copy(S, 1, Index - 1), '\.', '.');
        Delete(S, 1, Index);
      end;
      if (Pos('.', S) = 0) then
        DatabaseName := ''
      else
      begin
        DatabaseName := Copy(S, 1, Pos('.', S) - 1);
        Delete(S, 1, Length(DatabaseName) + 1);
      end;
      if (Pos('.', S) = 0) then
        TableName := S
      else
        TableName := '';

      Table := Client.DatabaseByName(SelectedDatabase).BaseTableByName(TableName);

      aEPasteEnabled := (AccountName = Client.Account.Name) and (DatabaseName = SelectedDatabase)
        and Assigned(Table) and Assigned(Table.Engine) and Table.Engine.ForeignKeyAllowed
        and not Assigned(ActiveWorkbench.TableByCaption(Table.Name));
    end;

    Values.Free();
  end;

  Database := Client.DatabaseByName(SelectedDatabase);

  aPOpenInNewWindow.Enabled := (Control is TWTable);
  aPOpenInNewTab.Enabled := (Control is TWTable);
  MainAction('aFExportSQL').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportText').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportExcel').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportAccess').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportSQLite').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportODBC').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportXML').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportHTML').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportBitmap').Enabled := not Assigned(Control) and Assigned(ActiveWorkbench) and (ActiveWorkbench.ControlCount > 0);
  MainAction('aFPrint').Enabled := Assigned(ActiveWorkbench) and (ActiveWorkbench.ControlCount > 0);
  MainAction('aECopy').Enabled := Assigned(Control) and (not (Control is TWLink) or (Control is TWForeignKey));
  MainAction('aEPaste').Enabled := aEPasteEnabled;
  MainAction('aDCreateTable').Enabled := not Assigned(Control) or (Control is TWSection);
  mwDCreateTable.Enabled := MainAction('aDCreateTable').Enabled;
  MainAction('aDCreateKey').Enabled := Control is TWTable;
  MainAction('aDCreateField').Enabled := Control is TWTable;
  MainAction('aDCreateForeignKey').Enabled := (Control is TWTable) and Assigned(TWTable(Control).BaseTable.Engine) and TWTable(Control).BaseTable.Engine.ForeignKeyAllowed;
  mwCreateSection.Enabled := not Assigned(Control);
  mwCreateLink.Enabled := Control is TWTable;
  MainAction('aDCreateTrigger').Enabled := (Control is TWTable) and Assigned(TWTable(Control).BaseTable) and Assigned(Database.Triggers);
  MainAction('aDDeleteTable').Enabled := Control is TWTable;
  MainAction('aDDeleteForeignKey').Enabled := (Control is TWForeignKey);
  MainAction('aDEmpty').Enabled := Control is TWTable;
  mwDProperties.Enabled := Assigned(Control) and (not (Control is TWLink) or (Control is TWForeignKey));

  aDDelete.Enabled := MainAction('aDDeleteTable').Enabled or MainAction('aDDeleteForeignKey').Enabled;
end;

procedure TFClient.WorkbenchCursorMove(Sender: TObject; X, Y: Integer);
begin
  StatusBar.Panels[sbItem].Text := IntToStr(X) + ':' + IntToStr(Y);
end;

procedure TFClient.WorkbenchDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if (Assigned(MouseDownNode) and (MouseDownNode.TreeView = Source) and (Source = FNavigator)
    and (FNavigator.Selected = MouseDownNode.Parent) and (MouseDownNode.ImageIndex = iiBaseTable)) then
  begin
    ActiveWorkbench.AddExistingTable(X, Y, TCBaseTable(MouseDownNode.Data));
    Window.ActiveControl := ActiveWorkbench;
  end;
end;

procedure TFClient.WorkbenchDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  BaseTable: TCBaseTable;
begin
  Accept := False;

  if (Assigned(MouseDownNode) and (MouseDownNode.TreeView = Source) and (Source = FNavigator)
    and (FNavigator.Selected = MouseDownNode.Parent) and (MouseDownNode.ImageIndex = iiBaseTable)) then
  begin
    BaseTable := TCBaseTable(MouseDownNode.Data);
    Accept := Assigned(BaseTable) and not Assigned(ActiveWorkbench.TableByBaseTable(BaseTable));
  end;
end;

procedure TFClient.WorkbenchEmptyExecute(Sender: TObject);
var
  BaseTable: TCBaseTable;
begin
  if (ActiveWorkbench.Selected is TWTable) then
  begin
    BaseTable := TWTable(ActiveWorkbench.Selected).BaseTable;

    if (MsgBox(Preferences.LoadStr(375, BaseTable.Name), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
      BaseTable.Empty();
  end;
end;

procedure TFClient.WorkbenchEnter(Sender: TObject);
begin
  if (Sender is TWWorkbench) then
  begin
    MainAction('aDEmpty').OnExecute := WorkbenchEmptyExecute;

    WorkbenchChange(Sender, TWWorkbench(Sender).Selected);
  end;
end;

procedure TFClient.WorkbenchExit(Sender: TObject);
begin
  MainAction('aFExportSQL').Enabled := False;
  MainAction('aFExportText').Enabled := False;
  MainAction('aFExportExcel').Enabled := False;
  MainAction('aFExportAccess').Enabled := False;
  MainAction('aFExportSQLite').Enabled := False;
  MainAction('aFExportODBC').Enabled := False;
  MainAction('aFExportXML').Enabled := False;
  MainAction('aFExportHTML').Enabled := False;
  MainAction('aFExportBitmap').Enabled := False;
  MainAction('aFPrint').Enabled := False;
  MainAction('aECopy').Enabled := False;
  MainAction('aDCreateTable').Enabled := False;
  MainAction('aDCreateView').Enabled := False;
  MainAction('aDCreateProcedure').Enabled := False;
  MainAction('aDCreateFunction').Enabled := False;
  MainAction('aDCreateEvent').Enabled := False;
  MainAction('aDCreateKey').Enabled := False;
  MainAction('aDCreateField').Enabled := False;
  MainAction('aDCreateForeignKey').Enabled := False;
  MainAction('aDCreateTrigger').Enabled := False;
  MainAction('aDDeleteTable').Enabled := False;
  MainAction('aDDeleteForeignKey').Enabled := False;
  mwCreateSection.Enabled := False;
  mwCreateLink.Enabled := False;
  MainAction('aDEmpty').Enabled := False;
end;

procedure TFClient.WorkbenchPasteExecute(Sender: TObject);
var
  aEPasteEnabled: Boolean;
  ClipboardData: HGLOBAL;
  DatabaseName: string;
  Index: Integer;
  MenuItem: TMenuItem;
  P: TPoint;
  S: string;
  AccountName: string;
  BaseTable: TCBaseTable;
  TableName: string;
  Values: TStringList;
begin
  Wanted.Clear();

  if (not Clipboard.HasFormat(CF_MYSQLTABLE) or not OpenClipboard(Handle)) then
    MessageBeep(MB_ICONERROR)
  else
  begin
    ClipboardData := GetClipboardData(CF_MYSQLTABLE);
    S := PChar(GlobalLock(ClipboardData));
    GlobalUnlock(ClipboardData);
    CloseClipboard();

    Values := TStringList.Create();
    Values.Text := ReplaceStr(Trim(S), ',', #13#10);

    aEPasteEnabled := Values.Count = 1;

    if (aEPasteEnabled) then
    begin
      S := Values[0];

      if (Pos('.', S) = 0) then
        AccountName := ''
      else
      begin
        Index := Pos('.', S);
        while ((Index > 1) and (S[Index - 1] = '\')) do
          Inc(Index, Pos('.', Copy(S, Index + 1, Length(S) - Index)));
        AccountName := ReplaceStr(Copy(S, 1, Index - 1), '\.', '.');
        Delete(S, 1, Index);
      end;
      if (Pos('.', S) = 0) then
        DatabaseName := ''
      else
      begin
        DatabaseName := Copy(S, 1, Pos('.', S) - 1);
        Delete(S, 1, Length(DatabaseName) + 1);
      end;
      if (Pos('.', S) = 0) then
        TableName := S
      else
        TableName := '';

      P := Point(0, 0);
      if (Sender is TMenuItem) then
      begin
        MenuItem := TMenuItem(Sender);

        if ((MenuItem.GetParentMenu() is TPopupMenu)) then
          P := ActiveWorkbench.ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint);
      end;
      BaseTable := Client.DatabaseByName(SelectedDatabase).BaseTableByName(TableName);

      ActiveWorkbench.AddExistingTable(P.X, P.Y, BaseTable);
    end;

    Values.Free();
  end;
end;

procedure TFClient.WorkbenchPrintExecute(Sender: TObject);
begin
  Wanted.Clear();

  PrintDialog.FromPage := 1;
  PrintDialog.ToPage := 1;
  PrintDialog.MinPage := 1;
  PrintDialog.MaxPage := 1;
  if (FSQLEditorPrint.PageCount = 1) then
    PrintDialog.Options := PrintDialog.Options - [poPageNums]
  else
    PrintDialog.Options := PrintDialog.Options + [poPageNums];
  PrintDialog.Options := PrintDialog.Options - [poSelection];
  PrintDialog.PrintRange := prAllPages;
  if (PrintDialog.Execute()) then
    ActiveWorkbench.Print(SelectedDatabase);
end;

function TFClient.WorkbenchValidateControl(Sender: TObject; Control: TWControl): Boolean;
var
  ChildTable: TCBaseTable;
  ParentTable: TCBaseTable;
begin
  if (Control is TWTable) then
  begin
    if (Assigned(TWTable(Control).BaseTable)) then
      Result := TWTable(Control).BaseTable.Update()
    else
    begin
      DTable.Database := Control.Workbench.Database;
      DTable.Table := nil;
      DTable.Tables := nil;
      Result := DTable.Execute();
      if (Result) then
        Client.Update();
    end
  end
  else if (Control is TWForeignKey) then
  begin
    ChildTable := TWForeignKey(Control).ChildTable.BaseTable;
    ParentTable := TWForeignKey(Control).ParentTable.BaseTable;

    if (Assigned(ChildTable) and Assigned(ParentTable) and not Assigned(TWForeignKey(Control).BaseForeignKey)) then
    begin
      DForeignKey.Database := Control.Workbench.Database;
      DForeignKey.Table := ChildTable;
      DForeignKey.ParentTable := ParentTable;
      DForeignKey.ForeignKey := nil;
      Result := DForeignKey.Execute();
      if (Result) then
        Client.Update();
    end
    else
      Result := False;
  end
  else if (Control is TWLink) then
  begin
    ChildTable := TWForeignKey(Control).ChildTable.BaseTable;
    ParentTable := TWForeignKey(Control).ParentTable.BaseTable;

    Result := Assigned(ChildTable) and Assigned(ParentTable);
  end
  else
    Result := False;
end;

end.

