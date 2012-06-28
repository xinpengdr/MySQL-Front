object WForeignKeySelect: TWForeignKeySelect
  Left = 727
  Top = 261
  BorderIcons = []
  Caption = 'WForeignKeySelect'
  ClientHeight = 277
  ClientWidth = 504
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 100
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 13
  object FParentGrid: TMySQLDBGrid
    Left = 0
    Top = 0
    Width = 504
    Height = 277
    Align = alClient
    BorderStyle = bsNone
    Constraints.MinHeight = 30
    DataSource = DataSource
    DefaultDrawing = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
    ParentFont = False
    ParentShowHint = False
    ReadOnly = True
    ShowHint = False
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnDblClick = FParentGridDblClick
    OnKeyDown = FParentGridKeyDown
  end
  object ParentDataSet: TMySQLDataSet
    AfterOpen = ParentDataSetAfterOpen
    AfterReceivingRecords = ParentDataSetAfterReceivingRecords
    FilterOptions = [foNoPartialCompare]
    Left = 112
    Top = 64
  end
  object DataSource: TDataSource
    DataSet = ParentDataSet
    Left = 48
    Top = 64
  end
end
