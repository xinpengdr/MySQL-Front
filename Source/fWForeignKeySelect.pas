unit fWForeignKeySelect;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, DB,
  MySQLDBGrid, MySQLDB,
  fClient, fBase;

type
  TWForeignKeySelect = class(TForm)
    DataSource: TDataSource;
    FParentGrid: TMySQLDBGrid;
    ParentDataSet: TMySQLDataSet;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FParentGridDblClick(Sender: TObject);
    procedure FParentGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ParentDataSetAfterOpen(DataSet: TDataSet);
    procedure ParentDataSetAfterReceivingRecords(DataSet: TDataSet);
    procedure ParentDataSetBeforeReceivingRecords(DataSet: TDataSet);
  private
    Client: TCClient;
    DataSets: array of TMySQLQuery;
    FForeignKey: TCForeignKey;
    function SendSQLEvent(const Connection: TMySQLConnection; const Data: Boolean): Boolean;
    procedure SetForeignKey(const AForeignKey: TCForeignKey);
    procedure CMAfterReceivingDataSet(var Message: TMessage); message CM_AFTER_RECEIVING_DATASET;
    procedure CMBeforeReceivingDataSet(var Message: TMessage); message CM_BEFORE_RECEIVING_DATASET;
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMPostShow(var Message: TMessage); message CM_POSTSHOW;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    ChildGrid: TMySQLDBGrid;
    property ForeignKey: TCForeignKey read FForeignKey write SetForeignKey;
  end;

function WForeignKeySelect(): TWForeignKeySelect;

implementation {***************************************************************}

{$R *.dfm}

uses
  System.Types,
  DBCommon,
  SQLUtils,
  fPreferences;

var
  FForeignKeySelect: TWForeignKeySelect;

function WForeignKeySelect(): TWForeignKeySelect;
begin
  if (not Assigned(FForeignKeySelect)) then
  begin
    Application.CreateForm(TWForeignKeySelect, FForeignKeySelect);
    FForeignKeySelect.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FForeignKeySelect;
end;

{ WForeignKeySelect ***********************************************************}

procedure TWForeignKeySelect.CMAfterReceivingDataSet(var Message: TMessage);
var
  ChildDatabase: TCDatabase;
  ChildField: TCTableField;
  ChildFieldInfo: TFieldInfo;
  ChildTable: TCBaseTable;
  I: Integer;
  J: Integer;
  KeyFields: string;
  KeyValues: array of Variant;
begin
  if (not ParentDataSet.IsEmpty and ((Message.LParam = 1) or (ParentDataSet.RecNo = 0))) then
  begin
    KeyFields := '';
    for I := 0 to Length(FForeignKey.Parent.FieldNames) - 1 do
    begin
      if (I > 0) then KeyFields := KeyFields + ';';
      KeyFields := KeyFields + FForeignKey.Parent.FieldNames[I];
    end;

    SetLength(KeyValues, Length(FForeignKey.Fields));
    for I := 0 to ChildGrid.Columns.Count - 1 do
      if (GetFieldInfo(ChildGrid.Fields[I].Origin, ChildFieldInfo)) then
      begin
        ChildDatabase := ForeignKey.Table.Database.Client.DatabaseByName(ChildFieldInfo.DatabaseName);
        if (Assigned(ChildDatabase)) then
        begin
          ChildTable := ChildDatabase.BaseTableByName(ChildFieldInfo.TableName);
          if (Assigned(ChildTable)) then
          begin
            ChildField := ChildTable.FieldByName(ChildFieldInfo.OriginalFieldName);
            for J := 0 to Length(ForeignKey.Fields) - 1 do
              if (ForeignKey.Fields[J] = ChildField) then
                KeyValues[J] := ChildGrid.Fields[I].NewValue;
          end;
        end;
      end;

    ParentDataSet.Locate(KeyFields, KeyValues, []);
  end;
end;

procedure TWForeignKeySelect.CMBeforeReceivingDataSet(var Message: TMessage);
begin
  TMySQLDataSet(Message.WParam).Open();
end;

procedure TWForeignKeySelect.CMChangePreferences(var Message: TMessage);
begin
  FParentGrid.Font.Name := Preferences.GridFontName;
  FParentGrid.Font.Style := Preferences.GridFontStyle;
  FParentGrid.Font.Color := Preferences.GridFontColor;
  FParentGrid.Font.Size := Preferences.GridFontSize;
  FParentGrid.Font.Charset := Preferences.GridFontCharset;
  FParentGrid.Canvas.Font := FParentGrid.Font;
end;

procedure TWForeignKeySelect.CMPostShow(var Message: TMessage);
var
  I: Integer;
  ParentDatabase: TCDatabase;
  ParentTable: TCBaseTable;
  SQL: string;
begin
  if (Assigned(FForeignKey) and Assigned(ChildGrid)) then
    if (not ParentDataSet.Active) then
    begin
      ParentDatabase := Client.DatabaseByName(FForeignKey.Parent.DatabaseName);
      ParentTable := ParentDatabase.BaseTableByName(FForeignKey.Parent.TableName);
      SQL := '';
      for I := 0 to ParentTable.Fields.Count - 1 do
      begin
        if (I > 0) then SQL := SQL + ',';
        case (ParentTable.Fields[I].FieldType) of
          mfChar, mfVarChar, mfBinary, mfVarBinary:
            SQL := SQL + 'SUBSTRING(' + Client.EscapeIdentifier(ParentTable.Fields[I].Name) + ',1,' + IntToStr(Preferences.GridMaxColumnWidth) + ') AS ';
          mfTinyText, mfText, mfMediumText, mfLongText:
            if (not Preferences.GridShowMemoContent) then
              SQL := SQL + SQLEscape('<MEMO>') + ' AS '
            else
              SQL := SQL + 'SUBSTRING(' + Client.EscapeIdentifier(ParentTable.Fields[I].Name) + ',1,' + IntToStr(Preferences.GridMaxColumnWidth) + ') AS ';
          mfTinyBlob, mfBlob, mfMediumBlob, mfLongBlob:
            SQL := SQL + SQLEscape('<BLOB>') + ' AS ';
          mfGeometry, mfPoint, mfLineString, mfPolygon, mfMultiPoint, mfMultiLineString, mfMultiPolygon, mfGeometryCollection:
            SQL := SQL + SQLEscape('<GEO>') + ' AS ';
        end;
        SQL := SQL + Client.EscapeIdentifier(ParentTable.Fields[I].Name);
      end;
      SQL := 'SELECT ' + SQL + ' FROM ' + Client.EscapeIdentifier(ParentDatabase.Name) + '.' + Client.EscapeIdentifier(ParentTable.Name);
      if (Client.Account.DefaultSorting and (ParentTable.Indices.Count > 0) and (ParentTable.Indices[0].Name = '')) then
      begin
        SQL := SQL + ' ORDER BY ';
        for I := 0 to ParentTable.Indices[0].Columns.Count - 1 do
        begin
          if (I > 0) then SQL := SQL + ',';
          SQL := SQL + Client.EscapeIdentifier(ParentTable.Indices[0].Columns[I].Field.Name);
        end;
      end;

      Client.SendSQL(SQL, SendSQLEvent);

      if (not Client.Asynchron) then
        SendMessage(Handle, CM_AFTER_RECEIVING_DATASET, WParam(ParentDataSet), 1);
    end
    else
    begin
      ParentDataSetAfterOpen(ParentDataSet);
      SendMessage(Handle, CM_AFTER_RECEIVING_DATASET, WParam(ParentDataSet), 1);
    end;
end;

procedure TWForeignKeySelect.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.Style := WS_POPUP or WS_SIZEBOX;
end;

procedure TWForeignKeySelect.FormCreate(Sender: TObject);
begin
  inherited;

  FForeignKey := nil;

  Height := 120;
  Width := 260;
end;

procedure TWForeignKeySelect.FormDeactivate(Sender: TObject);
begin
  Hide();
end;

procedure TWForeignKeySelect.FormDestroy(Sender: TObject);
begin
  SetLength(DataSets, 0);
end;

procedure TWForeignKeySelect.FormShow(Sender: TObject);
var
  HeaderRect: TRect;
  I: Integer;
  L: Integer;
  T: Integer;
begin
  if (Assigned(ChildGrid)) then
  begin
    L := 0;
    for I := ChildGrid.LeftCol to ChildGrid.SelectedIndex - 1 do
      if (dgColLines in ChildGrid.Options) then
        Inc(L, ChildGrid.Columns[I].Width + 1)
      else
        Inc(L, ChildGrid.Columns[I].Width);
    Left := ChildGrid.ClientToScreen(Point(0, 0)).X - GetSystemMetrics(SM_CXEDGE) + L - 1;

    GetWindowRect(ChildGrid.Header, HeaderRect);
    if (dgRowLines in ChildGrid.Options) then
      T := (ChildGrid.Row - ChildGrid.TopRow + 1) * (ChildGrid.DefaultRowHeight + 1)
    else
      T := (ChildGrid.Row - ChildGrid.TopRow + 1) * ChildGrid.DefaultRowHeight;
    Top := ChildGrid.ClientToScreen(Point(0, 0)).Y - GetSystemMetrics(SM_CYEDGE) + HeaderRect.Bottom - HeaderRect.Top + T;

    FParentGrid.DefaultDrawing := not Assigned(ChildGrid.OnDrawColumnCell);
    if (not FParentGrid.DefaultDrawing) then
      FParentGrid.OnDrawColumnCell := ChildGrid.OnDrawColumnCell;

    PostMessage(Self.Handle, CM_POSTSHOW, 0, 0);
  end;
end;

procedure TWForeignKeySelect.FParentGridDblClick(Sender: TObject);
var
  ChildDatabase: TCDatabase;
  ChildField: TCTableField;
  ChildTable: TCBaseTable;
  FieldInfo: TFieldInfo;
  I: Integer;
  J: Integer;
begin
  if (Assigned(ChildGrid) and Assigned(ForeignKey) and ChildGrid.DataSource.DataSet.CanModify) then
  begin
    ChildGrid.DataSource.DataSet.Edit();

    for I := 0 to ChildGrid.Columns.Count - 1 do
      if (GetFieldInfo(ChildGrid.Fields[I].Origin, FieldInfo)) then
      begin
        ChildDatabase := ForeignKey.Table.Database.Client.DatabaseByName(FieldInfo.DatabaseName);
        if (Assigned(ChildDatabase)) then
        begin
          ChildTable := ChildDatabase.BaseTableByName(FieldInfo.TableName);
          if (Assigned(ChildTable)) then
          begin
            ChildField := ChildTable.FieldByName(FieldInfo.OriginalFieldName);
            for J := 0 to Length(ForeignKey.Fields) - 1 do
              if (ForeignKey.Fields[J] = ChildField) then
                ChildGrid.Columns[I].Field.Value := ParentDataSet.FieldByName(ForeignKey.Parent.FieldNames[J]).Value;
          end;
        end;
      end;
  end;

  Hide();
end;

procedure TWForeignKeySelect.FParentGridKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if ((Key = VK_LEFT) and (Shift = [])) then
  begin
    if (FParentGrid.LeftCol > 0) then
      FParentGrid.LeftCol := FParentGrid.LeftCol - 1;
    Key := 0;
  end
  else if ((Key = VK_RIGHT) and (Shift = [])) then
  begin
    if (FParentGrid.LeftCol < FParentGrid.Columns.Count - 1) then
      FParentGrid.LeftCol := FParentGrid.LeftCol + 1;
    Key := 0;
  end
  else if ((Key = VK_HOME) and (Shift = [])) then
  begin
    FParentGrid.LeftCol := 0;
    Key := 0;
  end
  else if ((Key = VK_END) and (Shift = [])) then
  begin
    FParentGrid.LeftCol := FParentGrid.Columns.Count - 1;
    Key := 0;
  end
  else if ((Key = VK_ESCAPE) and (Shift = [])) then
  begin
    Hide();
    Key := 0;
  end
  else if ((Key = VK_RETURN) and (Shift = [])) then
  begin
    FParentGridDblClick(Sender);
    Key := 0;
  end;
end;

procedure TWForeignKeySelect.ParentDataSetAfterOpen(DataSet: TDataSet);
var
  ChildDatabase: TCDatabase;
  ChildField: TCTableField;
  ChildFieldInfo: TFieldInfo;
  ChildTable: TCBaseTable;
  I: Integer;
begin
  if (Assigned(ChildGrid) and Assigned(ForeignKey)) then
  begin
    for I := 0 to FParentGrid.Columns.Count - 1 do
    begin
      if (FParentGrid.Columns[I].Width > Preferences.GridMaxColumnWidth) then
        FParentGrid.Columns[I].Width := Preferences.GridMaxColumnWidth;

      if (FParentGrid.Columns[I].Field.IsIndexField) then
        FParentGrid.Columns[I].Font.Style := FParentGrid.Columns[I].Font.Style + [fsBold]
      else
        FParentGrid.Columns[I].Font.Style := FParentGrid.Columns[I].Font.Style - [fsBold];
    end;

    if (GetFieldInfo(ChildGrid.SelectedField.Origin, ChildFieldInfo)) then
    begin
      ChildDatabase := ForeignKey.Table.Database.Client.DatabaseByName(ChildFieldInfo.DatabaseName);
      if (Assigned(ChildDatabase)) then
      begin
        ChildTable := ChildDatabase.BaseTableByName(ChildFieldInfo.TableName);
        if (Assigned(ChildTable)) then
        begin
          ChildField := ChildTable.FieldByName(ChildFieldInfo.OriginalFieldName);
          for I := 0 to Length(ForeignKey.Fields) - 1 do
            if (ForeignKey.Fields[I] = ChildField) then
              FParentGrid.LeftCol := I;
        end;
      end;
    end;
  end;
end;

procedure TWForeignKeySelect.ParentDataSetAfterReceivingRecords(
  DataSet: TDataSet);
begin
  PostMessage(Handle, CM_AFTER_RECEIVING_DATASET, WParam(DataSet), 0);
end;

procedure TWForeignKeySelect.ParentDataSetBeforeReceivingRecords(
  DataSet: TDataSet);
begin
  PostMessage(Handle, CM_BEFORE_RECEIVING_DATASET, WParam(DataSet), 0);
end;

function TWForeignKeySelect.SendSQLEvent(const Connection: TMySQLConnection; const Data: Boolean): Boolean;
begin
  ParentDataSet.Open();

  Result := False;
end;

procedure TWForeignKeySelect.SetForeignKey(const AForeignKey: TCForeignKey);
begin
  if (AForeignKey <> FForeignKey) then
  begin
    if (Assigned(ParentDataSet)) then
      ParentDataSet.Close();

    Client := AForeignKey.Table.Database.Client;
    FForeignKey := AForeignKey;
  end;
end;

initialization
  FForeignKeySelect := nil;
end.

