unit MySQLReg;

interface {********************************************************************}

uses
  DBReg;

procedure Register();

implementation {***************************************************************}

uses
  Classes, SysUtils, DesignIntf, DSDesign, DesignEditors,
  MySQLDB, MySQLDBGrid;

type
  TLibraryNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure Register();
begin
  RegisterComponents('MySQL', [TMySQLConnection, TMySQLQuery, TMySQLDataSet, TMySQLTable, TMySQLMonitor, TMySQLDBGrid]);
end;

{ TLibraryNameProperty ****************************Ü***************************}

procedure TLibraryNameProperty.GetValueList(List: TStrings);
begin
  if (List.IndexOf('libMySQL.dll') < 0) then
    List.Add('libMySQL.dll');
end;

end.
