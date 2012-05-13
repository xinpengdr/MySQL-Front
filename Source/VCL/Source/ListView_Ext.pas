unit ListView_Ext;

interface {********************************************************************}

uses
  SysUtils, Classes, Controls, ComCtrls, TntComCtrls;

type
  TListView_Ext = class(TTntListView)
  public
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  end;

procedure Register;

implementation {***************************************************************}

uses
  StdActns;

procedure Register;
begin
  RegisterComponents('VCL Extensions', [TListView_Ext]);
end;

{ TListView_Ext ***************************************************************}

function TListView_Ext.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if (Action is TEditSelectAll) then
    begin SelectAll(); Result := True; end
  else
    Result := inherited ExecuteAction(Action);
end;

function TListView_Ext.UpdateAction(Action: TBasicAction): Boolean;
begin
  if (Action is TEditSelectAll) then
  begin
    Result := Focused;
    if (Result) then
      TEditSelectAll(Action).Enabled := Items.Count > 0;
  end
  else
    Result := inherited UpdateAction(Action);
end;

end.
