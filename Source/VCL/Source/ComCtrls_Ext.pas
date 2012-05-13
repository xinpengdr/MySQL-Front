unit ComCtrls_Ext;

interface {********************************************************************}

uses
  Windows, SysUtils, Classes, Controls, ComCtrls, Messages, CommCtrl;

type
  TListView_Ext = class(TListView)
  public
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  end;

type
  TTreeNode_Ext = class(TTreeNode)
  private
    function GetBold(): Boolean;
    procedure SetBold(ABold: Boolean);
  public
    property Bold: Boolean read GetBold write SetBold;
  end;

  TTreeView_Ext = class(TTreeView)
  private
    ShiftDownSelected: TTreeNode;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;

procedure Register();

implementation {***************************************************************}

uses
  StdActns, StdCtrls, Math, Graphics;

procedure Register();
begin
  RegisterComponents('VCL Extensions', [TListView_Ext]);
  RegisterComponents('VCL Extensions', [TTreeView_Ext]);
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

{ TTreeNode_Ext ***************************************************************}

function TTreeNode_Ext.GetBold(): Boolean;
var
  Item: TTVItemW;
begin
  Item.mask := TVIF_STATE;
  Item.hItem := ItemId;
  TreeView_GetItem(TreeView.Handle, Item);
  Result := Item.state and TVIS_BOLD <> 0;
end;

procedure TTreeNode_Ext.SetBold(ABold: Boolean);
var
  Item: TTVItemW;
begin
  Item.mask := TVIF_STATE;
  Item.hItem := ItemId;
  if (TreeView_GetItem(TreeView.Handle, Item)) then
  begin
    if (ABold) then
      Item.state := Item.state or TVIS_BOLD
    else
      Item.state := Item.state and not TVIS_BOLD;
    Item.stateMask := TVIS_BOLD;
    TreeView_SetItem(TreeView.Handle, Item);
  end;
end;

{ TTreeView_Ext ***************************************************************}

procedure TTreeView_Ext.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (ssShift in Shift) then
    ShiftDownSelected := Selected;

  inherited;
end;

procedure TTreeView_Ext.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Child: TTreeNode;
  Node: TTreeNode;
  NodeIndex: Integer;
  OldSelectedIndex: Integer;
begin
  if (not Assigned(Parent)) then
    Node := nil
  else
    Node := GetNodeAt(X, Y);

  inherited;

  if (MultiSelect and Assigned(Node) and Assigned(ShiftDownSelected) and (ShiftDownSelected.Parent = Node.Parent) and (Shift = [ssShift, ssLeft])) then
  begin
    OldSelectedIndex := ShiftDownSelected.Parent.IndexOf(ShiftDownSelected);
    NodeIndex := Node.Parent.IndexOf(Node);

    Child := Node.Parent.getFirstChild();
    repeat
      if ((Child.Parent.IndexOf(Child) in [Min(OldSelectedIndex, NodeIndex) .. Max(OldSelectedIndex, NodeIndex)]) and not Child.Selected) then
        Select(Child, [ssCtrl]);
      Child := Node.Parent.GetNextChild(Child);
    until (not Assigned(Child));
  end;
end;

end.
