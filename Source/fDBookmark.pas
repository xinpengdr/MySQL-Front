unit fDBookmark;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  Forms_Ext, StdCtrls_Ext,
  fBase, fAccount;

type
  TDBookmark = class(TForm_Ext)
    FAddress: TEdit;
    FBCancel: TButton;
    FBHelp: TButton;
    FBOk: TButton;
    FCaption: TEdit;
    FLAddress: TLabel;
    FLCaption: TLabel;
    GBasics: TGroupBox_Ext;
    procedure FBHelpClick(Sender: TObject);
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Bookmark: TABookmark;
    Bookmarks: TABookmarks;
    NewCaption: string;
    NewURI: string;
    function Execute(): Boolean;
  end;

function DBookmark(): TDBookmark;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils,
  fPreferences;

var
  FBookmark: TDBookmark;

function DBookmark(): TDBookmark;
begin
  if (not Assigned(FBookmark)) then
  begin
    Application.CreateForm(TDBookmark, FBookmark);
    FBookmark.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FBookmark;
end;

{ TDBookmark ******************************************************************}

procedure TDBookmark.CMChangePreferences(var Message: TMessage);
begin
  GBasics.Caption := Preferences.LoadStr(85);
  FLCaption.Caption := Preferences.LoadStr(35) + ':';
  FLAddress.Caption := Preferences.LoadStr(730) + ':';

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDBookmark.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDBookmark.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDBookmark.FBOkCheckEnabled(Sender: TObject);
var
  Index: Integer;
begin
  Index := Bookmarks.IndexOf(Bookmarks.ByCaption(FCaption.Text));

  FBOk.Enabled := (FCaption.Text <> '') and (FAddress.Text <> '')
    and ((not Assigned(Bookmark) and (Index < 0)) or (Assigned(Bookmark) and ((Index = -1) or (Index = Bookmarks.IndexOf(Bookmark)))));
end;

procedure TDBookmark.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  NewBookmark: TABookmark;
begin
  if (ModalResult = mrOk) then
  begin
    NewBookmark := TABookmark.Create(Bookmarks);
    if (Assigned(Bookmark)) then
      NewBookmark.Assign(Bookmark);

    NewBookmark.Caption := Trim(FCaption.Text);
    NewBookmark.URI := Trim(FAddress.Text);

    if (not Assigned(Bookmark)) then
      CanClose := Bookmarks.AddBookmark(NewBookmark)
    else
      CanClose := Bookmarks.UpdateBookmark(Bookmark, NewBookmark);

    NewBookmark.Free();
  end;
end;

procedure TDBookmark.FormShow(Sender: TObject);
var
  I: Integer;
begin
  if (not Assigned(Bookmark)) then
  begin
    HelpContext := 1087;
    Caption := ReplaceStr(Preferences.LoadStr(728), '&', '');
  end
  else
  begin
    HelpContext := 1088;
    Caption := Preferences.LoadStr(842, Bookmark.Caption);
  end;

  if (not Assigned(Bookmark)) then
  begin
    FCaption.Text := NewCaption;
    FAddress.Text := NewURI;

    I := 2;
    while (Assigned(Bookmarks.ByCaption(FCaption.Text))) do
    begin
      FCaption.Text := NewCaption + ' (' + IntToStr(I) + ')';
      Inc(I);
    end;
  end
  else
  begin
    FCaption.Text := Bookmark.Caption;
    FAddress.Text := Bookmark.URI;
  end;

  FBOkCheckEnabled(Sender);

  ActiveControl := FBCancel;
  ActiveControl := FCaption;
end;

initialization
  FBookmark := nil;
end.
