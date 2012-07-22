unit ExtCtrls_Ext;

interface {********************************************************************}

uses
  SysUtils, Classes, Controls, ExtCtrls, Messages, Graphics;

type
  TPanel_Ext = class(TPanel)
  private
    FOnPaint: TNotifyEvent;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
  protected
    procedure Paint(); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  published
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TSplitter_Ext = class(TSplitter)
  private
    FActiveBorder: TAlign;
    FActiveBorderColor: TColor;
  protected
    procedure Paint(); override;
    procedure SetActiveBorder(AActiveBorder: TAlign);
    procedure SetActiveBorderColor(AActiveBorderColor: TColor);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ActiveBorder: TAlign read FActiveBorder write SetActiveBorder default alNone;
    property ActiveBorderColor: TColor read FActiveBorderColor write SetActiveBorderColor default clBtnFace;
  end;

procedure Register();

implementation {***************************************************************}

uses
  Windows, CommCtrl, Themes;

procedure Register();
begin
  RegisterComponents('VCL Extensions', [TPanel_Ext]);
  RegisterComponents('VCL Extensions', [TSplitter_Ext]);
end;

{ TPanel_Ext ******************************************************************}

procedure TPanel_Ext.CMVisibleChanged(var Message: TMessage);
begin
  RequestAlign();

  inherited;
end;

constructor TPanel_Ext.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle - [csParentBackground] + [csOpaque];
end;

procedure TPanel_Ext.Paint();
begin
  inherited;

  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TPanel_Ext.WMNotify(var Message: TWMNotify);
begin
  if (not Assigned(Parent)) then
    inherited
  else
    Message.Result := Parent.Perform(Message.Msg, TMessage(Message).WParam, TMessage(Message).LParam);
end;

{ TSplitter_Ext ***************************************************************}

constructor TSplitter_Ext.Create(AOwner: TComponent);
begin
  inherited;

  FActiveBorderColor := Color;
end;

procedure TSplitter_Ext.Paint();
var
  Rect: TRect;
begin
  inherited;

  if (StyleServices.Enabled and CheckWin32Version(6) and (ActiveBorder <> alNone)) then
  begin
    Rect := ClientRect;

    Canvas.Brush.Color := ActiveBorderColor;
    case (ActiveBorder) of
      alLeft: Rect.Right := Rect.Left + 1;
      alTop: Rect.Bottom := Rect.Top + 1;
      alRight: Rect.Left := Rect.Right - 1;
      alBottom: Rect.Top := Rect.Bottom - 1;
    end;
    Canvas.FillRect(Rect);
  end;
end;

procedure TSplitter_Ext.SetActiveBorder(AActiveBorder: TAlign);
begin
  FActiveBorder := AActiveBorder;
  Invalidate();
end;

procedure TSplitter_Ext.SetActiveBorderColor(AActiveBorderColor: TColor);
begin
  FActiveBorderColor := AActiveBorderColor;
  Invalidate();
end;

end.
