{ gfxform.pas: Form base definition
  File maintainer: nvitya@freemail.hu

History:
}

unit gfxform;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, messagequeue, GfxBase, GfxWidget, schar16;
  
type
  TWindowPosition = (wpUser, wpAuto, wpScreenCenter);

  TWMOptionValues = (wmNoBorder, wmMaximizeBtn, wmMinizeBtn, wmCloseBtn);
  TWMOptions = set of TWMOptionValues;

  TGfxForm = class(TWidget)
  private
    FPrevModalForm : TGfxForm;

    FResizeable: boolean;
    procedure SetWindowTitle(const AValue: string);
    function GetWindowTitle8: string;
    procedure SetWindowTitle8(const Value: string);
  protected
    FModalResult : integer;

    FWMOptions  : TWMOptions;

    FWindowTitle : string;

    FParentForm : TGfxForm;

    FWindowPosition : TWindowPosition;

    procedure MsgActivate(var msg : TMessageRec); message MSG_ACTIVATE;
    procedure MsgDeActivate(var msg : TMessageRec); message MSG_DEACTIVATE;

    procedure MsgClose(var msg : TMessageRec); message MSG_CLOSE;

    procedure SetWindowParameters; override; // for X
    procedure AdjustWindowStyle(var ws,es : longword; var pwh : TWinHandle); override; // for win32
    function GetWindowName : string; override;
    procedure MoveWindow(x,y : TGfxCoord);
  public
    constructor Create(AOwner : TComponent); override;

    procedure HandleClose; virtual;

    procedure Show;
    function ShowModal : integer;

    procedure Close;
    procedure Hide;
    procedure PostKillMe;

    procedure SetMinSize;

    property WindowPosition : TWindowPosition read FWindowPosition write FWindowPosition;

    property Resizeable : boolean read FResizeable write FResizeable;

    property WMOptions : TWMOptions read FWMOptions write FWMOptions;

    property ModalResult : integer read FModalResult write FModalResult;

    property WindowTitle8 : string read GetWindowTitle8 write SetWindowTitle8;

    property ParentForm : TGfxForm read FParentForm write FParentForm;

  published

    property WindowTitle : string read FWindowTitle write SetWindowTitle;

  end;

var
  GfxMainForm : TGfxForm;

  GfxTopModalForm : TGfxForm;

function WidgetParentForm(wg : TWidget) : TGfxForm;

implementation

uses
{$ifdef Win32}windows{$else}Xutil, Xlib{$endif};

function WidgetParentForm(wg : TWidget) : TGfxForm;
var
  w : TWidget;
begin
  w := wg;
  while w <> nil do
  begin
    if w is TGfxForm then
    begin
      Result := TGfxForm(w);
      Exit;
    end;
    w := w.Parent;
  end;
  result := nil;
end;


{ TGfxForm }

procedure TGfxForm.MoveWindow(x,y : TGfxCoord);
begin
  if WinHandle > 0 then
{$ifdef Win32}
       Windows.SetWindowPos(WinHandle,0,x,y,0,0,SWP_NOZORDER or SWP_NOSIZE or SWP_NOREDRAW);
{$else}
       XMoveWindow(display, WinHandle, x,y);
{$endif}
end;

procedure TGfxForm.SetWindowTitle(const AValue: string);
var
  p : PByte;
  s8 : string;
begin
  if FWindowTitle=AValue then exit;
  FWindowTitle:=AValue;
  
  if FWinHandle <= 0 then Exit;

  s8 := u16u8trunc(FWindowTitle);
  
{$ifdef Win32}
  SetWindowText(FWinHandle, PChar(s8));
{$else}
  if length(s8) > 0 then p := @s8[1] else p := nil;
  XChangeProperty(display, FWinHandle, 39, 31, 8, 0, p, length(s8));
  XChangeProperty(display, FWinHandle, 37, 31, 8, 0, p, length(s8));
{$endif}
end;

procedure TGfxForm.MsgActivate(var msg : TMessageRec);
begin
  //writeln('activate: ',ClassName);
  if (GfxTopModalForm = nil) or (GfxTopModalForm = self) then
  begin
    FocusRoot := self;

    if FFormDesigner <> nil then
    begin
      FFormDesigner.Dispatch(msg);
      Exit;
    end;

    if ActiveWidget = nil then ActiveWidget := FindFocusWidget(nil,sdFirst);
    if FActiveWidget <> nil then FActiveWidget.SetFocus;
  end;
end;

procedure TGfxForm.MsgDeActivate(var msg: TMessageRec);
begin
  //writeln('deactivate: ',ClassName);
  if FActiveWidget <> nil then ActiveWidget.KillFocus;
end;

procedure TGfxForm.MsgClose(var msg: TMessageRec);
begin
  HandleClose;
end;

procedure TGfxForm.AdjustWindowStyle(var ws, es: longword; var pwh: TWinHandle);
begin
  inherited;
{$ifdef Win32}
  if GfxTopModalForm = self then
  begin
    // for modal windows, this is necessary
    ws := WS_OVERLAPPEDWINDOW or WS_POPUPWINDOW;
    ws := ws and not (WS_MINIMIZEBOX);
    es := 0;
    if ParentForm <> nil then pwh := ParentForm.WinHandle;
    if FWindowPosition = wpAuto then FWindowPosition := wpScreenCenter;
  end
  else
  begin
    if FWindowPosition = wpAuto then
    begin
      FLeft := TGfxCoord(CW_USEDEFAULT);
      FTop  := TGfxCoord(CW_USEDEFAULT);
    end;
    
    if GfxMainForm <> nil then
    begin
      pwh := GfxMainForm.WinHandle;
      ws := WS_OVERLAPPEDWINDOW;
      es := 0;
    end;
  end;  

  if not FResizeable then
  begin
    ws := ws and not (WS_SIZEBOX or WS_MAXIMIZEBOX or WS_MINIMIZEBOX);
  end;
{$else}{$endif}
end;

procedure TGfxForm.SetWindowParameters;
{$ifdef Win32}
var
  r : TRECT;
begin
  if FWindowPosition = wpScreenCenter then
  begin
    GetWindowRect(WinHandle, r);
    FLeft := (ScreenWidth-(r.Right - r.Left)) div 2;
    FTop := (ScreenHeight-(r.Bottom - r.Top)) div 2;
    MoveWindow(FLeft,FTop);
  end;
end;
{$else}
var
  hints : TXSizeHints;
//  wnprop : TXTextProperty;
  p : PByte;
  s8 : string;
  pf : TGfxForm;
begin
  inherited SetWindowParameters;

  hints.flags := 0;

  if FWindowPosition <> wpAuto then
  begin
    hints.flags := hints.flags or PPosition;

    if FWindowPosition = wpScreenCenter then
    begin
      FLeft := (ScreenWidth-Width) div 2;
      FTop := (ScreenHeight-Height) div 2;
      MoveWindow(FLeft,FTop);
    end;
  end;

  if FResizeable then
  begin
    if (FSizeParams.min_Width > 0) or (FSizeParams.min_height > 0) then
    begin
      hints.flags := hints.flags or PMinSize;
      hints.min_width  := FSizeParams.min_width;
      hints.min_height  := FSizeParams.min_height;
    end;

    if (FSizeParams.max_Width > 0) or (FSizeParams.max_height > 0) then
    begin
      hints.flags := hints.flags or PMaxSize;
      hints.max_width  := FSizeParams.max_width;
      hints.max_height  := FSizeParams.max_height;
    end;
  end
  else
  begin
      hints.flags := hints.flags or PMinSize or PMaxSize;
      hints.min_width  := width;
      hints.min_height  := height;
      hints.max_width  := width;
      hints.max_height  := height;
  end;

  XSetWMNormalHints(display, FWinHandle, @hints);

  // Setting the window names:

//  p := PChar(FWindowTitle);
//  XStringListToTextProperty(@p,1, @wnprop);

//  XSetWMName(display, FWinHandle, @wnprop);
//  XSetWMIconName(display, FWinHandle, @wnprop);

  s8 := u16u8trunc(FWindowTitle);
  
  if length(s8) > 0 then p := @s8[1] else p := nil;

  XChangeProperty(display, FWinHandle, 39, 31, 8, 0, p, length(s8));
  XChangeProperty(display, FWinHandle, 37, 31, 8, 0, p, length(s8));

  // send close event instead of quitting the whole application...
  XSetWMProtocols(display, FWinHandle, @xia_wm_delete_window, 1);

  // for modal windows, this is necessary
  if GfxTopModalForm = self then
  begin
    pf := FParentForm;
    if pf <> nil then XSetTransientForHint(display, self.FWinHandle, pf.WinHandle);
  end;

  //GfxSetWMOptions(FWinHandle,FWMOptions);
end;
{$endif}

constructor TGfxForm.Create(AOwner: TComponent);
begin
  FWindowTitle := u8(ClassName);
  FWMOptions := []; //[wmAutoPosition];
  FWindowPosition := wpScreenCenter;

  if GfxMainForm = nil then GfxMainForm := self;

  if (FocusRoot <> nil) and (FocusRoot is TGfxForm)
    then FParentForm := TGfxForm(FocusRoot)
    else FParentForm := nil;

  FActiveWidget := nil;
  FResizeable := true;

  FPrevModalForm := nil;


  inherited Create(AOwner);
  // do not put anything after inherited !!!!
  // it will call AfterCreate!
end;

procedure TGfxForm.Show;
begin
  DoShow;
//  FocusRoot := self;
end;

function TGfxForm.ShowModal : integer;
begin
  FPrevModalForm := GfxTopModalForm;
  GfxTopModalForm := self;

  ModalResult := 0;
  
  Show;
  
  // processing messages until this form ends.
  
  // delivering the remaining messages
  DeliverMessages;

  repeat
    WaitWindowMessage;
    DeliverMessages;
  until (ModalResult <> 0) or (not Visible);

  GfxTopModalForm := FPrevModalForm;
  
  Result := ModalResult;
end;

procedure TGfxForm.HandleClose;
begin
  Close;
end;

procedure TGfxForm.Close;
begin
  Hide;
  if GfxMainForm = self then
  begin
    Halt(0);
  end;
end;

procedure TGfxForm.Hide;
begin
  if (GfxTopModalForm = self) then GfxTopModalForm := self.FPrevModalForm;
  DoHide;
  if ModalResult = 0 then ModalResult := -1;
end;

procedure TGfxForm.PostKillMe;
begin
  messagequeue.PostMessage(self,self,MSG_KILLME,0,0,0);
end;

procedure TGfxForm.SetMinSize;
begin
  FSizeParams.min_width := width;
  FSizeParams.min_height := height;
end;

function TGfxForm.GetWindowName: string;
begin
  result := u16u8trunc(FWindowTitle);
end;

function TGfxForm.GetWindowTitle8: string;
begin
  result := u16u8(WindowTitle);
end;

procedure TGfxForm.SetWindowTitle8(const Value: string);
begin
  WindowTitle := u8(Value);
end;

initialization
begin
  GfxMainForm := nil;
  GfxTopModalForm := nil;
end;

end.

