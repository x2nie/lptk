{ gfxform.pas: Form base definition
  File maintainer: nvitya@freemail.hu

History:
}

unit ptkform;

{$include lptk_config.inc}

interface

uses
  Classes, SysUtils, lptk, ptkwidget;
  
type
  TWindowPosition = (wpUser, wpAuto, wpScreenCenter);

  TWMOptionValues = (wmNoBorder, wmMaximizeBtn, wmMinizeBtn, wmCloseBtn);
  TWMOptions = set of TWMOptionValues;

  TptkForm = class(TptkWidget)
  private
    FPrevModalForm : TptkForm;

    FResizeable: boolean;
    procedure SetWindowTitle(const AValue: widestring);
    function GetWindowTitle8: string;
    procedure SetWindowTitle8(const Value: string);
  protected
    FModalResult : integer;

    FWMOptions  : TWMOptions;

    FWindowTitle : widestring;

    FParentForm : TptkForm;

    FWindowPosition : TWindowPosition;

    procedure MsgActivate(var msg : TptkMessageRec); message MSG_ACTIVATE;
    procedure MsgDeActivate(var msg : TptkMessageRec); message MSG_DEACTIVATE;

    procedure MsgClose(var msg : TptkMessageRec); message MSG_CLOSE;

    procedure SetWindowParameters; override; // for X
    procedure AdjustWindowStyle(var ws,es : longword; var pwh : TptkWinHandle); override; // for win32
    function GetWindowName : string; override;
    procedure MoveWindow(x,y : TptkCoord);
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

    property ParentForm : TptkForm read FParentForm write FParentForm;

  published

    property WindowTitle : widestring read FWindowTitle write SetWindowTitle;

  end;

var
  ptkMainForm : TptkForm;

  ptkTopModalForm : TptkForm;

function WidgetParentForm(wg : TptkWidget) : TptkForm;

implementation

uses
{$ifdef Win32}windows{$else}Xutil, Xlib{$endif};

function WidgetParentForm(wg : TptkWidget) : TptkForm;
var
  w : TptkWidget;
begin
  w := wg;
  while w <> nil do
  begin
    if w is TptkForm then
    begin
      Result := TptkForm(w);
      Exit;
    end;
    w := w.Parent;
  end;
  result := nil;
end;


{ TptkForm }

procedure TptkForm.MoveWindow(x,y : TptkCoord);
begin
  if WinHandle > 0 then
{$ifdef Win32}
       Windows.SetWindowPos(WinHandle,0,x,y,0,0,SWP_NOZORDER or SWP_NOSIZE or SWP_NOREDRAW);
{$else}
       XMoveWindow(display, WinHandle, x,y);
{$endif}
end;

procedure TptkForm.SetWindowTitle(const AValue: widestring);
var
  p : PByte;
  s8 : string;
begin
  if FWindowTitle=AValue then exit;
  FWindowTitle:=AValue;
  
  if FWinHandle <= 0 then Exit;

  s8 := FWindowTitle;
  
{$ifdef Win32}
  SetWindowText(FWinHandle, PChar(s8));
{$else}
  if length(s8) > 0 then p := @s8[1] else p := nil;
  XChangeProperty(display, FWinHandle, 39, 31, 8, 0, p, length(s8));
  XChangeProperty(display, FWinHandle, 37, 31, 8, 0, p, length(s8));
{$endif}
end;

procedure TptkForm.MsgActivate(var msg : TptkMessageRec);
begin
  //writeln('activate: ',ClassName);
  if (ptkTopModalForm = nil) or (ptkTopModalForm = self) then
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

procedure TptkForm.MsgDeActivate(var msg: TptkMessageRec);
begin
  //writeln('deactivate: ',ClassName);
  if FActiveWidget <> nil then ActiveWidget.KillFocus;
end;

procedure TptkForm.MsgClose(var msg: TptkMessageRec);
begin
  HandleClose;
end;

procedure TptkForm.AdjustWindowStyle(var ws, es: longword; var pwh: TptkWinHandle);
begin
  inherited;
{$ifdef Win32}
  if ptkTopModalForm = self then
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
      FLeft := TptkCoord(CW_USEDEFAULT);
      FTop  := TptkCoord(CW_USEDEFAULT);
    end;
    
    if ptkMainForm <> nil then
    begin
      pwh := ptkMainForm.WinHandle;
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

procedure TptkForm.SetWindowParameters;
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
  pf : TptkForm;
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

  s8 := FWindowTitle;
  
  if length(s8) > 0 then p := @s8[1] else p := nil;

  XChangeProperty(display, FWinHandle, 39, 31, 8, 0, p, length(s8));
  XChangeProperty(display, FWinHandle, 37, 31, 8, 0, p, length(s8));

  // send close event instead of quitting the whole application...
  XSetWMProtocols(display, FWinHandle, @xia_wm_delete_window, 1);

  // for modal windows, this is necessary
  if ptkTopModalForm = self then
  begin
    pf := FParentForm;
    if pf <> nil then XSetTransientForHint(display, self.FWinHandle, pf.WinHandle);
  end;

  //ptkSetWMOptions(FWinHandle,FWMOptions);
end;
{$endif}

constructor TptkForm.Create(AOwner: TComponent);
begin
  FWindowTitle := ClassName;
  FWMOptions := []; //[wmAutoPosition];
  FWindowPosition := wpScreenCenter;

  if ptkMainForm = nil then ptkMainForm := self;

  if (FocusRoot <> nil) and (FocusRoot is TptkForm)
    then FParentForm := TptkForm(FocusRoot)
    else FParentForm := nil;

  FActiveWidget := nil;
  FResizeable := true;

  FPrevModalForm := nil;


  inherited Create(AOwner);
  // do not put anything after inherited !!!!
  // it will call AfterCreate!
end;

procedure TptkForm.Show;
begin
  DoShow;
//  FocusRoot := self;
end;

function TptkForm.ShowModal : integer;
begin
  FPrevModalForm := ptkTopModalForm;
  ptkTopModalForm := self;

  ModalResult := 0;
  
  Show;
  
  // processing messages until this form ends.
  
  // delivering the remaining messages
  ptkDeliverMessages;

  repeat
    ptkWaitWindowMessage;
    ptkDeliverMessages;
  until (ModalResult <> 0) or (not Visible);

  ptkTopModalForm := FPrevModalForm;
  
  Result := ModalResult;
end;

procedure TptkForm.HandleClose;
begin
  Close;
end;

procedure TptkForm.Close;
begin
  Hide;
  if ptkMainForm = self then
  begin
    Halt(0);
  end;
end;

procedure TptkForm.Hide;
begin
  if (ptkTopModalForm = self) then ptkTopModalForm := self.FPrevModalForm;
  DoHide;
  if ModalResult = 0 then ModalResult := -1;
end;

procedure TptkForm.PostKillMe;
begin
  ptkPostMessage(self,self,MSG_KILLME,0,0,0);
end;

procedure TptkForm.SetMinSize;
begin
  FSizeParams.min_width := width;
  FSizeParams.min_height := height;
end;

function TptkForm.GetWindowName: string;
begin
  result := FWindowTitle;
end;

function TptkForm.GetWindowTitle8: string;
begin
  result := WindowTitle;
end;

procedure TptkForm.SetWindowTitle8(const Value: string);
begin
  WindowTitle := Value;
end;

initialization
begin
  ptkMainForm := nil;
  ptkTopModalForm := nil;
end;

end.

