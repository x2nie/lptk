unit lp_widget;

{$include pgf_config.inc}

interface

uses
  Classes, SysUtils,
  lp_defs,
  lp_main
  ;

type
  TFocusSearchDirection = (fsdFirst, fsdLast, fsdNext, fsdPrev);

  { TpgfWidget }

  TpgfWidget = class(TpgfWindow)
  private
    FAlignRect : TpgfRect;

    FOnScreen : boolean;
    FWidgetStyle: TWidgetStyle;
    function GetParentName: string;
    procedure SetActiveWidget(const AValue: TpgfWidget);
    procedure SetEnabled(const AValue: boolean);
    procedure SetVisible(const AValue: boolean);
    function GetWidgets(Index: integer): TpgfWidget;
    procedure SetParent(const Value: TpgfWidget);


  protected
    FParent : TpgfWidget;
    FChilds: TList; // list of TMyWidget

    FVisible : boolean;

    FEnabled : boolean;
    FFocusable : boolean; 
    FFocused   : boolean;
    FTabOrder  : integer;

    FAnchors : TAnchors;

    FActiveWidget : TpgfWidget;

    FAlign : TAlign;
    FText  : widestring;

    procedure DoAlign(aalign : TAlign);
    procedure SetText(AValue: widestring); virtual;

    property Text : widestring read FText write SetText;

  public
    constructor Create(aowner : TComponent); override;
    destructor Destroy; override;

  public
    procedure MsgPaint(var msg : TpgfMessageRec); message PGFM_PAINT;

    procedure MsgResize(var msg : TpgfMessageRec); message PGFM_RESIZE;
    procedure MsgMove(var msg : TpgfMessageRec); message PGFM_MOVE;

    procedure MsgKeyChar(var msg : TpgfMessageRec); message PGFM_KEYCHAR;
    procedure MsgKeyPress(var msg : TpgfMessageRec); message PGFM_KEYPRESS;
    procedure MsgKeyRelease(var msg : TpgfMessageRec); message PGFM_KEYRELEASE;

    procedure MsgMouseDown(var msg : TpgfMessageRec); message PGFM_MOUSEDOWN;
    procedure MsgMouseUp(var msg : TpgfMessageRec); message PGFM_MOUSEUP;
    procedure MsgMouseMove(var msg : TpgfMessageRec); message PGFM_MOUSEMOVE;
    procedure MsgDoubleClick(var msg : TpgfMessageRec); message PGFM_DOUBLECLICK;

    procedure MsgMouseEnter(var msg : TpgfMessageRec); message PGFM_MOUSEENTER;
    procedure MsgMouseExit(var msg : TpgfMessageRec); message PGFM_MOUSEEXIT;

  protected
    procedure HandlePaint; virtual;

    procedure HandleResize(awidth, aheight : TpgfCoord); virtual;

    procedure HandleMove(x, y : TpgfCoord); virtual;

    procedure HandleKeyChar(var keycode: word; var shiftstate: word; var consumed : boolean); virtual;
    procedure HandleKeyPress(var keycode: word; var shiftstate: word; var consumed : boolean); virtual;
    procedure HandleKeyRelease(var keycode: word; var shiftstate: word; var consumed : boolean); virtual;

    procedure HandleSetFocus; virtual;
    procedure HandleKillFocus; virtual;

    procedure HandleLMouseDown(x,y : integer; shiftstate : word); virtual;
    procedure HandleRMouseDown(x,y : integer; shiftstate : word); virtual;
    procedure HandleLMouseUp(x,y : integer; shiftstate : word); virtual;
    procedure HandleRMouseUp(x,y : integer; shiftstate : word); virtual;

    procedure HandleMouseMove(x,y : integer; btnstate : word; shiftstate : word); virtual;
    procedure HandleDoubleClick(x,y : integer; button : word; shiftstate : word); virtual;

    procedure HandleMouseEnter; virtual;
    procedure HandleMouseExit; virtual;
  protected
    {designer need}
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    {designer need}
    property Parent : TpgfWidget read FParent write SetParent;
    function ChildCount: integer;
    property Children[Index: integer]: TpgfWidget read GetWidgets;
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
  public
    function FindFocusWidget(startwg : TpgfWidget; direction : TFocusSearchDirection) : TpgfWidget;
    property WidgetStyle : TWidgetStyle read FWidgetStyle write FWidgetStyle;
  public
    procedure HandleAlignments(dwidth, dheight : TpgfCoord); virtual;
    
    procedure HandleShow; virtual;
    procedure HandleHide; virtual;

    procedure Invalidate; virtual;

    procedure MoveAndResize(aleft, atop, awidth, aheight : TpgfCoord);
    procedure MoveAndResizeBy(dx,dy,dw,dh : TpgfCoord);

    procedure SetPosition(aleft, atop, awidth, aheight : TpgfCoord);

    procedure RePaint;

    procedure SetFocus;
    procedure KillFocus;

  public

    OnKeyPress : TKeyPressNotifyEvent;

  public


    property ActiveWidget : TpgfWidget read FActiveWidget write SetActiveWidget;




    property Focusable : boolean read FFocusable write FFocusable;
    property Focused   : boolean read FFocused write FFocused;



  published
    //debug
    property ParentName : string read GetParentName;

    property Left;
    property Top;
    property Width;
    property Height;
    property Cursor;
    property Align : TAlign read FAlign write FAlign;
    property Anchors : TAnchors read FAnchors write FAnchors;
    property Enabled : boolean read FEnabled write SetEnabled;
    property TabOrder  : integer read FTabOrder write FTabOrder;
    property Visible : boolean read FVisible write SetVisible;
  end;

var
  FocusRootWidget : TpgfWidget;

function FindKeyboardFocus : TpgfWidget;

implementation

uses lp_form;

function FindKeyboardFocus : TpgfWidget;
begin
  Result := nil;

  if FocusRootWidget <> nil then
  begin
    Result := FocusRootWidget;
    while (Result <> nil) and (result.ActiveWidget <> nil) do result := result.ActiveWidget;
  end;
end;


{ TpgfWidget }

procedure TpgfWidget.SetEnabled(const AValue: boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
  RePaint;
end;

procedure TpgfWidget.SetText(AValue: widestring);
begin
  if FText=AValue then Exit;
  FText:=AValue;
end;

procedure TpgfWidget.SetActiveWidget(const AValue: TpgfWidget);
begin
  if FActiveWidget = AValue then exit;

//  if FFormDesigner <> nil then Exit;

  if FActiveWidget <> nil then FActiveWidget.HandleKillFocus;
  FActiveWidget := AValue;
  if FActiveWidget <> nil then FActiveWidget.HandleSetFocus;
end;

function TpgfWidget.GetParentName: string;
begin
  result := 'nil!';
  if hasParent then
     result := Parent.Name;
end;

procedure TpgfWidget.SetVisible(const AValue: boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  
  if FOnScreen then
  begin
    if FVisible then HandleShow
    else
    begin
      HandleHide;
      FOnScreen := True;
    end;
  end;
end;

constructor TpgfWidget.Create(aowner: TComponent);
begin
  FOnScreen := false;

  FVisible := true;

  FActiveWidget := nil;
  FEnabled   := true;

  FFocusable := false;
  FFocused   := false;
  FTabOrder  := 0;

  FAnchors := [anLeft, anTop];

  FAlign := alNone;

  OnKeyPress := nil;

  FParent := nil;
  if (aowner <> nil) and (aowner is TpgfWidget) then
     //FParent := TpgfWidget(aowner);
     SetParent(TpgfWidget(aowner));
    
  if FParent <> nil then
  begin
    FWindowType := wtChild;
  end;
    
  inherited;
  FChilds:=TList.Create;

end;

destructor TpgfWidget.Destroy;
begin
  HandleHide;
  Parent:=nil;
  while ChildCount>0 do Children[ChildCount-1].Free;
  FreeAndNil(FChilds);
  inherited;
end;

procedure TpgfWidget.MsgPaint(var msg: TpgfMessageRec);
begin
  HandlePaint;
end;

procedure TpgfWidget.MsgKeyChar(var msg: TpgfMessageRec);
var
  key, ss : word;
  consumed : boolean;
  wg : TpgfWidget;
begin
{
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
}
  key := msg.params.keyboard.keycode;
  ss := msg.params.keyboard.shiftstate;

  consumed := false;
  HandleKeyChar(key, ss, consumed);

  if not consumed then
  begin
    wg := Parent;
    while (not consumed) and (wg <> nil) do
    begin
      wg.HandleKeyChar(key, ss, consumed);
      wg := wg.Parent;
    end;
  end;
end;

procedure TpgfWidget.MsgKeyPress(var msg: TpgfMessageRec);
var
  key, ss : word;
  consumed : boolean;
  wg : TpgfWidget;
begin
{
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
}
  key := msg.params.keyboard.keycode;
  ss := msg.params.keyboard.shiftstate;

  consumed := false;
  HandleKeyPress(key, ss, consumed);

  if not consumed then
  begin
    wg := Parent;
    while (not consumed) and (wg <> nil) do
    begin
      wg.HandleKeyPress(key, ss, consumed);
      wg := wg.Parent;
    end;
  end;
end;

procedure TpgfWidget.MsgKeyRelease(var msg: TpgfMessageRec);
var
  key, ss : Word;
  consumed : boolean;
  wg : TpgfWidget;
begin
{
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
}
  key := msg.params.keyboard.keycode;
  ss := msg.params.keyboard.shiftstate;
  
  consumed := false;
  HandleKeyRelease(key, ss, consumed);
  
  if not consumed then
  begin
    wg := Parent;
    while (not consumed) and (wg <> nil) do
    begin
      wg.HandleKeyRelease(key, ss, consumed);
      wg := wg.Parent;
    end;
  end;
end;

procedure TpgfWidget.MsgMouseDown(var msg: TpgfMessageRec);
begin
{
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
}
  case msg.Params.mouse.buttons of
    MOUSE_LEFT:  HandleLMouseDown(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);
    MOUSE_RIGHT: HandleRMouseDown(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);
  end;
end;

procedure TpgfWidget.MsgMouseUp(var msg: TpgfMessageRec);
begin
{
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
}
  case msg.Params.mouse.buttons of
    MOUSE_LEFT:  HandleLMouseUp(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);
    MOUSE_RIGHT: HandleRMouseUp(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);
  end;
end;

procedure TpgfWidget.MsgMouseMove(var msg: TpgfMessageRec);
begin
  HandleMouseMove(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.buttons, msg.Params.mouse.shiftstate);
end;

procedure TpgfWidget.MsgDoubleClick(var msg: TpgfMessageRec);
begin

end;

procedure TpgfWidget.MsgMouseEnter(var msg: TpgfMessageRec);
begin
{
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
}
  HandleMouseEnter;
end;

procedure TpgfWidget.MsgMouseExit(var msg: TpgfMessageRec);
begin
{
  if FFormDesigner <> nil then
  begin
    FFormDesigner.Dispatch(msg);
    Exit;
  end;
}
  HandleMouseExit;
end;

procedure TpgfWidget.HandleShow;
var
  n : integer;
  c : TComponent;
begin
  FOnScreen := True;
  if FVisible then
  begin
    FParentWindow := FParent;
    AllocateWindowHandle;

    for n := 0 to ComponentCount-1 do
    begin
      c := Components[n];
      if (c is TpgfWidget) and (TpgfWidget(c).Parent = self) then TpgfWidget(c).HandleShow;
    end;
  end;
end;

procedure TpgfWidget.HandleHide;
var
  n : integer;
  c : TComponent;
begin
  for n := 0 to ComponentCount-1 do
  begin
    c := Components[n];
    if (c is TpgfWidget) and (TpgfWidget(c).Parent = self) then TpgfWidget(c).HandleHide;
  end;
  FOnScreen := False;
  
  ReleaseWindowHandle;
end;

procedure TpgfWidget.RePaint;
begin
  if HasHandle then HandlePaint;
end;

procedure TpgfWidget.SetFocus;
begin
  HandleSetFocus;
end;

procedure TpgfWidget.KillFocus;
begin
  HandleKillFocus;
end;

procedure TpgfWidget.HandlePaint;
begin
  //
end;

procedure TpgfWidget.HandleKeyPress(var keycode: word; var shiftstate: word;
  var consumed: boolean);
begin
  // nothing yet.
end;

procedure TpgfWidget.HandleKeyChar(var keycode: word; var shiftstate: word; var consumed: boolean);
var
  wg : TpgfWidget;
  dir : integer;
begin
//  Writeln('keychar: ',self.classname);

  if Assigned(OnKeyPress) then OnKeyPress(self, keycode, shiftstate, consumed);

  if consumed then Exit;

  dir := 0;

  case keycode of
    KEY_TAB: if (shiftstate and ss_shift) <> 0 then dir := -1 else dir := 1;

    KEY_ENTER, KEY_DOWN, KEY_RIGHT: dir := 1;

    KEY_UP, KEY_LEFT: dir := -1;
  end;

  if dir = 1 then
  begin
    // forward
    wg := FindFocusWidget(ActiveWidget,fsdNext);
    ActiveWidget := wg;
    if wg <> nil then
    begin
      consumed := true;
    end
    else
    begin
      if Parent = nil then
      begin
        wg := FindFocusWidget(ActiveWidget,fsdFirst);
        ActiveWidget := wg;
        consumed := true;
      end;
    end;
  end
  else if dir = -1 then
  begin
    // backward
    wg := FindFocusWidget(ActiveWidget,fsdPrev);
    ActiveWidget := wg;
    if wg <> nil then
    begin
      consumed := true;
      // we must find the last one!
      while wg <> nil do
      begin
        wg.ActiveWidget := wg.FindFocusWidget(ActiveWidget,fsdLast);
        wg := wg.ActiveWidget;
      end;
    end
    else
    begin
      if Parent = nil then
      begin
        wg := FindFocusWidget(ActiveWidget,fsdLast);
        ActiveWidget := wg;
        consumed := true;
      end;
    end;
  end;
end;

procedure TpgfWidget.HandleKeyRelease(var keycode: word; var shiftstate: word; var consumed: boolean);
begin
  // nothing yet.
end;

procedure TpgfWidget.HandleSetFocus;
var
  awg : TpgfWidget;
begin
  //Writeln('DoSetFocus: ',self.ClassName);

  if not FFocused then
  begin
    FFocused := true;

    RePaint;

    // focusing a child

    if ActiveWidget <> nil then
    begin
      ActiveWidget.SetFocus;
    end
    else
    begin
      // try to find it for the first time.
      awg := FindFocusWidget(nil, fsdFirst);
      if awg <> nil then ActiveWidget := awg;
    end;
  end;

  if Parent <> nil then
  begin
    Parent.ActiveWidget := self;
    Parent.SetFocus;
  end;

end;

procedure TpgfWidget.HandleKillFocus;
begin
  FFocused := false;
  RePaint;

  if ActiveWidget <> nil then ActiveWidget.KillFocus;
end;

procedure TpgfWidget.HandleLMouseDown(x, y: integer; shiftstate: word);
var
  pw : TpgfWidget;
  w : TpgfWidget;
begin
  // setting the focus trough all parents
  pw := Parent;
  w := self;
  while pw <> nil do
  begin
    if w.Visible and w.Enabled and w.Focusable then
       pw.ActiveWidget := w;
    w := pw;
    pw := pw.Parent;
  end;
end;

procedure TpgfWidget.HandleRMouseDown(x, y: integer; shiftstate: word);
begin
  //
end;

procedure TpgfWidget.HandleLMouseUp(x, y: integer; shiftstate: word);
begin
  //
end;

procedure TpgfWidget.HandleRMouseUp(x, y: integer; shiftstate: word);
begin
  //
end;

procedure TpgfWidget.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: word);
begin
  //
end;

procedure TpgfWidget.HandleDoubleClick(x, y: integer; button: word; shiftstate: word);
begin
  //
end;

procedure TpgfWidget.HandleMouseEnter;
begin
  //
end;

procedure TpgfWidget.HandleMouseExit;
begin
  //
end;

procedure TpgfWidget.SetName(const NewName: TComponentName);
var
  ChangeText: Boolean;
begin
  if Name=NewName then exit;
  ChangeText :=
    {(csSetCaption in ControlStyle) and} not (csLoading in ComponentState) and
    (Name = Text) and
    ((Owner = nil) or not (Owner is TpgfWidget) or not (csLoading in TpgfWidget(Owner).ComponentState));
  inherited SetName(NewName);
  if ChangeText then Text := NewName;
end;

function TpgfWidget.FindFocusWidget(startwg: TpgfWidget; direction: TFocusSearchDirection): TpgfWidget;
var
  w : TpgfWidget;
  n : integer;
  FoundIt : boolean;
  lasttaborder : integer;
begin
  result := nil;
  FoundIt := false;
  if direction in [fsdLast,fsdPrev] then lasttaborder := -999999
                                    else lasttaborder := 999999;

  for n := 0 to ComponentCount-1 do
  begin
    if Components[n] is TpgfWidget then
    begin
      w := TpgfWidget(Components[n]);
      
      if w.Visible and w.Enabled and w.Focusable then
      begin
        case direction of
          fsdFirst:
            begin
              if w.TabOrder < lasttaborder then
              begin
                Result := w;
                lasttaborder := w.TabOrder;
              end;
            end;

          fsdLast:
            begin
              if lasttaborder <= w.TabOrder then
              begin
                Result := w;
                lasttaborder := w.TabOrder;
              end;
            end;

          fsdNext:
            begin
              if startwg = w then FoundIt := true
              else if w.TabOrder < lasttaborder then
              begin
                if (startwg = nil) or
                   (w.TabOrder > startwg.TabOrder) or
                   (FoundIt and (w.TabOrder = startwg.TabOrder)) then
                begin
                  result := w;
                  lasttaborder := w.TabOrder;
                end;
              end;
            end;

          fsdPrev:
            begin
              if startwg = w then FoundIt := true
              else if w.TabOrder >= lasttaborder then
              begin
                if (startwg = nil) or
                   (w.TabOrder < startwg.TabOrder) or
                   (not FoundIt and (w.TabOrder = startwg.TabOrder)) then
                begin
                  result := w;
                  lasttaborder := w.TabOrder;
                end;
              end;

            end;

        end;
      end;
    end;
  end;
end;

procedure TpgfWidget.MsgResize(var msg: TpgfMessageRec);
begin
  HandleResize(msg.Params.rect.Width, msg.params.rect.Height);
end;

procedure TpgfWidget.HandleResize(awidth, aheight: TpgfCoord);
var
  dw, dh : integer;
begin
  dw := awidth - FWidth;
  dh := aheight - FHeight;

  //Writeln('resize: ',dw,',',dh);

  FWidth := awidth;
  FHeight := aheight;

  HandleAlignments(dw,dh);
end;

procedure TpgfWidget.MsgMove(var msg: TpgfMessageRec);
begin
  HandleMove(msg.Params.rect.left, msg.Params.rect.top);
end;

procedure TpgfWidget.HandleMove(x, y: TpgfCoord);
//var
//  dx, dy : integer;
begin
//  dx := x - FLeft;
//  dy := y - FTop;

  //Writeln('Handlemove: ',x,',',y);

  FLeft := x;
  FTop  := y;
end;

procedure TpgfWidget.HandleAlignments(dwidth, dheight: TpgfCoord);
var
  n : integer;
  wg : TpgfWidget;
  dx,dy,dw,dh : integer;
begin
  FAlignRect.top  := 0;
  FAlignRect.Left := 0;
  FAlignRect.width  := width;
  FAlignRect.height := Height;

  DoAlign(alTop);
  DoAlign(alBottom);
  DoAlign(alLeft);
  DoAlign(alRight);
  DoAlign(alClient);

  // handle anchors finally for alNone
  for n:=0 to ComponentCount-1 do
  begin
    if (Components[n] is TpgfWidget) then
    begin
      wg := TpgfWidget(Components[n]);

      if (wg.FAlign = alNone) and
         (anBottom in wg.Anchors) or (anRight in wg.Anchors) then
      begin
        // we must alter the window
        dx := 0; dy := 0; dw := 0; dh := 0;

        if (anLeft in wg.Anchors) and (anRight in wg.Anchors) then
        begin
          dw := dwidth;
        end else if anRight in wg.Anchors then
        begin
          dx := dwidth;
        end;

        if (anTop in wg.Anchors) and (anBottom in wg.Anchors) then
        begin
          dh := dheight;
        end else if anBottom in wg.Anchors then
        begin
          dy := dheight;
        end;

        wg.MoveAndResizeBy(dx,dy,dw,dh);
      end;
    end;
  end;

  RePaint;
end;

procedure TpgfWidget.MoveAndResize(aleft, atop, awidth, aheight: TpgfCoord);
begin
  if (aleft <> FLeft) or (atop <> FTop) then
  begin
    HandleMove(aleft, atop);
  end;
  if (awidth <> FWidth) or (aheight <> FHeight) then
  begin
    HandleResize(awidth, aheight);
  end;
  
  UpdateWindowPosition;
end;

procedure TpgfWidget.MoveAndResizeBy(dx, dy, dw, dh: TpgfCoord);
begin
  if (dx <> 0) or (dy <> 0) or
     (dw <> 0) or (dh <> 0) then
  begin
    MoveAndResize(FLeft + dx, FTop + dy, FWidth + dw, FHeight + dh);
  end;
end;

function CompareInts(i1,i2 : integer) : integer;
begin
  if i1 < i2 then result := -1
  else if i1 > i2 then result := 1
  else result := 0;
end;

function AlignCompare(p1,p2 : pointer) : integer;
var
  w1,w2 : TpgfWidget;
begin
  w1 := TpgfWidget(p1);
  w2 := TpgfWidget(p2);
  case w1.Align of
  alTop:     result := CompareInts(w1.Top, w2.Top);
  alBottom:  result := CompareInts(w2.Top, w1.Top);
  alLeft:    result := CompareInts(w1.Left, w2.Left);
  alRight:   result := CompareInts(w2.Left, w1.Left);
  else
    result := 0;
  end;
end;

procedure TpgfWidget.DoAlign(aalign: TAlign);
var
  alist : TList;
  w : TpgfWidget;
  n : integer;
begin
  alist := TList.Create;
  for n := 0 to ComponentCount-1 do
  begin
    if Components[n] is TpgfWidget then
    begin
      w := TpgfWidget(Components[n]);
      if w.Align = aalign then alist.Add(w);
    end;
  end;

  alist.Sort(@AlignCompare);

  // and process this list in order
  for n := 0 to alist.Count-1 do
  begin
    w := TpgfWidget(alist[n]);
    case aalign of

    alTop:
      begin
        w.MoveAndResize(FAlignRect.Left,FAlignRect.Top,FAlignRect.Width, w.Height);
        inc(FAlignRect.top, w.Height);
        dec(FAlignRect.height, w.Height);
      end;

    alBottom:
      begin
        w.MoveAndResize(FAlignRect.Left,FAlignRect.Top+FAlignRect.Height-w.Height, FAlignRect.Width, w.Height);
        dec(FAlignRect.height, w.Height);
      end;

    alLeft:
      begin
        w.MoveAndResize(FAlignRect.Left,FAlignRect.Top, w.Width, FAlignRect.Height);
        inc(FAlignRect.Left, w.Width);
        dec(FAlignRect.Width, w.Width);
      end;

    alRight:
      begin
        w.MoveAndResize(FAlignRect.Left+FAlignRect.Width-w.Width, FAlignRect.Top, w.Width, FAlignRect.Height);
        dec(FAlignRect.Width, w.Width);
      end;

    alClient:
      begin
        w.MoveAndResize(FAlignRect.Left, FAlignRect.Top, FAlignRect.Width, FAlignRect.Height);
      end;

    end; // case
  end;

  alist.Free;
end;

procedure TpgfWidget.SetPosition(aleft, atop, awidth, aheight: TpgfCoord);
begin
  MoveAndResize(aleft, atop, awidth, aheight);
end;


procedure TpgfWidget.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
  OwnedComponent: TComponent;
begin
  {for i := 0 to ComponentCount-1 do
  begin
    if Components[i].Owner=Root then
      Proc(Components[i]);
  end;}
  //inherited GetChildren(Proc, Root);
  {if Root = Self then begin
    for I := 0 to ComponentCount - 1 do
    begin
      OwnedComponent := Components[I];
      if not OwnedComponent.HasParent then Proc(OwnedComponent);
    end;
  end;}
  {for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if Control.Owner = Root then Proc(Control);
  end;]}
  for i:=0 to ChildCount-1 do
      if Children[i].Owner=Root then
        Proc(Children[i]);

  if Root = Self then
    for I := 0 to ComponentCount - 1 do
    begin
      OwnedComponent := Components[I];
      if not OwnedComponent.HasParent then Proc(OwnedComponent);
    end;
  {
  if self is TMyForm then
  begin
    for i := 0 to ComponentCount-1 do
        if not (Components[i] is TpgfWidget) then
           Proc(Components[i])
  end}
end;

procedure TpgfWidget.SetParentComponent(Value: TComponent);
begin
  if Value is TpgfWidget then
    Parent:=TpgfWidget(Value);
end;

function TpgfWidget.GetParentComponent: TComponent;
begin
  result := Parent;
end;

function TpgfWidget.HasParent: Boolean;
begin
  Result:=Parent<>nil;
end;

function TpgfWidget.ChildCount: integer;
begin
  Result:=FChilds.Count;
end;


function TpgfWidget.GetWidgets(Index: integer): TpgfWidget;
begin
  Result:=TpgfWidget(FChilds[Index]);
end;

procedure TpgfWidget.SetParent(const Value: TpgfWidget);
begin
if FParent=Value then exit;
  if FParent<>nil then begin
    Invalidate;
    FParent.FChilds.Remove(Self);
  end;
  FParent:=Value;
  if FParent<>nil then begin
    FParent.FChilds.Add(Self);
  end;
  Invalidate;
end;

procedure TpgfWidget.Invalidate;
begin

end;

initialization
begin
  FocusRootWidget := nil;
end;

end.

