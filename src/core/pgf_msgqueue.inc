// PGF internal message queue implementation

type
  TMessageListElement = class
  protected
    Next : TMessageListElement;
    Prev : TMessageListElement;
  public
    msg  : TpgfMessageRec;
  end;

var
  UsedFirstMessage,
  UsedLastMessage,
  FreeFirstMessage,
  FreeLastMessage    : TMessageListElement;

procedure MsgListInsertElement(Elem : TMessageListElement; var First : TMessageListElement; var Last : TMessageListElement);
begin
  Elem.Prev := nil;
  Elem.Next := nil;

  if First = nil then
  begin
    First := Elem;
    Last  := Elem;
  end
  else
  begin
    Last.Next := Elem;
    Elem.Prev := Last;
    Last := Elem;
  end;
end;

procedure MsgListRemoveElement(Elem : TMessageListElement; var First : TMessageListElement; var Last : TMessageListElement);
begin
  if Elem = First then First := Elem.Next;
  if Elem = Last  then Last  := Elem.Prev;
  if Elem.Next <> nil then Elem.Next.Prev := Elem.Prev;
  if Elem.Prev <> nil then Elem.Prev.Next := Elem.Next;
end;

function pgfAllocateMessage : PpgfMessageRec;
var
  e : TMessageListElement;
begin
  e := FreeFirstMessage;
  if e <> nil then
  begin
    MsgListRemoveElement(e, FreeFirstMessage, FreeLastMessage);
    MsgListInsertElement(e, UsedFirstMessage, UsedLastMessage);
    result := @(e.msg);
  end
  else result := nil;
end;

procedure pgfDeleteFirstMessage;
var
  e : TMessageListElement;
begin
  e := UsedFirstMessage;
  if e <> nil then
  begin
    MsgListRemoveElement(e, UsedFirstMessage, UsedLastMessage);
    MsgListInsertElement(e, FreeFirstMessage, FreeLastMessage);
  end;
end;

function pgfGetFirstMessage : PpgfMessageRec;
begin
  if UsedFirstMessage <> nil then Result := @(UsedFirstMessage.msg)
                             else Result := nil;
end;

procedure pgfInitMsgQueue;
var
  n : integer;
  e : TMessageListElement;
begin
  UsedFirstMessage := nil;
  UsedLastMessage  := nil;
  FreeFirstMessage := nil;
  FreeLastMessage  := nil;

  for n := 1 to pgfMessageQueueSize do
  begin
    e := TMessageListElement.Create;
    MsgListInsertElement(e,FreeFirstMessage,FreeLastMessage);
  end;
end;

procedure pgfPostMessage(Sender, Dest : TObject; MsgCode : integer; var aparams : TpgfMessageParams); overload;
var
  p : PpgfMessageRec;
begin
  if Dest = nil then Exit;

  p := pgfAllocateMessage;
  if p <> nil then
  begin
    p^.MsgCode := MsgCode;
    p^.Sender  := Sender;
    p^.Dest    := Dest;
    p^.Params  := aparams;
  end
  else
  begin
    Writeln('THE MESSAGE QUEUE IS FULL.');
  end;
end;

procedure pgfPostMessage(Sender, Dest : TObject; MsgCode : integer); overload;
var
  p : PpgfMessageRec;
begin
  if Dest = nil then Exit;

  p := pgfAllocateMessage;
  if p <> nil then
  begin
    p^.MsgCode := MsgCode;
    p^.Sender  := Sender;
    p^.Dest    := Dest;
  end
  else
  begin
    Writeln('THE MESSAGE QUEUE IS FULL.');
  end;
end;


procedure pgfSendMessage(Sender, Dest : TObject; MsgCode : integer; var aparams : TpgfMessageParams); overload;
var
  m : TpgfMessageRec;
begin
  if Dest = nil then Exit;

  m.MsgCode := MsgCode;
  m.Sender  := Sender;
  m.Dest    := Dest;
  m.Params  := aparams;

  m.Dest.Dispatch(m)
end;

procedure pgfSendMessage(Sender, Dest : TObject; MsgCode : integer); overload;
var
  m : TpgfMessageRec;
begin
  if Dest = nil then Exit;

  m.MsgCode := MsgCode;
  m.Sender  := Sender;
  m.Dest    := Dest;

  m.Dest.Dispatch(m)
end;


procedure pgfDeliverMessage(var msg : TpgfMessageRec);
begin
  if msg.MsgCode = PGFM_KILLME then
  begin
    //writeln('Killing...');
    msg.Dest.Free
  end
  else
  begin
    msg.Dest.Dispatch(msg);
  end;
end;

procedure pgfDeliverMessages;
var
  mp : PpgfMessageRec;
  m : TpgfMessageRec;
begin
  repeat
    mp := pgfGetFirstMessage;
    if mp <> nil then
    begin
      m := mp^;
      pgfDeleteFirstMessage;

      pgfDeliverMessage(m);
    end;
  until mp = nil;
end;

