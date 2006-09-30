{ $Id$ }

// LPTK internal message queue implementation

type
  TMessageListElement = class
  protected
    Next : TMessageListElement;
    Prev : TMessageListElement;
  public
    msg  : TptkMessageRec;
  end;

  PValidDest = ^ValidDestItem;
  ValidDestItem = record
    ValidObj : TObject;
    Next : PValidDest;
  end;

var
  UsedFirstMessage,
  UsedLastMessage,
  FreeFirstMessage,
  FreeLastMessage    : TMessageListElement;

  FirstValidItem : PValidDest;
  LastValidItem  : PValidDest;

procedure ptkRegisterValidMsgDest(obj : TObject);
var
  p : PValidDest;
begin
  if not ptkValidateMsgDest then Exit;

  New(p);
  p^.ValidObj := obj;
  p^.Next := nil;
  if FirstValidItem = nil then FirstValidItem := p
                     else LastValidItem^.Next := p;
  LastValidItem := p;
end;

procedure ptkUnRegisterValidMsgDest(obj : TObject);
var
  prevp, p, px : PValidDest;
begin
  if not ptkValidateMsgDest then Exit;

  p := FirstValidItem;
  prevp := nil;

  while p <> nil do
  begin
    if p^.ValidObj = obj then
    begin
      if prevp = nil then FirstValidItem := p^.Next
                     else prevp^.Next := p^.Next;
      if LastValidItem = p then LastValidItem := prevp;
      px := p;
      p := p^.Next;
      Dispose(px);
    end
    else
    begin
      prevp := p;
      p := p^.Next;
    end;
  end;
end;

function ptkMsgDestIsValid(obj : TObject; const msg : TptkMessageRec) : boolean;
var
  p : PValidDest;
begin
  p := FirstValidItem;
  while p <> nil do
  begin
    if p^.ValidObj = obj then
    begin
      Result := True;
      Exit;
    end;
    p := p^.Next;
  end;

  Writeln('INVALID MESSAGE DESTINATION! (msg=',msg.MsgCode,' p1=',msg.Param1,' p2=',msg.Param2, ' p3=',msg.Param3,')');

  result := False;
end;


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

function ptkAllocateMessage : PptkMessageRec;
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

procedure ptkDeleteFirstMessage;
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

function ptkGetFirstMessage : PptkMessageRec;
begin
  if UsedFirstMessage <> nil then Result := @(UsedFirstMessage.msg)
                             else Result := nil;
end;

procedure ptkInitMsgQueue;
var
  n : integer;
  e : TMessageListElement;
begin
  for n := 1 to ptkMessageQueueSize do
  begin
    e := TMessageListElement.Create;
    MsgListInsertElement(e,FreeFirstMessage,FreeLastMessage);
  end;
end;

procedure ptkPostMessage(Sender, Dest : TObject; MsgCode : integer; Param1, Param2, Param3 : integer);
var
  p : PptkMessageRec;
begin
  if Dest = nil then Exit;

  p := ptkAllocateMessage;
  if p <> nil then
  begin
    p^.MsgCode := MsgCode;
    p^.Sender  := Sender;
    p^.Dest    := Dest;
    p^.Param1  := Param1;
    p^.Param2  := Param2;
    p^.Param3  := Param3;
  end
  else
  begin
    Writeln('THE MESSAGE QUEUE IS FULL.');
  end;
end;

procedure ptkSendMessage(Sender, Dest : TObject; MsgCode : integer; Param1, Param2, Param3 : integer);
var
  m : TptkMessageRec;
begin
  if Dest = nil then Exit;

  m.MsgCode := MsgCode;
  m.Sender  := Sender;
  m.Dest    := Dest;
  m.Param1  := Param1;
  m.Param2  := Param2;
  m.Param3  := Param3;

  if not ptkValidateMsgDest or ptkMsgDestIsValid(m.Dest,m) then
  begin
    m.Dest.Dispatch(m)
  end;
end;


procedure ptkDeliverMessage(var msg : TptkMessageRec);
begin
  if not ptkValidateMsgDest or ptkMsgDestIsValid(msg.Dest,msg) then
  begin
    if msg.MsgCode = MSG_KILLME then
    begin
      writeln('Killing...');
      msg.Dest.Free
    end
    else
    begin
      msg.Dest.Dispatch(msg);
    end;
  end;
end;

procedure ptkDeliverMessages;
var
  mp : PptkMessageRec;
  m : TptkMessageRec;
begin
  repeat
    mp := ptkGetFirstMessage;
    if mp <> nil then
    begin
      m := mp^;
      ptkDeleteFirstMessage;

      ptkDeliverMessage(m);
    end;
  until mp = nil;
end;

