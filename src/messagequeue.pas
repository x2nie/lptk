{ messagequeue.pas: Message queue handling. Heavy use on unix, rare on Windows.
  The solution avoids memory allocation/deallocation for messages.
  File maintainer: nvitya@freemail.hu

History:
}
unit messagequeue;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils;

const
  KMessageQueueSize = 512;

const
  MSG_KILLME = 9999;

type
  TMessageRec = record
    MsgCode : integer;
    Sender  : TObject;
    Dest    : TObject;
    Param1  : integer;
    Param2  : integer;
    Param3  : integer;
  end;
  PMessageRec = ^TMessageRec;
  
var
  ValidateMessageDest : boolean;

procedure PostMessage(Sender, Dest : TObject; MsgCode : integer; Param1, Param2, Param3 : integer);
procedure SendMessage(Sender, Dest : TObject; MsgCode : integer; Param1, Param2, Param3 : integer);

procedure DeliverMessage(var msg : TMessageRec);
procedure DeliverMessages;

// Destination address validation:
procedure RegisterValidDest(obj : TObject);
procedure UnRegisterValidDest(obj : TObject);

//function AllocateMessage : PMessageRec;

function GetFirstMessage : PMessageRec;
procedure DeleteFirstMessage;

implementation

type
  TMessageListElement = class
  protected
    Next : TMessageListElement;
    Prev : TMessageListElement;
  public
    msg  : TMessageRec;
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

procedure RegisterValidDest(obj : TObject);
var
  p : PValidDest;
begin
  if not ValidateMessageDest then Exit;
  
  New(p);
  p^.ValidObj := obj;
  p^.Next := nil;
  if FirstValidItem = nil then FirstValidItem := p
                     else LastValidItem^.Next := p;
  LastValidItem := p;
end;

procedure UnRegisterValidDest(obj : TObject);
var
  prevp, p, px : PValidDest;
begin
  if not ValidateMessageDest then Exit;
  
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

function DestIsValid(obj : TObject; const msg : TMessageRec) : boolean;
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


procedure InsertElement(Elem : TMessageListElement; var First : TMessageListElement; var Last : TMessageListElement);
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

procedure RemoveElement(Elem : TMessageListElement; var First : TMessageListElement; var Last : TMessageListElement);
begin
  if Elem = First then First := Elem.Next;
  if Elem = Last  then Last  := Elem.Prev;
  if Elem.Next <> nil then Elem.Next.Prev := Elem.Prev;
  if Elem.Prev <> nil then Elem.Prev.Next := Elem.Next;
end;

function AllocateMessage : PMessageRec;
var
  e : TMessageListElement;
begin
  e := FreeFirstMessage;
  if e <> nil then
  begin
    RemoveElement(e, FreeFirstMessage, FreeLastMessage);
    InsertElement(e, UsedFirstMessage, UsedLastMessage);
    result := @(e.msg);
  end
  else result := nil;
end;

procedure DeleteFirstMessage;
var
  e : TMessageListElement;
begin
  e := UsedFirstMessage;
  if e <> nil then
  begin
    RemoveElement(e, UsedFirstMessage, UsedLastMessage);
    InsertElement(e, FreeFirstMessage, FreeLastMessage);
  end;
end;

function GetFirstMessage : PMessageRec;
begin
  if UsedFirstMessage <> nil then Result := @(UsedFirstMessage.msg)
                             else Result := nil;
end;

procedure InitQueue;
var
  n : integer;
  e : TMessageListElement;
begin
  for n := 1 to KMessageQueueSize do
  begin
    e := TMessageListElement.Create;
    InsertElement(e,FreeFirstMessage,FreeLastMessage);
  end;
end;

procedure PostMessage(Sender, Dest : TObject; MsgCode : integer; Param1, Param2, Param3 : integer);
var
  p : PMessageRec;
begin
  if Dest = nil then Exit;

  p := AllocateMessage;
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

procedure SendMessage(Sender, Dest : TObject; MsgCode : integer; Param1, Param2, Param3 : integer);
var
  m : TMessageRec;
begin
  if Dest = nil then Exit;

  m.MsgCode := MsgCode;
  m.Sender  := Sender;
  m.Dest    := Dest;
  m.Param1  := Param1;
  m.Param2  := Param2;
  m.Param3  := Param3;

  if not ValidateMessageDest or DestIsValid(m.Dest,m) then
  begin
    m.Dest.Dispatch(m)
  end;
end;


procedure DeliverMessage(var msg : TMessageRec);
begin
  if not ValidateMessageDest or DestIsValid(msg.Dest,msg) then
  begin
    if msg.MsgCode = MSG_KILLME then
    begin
      writeln('Killing...');
      msg.Dest.Free
    end
    else
        msg.Dest.Dispatch(msg)
  end;
end;

procedure DeliverMessages;
var
  mp : PMessageRec;
  m : TMessageRec;
begin
  repeat
    mp := GetFirstMessage;
    if mp <> nil then
    begin
      m := mp^;
      DeleteFirstMessage;
      
      DeliverMessage(m);
    end;
  until mp = nil;
end;


initialization
begin
  UsedFirstMessage := nil;
  UsedLastMessage  := nil;
  FreeFirstMessage := nil;
  FreeLastMessage  := nil;
  ValidateMessageDest := True;
  InitQueue;
end;

finalization
begin
  //
end;


end.

