{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

unit hd_utils;

interface

uses
  SysUtils, Classes;

type
  {$WARNINGS OFF}

  TWideStrings = class(TStrings)
  private
    FUpdateCount: Integer;
    function GetCommaText: WideString;
    function GetName(Index: Integer): WideString;
    function GetValue(const Name: WideString): WideString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: WideString);
    procedure SetValue(const Name, Value: WideString);
    procedure WriteData(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: string; Data: Integer);
    function Get(Index: Integer): WideString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: WideString; virtual;
    procedure Put(Index: Integer; const S: WideString); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: WideString); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
  public
    constructor Create;
    function Add(const S: WideString): Integer; virtual;
    function AddObject(const S: WideString; AObject: TObject): Integer; virtual;
    procedure Append(const S: WideString);
    procedure AddStrings(aStrings: TWideStrings); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(aStrings: TWideStrings): Boolean;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetText: PWideChar; virtual;
    function IndexOf(const S: WideString): Integer; virtual;
    function IndexOfName(const Name: WideString): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: WideString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: WideString; AObject: TObject);
    procedure LoadFromFile(const FileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(aText: PWideChar); virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: WideString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: WideString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property Strings[Index: Integer]: WideString read Get write Put; default;
    property Text: WideString read GetTextStr write SetTextStr;
  end;

{ TWideStringList class }

  PWideStringItem = ^TWideStringItem;
  TWideStringItem = record
    FString: WideString;
    FObject: TObject;
  end;

  PWideStringItemList = ^TWideStringItemList;
  TWideStringItemList = array[0..MaxListSize] of TWideStringItem;

  TWideStringList = class(TWideStrings)
  private
    FList: PStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer);
    procedure InsertItem(Index: Integer; const S: WideString);
    procedure SetSorted(Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): WideString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: WideString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    destructor Destroy; override;
    function Add(const S: WideString): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: WideString; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: WideString): Integer; override;
    procedure Insert(Index: Integer; const S: WideString); override;
    procedure Sort; virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  {$WARNINGS ON}


function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
function WideExtractQuotedStr(var S: WideString; Quote: WideChar): WideString;

implementation

uses
  RTLConsts;

const
  BOM: Word = $FFFE; // Byte Order Mark

function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
var
  slen : integer;
  i : integer;
begin
  result := Quote + S + Quote;
  slen := length(result);
  i := 2;
  while i < slen do
  begin
    if result[i] = quote then
    begin
      insert(quote,result,i);
      inc(i);
      inc(slen);
    end;
    inc(i);
  end;
end;

function WideExtractQuotedStr(var S: widestring; Quote: WideChar): WideString;
var
  i : integer;
  slen : integer;
begin
  if length(s) < 2 then result := s
  else
  begin
    result := copy(s,2,length(s)-2);
    slen := length(result);
    i := 1;
    while i < slen do
    begin
      if (result[i] = quote) and (result[i+1] = quote) then
      begin
        delete(result,i,1);
        dec(slen);
      end;
      inc(i);
    end;
  end;
end;

function WideCompareText(W1, W2: WideString): Integer;
begin
  if W1 < W2 then result := -1
  else if W1 > W2 then result := 1
  else result := 0;
end;


constructor TWideStrings.Create;
begin
  inherited;
end;

function TWideStrings.Add(const S: WideString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TWideStrings.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TWideStrings.Append(const S: WideString);
begin
  Add(S);
end;

procedure TWideStrings.AddStrings(aStrings: TWideStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to aStrings.Count - 1 do
      AddObject(aStrings[I], aStrings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TWideStrings then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TWideStrings(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end
  else if Source is TStrings then
  begin
    BeginUpdate;
    try
      for I := 0 to TStrings(Source).Count - 1 do
        AddObject(TStrings(Source)[I], TStrings(Source).Objects[I]);
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TWideStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TWideStrings.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TWideStrings then
        Result := not Equals(TWideStrings(Filer.Ancestor))
    end
    else Result := Count > 0;
  end;

begin
  Filer.DefineProperty('WideStrings', ReadData, WriteData, DoWrite);
end;

procedure TWideStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

function TWideStrings.Equals(aStrings: TWideStrings): Boolean;
var
  I, cnt: Integer;
begin
  Result := False;
  cnt := GetCount;
  if cnt <> aStrings.GetCount then Exit;
  for I := 0 to cnt - 1 do if Get(I) <> aStrings.Get(I) then Exit;
  Result := True;
end;

procedure TWideStrings.Error(const Msg: string; Data: Integer);
{
  function ReturnAddr: Pointer; assembler;
  asm
          MOV     EAX,[EBP+4]
  end;
}
begin
  raise EStringListError.CreateFmt(Msg, [Data]) { at ReturnAddr};
end;

procedure TWideStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TWideStrings.GetCapacity: Integer;
begin  // descendants may optionally override/replace this default implementation
  Result := Count;
end;


function TWideStrings.GetCommaText: WideString;
var
  S: WideString;
  P: PWideChar;
  I, cnt: Integer;
begin
  cnt := GetCount;
  if (cnt = 1) and (Get(0) = '') then Result := '""'
  else
  begin
    Result := '';
    for I := 0 to cnt - 1 do
    begin
      S := Get(I);
      P := PWideChar(S);
      while not (P^ in [WideChar(#0)..WideChar(' '),WideChar('"'),WideChar(',')]) do inc(P);
      if (P^ <> #0) then S := WideQuotedStr(S, '"');
      
      if I > 0 then result := result + ',';
      Result := Result + S;
    end;
  end;
end;


function TWideStrings.GetName(Index: Integer): WideString;
var
  P: Integer;
begin
  Result := Get(Index);
  P:= 1;
  while Result[P]<>'=' do
    Inc(P);
  if P <> 0 then
    SetLength(Result, P-1) else
    SetLength(Result, 0);
end;

function TWideStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TWideStrings.GetText: PWideChar;
var
  TempStr: WideString;
begin
  TempStr:= GetTextStr;
  Result := AllocMem(2*Length(TempStr)+10);
  System.Move(TempStr[1], Result^, 2*Length(TempStr)+2);
end;

function TWideStrings.GetTextStr: WideString;
var
  I, L, Size, cnt: Integer;
  P: PWideChar;
  S: WideString;
begin
  cnt := GetCount;
  Size := 0;
  for I := 0 to cnt - 1 do Inc(Size, Length(Get(I)) + 2);
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to cnt - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L*2);
      Inc(P, L);
    end;
    P^ := #13;
    Inc(P);
    P^ := #10;
    Inc(P);
  end;
end;

function TWideStrings.GetValue(const Name: WideString): WideString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TWideStrings.IndexOf(const S: WideString): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if Get(Result) = S then Exit;
  Result := -1;
end;

function TWideStrings.IndexOfName(const Name: WideString): Integer;
var
  P: Integer;
  S: string;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P:= 1;
    
    while S[P]<>'=' do Inc(P);
    
    if (P <> 0) and (Copy(S, 1, P - 1) = Name) then Exit;
  end;
  Result := -1;
end;

function TWideStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TWideStrings.InsertObject(Index: Integer; const S: WideString;
  AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TWideStrings.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TWideStrings.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S: WideString;
  Reverse: Boolean;
  rBOM: Word;
  I: Integer;
begin
  BeginUpdate;
  try
    rBOM := 0;
    Stream.Read(rBOM, 2);
    Reverse:= False;
    if rBOM=$FEFF
    then Reverse:= True
    else if rBOM<>$FFFE
    then Stream.Seek(-2, soFromCurrent);
      
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size div 2);
    Stream.Read(Pointer(S)^, Size);
    if Reverse then
      for I:= 1 to Length(S) do
        S[I]:= WideChar(Swap(Word(S[I])));
    SetTextStr(S);
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TWideStrings.Put(Index: Integer; const S: WideString);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TWideStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

procedure TWideStrings.ReadData(Reader: TReader);
var
  S: String;
  W: WideString;
  I: Integer;
  Z: Integer;
  N: Word;
begin
  BeginUpdate;
  try
    Clear;
    S:= Reader.ReadString;
    SetLength(W, Length(S) div 4);
    for I:= 1 to Length(S) div 4 do
      begin
        Val('$'+S[I*4-3]+S[I*4-2]+S[I*4-1]+S[I*2], N, Z);
        W[I]:= WideChar(N);
      end;
    Text:= W;
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TWideStrings.SaveToStream(Stream: TStream);
var
  S: WideString;
begin
  S := GetTextStr;
  Stream.Write(BOM, 2);
  Stream.WriteBuffer(Pointer(S)^, Length(S)*2);
end;

procedure TWideStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendants may optionally implement this method
end;

procedure TWideStrings.SetCommaText(const Value: WideString);
var
  s: WideString;
  i : integer;
  inquote : boolean;
begin
  BeginUpdate;
  
  try
    Clear;
    
    inquote := false;
    i := 1;
    s := '';
    while i <= length(Value) do
    begin
    
      if value[i] = '"' then
      begin
        if inquote then
        begin
          if (i < length(value)) and (value[i] = '"') then
          begin
            s := s + '"';
            inc(i);
          end
          else inquote := false;
        end
        else
        begin
          inquote := true;
        end;
      end
      else if (value[i] = ',') and not inquote then
      begin
        Add(s);
        s := '';
      end
      else
      begin
        s := s + value[i];
      end;
      
      inc(i);
    end;
    
    if s <> '' then Add(s);
    
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.SetText(aText: PWideChar);
begin
  SetTextStr(aText);
end;

procedure TWideStrings.SetTextStr(const Value: WideString);
var
  P, Start: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not (P^ in [WideChar(#0), WideChar(#10), WideChar(#13)]) do Inc(P);
        SetString(S, Start, P - Start);
        Add(S);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TWideStrings.SetValue(const Name, Value: WideString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + '=' + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

procedure TWideStrings.WriteData(Writer: TWriter);
var
  I: Integer;
  S: String;
  W: WideString;
begin
  W:= Text;
  S:= '';
  for I := 1 to Length(W) do
    S:= S+IntToHex(Word(W[1]), 4);
  Writer.WriteString(S);
end;

{ TWideStringList }

destructor TWideStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TWideStringList.Add(const S: WideString): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(SDuplicateString, 0);
      end;
  InsertItem(Result, S);
end;

procedure TWideStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TWideStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TWideStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TWideStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Changing;
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TStringItem));
  Changed;
end;

procedure TWideStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TWideStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp : pointer;
  Item1, Item2: PWideStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := pointer(Item1^.FString);
  pointer(Item1^.FString) := pointer(Item2^.FString);
  pointer(Item2^.FString) := Temp;
  Temp := pointer(Item1^.FObject);
  pointer(Item1^.FObject) := pointer(Item2^.FObject);
  pointer(Item2^.FObject) := Temp;
end;

function TWideStringList.Find(const S: WideString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := WideCompareText(FList^[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TWideStringList.Get(Index: Integer): WideString;
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Result := FList^[Index].FString;
end;

function TWideStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TWideStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TWideStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Result := FList^[Index].FObject;
end;

procedure TWideStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TWideStringList.IndexOf(const S: WideString): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

procedure TWideStringList.Insert(Index: Integer; const S: WideString);
begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(SListIndexError, Index);
  InsertItem(Index, S);
end;

procedure TWideStringList.InsertItem(Index: Integer; const S: WideString);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

procedure TWideStringList.Put(Index: Integer; const S: WideString);
begin
  if Sorted then Error(SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Changing;
  FList^[Index].FString := S;
  Changed;
end;

procedure TWideStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Changing;
  FList^[Index].FObject := AObject;
  Changed;
end;

procedure TWideStringList.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P: WideString;
begin
  repeat
    I := L;
    J := R;
    P := FList^[(L + R) shr 1].FString;
    repeat
      while WideCompareText(FList^[I].FString, P) < 0 do Inc(I);
      while WideCompareText(FList^[J].FString, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TWideStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TStringItem));
  FCapacity := NewCapacity;
end;

procedure TWideStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TWideStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

procedure TWideStringList.Sort;
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1);
    Changed;
  end;
end;

end.
