{ Copyright (c) 2003, Nagy Viktor 

 not used?
}

unit pasparser;

{$H+}

interface

type

  chararray = array[0..250000] of char;

  TPasParser = class
  public
    dataptr : ^chararray;
    datalen : integer;

    datapos : integer;

    linenum : integer;
    curch : char;

    curword : string[50];
    curline : string;

    error  : boolean;
    errmsg : string;

    htmlfilter : boolean;

    constructor Create(adata : pointer; len : longint);
    procedure SetData(adata : pointer; len : longint);

    function NextCh : char;

    procedure NextLine;

    procedure NextWord;
    procedure NextTag;
    procedure NextWordUC;
    procedure SkipSpaces;
    procedure SkipLineEnd;

    function IsWordChar : boolean;
    function IsDigit : boolean;

    function GetInt : integer;
    function GetStr : string;

    procedure BeginParse;

    function GetIntValue(name: string; var res : integer) : boolean;
    function GetStrValue(name: string; var res : string) : boolean;
    
    function CheckChar(ch : char) : boolean;

  end;

implementation

uses SysUtils;

constructor TPasParser.Create(adata : pointer; len : longint);
begin
  SetData(adata, len);
  htmlfilter := false;
end;

function TPasParser.NextCh : char;
var
  hs : string[20];

  dpsave : integer;
  lnsave : integer;
  
  procedure ReadNextCh;
  begin
    inc(datapos);
    if datapos >= datalen then curch := #0 else curch := dataptr^[datapos];
    if (curch = #10) then inc(linenum);
  end;

begin
  ReadNextCh;

  if htmlfilter and (curch = '&') then
  begin
    dpsave := datapos;
    lnsave := linenum;
    
    hs := '';
    ReadNextCh;
    while (curch <> #0) and (curch <> ';') and (curch <> '&') do
    begin
      hs := hs + curch;
      ReadNextCh;
    end;

    if hs = 'quot' then curch := '"'
    else
    begin
      { restore the state }
      datapos := dpsave;
      linenum := lnsave;
      curch := dataptr^[datapos];
    end;
  end;

  result := curch;
end;

procedure TPasParser.SkipSpaces;
begin
  while (curch = ' ') or (curch = #10) or (curch = #13) or (curch = #9) do NextCh;
end;

procedure TPasParser.BeginParse;
begin
  datapos := 0;
  linenum := 0;
  curword := '';
  curline := '';
  error := false;
  errmsg := '';
  curch := #0;
  if datalen > 0 then curch := dataptr^[0];
end;

procedure TPasParser.NextLine;
begin
  curline := '';
  while (curch <> #0) and (curch <> #13) and (curch <> #10) do
  begin
     curline := curline + curch;
     NextCh;
  end;
end;

procedure TPasParser.NextWord;
begin
  curword := '';
  if not IsWordChar then exit;

  while IsWordChar or IsDigit do
  begin
    curword := curword + curch;
    NextCh;
  end;
end;

procedure TPasParser.NextTag;
begin
  curword := '';
  while IsWordChar or IsDigit do
  begin
    curword := curword + curch;
    NextCh;
  end;
end;

function TPasParser.IsWordChar: boolean;
begin
  // faster than set handling
  result := ((curch >= 'A') and (curch <= 'Z')) or
            ((curch >= 'a') and (curch <= 'z')) or (curch = '_');
end;

function TPasParser.IsDigit: boolean;
begin
  // faster than set handling
  result := ((curch >= '0') and (curch <= '9'));
end;

procedure TPasParser.NextWordUC;
begin
  NextWord;
  curword := UpperCase(curword);
end;

function TPasParser.GetIntValue(name: string; var res : integer) : boolean;
begin
  SkipSpaces;
  NextWordUC;

  result := False;

  if curword <> name then Exit;
  SkipSpaces;
  if curch <> '=' then Exit;
  NextCh;

  res := GetInt;
  result := True;
end;

function TPasParser.GetStrValue(name: string; var res : string) : boolean;
begin
  SkipSpaces;
  NextWordUC;

  result := False;

  if curword <> name then Exit;
  SkipSpaces;
  if curch <> '=' then Exit;
  NextCh;

  res := GetStr;
  result := True;
end;

function TPasParser.CheckChar(ch: char): boolean;
begin
  SkipSpaces;
  if curch = ch then
  begin
    NextCh;
    SkipSpaces;
    result := true;
  end
  else result := false;
end;

function TPasParser.GetInt : integer;
var
  v : integer;
begin
  SkipSpaces;
  v := 0;
  while IsDigit do
  begin
    v := v*10 + ord(curch)-ord('0');
    NextCh;
  end;
  result := v;
end;

function TPasParser.GetStr : string;
begin
  result := '';
  if curch = '''' then NextCh;

  repeat
    while (curch <> #0) and (curch <> #10) and (curch <> #13) and (curch <> '''') do
    begin
      result := result + curch;
      NextCh;
    end;

    if curch = '''' then NextCh;
    
    if curch <> '''' then break;
    
    result := result + '''';
    NextCh;
    
  until false;
end;

procedure TPasParser.SkipLineEnd;
begin
  if curch = #13 then NextCh;
  if curch = #10 then NextCh; 
end;

procedure TPasParser.SetData(adata : pointer; len: longint);
begin
  dataptr := adata;
  datalen := len;
  datapos := 0;
end;

end.
