{ Copyright (c) 2003, Nagy Viktor 

 File load/save/merge functions - marker searching
}

unit vfdfile;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses SysUtils, Classes;

type
  TVFDFileBlock = class
  public
    BlockID : string;
    FormName : string;
    Position : integer;
    Data : string;
  end;

  TVFDFile = class
  protected
    FFileData : string;
    FParsedData : string;

    FBlocks : TList;

  public
    NewFormsDecl : string;
    NewFormsImpl : string;

    constructor Create;
    destructor Destroy; override;

    function LoadFile(fname : string) : boolean;

    procedure AddBlock(aposition : integer; ablockid, aformname, ablockdata : string);
    function BlockCount : integer;
    function Block(index : integer) : TVFDFileBlock;

    procedure FreeBlocks;

    function GetBlocks : integer;   // parse file
    function MergeBlocks : string;  // store file

    procedure AddNewFormDecl(formname, formheadblock : string);
    procedure AddNewFormImpl(formname, formbody : string);

    function FindFormBlock(blockid, formname : string) : TVFDFileBlock;

    procedure SetFormData(formname, headblock, bodyblock : string);

    procedure NewFileSkeleton(unitname : string);
  end;

implementation

{ TVFDFile }

procedure TVFDFile.AddBlock(aposition: integer; ablockid, aformname, ablockdata: string);
var
  fb : TVFDFileBlock;
begin
  fb := TVFDFileBlock.Create;
  fb.Position := aposition;
  fb.BlockID := ablockid;
  fb.FormName := aformname;
  fb.Data := ablockdata;
  FBlocks.Add(fb);
end;

procedure TVFDFile.AddNewFormDecl(formname, formheadblock: string);
var
  s : string;
begin
  s :=
   '  T'+formname+' = class(TGfxForm)'#10
  +'  public'#10
  +'    {@VFD_HEAD_BEGIN: '+formname+'}'#10
  + formheadblock
  +'    {@VFD_HEAD_END: '+formname+'}'#10
  + #10
  +'    procedure AfterCreate; override;'#10
  +'  end;'#10
  +''#10
  ;
  NewFormsDecl := NewFormsDecl + s;
end;

procedure TVFDFile.AddNewFormImpl(formname, formbody: string);
var
  s : string;
begin
  s := #10#10
  +'procedure T'+formname+'.AfterCreate;'#10
  +'begin'#10
  +'  {@VFD_BODY_BEGIN: '+formname+'}'#10
  + formbody
  +'  {@VFD_BODY_END: '+formname+'}'#10
  +'end;'#10
  ;
  NewFormsImpl := NewFormsImpl + s;
end;

function TVFDFile.Block(index: integer): TVFDFileBlock;
begin
  result := nil;
  if (index < 1) or (index > FBlocks.Count) then Exit;
  result := TVFDFileBlock(FBlocks[index-1]); 
end;

function TVFDFile.BlockCount: integer;
begin
  result := FBlocks.Count;
end;

constructor TVFDFile.Create;
begin
  FFileData := '';
  FParsedData := '';
  NewFormsDecl := '';
  NewFormsImpl := '';
  FBlocks := TList.Create;
end;

destructor TVFDFile.Destroy;
begin
  FreeBlocks;
  FBlocks.Free;
  inherited;
end;

function TVFDFile.FindFormBlock(blockid, formname: string): TVFDFileBlock;
var
  n : integer;
  fb : TVFDFileBlock;
begin
  result := nil;
  for n:=1 to BlockCount do
  begin
    fb := Block(n);
    if (fb.BlockID = blockid) and (UpperCase(fb.FormName) = UpperCase(formname)) then
    begin
      result := fb;
      exit;
    end;
  end;
end;

procedure TVFDFile.FreeBlocks;
var
  n : integer;
begin
  for n:=0 to FBlocks.Count-1 do TVFDFileBlock(FBlocks[n]).Free;
  FBlocks.Clear;
  NewFormsDecl := '';
  NewFormsImpl := '';
end;

function TVFDFile.GetBlocks: integer;
var
  n : integer;

  s : string;

  startp, endp : integer;
  formname : string;
  bname, startmarker, endmarker : string;
  datablock : string;
  deletelen : integer;

  dropmarker : boolean;
begin
  FreeBlocks;

  FParsedData := FFileData;

  // searching blocks:

  repeat

    bname := '';
    formname := '';
    datablock := '';

    s := '{@VFD_';
    startp := pos(s, FParsedData);
    if startp > 0 then
    begin
      // marker found
      n := startp + 2;
      while (n < length(FParsedData)) and (FParsedData[n] in ['_','A'..'Z']) do
      begin
        bname := bname + FParsedData[n];
        inc(n);
      end;

      if FParsedData[n] = ':' then
      begin
        inc(n,2);
      end;

      while (n < length(FParsedData)) and (FParsedData[n] <> '}') do
      begin
        formname := formname + FParsedData[n];
        inc(n);
      end;

      startmarker := copy(FParsedData,startp,n-startp+1);
      deletelen := length(startmarker);
      dropmarker := false;

      Writeln('marker: ',startmarker);

      // block marker ?

      endmarker := '';
      if bname = 'VFD_HEAD_BEGIN' then //or (bname = 'VFD_BODY_BEGIN') then
      begin
        endmarker := '{@VFD_HEAD_END: '+formname+'}';
      end
      else if bname = 'VFD_BODY_BEGIN' then
      begin
        endmarker := '{@VFD_BODY_END: '+formname+'}';
      end;

      if endmarker <> '' then
      begin
        //Writeln('Block: ',bname,' form: ',formname);
        // find the end of the block
        endp := pos(endmarker,FParsedData);
        if endp > 0 then
        begin
          //Writeln('end marker found.');
          datablock := copy(FParsedData,startp+length(startmarker),endp-startp-length(startmarker));
          //Writeln('data block:');
          //writeln(datablock);
          //writeln('.');
          deletelen := endp - startp + length(endmarker);
        end
        else
        begin
          // error: end marker did not found
          //Writeln('file error: ',endmarker,' marker wasn''t found.');
          dropmarker := true;
          // block length = 0
        end;
      end;

      delete(FParsedData,startp,deletelen);

      if not dropmarker then AddBlock(startp,bname,formname,datablock);

    end;

  until startp <= 0;

  //writeln(FParsedData);

  result := BlockCount;
end;

function TVFDFile.LoadFile(fname: string) : boolean;
var
  ff : File;
  s : string;
  cnt : integer;
begin
  result := false;
  AssignFile(ff, fname);
  try
    Reset(ff,1);
  except
    Exit;
  end;

  FFileData := '';
  try
    while not eof(ff) do
    begin
      SetLength(s,4096);
      BlockRead(ff,s[1],length(s),cnt);
      FFileData := FFileData + copy(s,1,cnt);
      if cnt < length(s) then break;
    end;
    result := true;
  finally
    CloseFile(ff);
  end;
  //Writeln('data length: ',length(FFileData));
end;

function TVFDFile.MergeBlocks: string;
var
  rs : string;
  n : integer;
  iofs, startp : integer;
  fb : TVFDFileBlock;
  startmarker, endmarker : string;
  iblock : string;

  newsaved : boolean;

begin
//  Writeln('merging blocks: ');
  newsaved := false;
  rs := FParsedData;
  iofs := 0;
  for n:=0 to FBlocks.Count-1 do
  begin
    fb := TVFDFileBlock(FBlocks[n]);
    startmarker := '{@'+fb.BlockID;
    if fb.formname <> '' then startmarker := startmarker + ': ' + fb.FormName;
    startmarker := startmarker + '}';
    if fb.BlockID = 'VFD_HEAD_BEGIN' then endmarker := '    {@VFD_HEAD_END: '+fb.FormName+'}'
    else if fb.BlockID = 'VFD_BODY_BEGIN' then endmarker := '  {@VFD_BODY_END: '+fb.FormName+'}'
    else endmarker := '';

    iblock := startmarker;
    if endmarker <> '' then iblock := iblock + #10 + fb.Data + endmarker;

    if fb.BlockID = 'VFD_NEWFORM_DECL' then iblock := NewFormsDecl + iblock
    else if fb.BlockID = 'VFD_NEWFORM_IMPL' then
    begin
      iblock := iblock + NewFormsImpl;
      newsaved := true;
    end
    ;

    startp := fb.Position + iofs;
    insert(iblock,rs,startp);
    inc(iofs,length(iblock));
  end;

  if not newsaved and (NewFormsImpl <> '') then
  begin
    // do not loose new form data.
    rs := rs + NewFormsImpl;
  end;

  //writeln(rs);
  result := rs;
end;

procedure TVFDFile.NewFileSkeleton(unitname : string);
begin
  FFileData :=
    'unit '+unitname+';'#10
    +#10
    +'{$ifdef FPC}'#10
    +'{$mode objfpc}{$H+}'#10
    +'{$endif}'#10
    +''#10
    +'interface'#10
    +''#10
    +'uses'#10
    +'  SysUtils, Classes, gfxbase, wgedit, unitkeys, schar16, gfxstyle,'#10
    +'  gfxwidget, gfxform, wglabel, wgbutton,'#10
    +'  wglistbox, wgmemo, wgchoicelist, wggrid, sqldb, sqluis,'#10
    +'  wgdbgrid, gfxdialogs, wgcheckbox;'#10
    +''#10
    +'type'#10
    +''#10
    +'{@VFD_NEWFORM_DECL}'#10
    +''#10
    +'implementation'#10
    +''#10
    +'{@VFD_NEWFORM_IMPL}'#10
    +''#10
    +'end.'#10
    ;

  GetBlocks;
end;

procedure TVFDFile.SetFormData(formname, headblock, bodyblock: string);
var
  fb : TVFDFileBlock;
begin
  fb := FindFormBlock('VFD_HEAD_BEGIN',formname);
  if fb <> nil then fb.Data := HeadBlock else AddNewFormDecl(formname, headblock);

  fb := FindFormBlock('VFD_BODY_BEGIN',formname);
  if fb <> nil then fb.Data := bodyblock else AddNewFormImpl(formname, bodyblock);
end;


end.
