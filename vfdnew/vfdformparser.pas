{ Copyright (c) 2003, Nagy Viktor

 Some pascal source code parsing functionality
}

unit vfdformparser;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, gfxbase, messagequeue, schar16, gfxwidget, gfxform, gfxdialogs, sqldb, gfxstyle,
  wglabel, wgedit, wgbutton, wglistbox, wgmemo, wgchoicelist, wggrid, wgdbgrid, wgcheckbox,
  vfdresizer, vfdforms, vfddesigner, vfdfile, vfdutils,
  vfdwidgetclass, vfdwidgets;

type
  TVFDFormParser = class
  protected
    ffd : TFormDesigner;
    fformname : string;

    BodyLines : TStringList;

    eob : boolean;

    line : string;
    lineindex : integer;

  public
    procedure nextline;

  public
    constructor Create(const FormName, FormHead, FormBody: string);
    destructor Destroy; override;

    function ParseForm : TFormDesigner;

    procedure ParseFormProperties;

    procedure ParseFormWidgets;

    function ReadWGProperty(propline : string; wg : TWidget; wgc : TVFDWidgetClass) : boolean;
  end;


function GetIdentifier(var s : string) : string;
function GetStringValue(var s : string) : string;
procedure SkipSpaces(var s : string);
function CheckSymbol(var s : string; const sym : string) : boolean;
function GetIntValue(var s : string) : integer;

implementation

{ TVFDFormParser }

constructor TVFDFormParser.Create(const FormName, FormHead, FormBody: string);
begin
  fformname := FormName;
  ffd := nil;
  BodyLines := TStringList.Create;
  BodyLines.Text := FormBody;
  lineindex := 0;
end;

destructor TVFDFormParser.Destroy;
begin
  BodyLines.Free;
  inherited;
end;

procedure TVFDFormParser.nextline;
begin
  repeat
    inc(lineindex);
    eob := (lineindex > BodyLines.Count);
    if not eob then
    begin
      line := trim(bodylines.Strings[lineindex-1]);
    end
    else line := '';
  until eob or (line <> '');
end;

function TVFDFormParser.ParseForm: TFormDesigner;
begin
  ffd := TFormDesigner.Create;
  ffd.Form.Name := fformname;

  // parsing line by line
  // the unknown lines will be "other properties"
  lineindex := 0;
  nextline;

  ParseFormProperties;

  ParseFormWidgets;

  result := ffd;
end;

procedure SkipSpaces(var s : string);
begin
  while (s <> '') and (s[1] in [' ',#9,#13,#10]) do delete(s,1,1);
end;

function CheckSymbol(var s : string; const sym : string) : boolean;
begin
  SkipSpaces(s);
  result := (pos(sym,s) = 1);
  if result then delete(s,1,length(sym));
end;

function GetIntValue(var s : string) : integer;
var
  n : integer;
  ns : string;
begin
  SkipSpaces(s);
  ns := '';
  n := 1;
  while (n <= length(s)) and (s[n] in ['0'..'9','-']) do
  begin
    ns := ns + s[n];
    inc(n);
  end;
  result := StrToIntDef(ns,0);
  delete(s,1,length(ns));
end;

function GetStringValue(var s : string) : string;
var
  n : integer;
  quot : boolean;
  c, prevc : char;
  ccode : string;
  ids : string;
begin
  result := '';
  ids := GetIdentifier(s);
  if ids <> '' then
  begin
    if ids = 'u8' then
    begin
      if not CheckSymbol(s,'(') then Exit;
    end
    else Exit
  end;
  SkipSpaces(s);
  prevc := #0;
  n := 1;
  quot := false;
  while n <= length(s) do
  begin
    c := s[n];
    if c = '''' then
    begin
      quot := not quot;
      if quot and (prevc = '''') then result := result + c;
    end
    else
    if not quot then
    begin
      if not (c in ['+',' ',#9,#13,#10]) then
      begin
        if (c = '#') then
        begin
          inc(n);
          ccode := '';
          while (n <= length(s)) and (s[n] in ['0'..'9']) do
          begin
            ccode := ccode + s[n];
            inc(n);
          end;
          c := chr(StrToIntDef(ccode,ord('?')) and $FF);
          result := result + c;
        end
        else break;
      end;
    end
    else
    begin
      result := result + c;
    end;
    prevc := c;
    inc(n);
  end;
  if (n-1) > 0 then delete(s,1,n-1);

  SkipSpaces(s);
  if ids <> '' then CheckSymbol(s, ')');
  if ids = 'u8' then result := u8(result);
end;

function GetIdentifier(var s : string) : string;
var
  n : integer;
begin
  SkipSpaces(s);
  result := '';
  n := 1;
  while n <= length(s) do
  begin
    if s[n] in ['_','a'..'z','A'..'Z','0'..'9'] then
    begin
      result := result + s[n];
    end
    else Break;
    inc(n);
  end;
  if length(result) > 0 then delete(s,1,length(result))
end;

procedure TVFDFormParser.ParseFormProperties;
var
  lok : boolean;
begin
  while not eob and (pos('.CREATE(',UpperCase(line)) = 0) do
  begin
    lok := ReadWGProperty(line, ffd.Form, VFDFormWidget);

    if not lok then ffd.FormOther := ffd.FormOther + line + #10;

    NextLine;
  end;
end;

procedure TVFDFormParser.ParseFormWidgets;
var
  n : integer;
  lok : boolean;
  s : string;
  ident : string;
  wgname,wgclass,wgclassuc, wgparent : string;
  pwg, wg : TWidget;
  wgother : string;
  wd : TWidgetDesigner;
  wgc : TVFDWidgetClass;
begin
  while not eob do
  begin
    //s := UpperCase(line);
    s := line;

    wgname := GetIdentifier(s);
    //writeln('wg: ',wgname);
    lok := CheckSymbol(s,':=');
    if lok then wgclass := GetIdentifier(s);
    lok := lok and CheckSymbol(s,'.');
    lok := lok and (UpperCase(GetIdentifier(s)) = 'CREATE');
    lok := lok and CheckSymbol(s,'(');
    if lok then wgparent := GetIdentifier(s);
    lok := lok and CheckSymbol(s,')');
    lok := lok and CheckSymbol(s,';');

    if lok then
    begin
      //writeln('wg create: ',wgname,' (',wgclass,') - ',wgparent);

      // searching for the parent ...
      pwg := nil;
      if UpperCase(wgparent) <> 'SELF' then
      begin
        pwg := ffd.FindWidgetByName(wgparent);
        if pwg = nil then
        begin
          Writeln('Warning! parent object "'+wgparent+'" not found for "'+wgname+'"');
        end;
      end;
      if pwg = nil then pwg := ffd.Form;

      wgclassuc := UpperCase(wgclass);

      wg := nil;
      wgc := nil;
      for n := 1 to VFDWidgetCount do
      begin
        wgc := VFDWidget(n);
        if wgclassuc = UpperCase(wgc.WidgetClass.ClassName) then
        begin
          wg := wgc.CreateWidget(pwg);
          break;
        end;
      end;

{
      if      wgclassuc = 'TWGLABEL'     then wg := TwgLabel.Create(pwg)
      else if wgclassuc = 'TWGEDIT'      then wg := TwgEdit.Create(pwg)
      else if wgclassuc = 'TWGCHECKBOX'  then wg := TwgCheckBox.Create(pwg)
      else if wgclassuc = 'TWGBUTTON'    then wg := TwgButton.Create(pwg)
      else if wgclassuc = 'TWGMEMO'      then wg := TwgMemo.Create(pwg)
      else if wgclassuc = 'TWGCHOICELIST'     then wg := TwgChoiceList.Create(pwg)
      else if wgclassuc = 'TWGDBGRID'     then wg := TwgDBGrid.Create(pwg)
      else if wgclassuc = 'TWGTEXTLISTBOX'     then wg := TwgTextListBox.Create(pwg)
      else
}
      if wg = nil then
      begin
        wgc := VFDOtherWidget;
        wg := TOtherWidget.Create(pwg);
        TOtherWidget(wg).wgClassName := wgclass;
      end;

      wg.name := wgname;

      NextLine;
      s := UpperCase(line);
      ident := GetIdentifier(s);
      if ident = 'WITH' then
      begin
        // skip with line...
        NextLine;

        s := UpperCase(line);
        ident := GetIdentifier(s);
        if ident = 'BEGIN' then
        begin
          NextLine;
        end;

        // reading widget properties...
        wgother := '';

        while (not eob) and (pos('END;',UpperCase(line)) <> 1) do
        begin
          lok := ReadWGProperty(line,wg,wgc);
          if not lok then wgother := wgother + line + #10;
          nextline;
        end;

        if (pos('END;',UpperCase(line)) = 1) then nextline;

      end;

      wd := ffd.AddWidget(wg, nil);
      wd.FVFDClass := wgc;
      wd.other.Text := wgother;

    end
    else
    begin
      ffd.FormOther := ffd.FormOther + line + #10;
      NextLine;
    end;

  end;
end;

function TVFDFormParser.ReadWGProperty(propline: string; wg: TWidget; wgc : TVFDWidgetClass): boolean;
var
  s : string;
  n : integer;
  ident : string;
  lok : boolean;
  sval : string;
  wga  : TAnchors;
begin
  s := propline;

  ident := UpperCase(GetIdentifier(s));
  //writeln('ident: ',ident);
  sval := '';

  lok := false;

  if ident = 'NAME' then
  begin
    lok := CheckSymbol(s, ':=');
    if lok then
    begin
      sval := GetStringValue(s);
      lok := CheckSymbol(s, ';');
    end;
    if lok then wg.Name := sval;
  end
{
  else if ident = 'TEXT' then
  begin
    lok := CheckSymbol(s, ':=');
    if lok then
    begin
      sval := GetStringValue(s);
      lok := CheckSymbol(s, ';');
    end;
    if lok then
    begin
      if wg is TwgLabel then TwgLabel(wg).Text := sval
      else if wg is TwgEdit then TwgEdit(wg).Text := sval
      else if wg is TwgButton then TwgButton(wg).Text := sval
      else if wg is TwgCheckBox then TwgCheckBox(wg).Text := sval
      else
          lok := false;
    end;
    if lok then SetWidgetText(wg, sval);
  end
  else if (ident = 'LINES') or (ident = 'ITEMS') then
  begin
    lok := CheckSymbol(s, '.');
    lok := lok and (UpperCase(GetIdentifier(s)) = 'ADD');
    lok := lok and CheckSymbol(s, '(');
    if lok then
    begin
      sval := GetStringValue(s);
      lok := lok and CheckSymbol(s, ')');
      lok := lok and CheckSymbol(s, ';');
    end;
    if lok then
    begin
      if wg is TwgMemo then TwgMemo(wg).Lines.Add(sval)
      else if wg is TwgChoiceList then TwgChoiceList(wg).Items.Add(sval)
      else if wg is TwgTextListBox then TwgTextListBox(wg).Items.Add(sval)
      else
          lok := false;
    end;
  end
}
  else if ident = 'ANCHORS' then
  begin
    lok := CheckSymbol(s, ':=');
    lok := lok and CheckSymbol(s, '[');
    if lok then
    begin
      wga := [];
      repeat
        sval := UpperCase(GetIdentifier(s));
        if      sval = 'ANLEFT'   then wga := wga + [anLeft]
        else if sval = 'ANTOP'    then wga := wga + [anTop]
        else if sval = 'ANRIGHT'  then wga := wga + [anRight]
        else if sval = 'ANBOTTOM' then wga := wga + [anBottom]
        ;
      until not CheckSymbol(s,',');
    end;
    lok := lok and CheckSymbol(s, ']');
    lok := lok and CheckSymbol(s, ';');

    if lok then wg.Anchors := wga;
  end
  else if ident = 'WINDOWTITLE8' then
  begin
    lok := (wg is TgfxForm);
    if lok then
    begin
      lok := CheckSymbol(s, ':=');
      if lok then
      begin
        sval := GetStringValue(s);
        lok := CheckSymbol(s, ';');
      end;
      if lok then TGfxForm(wg).WindowTitle8 := sval;
    end;
  end
  else if ident = 'SETDIMENSIONS' then
  begin
    lok := CheckSymbol(s, '(');
    if lok then wg.Left := GetIntValue(s);
    lok := lok and CheckSymbol(s, ',');
    if lok then wg.Top := GetIntValue(s);
    lok := lok and CheckSymbol(s, ',');
    if lok then wg.Width := GetIntValue(s);
    lok := lok and CheckSymbol(s, ',');
    if lok then wg.Height := GetIntValue(s);
    lok := lok and CheckSymbol(s, ')');
    lok := lok and CheckSymbol(s, ';');
    //if lok then Writeln('sd ok.');
    //writeln('WT: ',sval);
  end
{
  else if (wg is TwgDBGrid) and (ident = 'ADDCOLUMN8') then
  begin
    c := TDBColumn.Create;
    lok := CheckSymbol(s, '(');

    if lok then c.Title := u8(GetStringValue(s));
    lok := lok and CheckSymbol(s, ',');
    if lok then c.FieldName8 := GetStringValue(s);
    lok := lok and CheckSymbol(s, ',');
    if lok then c.Width := GetIntValue(s);
    lok := lok and CheckSymbol(s, ',');
    if lok then
    begin
      sval := UpperCase(GetIdentifier(s));
      if sval = 'ALRIGHT' then c.Alignment := alRight
      else if sval = 'ALCENTER' then c.Alignment := alCenter
      else c.Alignment := alLeft;
    end;

    lok := lok and CheckSymbol(s, ')');
    lok := lok and CheckSymbol(s, ';');

    if lok then
    begin
      TwgDBGrid(wg).AddColumn(c.Title, c.FieldName8, c.Width, c.Alignment)
    end;

    c.Free;
  end;
}
  ;

  if not lok then
  begin
    if wgc <> nil then
    begin
      for n:=1 to wgc.PropertyCount do
      begin
        lok := wgc.GetProperty(n).ParseSourceLine(wg,line);
        if lok then Break;
      end;
    end;
  end;

  if not lok then
  begin
    Writeln('unknown: ',line);
  end;

  result := lok;
end;

end.
