{ sqluis.pas: SQL Update or Insert Statement creator object
  File maintainer: nvitya@freemail.hu

History:
}

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

unit sqluis;

interface

uses Classes, sqldb;

type
  TuisFieldType = (uftDirect, uftNull, uftString, uftInteger, uftDate, uftDateTime, uftFloat, uftCurrency);

  TFieldValue = record
    SValue : string;
    case integer of
      0 : ( IValue : integer    );
      1 : ( DValue : TDateTime  );
      2 : ( FValue : double     );
      3 : ( CValue : currency   );
  end;

  TFieldPair = class
  public
    FieldName : string;
    FieldType : TuisFieldType;
    Value : TFieldValue;

    function GetValueText : string;
  end;

  TSqlUIS = class
  protected
    function GetFieldPair(FieldName : string) : TFieldPair;
    function AddFieldPair(FieldName : string) : TFieldPair;

  public
    TableName  : string;

    FieldCount : integer;
    FieldPairs : TList;

    Parametrized : boolean;

    constructor Create(tblname : string);
    destructor Destroy; override;

    procedure ClearFields;

    procedure SetField(FieldName, Value : string);

    procedure SetFieldS(FieldName, Value : string);

    procedure SetFieldI(FieldName : string; Value : integer);
    procedure SetFieldD(FieldName : string; Value : TDateTime);
    procedure SetFieldDT(FieldName : string; Value : TDateTime);
    procedure SetFieldF(FieldName : string; Value : Double);
    procedure SetFieldC(FieldName : string; Value : Currency);

    procedure SetFieldNull(FieldName : string);

    function GetFieldValueText(fnum : integer) : string;

    function InsertStatement(ExtraFields,ExtraValues : string) : string;
    function UpdateStatement(ExtraString : string) : string;

  end;

  TDBuis = class(TSqluis)
  protected
    FDataBase : TSqlDBConnection;
  public

    constructor Create(ATableName : string; db : TSqlDBConnection; ParamMode : boolean); reintroduce;

    procedure ExecSQL(sqltext : string);
    procedure ExecInsertX(ExtraFields,ExtraValues : string);
    procedure ExecInsert;
    procedure ExecUpdate(ExtraString : string);

    //procedure UpdateParams;

  end;


implementation

uses SysUtils;

{ TSqlUIS }

function TSqlUIS.AddFieldPair(FieldName: string) : TFieldPair;
var
  fp : TFieldPair;
begin
  fp := TFieldPair.Create;
  fp.FieldName := FieldName;
  FieldPairs.Add(fp);
  Inc(FieldCount);
  result := fp;
end;

constructor TSqlUIS.Create(tblname: string);
begin
  TableName  := tblname;
  FieldCount := 0;
  FieldPairs := TList.Create;
  Parametrized := False;
end;

destructor TSqlUIS.Destroy;
begin
  ClearFields;
  FieldPairs.Free;
  inherited;
end;

function TSqlUIS.InsertStatement(ExtraFields, ExtraValues: string): string;
var
  n : integer;
  fs,vs : string;
  comma : string;
  fp : TFieldPair;
begin
  fs := '';
  vs := '';
  comma := '';
  for n := 0 to FieldCount-1 do
  begin
    fp := TFieldPair(FieldPairs[n]);
    fs := fs + comma + fp.FieldName;
    if Parametrized then vs := vs + comma + ':'+fp.FieldName
                    else vs := vs + comma + GetFieldValueText(n);
    comma := ', ';
  end;
  if ExtraFields <> '' then fs := fs + comma + ExtraFields;
  if ExtraValues <> '' then vs := vs + comma + ExtraValues;
  result := 'insert into '+TableName + #13#10
    + '('+fs+')' + #13#10
    + ' values ('+vs+')';
end;

function TSqlUIS.UpdateStatement(ExtraString: string): string;
var
  n : integer;
  s : string;
  comma : string;
  fp : TFieldPair;
begin
  s := '';
  comma := '';
  for n := 0 to FieldCount-1 do
  begin
    fp := TFieldPair(FieldPairs[n]);
    if Parametrized then s := s + comma + fp.FieldName + '=:' + fp.FieldName
                    else s := s + comma + fp.FieldName + '=' + GetFieldValueText(n);
    comma := ', ';
  end;
  if ExtraString <> '' then s := s + #13#10 + ExtraString;

  result := 'update '+TableName + ' set' + #13#10 + s;
end;

procedure TSqlUIS.SetFieldC(FieldName: string; Value: Currency);
var
  fp : TFieldPair;
begin
  fp := GetFieldPair(FieldName);
  fp.FieldType := uftCurrency;
  fp.Value.CValue := Value;
end;

procedure TSqlUIS.SetFieldD(FieldName: string; Value: TDateTime);
var
  fp : TFieldPair;
begin
  fp := GetFieldPair(FieldName);
  fp.FieldType := uftDate;
  fp.Value.DValue := Value;
end;

procedure TSqlUIS.SetFieldDT(FieldName: string; Value: TDateTime);
var
  fp : TFieldPair;
begin
  fp := GetFieldPair(FieldName);
  fp.FieldType := uftDateTime;
  fp.Value.DValue := Value;
end;

procedure TSqlUIS.SetFieldF(FieldName: string; Value: Double);
var
  fp : TFieldPair;
begin
  fp := GetFieldPair(FieldName);
  fp.FieldType := uftFloat;
  fp.Value.FValue := Value;
end;

procedure TSqlUIS.SetFieldI(FieldName: string; Value: integer);
var
  fp : TFieldPair;
begin
  fp := GetFieldPair(FieldName);
  fp.FieldType := uftInteger;
  fp.Value.IValue := Value;
end;

procedure TSqlUIS.SetFieldNull(FieldName: string);
var
  fp : TFieldPair;
begin
  fp := GetFieldPair(FieldName);
  fp.FieldType := uftNull;
end;

procedure TSqlUIS.SetFieldS(FieldName, Value: string);
var
  fp : TFieldPair;
begin
  fp := GetFieldPair(FieldName);
  fp.FieldType := uftString;
  fp.Value.SValue := Value;
end;

procedure TSqlUIS.SetField(FieldName, Value: string);
var
  fp : TFieldPair;
begin
  fp := GetFieldPair(FieldName);
  fp.FieldType := uftDirect;
  fp.Value.SValue := Value;
end;

function TSqlUIS.GetFieldPair(FieldName: string) : TFieldPair;
var
  n : integer;
begin
  for n := 0 to FieldCount - 1 do
  begin
    if TFieldPair(FieldPairs[n]).FieldName = FieldName then
    begin
      result := TFieldPair(FieldPairs[n]);
      Exit;
    end;
  end;
  Result := AddFieldPair(FieldName);
end;

function TSqlUIS.GetFieldValueText(fnum: integer): string;
var
  fp : TFieldPair;
begin
  fp := TFieldPair(FieldPairs[fnum]);
  if Parametrized and (fp.FieldType <> uftDirect) then result := ':'+fp.FieldName
  else result := fp.GetValueText;
end;

procedure TSqlUIS.ClearFields;
var
  n : integer;
begin
  for n:=0 to FieldPairs.Count-1 do TFieldPair(FieldPairs.Items[n]).Free;
  FieldPairs.Count := 0;
  FieldCount := 0;
end;

{ TFieldPair }

function TFieldPair.GetValueText: string;
begin
  with value do
  begin
    case self.FieldType of
      uftNull:     result := 'NULL';
      uftInteger:  result := IntToStr(IValue);
      uftString:   result := QuotedStr(SValue);
      uftDate:     result := ''''+FormatDateTime('yyyy-mm-dd',DValue)+'''';
      uftDateTime: result := ''''+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',DValue)+'''';
      uftFloat:    result := FloatToStr(FValue);
      uftCurrency: result := '!CURRENCY!';
    else
      // assuming direct
      result := SValue;
    end;
  end;
end;

{ TDBuis }

constructor TDBuis.Create(ATableName: string; db: TSqlDBConnection; ParamMode: boolean);
begin
  inherited Create(ATableName);
  FDataBase := db;
  Parametrized := ParamMode;
end;

procedure TDBuis.ExecInsert;
begin
  ExecInsertX('','');
end;

procedure TDBuis.ExecInsertX(ExtraFields, ExtraValues: string);
var
  s : string;
begin
  s := InsertStatement(ExtraFields,ExtraValues);
  //if not Parametrized or (FLastStatement <> s) then FIBSQL.SQL.Text := s;
  //if Parametrized then UpdateParams;
  FDataBase.ExecStatement(s);
  //FLastStatement := s;
end;

procedure TDBuis.ExecSQL(sqltext: string);
begin
  FDataBase.ExecStatement(sqltext);
end;

procedure TDBuis.ExecUpdate(ExtraString: string);
var
  s : string;
begin
  s := UpdateStatement(ExtraString);
  //if not Parametrized or (FLastStatement <> s) then FIBSQL.SQL.Text := s;
  //if Parametrized then UpdateParams;
  FDataBase.ExecStatement(s);
  //FLastStatement := s;
end;

end.
