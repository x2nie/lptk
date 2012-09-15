{
  Easy ODBC access objects

  $Id$
}

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

unit sqldb;

interface

uses SysUtils, Classes, odbcsql;

const
  FieldNameMaxLength = 47;
  InitialRecordIndex = 256;  // initially we allocate index for 256 records = 2048 byte

type
  TFieldType = (ftUnknown,ftString,ftInteger,ftBigInt,ftDouble,ftCurrency,ftDateTime);
  
  TSqlResult = class;

  TSqlField = class
  private
    FResultSet : TSqlResult; // parent
    FFieldIndex : integer;   // 1=first field
  public
    // type descriptors
    FieldName : string[FieldNameMaxLength];
    FieldType : TFieldType;
    SQLType   : smallint;
    FieldSize : integer;
    Decimals  : smallint;
    Nullable  : boolean;
    
  protected
    // EDIT BUFFER:           // this uses some unnecessary space, but the code is simpler and maybe faster
    FIsNull   : boolean;
    
    FstrValue : string;
    FintValue : integer;
    FbigintValue : int64;
    FdoubleValue : double;
    // FcurrValue : currency;
    FdtValue : TDateTime;

  public
     constructor Create(AResultSet : TSqlResult; AFieldIndex : integer);
     destructor Destroy; override;

    // qr.GetFieldI('SORSZAM')
    // qr.Field('SORSZAM').asInteger
    // qr.FieldByName('SORSZAM').asInteger

  protected
  
    function GetAsString : string;
    function GetAsInteger : integer;
    
    function GetAsFloat : double;
    function GetAsDateTime : TDateTime;

  public
  
    property AsString  : string read GetAsString;
    property AsInteger : integer read GetAsInteger;
    
    property AsFloat : double read GetAsFloat;
    property AsDateTime : TDateTime read GetAsDateTime;
    
  end;

  TRowIndexItem = record
    Flags  : longword;
    BufPtr : pointer;
  end;
  PRowIndexItem = ^TRowIndexItem;

  TSqlResult = class
  private
    procedure SetRecNo(const AValue: integer);
    
    function GetRowBuffer : pointer;
    
  protected
    FFields : TList;
  
  protected
    FAllFetched : boolean;

    FEditing : boolean;

    FBof, FEof : boolean;

    FRowBuffer : pointer;  // pointer to the current row's data

    FRowIndexPage : PRowIndexItem;
    FIndexPageSize : integer;

    FRowCount : integer;
    FRowIndex : integer;

    procedure FreeRecords;

    function CreateRecordBuf : Pointer;

    function AllocateIndexRow : PRowIndexItem;
    
  protected
    
    procedure ReadRecord;

  public

    procedure FetchAll;

    procedure Next;
    procedure Prior;
    procedure First;
    procedure Last;
    
    property RowBuffer : pointer read GetRowBuffer;

    property Eof : boolean read FEof;
    property Bof : boolean read FBof;
    property RecNo : integer read FRowIndex write SetRecNo;

    property RowCount : integer read FRowCount;
    
  protected

    procedure GetFieldInfo;

  protected
    FHandle : SQLHSTMT;

    FFieldCount : integer;

    function GetFieldCount : integer;

  public

    property FieldCount : integer read FFieldCount;

    constructor Create(hstmt : SQLHSTMT);
    destructor Destroy; override;

    function Field(index : word) : TSqlField; overload;
    function Field(name : string) : TSqlField; overload;

    function FieldName(index : word) : string;

    function GetEditField(index : word) : string;  overload;

    function GetFieldIndex(name : string) : integer;
    
    procedure FetchNext;
    
  public
    function GetFieldS(name : string) : string; overload;
    function GetFieldS(index : word) : string;  overload;

    function GetFieldI(name : string) : integer; overload;
    function GetFieldI(index : word) : integer;  overload;

    function GetFieldD(name : string) : TDateTime; overload;
    function GetFieldD(index : word) : TDateTime;  overload;

    function GetFieldF(name : string) : double; overload;
    function GetFieldF(index : word) : double;  overload;
  end;

  TSqlDBConnection = class
  protected
    FConnected : boolean;

    FDBHandle : SQLHandle;

    FConnectionString : string;

  public

     LastError : integer;
     ErrorMsg8  : string;

     constructor Create;
     destructor Destroy; override;

     function ConnectByDriver(DriverName,ServerName,DatabaseName,UserName,UserPass,otherparams : string) : boolean;
     function ConnectByDSN(const DatabaseName,UserName,UserPass : string) : boolean;

     procedure Disconnect;

     procedure GetDBError;
     procedure GetSQLError(stmtHandle: SQLHSTMT);

     function ExecStatement(query : string) : boolean;
     function RunQuery(query : string) : TSqlResult;

     property Connected : boolean read FConnected;

     procedure PopulateStringList16(sqltext : string; sl : TStringList; conv8to16, withID : boolean);
  end;

var
  ODBCEnvHandle  : SQLHandle;

function ReadDSNList(dsnname, dsndesc : TStringList) : integer;
function CreateDSNList : TStringList;
function CreateDriverNameList : TStringList;

implementation

uses Math, schar16, gfxdialogs;

const
  SQL_SIGNED_OFFSET   = -20;
  SQL_UNSIGNED_OFFSET = -22;
  SQL_C_SBIGINT = SQL_BIGINT + SQL_SIGNED_OFFSET;
  SQL_C_UBIGINT = SQL_BIGINT + SQL_UNSIGNED_OFFSET;

Function ODBCSuccess(Res : Integer) : Boolean;
begin
  ODBCSuccess:= (res=SQL_SUCCESS) or (res=SQL_SUCCESS_WITH_INFO);
end;

procedure GetSQLErrorDiag(htype, ahandle : integer; var LastError : integer; var ErrorMsg8 : string);
var
  s : string;
  stat : array[1..10] of char;
  mlen : SQLSMALLINT;
  err  : SQLINTEGER;
  r : integer;
begin
  SetLength(s,2048);
  r := SQLGetDiagRec(htype, ahandle, 1, @stat, err, PChar(@(s[1])), length(s), mlen);

  if ODBCSuccess(r) then
  begin
    SetLength(s,mlen);
    LastError := err;
    ErrorMsg8 := s;

    writeln('SQL ERROR: ',err);
    writeln(s);
  end
  else
  begin
    LastError := -1;
    ErrorMsg8 := 'Getting error message failed!';
  end;
end;


procedure InitSQL;
var
  res : integer;
begin
  if ODBCEnvHandle <> 0 then Exit;
  
  loadODBC;
  Res := SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, ODBCEnvHandle);
  if Res <> SQL_SUCCESS then
  begin
    ODBCEnvHandle := 0;
    Exit;
  end;

  Res := SQLSetEnvAttr(ODBCEnvHandle, SQL_ATTR_ODBC_VERSION, SQLPOINTER(SQL_OV_ODBC3), 0);
  If Not ODBCSuccess(res) then
  begin
    //...
    //    DoError('Could not set environment',Res);
  end;
end;

function ReadDSNList(dsnname, dsndesc : TStringList) : integer;
var
  r, sdir : integer;
  sname, sdesc : string;
  nlen, dlen : SQLSMALLINT;
begin
  result := 0;

  dsnname.Clear;
  if dsndesc <> nil then dsndesc.Clear;

  SetLength(sname,250);
  SetLength(sdesc,250);

  sdir := SQL_FETCH_FIRST;

  repeat
  
    r := SQLDataSources(ODBCEnvHandle, sdir,
             @sname[1], length(sname), @nlen,
             @sdesc[1], length(sdesc), @dlen);
             
    if ODBCSuccess(r) then
    begin
      dsnname.Add(copy(sname,1,nlen));
      if dsndesc <> nil then dsndesc.Add(copy(sdesc,1,dlen));
      inc(result);
    end;

    sdir := SQL_FETCH_NEXT;

  until not ODBCSuccess(r);
end;

function CreateDriverNameList : TStringList;
var
  sl : TStringList;
  r, sdir : integer;
  sname, sdesc : string;
  nlen, dlen : SQLSMALLINT;
begin
  sl := TStringList.Create;

  SetLength(sname,250);
  SetLength(sdesc,250);
  
  sdir := SQL_FETCH_FIRST;
  repeat
  
    r := SQLDrivers(ODBCEnvHandle, sdir,
             @sname[1], length(sname), @nlen,
             @sdesc[1], length(sdesc), @dlen);

    if ODBCSuccess(r) then
    begin
      sl.Add(copy(sname,1,nlen));
    end;

    sdir := SQL_FETCH_NEXT;

  until not ODBCSuccess(r);

  result := sl;
end;

function CreateDSNList : TStringList;
var
  sl : TStringList;
begin
  sl := TStringList.Create;
  ReadDSNList(sl,nil);
{
  for n:=0 to nl.Count-1 do
    sl.Add(nl.Strings[n]+' ('+dl.Strings[n]+')');
  nl.Free;
}
  result := sl;
end;

{ TSqlDBConnection }

function TSqlDBConnection.ConnectByDriver(
   DriverName, ServerName, DatabaseName, UserName, UserPass, otherparams: string): boolean;
var
  connstring : string;
  len : SQLSMALLINT;
  res : integer;
begin
  Disconnect;

  connstring := 'DRIVER={'+DriverName+'};SERVER='+ServerName+';DATABASE='+DatabaseName
     +';UID='+UserName+';PWD='+UserPass;
  if otherparams <> '' then connstring := connstring + ';' + otherparams;

  connstring := connstring + #0;

  SetLength(FConnectionString,1024);

  Res := SQLDriverConnect(FDBHandle, 0, PSQLCHAR(@(connstring[1])),SQL_NTS,
                        PSQLChar(@(FConnectionString[1])),length(FConnectionString), len, SQL_DRIVER_NOPROMPT);

  if len > 0 then SetLength(FConnectionString,len);

  FConnected := ODBCSuccess(res);
  
  if FConnected then
  begin
    LastError := 0;
    ErrorMsg8 := '';
  end
  else
  begin
    GetDBError;
  end;

  result := FConnected;

end;

function TSqlDBConnection.ConnectByDSN(const DatabaseName, UserName, UserPass: string): boolean;
var
  res : integer;
  dsn, user, pass : string;
begin
  dsn := DatabaseName + #0;
  user := UserName + #0;
  pass := UserPass + #0;
  Res := SQLConnect(FDBHandle, @dsn[1], SQL_NTS, @user[1], SQL_NTS, @pass[1], SQL_NTS );

  FConnected := ODBCSuccess(res);

  if FConnected then
  begin
    LastError := 0;
    ErrorMsg8 := '';
  end
  else
  begin
    GetDBError;
  end;

  result := FConnected;
end;

constructor TSqlDBConnection.Create;
var
  res : integer;
begin
  Res := SQLAllocHandle(SQL_HANDLE_DBC, ODBCenvHandle, FDBHandle);

  if res <> SQL_SUCCESS then
  begin
    raise Exception.Create('ODBC ERROR: '+IntToStr(res)+' = Could not create database handle');
  end;
end;

destructor TSqlDBConnection.Destroy;
begin
  Disconnect;
  SQLFreeHandle(SQL_HANDLE_DBC,FDBHandle);
  inherited;
end;

procedure TSqlDBConnection.Disconnect;
begin
  if FConnected then SQLDisconnect(FDBHandle);
  FConnected := false;
end;

function TSqlDBConnection.ExecStatement(query: string): boolean;
var
  stmtHandle : SQLHSTMT;
  res : integer;
begin
  result := false;
  Res := SQLAllocHandle(SQL_HANDLE_STMT, FDBHandle, stmtHandle);
  If not ODBCSuccess(res) then
  begin
    LastError := res;
    Exit;
  end;

  query := query + #0;
  Res := SQLExecDirect(stmtHandle, PChar(@query[1]), SQL_NTS);
  if ODBCSuccess(res) then
  begin
    LastError := 0;
    ErrorMsg8 := '';
    result := true;
  end
  else
  begin
    GetSQLError(stmtHandle);
  end;
  SQLFreeHandle(SQL_HANDLE_STMT, stmtHandle);
end;

procedure TSqlDBConnection.GetDBError;
var
  s : string;
  stat : array[1..10] of char;
  mlen : SQLSMALLINT;
  err  : SQLINTEGER;
  r : integer;
begin
  SetLength(s,2048);
  r := SQLGetDiagRec(SQL_HANDLE_DBC, FDBHandle, 1, @stat, err, PChar(@(s[1])), length(s), mlen);
  if ODBCSuccess(r) then
  begin
    SetLength(s,mlen);
    LastError := err;
    ErrorMsg8 := s;

    writeln('DB ERROR: ',err);
    writeln(s);
  end
  else
  begin
    LastError := -1;
    ErrorMsg8 := 'Getting error message failed!';
  end;
end;

procedure TSqlDBConnection.GetSQLError(stmtHandle: SQLHSTMT);
var
  s : string;
  stat : array[1..10] of char;
  mlen : SQLSMALLINT;
  err  : SQLINTEGER;
  r : integer;
begin
  SetLength(s,2048);
  r := SQLGetDiagRec(SQL_HANDLE_STMT, stmtHandle, 1, @stat, err, PChar(@(s[1])), length(s), mlen);

  if ODBCSuccess(r) then
  begin
    SetLength(s,mlen);
    LastError := err;
    ErrorMsg8 := s;

    writeln('SQL ERROR: ',err);
    writeln(s);
  end
  else
  begin
    LastError := -1;
    ErrorMsg8 := 'Getting error message failed!';
  end;
end;

function TSqlDBConnection.RunQuery(query: string): TSqlResult;
var
  stmtHandle : SQLHSTMT;
  res : integer;
begin
  result := nil;

  Res := SQLAllocHandle(SQL_HANDLE_STMT, FDBHandle, stmtHandle);
  If not ODBCSuccess(res) then
  begin
    LastError := res;
    Exit;
  end;

  query := query + #0;
  Res := SQLExecDirect(stmtHandle, PChar(@query[1]), SQL_NTS);
  if ODBCSuccess(res) then
  begin
    LastError := 0;
    ErrorMsg8 := '';
    result := TSqlResult.Create(stmtHandle);
  end
  else
  begin
    GetSQLError(stmtHandle);

    SQLFreeHandle(SQL_HANDLE_STMT, stmtHandle);
  end;
end;

{ TSqlResult }

function TSqlResult.AllocateIndexRow: PRowIndexItem;
begin
  inc(FRowCount);
  if FRowCount > FIndexPageSize then
  begin
    // reallocation needed.
    FIndexPageSize := (FIndexPageSize shl 1);

    ReAllocMem(FRowIndexPage, FIndexPageSize*SizeOf(TRowIndexItem));
  end;
  result := FRowIndexPage;
  inc(result,FRowCount - 1);
end;

constructor TSqlResult.Create(hstmt: SQLHSTMT);
var
  n : integer;
begin
  FHandle := hstmt;
  
  FRowIndex := 0;
  feof := True;
  fBof := False;
  
  FRowBuffer := nil;

  FAllFetched := false;
  FEditing := false;

  FFieldCount := GetFieldCount;
  
  FFields := TList.Create;
  for n := 1 to FFieldCount do FFields.Add(TSqlField.Create(self,n));

  FIndexPageSize := InitialRecordIndex;
  GetMem(FRowIndexPage,SizeOf(TRowIndexItem)*FIndexPageSize);
  FRowCount := 0;

  GetFieldInfo;
  
  FetchNext;
end;

function TSqlResult.CreateRecordBuf: Pointer;
var
  n : integer;

  mlen : integer;

  buf : Pointer;

  pdata : Pointer;
  phead : ^integer;
  
  f : TSqlField;
begin
  // in memory format
  // head: Fieldcount * offset (integer)
  // data.
  // string data: length(integer), data...

// 1. calculating memory needed.

  mlen := 0;
  for n:=1 to FieldCount do
  begin
    f := TSqlField(FFields[n-1]);
    inc(mlen,4);
    if not f.FIsNull then
    begin
      case f.FieldType of
        ftString: inc(mlen, SizeOf(integer) + length(f.FstrValue));
        ftInteger: inc(mlen, SizeOf(integer));
        ftBigInt: inc(mlen, SizeOf(int64));
        ftCurrency,
        ftDouble: inc(mlen, SizeOf(double));
        ftDateTime: inc(mlen, SizeOf(TDateTime));
//        ftCurrency: inc(mlen, SizeOf(double));
      end;
    end;
  end;

// 2. allocating memory
  GetMem(buf,mlen);

// 3. filling the memory
  phead := buf;
  pdata := buf;
  inc(PChar(pdata), FieldCount*SizeOf(integer));

  for n:=1 to FieldCount do
  begin
    f := TSqlField(FFields[n-1]);
    if not f.FIsNull then
    begin
      phead^ := (longword(pdata) - longword(buf));
      case f.FieldType of
        ftString:  begin
                     integer(pdata^) := length(f.FstrValue);
                     inc(PChar(pdata),SizeOf(integer));
                     if length(f.FstrValue) > 0 then
                       move(f.FstrValue[1],pdata^,length(f.FstrValue));
                     inc(PChar(pdata),length(F.FstrValue));
                   end;
        ftInteger: begin
                     Integer(pdata^) := f.FintValue;
                     inc(PChar(pdata), SizeOf(integer));
                   end;
        ftBigInt:  begin
                     Int64(pdata^) := f.FbigintValue;
                     inc(PChar(pdata), SizeOf(int64));
                   end;
        ftCurrency,
        ftDouble:  begin
                     double(pdata^) := f.FdoubleValue;
                     inc(PChar(pdata), SizeOf(double));
                   end;
        ftDateTime:begin
                     TDateTime(pdata^) := f.FdtValue;
                     inc(PChar(pdata), SizeOf(TDateTime));
                   end;
{
        ftCurrency:begin
                     double(pdata^) := f.FdoubleValue;
                     inc(PChar(pdata), SizeOf(double));
                   end;
}
      else
        phead^ := 0;
      end;
    end
    else phead^ := 0;
    inc(phead);
  end;

  result := buf;
end;

destructor TSqlResult.Destroy;
var
  n : integer;
begin
  FreeRecords;

  for n:=0 to FFields.Count - 1 do TSqlField(FFields[n]).Free;
  FFields.Free;

  SQLFreeHandle(SQL_HANDLE_STMT, FHandle);
  
  inherited;
end;

procedure TSqlResult.FetchAll;
begin
  while not Eof do FetchNext;
  First;
end;

function TSqlResult.Field(index: word): TSqlField;
begin
  if (index < 1) or (index > FieldCount) then
  begin
    raise exception.Create('Invalid field index');
    result := nil;
    exit;
  end;
  result := TSqlField(FFields[index-1]);
end;

function TSqlResult.Field(name: string): TSqlField;
var
  n : integer;
  f : TSqlField;
begin
  for n:=1 to FieldCount do
  begin
    f := TSqlField(FFields[n-1]);
    if UpperCase(f.FieldName) = UpperCase(name) then
    begin
      result := f;
      exit;
    end;
  end;

  raise Exception.Create('Invalid field name: '+name);
end;

function TSqlResult.FieldName(index: word): string;
begin
  result := Field(index).FieldName;
end;

procedure TSqlResult.SetRecNo(const AValue: integer);
begin
  if not FAllFetched then
  begin
    raise Exception.Create('Movement in not fetched ResultSet');
  end;
  
  FEof := false;
  FBof := false;
  if (AValue < 1) then FBof := true
  else if AValue > RowCount then FEof := true
  else
  begin
    FRowIndex:=AValue;
  end;
end;

function TSqlResult.GetRowBuffer: pointer;
var
  pr : PRowIndexItem;
begin
  pr := FRowIndexPage;
  inc(pr,FRowIndex - 1);
  result := pr^.BufPtr;
end;

procedure TSqlResult.FreeRecords;
var
  n : integer;
  p : PRowIndexItem;
begin
  p := FRowIndexPage;
  for n:=1 to FRowCount do
  begin
    FreeMem(p^.BufPtr);
    inc(p);
  end;
  FreeMem(FRowIndexPage);
  FRowIndexPage := nil;
end;

function TSqlResult.GetEditField(index: word): string;
var
  f : TSqlField;
begin

  f := Field(index);   // can cause access violation

  if not f.FIsNull then
  begin
    case f.FieldType of
      ftString  : result := f.FstrValue;
      ftInteger : result := IntToStr(f.FintValue);
      ftBigInt  : result := IntToStr(f.FbigintValue);
      ftDouble  : result := FloatToStr(f.FdoubleValue);
      ftDateTime: result := DateTimeToStr(f.FdtValue);
    else
      result := '?Unknown?';
    end;
  end
  else result := '<null>';
end;

function TSqlResult.GetFieldS(name: string): string;
begin
  result := Field(name).asString;
end;

function TSqlResult.GetFieldS(index: word): string;
begin
  result := Field(index).asString;
end;

function TSqlResult.GetFieldI(name: string): integer;
begin
  result := Field(name).asInteger;
end;

function TSqlResult.GetFieldI(index: word): integer;
begin
  result := Field(index).asInteger
end;

function TSqlResult.GetFieldD(name: string): TDateTime;
begin
  result := Field(name).asDateTime;
end;

function TSqlResult.GetFieldD(index: word): TDateTime;
begin
  result := Field(index).asDateTime
end;

function TSqlResult.GetFieldF(name: string): double;
begin
  result := Field(name).asFloat;
end;

function TSqlResult.GetFieldF(index: word): double;
begin
  result := Field(index).asFloat;
end;

function TSqlResult.GetFieldCount: integer;
var
  //r : integer;
  cc : SQLSMALLINT;
begin
  //r :=
  SQLNumResultCols(FHandle, cc);
  result := cc;
end;

procedure TSqlResult.GetFieldInfo;
var
  n : integer;
  nlen : smallint;
  nullable : smallint;
  f : TSqlField;
begin
  for n:=1 to FieldCount do
  begin
    f := TSqlField(FFields[n-1]);
  
    SqlDescribeCol(FHandle, n, PChar(@(f.FieldName[1])), FieldNameMaxLength, nlen,
    f.SQLType, longword(f.FieldSize), f.Decimals, nullable);
    
    case f.SQLType of
      SQL_CHAR,SQL_VARCHAR,SQL_LONGVARCHAR,
      SQL_BINARY,SQL_VARBINARY, SQL_LONGVARBINARY,
      SQL_WCHAR, SQL_WVARCHAR, SQL_WLONGVARCHAR:

         f.FieldType := ftString;

      SQL_INTEGER, SQL_SMALLINT, SQL_TINYINT:

         f.FieldType := ftInteger;

      SQL_BIGINT:
      
         f.FieldType := ftBigInt;

      SQL_FLOAT, SQL_REAL, SQL_DOUBLE:
//      SQL_NUMERIC, SQL_DECIMAL:

         f.FieldType := ftDouble;

      {SQL_DATETIME,}
      SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TYPE_TIMESTAMP,
      SQL_DATE, SQL_TIME, SQL_TIMESTAMP:

         f.FieldType := ftDateTime;

     SQL_NUMERIC, SQL_DECIMAL:

         f.FieldType := ftCurrency;

    else
      f.FieldType := ftUnknown;
    end;

    f.Nullable := (nullable <> 0);
    SetLength(f.FieldName,nlen);

  end;
end;

procedure TSqlResult.FetchNext;
var
  res : integer;
  
  ec : integer;
  es : string;
begin
  if FAllFetched then Exit;
  
  Res := SQLFetch(FHandle);
  FEof := not ODBCSuccess(res);
  if not FEof then
  begin
    inc(FRowIndex);
    ReadRecord;
  end
  else
  begin
    if res < 0 then
    begin
      writeln('fetch res=',res);
      GetSQLErrorDiag(SQL_HANDLE_STMT, FHandle, ec, es);
      writeln('SQL ERROR: ',ec);
      writeln(es);
      ShowMessage8(es, 'SQL error: '+IntToStr(ec));
    end;
    FAllFetched := true;
  end;
end;

function TimeStampStruct2DateTime(var sqlts : SQL_TIMESTAMP_STRUCT) : TDateTime;
begin
  with sqlts do
  begin
    Result := EncodeDate(Year, Month, Day);

    if Result >= 0 then Result := Result + EncodeTime(Hour, Minute, Second, Fraction)
                   else Result := Result - EncodeTime(Hour, Minute, Second, Fraction);
  end;
end;

procedure TSqlResult.ReadRecord;
var
  n, r : integer;
  len : SQLINTEGER;
  dpos : integer;
  buflen : integer;

  sqlts : SQL_TIMESTAMP_STRUCT;
  
  irow : PRowIndexItem;
  
  f : TSqlField;
begin
  for n:=1 to FieldCount do
  begin
    f := TSqlField(FFields[n-1]);
    case f.FieldType of
      ftString:
        begin
          case f.SQLType of
          SQL_BINARY,SQL_VARBINARY, SQL_LONGVARBINARY:
            begin
              buflen := 256;
              SetLength(f.FstrValue,buflen);

              r := SQLGetData(FHandle, n, SQL_BINARY, @(f.FstrValue[1]), buflen, @len);
              if (len >= 0) and ((r = SQL_SUCCESS) or (r = SQL_SUCCESS_WITH_INFO)) then
              begin
                SetLength(f.FstrValue,len);
                if (r = SQL_SUCCESS_WITH_INFO) and (len > buflen) then
                begin
                  // more to read
                  SetLength(f.FstrValue,len);
                  dpos := buflen + 1;
                  r := SQLGetData(FHandle, n, SQL_BINARY, @(f.FstrValue[dpos]), len-buflen, @len);
                end;
              end;
              
            end;
          else
            // retriving as char
            buflen := 256;

            SetLength(f.FstrValue,buflen);

            r := SQLGetData(FHandle, n, SQL_CHAR, @(f.FstrValue[1]), buflen, @len);
            if (len >= 0) and ((r = SQL_SUCCESS) or (r = SQL_SUCCESS_WITH_INFO)) then
            begin
              SetLength(f.FstrValue,len);
              if (r = SQL_SUCCESS_WITH_INFO) and (len > buflen-1 ) then
              begin
                // more to read
                dpos := buflen;
                buflen := len;
                SetLength(f.FstrValue, buflen+1);  // +1 for the #0
                r := SQLGetData(FHandle, n, SQL_CHAR, @(f.FstrValue[dpos]), buflen-dpos+2, @len);  // +2 = 2 x char #0
                SetLength(f.FstrValue,buflen); // drop char #0
              end;
            end;

          end;

        
        end;

      ftInteger:

          r := SQLGetData(FHandle, n, SQL_INTEGER, @(f.FintValue), SizeOf(f.FintValue), @len);

      ftBigInt:

          r := SQLGetData(FHandle, n, SQL_C_SBIGINT, @(f.FbigintValue), SizeOf(f.FbigintValue), @len);

      ftDouble:

          r := SQLGetData(FHandle, n, SQL_DOUBLE, @(f.FdoubleValue), SizeOf(f.FdoubleValue), @len);

      ftDateTime:
        begin
          r := SQLGetData(FHandle, n, SQL_TIMESTAMP, @sqlts, SizeOf(sqlts), @len);
          if (len > 0) and ((r = SQL_SUCCESS) or (r = SQL_SUCCESS_WITH_INFO)) then
          begin
            f.FdtValue := TimeStampStruct2DateTime(sqlts);
          end
          else f.FdtValue := 0;

        end;

      ftCurrency:
        begin

          r := SQLGetData(FHandle, n, SQL_DOUBLE, @(f.FdoubleValue), SizeOf(f.FdoubleValue), @len);
          {
          multiplier := IntPower(10,pd^.Decimals);
          if p^.doubleValue >= 0 then p^.doubleValue := int(p^.doubleValue * multiplier + 0.5) / multiplier
                                 else p^.doubleValue := int(p^.doubleValue * multiplier - 0.5) / multiplier;
          }
        end;


    else
      r := 9999;
    end;

    f.FIsNull := (len = SQL_NULL_DATA);

    if not ODBCSuccess(r) then
    begin
      //raise Exception.Create(
      WriteLn(
      IntToStr(r)+' - Error retrieving field value: '+f.FieldName);
    end;

{
    write(pd^.FieldName,'=');
    if not p^.IsNull then
    begin
      case pd^.FieldType of
        ftString  : write(p^.strValue);
        ftInteger : write(p^.intValue);
        ftBigInt  : write(p^.bigintValue);
        ftDouble  : write(p^.doubleValue);
        ftDateTime: write(FormatDateTime('',p^.dtValue));
      end;
    end
    else write('<null>');
    writeln;
}
  end;

  // saving into memory

  irow := AllocateIndexRow;

  irow^.Flags := 0;
  irow^.BufPtr := CreateRecordBuf;

end;

procedure TSqlResult.Next;
begin
  if not FAllFetched then
  begin
    raise Exception.Create('Movement in not fetched ResultSet');
  end;

  // leaving the record...

  if FRowIndex < FRowCount then
  begin
    Inc(FRowIndex);
  end
  else
  begin
    FEof := true;
  end;
  FBof := (FRowCount < 1);
end;

procedure TSqlResult.Prior;
begin
  if not FAllFetched then
  begin
    raise Exception.Create('Movement in not fetched ResultSet');
  end;

  // leaving the record...

  if FRowIndex > 1 then
  begin
    Dec(FRowIndex);
  end
  else
  begin
    FBof := True;
  end;
  FEof := (FRowCount < 1);
end;

procedure TSqlResult.First;
begin
  if not FAllFetched then
  begin
    raise Exception.Create('Movement in not fetched ResultSet');
  end;

  FRowIndex := 1;
  FEof := (FRowCount < 1);
  FBof := FEof;
end;

procedure TSqlResult.Last;
begin
  if not FAllFetched then
  begin
    raise Exception.Create('Movement in not fetched ResultSet');
  end;

  FRowIndex := FRowCount;
  FEof := (FRowCount < 1);
  FBof := FEof;
end;

function TSqlResult.GetFieldIndex(name: string): integer;
var
  n : integer;
begin
  for n:=1 to FieldCount do
   if UpperCase(FieldName(n)) = UpperCase(name) then
   begin
     result := n;
     exit;
   end;
   
  result := -1;
end;

procedure TSqlDBConnection.PopulateStringList16(sqltext : string; sl : TStringList; conv8to16, withID : boolean);
var
  lsql : TSqlResult;
  id : ptrint;
begin
  sl.Clear;
  lsql := RunQuery(sqltext);
  if lsql <> nil then
  begin
    while not lsql.Eof do
    begin
      if withid then id := StrToIntDef(lsql.GetFieldS(2),-1)
                else id := 0;
      if conv8to16 then sl.AddObject(str8to16(lsql.GetFieldS(1)),TObject(id))
                   else sl.AddObject(lsql.GetFieldS(1),TObject(id));
      lsql.FetchNext;
    end;
    lsql.Free;
  end;
end;


{ TSqlField }

constructor TSqlField.Create(AResultSet: TSqlResult; AFieldIndex: integer);
begin
  FResultSet := AResultSet;
  FFieldIndex := AFieldIndex;
  FstrValue := '';
  FieldType := ftUnknown;
end;

destructor TSqlField.Destroy;
begin
  FstrValue := '';
  inherited Destroy;
end;

function TSqlField.GetAsString: string;
var
  p, pr  : Pointer;
  offs : integer;
  d : TDateTime;
begin

  if FResultSet.Eof then
  begin
    raise exception.Create('Reading at EOF');
  end;
  
  if FResultSet.FEditing then
  begin
    if not FIsNull then
    begin
      case FieldType of
        ftString  : result := FstrValue;
        ftInteger : result := IntToStr(FintValue);
        ftBigInt  : result := IntToStr(FbigintValue);
        ftDouble  : result := FloatToStr(FdoubleValue);
        ftDateTime: begin
                      if int(FdtValue) = FdtValue then result := FormatDateTime('yyyy-mm-dd',FdtValue)
                                                  else result := FormatDateTime('yyyy-mm-dd hh:nn:ss',FdtValue);
                    end;
        ftCurrency: begin
                      result := FloatToStrF(FdoubleValue,ffFixed,18,self.Decimals);
                    end;
      else
        result := '?Unknown?';
      end;
    end
    else result := ''; //'<null>';
    Exit;
  end;
  
  // if not editing:

  pr := FResultSet.RowBuffer;
  p := pr;

  inc(PInteger(p),FFieldIndex-1);
  offs := integer(p^);
  if offs = 0 then
  begin
    // null
    result := ''; //'<null>';
  end
  else
  begin
    p := pr;
    inc(PChar(p),offs);

    case FieldType of
      ftString  : begin
                    SetLength(result,integer(p^));
                    inc(PInteger(p),1);
                    if length(result) > 0 then
                      move(p^,result[1],Length(result));
                  end;
      ftInteger : result := IntToStr(integer(p^));
      ftBigInt  : result := IntToStr(int64(p^));
      ftDouble  : result := FloatToStr(double(p^));
      ftDateTime: begin
                    d := TDateTime(p^);
                    if int(d) = d then result := FormatDateTime('yyyy-mm-dd',d)
                                  else result := FormatDateTime('yyyy-mm-dd hh:nn:ss',d);
                  end;
      ftCurrency: begin
                    result := FloatToStrF(double(p^),ffFixed,18,self.Decimals);
                  end;
    else
      result := '?Unknown?';
    end;
  end;

end;

function TSqlField.GetAsInteger: integer;
var
  p, pr  : Pointer;
  offs : integer;
  s : string;
begin

  if FResultSet.Eof then
  begin
    raise exception.Create('Reading at EOF');
  end;

  if FResultSet.FEditing then
  begin
    if not FIsNull then
    begin
      case FieldType of
        ftString  : result := StrToIntDef(FstrValue,0);
        ftInteger : result := FintValue;
        ftBigInt  : result := FbigintValue;
        ftCurrency,
        ftDouble  : result := Round(FdoubleValue);  // round to even - unuseable for finance systems !!!!!!!!!!!!!!!!!!!!
        ftDateTime: begin
                      result := trunc(FdtValue);
                    end;
      else
        result := 0;
      end;
    end
    else result := 0;
    Exit;
  end;

  // if not editing:

  pr := FResultSet.RowBuffer;
  p := pr;

  inc(PInteger(p),FFieldIndex-1);
  offs := integer(p^);
  if offs = 0 then
  begin
    // null
    result := 0;
  end
  else
  begin
    p := pr;
    inc(PChar(p),offs);

    case FieldType of
      ftString  : begin
                    SetLength(s,integer(p^));
                    inc(PInteger(p),1);
                    if length(s) > 0 then
                      move(p^,s[1],Length(s));
                      
                    result := StrToIntDef(s,0);
                  end;
      ftInteger : result := integer(p^);
      ftBigInt  : result := int64(p^);
      ftCurrency,
      ftDouble  : result := round(double(p^));     // round to even !!!!!!!!!!!!!!!!!!
      ftDateTime: result := trunc(TDateTime(p^));
    else
      result := 0;
    end;
  end;

end;

{$ifdef FPC}

function StrToFloatDef(const s : string; def : double) : double;
begin
  try
    result := StrToFloat(s);
  except
    result := def;
  end;
end;

{$endif}

function TSqlField.GetAsFloat: double;
var
  p, pr  : Pointer;
  offs : integer;
  s : string;
begin

  if FResultSet.Eof then
  begin
    raise exception.Create('Reading at EOF');
  end;

  if FResultSet.FEditing then
  begin
    if not FIsNull then
    begin
      case FieldType of
        ftString  : result := StrToFloatDef(FstrValue,0);
        ftInteger : result := FintValue;
        ftBigInt  : result := FbigintValue;
        ftCurrency,
        ftDouble  : result := FdoubleValue;
        ftDateTime: begin
                      result := FdtValue;
                    end;
      else
        result := 0;
      end;
    end
    else result := 0;
    Exit;
  end;

  // if not editing:

  pr := FResultSet.RowBuffer;
  p := pr;

  inc(PInteger(p),FFieldIndex-1);
  offs := integer(p^);
  if offs = 0 then
  begin
    // null
    result := 0;
  end
  else
  begin
    p := pr;
    inc(PChar(p),offs);

    case FieldType of
      ftString  : begin
                    SetLength(s,integer(p^));
                    inc(PInteger(p),1);
                    if length(s) > 0 then
                      move(p^,s[1],Length(s));

                    result := StrToFloatDef(s,0);
                  end;
      ftInteger : result := integer(p^);
      ftBigInt  : result := int64(p^);
      ftCurrency,
      ftDouble  : result := double(p^);
      ftDateTime: result := TDateTime(p^);
    else
      result := 0;
    end;
  end;

end;

function TSqlField.GetAsDateTime: TDateTime;
var
  p, pr  : Pointer;
  offs : integer;
  s : string;
begin

  if FResultSet.Eof then
  begin
    raise exception.Create('Reading at EOF');
  end;

  if FResultSet.FEditing then
  begin
    if not FIsNull then
    begin
      case FieldType of
        ftString  : result := StrToTime(FstrValue);    // str -> date conversion !!!!!!!!!!!
        ftInteger : result := FintValue;
        ftBigInt  : result := FbigintValue;
        ftCurrency,
        ftDouble  : result := FdoubleValue;
        ftDateTime: begin
                      result := FdtValue;
                    end;
      else
        result := 0;
      end;
    end
    else result := 0;
    Exit;
  end;

  // if not editing:

  pr := FResultSet.RowBuffer;
  p := pr;

  inc(PInteger(p),FFieldIndex-1);
  offs := integer(p^);
  if offs = 0 then
  begin
    // null
    result := 0;
  end
  else
  begin
    p := pr;
    inc(PChar(p),offs);

    case FieldType of
      ftString  : begin
                    SetLength(s,integer(p^));
                    inc(PInteger(p),1);
                    if length(s) > 0 then
                      move(p^,s[1],Length(s));

                    result := StrToTime(s);         // !!!!!!!!!!!!!!!!!!!!!
                  end;
      ftInteger : result := integer(p^);
      ftBigInt  : result := int64(p^);
      ftCurrency,
      ftDouble  : result := double(p^);
      ftDateTime: result := TDateTime(p^);
    else
      result := 0;
    end;
  end;

end;

initialization
begin
  ODBCEnvHandle := 0;
  InitSQL;
end;

finalization
begin
  if ODBCEnvHandle <> 0 then SQLFreeHandle(SQL_HANDLE_ENV, ODBCEnvHandle);
end;

{--------------------------------------------------------------------------------
  $Log$
  Revision 1.5  2012/09/15 10:15:52  nvitya
  64-bit fixes

  Revision 1.4  2004/01/02 00:06:26  nvitya
  bigint bugfix


}

end.
