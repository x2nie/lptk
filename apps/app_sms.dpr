// GUI and char converter for gsmsendsms
// requires libgsm - gsm-utils package on debian
// only for linux

{ $Id$ }

program app_sms;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$else}
 {$APPTYPE CONSOLE}
{$endif}

uses
  SysUtils, Classes, gfxbase, wgedit, unitkeys, schar16, gfxstyle,
  gfxwidget, gfxform, wglabel, wgbutton,
  wglistbox, wgmemo, wgchoicelist, wggrid, sqldb, sqluis,
  wgdbgrid, gfxdialogs, wgcheckbox,
  linux;

const
  UnknownChar = '?'; // this will be sent if the character not in the Default GSM alphabet

type

  TfrmMain = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmMain}
    lbLabel1 : TWGLABEL;
    chlDevice : TWGCHOICELIST;
    lbLabel2 : TWGLABEL;
    chlSpeed : TWGCHOICELIST;
    lbLabel3 : TWGLABEL;
    edPhone : TWGEDIT;
    lbLabel4 : TWGLABEL;
    edMessage : TWGMEMO;
    btnSend : TWGBUTTON;
    btnExit : TWGBUTTON;
    lbLabel5 : TWGLABEL;
    txtCharNum : TWGLABEL;
    {@VFD_HEAD_END: frmMain}

    procedure AfterCreate; override;

    procedure SendClick(Sender : TObject);

    procedure ExitClick(Sender : TObject);

    procedure TextChange(Sender : TObject);

    function GetSMSText : string;
  end;

  TfrmWait = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmWait}
    lbLabel1 : TWGLABEL;
    {@VFD_HEAD_END: frmWait}

    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

procedure Sleep(ms : integer);
begin
  select(0,0,0,0,ms);
end;

function u16toGSMold(const s16 : string) : string;
var
  uc : word;
  c  : char;
  n  : integer;
begin

  { See the default GSM alphabet at http://www.dreamfabric.com/sms/default_alphabet.html }

  result := '';
  for n:=1 to length16(s16) do
  begin
    c := s16[n*2-1];
    uc := ord(s16[n*2]) shl 8 + ord(c);
    if uc > 255 then
    begin
      // some hungarian unicode translation
      case uc of
        $0150 : c := chr(92);
        $0151 : c := chr(124);

        $0170 : c := chr(94);
        $0171 : c := chr(126);
      else
        c := UnknownChar;
      end;
    end
    else
    begin
      case c of
        #32..#35,
        #37..#63,
        'A'..'Z',  // 65-90
        'a'..'z'   // 97-122
                 :  ; // no translation required

        #163:  c := #01; //
        #36:   c := #02; // $
        #64:   c := #00; // @ !!!

        #167:  c := #95;
        #95:   c := #17;

        #216:  c := #11;
        #248:  c := #12;
        #197:  c := #14;
        #229:  c := #15;

        #232:  c := #4;

        #201:  c := #31; // É
        #233:  c := #5;  // é

        #196:  c := #91;     // Ä
        #209:  c := #93;
        #228:  c := #123;    // ä
        #241:  c := #125;

        #214:  c := #92;  // Ö
        #220:  c := #94;  // Ü

        #246:  c := #124; // ö
        #252:  c := #126; // ü

        #193:  c := #14;  // Á
        #224,
        #225:  c := #127; // á

        #218:  c := #6; // Ú
        #249,
        #250:  c := #6; // ú

        #205:  c := #7; // Í
        #236,
        #237:  c := #7; // í

        #211:  c := #8; // Ó
        #242,
        #243:  c := #8; // ó

      else
        c := UnknownChar;
      end;
    end;

    result := result + c;
  end;
end;

function u16toGSM(const s16 : string) : string;
var
  uc : word;
  c  : char;
  n  : integer;
begin

  { See the default GSM alphabet at http://www.dreamfabric.com/sms/default_alphabet.html }

  result := '';
  for n:=1 to length16(s16) do
  begin
    c := s16[n*2-1];
    uc := ord(s16[n*2]) shl 8 + ord(c);
    if uc > 255 then
    begin
      // some hungarian unicode translation
      case uc of
        $0150 : c := chr(214);
        $0151 : c := chr(246);

        $0170 : c := chr(220);
        $0171 : c := chr(252);
      else
        c := UnknownChar;
      end;
    end
    else
    begin
      case c of
        #193:  c := #197;  // Á
        #225:  c := #224;  // á

        #218,
        #250:  c := #249; // Ú, ú

        #205,             // Í
        #237:  c := #236; // í

        #211,             // Ó
        #243:  c := #242; // ó

      end;
    end;

    result := result + c;
  end;
end;

// program execution for linux:

function ExecPrg(const fnameandparams : string; const ainput : string; var aoutput : string; waitsecs : integer) : integer;
var
  p2ci, p2co, c2pi, c2po : integer;

  cpid : integer;
  pars : string;
  exitstat, epid : integer;
  rn, wnum : integer;
  buf : string;

begin
  result := -1;

  // 1. create input pipe, create output pipe
  AssignPipe(p2ci,p2co);
  AssignPipe(c2pi,c2po);

  // 2. fork
  cpid := FORK; // !!!!

  if cpid < 0 then
  begin
    fdclose(p2ci);
    fdclose(p2co);
    fdclose(c2pi);
    fdclose(c2po);
    Exit;
  end;

  if cpid = 0 then
  begin
    // child process
    fdclose(c2pi);
    fdclose(p2co);

    // set std. in and std out
    Dup2( c2po, 1 );
    Dup2( 1, 2 );
    Dup2( p2ci, 0 );

    // 6. execute the program

    pars := fnameandparams + #0;

    execlp(pars, envp);

    Halt(127);

  end
  else
  begin
    // parent:
    fdclose(p2ci);
    fdclose(c2po);

    // wait for the child for finish
    // read the pipe data into memory
    // send back the result

    // send the input
    if length(ainput) > 0 then
    begin
      fdwrite(p2co, ainput[1], length(ainput));
    end;
    fdclose(p2co);

    // checking the exit status

    wnum := 0;
    repeat
      epid := waitpid(cpid, @exitstat, WNOHANG);
      if epid > 0 then break;

      //writeln('waiting...', wnum);

      sleep(100);
      inc(wnum);
    until wnum > waitsecs * 10;

    if wnum > waitsecs * 10 then
    begin
      //Writeln('KILLING!');

      // sending kill signal
      Kill(cpid, SIGTERM);
      waitpid(cpid, @exitstat, 0);
    end;

    // getting the output from the child

    aoutput := '';
    repeat
      setlength(buf,256);
      rn := fdread(c2pi, buf[1], length(buf));
      if rn > 0 then
      begin
        //fdwrite(1, buf[1], rn);
        aoutput := aoutput + copy(buf, 1, rn);
      end;
    until rn <= 0;
    fdclose(c2pi);

    if wnum > waitsecs * 10 then result := -3
    else if epid > 0 then
    begin
      result := (exitstat shr 8) and $FF;
    end
    else result := -2;

  end;

end;

{@VFD_NEWFORM_IMPL}

procedure TfrmWait.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmWait}
  SetDimensions(292,536,244,45);
  WindowTitle8 := 'frmWait';

  lbLabel1 := TWGLABEL.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(20,12,200,16);
    Text := u8('Sending the SMS message...');
  end;

  {@VFD_BODY_END: frmWait}
end;


{ TfrmMain }

procedure TfrmMain.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmMain}
  SetDimensions(295,101,391,299);
  WindowTitle8 := 'SMS Sender';

  lbLabel1 := TWGLABEL.Create(self);
  with lbLabel1 do
  begin
    SetDimensions(8,8,97,16);
    Text := u8('Serial device:');
  end;

  chlDevice := TWGCHOICELIST.Create(self);
  with chlDevice do
  begin
    SetDimensions(8,28,114,22);
    Items.Add(u8('/dev/ttyS0'));
    Items.Add(u8('/dev/ttyS1'));
  end;

  lbLabel2 := TWGLABEL.Create(self);
  with lbLabel2 do
  begin
    SetDimensions(132,8,72,16);
    Text := u8('Baud rate:');
  end;

  chlSpeed := TWGCHOICELIST.Create(self);
  with chlSpeed do
  begin
    SetDimensions(132,28,78,22);
    Items.Add(u8('9600'));
    Items.Add(u8('19200'));
    Items.Add(u8('38400'));
    FocusItem := 2;
  end;

  lbLabel3 := TWGLABEL.Create(self);
  with lbLabel3 do
  begin
    SetDimensions(8,60,104,16);
    Text := u8('Phone number:');
  end;

  edPhone := TWGEDIT.Create(self);
  with edPhone do
  begin
    SetDimensions(8,80,202,22);
    Text := u8('');
    //Font := guistyle.EditFont2;
  end;

  lbLabel4 := TWGLABEL.Create(self);
  with lbLabel4 do
  begin
    SetDimensions(8,108,61,16);
    Text := u8('Message:');
  end;

  edMessage := TWGMEMO.Create(self);
  with edMessage do
  begin
    SetDimensions(8,128,371,104);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Lines.Add(u8(''));
    OnChange := @TextChange;
  end;

  btnSend := TWGBUTTON.Create(self);
  with btnSend do
  begin
    SetDimensions(8,268,81,24);
    Anchors := [anLeft,anBottom];
    Text := u8('Send');
    OnClick := @SendClick;
    imagename := 'stdimg.ok';
  end;

  btnExit := TWGBUTTON.Create(self);
  with btnExit do
  begin
    SetDimensions(295,268,81,24);
    Anchors := [anRight,anBottom];
    Text := u8('Exit');
    OnClick := @ExitClick;
    imagename := 'stdimg.cancel';
  end;

  lbLabel5 := TWGLABEL.Create(self);
  with lbLabel5 do
  begin
    SetDimensions(8,238,111,16);
    Anchors := [anLeft,anBottom];
    Text := u8('Number of chars:');
  end;

  txtCharNum := TWGLABEL.Create(self);
  with txtCharNum do
  begin
    SetDimensions(124,238,52,20);
    Anchors := [anLeft,anBottom];
    Text := u8('0');
    Font := guistyle.LabelFont2;
  end;

  {@VFD_BODY_END: frmMain}

  ActiveWidget := edPhone;
end;

var
  frmMain : TfrmMain;

procedure TfrmMain.ExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.TextChange(Sender: TObject);
begin
  txtCharNum.Text8 := IntToStr(length(GetSMSText));
end;

function TfrmMain.GetSMSText : string;
var
  s : string;
  n : integer;
begin
  s := edMessage.Text8;
  result := '';
  for n:=0 to edMessage.Lines.Count-1 do
  begin
    if (n > 0) then result := result + ' ';
    result := result + u16toGSM(edMessage.Lines[n]);
  end;
end;

procedure TfrmMain.SendClick(Sender: TObject);
var
  cmd : string;
  msg, pout : string;
  rcode : integer;
  s, phn : string;
  n : integer;
  frm : TfrmWait;
begin
  phn := '';
  s := edPhone.Text8;
  for n:=1 to length(s) do
    if s[n] in ['0'..'9'] then phn := phn + s[n];

  if phn = '' then
  begin
    ShowMessage8('Enter the phone number!');
    ActiveWidget := edPhone;
    Exit;
  end;

  if phn <> edPhone.Text8 then
  begin
    ShowMessage8('Check the phone number!');
    edPhone.Text8 := phn;
    ActiveWidget := edPhone;
    Exit;
  end;

  if GetSMSText = '' then
  begin
    ShowMessage8('Enter some message!');
    ActiveWidget := edMessage;
    Exit;
  end;

  cmd := 'gsmsendsms -d '+ chlDevice.Text8 + ' -b '+chlSpeed.Text8 +' '+phn;

  //ShowMessage8(cmd);

  frm := TfrmWait.Create(nil);
  frm.Show;

  GfxProcessMessages;

  pout := '';
  //rcode := ExecPrg('gsmsendsms -t 1234','abcd',pout);
  rcode := ExecPrg(cmd,GetSMSText,pout, 30);

  frm.Free;

  if rcode = 127 then
  begin
    ShowMessage8('Error executing gsmsendsms!'#10'Is the gsm library (gsm-utils on debian systems) installed?','Error');
  end
  else
  begin
    msg := '';
    if rcode = 0 then msg := 'The SMS has been sended successfully.'
                 else msg := 'Error sending the SMS message! ('+IntToStr(rcode)+')';
    if pout <> '' then msg := msg + #10 + 'gsmsendsms output:' + #10 + pout;

    ShowMessage8(msg);
  end;
  //ShowMessage8(GetSMSText);
end;

begin
  Writeln('LPTK SMS sender');

  GfxOpenDisplay('');

  frmMain := TfrmMain.Create(nil);
  frmMain.Show;

  GfxDoMessageLoop;

  GfxCloseDisplay;
end.
