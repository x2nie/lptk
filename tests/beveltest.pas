{*
 *  A kovetkezo nagy problema merult fel: az AfterCreate-ben a Bevel-t
 *  mindenkeppen a legelso helyen kell letrehozni, kulonben eltakarja
 *  a ket Label-t. Nem lenne-e valamilyen mod arra, hogy az ilyen
 *  komponenseket ugymond "atlatszora" allitsuk. Mert ha a terulete
 *  transzparens lenne, akkor csak a kozvetlenul kirajzolt forma
 *  jelenne meg, es nem takarna le az alatta levo elemeket?
 *}

program BevelTest;

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses SysUtils, Classes, gfxBase, gfxForm, wgButton, wgLabel, wgBevel, SChar16;

type
  TMainForm = class(TgfxForm)
    Bevel   : TwgBevel;
    Label1  : TwgLabel;
    Label2  : TwgLabel;
    Button1 : TwgButton;
    Button2 : TwgButton;
    procedure AfterCreate; override;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  end;

var
  MainForm: TMainForm;

procedure TMainForm.AfterCreate;
begin
  WindowTitle8 := 'Bevel test';
  Height := 205;
  Width := 300;
  Bevel := CreateBevel(Self, 10, 10, 280, 150, bsBox, bsRaised);
  Label1 := TwgLabel.Create(Self);
  Label1.SetDimensions(30, 55, 220, 20);
  Label1.Text8 := 'Bevel is Raised';
  Label2 := TwgLabel.Create(Self);
  Label2.SetDimensions(30, 90, 220, 20);
  Label2.Text8 := 'Bevel style is bsBox';
  Button1 := TwgButton.Create(Self);
  Button1.SetDimensions(10, 170, 100, 25);
  Button1.Text := str8to16('Lowered');
  Button1.OnClick := {$ifdef FPC}@{$endif}Button1Click;
  Button2 := TwgButton.Create(Self);
  Button2.SetDimensions(150, 170, 140, 25);
  Button2.Text := str8to16('bsFrame');
  Button2.OnClick := {$ifdef FPC}@{$endif}Button2Click;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if Bevel.Style = bsRaised then Bevel.Style := bsLowered
                            else Bevel.Style := bsRaised;
  if Bevel.Style = bsRaised then
  begin
     Button1.Text := str8to16('Lowered');
     Label1.Text8 := 'Bevel is Raised';
  end else begin
    Button1.Text := str8to16('Raised'); 
    Label1.Text8 := 'Bevel is Lowered';
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  case Bevel.Shape of
    bsBottomLine : begin
                     Bevel.Shape  := bsRightLine;
                     Label2.Text8 := 'Bevel style is bsRightLine';
                     Button2.Text := str8to16('bsBox');
                   end;
    bsBox :        begin
                     Bevel.Shape  := bsFrame;
                     Label2.Text8 := 'Bevel style is bsFrame';
                     Button2.Text := str8to16('bsTopLine');
                   end;
    bsFrame :      begin
                     Bevel.Shape  := bsTopLine;
                     Label2.Text8 := 'Bevel style is bsTopLine';
                     Button2.Text := str8to16('bsLeftLine');
                   end;
    bsLeftLine :   begin
                     Bevel.Shape  := bsBottomLine;
                     Label2.Text8 := 'Bevel style is bsBottomLine';
                     Button2.Text := str8to16('bsRightLine');
                   end;
    bsRightLine :  begin
                     Bevel.Shape  := bsBox;
                     Label2.Text8 := 'Bevel style is bsBevel';
                     Button2.Text := str8to16('bsFrame');
                   end;
    bsTopLine :    begin
                     Bevel.Shape  := bsLeftLine;
                     Label2.Text8 := 'Bevel style is bsLeftLine';
                     Button2.Text := str8to16('bsBottomLine');
                   end;
  end;
end;

begin
  gfxOpenDisplay('');
  MainForm := TMainForm.Create(nil);
  MainForm.Show;
  gfxDoMessageLoop;
  gfxCloseDisplay;
end.
