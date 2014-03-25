unit Unit1;

{$ifdef fpc}
{$mode delphi}{$h+}
{$endif}

interface

uses  
  Classes, SysUtils, hd_defs, hd_main, hd_form, hd_widget;

type

  { TpgfForm1 }

  TpgfForm1 = class(TpgfForm)
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AfterCreate; override;

    procedure HandlePaint; override;
  end;

var
  pgfForm1: TpgfForm1;

implementation

{.$R *.dfm}


{ TpgfForm1 }

procedure TpgfForm1.AfterCreate;
begin
  //inherited AfterCreate;
  SetPosition(100,100,300,300);

  //Sizeable := false;
  MinWidth  := 200;
  MinHeight := 150;
  WindowTitle := 'valami';
end;

procedure TpgfForm1.HandlePaint;
begin
  inherited HandlePaint;
end;

end.
 
