program Treetest;

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses gfxbase, gfxform, wgtree, schar16;

type
    TMainForm = class(TGfxForm)
	public
	    tree : twgtree;
	    procedure AfterCreate; override;
    end;

procedure TMainForm.AfterCreate;
var
    subnode : twgtreenode;
begin
    Height := 250;
    Width := 200;
    inherited AfterCreate;
    
    tree := twgtree.create(self);
    tree.top := 40;
    tree.left := 10;
    tree.space := 20;
    tree.height := 100;
    tree.Anchors := [anBottom, anleft, anright, antop];
    tree.width := 150;
    tree.rootnode.text := Str8to16('Root');
    tree.rootnode.expand;
    tree.rootnode.AppendText(Str8to16('SubNode1'));
    
    subnode := tree.rootnode.FindSubNode(Str8to16('SubNode1'));
    
    if subnode = nil then
    begin
      Writeln('nil');
//      Exit;
    end;
    
{    subnode.expand; }
    tree.selection:= subnode;
    subnode.AppendText(Str8To16('SubSubNode1'));
    subnode.AppendText(Str8To16('SubSubNode2'));
    subnode.AppendText(Str8To16('SubSubNode3'));
    subnode.AppendText(Str8To16('SubSubNode4'));
    subnode.AppendText(Str8To16('SubSubNode5'));
    subnode.AppendText(Str8To16('SubSubNode6'));
    subnode := subnode.getfirstsubnode;
    subnode := subnode.next;
    subnode.AppendText(Str8To16('SubSubSubNode1'));
    subnode.AppendText(Str8To16('SubSubSubNode2'));
    tree.rootnode.AppendText(Str8to16('SubNode2'));    
end;

var
    MainForm : TMainForm;

begin
    gfxOpenDisplay('');
    MainForm := TMainForm.Create(nil);
    MainForm.Show;
    gfxDoMessageLoop;
    gfxCloseDisplay;
end.
