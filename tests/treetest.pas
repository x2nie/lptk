program Treetest;

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses gfxbase, gfxform, wgtree, schar16, gfximagelist, gfxbmpimage;

type
    TMainForm = class(TGfxForm)
	public
              ImageList : TgfxImageList;
              ImageItem : TgfxImageItem;
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
    ImageItem := TgfxImageItem.Create;
    ImageItem.LoadFromFile('/home/aegluke/folder.bmp');
    ImageList := TgfxImageList.Create;
    ImageList.Item[0] := ImageItem;
    tree := twgtree.create(self);
    tree.ImageList := ImageList;
    tree.ShowImages := true;
    tree.top := 40;
    tree.left := 10;
    tree.height := 100;
    tree.Anchors := [anBottom, anleft, anright, antop];
    tree.width := 150;
    tree.rootnode.text := Str8to16('Root');
    tree.rootnode.expand;
    tree.rootnode.AppendText(Str8to16('SubNode1'));
    tree.ShowColumns := true;
    subnode := tree.rootnode.FindSubNode(Str8to16('SubNode1'));
    tree.selection:= subnode;
    subnode.AppendText(Str8To16('SubSubNode1'));
    subnode.AppendText(Str8To16('SubSubNode2'));
    subnode.AppendText(Str8To16('SubSubNode3'));
    subnode.AppendText(Str8To16('SubSubNode4'));
    subnode.AppendText(Str8To16('SubSubNode5'));
    subnode.AppendText(Str8To16('SubSubNode6'));
    subnode := subnode.firstsubnode;
    SubNode.ImageIndex := 0;
    subnode := subnode.next;
    SubNode.ImageIndex := 0;
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
