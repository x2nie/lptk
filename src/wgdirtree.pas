unit wgdirtree;

// Bugs or Feature Requests - mail to: Erik@Grohnwaldt.de
// For newer versions look at lptk.sourceforge.net or www.grohnwaldt.de
// $Log$
// Revision 1.1  2003/10/29 12:48:47  aegluke
// simple directory tree - first release - not tested on windows
//

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ENDIF}
{$DEFINE DEBUG}

interface

uses
    wgtree, classes, schar16;

const 
    {$IFDEF win32}
	cDirSeparator = '\';
    {$ELSE}
	cDirSeparator = '/';
    {$ENDIF}
    
type 
    TwgDirTree = class(TwgTree)
	private
	    FActiveDirectory : string;
	    procedure SetActiveDirectory(aValue : string);
	protected
	    function GetAbsoluteDir(aNode : TwgTreeNode) : string;
	    procedure DoChange; override;
	public
	    constructor Create(aOwner : TComponent); override;
	    procedure ReadDirectories(aParentNode : TwgTreeNode);	    
	    // read's the directory entries of the given dirname in the parent-node.text	    
	    property ActiveDirectory : string read FActiveDirectory write SetActiveDirectory;
    end;

implementation

uses
    sysutils;

procedure TwgDirTree.DoChange;
begin
    inherited DoChange;
    ActiveDirectory := GetAbsoluteDir(Selection);
end;

constructor TwgDirTree.Create(aOwner : TComponent);
begin
    inherited Create(aOwner);
    ActiveDirectory := GetCurrentDir;
end;

procedure TwgDirTree.ReadDirectories(aParentNode : TwgTreeNode);
var
    Items : TStringList;
    r : TSearchRec;
    i : integer;
begin
    Items := TStringList.Create;
    {$IFDEF DEBUG}writeln('ReadDirectories');{$ENDIF}
    if FindFirst(GetAbsoluteDir(aParentNode)+'*',faAnyFile,r)=0 then
    begin
	repeat
	    if (faDirectory and r.attr = faDirectory) and (r.name <> '..') and (r.name <> '.') then Items.Append(Str8To16(r.name));
	until FindNext(r) <> 0;
    end;
    Items.Sort;
    FindClose(r);    
    
    // all directory entries are in the stringlist and sorted
    
    aParentNode.Clear;
    for i := 0 to Items.Count - 1 do
	aParentNode.AppendText(Items[i]);
    Items.Destroy;
end;

function TwgDirTree.GetAbsoluteDir(aNode : TwgTreeNode) : string;
var
    DirStr : String;
begin
    DirStr := '';
    if aNode = nil then
    begin
	DirStr := GetCurrentDir;
	{$IFDEF win32}
	    Result := copy(DirStr,1,pos('\',DirStr));	// drive-name
	{$ELSE}
	    Result := cDirSeparator;			// root
	{$ENDIF}
	exit;
    end;
    while aNode.Parent <> nil do
    begin
	DirStr := aNode.Text8+cDirSeparator+DirStr;
	aNode := aNode.Parent;
    end;
    result := DirStr;
end;

procedure TwgDirTree.SetActiveDirectory(aValue : string);
var
    aNode : TwgTreeNode;
    searchstr : string;
    tmpNode : TwgTreeNode;
begin
    {$IFDEF DEBUG}
    writeln('SetactiveDirectory:',aValue);
    {$ENDIF}
    if aValue = '' then aValue := GetCurrentDir;    
    if (FActiveDirectory = '') then // nothing shown
    begin
	if aValue[Length(AValue)] <> cDirSeparator then aValue := aValue + cDirSeparator;
	FActiveDirectory := aValue;	
	RootNode.Clear;		
	RootNode.AppendText8(copy(aValue,1,pos(cDirSeparator,aValue)-1));
	ReadDirectories(RootNode.FirstSubNode);
	delete(aValue,1,pos(cDirSeparator,aValue));	
	aNode := RootNode.FirstSubNode;
	while pos(cDirSeparator,aValue) <> 0 do
	begin
	    searchstr := copy(aValue,1,pos(cDirSeparator,aValue)-1);
	    aNode := aNode.FindSubNode(Str8To16(searchstr));
	    delete(aValue,1,pos(cDirSeparator,aValue));
	    ReadDirectories(aNode);
	end;
    end
    else
    begin	
	if aValue[Length(aValue)] <> cDirSeparator then aValue := aValue + cDirSeparator;
	searchstr := aValue;	
	FActiveDirectory := aValue;
	// drive into already read pathes
	aNode := RootNode;
	while pos(cDirSeparator,aValue) <> 0 do	// liest alle verzeichnisse ein
	begin
	    searchstr := copy(aValue,1,pos(cDirSeparator,aValue)-1);
	    delete(aValue,1,pos(cDirSeparator,aValue));
	    if aNode.Count > 0 then
	    begin
		tmpNode := aNode.FindSubNode(Str8To16(searchstr));
		if tmpNode = nil then
		begin
		    writeln('Verzeichnis nicht gefunden: ',searchstr);
// TODO - messagebox with error
		    break;
		end
		else
		begin
		    aNode := tmpNode;
		    if length(aValue) <> 0 then aNode.Expand;
		end;
	    end
	    else	// directory not read yet
	    begin
		ReadDirectories(aNode);
		tmpNode := aNode.FindSubNode(Str8To16(searchstr));
		if tmpNode = nil then
		begin
		    writeln('Verzeichnis nicht gefunden: ',searchstr);
// TODO - Messagebox with error
		    break;
		end
		else
		begin
		    aNode := tmpNode;
		    if length(aValue) <> 0 then aNode.Expand;
		end;
	    end;
	end;
    end;
    if aNode.count = 0 then 
    begin
	ReadDirectories(aNode);
	aNode.Collapse;
    end;
    Selection := aNode;
    RePaint;
end;

end.