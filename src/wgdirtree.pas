unit wgdirtree;

// Bugs or Feature Requests - mail to: Erik@Grohnwaldt.de
// For newer versions look at lptk.sourceforge.net or www.grohnwaldt.de
// $Log$
// Revision 1.5  2004/01/09 14:29:31  aegluke
// Windows-Drive support
//
// Revision 1.4  2004/01/02 19:35:02  aegluke
// SetDirectoryIndex-Update
//
// Revision 1.3  2004/01/02 19:13:17  aegluke
// ImageList-Support
//
// Revision 1.2  2003/10/30 11:24:41  aegluke
// pre-read not opened directories
//
// Revision 1.1  2003/10/29 12:48:47  aegluke
// simple directory tree - first release - not tested on windows
//

{$IFDEF FPC}
    {$mode objfpc}
    {$H+}
{$ENDIF}
{//$DEFINE DEBUG}

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
      FDirectoryIndex : word;
	    procedure SetActiveDirectory(aValue : string);
	protected
	    function GetAbsoluteDir(aNode : TwgTreeNode) : string;
	    procedure DoChange; override;
	    procedure DoExpand(aNode : TwgTreeNode); override;
      procedure SetDirectoryIndex(AValue : Word);
      {$IFDEF win32}
      procedure ReadDriveNames;
      {$endif}
	public
	    constructor Create(aOwner : TComponent); override;
	    procedure ReadDirectories(aParentNode : TwgTreeNode);	    
	    // read's the directory entries of the given dirname in the parent-node.text	    
	    property ActiveDirectory : string read FActiveDirectory write SetActiveDirectory;
      property DirectoryIndex : word read FDirectoryIndex write FDirectoryIndex;
    end;

implementation

uses
    sysutils{$IFDEF win32},windows{$ENDIF};

{$IFDEF win32}
procedure TwgDirTree.ReadDriveNames;
var
  ADrive : String;
  ACounter : Integer;
  ANumber : Integer;
begin
  for ACounter := 0 to 25 do
  begin
    ADrive := Chr(Ord('A')+ACounter) + ':\';
    ANumber := Windows.GetDriveType(PChar(ADrive));
    if ANumber <> 1 then
    begin
      RootNode.AppendText8(ADrive+':');
    end;
  end;
end;
{$ENDIF}

procedure TwgDirTree.SetDirectoryIndex(AValue : Word);
// Sets the new Directory-Image-Index to all SubNodes
var
   ANode : TwgTreeNode;
begin
     {$IFDEF DEBUG}
     writeln('TwgDirTree.SetDirectoryIndex');
     {$ENDIF}
     ANode := RootNode;
     ANode.ImageIndex := AValue;
     ANode := RootNode.FirstSubNode;
     while ANode <> nil do
     begin
          if (ANode.count > 0) then
          begin
             ANode := ANode.FirstSubNode;
             ANode.ImageIndex := DirectoryIndex;
          end
          else
          begin
               if ANode.next <> nil then
               begin
                  ANode := ANode.next;
                  ANode.ImageIndex := DirectoryIndex;
               end
               else
               begin
                    while ANode.next = nil do
                    begin
                         ANode := ANode.parent;
                         if ANode = nil then
                         begin
                              Exit;
                         end;
                    end;
                    ANode := ANode.next;
                    ANode.ImageIndex := DirectoryIndex;
               end;
          end;
     end;
end;

procedure TwgDirTree.DoExpand(aNode : TwgTreeNode);
var
    tmpNode : TwgTreeNode;
begin
    inherited DoExpand(aNode);
    tmpNode := aNode.FirstSubNode;
    while tmpNode <> nil do
    begin
	if TmpNode.Count = 0 then
	begin
	    ReadDirectories(tmpNode);
	    tmpNode.Collapse;
	end;
	TmpNode := TmpNode.next;
    end;
end;

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
    AItem : TwgTreeNode;
begin
    Items := TStringList.Create;
    {$IFDEF DEBUG}writeln('ReadDirectories');{$ENDIF}
    if FindFirst(GetAbsoluteDir(aParentNode)+'*',faAnyFile,r)=0 then
    begin
	    repeat
	      if (faDirectory and r.attr = faDirectory) and (r.name <> '..') and (r.name <> '.') then
                 Items.Append(Str8To16(r.name));
	    until FindNext(r) <> 0;
    end;
    Items.Sort;
    Sysutils.FindClose(r);

    // all directory entries are in the stringlist and sorted

    aParentNode.Clear;
    for i := 0 to Items.Count - 1 do
    begin
	AItem := aParentNode.AppendText(Items[i]);
        AItem.ImageIndex := FDirectoryIndex;
    end;
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
      {$IFDEF win32}
      ReadDriveNames;
      {$ELSE}
	      RootNode.AppendText8(copy(aValue,1,pos(cDirSeparator,aValue)-1));
      {$ENDIF}
	    ReadDirectories(RootNode.FirstSubNode);
	    delete(aValue,1,pos(cDirSeparator,aValue));
	    aNode := RootNode.FirstSubNode;
	    while aNode <> nil do	// on windows - maybe there are more than one drive :)
	    begin
	      ReadDirectories(aNode);
	      aNode.Collapse;
	      aNode := aNode.Next;
	    end;
	    aNode := RootNode.FirstSubNode.FirstSubNode;
	    while aNode <> nil do
	    begin
	      ReadDirectories(aNode);
	      aNode.Collapse;
	      aNode := aNode.Next;
	    end;
	    aNode := RootNode.FirstSubNode;
	    while pos(cDirSeparator,aValue) <> 0 do
	    begin
	      searchstr := copy(aValue,1,pos(cDirSeparator,aValue)-1);
	      aNode := aNode.FindSubNode(Str8To16(searchstr));
	      aNode.Expand;
	      delete(aValue,1,pos(cDirSeparator,aValue));
	      ReadDirectories(aNode);
	      tmpNode := aNode.FirstSubNode;
	      while tmpNode <> nil do
	      begin
		      ReadDirectories(tmpNode);
		      tmpNode.Collapse;
		      tmpNode := tmpNode.Next;
	      end;
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
		      tmpNode := aNode.FirstSubNode;
		      while tmpNode <> nil do
		      begin
		        ReadDirectories(tmpNode);
		        tmpNode.Collapse;
		        tmpNode := tmpNode.Next;
		      end;
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
