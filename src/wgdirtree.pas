unit wgdirtree;

// Bugs or Feature Requests - mail to: Erik@Grohnwaldt.de
// For newer versions look at lptk.sourceforge.net or www.grohnwaldt.de
// $Log$
// Revision 1.10  2004/01/29 12:48:10  aegluke
// Windows-Changes
//
// Revision 1.9  2004/01/24 18:35:51  aegluke
// wgfiledialog-changes
//
// Revision 1.8  2004/01/19 18:19:35  aegluke
// TwgDirTreePopup added
//
// Revision 1.7  2004/01/14 08:22:31  aegluke
// Fixed wrong use of FindSubNode
//
// Revision 1.6  2004/01/09 17:29:56  aegluke
// Windows-Drive support - Update
//
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
    wgtree, classes, schar16, popupwindow, gfxbase, gfxwidget, gfxstyle, messagequeue, gfximagelist;

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
            property DirectoryIndex : word read FDirectoryIndex write SetDirectoryIndex;
    end;

    TwgDirTreePopupTree = class(TwgDirTree)
           protected
                    procedure HandleDoubleClick(AX, AY : Longint; AButton : Word; AShiftState : Word); override; // Hide the DoubleClick
    end;

    TwgDirTreePopupWindow = class(TPopupWindow)
      private
             FOldDirectory : String;
             FPopupDir : String;
             FDirTree : TwgDirTreePopupTree;
      protected
               procedure HandleKeyPress(var AKeyCode : Word; var AShiftState : Word; var AConsumed : Boolean); override;
               procedure HandleDoubleClick(AX, AY : Longint; AButton : Word; AShiftState : Word); override;
      public
            constructor Create(AOwner : TComponent); override;
            destructor Destroy; override;
            procedure DoShow; override;
            property OldDirectory : String read FOldDirectory write FOldDirectory;
            property DirTree : TwgDirTreePopupTree read FDirTree;
            property PopupDir : String read FPopupDir write FPopupDir;
    end;

    TwgDirTreePopup = class(TWidget)
       private
              FPopup : TwgDirTreePopupWindow;
              FDropDownRows : Word;
              FDroppedDown : Boolean;
              FBlockDrop : Boolean;
              FFont : TgfxFont;
              FHotTrack : Boolean;
       protected
                procedure MsgPopupClose(var AMsg : TMessageRec); message MSG_POPUPCLOSE;
                procedure SetDropDownRows(AValue : Word);
                procedure HandleMouseDown(AX, AY : Integer; AButton : Word; AShiftState : Word); override;
                procedure HandleMouseUp(AX, AY : Integer; AButton : Word; AShiftState : Word); override;
                procedure DoSetFocus; override;
                procedure SetActiveDirectory(AValue : String);
                procedure SelectionChange(ASender : TObject);
                function GetActiveDirectory : String;
                procedure SetImageList(AValue : TgfxImageList);
                function GetImageList : TgfxImageList;
                procedure SetImageIndex(AValue : Word);
                function GetImageIndex : Word;
                procedure SetShowImages(AValue : Boolean);
                function GetShowImages : Boolean;
                
       public
             procedure DoChange;
             procedure RePaint; override;
             procedure DropDown;
             constructor Create(AOwner : TComponent); override;
             destructor Destroy; override;
             property HotTrack : Boolean read FHotTrack write FHotTrack;
             property ShowImages : Boolean read GetShowImages write SetShowImages;
             property ImageIndex : Word read GetImageIndex write SetImageIndex;
             property ImageList : TgfxImageList read GetImageList write SetImageList;
             property DropDownRows : Word read FDropDownRows write SetDropDownRows;
             property ActiveDirectory : String read GetActiveDirectory write SetActiveDirectory;
	           property Font : TgfxFont read FFont;
       public
             onChange : TNotifyEvent;
    end;

implementation

uses
    sysutils{$IFDEF win32},windows{$ENDIF};

{ TwgDirTreePopupTree }

procedure TwgDirTreePopupTree.HandleDoubleClick(AX, AY : Longint; AButton : Word; AShiftState : Word);
begin
     MessageQueue.PostMessage(self, Owner, MSG_DOUBLECLICK, AX, AY, AButton);
end;

{ TwgDirTreePopup }

procedure TwgDirTreePopup.DoChange;
begin
     if Assigned(onChange) then onChange(Self);
end;

function TwgDirTreePopup.GetShowImages : Boolean;
begin
     Result := FPopup.DirTree.ShowImages;
end;

procedure TwgDirTreePopup.SetShowImages(AValue : Boolean);
begin
     FPopup.DirTree.ShowImages := AValue;
end;

function TwgDirTreePopup.GetImageIndex : Word;
begin
     result := FPopup.DirTree.DirectoryIndex;
end;

procedure TwgDirTreePopup.SetImageIndex(AValue : Word);
begin
     FPopup.DirTree.DirectoryIndex := AValue;
end;

function TwgDirTreePopup.GetImageList : TgfxImageList;
begin
     Result := FPopup.DirTree.ImageList;
end;

procedure TwgDirTreePopup.SetImageList(AValue : TgfxImageList);
begin
     FPopup.DirTree.ImageList := AValue;
end;

procedure TwgDirTreePopup.SelectionChange(ASender : TObject);
begin
     {$IFDEF DEBUG}
     writeln('TwgDirTreePopup.SelectionChange');
     {$ENDIF}
     RePaint;
     if HotTrack then
     begin
          DoChange;
          FPopup.OldDirectory := ActiveDirectory;
     end;
end;

procedure TwgDirTreePopup.SetActiveDirectory(AValue : String);
begin
     {$IFDEF DEBUG}
     writeln('TwgDirTreePopup.SetActiveDirectory');
     {$ENDIF}
     FPopup.DirTree.ActiveDirectory := AValue;
     RePaint;
     FPopup.OldDirectory := ActiveDirectory;
end;

function TwgDirTreePopup.GetActiveDirectory : String;
begin
     {$IFDEF DEBUG}
     writeln('TwgDirTreePopup.GetActiveDirectory');
     {$ENDIF}
     result := FPopup.DirTree.ActiveDirectory;
end;

procedure TwgDirTreePopup.DoSetFocus;
begin
     {$IFDEF DEBUG}
     writeln('TwgDirTreePopup.DoSetFocus');
     {$ENDIF}
     inherited DoSetFocus;
     FBlockDrop := False;
end;

procedure TwgDirTreePopup.MsgPopupClose(var AMsg : TMessageRec);
begin
     {$IFDEF DEBUG}
     writeln('TwgDirTreePopup.MsgPopupClose');
     {$ENDIF}
     FBlockDrop := FDroppedDown;
     if ActiveDirectory <> FPopup.OldDirectory then
     begin
          DoChange;
          FPopup.OldDirectory := ActiveDirectory;
     end;
end;

procedure TwgDirTreePopup.HandleMouseUp(AX, AY : Integer; AButton : Word; AShiftState : Word);
begin
     {$IFDEF DEBUG}
     writeln('TwgDirTreePopup.HandleMouseUp');
     {$ENDIF}
     inherited HandleMouseUp(AX, AY, AButton, AShiftState);
     if not FBlockDrop then
     begin
          DropDown;
     end
     else
     begin
          if FPopup.OldDirectory <> ActiveDirectory then
          begin
               DoChange;
               FPopup.OldDirectory := ActiveDirectory;
          end;
     end;
     FBlockDrop := False;
     FDroppedDown := FPopup.WinHandle > 0;
end;

procedure TwgDirTreePopup.HandleMouseDown(AX, AY : Integer; AButton : Word; AShiftState : Word);
begin
     {$IFDEF DEBUG}
     writeln('TwgDirTreePopup.HandleMouseDown');
     {$ENDIF}
     inherited HandleMouseDown(AX, AY, AButton, AShiftState);
     FDroppedDown := FPopup.WinHandle > 0;
end;

procedure TwgDirTreePopup.SetDropDownRows(AValue : Word);
begin
     {$IFDEF DEBUG}
     writeln('TwgDirTreePopup.SetPopupLines');
     {$ENDIF}
     if AValue = 0 then
        AValue := 1;
     FDropDownRows := AValue;
end;

procedure TwgDirTreePopup.DropDown;
begin
     {$IFDEF DEBUG}
     writeln('TwgDirTreePopup.DropDown');
     {$ENDIF}
     FPopup.Width := Width;
     FPopup.Height := FPopup.DirTree.GetNodeHeight * FDropDownRows;
     FDroppedDown := True;
     FPopup.ShowAt(WinHandle,0,Height);
     FPopup.PopupDir := ActiveDirectory;
end;

procedure TwgDirTreePopup.RePaint;
begin
     {$IFDEF DEBUG}
     writeln('TwgDirTreePopup.RePaint');
     {$ENDIF}
     if not Windowed then exit;
     Canvas.Clear(FBackgroundColor);
     if Focused then
        Canvas.SetColor(clWidgetFrame)
     else
         Canvas.SetColor(clInactiveWGFrame);
     Canvas.DrawRectangle(0,0,Width,Height);
     Canvas.DrawString16(4,Height div 2 - FFont.Height div 2,Str8To16(ActiveDirectory));
     DrawButtonFace(Canvas, width - height + 1, 1, height - 2, height - 2);
     DrawDirectionArrow(Canvas, Width - Height + 1, 1, Height - 2, Height - 2, 1);
end;

constructor TwgDirTreePopup.Create(AOwner : TComponent);
begin
     inherited Create(AOwner);
     FPopup := TwgDirTreePopupWindow.Create(Self);
     FDropDownRows := 8;
     OnChange := nil;
     FHotTrack := False;
     FDroppedDown := False;
     FBlockDrop := False;
     FFocusAble := True;
     FDroppedDown := True;
     FBackgroundColor := clChoiceListBox;
     FFont := guistyle.ListFont;
     FPopup.DirTree.onChange := {$IFDEF fpc}@{$ENDIF}SelectionChange;
     FPopup.OldDirectory := ActiveDirectory;
end;

destructor TwgDirTreePopup.Destroy;
begin
     FPopup.Destroy;
     inherited Destroy;
end;

{ TwgDirTreePopupWindow }

procedure TwgDirTreePopupWindow.HandleDoubleClick(AX, AY : Longint; AButton : Word; AShiftState : Word);
begin
     Close;
     if OldDirectory <> FDirTree.ActiveDirectory then TwgDirTreePopup(Owner).DoChange;
end;

procedure TwgDirTreePopupWindow.HandleKeyPress(var AKeyCode : Word; var AShiftState : Word; var AConsumed : Boolean);
begin
     inherited HandleKeyPress(AKeyCode, AShiftState, AConsumed);
     case AKeyCode of
          KEY_ESC:
          begin
               Close;
               FDirTree.ActiveDirectory := FPopupDir;
               AConsumed := True;
               TwgDirTreePopup(Owner).RePaint;
          end;
          KEY_ENTER:
          begin
               Close;
               MessageQueue.PostMessage(self, Owner, MSG_KEYPRESS, KEY_TAB, 0, 0);
               if OldDirectory <> FDirTree.ActiveDirectory then TwgDirTreePopup(Owner).DoChange;
               OldDirectory := FDirTree.ActiveDirectory;
               AConsumed := True;
          end;
     end;
end;

destructor TwgDirTreePopupWindow.Destroy;
begin
     DirTree.Free;
     inherited Destroy;
end;

procedure TwgDirTreePopupWindow.DoShow;
begin
     DirTree.SetDimensions(0,0,Width,Height);
     inherited DoShow;
     ActiveWidget := DirTree;
end;

constructor TwgDirTreePopupWindow.Create(AOwner : TComponent);
begin
     inherited Create(AOwner);
     FDirTree := TwgDirTreePopupTree.Create(Self);
end;

{ TwgDirTree }

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
      RootNode.AppendText8(ADrive[1]+':');
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
     FDirectoryIndex := AValue;
     while ANode <> nil do
     begin
          if (ANode.count > 0) then
          begin
             ANode := ANode.FirstSubNode;
             ANode.ImageIndex := AValue;
          end
          else
          begin
               if ANode.next <> nil then
               begin
                  ANode := ANode.next;
                  ANode.ImageIndex := AValue;
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
                    ANode.ImageIndex := AValue;
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
    ActiveDirectory := GetAbsoluteDir(Selection);
    inherited DoChange;
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
	      aNode := aNode.FindSubNode(Str8To16(searchstr), false);
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
		      tmpNode := aNode.FindSubNode(Str8To16(searchstr), false);
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
		      tmpNode := aNode.FindSubNode(Str8To16(searchstr), false);
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
