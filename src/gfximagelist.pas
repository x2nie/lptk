unit gfximagelist;
{$IFDEF fpc}
        {$mode objfpc}
        {$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, gfxbase;

type
    EItemExists = class(Exception);
    
    TgfxImageList = class;
    
    TgfxImageItem = class
            private
                   FImage : TgfxImage;
                   FIndex : Word;
                   FImageList : TgfxImageList;
            protected
                     procedure SetImageList(AImageList : TgfxImageList);
                     procedure SetIndex(AIndex : word);
                     procedure SetImage(AImage : TgfxImage);
            public
                   property Index : Word read FIndex write SetIndex;
                   property Image : TgfxImage read FImage write SetImage;
                   property ImageList : TgfxImageList read FImageList write SetImageList;
                   procedure LoadFromFile(AFileName : String);
                   destructor Destroy; override;
                   constructor Create; {$IFNDEF FPC}overload; {$ENDIF}
                   constructor Create(AImageList : TgfxImageList; AIndex : Word; AImage : TgfxImage); {$IFNDEF FPC}overload;{$ENDIF}
                   constructor Create(AFileName : string; AIndex : Word); {$IFNDEF FPC}overload;{$ENDIF}
     end;
    
    TgfxImageList = class
           private
                  FList : TList;
                  function GetFListIndex(AIndex : Integer) : Integer;
           protected
                    function GetItem(AIndex : integer) : TgfxImageItem;
                    procedure SetItem(AIndex : integer; AItem : TgfxImageItem);
           public
                 property Item[AIndex : integer] : TgfxImageItem read GetItem write SetItem;
                 procedure AddItemFromFile(AFileName : String; AIndex : Word);
                 procedure AddImage(AImage : TGfxImage; AIndex : Word);
                 procedure RemoveIndex(AIndex : integer);
                 function GetMaxItem : word;
                 constructor Create;
                 destructor Destroy; override;
    end;

implementation

uses
    gfxbmpimage;

procedure TgfxImageList.AddItemFromFile(AFileName : String; AIndex : Word);
var
    AImageItem : TgfxImageItem;
begin
    {$IFDEF DEBUG}
    writeln('TgfxImageList.AddItemFromFile');
    {$ENDIF}
    AImageItem := TgfxImageItem.Create;
    AImageItem.LoadFromFile(AFileName);
    Item[AIndex] := AImageItem;
end;

procedure TgfxImageList.AddImage(AImage: TGfxImage; AIndex: Word);
var
    AImageItem : TgfxImageItem;
begin
    AImageItem := TgfxImageItem.Create;
    AImageItem.Image := AImage;
    Item[AIndex] := AImageItem;
end;

function TgfxImageList.GetFListIndex(AIndex : Integer) : Integer;
var
   ACounter : integer;
begin
     {$IFDEF DEBUG}
     writeln('TgfxImageList.GetFListIndex');
     {$ENDIF}
     result := -1;
     for ACounter := 0 to FList.Count - 1 do
         if TgfxImageItem(FList[ACounter]).Index = AIndex then
         begin
              result := ACounter;
              Break;
         end;
end;

procedure TgfxImageList.RemoveIndex(AIndex : integer);
begin
     {$IFDEF DEBUG}
     writeln('TgfxImageList.RemoveIndex');
     {$ENDIF}
     AIndex := GetFListIndex(AIndex);
     if AIndex <> -1 then
     begin
        TgfxImageItem(FList[AIndex]).Destroy;
        FList.Delete(AIndex);
     end;
end;

function TgfxImageList.GetItem(AIndex : integer) : TgfxImageItem;
var
   AFindIndex : integer;
begin
     {$IFDEF DEBUG}
     writeln('TgfxImageList.GetItem');
     {$ENDIF}
     result := nil;
     AFindIndex := GetFListIndex(AIndex);
     if AFindIndex > -1 then result := TgfxImageItem(FList[AFindIndex]);
end;

procedure TgfxImageList.SetItem(AIndex : integer; AItem : TgfxImageItem);
begin
     if AItem = nil then exit;
     if GetItem(AIndex) = AItem then exit;
     RemoveIndex(AIndex);      // delete existing Item
     AItem.Index := AIndex;
     FList.Add(AItem);
end;


function TgfxImageList.GetMaxItem : word;
var
   ACounter : integer;
begin
     result := 0;
     for ACounter := 0 to FList.Count - 1 do
        if TgfxImageItem(FList[ACounter]).Index > result then result := TgfxImageItem(FList[ACounter]).Index;
end;

destructor TgfxImageList.Destroy;
var
   ACounter : integer;
begin
     for ACounter := 0 to FList.Count - 1 do
         TgfxImageItem(FList[ACounter]).Destroy;  // frees images
     FList.Destroy;
     inherited Destroy;
end;

constructor TgfxImageList.Create;
begin
     FList := TList.Create;
end;

// TgfxImageItem

procedure TgfxImageItem.SetImage(AImage : TgfxImage);
begin
     {$IFDEF DEBUG}
     writeln('TgfxImageItem.SetImage');
     {$ENDIF}
     if AImage <> FImage then
     begin
          FImage := AImage;
          FImage.CreateMaskFromSample(0,0);
     end;
end;

procedure TgfxImageItem.SetIndex(AIndex : Word);
begin
     {$IFDEF DEBUG}
     writeln('TgfxImageItem.SetIndex');
     {$ENDIF}
     if AIndex <> FIndex then
     begin
          if ImageList <> nil then ImageList.RemoveIndex(AIndex);
          FIndex := AIndex;
     end;
end;

constructor TgfxImageItem.Create(AImageList : TgfxImageList; AIndex : Word; AImage : TgfxImage);
begin
     if AImageList = nil then exit;
     FImage := AImage;
     FIndex := AIndex;
     FImageList := nil;
     ImageList := AImageList;
end;

constructor TgfxImageItem.Create;
begin
     ImageList := nil;
     FIndex := 0;
     FImage := nil;
end;

constructor TgfxImageItem.Create(AFileName : string; AIndex : Word);
begin
     {$IFDEF DEBUG}
     writeln('TgfxImageItem.Create('+AFileName+',',AIndex,')');
     {$ENDIF}
     Index := AIndex;
     LoadFromFile(AFileName);
end;

destructor TgfxImageItem.Destroy;
begin
     if FImage <> nil then
        FImage.Destroy;
     inherited Destroy;
end;

procedure TgfxImageItem.SetImageList(AImageList : TgfxImageList);
begin
     if AImageList = nil then
     begin
          FImageList := nil;
     end
     else
     begin
          if FImageList <> nil then
          begin
               FImageList.RemoveIndex(Index);
          end;
          FImageList := AImageList;
          FImageList.Item[Index] := self;
     end;
end;

procedure TgfxImageItem.LoadFromFile(AFileName : String);
begin
     {$IFDEF DEBUG}
     writeln('TgfxImageItem.LoadFromFile');
     {$ENDIF}
     if FImage <> nil then FImage.Destroy;
     FImage := TgfxImage.Create;
     FImage.LoadFromFile(AFileName);
end;

end.

