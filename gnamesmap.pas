unit gnamesmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrHashMap;

type

  TOtuName = class(TObject)
    name: String;
  end;

  { TNamesMapLoader }

  TNamesMapLoader = class(TObject)
    private
      FNameList: TList;
    public
      constructor Create;
      destructor Destroy; override;
      function LoadMap(aFile: String): Boolean;
      function LoadMap2(aFileName: String): Boolean;

  end;

  { TLoadNamesMapThread }

  TLoadNamesMapThread = class(TThread)
    private
      FEndTime: TDateTime;
      FIsSuccess: Boolean;
      FLogStrings: TStringList;
      FMapFile: String;
      FMapLoader: TNamesMapLoader;
      FStartTime: TDateTime;
      procedure LoadNamesMap;
      procedure SetLogStrings(AValue: TStringList);
      procedure SetMapFile(AValue: String);
    public
      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;
      procedure Execute; override;

      property MapFile: String read FMapFile write SetMapFile;
      property IsSuccess: Boolean read FIsSuccess;
      property StartTime: TDateTime read FStartTime;
      property EndTime: TDateTime read FEndTime;
      property LogStrings: TStringList read FLogStrings write SetLogStrings;
  end;

var
  NamesMap: TStringHashMap;

implementation

uses
  ttconst, dateutils;

{ TLoadNamesMapThread }

procedure TLoadNamesMapThread.LoadNamesMap;
var
  temp: String;
begin
  if not FileExists(MapFile) then
    Exit;
  FStartTime := Now;
  FIsSuccess := FMapLoader.LoadMap2(FMapFile);
  FEndTime := Now;
  if Assigned(FLogStrings) then
  begin
    try
      LogfileCriticalSect.Acquire;
      temp := 'Load names: execution time = ' + IntToStr(MilliSecondsBetween(FEndTime, FStartTime)) + ' ms';
      FLogStrings.Add(temp);
    finally
      LogfileCriticalSect.Release;
    end;
  end;
end;

procedure TLoadNamesMapThread.SetLogStrings(AValue: TStringList);
begin
  if FLogStrings=AValue then Exit;
  FLogStrings:=AValue;
end;

procedure TLoadNamesMapThread.SetMapFile(AValue: String);
begin
  if FMapFile=AValue then Exit;
  FMapFile:=AValue;
end;

constructor TLoadNamesMapThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FMapLoader := TNamesMapLoader.Create;
  FIsSuccess := False;
  FLogStrings := nil;
end;

destructor TLoadNamesMapThread.Destroy;
begin
  if Assigned(FMapLoader) then
    FMapLoader.Free;
  inherited Destroy;
end;

procedure TLoadNamesMapThread.Execute;
begin
  LoadNamesMap;
end;

{ TNamesMapLoader }

constructor TNamesMapLoader.Create;
begin
  FNameList := TList.Create;
end;

destructor TNamesMapLoader.Destroy;
var
  i: Integer;
begin
  if FNameList.Count > 0 then
    for i := 0 to FNameList.Count - 1 do
      TOtuName(FNameList[i]).Free;
  inherited Destroy;
end;

function TNamesMapLoader.LoadMap(aFile: String): Boolean;
var
  aList: TStringList = nil;
  i: Integer;
  key: String;
  value: String;
  aName: TOtuName;
begin
  Result := False;
  if not FileExists(aFile) then
    raise Exception.Create('IDs to names map file does not exist');
  try
    aList := TStringList.Create;
    aList.LoadFromFile(aFile);
    if aList.Count > 0 then
    begin
      NamesMap := TStringHashMap.Create(TCaseSensitiveTraits.Create, 1023);
      for i := 0 to aList.Count - 1 do
      begin
        key := aList.Names[i];
        value := aList.ValueFromIndex[i];
        aName := TOtuName.Create;
        aName.Name := value;
        FNameList.Add(aName);
        NamesMap[key] := aName;
      end;
      Result := True;
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TNamesMapLoader.LoadMap2(aFileName: String): Boolean;
var
  aFile: Text;
  key: String;
  value: String;
  aName: TOtuName;
  temp: String;
begin
  Result := False;
  if not FileExists(aFileName) then
    raise Exception.Create('IDs to names map file does not exist');
  try
    NamesMap := TStringHashMap.Create(TCaseSensitiveTraits.Create, 1023);
    AssignFile(aFile, aFileName);
    Reset(aFile);
    while not EOF(aFile) do
    begin
      ReadLn(aFile, temp);
      key := Copy(temp, 1, Pos('=', temp)-1);
      value := Copy(temp, Pos('=', temp)+1, Length(temp));
      aName := TOtuName.Create;
      aName.Name := value;
      FNameList.Add(aName);
      NamesMap[key] := aName;
    end;
    Result := True;
  finally
    CloseFile(aFile);
  end;
end;

end.

