unit gnamestoidsmapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gtaxonomicrank;

type


  { TNodeNameToIdsMap }

  TNodeNameToIdsMap = class(TObject)
    private
      FClassId: Integer;
      FFamilyid: Integer;
      FGenusId: Integer;
      FName: String;
      FOrderId: Integer;
      FPhylumId: Integer;
      FSpeciesId: Integer;

    public
      constructor Create;
      destructor Destroy; override;
      function GetCsvString: String; overload;
      function GetCustomCsvString(numSpeciesStr: String): String; overload;
      function GetCsvHeaderString: String;
      function GetCustomCsvHeaderString(toAddStr: String): String;
      property Name: String read FName write FName;
      property SpeciesId: Integer read FSpeciesId write FSpeciesId;
      property GenusId: Integer read FGenusId write FGenusId;
      property FamilyId: Integer read FFamilyid write FFamilyId;
      property OrderId: Integer read FOrderId write FOrderId;
      property ClassId: Integer read FClassId write FClassId;
      property PhylumId: Integer read FPhylumId write FPhylumId;
  end;

  { TNamesToIdsMapper }

  TNamesToIdsMapper = class(TObject)
    private
      FRankCounts: TRanksCountArray;
      FMessageLog: TStringList;
      FNamesMapFiles: TStringList;
      FNamesToIdsMap: TList;
      FDataLoader: TStringList;
      FRanksLoader: TStringList;
      FTokenSplitter: TStringList;
      FCsvText: TStringList;
      procedure ReleaseGarbage;
      function processSpecies: Boolean;
      function processGenera: Boolean;
      function processFamilies: Boolean;
      function processOrders: Boolean;
      function processClasses: Boolean;
      function processPhyla: Boolean;
      function FindByName(aName: String): TNodeNameToIdsMap;
      function LoadNodeToIdsMap(const NodeToIdsMapFile: String): Boolean;
      function LoadRanksFile(const RanksFile: String): Boolean;
      function SplitNamesToIdsString(aStr: String): Boolean;
      procedure Finalize;
    public
      constructor Create;
      destructor Destroy; override;

      function GenerateFullNamesToIdsMap(var Destination: TStringList): Boolean;
      function GenerateLeafNodesCount(var Destination: TStringList; const NodeToIdsMapFile: String; const RanksFile: String): Boolean;
      function GetNumFileNames: Integer;

      property NamesMapFiles: TStringList read FNamesMapFiles write FNamesMapFiles;
      property MessageLog: TStringList read FMessageLog;

  end;

  function CompareMapEntries(Item1: Pointer; Item2: Pointer): Integer;

implementation

uses
  gutils, gsvgstrings;

function CompareMapEntries(Item1: Pointer; Item2: Pointer): Integer;
begin
  Result := CompareStr(TNodeNameToIdsMap(Item1).Name, TNodeNameToIdsMap(Item2).Name);
end;

{ TNodeNameToIdsMap }

constructor TNodeNameToIdsMap.Create;
begin
  FClassId := -1;
  FFamilyid := -1;
  FGenusId := -1;
  FName := EmptyStr;
  FOrderId := -1;
  FPhylumId := -1;
  FSpeciesId := -1;
end;

destructor TNodeNameToIdsMap.Destroy;
begin
  inherited Destroy;
end;

function TNodeNameToIdsMap.GetCsvString: String;
begin
  Result := #39 + FName + #39 + Format(#9'%d'#9'%d'#9'%d'#9'%d'#9'%d'#9'%d', [FSpeciesId, FGenusId, FFamilyId, FOrderId, FClassId, FPhylumId]);
end;

function TNodeNameToIdsMap.GetCustomCsvString(numSpeciesStr: String): String;
begin
  Result := GetCsvString + #9 + numSpeciesStr;
end;

function TNodeNameToIdsMap.GetCsvHeaderString: String;
begin
  Result := 'name' + #9 + 'species' + #9 + 'genus' + #9 + 'family' + #9 + 'order' + #9 + 'class' + #9 + 'phylum';
end;

function TNodeNameToIdsMap.GetCustomCsvHeaderString(toAddStr: String): String;
begin
  Result := GetCsvHeaderString + #9 + ToAddStr;
end;

{ TNamesToIdsMapper }

procedure TNamesToIdsMapper.ReleaseGarbage;
var
  i: Integer;
begin
  if FNamesToIdsMap.Count > 0 then
    for i := 0 to FNamesToIdsMap.Count - 1 do
      TNodeNameToIdsMap(FNamesToIdsMap[i]).Free;
  FNamesToIdsMap.Clear;
  FNamesToIdsMap.Free;
  if Assigned(FMessageLog) then
    FMessageLog.Free;
  if Assigned(FNamesMapFiles) then
    FNamesMapFiles.Free;
  if Assigned(FDataLoader) then
    FDataLoader.Free;
  if Assigned(FTokenSplitter) then
    FTokenSplitter.Free;
  if Assigned(FCsvText) then
    FCsvText.Free;
  if Assigned(FRanksLoader) then
    FRanksLoader.Free;
end;

function TNamesToIdsMapper.processSpecies: Boolean;
var
  i: Integer;
  aMap: TNodeNameToIdsMap;
  debug: String;
  debug2: Integer;
begin
  Result := False;
  if (pos('species', LowerCase(FNamesMapFiles[0])) < 1) or (not FileExists(FNamesMapFiles[0])) then
  begin
    FMessageLog.Add('missing species map file');
    Exit;
  end;
  FDataLoader.LoadFromFile(FNamesMapFiles[0]);
  if FDataLoader.Count > 0 then
  begin
    for i := 0 to FDataLoader.Count - 1 do
    begin
      FTokenSplitter.Clear;
      SplitOnSingleChar(FDataLoader[i], '=', FTokenSplitter, False);
      if not (FTokenSplitter.Count = 2) then
      begin
        FMessageLog.Add(Format('malformed entry at line %d : %s', [i + 1, FDataLoader[i]]));
        Exit;
      end;
      aMap := TNodeNameToIdsMap.Create;
      aMap.Name := FTokenSplitter[1];
      aMap.SpeciesId := StrToInt(FTokenSplitter[0]);
      FNamesToIdsMap.Add(aMap);
    end;
    //FNamesToIdsMap.Sort(@CompareMapEntries);
    Result := True;
  end;
end;

function TNamesToIdsMapper.processGenera: Boolean;
var
  i: Integer;
  aMap: TNodeNameToIdsMap;
begin
  Result := False;
  if (pos('genus', LowerCase(FNamesMapFiles[1])) < 1) or (not FileExists(FNamesMapFiles[1])) then
  begin
    FMessageLog.Add('missing genus map file');
    Exit;
  end;
  FDataLoader.LoadFromFile(FNamesMapFiles[1]);
  if FDataLoader.Count > 0 then
  begin
    for i := 0 to FDataLoader.Count - 1 do
    begin
      FTokenSplitter.Clear;
      SplitOnSingleChar(FDataLoader[i], '=', FTokenSplitter, False);
      if not FTokenSplitter.Count = 2 then
      begin
        FMessageLog.Add(Format('malformed entry at line %d : %s', [i + 1, FDataLoader[i]]));
        Exit;
      end;
      aMap := FindByName(FTokenSplitter[1]);
      if Assigned(aMap) then
        aMap.GenusId := StrToInt(FTokenSplitter[0])
      else
      begin
        aMap := TNodeNameToIdsMap.Create;
        aMap.Name := FTokenSplitter[1];
        aMap.GenusId := StrToInt(FTokenSplitter[0]);
        FNamesToIdsMap.Add(aMap);
      end;
    end;
    Result := True;
  end;
end;

function TNamesToIdsMapper.processFamilies: Boolean;
var
  i: Integer;
  aMap: TNodeNameToIdsMap;
begin
  Result := False;
  if (pos('family', LowerCase(FNamesMapFiles[2])) < 1) or (not FileExists(FNamesMapFiles[2])) then
  begin
    FMessageLog.Add('missing family map file');
    Exit;
  end;
  FDataLoader.LoadFromFile(FNamesMapFiles[2]);
  if FDataLoader.Count > 0 then
  begin
    for i := 0 to FDataLoader.Count - 1 do
    begin
      FTokenSplitter.Clear;
      SplitOnSingleChar(FDataLoader[i], '=', FTokenSplitter, False);
      if not FTokenSplitter.Count = 2 then
      begin
        FMessageLog.Add(Format('malformed entry at line %d : %s', [i + 1, FDataLoader[i]]));
        Exit;
      end;
      aMap := FindByName(FTokenSplitter[1]);
      if Assigned(aMap) then
        aMap.FamilyId := StrToInt(FTokenSplitter[0])
      else
      begin
        aMap := TNodeNameToIdsMap.Create;
        aMap.Name := FTokenSplitter[1];
        aMap.FamilyId := StrToInt(FTokenSplitter[0]);
        FNamesToIdsMap.Add(aMap);
      end;
    end;
    Result := True;
  end;
end;

function TNamesToIdsMapper.processOrders: Boolean;
var
  i: Integer;
  aMap: TNodeNameToIdsMap;
begin
  Result := False;
  if (pos('order', LowerCase(FNamesMapFiles[3])) < 1) or (not FileExists(FNamesMapFiles[3])) then
  begin
    FMessageLog.Add('missing order map file');
    Exit;
  end;
  FDataLoader.LoadFromFile(FNamesMapFiles[3]);
  if FDataLoader.Count > 0 then
  begin
    for i := 0 to FDataLoader.Count - 1 do
    begin
      FTokenSplitter.Clear;
      SplitOnSingleChar(FDataLoader[i], '=', FTokenSplitter, False);
      if not FTokenSplitter.Count = 2 then
      begin
        FMessageLog.Add(Format('malformed entry at line %d : %s', [i + 1, FDataLoader[i]]));
        Exit;
      end;
      aMap := FindByName(FTokenSplitter[1]);
      if Assigned(aMap) then
        aMap.OrderId := StrToInt(FTokenSplitter[0])
      else
      begin
        aMap := TNodeNameToIdsMap.Create;
        aMap.Name := FTokenSplitter[1];
        aMap.OrderId := StrToInt(FTokenSplitter[0]);
        FNamesToIdsMap.Add(aMap);
      end;
    end;
    Result := True;
  end;
end;

function TNamesToIdsMapper.processClasses: Boolean;
var
  i: Integer;
  aMap: TNodeNameToIdsMap;
begin
  Result := False;
  if (pos('class', LowerCase(FNamesMapFiles[4])) < 1) or (not FileExists(FNamesMapFiles[4])) then
  begin
    FMessageLog.Add('missing class map file');
    Exit;
  end;
  FDataLoader.LoadFromFile(FNamesMapFiles[4]);
  if FDataLoader.Count > 0 then
  begin
    for i := 0 to FDataLoader.Count - 1 do
    begin
      FTokenSplitter.Clear;
      SplitOnSingleChar(FDataLoader[i], '=', FTokenSplitter, False);
      if not FTokenSplitter.Count = 2 then
      begin
        FMessageLog.Add(Format('malformed entry at line %d : %s', [i + 1, FDataLoader[i]]));
        Exit;
      end;
      aMap := FindByName(FTokenSplitter[1]);
      if Assigned(aMap) then
        aMap.ClassId := StrToInt(FTokenSplitter[0])
      else
      begin
        aMap := TNodeNameToIdsMap.Create;
        aMap.Name := FTokenSplitter[1];
        aMap.ClassId := StrToInt(FTokenSplitter[0]);
        FNamesToIdsMap.Add(aMap);
      end;
    end;
    Result := True;
  end;
end;

function TNamesToIdsMapper.processPhyla: Boolean;
var
  i: Integer;
  aMap: TNodeNameToIdsMap;
begin
  Result := False;
  if (pos('phylum', LowerCase(FNamesMapFiles[5])) < 1) or (not FileExists(FNamesMapFiles[5])) then
  begin
    FMessageLog.Add('missing phylum map file');
    Exit;
  end;
  FDataLoader.LoadFromFile(FNamesMapFiles[5]);
  if FDataLoader.Count > 0 then
  begin
    for i := 0 to FDataLoader.Count - 1 do
    begin
      FTokenSplitter.Clear;
      SplitOnSingleChar(FDataLoader[i], '=', FTokenSplitter, False);
      if not FTokenSplitter.Count = 2 then
      begin
        FMessageLog.Add(Format('malformed entry at line %d : %s', [i + 1, FDataLoader[i]]));
        Exit;
      end;
      aMap := FindByName(FTokenSplitter[1]);
      if Assigned(aMap) then
        aMap.PhylumId := StrToInt(FTokenSplitter[0])
      else
      begin
        aMap := TNodeNameToIdsMap.Create;
        aMap.Name := FTokenSplitter[1];
        aMap.PhylumId := StrToInt(FTokenSplitter[0]);
        FNamesToIdsMap.Add(aMap);
      end;
    end;
    Result := True;
  end;
end;

function TNamesToIdsMapper.FindByName(aName: String): TNodeNameToIdsMap;
var
  i: Integer;
begin
  Result := nil;
  if FNamesToIdsMap.Count > 0 then
    for i := 0 to FNamesToIdsMap.Count - 1 do
    begin
      if TNodeNameToIdsMap(FNamesToIdsMap[i]).Name = aName then
      begin
        Result := TNodeNameToIdsMap(FNamesToIdsMap[i]);
        Exit;
      end;
    end;
end;

function TNamesToIdsMapper.LoadNodeToIdsMap(const NodeToIdsMapFile: String): Boolean;
var
  i: Integer;
  aMap: TNodeNameToIdsMap;
begin
  Result := False;
  FDataLoader.LoadFromFile(NodeToIdsMapFile);
  if not (FDataLoader.Count > 0)  then
  begin
    FMessageLog.Add('Failed to load names map file');
    Exit;
  end;
  try
    for i := 1 to FDataLoader.Count - 1 do
    begin
      if not SplitNamesToIdsString(FDataLoader[i]) then
      begin
        FMessageLog.Add('failed to load names to IDs string: ' + FDataLoader[i]);
        Exit;
      end;
      aMap := TNodeNameToIdsMap.Create;
      aMap.Name := Copy(FTokenSplitter[0], 2, Length(FTokenSplitter[0]) - 2);
      aMap.SpeciesId := StrToInt(FTokenSplitter[1]);
      aMap.GenusId := StrToInt(FTokenSplitter[2]);
      aMap.FamilyId := StrToInt(FTokenSplitter[3]);
      aMap.OrderId := StrToInt(FTokenSplitter[4]);
      aMap.ClassId := StrToInt(FTokenSplitter[5]);
      aMap.PhylumId := StrToInt(FTokenSplitter[6]);
      FNamesToIdsMap.Add(aMap);
    end;
    Result := True;
  except
    on E:Exception do
      FMessageLog.Add('Exception encountered in LoadNodeToIdsMap: ' + E.Message);
  end;
end;

function TNamesToIdsMapper.LoadRanksFile(const RanksFile: String): Boolean;
var
  i: Integer;
  RanksStringSplitter: TStringList = nil;
  id, aCount: Integer;
  debug: String;
begin
  Result := False;
  if not FileExists(RanksFile) then
  begin
    FMessageLog.Add('Failed to load ranks file');
    Exit;
  end;

  FRanksLoader.LoadFromFile(Ranksfile);
  if not (FRanksLoader.Count > 0) then
  begin
    FMessageLog.Add('Failed to load ranks file');
    Exit;
  end;

  try
    try
      SetLength(FRankCounts, 5561270 + 10000);
      RanksStringSplitter := TStringList.Create;
      for i := 1 to FRanksLoader.Count - 1 do
      begin
        RanksStringSplitter.Clear;
        SplitOnSingleChar(FRanksLoader[i], ',', RanksStringSplitter, False);
        if not (RanksStringSplitter.Count = 34) then
          raise Exception.Create(Format('Expected 34 tokens but got %d at line %d', [RanksStringSplitter.Count, i + 1]));

        debug := RanksStringSplitter[0];
        id := StrToInt(RanksStringSplitter[0]);
        aCount := StrToInt(RanksStringSplitter[30]);
        FRankCounts[id].species := aCount;
      end;
      Result := True;
    except
      on E:Exception do
      begin
        FMessageLog.Add('Exception encountered in LoadRanksFile: ' + E.Message);
        WriteLn(FMessageLog.Text);
      end;
    end;
  finally
    if Assigned(RanksStringSplitter) then
      RanksStringSplitter.Free;
  end;
end;

function TNamesToIdsMapper.SplitNamesToIdsString(aStr: String): Boolean;
begin
  Result := False;
  FTokenSplitter.Clear;
  SplitOnSingleChar(aStr, #9, FTokenSplitter, False);
  if not (FTokenSplitter.Count = 7) then
  begin
    FMessageLog.Add(Format('malformed entry : %s', [aStr]));
    Exit;
  end;
  Result := True;
end;

procedure TNamesToIdsMapper.Finalize;
var
  i: Integer;
  aText: String;
begin
  if FNamesToIdsMap.Count > 0 then
  begin
    aText := TNodeNameToIdsMap(FNamesToIdsMap[0]).GetCsvHeaderString;
    FCsvText.Add(aText);
    for i := 0 to FNamesToIdsMap.Count - 1 do
    begin
      aText := TNodeNameToIdsMap(FNamesToIdsMap[i]).GetCsvString;
      FCsvText.Add(aText);
    end;
  end;
end;

constructor TNamesToIdsMapper.Create;
begin
  FMessageLog := TStringList.Create;
  FDataLoader := TStringList.Create;
  FTokenSplitter := TStringList.Create;
  FCsvText := TStringList.Create;
  FTokenSplitter.Delimiter := '=';
  FNamesMapFiles := nil; { given to us later}
  FNamesToIdsMap := TList.Create;
  FRanksLoader := TStringList.Create;
end;

destructor TNamesToIdsMapper.Destroy;
begin
  ReleaseGarbage;
  inherited Destroy;
end;

function TNamesToIdsMapper.GenerateFullNamesToIdsMap(var Destination: TStringList): Boolean;
begin
  Result := ProcessSpecies;
  if not Result then
    Exit;

  Result := ProcessGenera;
  if not Result then
    Exit;

  Result := ProcessFamilies;
  if not Result then
    Exit;

  Result := ProcessOrders;
  if not Result then
    Exit;

  Result := ProcessClasses;
  if not Result then
    Exit;

  Result := ProcessPhyla;
  if not Result then
    Exit;
  Finalize;
  Destination.Clear;
  Destination.AddStrings(FCsvText);
  Result := Destination.Count > 1;
end;

function TNamesToIdsMapper.GenerateLeafNodesCount(var Destination: TStringList; const NodeToIdsMapFile: String; const RanksFile: String): Boolean;
var
  i: Integer;
  aMap: TNodeNameToIdsMap;
  aStr: String;
  numLeafNodes: Integer;
  debug: Integer;
begin
  Destination.Clear;
  Result := False;
  if not LoadNodeToIdsMap(NodeToIdsMapFile) then
  begin
    FMessageLog.Add('Failed to generate leaf nodes count');
    Exit;
  end;

  if not LoadRanksFile(RanksFile) then
  begin
    FMessageLog.Add('Failed to generate leaf nodes count');
    Exit;
  end;
  aMap := TNodeNameToIdsMap(FNamesToIdsMap[0]);
  Destination.Add(aMap.GetCustomCsvHeaderString('leaf_node_count'));
  for i := 0 to FNamesToIdsMap.Count - 1 do
  begin
    aMap := TNodeNameToIdsMap(FNamesToIdsMap[i]);
    debug := aMap.SpeciesId;
    if aMap.SpeciesId > 0 then
    begin
      numLeafNodes := FRankCounts[aMap.SpeciesId].species;
      aStr := aMap.GetCustomCsvString(IntToStr(numLeafNodes));
      Destination.Add(aStr);
    end;
  end;
  Result := True;
end;

function TNamesToIdsMapper.GetNumFileNames: Integer;
begin
  Result := NamesMapFiles.Count;
end;

end.

