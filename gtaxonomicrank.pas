unit gtaxonomicrank;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TLoadRanksThread }

  TLoadRanksThread = class(TThread)
    private
      FEndTime: TDateTime;
      FIsSuccess: Boolean;
      FLogStrings: TStringList;
      FRanksFile: String;
      FStartTime: TDateTime;

      procedure LoadRanks;
      procedure SetLogStrings(AValue: TStringList);
    public
      constructor Create;
      destructor Destroy; override;

      procedure Execute; override;

      property RanksFile: String read FRanksFile write FRanksFile;
      property IsSuccess: Boolean read FIsSuccess;
      property StartTime: TDateTime read FStartTime;
      property EndTime: TDateTime read FEndTime;
      property LogStrings: TStringList read FLogStrings write SetLogStrings;
  end;

  TTaxonomicRank = (trNoRank, trUnknown, trDomain, trSuperKingdom, trKingdom, trSubKingdom,
                    trSuperPhylum, trPhylum, trSubPhylum,
                    trSuperClass, trClass, trSubClass, trInfraClass, trCohort, trSubCohort, trSuperOrder, trOrder,
                    trSubOrder, trInfraOrder, trParvOrder, trSuperFamily, trFamily, trSubFamily,
                    trTribe, trSubTribe, trGenus, trSubGenus, trSpeciesGroup,
                    trSpeciesSubGroup, trSpecies, trSubSpecies, trVarietas, trClade);
  TTaxonomicRankArray = array of TTaxonomicRank;

  TRanksCount = record
    numChildren: Integer;
    noRank: Integer;
    unknown: Integer;
    domain: Integer;
    superKingdom: Integer;
    kingdom: Integer;
    subKingdom: Integer;
    superPhylum: Integer;
    phylum: Integer;
    subPhylum: Integer;
    superClass: Integer;
    fClass: Integer;
    subClass: Integer;
    infraClass: Integer;
    superOrder: Integer;
    order: Integer;
    parvOrder: Integer;
    subOrder: Integer;
    infraOrder: Integer;
    superFamily: Integer;
    family: Integer;
    subFamily: Integer;
    tribe: Integer;
    subTribe: Integer;
    genus: Integer;
    subGenus: Integer;
    speciesGroup: Integer;
    species: Integer;
    speciesSubGroup: Integer;
    subSpecies: Integer;
    varietas: Integer;
    clade: Integer;
  end;

  TRanksCountArray = array of TRanksCount;
  PRanksCount = ^TRanksCount;

  function ImportTaxonomicRanks(const aFile: String; const NumNodes: Integer; var IsSuccess: Boolean): TTaxonomicRankArray;
  function StringToTaxonomicRank(const aStr: String): TTaxonomicRank;
  function TaxonomicRankToString(const aRank: TTaxonomicRank): String;
  function IsValidRankForSearch(const aRank: TTaxonomicRank): Boolean;



var
  TaxonomicRanks: TTaxonomicRankArray;

implementation

uses
  ttconst, dateutils;

function ImportTaxonomicRanks(const aFile: String; const NumNodes: Integer; var IsSuccess: Boolean): TTaxonomicRankArray;
var
  sList: TStringList;
  i: Integer;
  Index: Integer;
  Rank: TTaxonomicRank;
  //debug: Integer;
begin
  IsSuccess := False;
  sList := nil;
  if not FileExists(aFile) then
    Exit;

  try
    try
      sList := TStringList.Create;
      sList.LoadFromFile(aFile);
      if sList.Count > 0 then
      begin
        //debug := sList.Count;
        SetLength(Result, 5561270 + 10000); { there are IDs in the timetree DB that skip a number so just add extra, we don't want to search this list, just access items directly by their timetree ID}
        for i := 0 to Length(Result) - 1 do
          Result[i] := trUnknown;
        for i := 0 to sList.Count - 1 do
        begin
          if sList.ValueFromIndex[i] <> EmptyStr then
          begin
            Index := StrToInt(sList.Names[i]);
            Rank := StringToTaxonomicRank(sList.ValueFromIndex[i]);
            Result[Index] := Rank;
          end;
        end;
        IsSuccess := True;
      end;
    except
      SetLength(Result, 0);
    end;
  finally
    if Assigned(sList) then
      sList.Free;
  end;
end;

function StringToTaxonomicRank(const aStr: String): TTaxonomicRank;
var
  Temp: String;
begin
  //Temp := LowerCase(aStr);
  Temp := aStr;
  Temp := StringReplace(Temp, ' ', '_', [rfReplaceAll]);

  if Temp = 'species' then
    Result := trSpecies
  else if Temp = 'genus' then
    Result := trGenus
  else if Temp = 'subgenus' then
    Result := trSubGenus
  else  if Temp = 'subspecies' then
    Result := trSubSpecies
  else if Temp = 'species_subgroup' then
    Result := trSpeciesSubGroup
  else if Temp = 'subclass' then
    Result := trSubClass
  else if Temp = 'order' then
    Result := trOrder
  else if Temp = 'suborder' then
    Result := trSubOrder
  else if Temp = 'phylum' then
    Result := trPhylum
  else if Temp = 'class' then
    Result := trClass
  else if Temp = 'family' then
    Result := trFamily
  else if Temp = 'species_group' then
    Result := trSpeciesGroup
  else if Temp = 'no_rank' then
    Result := trNoRank
  else if Temp = 'superphylum' then
    Result := trSuperPhylum
  else if Temp = 'subphylum' then
    Result := trSubPhylum
  else if Temp = 'subfamily' then
    Result := trSubFamily
  else if Temp = 'kingdom' then
    Result := trKingdom
  else if Temp = 'superorder' then
    Result := trSuperOrder
  else if Temp = 'tribe' then
    Result := trTribe
  else if Temp = 'subtribe' then
    Result := trSubTribe
  else if Temp = 'varietas' then
    Result := trVarietas
  else if Temp = 'subkingdom' then
    Result := trSubKingdom
  else if Temp = 'infraorder' then
    Result := trInfraOrder
  else if Temp = 'parvorder' then
    Result := trParvOrder
  else if Temp = 'superfamily' then
    Result := trSuperFamily
  else if Temp = 'superclass' then
    Result := trSuperClass
  else if Temp = 'infraclass' then
    Result := trInfraClass
  else if Temp = 'superkingdom' then
    Result := trSuperKingdom
  else if Temp = 'domain' then
    Result := trDomain
  else if Temp = 'unknown' then
    Result := trUnknown
  else if Temp = 'cohort' then
    Result := trCohort
  else if Temp = 'subcohort' then
    Result := trSubCohort
  else if Temp = 'clade' then
    Result := trClade
  else
    raise Exception.Create('invalid taxonomic rank string: ' + Temp);
end;

function TaxonomicRankToString(const aRank: TTaxonomicRank): String;
begin
  case aRank of
  trDomain: Result := 'domain';
  trSuperKingdom: Result := 'superkingdom';
  trKingdom: Result := 'kingdom';
  trSubKingdom: Result := 'subkingdom';
  trSuperPhylum: Result := 'superphylum';
  trPhylum: Result := 'phylum';
  trSubPhylum: Result := 'subphylum';
  trSuperClass: Result := 'superclass';
  trClass: Result := 'class';
  trSubClass: Result := 'subclass';
  trInfraClass: Result := 'infraclass';
  trSuperOrder: Result := 'superorder';
  trOrder: Result := 'order';
  trParvOrder: Result := 'parvorder';
  trSubOrder: Result := 'suborder';
  trInfraOrder: Result := 'infraorder';
  trSuperFamily: Result := 'superfamily';
  trFamily: Result := 'family';
  trSubFamily: Result := 'subfamily';
  trTribe: Result := 'tribe';
  trSubTribe: Result := 'subtribe';
  trGenus: Result := 'genus';
  trSubGenus: Result := 'subgenus';
  trSpeciesGroup: Result := 'species_group';
  trSpecies: Result := 'species';
  trSpeciesSubGroup: Result := 'species_subgroup';
  trSubSpecies: Result := 'subspecies';
  trVarietas: Result := 'varietas';
  trNoRank: Result := 'no rank';
  trCohort: Result := 'cohort';
  trSubCohort: Result := 'subcohort';
  trClade: Result := 'clade';
  trUnknown: Result := 'unknown';
  end;
end;

function IsValidRankForSearch(const aRank: TTaxonomicRank): Boolean;
begin
  case aRank of
    trVarietas, trNoRank, trUnknown, trSuperKingdom, trClade: Result := False;
    trKingdom: Result := True;
    trSubKingdom: Result := True;
    trSuperPhylum: Result := True;
    trPhylum: Result := True;
    trSubPhylum: Result := True;
    trSuperClass: Result := True;
    trClass: Result := True;
    trSubClass: Result := True;
    trInfraClass: Result := True;
    trSuperOrder: Result := True;
    trOrder: Result := True;
    trParvOrder: Result := True;
    trSubOrder: Result := True;
    trInfraOrder: Result := True;
    trSuperFamily: Result := True;
    trFamily: Result := True;
    trSubFamily: Result := True;
    trTribe: Result := True;
    trSubTribe: Result := True;
    trGenus: Result := True;
    trSubGenus: Result := True;
    trSpeciesGroup: Result := True;
    trSpecies: Result := True;
    trSpeciesSubGroup: Result := True;
    trSubSpecies: Result := True;
    trCohort: Result := True;
    trSubCohort: Result := True;
    else
      raise Exception.Create('Unhandled taxonomic rank. This is a bug!');
  end;
end;

{ TLoadRanksThread }

procedure TLoadRanksThread.LoadRanks;
var
  sList: TStringList;
  i: Integer;
  Index: Integer;
  Rank: TTaxonomicRank;
  temp: String;
begin
  FStartTime := Now;
  FIsSuccess := False;
  sList := nil;
  if not FileExists(RanksFile) then
    Exit;

  try
    try
      sList := TStringList.Create;
      sList.LoadFromFile(RanksFile);
      if sList.Count > 0 then
      begin
        SetLength(TaxonomicRanks, 5561270 + 10000); { there are IDs in the timetree DB that skip a number so just add extra, we don't want to search this list, just access items directly by their timetree ID}
        for i := 0 to Length(TaxonomicRanks) - 1 do
          TaxonomicRanks[i] := trUnknown;
        for i := 0 to sList.Count - 1 do
        begin
          if sList.ValueFromIndex[i] <> EmptyStr then
          begin
            Index := StrToInt(sList.Names[i]);
            Rank := StringToTaxonomicRank(sList.ValueFromIndex[i]);
            TaxonomicRanks[Index] := Rank;
          end;
        end;
        FIsSuccess := True;
      end;
      FEndTime := Now;
      if Assigned(FLogStrings) then
      begin
        try
          LogfileCriticalSect.Acquire;
          temp := 'Load ranks: execution time = ' + IntToStr(MilliSecondsBetween(FEndTime, FStartTime)) + ' ms';
          FLogStrings.Add(temp);
        finally
          LogfileCriticalSect.Release;
        end;
      end;
    except
      SetLength(TaxonomicRanks, 0);
    end;
  finally
    if Assigned(sList) then
      sList.Free;
  end;
end;

procedure TLoadRanksThread.SetLogStrings(AValue: TStringList);
begin
  if FLogStrings=AValue then Exit;
  FLogStrings:=AValue;
end;

constructor TLoadRanksThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FLogStrings := nil;
end;

destructor TLoadRanksThread.Destroy;
begin
  inherited Destroy;
end;

procedure TLoadRanksThread.Execute;
begin
  LoadRanks;
end;


end.

