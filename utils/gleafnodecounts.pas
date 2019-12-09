unit gleafnodecounts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gtaxonomicrank;

type
  TTimetreeLeafNodeCount = record
    Name: String;
    SpeciesId: Integer;
    GenusId: Integer;
    Familyid: Integer;
    ClassId: Integer;
    OrderId: Integer;
    PhylumId: Integer;
    NumLeafNodes: Integer;
  end;

  TTimetreeLeafNodeCountArray = array of TTimetreeLeafNodeCount;

  { TLeafNodeCountsLoader }

  TLeafNodeCountsLoader = class(TObject)
    private
      FDataLoader: TStringList;
      FMessagesLog: TStringList;
      FTargetRank: TTaxonomicRank;
      FTokens: TStringList;
      function ProcessLine(aLineStr: String): Boolean;
    public
      constructor Create;
      destructor Destroy; override;

      function LoadLeafNodeCounts(const lncFile: String): Boolean;
      property MessagesLog: TStringList read FMessagesLog;
      property TargetRank: TTaxonomicRank read FTargetRank write FTargetRank;
  end;

  { TLeafNodeCountsLoaderThread }

  TLeafNodeCountsLoaderThread = class(TThread)
    private
      FIsSuccess: Boolean;
      FLeafNodeCountsFile: String;
      FLeafNodeCountsLoader: TLeafNodeCountsLoader;
      FTargetRank: TTaxonomicRank;
      function GetMessagesLog: TStringList;
      procedure LoadLeafNodeCounts;
    public
      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;
      procedure Execute; override;

      property LeafNodeCountsFile: String read FLeafNodeCountsFile write FLeafNodeCountsFile;
      property MessagesLog: TStringList read GetMessagesLog;
      property IsSuccess: Boolean read FIsSuccess;
      property TargetRank: TTaxonomicRank read FTargetRank write FTargetRank;
  end;

  function GetLeafNodeCount(timetreeId: Integer): Integer;

var
  LeafNodeCounts: TTimetreeLeafNodeCountArray;

implementation

uses
  ttconst, gutils;

function GetLeafNodeCount(timetreeId: Integer): Integer;
begin
  Result := LeafNodeCounts[timetreeId].NumLeafNodes;
end;

{ TLeafNodeCountsLoaderThread }

function TLeafNodeCountsLoaderThread.GetMessagesLog: TStringList;
begin

end;

procedure TLeafNodeCountsLoaderThread.LoadLeafNodeCounts;
begin
  FLeafNodeCountsLoader.TargetRank := FTargetRank;
  FIsSuccess := FLeafNodeCountsLoader.LoadLeafNodeCounts(FLeafNodeCountsFile);
end;

constructor TLeafNodeCountsLoaderThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FLeafNodeCountsLoader := TLeafNodeCountsLoader.Create;
  FIsSuccess := False;
end;

destructor TLeafNodeCountsLoaderThread.Destroy;
begin
  if Assigned(FLeafNodeCountsLoader) then
    FLeafNodeCountsLoader.Free;
  inherited Destroy;
end;

procedure TLeafNodeCountsLoaderThread.Execute;
begin
  LoadLeafNodeCounts;
end;

{ TLeafNodeCountsLoader }

function TLeafNodeCountsLoader.ProcessLine(aLineStr: String): Boolean;
var
  id: Integer;
begin
  Result := False;
  FTokens.Clear;
  SplitOnSingleChar(aLineStr, #9, FTokens, False);
  if not (FTokens.Count = 8) then
    Exit;
  try
    case TargetRank of
      trPhylum: id := StrToInt(FTokens[6]);
      trClass: id := StrToInt(FTokens[5]);
      trOrder: id := StrToInt(FTokens[4]);
      trFamily: id := StrToInt(FTokens[3]);
      trGenus: id := StrToInt(FTokens[2]);
      trSpecies: id := StrToInt(FTokens[1]);
      else
        raise Exception.Create('unsupported taxonomic rank given');
    end;
    if id >= 0 then
    begin
      LeafNodeCounts[id].SpeciesId := StrToInt(FTokens[1]);
      LeafNodeCounts[id].GenusId := StrToInt(FTokens[2]);
      LeafNodeCounts[id].FamilyId := StrToInt(FTokens[3]);
      LeafNodeCounts[id].OrderId := StrToInt(FTokens[4]);
      LeafNodeCounts[id].ClassId := StrToInt(FTokens[5]);
      LeafNodeCounts[id].PhylumId := StrToInt(FTokens[6]);
      LeafNodeCounts[id].NumLeafNodes := StrToInt(FTokens[7]);
    end;

    Result := True;
  except
    Result := False;
  end;
end;

constructor TLeafNodeCountsLoader.Create;
begin
  FDataLoader := TStringList.Create;
  FMessagesLog := TStringList.Create;
  FTokens := TStringList.Create;
end;

destructor TLeafNodeCountsLoader.Destroy;
begin
  if Assigned(FDataLoader) then
    FDataLoader.Free;
  if Assigned(FMessagesLog) then
    FMessagesLog.Free;
  if Assigned(FTokens) then
    FTokens.Free;
  inherited Destroy;
end;

function TLeafNodeCountsLoader.LoadLeafNodeCounts(const lncFile: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not FileExists(lncFile) then
  begin
    FMessagesLog.Add('leaf node counts file not found: ' + lncFile);
    Exit;
  end;
  FDataLoader.LoadFromFile(lncFile);
  SetLength(LeafNodeCounts, MAX_TIMETREE_NODE_ID);
  if FDataLoader.Count > 1 then
  begin
    for i := 1 to FDataLoader.Count - 1 do
    begin
      if not ProcessLine(FDataLoader[i]) then
      begin
        FMessagesLog.Add('failed to process string from leaf node counts file: ' + FDataLoader[i]);
        Exit;
      end;
    end;
  end
  else
  begin
    FMessagesLog.Add('failed to load data from leaf node counts file');
    Exit;
  end;
  Result := True;
end;

end.

