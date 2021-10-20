unit gtreedata;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, ttconst;
{$M+}

type

  TArrayOfBoolean = array of Boolean;

  { TTimeTreeData }

  TTimeTreeData = class(TObject)
  private
    FTimetreeIDs: TArrayOfInteger;
    FFreq : double;
    FValue: double;
    FValue2: double;
    FNoOfOTUs : integer;
    FNode : PArrayOfNodeData;
    FBLen : PArrayOfDouble;
    FSE   : PArrayOfDouble;
    FStats: PArrayOfDouble;
    FIsGeneDupEvent: TArrayOfBoolean;
    FIsSpeciationEvent: TArrayOfBoolean;
    FIsOutgroupMember: TArrayOfBoolean;
    FDataCoverage: ArrayOfDouble; { the proportion of sites for which at least on taxa in each descendent lineage have usable data}
    FChildNodeCounts: ArrayOfInteger;
    FIsStats: boolean;

    function GetDataCoverage(Index: Integer): Double;
    procedure SetDataCoverage(Index: Integer; AValue: Double);
    procedure SetNoOfOTUs(value:integer);
    procedure SetNode(index: integer; nodedata: TNodeData);
    procedure SetBLen(index: integer; value: double);
    procedure SetSE(index: integer; value: double);
    procedure SetStats(index: integer; value: double);
    function  GetNode(index: integer):TNodeData;
    function  GetBLen(index: integer):double;
    function  GetSE(index: integer):double;
    function  GetStats(index: integer):double;
    function  GetIsBLen:boolean;
    function  GetIsSE:boolean;
    function  GetIsStats:boolean;
    procedure SetIsBLen(value: boolean);
    procedure SetIsSE(value: boolean);
    procedure SetIsStats(value: boolean);
    function  GetSBL:double;
    function  GetIsGeneDupEvent(Index: Integer): Boolean;
    procedure SetIsGeneDupEvent(Index: Integer; const Value: Boolean);
    function GetIsSpeciationEvent(Index: Integer): Boolean;
    procedure SetIsSpeciationEvent(Index: Integer; const Value: Boolean);
    procedure WriteGeneDupInfoToSession(var SessionFile: File);
    procedure ReadGeneDupInfoFromSession(var SessionFile: File);
    function GetIsOutgroupMember(Index: Integer): Boolean;
    procedure SetIsOutgroupMember(Index: Integer; const Value: Boolean);
  published
    property NoOfOTUs: integer read FNoOfOTUs write SetNoOfOTUs;
    property Value: double read FValue write FValue;
    property Value2: double read FValue2 write FValue2;
    property Freq: double read FFreq write FFreq;
    property SBL: double read GetSBL;
    property isBLen: boolean read GetIsBLen write SetIsBLen;
    property isSE: boolean read GetIsSE write SetIsSE;
    property isStats: boolean read GetIsStats write SetIsStats;
  public

    procedure RemoveTimetreeID(Index: Integer);
    function GetChildNodeCount(Index: Integer): Integer;
    procedure SetChildNodeCount(Index: Integer; aValue: Integer);
    function GetTimetreeID(Index: Integer): Integer;
    procedure SetTimetreeID(Index: Integer; aValue: Integer);
    function HasTimetreeID(aID: Integer): Boolean; { for validating that all nodes that are supposed to be included are present }
    function UpdateTimeTreeIDs(const OtuNames: TStringList; const IntNodeLbls: TStringList): Boolean; { convert the otunames and node labels from strings to integers}

    function NumGeneDups: Integer;
    function NumSpeciations: Integer;
    property Node[Index:integer]: TNodeData read GetNode write SetNode; default;
    property BLen[Index:integer]: double read GetBLen write SetBLen;
    property SE[Index:integer]: double read GetSE write SetSE;
    property Stats[Index:integer]: double read GetStats write SetStats;
    property IsGeneDupEvent[Index: Integer]: Boolean read GetIsGeneDupEvent write SetIsGeneDupEvent;
    property IsSpeciationEvent[Index: Integer]: Boolean read GetIsSpeciationEvent write SetIsSpeciationEvent;
    property IsOutgroupMember[Index: Integer]: Boolean read GetIsOutgroupMember write SetIsOutgroupMember;
    property DataCoverage[Index: Integer]: Double read GetDataCoverage write SetDataCoverage;
    property NodeArray: PArrayOfNodeData read FNode write FNode;
    property BLenArray: PArrayOfDouble read FBLen write FBLen;
    property SEArray: PArrayOfDouble read FSE write FSE;
    property StatsArray: PArrayOfDouble read FStats write FStats;

    {property NodeLabelArray: TArrayOfString read FNodeL write FNodeL;}
    { These six properties break the protection of object data.
      Use very carefully only when the maximum efficiency is needed.
      Used in object TTrees.}

    function Clone: TTimeTreeData;
    function MRCA(taxaANodeID, taxaBNodeID: Integer): Integer; overload;
    function RootIndex: integer;
    function Assign(Source: TTimeTreeData):boolean;
    function AssignDataCoverage(Source: TTimeTreeData): Boolean;
    function Identical(Source: TTimeTreeData): boolean;
    procedure RemoveOTU(target: integer);
    procedure AddOTU(node: integer);
    procedure ReadFromFile(var SessionFile: File; SessionFileVersion: integer);
    procedure WriteToFile(var SessionFile: File);
    function StateToStringList(Comment: AnsiString): TStringList;
    function HasNegativeBranchLengths: Boolean;
    constructor Create(N:integer; BLen, SE, Stats:boolean; NodeLabels: Boolean = True);
    destructor Destroy; override;
  end;


implementation

uses
  gutils;

///////////////////////////
// TTimeTreeData
///////////////////////////

function TTimeTreeData.Clone: TTimeTreeData;
begin
  Result := TTimeTreeData.Create(FNoOfOtus, IsBlen, IsSE, IsStats);
  Result.Assign(Self);
end;

constructor TTimeTreeData.Create(N:integer; BLen, SE, Stats:boolean; NodeLabels: Boolean = True);
var
  i: Integer;
begin
  SetLength(FTimetreeIDs, 2 * N - 1);

  if N < 2 then
    FNoOfOTUs := 2
  else
    FNoOfOTUs := N;
  FFreq  := 0.0;
  FValue := 0.0;
  FValue2 := 0.0;
  if NoOfOTUs > 0 then
  begin
    // Alloc space for FDataCoverage (stores data only for internal nodes)
    SetLength(FDataCoverage, NoOfOtus - 1);
    // Alloc space for FNode (stores only internal nodes)
    GetMem(FNode, SizeOf(TNodeData)*(NoOfOTUs-1));
    FillChar(FNode^, SizeOf(TNodeData)*(NoOfOTUs-1), 0);

    // Alloc space for FBlen (stores branch lengths for ALL nodes)
    if BLen then begin
      GetMem(FBLen, SizeOf(Double)*2*(NoOfOTUs-1));
      FillChar(FBLen^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
    end
    else
      FBLen := nil;

    // Alloc space for FSE (stores standard error for ALL nodes)
    if SE then begin
      GetMem(FSE, SizeOf(Double)*2*(NoOfOTUs-1));
      FillChar(FSE^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
    end
    else
      FSE := nil;

    SetLength(FChildNodeCounts, 2 * NoOfOtus - 1);
    SetLength(FIsGeneDupEvent, 2 * NoOfOTUs - 1); { this is larger because the root node is included}
    SetLength(FIsSpeciationEvent, 2 * NoOfOtus - 1); { root node included}
    for i := Low(FIsGeneDupEvent) to High(FIsGeneDupEvent) do
    begin
      FIsGeneDupEvent[i] := False;
      FIsSpeciationEvent[i] := False;
      FChildNodeCounts[i] := 0;
    end;

    SetLength(FIsOutgroupMember, NoOfOtus);
    for i := Low(FIsOutgroupMember) to High(FIsOutgroupMember) do
      FIsOutgroupMember[i] := False;

    FIsStats := Stats;
    // Alloc space for FStats (Stores ???)
    GetMem(FStats, SizeOf(Double)*2*(NoOfOTUs-1));
    FillChar(FStats^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
  end;
end;

destructor TTimeTreeData.Destroy;
begin
  if NoOfOTUs > 0 then
  begin
    FreeMemAndNil(FNode);
    FreeMemAndNil(FBLen);
    FreeMemAndNil(FSE);
    FreeMemAndNil(FStats);
    SetLength(FIsGeneDupEvent, 0);
    SetLength(FIsSpeciationEvent, 0);
    SetLength(FIsOutgroupMember, 0);
    SetLength(FDataCoverage, 0);
  end;
  FNoOfOTUs := 0;
  inherited
end;

procedure TTimeTreeData.SetNoOfOTUs(value:integer);
var
  i: Integer;
begin
  if (value < 2) or (value = NoOfOTUs) then exit;
  FNoOfOTUs := value;
  ReAllocMem(FNode, SizeOf(TNodeData)*(NoOfOTUs-1));
  if FBLen <> nil then
    ReAllocMem(FBLen, SizeOf(Double)*2*(NoOfOTUs-1));
  if FSE <> nil then
    ReAllocMem(FSE, SizeOf(Double)*2*(NoOfOTUs-1));
  if FStats <> nil then
   ReAllocMem(FStats, SizeOf(Double)*2*(NoOfOTUs-1));
  SetLength(FIsGeneDupEvent, 2 * NoOfOtus - 1); // root node included
  SetLength(FIsSpeciationEvent, 2 * NoOfOtus - 1); // root node included
  SetLength(FIsOutgroupMember, NoOfOtus);
  SetLength(FTimetreeIDs, 2 * NoOfOtus - 1);
  //for i  := 0 to Length(FTimetreeIDs) - 1 do
  //  FTimetreeIDs[i] := -1;
end;

function TTimeTreeData.GetDataCoverage(Index: Integer): Double;
begin
  Assert((Index >= 0) and (Index <= NoOfOtus));
  Result := FDataCoverage[Index];
end;

procedure TTimeTreeData.SetDataCoverage(Index: Integer; AValue: Double);
begin
  Assert((Index >= 0) and (Index <= NoOfOtus));
  FDataCoverage[Index] := AValue;
end;

procedure TTimeTreeData.SetNode(index: integer; nodedata: TNodeData);
begin
  if (index < 0) or (index >= NoOfOTUs) then Exit;
  FNode[index].des1 := nodedata.des1;
  FNode[index].des2 := nodedata.des2;
end;

procedure TTimeTreeData.SetBLen(index: integer; value: double);
begin
  if (index < 0) or (index >= 2*(NoOfOTUs-1)) then Exit;
  if FBLen = nil then
    Raise Exception.Create('Error: Attempting to set BLen when BLen = nil.');
  FBLen[index] := value;
end;

procedure TTimeTreeData.SetSE(index: integer; value: double);
begin
  if (index < 0) or (index >= 2*(NoOfOTUs-1)) then Exit;
  FSE[index] := value;
end;

procedure TTimeTreeData.SetStats(index: integer; value: double);
begin
  if (index < 0) or (index >= 2*(NoOfOTUs-1)) then Exit;
  FStats[index] := value;
end;

function TTimeTreeData.GetNode(index: integer):TNodeData;
begin
  if (index < 0) or (index >= NoOfOTUs) then Exit;
  Result.des1 := FNode[index].des1;
  Result.des2 := FNode[index].des2;
end;


function TTimeTreeData.GetBLen(index: integer):double;
begin
  Result := 0.0;
  if (index < 0) or (index >= 2*(NoOfOTUs-1)) or (not isBLen) then Exit;
  Result := FBLen[index];
end;


function TTimeTreeData.GetSE(index: integer):double;
begin
  Result := 0.0;
  if (index < 0) or (index >= 2*(NoOfOTUs-1)) or (not isSE) then Exit;
  Result := FSE[index];
end;

function TTimeTreeData.GetStats(index: integer):double;
begin
  Result := 0.0;
  if (index < 0) or (index >= 2*(NoOfOTUs-1)) {or (not isStats)} then Exit;
  Result := FStats[index];
end;

function TTimeTreeData.GetIsBLen:boolean;
begin
  if FBlen = nil then
    Result := false
  else
    Result := true;
end;

function TTimeTreeData.GetIsGeneDupEvent(Index: Integer): Boolean;
begin
  Result := FIsGeneDupEvent[Index];
end;

function TTimeTreeData.GetIsOutgroupMember(Index: Integer): Boolean;
begin
  Result := FIsOutgroupMember[Index];
end;

function TTimeTreeData.GetIsSE:boolean;
begin
  if FSE = nil then
    Result := false
  else
    Result := true;
end;

function TTimeTreeData.GetIsSpeciationEvent(Index: Integer): Boolean;
begin
  Result := FIsSpeciationEvent[Index];
end;

procedure TTimeTreeData.SetIsBLen(value: boolean);
begin
  if value = IsBLen then Exit;
  if value = true then begin
      GetMem(FBLen, SizeOf(Double)*2*(NoOfOTUs-1));
      FillChar(FBLen^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
  end
  else begin
      FreeMemAndNil(FBLen);
  end;
end;

procedure TTimeTreeData.SetIsGeneDupEvent(Index: Integer; const Value: Boolean);
begin
  if FIsGeneDupEvent[Index] = Value then
    Exit;
  FIsGeneDupEvent[Index] := Value;
end;

procedure TTimeTreeData.SetIsOutgroupMember(Index: Integer; const Value: Boolean);
begin
  if FIsOutgroupMember[Index] <> value then
    FIsOutgroupMember[Index] := Value;
end;




procedure TTimeTreeData.RemoveTimetreeID(Index: Integer);
var
  i: Integer;
begin
  Assert(False, 'not implemented');
end;

function TTimeTreeData.GetChildNodeCount(Index: Integer): Integer;
begin
  Assert((Index >= 0) and (Index < Length(FChildNodeCounts)));
  Result := FChildNodeCounts[Index];
end;

procedure TTimeTreeData.SetChildNodeCount(Index: Integer; aValue: Integer);
begin
  Assert((Index >= 0) and (Index < Length(FChildNodeCounts)));
  FChildNodeCounts[Index] := aValue;
end;

function TTimeTreeData.GetTimetreeID(Index: Integer): Integer;
begin
  Assert((Index >= 0) and (Index < Length(FTimetreeIDs)));
  Result := FTimetreeIDs[Index];
end;

procedure TTimeTreeData.SetTimetreeID(Index: Integer; aValue: Integer);
begin
  Assert((Index >= 0) and (Index < Length(FTimetreeIDs)));
  FTimetreeIDs[Index] := aValue;
end;

function TTimeTreeData.HasTimetreeID(aID: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := (Length(FTimetreeIDs) - 1) downto 0 do
  begin
    if aID = FTimetreeIDs[i] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TTimeTreeData.UpdateTimeTreeIDs(const OtuNames: TStringList; const IntNodeLbls: TStringList): Boolean;
var
  i: Integer;
  TempInt: Integer;
begin
  Result := False;
  Assert((OtuNames.Count = FNoOfOTUs) and (IntNodeLbls.Count = (FNoOfOTUs - 2)), Format('namesCount=%d, otuCount=%d, lblsCount=%d, expLblsCount=%d', [OtuNames.Count, FNoOfOTUs, IntNodeLbls.Count, FNoOfOTUs - 2]));
  for i := 0 to (FNoOfOTUs -  1) do
    if TryStrToInt(OtuNames[i], TempInt) then
      FTimetreeIDs[i] := TempInt
    else
      Exit;
  for i := 0 to FNoOfOTUs - 3 do
  begin
    if IntNodeLbls[i] = EmptyStr then { a multifurcation where we added a node ourselves, so it has no ID}
      FTimeTreeIDs[i + FNoOfOTUs] := -1
    else
      if TryStrToInt(IntNodeLbls[i], TempInt) then
        FTimeTreeIDs[i + FNoOfOTUs] := TempInt
      else
        Exit
  end;
  Result := True;
end;


procedure TTimeTreeData.SetIsSE(value: boolean);
begin
  if value = IsSE then Exit;
  if value = true then begin
      GetMem(FSE, SizeOf(Double)*2*(NoOfOTUs-1));
      FillChar(FSE^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
  end
  else begin
      FreeMemAndNil(FSE);
  end;
end;

procedure TTimeTreeData.SetIsSpeciationEvent(Index: Integer; const Value: Boolean);
begin
  if FIsSpeciationEvent[Index] = Value then
    Exit;
  FIsSpeciationEvent[Index] := Value;
end;

procedure TTimeTreeData.SetIsStats(value: boolean);
begin
    if value = IsStats then Exit;
    FillChar(FStats^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
end;

function TTimeTreeData.GetIsStats:boolean;
begin
  Result := FIsStats;
end;

function TTimeTreeData.GetSBL:double;
var i: integer;
begin
  Result := 0.0;
  if isBLen then
    for i := 0 to 2*NoOfOTUs-3 do
      Result := Result +BLenArray[i];
end;

function TTimeTreeData.Assign(Source: TTimeTreeData):boolean;
var
  i: integer;
begin
  Result := false;
  if NoOfOTUs <> Source.NoOfOTUs then Exit;
  isBLen := Source.isBLen;
  isSE := Source.isSE;
  isStats := Source.isStats;
  for i := 0 to NoOfOTUs-2 do
  begin
    NodeArray[i] := Source.NodeArray[i];
    DataCoverage[i] := Source.DataCoverage[i];
  end;
  Freq := Source.Freq;
  Value := Source.Value;
  Value2 := Source.Value2;
  if IsBLen then
    for i := 0 to 2*NoOfOTUs-3 do
    begin
      BLenArray[i] := Source.BLenArray[i];

    end;

  if IsSE then
    for i := 0 to 2*NoOfOTUs-3 do
      SEArray[i] := Source.SEArray[i];
    for i := 0 to 2*NoOfOTUs-3 do
      StatsArray[i] := Source.StatsArray[i];

  for i := 0 to 2 * NoOfOtus - 2 do
  begin
    FIsGeneDupEvent[i] := Source.FIsGeneDupEvent[i];
    FIsSpeciationEvent[i] := Source.FIsSpeciationEvent[i];
    FChildNodeCounts[i] := Source.FChildNodeCounts[i];
    FTimetreeIDs[i] := Source.FTimetreeIDs[i];
  end;

  for i := 0 to Length(FIsOutgroupMember) - 1 do
    FIsOutgroupMember[i] := Source.FIsOutgroupMember[i];
  Result := true;
end;

function TTimeTreeData.AssignDataCoverage(Source: TTimeTreeData): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Source.NoOfOTUs = NoOfOtus then
    Exit;
  for i := Low(FDataCoverage) to High(FDataCoverage) do
    FDataCoverage[i] := Source.DataCoverage[i];
end;

function TTimeTreeData.Identical(Source: TTimeTreeData): boolean;
var
  i: integer;
begin
  result := false;
  if NoOfOTUs <> source.NoOfOTUs then exit;

  result := true;
  for i := 0 to NoOfOTUs-2 do
  begin
    if (NodeArray[i].des1 <> source.NodeArray[i].des1) or
       (NodeArray[i].des1 <> source.NodeArray[i].des2) then
    begin
      result := false;
      break;
    end;
    if (NodeArray[i].des2 <> source.NodeArray[i].des1) or
       (NodeArray[i].des2 <> source.NodeArray[i].des2) then
    begin
      result := false;
      break;
    end;
  end;
end;

// MRCA finds the ancestors of the current NodeIDs and looks for an intersection.
// The first intersection found is the MRCA.
// NOTE: TTimeTreeData nodes are (0:[n-1] are internal, n:[2*n-1] are leaf nodes]
// The Node[] array only contains internal nodes (size: 0:[n-1]).
function TTimeTreeData.MRCA(taxaANodeID, taxaBNodeID: Integer): Integer;
var
  i, j, taxaA, taxaB, mrcaID: Integer;
  taxaAParents, taxaBParents: Array of Integer;
begin
  taxaAParents := nil;
  taxaBParents := nil;
  result := -1;
  mrcaID := -1;
  // Check that the NodeIDs are valid
  if (taxaANodeID > (NoOfOTUs-1)*2) or (taxaANodeID < 0) or (taxaBNodeID > (NoOfOTUs-1)*2) or (taxaBNodeID < 0) then
  begin
    exit;
  end;

  taxaA := taxaANodeID;
  taxaB := taxaBNodeID;

  repeat
    // find parent of taxaA.
    // NOTE: Node[0] in TTimeTreeData is actually the first internal node.
    for i := 0 to NoOfOTUs - 1 do
    begin
      // locate direct ancestor of taxaA
      if (Node[i].des1 = taxaA) or (Node[i].des2 = taxaA) then
      begin
        SetLength(TaxaAParents, Length(TaxaAParents) + 1);
        TaxaAParents[Length(TaxaAParents) - 1] := i;
        break; // ONLY 1 possible direct parent
      end;
    end;

    // find parent of taxaB
    for i := 0 to NoOfOTUs - 1 do
    begin
      // locate direct ancestor of taxaB
      if (Node[i].des1 = taxaB) or (Node[i].des2 = taxaB) then
      begin
        SetLength(TaxaBParents, Length(TaxaBParents) + 1);
        TaxaBParents[Length(TaxaBParents) - 1] := i;
        break; // ONLY 1 possible direct parent
      end;
    end;

    // Look for an intersection, the intersection between taxaAParents and taxaBParents is the most recent common ancestor.
    for i := 0 to length(taxaAParents)-1 do
    begin
      for j := 0 to length(taxaBparents)-1 do
        if taxaAParents[i] = taxaBParents[j] then
        begin
          mrcaID := taxaAParents[i];
          break;
        end;
    end;


    // If no MRCA is found, we go up a level to the parents of the parents.
    if mrcaID = -1 then
    begin
      // NOTE: You need to add NoOfOTUs because we search for the NodeID, but we store/return the internal NodeID [0:n-1].
      taxaA := taxaAParents[length(taxaAParents)-1] + NoOfOTUs;
      taxaB := taxaBParents[length(taxaBParents)-1] + NoOfOTUs;
    end;

  until mrcaID > -1;

  result := mrcaID; // Return internal node ID (starts at 0 for the first internal node).
end;

function TTimeTreeData.NumGeneDups: Integer;
var
  i: Integer;
begin
  Result := 0;
  if Length(FIsGeneDupEvent) > 0 then
   for i := 0 to Length(FIsGeneDupEvent) - 1 do
     if FIsGeneDupEvent[i] = True then
       inc(Result);
end;

function TTimeTreeData.NumSpeciations: Integer;
var
  i: Integer;
begin
  Result := 0;
  if Length(FIsSpeciationEvent) > 0 then
    for i := 0 to Length(FIsSpeciationEvent) - 1 do
      if FIsSpeciationEvent[i] = True then
        inc(Result);
end;

procedure TTimeTreeData.ReadFromFile(var SessionFile: File; SessionFileVersion: integer);
var
  i, n: Integer;
begin
  BlockRead(SessionFile, FFreq, SizeOf(Double));
  BlockRead(SessionFile, FValue, SizeOf(Double));
  BlockRead(SessionFile, FValue2, SizeOf(Double));
  BlockRead(SessionFile, FNode^, SizeOf(TNodeData)*(NoOfOTUs-1));
  if isBLen then
    BlockRead(SessionFile, FBLen^, SizeOf(Double)*2*(NoOfOTUs-1));
  if isSE then
    BlockRead(SessionFile, FSE^, SizeOf(Double)*2*(NoOfOTUs-1));
  if SessionFileVersion > 400 then
    BlockRead(SessionFile, FStats^, SizeOf(Double)*2*(NoOfOTUs-1))
  else if isStats then
    BlockRead(SessionFile, FStats^, SizeOf(Double)*2*(NoOfOTUs-1));

  if SessionFileVersion >= 606 then
  begin
    BlockRead(SessionFile, n, SizeOf(n));
    if n > 0 then
      for i := 0 to n - 1 do
        BlockRead(SessionFile, FDataCoverage[i], SizeOf(Double));
  end;

end;

procedure TTimeTreeData.ReadGeneDupInfoFromSession(var SessionFile: File);
var
  NumNodes: Integer;
  i: Integer;
  IsDup, IsSpec: Boolean;
begin
  NumNodes := 0;
  IsDup := False;
  IsSpec := False;


  BlockRead(SessionFile, NumNodes, SizeOf(Integer));
  SetLength(FIsGeneDupEvent, NumNodes);
  if NumNodes > 0 then
    for i := 0 to NumNodes - 1 do
    begin
      BlockRead(SessionFile, IsDup, SizeOf(Boolean));
      FIsGeneDupEvent[i] := IsDup;
    end;

  BlockRead(SessionFile, NumNodes, SizeOf(Integer));
  SetLength(FIsSpeciationEvent, NumNodes);
  if NumNodes > 0 then
    for i := 0 to NumNodes - 1 do
    begin
      BlockRead(SessionFile, IsSpec, SizeOf(Boolean));
      FIsSpeciationEvent[i] := IsSpec;
    end;
end;

procedure TTimeTreeData.WriteGeneDupInfoToSession(var SessionFile: File);

var
  i: Integer;
  IsDup, IsSpec: Boolean;
begin

  i := length(FIsGeneDupEvent);
  BlockWrite(SessionFile, i, SizeOf(Integer));
  if i > 0 then
    for i := 0 to length(FIsGeneDupEvent) - 1do
    begin
      IsDup := FIsGeneDupEvent[i];
      BlockWrite(SessionFile, IsDup, SizeOf(Boolean));
    end;

  i := length(FIsSpeciationEvent);
  BlockWrite(SessionFile, i, SizeOf(Integer));
  if i > 0 then
    for i := 0 to Length(FIsSpeciationEvent) - 1 do
    begin
      IsSpec := FIsSpeciationEvent[i];
      BlockWrite(SessionFile, IsSpec, SizeOf(Boolean));
    end;
end;

procedure TTimeTreeData.WriteToFile(var SessionFile: File);
var
  i, n: Integer;
  Coverage: Double;
begin
  BlockWrite(SessionFile, FFreq, SizeOf(Double));
  BlockWrite(SessionFile, FValue, SizeOf(Double));
  BlockWrite(SessionFile, FValue2, SizeOf(Double));
  BlockWrite(SessionFile, FNode^, SizeOf(TNodeData)*(NoOfOTUs-1));
  if isBLen then
    BlockWrite(SessionFile, FBLen^, SizeOf(Double)*2*(NoOfOTUs-1));
  if isSE then
    BlockWrite(SessionFile, FSE^, SizeOf(Double)*2*(NoOfOTUs-1));
  BlockWrite(SessionFile, FStats^, SizeOf(Double)*2*(NoOfOTUs-1));

  n := Length(FDataCoverage);
  BlockWrite(SessionFile, n, SizeOf(n));
  if Length(FDataCoverage) > 0 then
    for i := 0 to Length(FDataCoverage) - 1 do
      BlockWrite(SessionFile, FDataCoverage[i], SizeOf(Double));
end;

procedure TTimeTreeData.RemoveOTU(target: integer);
var
  i,parent,sibling: integer;
begin
  if (target < 0) or (target >= NoOfOTUs) then
    Exit;
  parent := 0;
  sibling := 0;


  for i := 0 to NoOfOTUs-2 do
  begin
    if (NodeArray[i].des1 = target) or (NodeArray[i].des2 = target) then { find the parent node}
    begin
      parent := i;
      if NodeArray[i].des1 = target then { find the sibling node}
        sibling := NodeArray[i].des2
      else
        sibling := NodeArray[i].des1;
      if IsBLen then
        BLenArray[sibling] := BLenArray[sibling] +BLenArray[parent+NoOfOTUs]; { add parent blen to sibling}
      if IsSE then
        SEArray[sibling] := Sqrt(SEArray[sibling]*SEArray[sibling] +SEArray[parent+NoOfOTUs]*SEArray[parent+NoOfOTUs]);
      Break;
    end;
  end;

  for i := 0 to NoOfOTUs-3 do
  begin
    if i >= parent then
    begin
      NodeArray[i].des1 := NodeArray[i+1].des1;
      NodeArray[i].des2 := NodeArray[i+1].des2;
    end;

    if NodeArray[i].des1 = parent+NoOfOTUs then
      NodeArray[i].des1 := sibling
    else if NodeArray[i].des2 = parent+NoOfOTUs then
      NodeArray[i].des2 := sibling;

    if NodeArray[i].des1 > parent+NoOfOTUs then
      NodeArray[i].des1 := NodeArray[i].des1 -2
    else if NodeArray[i].des1 > target then
      NodeArray[i].des1 := NodeArray[i].des1 -1;
    if NodeArray[i].des2 > parent+NoOfOTUs then
      NodeArray[i].des2 := NodeArray[i].des2 -2
    else if NodeArray[i].des2 > target then
      NodeArray[i].des2 := NodeArray[i].des2 -1;
  end;

  for i := 0 to 2*NoOfOTUs-2 do
    if i > parent+NoOfOTUs then
    begin
      FTimeTreeIDs[i-2] := FTimeTreeIDs[i];
      FChildNodeCounts[i-2] := FChildNodeCounts[i];
    end
    else if i > target then
    begin
      FTimeTreeIDs[i-1] := FTimeTreeIDs[i];
      FChildNodeCounts[i-1] := FChildNodeCounts[i];
    end;

  if IsBLen then
    for i := 0 to 2*NoOfOTUs-3 do
      if i > parent+NoOfOTUs then
        BLenArray[i-2] := BLenArray[i]
      else if i > target then
        BLenArray[i-1] := BLenArray[i];
  if IsSE then
    for i := 0 to 2*NoOfOTUs-3 do
      if i > parent+NoOfOTUs then
        SEArray[i-2] := SEArray[i]
      else if i > target then
        SEArray[i-1] := SEArray[i];

    for i := 0 to 2*NoOfOTUs-3 do
      if i > parent+NoOfOTUs then
        StatsArray[i-2] := StatsArray[i]
      else if i > target then
        StatsArray[i-1] := StatsArray[i];
  NoOfOTUs := NoOfOTUs-1;
end;

procedure TTimeTreeData.AddOTU(node: integer);
var
  i: integer;
begin
  if (node < 0) or (node > 2*NoOfOTUs-2) then Exit;
  NoOfOTUs := NoOfOTUs +1;
  for i := 0 to NoOfOTUs-3 do
  begin
    if NodeArray[i].des1 = node then
      NodeArray[i].des1 := 2*NoOfOTUs-3
    else if NodeArray[i].des1 >= NoOfOTUs-1 then
      NodeArray[i].des1 := NodeArray[i].des1+1;
    if NodeArray[i].des2 = node then
      NodeArray[i].des2 := 2*NoOfOTUs-3
    else if NodeArray[i].des2 >= NoOfOTUs-1 then
      NodeArray[i].des2 := NodeArray[i].des2+1;
  end;
  if node = 2*(NoOfOTUs-1)-2 then
  begin
    if node < NoOfOTUs-1 then
      NodeArray[NoOfOTUs-2].des1 := node
    else
      NodeArray[NoOfOTUs-2].des1 := node+1;
    NodeArray[NoOfOTUs-2].des2 := NoOfOTUs-1;
  end
  else
  begin
    NodeArray[NoOfOTUs-2].des1 := NodeArray[NoOfOTUs-3].des1;
    NodeArray[NoOfOTUs-2].des2 := NodeArray[NoOfOTUs-3].des2;
    if node < NoOfOTUs-1 then
      NodeArray[NoOfOTUs-3].des1 := node
    else
      NodeArray[NoOfOTUs-3].des1 := node+1;
    NodeArray[NoOfOTUs-3].des2 := NoOfOTUs-1;
  end;

  if IsBLen then
  begin
    BLenArray[2*NoOfOTUs-3] := 0;
    for i := 2*NoOfOTUs-4 downto NoOfOTUs do
      BLenArray[i] := BLenArray[i-1];
    BLenArray[NoOfOTUs-1] := 0;
  end;
  if IsSE then
  begin
    SEArray[2*NoOfOTUs-3] := 0;
    for i := 2*NoOfOTUs-4 downto NoOfOTUs do
      SEArray[i] := SEArray[i-1];
    SEArray[NoOfOTUs-1] := 0;
  end;

    StatsArray[2*NoOfOTUs-3] := 0;
    for i := 2*NoOfOTUs-4 downto NoOfOTUs do
      StatsArray[i] := StatsArray[i-1];
    StatsArray[NoOfOTUs-1] := 0;

end;

function TTimeTreeData.RootIndex: integer;
var
  i: integer;
  flag: array of boolean;
begin
  result := -1;
  setlength(flag, 2*NoOfOTUs-1);
  for i := NoOfOTUs to 2*NoOfOTUs-2 do
    flag[i] := false;
  for i := 0 to NoOfOTUs-2 do
  begin
    flag[NodeArray[i].des1] := true;
    flag[NodeArray[i].des2] := true;
  end;
  for i := 2*NoOfOTUs-2 downto NoOfOTUs do
    if not flag[i] then
    begin
      result := i;
      break;
    end;
  setlength(flag, 0);
end;

function TTimeTreeData.StateToStringList(Comment: AnsiString): TStringList;
var
  MyStringList: TStringList;
  i: Integer;
  MyCount: Integer;
begin
  MyStringList := TStringList.Create();
  MyStringList.Add('TTreeData=MyName');

  if Comment <> '' then
	MyStringList.Add('TTreeData.Comment=' + Comment);

  MyStringList.Add('TTreeData.NoOfOTUs=' + IntToStr(NoOfOTUs));

  MyStringList.Add('TTreeData.Value=' + FloatToStrF(Value, ffFixed, 4, 4));
  MyStringList.Add('TTreeData.Value2=' + FloatToStrF(Value2, ffFixed, 4, 4));
  MyStringList.Add('TTreeData.Freq=' + FloatToStrF(Freq, ffFixed, 4, 4));
  MyStringList.Add('TTreeData.SBL=' + FloatToStrF(SBL, ffFixed, 4, 4));
  MyStringList.Add('TTreeData.isBLen=' + BoolToStr(isBLen, True));
  MyStringList.Add('TTreeData.isSE=' + BoolToStr(isSE, True));
  MyStringList.Add('TTreeData.isStats=' + BoolToStr(isStats, True));

  MyCount := (NoOfOTUs-1);
  for i := 0 to MyCount - 1 do
    MyStringList.Add('TTreeData.Node[' + IntToStr(i) + ']=des1:' + IntToStr(Node[i].des1) + ', des2:' + IntToStr(Node[i].des2));

  MyCount := 2*(NoOfOTUs-1);
  for i := 0 to MyCount - 1 do
    MyStringList.Add('BLen[' + IntToStr(i) + ']=' + FloatToStrF(BLen[i], ffFixed, 4, 4));


  for i := 0 to MyCount do
    MyStringList.Add('ttID[' + IntToStr(i) + ']=' + IntToStr(FTimetreeIDs[i]));
  if IsSe then
    for i := 0 to MyCount - 1 do
      MyStringList.Add('SE[' + IntToStr(i) + ']=' + FloatToStrF(FSE[i], ffFixed, 4, 4));

  if IsStats then
    for i := 0 to MyCount - 1 do
      MyStringList.Add('Stats[' + IntToStr(i) + ']=' + FloatToStrF(FStats[i], ffFixed, 4, 4));

  Result := MyStringList;
end;

function TTimeTreeData.HasNegativeBranchLengths: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to 2*(NoOfOTUs-1) - 1 do
    if BLen[i] < 0 then
    begin
      Result := True;
      break;
    end;
end;

end.
