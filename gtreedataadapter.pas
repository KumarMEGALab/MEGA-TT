unit gtreedataadapter;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, gtreedata, gtreenode,
  MLongintList, ttconst, gtaxonomicrank, gnamesmap;

type
  { given a TTimeTreeData object, provides a linked-tree interface that is easier to
    work with and then updates the TTimeTreeData object as needed}

  { TFpNodeTreeDataAdapter }

  TFpNodeTreeDataAdapter = class(TObject)
    private
      FNodes: ^TpNodeArray;
      FNumNodes: Integer;
      FNumTaxa: Integer;
      FRoot: TpNode;
      FIsBranchLength: Boolean;
      FIsSE: Boolean;
      FIsStats: Boolean;
      FMostShallowRank: TTaxonomicRank;
      { this stuff is for the MEGA TimeTree pruning function}
      FOtuNames: TStringList; { we don't own it so don't free it}
      FTTNodeIds: TLongIntList; { list of node IDs for the nodes that will be retained, anything not in the list is removed}
      FTimeTreeData: TTimeTreeData; { we don't own it so don't free it, we are trying to save time by not copying it because performance is an issue and the tree has 50k+ taxa}
      function DoPruneTreeSlow: Boolean; { just a second, more naive way of doing it for comparing results as a sanity check}
      function DoPruneTreeFast(var OtuNames: TStringList): Boolean; overload; { this is the Bad Mamba, does it in log(n) time}
      function DoPruneTreeFast(var OtuNames: TStringList; MostShallowRank: TTaxonomicRank): Boolean; overload;
      procedure CorrectBlens;
      function ReBuildTimetree(const n: Integer; var OtuNames: TStringList): Boolean; overload;
      function ReBuildTimetree(var OtuNames: TStringList): Boolean; overload;
      function RebuildTimetree2(var OtuNames: TStringList): Boolean;
      procedure RemoveSneakyTaxa(var OtuNames: TStringList);
      procedure CompressTree;
      function MarkIncludedNodes: Boolean; overload;
      function MarkIncludedNodes(MostShallowRank: TTaxonomicRank): Boolean; overload;
      function IsIncludedNode(aNode: TpNode): Boolean; overload;
      function IsIncludedNode(aNode: TpNode; MostShallowRank: TTaxonomicRank): Boolean; overload;
      function IsInternalNode(aNode: TpNode): Boolean;
      function HasDescendentAtMostShallowRank(aNode: TpNode): Boolean;
      function NodeIsNewLeafNode(aNode: TpNode; MostShallowRank: TTaxonomicRank): Boolean;
      function ChildrenShareParentsRank(aNode: TpNode): Boolean;
      function ChildrenShareParentsPutativeRank(aNode: TpNode): Boolean;
      function ChildrenPutativeRanksAreTooShallow(aNode: TpNode): Boolean;
      function PutativeRankIsTooShallow(aNode: TpNode): Boolean;
      function RealRankIsTooShallow(aNode: TpNode): Boolean;
      function RealRankIsDefined(aNode: TpNode): Boolean;
      function RealRankAndChildRealRanksDefined(aNode: TpNode): Boolean;
      function AllIdsPresentInTree(NodeIds: TLongIntList): Boolean;
      procedure FindPutativeRanks(aNode: TpNode); overload;
      procedure FindPutativeRanks(aNode: TpNode; StartRank: TTaxonomicRank); overload;
      procedure FindPutativeRankFromAncestors(aNode: TpNode);
      function FindHeightOfTimetreeNode(aNode: TpNode): double;
      procedure InitTpNodes;
      procedure InitRankCounts;
      procedure FreeRankCounts;
      procedure SetNumTaxa(AValue: Integer);
      procedure TurnNode(p : TpNode);
      function CountFlaggedNodes: Integer;
      function CountNodesWithShallowestRank(aNode: TpNode): Integer;
      function CountLeafNodesAtShallowestRank(aNode: TpNode): Integer;
      function CountOtus: Integer;
      function CountChildNodes(aNode: TpNode): Integer;
      procedure CountLeaves;
      function GetNewickString(aData: TTimeTreeData; TreeName: String=''): String; overload;
      function GetNewickString(aRoot: TpNode; TreeName: String=''): String; overload;
      function GetIncludedNodesInfo: TStringList;
      function WriteTreeAsCSV(aRoot: TpNode; TreeName: String=''): TStringList;
      { for the case where a single Id is used (like when restricting depth by taxonomic rank),
        this can be used. This CANNOT be used when multiple IDs are given because it may be
        necessary to find their common ancestor and link them together}
      function FindNewRootForSingleIdSearch(TimetreeId: LongInt): TpNode;
      procedure IterateNodeHeights;

      function GetLeafNodeIds(n: TpNode): TIntArray;
      function BothDescendentsAreOtus(n: TpNode): Boolean;
      function Descendent1IsOtu(n: TpNode): Boolean;
      function Descendent2IsOtu(n: TpNode): Boolean;
      function PairwiseDist(mrca, node1, node2: Integer): Extended;
    public

      constructor Create;
      destructor Destroy; override;
      function TreeToTabularFormat(const Tree: TTimeTreeData; const otuNames: TStringList; const filename: String): Boolean;
      procedure SetTreeData(const AData: TTimeTreeData; isConvertToTabular: Boolean);
      procedure GetTreeData(var AData: TTimeTreeData);
      procedure RootOnOutgroup;
      property NumTaxa: Integer read FNumTaxa write SetNumTaxa;
      property NumNodes: Integer read FNumNodes;
      function GetPercentMissingBlensData: Double;
      function GetNodeHeights: TDoubleArray;
      function GetTreeBoxTreeReference: TpNodeArray; { TSimpleTreeDataAdapter owns it and will free the memory used for it. Other classes should only reference it}
      function PruneTree(const NodeIds: TLongIntList; OtuNames: TStringList; DoSlowVersion: Boolean=False): Boolean; overload;
      function PruneTree(const NodeIds: TLongIntList; OtuNames: TStringList; MostShallowRank: TTaxonomicRank; DoSlowVersion: Boolean=False): Boolean; overload;
      function CountRanks(var aList: TStringList): Boolean;
      function RankCountsCsvHeaderStr: String;
      function RankCountsToCsvStr(aNode: TpNode): String;
      procedure AddRankCounts(var Destination: TRanksCount; const Source: TRanksCount);
      function GetMrcaMatrix: T2DArrayOfInteger;
      function GetPairwiseDistances: T2dArrayOfExtended;
      //function GetPairwiseDistances(aData: TTimeTreeData; IsRooted: Boolean; IsLinearized: Boolean): PDistanceMatrix; overload;
  end;

implementation

uses
  gutils, gtreeproc, gtreelist, math;

{ TFpNodeTreeDataAdapter }

function TFpNodeTreeDataAdapter.DoPruneTreeSlow: Boolean;
var
  i: Integer;
begin
  Result := False;
  MarkIncludedNodes;
  for i := NumTaxa downto 1 do
  begin
    if not FNodes[i].flag then
    begin
      FTimeTreeData.RemoveOTU(i-1);
      FOtuNames.Delete(i-1);
    end;
  end;
  Result := True;
end;

function TFpNodeTreeDataAdapter.DoPruneTreeFast(var OtuNames: TStringList): Boolean;
var
  NumTaxaKept: Integer;
  NewRoot: TpNode;

  function ProcessNode(aNode: TpNode): TpNode;
  begin
    if aNode.OTU then
    begin
      if aNode.flag then
      begin
        Result := aNode;
        inc(NumTaxaKept);
      end
      else
        Result := nil;
    end
    else
    begin
      aNode.des1 := ProcessNode(aNode.des1);
      aNode.des2 := ProcessNode(aNode.des2);

      if not aNode.flag then
      begin
        if (aNode.des1 <> nil) and (aNode.des2 <> nil) then { don't exclude a node if both its children are included}
        begin
          Result := aNode;
          aNode.flag := true; { important so it gets added back to TTimeTreeData}
        end
        else if aNode.des1 <> nil then
        begin
          Result := aNode.des1;
          Result.branch.length := Result.branch.length + aNode.branch.length;
        end
        else if aNode.des2 <> nil then
        begin
          Result := aNode.des2;
          Result.branch.length := Result.branch.length + aNode.branch.length;
        end
        else
          Result := nil;
      end
      else
      begin
        Result := aNode;
      end;
    end;
  end;

begin
  Result := False;
  NumTaxaKept := 0;
  MarkIncludedNodes;
  NewRoot := ProcessNode(FRoot);
  FRoot := NewRoot;
  CorrectBlens;
  CompressTree;
  Result := ReBuildTimetree(NumTaxaKept, OtuNames);
end;

function TFpNodeTreeDataAdapter.DoPruneTreeFast(var OtuNames: TStringList; MostShallowRank: TTaxonomicRank): Boolean;
var
  NumTaxaKept: Integer;
  NewRoot: TpNode;
  aRank: TTaxonomicRank;
  NodeInfo: TStringList=nil;
  RootId: LongInt;

  function ProcessNode(aNode: TpNode): TpNode;
  begin
    if aNode.OTU then
    begin
      if aNode.flag then
      begin
        Result := aNode;
        inc(NumTaxaKept);
      end
      else
        Result := nil;
    end
    else
    begin
      aNode.des1 := ProcessNode(aNode.des1);
      aNode.des2 := ProcessNode(aNode.des2);

      if not aNode.flag then
      begin
        if (aNode.des1 <> nil) and (aNode.des2 <> nil) then { don't exclude a node if both its children are included}
        begin
          Result := aNode;
          aNode.flag := true; { important so it gets added back to TTimeTreeData}
        end
        else if aNode.des1 <> nil then
        begin
          Result := aNode.des1;
        end
        else if aNode.des2 <> nil then
        begin
          Result := aNode.des2;
        end
        else
          Result := nil;
      end
      else
      begin
        Result := aNode;
      end;
    end;
  end;

begin
  FMostShallowRank := MostShallowRank;
  aRank := TaxonomicRanks[FTTNodeIds[0]];
  RootId := FTTNodeIds[0];
  if aRank >= FMostShallowRank then
    raise Exception.Create('invalid taxonomic rank given for search query');
  MarkIncludedNodes(MostShallowRank);
  {$IFDEF DEBUG}
  NodeInfo := GetIncludedNodesInfo;
  NodeInfo.SaveToFile('/home/gstecher/Downloads/nodeInfo.csv');
  NodeInfo.Free;
  {$ENDIF}
  NewRoot := FindNewRootForSingleIdSearch(RootId);
  {$IFDEF DEBUG}
  GetNewickString(NewRoot, 'before');
  {$ENDIF}
  NewRoot := ProcessNode(NewRoot);
  FRoot := NewRoot;
  {$IFDEF DEBUG}
  GetNewickString(NewRoot, 'after');
  {$ENDIF}
  Result := ReBuildTimetree2(OtuNames);
  RemoveSneakyTaxa(OtuNames);
end;

procedure TFpNodeTreeDataAdapter.CorrectBlens;
var
  TempNode: TpNode;

  procedure ProcessNode(aNode: TpNode);
  begin
    if not aNode.flag then
    begin
      if Assigned(aNode.des1) then
        ProcessNode(aNode.des1);
      if Assigned(aNode.des2) then
        ProcessNode(aNode.des2);
    end
    else
    begin

      TempNode := aNode;
      while Assigned(TempNode.anc) do
      begin
        TempNode := TempNode.anc;
        aNode.branch.length := aNode.branch.length - TempNode.branch.length;
      end;
    end;
  end;

begin
  ProcessNode(FRoot);
end;

function TFpNodeTreeDataAdapter.ReBuildTimetree(const n: Integer; var OtuNames: TStringList): Boolean;
var
  aData: TTimeTreeData;
  i: Integer; { current location in FNodes}
  Index: Integer; { current location in the TTimeTreeData arrays}
begin
  if n < 3 then
    raise Exception.Create('Pruned tree must have at least 3 taxa');
  Result := False;
  aData := nil;
  Index := 2*(n - 1); { start at last array element}
  try
    aData := TTimeTreeData.Create(n, True, False, False);
    for i := (NumNodes - 1) downto 0 do
    begin
      if not FNodes[i+1].flag then { node is pruned}
      begin
        if i < NumTaxa then { must remove it from OtuNames}
          OtuNames.Delete(i);
        continue;
      end;
      if i > (NumTaxa-1) then { still traversing internal nodes in the tree}
      begin
        AData.NodeArray[Index - n].des1 := FNodes[i + 1].des1.index-1; { Index-n because NodeArray does not include leaf nodes}
        AData.NodeArray[Index - n].des2 := FNodes[i + 1].des2.index-1;
      end;
      AData.BLenArray[Index] := FNodes[i+1].branch.length; { all nodes have branch lengths}
      AData.SetTimetreeID(Index, FNodes[i+1].timetreeId); { all nodes have IDs}
      dec(Index);
    end;
    FTimeTreeData.NoOfOTUs := n;
    FTimeTreeData.Assign(AData);
    Result := True;
  finally
    if Assigned(AData) then
      AData.Free;
  end;
end;

function TFpNodeTreeDataAdapter.CountOtus: Integer;
var
  j: Integer;
begin
  Result := 0;
  for j := 1 to NumNodes do
    if FNodes[j].flag and FNodes[j].OTU then
      inc(Result);
end;

function TFpNodeTreeDataAdapter.CountChildNodes(aNode: TpNode): Integer;
const
  ZERO_VAL = 0.0000000001;
var
  i: Integer;

  procedure CountChildren(currentNode: TpNode);
  begin
    if currentNode.Otu then
    begin
      inc(Result);
      Exit;
    end;

    if currentNode.des1.branch.length > ZERO_VAL then
    begin
      //if (currentNode.des1.rank <> trNoRank) and (currentNode.des1.rank <> trUnknown) then
        inc(Result);
    end
    else
      CountChildren(currentNode.des1);

    if currentNode.des2.branch.length > ZERO_VAL then
    begin
      //if (currentNode.des2.rank <> trNoRank) and (currentNode.des2.rank <> trUnknown) then
        inc(Result);
    end
    else
      CountChildren(currentNode.des2);
  end;

begin
  Result := 0;
  CountChildren(aNode);
end;

procedure TFpNodeTreeDataAdapter.CountLeaves;

  procedure CountLeavesRecursive(aNode: TpNode);
  begin
    if aNode.OTU then
      aNode.numLeaves := 0
    else
    begin
      CountLeavesRecursive(aNode.des1);
      CountLeavesRecursive(aNode.des2);
      if aNode.des1.OTU then
        aNode.numLeaves := 1
      else
        aNode.numLeaves := aNode.numLeaves + aNode.des1.numLeaves;
      if aNode.des2.OTU then
        aNode.NumLeaves := aNode.numLeaves + 1
      else
        aNode.numLeaves := aNode.numLeaves + aNode.des2.numLeaves;
    end;
  end;

begin
  CountLeavesRecursive(FRoot);
end;

function TFpNodeTreeDataAdapter.GetNewickString(aData: TTimeTreeData; TreeName: String=''): String;
var
  aNamesList: TStringList=nil;
  aTreeList: TTimeTreeList=nil;
  i: Integer;
  aName: String;
  aRank: TTaxonomicRank;
begin
  Result := EmptyStr;
  try
    aNamesList := TStringList.Create;
    aNamesList.Assign(FOtuNames);
    for i := 0 to aData.NoOfOTUs - 1 do
    begin
      aName := aNamesList[i];
      aRank := TaxonomicRanks[aData.GetTimetreeID(i)];
      aName := Format('%s-%d-%s', [aName, i, TaxonomicRankToString(aRank)]);
      aNamesList[i] := aName;
    end;
    aTreeList := TTimeTreeList.Create;
    aTreeList.Add(aData);
    aTreeList.OTUNameList := aNamesList;
    Result := aTreeList.OutputNewickTree(0, True, False, 0.0);
    aTreeList.Remove(0);
  finally
    { aNamesList will be freed by aTreeList}
    if Assigned(aTreeList) then
      aTreeList.Free;
  end;
end;

function TFpNodeTreeDataAdapter.GetNewickString(aRoot: TpNode; TreeName: String): String;
var
  aList: TStringList=nil;

  function ProcessNode(aNode: TpNode): String;
  var
    ttId: String;
    NameStr: String;
  begin
    ttId := IntToStr(aNode.timetreeId);
    if NamesMap.Contains(ttId) then
      NameStr := TOtuName(NamesMap[ttId]).Name
    else
      NameStr := ttId;

    if ANode.OTU then
    begin
      Result := '''' + NameStr + '-' + TaxonomicRankToString(aNode.rank) + '''';
      exit;
    end;
    Result := '(' + ProcessNode(ANode.des1) + ':' + Format('%.3e', [ANode.branch.length]) + ',' + ProcessNode(ANode.des2) + ':' + Format('%.3e', [ANode.branch.length]) + ')' + '''' + NameStr + '-' + TaxonomicRankToString(aNode.rank) + '''';
  end;

begin
  Result := ProcessNode(aRoot);
  {$IFDEF DEBUG}
  aList := TStringList.Create;
  aList.Add(Result);
  aList.SaveToFile('/home/gstecher/Downloads/' + TreeName + '_tree.nwk');
  aList.Free;
  {$ENDIF}
end;


function TFpNodeTreeDataAdapter.GetIncludedNodesInfo: TStringList;
var
  i: Integer;
  NodeInfo: String;
begin
  Result := TStringList.Create;
  NodeInfo := NodeInfoCSVHeader;
  Result.Add(NodeInfo);
  if NumNodes > 0 then
    for i := 1 to NumNodes do
      if FNodes[i].flag then
      begin
        NodeInfo := NodeInfoAsCSV(FNodes[i]);
        Result.Add(NodeInfo);
      end;
end;

function TFpNodeTreeDataAdapter.WriteTreeAsCSV(aRoot: TpNode; TreeName: String): TStringList;

  procedure WriteNode(aNode: TpNode);
  var
    NodeInfo: String;
  begin
    NodeInfo := NodeInfoAsCSV(aNode);
    Result.Add(NodeInfo);
    if Assigned(aNode.des1) then
      WriteNode(aNode.des1);
    if Assigned(aNode.des2) then
      WriteNode(aNode.des2);
  end;

begin
  Result := TStringList.Create;
  Result.Add(NodeInfoCSVHeader);
  WriteNode(aRoot);
  {$IFDEF DEBUG}
  Result.SaveToFile('/home/gstecher/Downloads/' + TreeName + '_tree.csv');
  {$ENDIF}
end;

function TFpNodeTreeDataAdapter.FindNewRootForSingleIdSearch(TimetreeId: LongInt): TpNode;
var
  i: Integer;
begin
  for i := 1 to NumNodes do
  begin
    if FNodes[i].timetreeId = TimetreeId then
    begin
      Result := FNodes[i];
      Exit;
    end;
  end;
end;

procedure TFpNodeTreeDataAdapter.IterateNodeHeights;

  procedure SetRootHeight(p : TpNode);
  begin
    with p^ do
      if OTU then
        height := 0
      else
      begin
        SetRootHeight(des1);
        SetRootHeight(des2);

        height := max(des1.height+des1.branch.length,
                      des2.height+des2.branch.length);
      end;
  end;

  procedure SetEachHeight(p : TpNode);
  begin
    with p^ do
      begin
        height := anc.height -branch.length;
        if not OTU then
        begin
          SetEachHeight(des1);
          SetEachHeight(des2);
        end;
      end
  end;

begin
  SetRootHeight(FRoot);
  SetEachHeight(FRoot.des1);
  SetEachHeight(FRoot.des2);
end;

function TFpNodeTreeDataAdapter.GetLeafNodeIds(n: TpNode): TIntArray;
var
  i: Integer;

  procedure FindLeafNodeIds(aNode: TpNode);
  begin
    if aNode.Otu then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := aNode.index;
    end
    else
    begin
      if Assigned(aNode.Des1) then
        FindLeafNodeIds(aNode.Des1);
      if Assigned(aNode.Des2) then
        FindLeafNodeIds(aNode.Des2);
    end;
  end;

begin
  SetLength(Result, 0);
  if n.Otu then
    Exit;
  if Assigned(n.Des1) then
    FindLeafNodeIds(n.Des1);
  if Assigned(n.Des2) then
    FindLeafNodeIds(n.Des2);
end;

function TFpNodeTreeDataAdapter.BothDescendentsAreOtus(n: TpNode): Boolean;
begin
  Result := (Descendent1IsOtu(n) and Descendent2IsOtu(n));
end;

function TFpNodeTreeDataAdapter.Descendent1IsOtu(n: TpNode): Boolean;
begin
  Result := (not n.Otu) and Assigned(n.Des1) and n.Des1.Otu;
end;

function TFpNodeTreeDataAdapter.Descendent2IsOtu(n: TpNode): Boolean;
begin
  Result := (not n.Otu) and Assigned(n.Des2) and n.Des2.Otu;
end;

function TFpNodeTreeDataAdapter.PairwiseDist(mrca, node1, node2: Integer): Extended;
var
  anc: TpNode;
begin
  Assert(FNodes[node1].Otu and FNodes[node2].Otu, 'PairwiseDist only available for leaf nodes');
  Result := FNodes[node1].Branch.length;
  anc := FNodes[node1].anc;
  while Assigned(anc) and (anc.index <> mrca) do
  begin
    Result := Result + anc.Branch.length;
    anc := anc.anc;
  end;
end;

function TFpNodeTreeDataAdapter.CountFlaggedNodes: Integer;
var
  j: Integer;
begin
  Result := 0;
  for j := 1 to NumNodes do
    if FNodes[j].flag then
      inc(Result);
end;

function TFpNodeTreeDataAdapter.CountNodesWithShallowestRank(aNode: TpNode): Integer;
var
  nodeCount: Integer;

  procedure ProcessNode(target: TpNode);
  begin
    if target.rank = FMostShallowRank then
      nodeCount := nodeCount + 1
    else
    begin
      if target.des1 <> nil then
        ProcessNode(target.des1);
      if target.des2 <> nil then
        ProcessNode(target.des2);
    end;
  end;

begin
  nodeCount := 0;
  ProcessNode(aNode);
  Result := nodeCount;
end;

function TFpNodeTreeDataAdapter.CountLeafNodesAtShallowestRank(aNode: TpNode): Integer;
var
  nodeCount: Integer;

  procedure ProcessNode(target: TpNode);
  begin
    if NodeIsNewLeafNode(target, FMostShallowRank) and RealRankIsDefined(target) and (target.rank <= FMostShallowRank) then
      nodeCount := nodeCount + 1
    else
    begin
      if Assigned(target.des1) then
        ProcessNode(target.des1);
      if Assigned(target.des2) then
        ProcessNode(target.des2);
    end;
  end;

begin
  nodeCount := 0;
  if HasDescendentAtMostShallowRank(aNode) then
    ProcessNode(aNode);
  Result := nodeCount;
end;

function TFpNodeTreeDataAdapter.ReBuildTimetree(var OtuNames: TStringList): Boolean;
var
  aData: TTimeTreeData;
  i: Integer; { current location in FNodes}
  Index: Integer; { current location in the TTimeTreeData arrays}
  n: Integer;

begin
  n := CountOtus;
  if n < 3 then
    raise Exception.Create('Pruned tree must have at least 3 taxa');
  Result := False;
  aData := nil;
  Index := 2*(n - 1); { start at last array element}
  try
    aData := TTimeTreeData.Create(n, True, False, False);
    for i := (NumNodes - 1) downto 0 do
    begin
      if not FNodes[i+1].flag then { node is pruned}
      begin
        if i < NumTaxa then { must remove it from OtuNames}
          OtuNames.Delete(i);
        continue;
      end;
      if not FNodes[i+1].OTU then { still traversing internal nodes in the tree}
      begin
        AData.NodeArray[Index - n].des1 := FNodes[i + 1].des1.index-1; { Index-n because NodeArray does not include leaf nodes}
        AData.NodeArray[Index - n].des2 := FNodes[i + 1].des2.index-1;
      end;
      AData.BLenArray[Index] := FNodes[i+1].branch.length; { all nodes have branch lengths}
      AData.SetTimetreeID(Index, FNodes[i+1].timetreeId); { all nodes have IDs}
      dec(Index);
    end;
    OtuNames.Clear;
    for i := 0 to n - 1 do
      OtuNames.Add(IntToStr(AData.GetTimetreeID(i)));
    FTimeTreeData.NoOfOTUs := n;
    FTimeTreeData.Assign(AData);
    Result := True;
  finally
    if Assigned(AData) then
      AData.Free;
  end;
end;

function TFpNodeTreeDataAdapter.RebuildTimetree2(var OtuNames: TStringList): Boolean;
var
  nOtu, nNodes, i: Integer;
  aNodes: array of TpNode;
  otuIndex: Integer = 0;
  internalIndex: Integer;
  aRoot: TpNode = nil;
  aData: TTimeTreeData = nil;
  dNode: TpNode;
  ttID: String;
  aString: String;

  procedure ProcessNode(aNode: TpNode);
  begin
    if Assigned(aNode.des1) then
      ProcessNode(aNode.des1);
    if Assigned(aNode.des2) then
      ProcessNode(aNode.des2);
    if aNode.flag then
    begin
      if aNode.Otu then
      begin
        aNodes[otuIndex] := aNode;
        inc(otuIndex);
      end
      else
      begin
        aNodes[internalIndex] := aNode;
        inc(internalIndex);
      end;
    end;
  end;

begin
  Result := False;
  nOtu := CountOtus;
  internalIndex := nOtu;
  nNodes := 2 * nOtu - 1;
  SetLength(aNodes, nNodes);
  for i := NumNodes downto 1 do
    if FNodes[i].flag and (FNodes[i].anc = nil) then
    begin
      aRoot := FNodes[i];
      break;
    end;
  Assert(Assigned(aRoot));
  ProcessNode(aRoot);
  Assert((otuIndex + internalIndex) = (nNodes + nOtu));
  for i := 0 to nNodes - 1 do
    TpNode(aNodes[i]).index := (i+1);

  FTimeTreeData.NoOfOTUs := nOtu;
  for i := 0 to nOtu - 2 do
  begin
    dNode := TpNode(aNodes[nOtu+i]);
    FTimeTreeData.NodeArray[i].des1 := aNodes[nOtu+i].des1.index-1;
    FTimeTreeData.NodeArray[i].des2 := aNodes[nOtu+i].des2.index-1;
  end;

  for i := 0 to nNodes-2 do
  begin
    FTimeTreeData.BLenArray[i] := aNodes[i].branch.length;
    FTimeTreeData.SetTimetreeID(i, aNodes[i].timetreeId);
    FTimeTreeData.SetChildNodeCount(i, aNodes[i].numLeaves);
  end;
  FTimeTreeData.SetTimetreeID(nNodes-1, aNodes[nNodes-1].timetreeId);
  FTimeTreeData.SetChildNodeCount(nNodes-1, aNodes[nNodes-1].numLeaves);

  OtuNames.Clear;
  for i := 0 to nOtu do
  begin
    ttId := IntToStr(aNodes[i].timetreeId);
    if NamesMap.Contains(ttId) then
    begin
      aString := TOtuName(NamesMap[ttId]).Name;
      OtuNames.Add(aString)
    end
    else
      OtuNames.Add('unnamed');
  end;
  Result := True;
end;

procedure TFpNodeTreeDataAdapter.RemoveSneakyTaxa(var OtuNames: TStringList);
var
  i: Integer;
  Id: Integer;
  aRank: TTaxonomicRank;
begin
  for i := (FTimeTreeData.NoOfOTUs - 1) downto 0 do
  begin
    Id := FTimeTreeData.GetTimetreeID(i);
    if (FRoot.des1.timetreeId = Id) or (FRoot.des2.timetreeId = Id) then
      continue;
    if Id > 0 then
    begin
      aRank := TaxonomicRanks[Id];
      if (aRank > FMostShallowRank) or (aRank = trNoRank) or (aRank = trUnknown) then
      begin
        FTimeTreeData.RemoveOTU(i);
        OtuNames.Delete(i);
      end;
    end;
  end;
end;

procedure TFpNodeTreeDataAdapter.CompressTree;
var
  i: Integer;
  index: Integer;
begin
  Index := 1;
  for i := 0 to NumNodes - 1 do
  begin
    if FNodes[i+1].flag then
    begin
      FNodes[i+1].index := Index;
      inc(Index);
    end;
  end;
end;

function TFpNodeTreeDataAdapter.MarkIncludedNodes: Boolean;

  procedure MarkNodesDown(aNode: TpNode);
  begin
    aNode.Flag := True;
    if Assigned(aNode.des1) then
      MarkNodesDown(aNode.des1);
    if Assigned(aNode.des2) then
      MarkNodesDown(aNode.des2);
  end;

  procedure ProcessNode(aNode: TpNode);
  begin
    if IsIncludedNode(aNode) then
      MarkNodesDown(aNode)
    else
    begin
      if Assigned(aNode.des1) then
        ProcessNode(aNode.des1);
      if Assigned(aNode.des2) then
        ProcessNode(aNode.des2)
    end;
  end;

begin
  ProcessNode(FRoot);
  Result := True;
end;

function TFpNodeTreeDataAdapter.MarkIncludedNodes(MostShallowRank: TTaxonomicRank): Boolean;
var
  NewRoot: TpNode;
  TreeCsv: TStringList;

  procedure UnmarkNodesDown(aNode: TpNode);
  begin
    aNode.flag := False;
    if aNode.OTU then
      aNode.OTU := False
    else
    begin
      if Assigned(aNode.des1) then
        UnmarkNodesDown(aNode.des1);
      if Assigned(aNode.des2) then
        UnmarkNodesDown(aNode.des2);
    end;
  end;

  procedure MarkNodesDown(aNode: TpNode);
  begin
    aNode.Flag := True;
    if Assigned(aNode.des1) then
      MarkNodesDown(aNode.des1);
    if Assigned(aNode.des2) then
      MarkNodesDown(aNode.des2);
  end;

  function ProcessNode(aNode: TpNode): TpNode;
  begin
    Result := nil;
    if IsIncludedNode(aNode) then
    begin
      MarkNodesDown(aNode);
      FindPutativeRankFromAncestors(aNode); { last chance to find putative rank since ancester is about to be nil}
      aNode.anc := nil;
      Result := aNode;
    end
    else
    begin
      if Assigned(aNode.des1) then
        Result := ProcessNode(aNode.des1);
      if (not Assigned(Result)) and Assigned(aNode.des2) then
        Result := ProcessNode(aNode.des2)
    end;
  end;

  procedure UnmarkExcludedNodes(aNode: TpNode);
  begin
    Assert(aNode.flag);
    if NodeIsNewLeafNode(aNode, MostShallowRank) then
    begin
      aNode.OTU := True;
      if Assigned(aNode.des1) then
      begin
        aNode.branch.length := aNode.branch.length + FindHeightOfTimetreeNode(aNode.des1);
        UnmarkNodesDown(aNode.des1);
      end;
      if Assigned(aNode.des2) then
        UnmarkNodesDown(aNode.des2);
      aNode.des1 := nil;
      aNode.des2 := nil;
    end
    else
    begin
      if Assigned(aNode.des1) then
        UnmarkExcludedNodes(aNode.des1);
      if Assigned(aNode.des2) then
        UnmarkExcludedNodes(aNode.des2);
    end;
  end;

begin
  NewRoot := ProcessNode(FRoot);
  {$IFDEF DEBUG}
  GetNewickString(NewRoot, 'mark_included');
  TreeCsv := WriteTreeAsCSV(NewRoot, 'unpruned');
  TreeCsv.Free;
  {$ENDIF}
  Assert(Assigned(NewRoot));
  FindPutativeRanks(NewRoot);
  {$IFDEF DEBUG}
  GetNewickString(NewRoot, 'putative_ranks');
  {$ENDIF}
  Assert((NewRoot.putativeRank <> trNoRank) and (NewRoot.putativeRank <> trUnknown));
  UnmarkExcludedNodes(NewRoot);
  {$IFDEF DEBUG}
  GetNewickString(NewRoot, 'unflag_nodes');
  TreeCsv := WriteTreeAsCSV(NewRoot, 'mark_included');
  TreeCsv.Free;
  {$ENDIF}
  Result := True;
end;

function TFpNodeTreeDataAdapter.IsIncludedNode(aNode: TpNode): Boolean;
var
  NodeId: Integer;
  Index: Integer;
begin
  NodeId := aNode.timetreeId;
  Index := FTTNodeIds.Find(NodeId);
  Result := (Index >= 0);
  if Result then
    FTTNodeIds.Delete(Index); { shrink the list for faster searching}
end;

function TFpNodeTreeDataAdapter.IsIncludedNode(aNode: TpNode; MostShallowRank: TTaxonomicRank): Boolean;
var
  NodeId: Integer;
  Index: Integer;
begin
  NodeId := aNode.timetreeId;
  Index := FTTNodeIds.Find(NodeId);
  Result := (Index >= 0);
  if Result then
  begin
    FTTNodeIds.Delete(Index); { shrink the list for faster searching}
    if aNode.rank > MostShallowRank then
      Result := False;
  end;
end;

function TFpNodeTreeDataAdapter.IsInternalNode(aNode: TpNode): Boolean;
begin
  if Assigned(aNode.des1) and Assigned(aNode.des2) then
    Result := True
  else
    Result := False;
end;

function TFpNodeTreeDataAdapter.HasDescendentAtMostShallowRank(aNode: TpNode): Boolean;

  function ProcessNode(target: TpNode): Boolean;
  begin
    Result := (target.rank = FMostShallowRank);
    if (not Result) and Assigned(target.des1) then
      Result := ProcessNode(target.des1);
    if (not Result) and Assigned(target.des2) then
      Result := ProcessNode(target.des2);
  end;

begin
  Result := False;
  if Assigned(aNode.des1) then
    Result := ProcessNode(aNode.des1);
  if (not Result) and Assigned(aNode.des2) then
    Result := ProcessNode(aNode.des2);
end;

function TFpNodeTreeDataAdapter.NodeIsNewLeafNode(aNode: TpNode; MostShallowRank: TTaxonomicRank): Boolean;
begin
  Assert((FMostShallowRank <> trNoRank) and (FMostShallowRank <> trUnknown));
  if IsInternalNode(aNode) then
  begin
    if RealRankAndChildRealRanksDefined(aNode) then { the easy case as real ranks will be all that is needed}
    begin
      if aNode.rank < MostShallowRank then
      begin
        if RealRankIsTooShallow(aNode.des1) and RealRankIsTooShallow(aNode.des2) then
          Result := True
        else
          Result := False; { the leaf/leaves for this lineage are somewhere deeper in the tree}
      end
      else
        Result := True;
    end
    else if RealRankIsDefined(aNode) then { either one or both child node ranks are not known}
    begin
      if RealRankIsDefined(aNode.des1) then { rank is known for target and des1 only}
      begin
        if RealRankIsTooShallow(aNode.des1) and (not HasDescendentDeeperThanRank(aNode.des2, FMostShallowRank)) then
          Result := True
        else
          Result := False;
      end
      else if RealRankIsDefined(aNode.des2) then { rank is known for target and des2 only}
      begin
        if RealRankIsTooShallow(aNode.des2) and (not HasDescendentDeeperThanRank(aNode.des1, FMostShallowRank)) then
          Result := True
        else
          Result := False;
      end
      else { rank is defined for target node but not for either child}
      begin
        if (not HasDescendentDeeperThanRank(aNode.des1, FMostShallowRank)) and (not HasDescendentDeeperThanRank(aNode.des2, FMostShallowRank)) then
          Result := True
        else
          Result := False;
      end;
    end
    else if PutativeRankIsTooShallow(aNode) then { putative rank comes from ancestors so no need to keep going shallower in the tree}
    begin
      Result := True;
    end
    else if RealRankIsDefined(aNode.des1) then { target node rank not defined but is for des1 and maybe des2}
    begin
      if not RealRankIsTooShallow(aNode.des1) then
        Result := False
      else
      begin
        if RealRankIsDefined(aNode.des2) then
        begin
          if not RealRankIsTooShallow(aNode.des2) then
            Result := False
          else
            Result := True;
        end
        else { rank for des1 defined but it is too shallow, look for a better node in subtree}
        begin
          if not HasDescendentDeeperThanRank(aNode.des2, FMostShallowRank) then
            Result := True
          else
            Result := False;
        end;
      end;
    end
    else if RealRankIsDefined(aNode.des2) then  { rank is only defined for des2}
    begin
      if RealRankIsTooShallow(aNode.des2) and (not HasDescendentDeeperThanRank(aNode.des1, FMostShallowRank)) then
        Result := True
      else
        Result := False;
    end
    else { no ranks are known for this node or its two children and only putative ranks can be used}
    begin
      if (not HasDescendentDeeperThanRank(aNode.des1, FMostShallowRank)) and (not HasDescendentDeeperThanRank(aNode.des2, FMostShallowRank)) then
        Result := True
      else
        Result := False;
    end;
  end
  else
    Result := True;
end;

function TFpNodeTreeDataAdapter.ChildrenShareParentsRank(aNode: TpNode): Boolean;
begin
  Assert(Assigned(aNode.des1) and Assigned(aNode.des2));
  Result := ((aNode.rank = aNode.des1.rank) and (aNode.rank = aNode.des2.rank));
end;

function TFpNodeTreeDataAdapter.ChildrenShareParentsPutativeRank(aNode: TpNode): Boolean;
begin
  Assert(Assigned(aNode.des1) and Assigned(aNode.des2));
  Result := ((aNode.putativeRank = aNode.des1.putativeRank) and (aNode.putativeRank = aNode.des2.putativeRank));
end;

function TFpNodeTreeDataAdapter.ChildrenPutativeRanksAreTooShallow(aNode: TpNode): Boolean;
begin
  Assert((FMostShallowRank <> trNoRank) and (FMostShallowRank <> trUnknown));
  Assert((aNode.des1.putativeRank <> trNoRank) and (aNode.des1.putativeRank <> trUnknown));
  Assert((aNode.des2.putativeRank <> trNoRank) and (aNode.des2.putativeRank <> trUnknown));

  if (aNode.des1.putativeRank > FMostShallowRank) and (aNode.des2.putativeRank > FMostShallowRank) then
    Result := True
  else
    Result := False;
end;

function TFpNodeTreeDataAdapter.PutativeRankIsTooShallow(aNode: TpNode): Boolean;
begin
  Assert((FMostShallowRank <> trNoRank) and (FMostShallowRank <> trUnknown));
  Assert((aNode.putativeRank <> trNoRank) and (aNode.putativeRank <> trUnknown));

  if aNode.putativeRank > FMostShallowRank then
    Result := True
  else
    Result := False;
end;

function TFpNodeTreeDataAdapter.RealRankIsTooShallow(aNode: TpNode): Boolean;
begin
  Assert((FMostShallowRank <> trNoRank) and (FMostShallowRank <> trUnknown));

  if aNode.rank > FMostShallowRank then
    Result := True
  else
    Result := False;
end;

function TFpNodeTreeDataAdapter.RealRankIsDefined(aNode: TpNode): Boolean;
begin
  if (aNode.Rank <> trNoRank) and (aNode.Rank <> trUnknown) then
    Result := True
  else
    Result := False;
end;

function TFpNodeTreeDataAdapter.RealRankAndChildRealRanksDefined(aNode: TpNode): Boolean;
begin
  Result := RealRankIsDefined(aNode);
  if Assigned(aNode.des1)  then
    Result := Result and RealRankIsDefined(aNode.des1);
  if Assigned(aNode.des2) then
    Result := Result and RealRankIsDefined(aNode.des2);
end;

function TFpNodeTreeDataAdapter.AllIdsPresentInTree(NodeIds: TLongIntList): Boolean;
var
  i: Integer;
begin
  Result := False;
  if NodeIds.Count > 0 then
    for i := 0 to NodeIds.Count - 1 do
    begin
      if not FTimeTreeData.HasTimetreeID(NodeIds[i]) then
      begin
        Exit;
      end;
    end;
  Result := True;
end;

procedure TFpNodeTreeDataAdapter.FindPutativeRanks(aNode: TpNode);
begin
  Assert((aNode.putativeRank <> trNoRank) and (aNode.putativeRank <> trUnknown));
  if RealRankIsDefined(aNode) then
    aNode.putativeRank := aNode.rank;

  if Assigned(aNode.des1) then
  begin
    aNode.des1.putativeRank := aNode.putativeRank;
    FindPutativeRanks(aNode.des1);
  end;
  if Assigned(aNode.des2) then
  begin
    aNode.des2.putativeRank := aNode.putativeRank;
    FindPutativeRanks(aNode.des2);
  end;
end;

procedure TFpNodeTreeDataAdapter.FindPutativeRanks(aNode: TpNode; StartRank: TTaxonomicRank);
begin
  aNode.putativeRank := StartRank;
  FindPutativeRanks(aNode);
end;

procedure TFpNodeTreeDataAdapter.FindPutativeRankFromAncestors(aNode: TpNode);
var
  tempNode: TpNode;
begin
  if (aNode.Rank = trNoRank) or (aNode.Rank = trUnknown) then
  begin
    tempNode := aNode;
    while Assigned(tempNode) do
    begin
      tempNode := tempNode.anc;
      if RealRankIsDefined(tempNode) then
      begin
        aNode.putativeRank := tempNode.rank;
        break;
      end;
    end;
  end
  else
    aNode.putativeRank := aNode.rank;
  //Assert(RealRankIsDefined(aNode));
end;

function TFpNodeTreeDataAdapter.FindHeightOfTimetreeNode(aNode: TpNode): double;
var
  Child: TpNode;
begin
  Result := aNode.branch.length;
  if Assigned(aNode.des1) then
  begin
    Child := aNode.des1;
    while Assigned(Child) do
    begin
      Result := Result + Child.branch.length;
      Child := Child.des1;
    end;
  end;
end;

procedure TFpNodeTreeDataAdapter.InitTpNodes;
var
  i: Integer;
begin
  Assert((NumNodes > 0) and (NumTaxa > 0));
  GetMem(FNodes,SizeOf(TpNode)*NumNodes);
  for i := 1 to NumTaxa do
  begin
      New(FNodes[i]);
      FNodes[i].index:= i;
      FNodes[i].minOTU := i;
      FNodes[i].size := 1;
      FNodes[i].depth := 0;
      FNodes[i].height := 0.0;
      FNodes[i].h0 := 0;
      FNodes[i].maxh := 0;
      FNodes[i].minh := 0;
      FNodes[i].rate := 0;
      FNodes[i].width := 0;
      FNodes[i].anc := nil;
      FNodes[i].des1 := nil;
      FNodes[i].des2 := nil;
      FNodes[i].OTU := true;
      FNodes[i].compressed := false;
      FNodes[i].outgroup := false;
      FNodes[i].hidden := false;
      FNodes[i].flag := false;
      FNodes[i].hilighted := false;
      FNodes[i].emphasized := false;
      FNodes[i].angle := 0.0;
      FNodes[i].branch.length := 0.0;
      FNodes[i].branch.SE := 0.0;
      FNodes[i].branch.maxlen1 := 0.0;
      FNodes[i].branch.maxlen2 := 0.0;
      FNodes[i].branch.bootnum := 0;
      FNodes[i].branch.stats := 0.0;
      FNodes[i].branch.stat2 := 0;
      FNodes[i].attrindex := 0;
      FNodes[i].namesize.x := 0;
      FNodes[i].namesize.y := 0;
      FNodes[i].groupindex := -1;
      FNodes[i].capdepth   := 0;
      FNodes[i].bracket.Left   := 0;
      FNodes[i].bracket.Right  := 0;
      FNodes[i].bracket.Top    := 0;
      FNodes[i].bracket.Bottom := 0;
      FNodes[i].IsGeneDuplication := False;
      FNodes[i].IsSpeciationEvent := False;
      FNodes[i].DataCoverage := 0.0;
      FNodes[i].rank := trUnknown;
      FNodes[i].putativeRank := trUnknown;
      FNodes[i].numLeaves := 0;
  end;
  for i := NumTaxa + 1 to NumNodes do
  begin
      NEW(FNodes[i]);
      FNodes[i].index:= i;
      FNodes[i].size := 0;
      FNodes[i].depth := 0;
      FNodes[i].height := 0.0;
      FNodes[i].h0 := 0;
      FNodes[i].maxh := 0;
      FNodes[i].minh := 0;
      FNodes[i].rate := 0;
      FNodes[i].width := 0;
      FNodes[i].anc := nil;
      FNodes[i].des1 := nil;
      FNodes[i].des2 := nil;
      FNodes[i].name := '';
      FNodes[i].SpeciesName := EmptyStr;
      FNodes[i].PrivateName := '';
      FNodes[i].OTU := false;
      FNodes[i].compressed := false;
      FNodes[i].outgroup := false;
      FNodes[i].hidden := false;
      FNodes[i].flag := false;
      FNodes[i].hilighted := false;
      FNodes[i].emphasized := false;
      FNodes[i].angle := 0.0;
      FNodes[i].branch.length := 0.0;
      FNodes[i].branch.SE := 0.0;
      FNodes[i].branch.maxlen1 := 0.0;
      FNodes[i].branch.maxlen2 := 0.0;
      FNodes[i].branch.bootnum := 0;
      FNodes[i].branch.stats := 0.0;
      FNodes[i].branch.stat2 := 0;
      FNodes[i].attrindex := 0;
      FNodes[i].namesize.x := 0;
      FNodes[i].namesize.y := 0;
      FNodes[i].groupindex := -1;
      FNodes[i].capdepth   := 0;
      FNodes[i].bracket.Left   := 0;
      FNodes[i].bracket.Right  := 0;
      FNodes[i].bracket.Top    := 0;
      FNodes[i].bracket.Bottom := 0;
      FNodes[i].IsGeneDuplication := False;
      FNodes[i].IsSpeciationEvent := False;
      FNodes[i].DataCoverage := 0.0;
      FNodes[i].rank := trUnknown;
      FNodes[i].putativeRank := trUnknown;
      FNodes[i].numLeaves := 0;
  end;
  FRoot := nil;
end;

procedure TFpNodeTreeDataAdapter.InitRankCounts;
var
  i: Integer;
begin
  for i := 1 to NumNodes do
  begin
    GetMem(FNodes[i].ranksCount, SizeOf(TRanksCount));
    FNodes[i].ranksCount.noRank := 0;
    FNodes[i].ranksCount.unknown := 0;
    FNodes[i].ranksCount.domain := 0;
    FNodes[i].ranksCount.superKingdom := 0;
    FNodes[i].ranksCount.kingdom := 0;
    FNodes[i].ranksCount.subKingdom := 0;
    FNodes[i].ranksCount.superPhylum := 0;
    FNodes[i].ranksCount.phylum := 0;
    FNodes[i].ranksCount.subPhylum := 0;
    FNodes[i].ranksCount.superClass := 0;
    FNodes[i].ranksCount.fClass := 0;
    FNodes[i].ranksCount.subClass := 0;
    FNodes[i].ranksCount.infraClass := 0;
    FNodes[i].ranksCount.superOrder := 0;
    FNodes[i].ranksCount.order := 0;
    FNodes[i].ranksCount.parvOrder := 0;
    FNodes[i].ranksCount.subOrder := 0;
    FNodes[i].ranksCount.infraOrder := 0;
    FNodes[i].ranksCount.superFamily := 0;
    FNodes[i].ranksCount.family := 0;
    FNodes[i].ranksCount.subFamily := 0;
    FNodes[i].ranksCount.tribe := 0;
    FNodes[i].ranksCount.subTribe := 0;
    FNodes[i].ranksCount.genus := 0;
    FNodes[i].ranksCount.subGenus := 0;
    FNodes[i].ranksCount.speciesGroup := 0;
    FNodes[i].ranksCount.species := 0;
    FNodes[i].ranksCount.speciesSubGroup := 0;
    FNodes[i].ranksCount.subSpecies := 0;
    FNodes[i].ranksCount.varietas := 0;
  end;
end;

procedure TFpNodeTreeDataAdapter.FreeRankCounts;
var
  i: Integer;
begin
  for i := 1 to NumNodes do
    FreeMem(FNodes[i].ranksCount);
end;

procedure TFpNodeTreeDataAdapter.SetNumTaxa(AValue: Integer);
begin
  if FNumTaxa=AValue then
    Exit;
  FNumTaxa := AValue;
  FNumNodes := 2 * FNumTaxa - 1;
end;

procedure TFpNodeTreeDataAdapter.TurnNode(p: TpNode);
begin
  if not p.OTU then
  begin
    SwapNode(p.des1,p.des2);
    TurnNode(p.des1);
    TurnNode(p.des2);
  end;
end;

constructor TFpNodeTreeDataAdapter.Create;
begin
  FMostShallowRank := trUnknown;
  FIsBranchLength := False;
  FIsSE := False;
  FIsStats := False;
  FTTNodeIds:= TLongIntList.Create;
end;

destructor TFpNodeTreeDataAdapter.Destroy;
var
  i: Integer;
begin
  if NumTaxa = 0 then
    Exit;
  FTimetreeData := nil; { freed elsewhere}
  if FNodes <> nil then
    for i := 1 to NumNodes do
      Dispose(FNodes[i]);
  FreeMemAndNil(FNodes,SizeOf(TpNode)*NumNodes);
  FNodes := nil;
  inherited Destroy;
end;

function TFpNodeTreeDataAdapter.TreeToTabularFormat(const Tree: TTimeTreeData; const otuNames: TStringList; const filename: String): Boolean;
var
  Output: TextFile;
  CurStr: String;

  procedure WriteInfo(AIndex: Integer);
  begin
    if FNodes[AIndex].des1.Index > NumTaxa then
      WriteInfo(FNodes[aIndex].des1.Index);

    if FNodes[aIndex].des2.index > NumTaxa then
      WriteInfo(FNodes[aIndex].des2.index);

    CurStr := IntToStr(aIndex)  + ', ';
    if FNodes[aIndex].des1.index <= NumTaxa then
      CurStr := CurStr + otuNames[FNodes[aIndex].des1.index - 1]
    else
      CurStr := CurStr + Format('%5d', [FNodes[aIndex].des1.index]);
    CurStr := CurStr + ', ';

    if FNodes[aIndex].des2.index <= NumTaxa then
      CurStr := CurStr + otuNames[FNodes[aIndex].des2.index - 1]
    else
      CurStr := CurStr + Format('%5d', [FNodes[aIndex].des2.index]);
    CurStr := CurStr + ', ';

    CurStr := CurStr + FloatToStrF(FNodes[aIndex].des1.branch.length, ffFixed,6,10) +', ';
    CurStr := CurStr + FloatToStrF(FNodes[aIndex].des2.branch.length, ffFixed,6,10);
    WriteLn(Output, CurStr);
  end;
begin
  try
    SetTreeData(Tree, True);
    AssignFile(Output, filename);
    Rewrite(Output);
    WriteLn(Output, 'AncId, Desc1, Desc2, Branch Length 1, Branch Length 2');
    WriteInfo(NumNodes); // root node
  finally
    CloseFile(Output);
  end;
  Result := FileExists(filename);
end;

procedure TFpNodeTreeDataAdapter.SetTreeData(const AData: TTimeTreeData; isConvertToTabular: Boolean);
var
  i,j,k : integer;
begin
  NumTaxa := AData.NoOfOtus;
  InitTpNodes;
  FTimeTreeData := AData;
  for i := 1 to NumNodes do
  begin
      FNodes[i].anc := nil;
      FNodes[i].compressed := false;
      FNodes[i].hidden := false;
      FNodes[i].flag := false;
      FNodes[i].height := 0.0;
  end;

  for i := 1 to NumTaxa do { mark the taxa that belong to the outgroup}
  begin
    FNodes[i].outgroup := AData.IsOutgroupMember[i - 1];
    FNodes[i].OTU := True;
  end;

  for i := 1 to NumTaxa - 1 do
  begin
      j := AData.NodeArray[i-1].des1+1;
      k := AData.NodeArray[i-1].des2+1;
      FNodes[NumTaxa+i].des1 := FNodes[j];
      FNodes[NumTaxa+i].des2 := FNodes[k];
      FNodes[j].anc := FNodes[NumTaxa + i];
      FNodes[k].anc := FNodes[NumTaxa + i];
      FNodes[NumTaxa+i].name := EmptyStr;
      FNodes[NumTaxa+i].charstate := EmptyStr;
  end;

  for i := NumNodes downto NumTaxa + 1 do
    if FNodes[i].anc = nil then
    begin
        FRoot := FNodes[i];
        Break;
    end;
  for i  := NumNodes downto NumTaxa + 1 do
    FNodes[i].dataCoverage := AData.DataCoverage[i - NumTaxa - 1];
  if AData.isBLen then
  begin
    FIsBranchLength := True;
    for i := 1 to NumNodes do
    begin
      if not IsConvertToTabular then
      begin
        FNodes[i].numLeaves := 0;
        FNodes[i].timetreeId := AData.GetTimetreeID(i-1);
        if FNodes[i] = FRoot then
          Continue;
        if FNodes[i].timetreeId >= 0 then
          FNodes[i].rank := TaxonomicRanks[FNodes[i].timetreeId];
      end;
      FNodes[i].branch.length := AData.BLen[i-1];
    end;
  end
  else
    FIsBranchLength := False;

  if AData.isSE then
  begin
    FIsSE := True;
    for i := 1 to NumNodes do
    begin
        if FNodes[i] = FRoot then
          Continue;
        FNodes[i].branch.SE := AData.SE[i-1];
    end;
  end
  else
    FIsSE := False;

  if AData.isStats then
  begin
    FIsStats := True;
    for i := 1 to NumNodes do
    begin
      if FNodes[i] = FRoot then
        Continue;
      FNodes[i].branch.stats := AData.Stats[i-1];
    end;
  end;
end;

procedure TFpNodeTreeDataAdapter.GetTreeData(var AData: TTimeTreeData);
var
  i: integer;
begin
  AData.NoOfOTUs := NumTaxa;
  AData.isBLen := FIsBranchLength;
  AData.isSE := FIsSE;
  AData.isStats := FIsStats;
  for i := 0 to NumTaxa - 2 do begin
    AData.NodeArray[i].des1 := FNodes[NumTaxa+i+1].des1.index-1;
    AData.NodeArray[i].des2 := FNodes[NumTaxa+i+1].des2.index-1;
  end;
  for i := 1 to NumTaxa do
    AData.IsOutgroupMember[i-1] := FNodes[i].outgroup;

  for i := 0 to NumNodes-2 do
  begin
    if AData.isBLen then
      AData.BLenArray[i] := FNodes[i+1].branch.length;
    if AData.isSE then
      AData.SE[i] := FNodes[i+1].branch.SE;
    if AData.isStats then
      AData.Stats[i] := FNodes[i+1].branch.stats;
  end;

  for i := 1 to NumNodes do
    AData.SetChildNodeCount(i-1, FNodes[i].numLeaves);
end;

procedure TFpNodeTreeDataAdapter.RootOnOutgroup;
var
  NewRoot : TpNode;
  i : integer;
begin
  SetClusterSize(FRoot);
  for i := 1 to NumTaxa do
    FNodes[i].flag := FNodes[i].outgroup;
  NewRoot := SearchCommonAncestor(FRoot);
  if NewRoot = FRoot then
    Exit;
  ChangeRoot(FRoot, NewRoot, true);
  SetClusterSize(FRoot);
  if FRoot.des1.flag then
      TurnNode(FRoot);
end;

function TFpNodeTreeDataAdapter.GetPercentMissingBlensData: Double;
var
  i: Integer;
  numMissing, numTotal: Integer;
begin
  Result := 1.0;
  if FNumNodes = 0 then
    Exit;
  numMissing := 0;
  numTotal := 0;
  for i := 1 to FNumNodes do
  begin
    if CompareValue(FNodes[i].branch.length, 0, FP_CUTOFF) < 0 then
      numMissing := numMissing + 1;
    numTotal := numTotal + 1
  end;
  if numTotal > 0 then
    Result := numMissing/numTotal
  else if numMissing > 0 then
    Result := 1.0
  else
    Result := 0.0;
end;

function TFpNodeTreeDataAdapter.GetNodeHeights: TDoubleArray;
var
  i: Integer;
begin
  if FNumTaxa > 2 then
    IterateNodeHeights;
  SetLength(Result, FNumNodes);
  for i := 1 to FNumNodes do
    Result[i - 1] := FNodes[i].height;
end;

function TFpNodeTreeDataAdapter.GetTreeBoxTreeReference: TpNodeArray;
begin
  Result := FNodes^;
end;

function TFpNodeTreeDataAdapter.PruneTree(const NodeIds: TLongIntList; OtuNames: TStringList; DoSlowVersion: Boolean=False): Boolean;
begin
  FOtuNames := OtuNames;
  Assert(Assigned(FRoot));
  FTTNodeIds.Assign(NodeIds);
  if DoSlowVersion then
    Result := DoPruneTreeSlow
  else
    Result := DoPruneTreeFast(OtuNames);
  Assert(AllIdsPresentInTree(NodeIds));
end;

function TFpNodeTreeDataAdapter.PruneTree(const NodeIds: TLongIntList;OtuNames: TStringList; MostShallowRank: TTaxonomicRank;DoSlowVersion: Boolean): Boolean;

begin
  if NodeIds.Count <> 1 then
    raise Exception.Create('restricted taxonomic rank search only allows 1 id to be used');
  FOtuNames := OtuNames;
  Assert(Assigned(FRoot));
  FTTNodeIds.Assign(NodeIds);
  if DoSlowVersion then
    Result := DoPruneTreeSlow
  else
    Result := DoPruneTreeFast(OtuNames, MostShallowRank);
  //Assert(AllIdsPresentInTree(NodeIds));
end;

function TFpNodeTreeDataAdapter.CountRanks(var aList: TStringList): Boolean;
var
  i: Integer;
  aNode: TpNode;
  aRank: TTaxonomicRank;
  aCount: Integer;
  csvString: String;
begin
  Result := False;
  InitRankCounts;
  FindPutativeRanks(FRoot, trDomain);
  for i := NumNodes downto 1 do
  begin
    aNode := FNodes[i];
    WriteLn(Format('Counting ranks: %d / %d', [i, NumNodes]));
    for aRank := trDomain to trVarietas do
    begin
      if aRank < aNode.putativeRank then
        continue;

      FMostShallowRank := aRank;
      //aCount := CountNodesWithShallowestRank(aNode);
      aCount := CountLeafNodesAtShallowestRank(aNode);
      case aRank of
      trNoRank: aNode.ranksCount.noRank := aCount;
      trUnknown: aNode.ranksCount.unknown := aCount;
      trDomain: aNode.ranksCount.domain := aCount;
      trSuperKingdom: aNode.ranksCount.superKingdom:=aCount;
      trKingdom: aNode.ranksCount.kingdom:=aCount;
      trSubKingdom: aNode.ranksCount.subKingdom:=aCount;
      trSuperPhylum: aNode.ranksCount.superPhylum:=aCount;
      trPhylum: aNode.ranksCount.phylum:=aCount;
      trSubPhylum: aNode.ranksCount.subPhylum:=aCount;
      trSuperClass: aNode.ranksCount.superClass:=aCount;
      trClass: aNode.ranksCount.fClass:=aCount;
      trSubClass: aNode.ranksCount.subClass:=aCount;
      trInfraClass: aNode.ranksCount.infraClass:=aCount;
      trSuperOrder: aNode.ranksCount.superOrder:=aCount;
      trOrder: aNode.ranksCount.order:=aCount;
      trParvOrder: aNode.ranksCount.parvOrder:=aCount;
      trSubOrder: aNode.ranksCount.subOrder:=aCount;
      trInfraOrder: aNode.ranksCount.infraOrder:=aCount;
      trSuperFamily: aNode.ranksCount.superFamily:=aCount;
      trFamily: aNode.ranksCount.family:=aCount;
      trSubFamily: aNode.ranksCount.subFamily:=aCount;
      trTribe: aNode.ranksCount.tribe:=aCount;
      trSubTribe: aNode.ranksCount.subTribe:=aCount;
      trGenus: aNode.ranksCount.genus:=aCount;
      trSubGenus: aNode.ranksCount.subGenus:=aCount;
      trSpeciesGroup: aNode.ranksCount.speciesGroup:=aCount;
      trSpecies: aNode.ranksCount.species:=aCount;
      trSpeciesSubGroup: aNode.ranksCount.speciesSubGroup:=aCount;
      trSubSpecies: aNode.ranksCount.subSpecies:=aCount;
      trVarietas: aNode.ranksCount.varietas:=aCount;
      end;
    end;
  end;

  for i := (NumTaxa + 1) to NumNodes do
    FNodes[i].ranksCount.numChildren := CountChildNodes(FNodes[i]);
  csvString := RankCountsCsvHeaderStr;
  aList.Add(csvString);
  for i := (NumTaxa + 1) to NumNodes do
  begin
    if FNodes[i].TimetreeId >= 0 then
    begin
      csvString := RankCountsToCsvStr(FNodes[i]);
      aList.Add(csvString);
    end;
  end;
  FreeRankCounts;
  Result := (aList.Count > 0);
end;

function TFpNodeTreeDataAdapter.RankCountsCsvHeaderStr: String;
begin
  Result := 'id,name,my_rank,child_nodes,no_rank,unknown,domain,superkingdom,kingdom,subkingdom,superphylum,';
  Result := Result + 'phylum,subphylum,superclass,class,subclass,infraclass,superorder,';
  Result := Result + 'order,parvorder,suborder,infraorder,superfamily,family,subfamily,';
  Result := Result + 'tribe,subtribe,genus,subgenus,species_group,species,species_subgroup,subspecies,varietas';
end;

function TFpNodeTreeDataAdapter.RankCountsToCsvStr(aNode: TpNode): String;
var
  NodeName: String;
  NodeIdStr: String;
begin
  NodeIdStr := IntToStr(aNode.TimetreeId);
  if NamesMap.Contains(NodeIdStr) then
  begin
    NodeName := TOtuName(NamesMap[NodeIdStr]).Name;
  end
  else
    NodeName := 'nameless';
  Result := NodeIdStr;
  Result := Result + ',' + NodeName;
  Result := Result + ',' + TaxonomicRankToString(aNode.rank);
  Result := Result + ',' + IntToStr(aNode.ranksCount.numChildren);
  Result := Result + ',' + IntToStr(aNode.ranksCount.noRank);
  Result := Result + ',' + IntToStr(aNode.ranksCount.unknown);
  Result := Result + ',' + IntToStr(aNode.ranksCount.domain);
  Result := Result + ',' + IntToStr(aNode.ranksCount.superKingdom);
  Result := Result + ',' + IntToStr(aNode.ranksCount.kingdom);
  Result := Result + ',' + IntToStr(aNode.ranksCount.subKingdom);
  Result := Result + ',' + IntToStr(aNode.ranksCount.superPhylum);
  Result := Result + ',' + IntToStr(aNode.ranksCount.phylum);
  Result := Result + ',' + IntToStr(aNode.ranksCount.subPhylum);
  Result := Result + ',' + IntToStr(aNode.ranksCount.superClass);
  Result := Result + ',' + IntToStr(aNode.ranksCount.fClass);
  Result := Result + ',' + IntToStr(aNode.ranksCount.subClass);
  Result := Result + ',' + IntToStr(aNode.ranksCount.infraClass);
  Result := Result + ',' + IntToStr(aNode.ranksCount.superOrder);
  Result := Result + ',' + IntToStr(aNode.ranksCount.order);
  Result := Result + ',' + IntToStr(aNode.ranksCount.parvOrder);
  Result := Result + ',' + IntToStr(aNode.ranksCount.subOrder);
  Result := Result + ',' + IntToStr(aNode.ranksCount.infraOrder);
  Result := Result + ',' + IntToStr(aNode.ranksCount.superFamily);
  Result := Result + ',' + IntToStr(aNode.ranksCount.family);
  Result := Result + ',' + IntToStr(aNode.ranksCount.subFamily);
  Result := Result + ',' + IntToStr(aNode.ranksCount.tribe);
  Result := Result + ',' + IntToStr(aNode.ranksCount.subTribe);
  Result := Result + ',' + IntToStr(aNode.ranksCount.genus);
  Result := Result + ',' + IntToStr(aNode.ranksCount.subGenus);
  Result := Result + ',' + IntToStr(aNode.ranksCount.speciesGroup);
  Result := Result + ',' + IntToStr(aNode.ranksCount.species);
  Result := Result + ',' + IntToStr(aNode.ranksCount.speciesSubGroup);
  Result := Result + ',' + IntToStr(aNode.ranksCount.subSpecies);
  Result := Result + ',' + IntToStr(aNode.ranksCount.varietas);
end;

procedure TFpNodeTreeDataAdapter.AddRankCounts(var Destination: TRanksCount; const Source: TRanksCount);
begin
  Destination.noRank := Destination.noRank + Source.noRank;
  Destination.unknown := Destination.unknown + Source.unknown;
  Destination.domain := Destination.domain + Source.domain;
  Destination.superKingdom := Destination.superKingdom + Source.superKingdom;
  Destination.kingdom := Destination.kingdom + Source.kingdom;
  Destination.subKingdom := Destination.subKingdom + Source.subKingdom;
  Destination.superPhylum := Destination.superPhylum + Source.superPhylum;
  Destination.phylum := Destination.phylum + Source.phylum;
  Destination.subPhylum := Destination.subPhylum + Source.subPhylum;
  Destination.superClass := Destination.superClass + Source.superClass;
  Destination.fClass := Destination.fClass + Source.fClass;
  Destination.subClass := Destination.subClass + Source.subClass;
  Destination.infraClass := Destination.infraClass + Source.infraClass;
  Destination.superOrder := Destination.superOrder + Source.superOrder;
  Destination.order := Destination.order + Source.order;
  Destination.parvOrder := Destination.parvOrder + Source.parvOrder;
  Destination.subOrder := Destination.subOrder + Source.subOrder;
  Destination.infraOrder := Destination.infraOrder + Source.infraOrder;
  Destination.superFamily := Destination.superFamily + Source.superFamily;
  Destination.family := Destination.family + Source.family;
  Destination.subFamily := Destination.subFamily + Source.subFamily;
  Destination.tribe := Destination.tribe + Source.tribe;
  Destination.subTribe := Destination.subTribe + Source.subTribe;
  Destination.genus := Destination.genus + Source.genus;
  Destination.subGenus := Destination.subGenus + Source.subGenus;
  Destination.speciesGroup := Destination.speciesGroup + Source.speciesGroup;
  Destination.species := Destination.species + Source.species;
  Destination.speciesSubGroup := Destination.speciesSubGroup + Source.speciesSubGroup;
  Destination.subSpecies := Destination.subSpecies + Source.subSpecies;
  Destination.varietas := Destination.varietas+ Source.varietas;
end;

function TFpNodeTreeDataAdapter.GetMrcaMatrix: T2DArrayOfInteger;
var
  i, j: Integer;

  procedure FillMrcaMatrix(aNode: TpNode);
  var
    desList: TIntArray;
    desList2: TIntArray;
    index, index2: Integer;
  begin
    if Assigned(aNode.Des1) then
      FillMrcaMatrix(aNode.Des1);
    if Assigned(aNode.Des2) then
      FillMrcaMatrix(aNode.Des2);
    if not aNode.Otu then
    begin
      if BothDescendentsAreOtus(aNode) then
      begin
        Result[aNode.Des1.index - 1][aNode.Des2.index - 1] := aNode.index;
        Result[aNode.Des2.index - 1][aNode.Des1.index - 1] := aNode.index;
      end
      else if Descendent1IsOtu(aNode) then
      begin
        desList := GetLeafNodeIds(aNode.Des2);
        if Length(desList) > 0 then
          for index := 0 to Length(desList) - 1 do
          begin
            Result[aNode.Des1.index - 1][desList[index] - 1] := aNode.index;
            Result[desList[index] - 1][aNode.Des1.index - 1] := aNode.index;
          end;
      end
      else if Descendent2IsOtu(aNode) then
      begin
        desList := GetLeafNodeIds(aNode.Des1);
        if Length(desList) > 0 then
          for index := 0 to Length(desList) - 1 do
          begin
            Result[aNode.Des2.index - 1][desList[index] - 1] := aNode.index;
            Result[desList[index] - 1][aNode.Des2.index - 1] := aNode.index;
          end;
      end
      else
      begin
        desList := GetLeafNodeIds(aNode.Des1);
        desList2 := GetLeafNodeIds(aNode.Des2);
        if (Length(desList) > 0) and (Length(desList2) > 0) then
        begin
          for index := 0 to Length(desList) - 1 do
            for index2 := 0 to Length(desList2) - 1 do
            begin
              Result[desList[index] - 1][desList2[index2] - 1] := aNode.index;
              Result[desList2[index2] - 1][desList[index] - 1] := aNode.index;
            end;
        end;
      end;
    end;
  end;

begin
  SetLength(Result, NumTaxa);
  for i := 0 to Length(Result) - 1 do
  begin
    SetLength(Result[i], NumTaxa);
    for j := 0 to Length(Result[i]) - 1 do
      Result[i][j] := -1;
  end;
  FillMrcaMatrix(FRoot);
end;

function TFpNodeTreeDataAdapter.GetPairwiseDistances: T2dArrayOfExtended;
var
  mrcaMatrix: T2DArrayOfInteger;
  i, j: Integer;
begin
  SetLength(Result, NumTaxa);
  for i := 0 to Length(Result) - 1 do
  begin
    SetLength(Result[i], NumTaxa);
    for j := 0 to Length(Result[i]) - 1 do
      Result[i][j] := 0;
  end;
  mrcaMatrix := GetMrcaMatrix;
  for i := 0 to Length(mrcaMatrix) - 1 do
    for j := 0 to Length(mrcaMatrix[i]) - 1 do
      if i <> j then
        Result[i][j] := PairwiseDist(mrcaMatrix[i][j], i + 1, j + 1);
end;

//function TFpNodeTreeDataAdapter.GetPairwiseDistances(aData: TTimeTreeData;
//  IsRooted: Boolean; IsLinearized: Boolean): PDistanceMatrix;
//begin
//
//end;

end.

