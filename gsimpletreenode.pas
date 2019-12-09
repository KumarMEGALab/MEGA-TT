unit gsimpletreenode;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface


uses
  Classes, SysUtils;

type


  { TSimpleTreeNode }

  TSimpleTreeNode = class(TObject)
    private
      FSpeciesName: AnsiString;
      function GetHasPatristicDists: Boolean;
      procedure SetHasPatristicDists(AValue: Boolean);
      procedure SetSpeciesName(const Value: AnsiString);
    public
      Depth: Integer;
      Ancestor: TSimpleTreeNode;
      Des1: TSimpleTreeNode;
      Des2: TSimpleTreeNode;
      NodeIndex: Integer;
      TempIndex: Integer; // needed for gene duplication inference, setup using a pre-order traversal
      IsDuplicationEvent: Boolean;
      IsSpeciationEvent: Boolean;
      IsOtu: Boolean;
      IsOutgroupMember: Boolean;
      IsRoot: Boolean;
      SequenceName: AnsiString;
      SequenceData: AnsiString;
      ExtantSpecies: TStringList; // the names (sorted) of all species at the leaves of this sub-tree
      AncSpeciesIndex: Integer;

      Id: AnsiString;
      Accession: AnsiString;
      ScientificName: AnsiString;
      CommonName: AnsiString;
      Code: AnsiString;
      Annotation: AnsiString;
      Symbol: AnsiString;
      DataCoverage: Double;
      StdError: Double;
      BLen: Double;
      Value: Double; {can be used for bootstrap value, divergence time, etc...}
      PatristicDists: TPatristicDistances;
      constructor Create;
      destructor Destroy; override;
      function StringDescription: AnsiString;
      function GetSpeciesName: AnsiString;
      property SpeciesName: AnsiString read FSpeciesName write SetSpeciesName; // SetSpeciesName forces lower case so we can search case-sensitive
      property HasPatristicDists: Boolean read GetHasPatristicDists write SetHasPatristicDists;

  end;

  TSimpleTreeNodeArray = array of TSimpleTreeNode;
  TSimpleTreeNodeArrayArray = array of TSimpleTreeNodeArray;

  function CompareSimpleTreeNodes(Item1: Pointer; Item2: Pointer): Integer;
  function TreeAsTable(RootNode: TSimpleTreeNode): TStringList; overload;
  function TreeAsTable(ATree: TSimpleTreeNodeArray): TStringList; overload;
  procedure CloneTreeNodeArray(Source: TSimpleTreeNodeArray; var Destination: TSimpleTreeNodeArray);


implementation


uses
  Math;

{ TSimpleTreeNode }



constructor TSimpleTreeNode.Create;
begin
  PatristicDists := nil;
  Value := 0.0;
  BLen := 0.0;
  StdError := 0.0;
  DataCoverage := 0.0;
  IsOutgroupMember := False;
  Depth := -1;
  Ancestor := nil;
  Des1 := nil;
  Des2 := nil;
  NodeIndex := -1;
  IsDuplicationEvent := False;
  IsSpeciationEvent := False;
  IsOtu := False;
  FSpeciesName := EmptyStr;
  AncSpeciesIndex := -1;
  ExtantSpecies := TStringList.Create;
  ExtantSpecies.Sorted := True;
  ExtantSpecies.CaseSensitive := True;
  IsRoot := False;
  TempIndex := -1;
  SequenceName := EmptyStr;
  Id := EmptyStr;
  Accession := EmptyStr;
  ScientificName := EmptyStr;
  CommonName := EmptyStr;
  Code := EmptyStr;
  Annotation := EmptyStr;
  Symbol := EmptyStr;
  SequenceData := EmptyStr;
end;

destructor TSimpleTreeNode.Destroy;
begin
  if Assigned(ExtantSpecies) then
    ExtantSpecies.Free;
  if Assigned(PatristicDists) then
    PatristicDists.Free;
  inherited;
end;

function TSimpleTreeNode.GetSpeciesName: AnsiString;
begin
  Result := EmptyStr;
  if ScientificName <> EmptyStr then
    Result := ScientificName
  else if SpeciesName <> EmptyStr then
    Result := SpeciesName
  else if CommonName <> EmptyStr then
    Result := CommonName;
end;

procedure TSimpleTreeNode.SetSpeciesName(const Value: AnsiString);
begin
  FSpeciesName := LowerCase(Value);
end;

procedure TSimpleTreeNode.SetHasPatristicDists(AValue: Boolean);
begin
  if AValue = True then
  begin
    if not Assigned(PatristicDists) then
      PatristicDists := TPatristicDistances.Create;
  end
  else
  begin
    if Assigned(PatristicDists) then
      FreeAndNil(PatristicDists);
  end;
end;

function TSimpleTreeNode.GetHasPatristicDists: Boolean;
begin
  Result := (Assigned(PatristicDists) and (PatristicDists.Count > 0));
end;

function TSimpleTreeNode.StringDescription: AnsiString;
begin
  if IsOtu then
    Result := Format('%8d %7d %6d %6d %6s %6s %6s %6s %-30s %-20s', [TempIndex, AncSpeciesIndex, Depth, NodeIndex + 1, '-', '-', BoolToStr(IsOtu, True), BoolToStr(IsDuplicationEvent, True), SequenceName, SpeciesName])
  else
    Result := Format('%8d %7d %6d %6d %6s %6s %6s %6s', [TempIndex, AncSpeciesIndex, Depth, NodeIndex + 1, IntToStr(Des1.NodeIndex + 1), IntToStr(Des2.NodeIndex + 1), BoolToStr(IsOtu, True), BoolToStr(IsDuplicationEvent, True)]);
end;

function CompareSimpleTreeNodes(Item1: Pointer; Item2: Pointer): Integer;
var
  Node1, Node2: TSimpleTreeNode;
begin
  Node1 := TSimpleTreeNode(Item1);
  Node2 := TSimpleTreeNode(Item2);

  if Node1.IsOtu and (not Node2.IsOtu) then
    Result := -1
  else if (not Node1.IsOtu) and Node2.IsOtu then
    Result := 1
  else if Node1.IsOtu and Node2.IsOtu then
    Result := CompareValue(Node1.NodeIndex, Node2.NodeIndex)
  else
    Result := CompareValue(Node2.NodeIndex, Node1.NodeIndex);
end;

function TreeAsTable(RootNode: TSimpleTreeNode): TStringList;
var
  Depth: Integer;

  procedure ProcessNode(ANode: TSimpleTreeNode);
  begin
    Result.Add(ANode.StringDescription);
    if Assigned(ANode.Des1) then
      ProcessNode(ANode.Des1);
    if Assigned(ANode.Des2) then
      Processnode(ANode.Des2);
  end;

begin
  Result := TStringList.Create;
  Result.Add(Format('%8s %7s %6s %6s %6s %6s %6s %6s %20s %20s', ['TempInd', 'AncInd', 'Depth', 'NodeId', 'Des1', 'Des2', 'IsOtu', 'IsDup', 'OtuName', 'SpName']));
  ProcessNode(RootNode);
end;

function TreeAsTable(ATree: TSimpleTreeNodeArray): TStringList;
var
  i: Integer;
  RootNode: TSimpleTreeNode;

  function FindRoot: TSimpleTreeNode;
  var
    j: Integer;
  begin
    for j := 0 to Length(ATree) - 1 do
      if not Assigned(ATree[j].Ancestor) then
      begin
        Result := ATree[j];
        break;
      end;
  end;

begin
  RootNode := FindRoot;
  Result := TreeAsTable(RootNode);
end;

procedure CloneTreeNodeArray(Source: TSimpleTreeNodeArray; var Destination: TSimpleTreeNodeArray);
var
  i: Integer;
begin
  Assert(Length(Source) = Length(Destination)); { it is assumed that memory is alread allocaton for the destination}
  for i := 0 to Length(Destination) - 1 do
  begin
    Destination[i].SpeciesName := Source[i].SpeciesName;
    Destination[i].Depth := Source[i].Depth;
    if Assigned(Source[i].Ancestor) then
      Destination[i].Ancestor := Destination[Source[i].Ancestor.NodeIndex];
    if not Source[i].IsOtu then
      Destination[i].Des1 := Destination[Source[i].Des1.NodeIndex];
    if not Source[i].IsOtu then
      Destination[i].Des2 := Destination[Source[i].Des2.NodeIndex];
    Destination[i].NodeIndex := Source[i].NodeIndex;
    Destination[i].TempIndex := Source[i].TempIndex; // needed for gene duplication inference, setup using a pre-order traversal
    Destination[i].IsDuplicationEvent := Source[i].IsDuplicationEvent;
    Destination[i].IsSpeciationEvent := Source[i].IsSpeciationEvent;
    Destination[i].IsOtu := Source[i].IsOtu;
    Destination[i].IsRoot := Source[i].IsRoot;
    Destination[i].SequenceName := Source[i].SequenceName;
    Destination[i].ExtantSpecies.Assign(Source[i].ExtantSpecies); // the names (sorted) of all species at the leaves of this sub-tree
    Destination[i].AncSpeciesIndex := Source[i].AncSpeciesIndex;
    Destination[i].Id := Source[i].Id;
    Destination[i].Accession := Source[i].Accession;
    Destination[i].ScientificName := Source[i].ScientificName;
    Destination[i].CommonName := Source[i].CommonName;
    Destination[i].Code := Source[i].Code;
    Destination[i].Annotation := Source[i].Annotation;
    Destination[i].Symbol := Source[i].Symbol;
    Destination[i].Value := Source[i].Value;
    if Source[i].HasPatristicDists then
    begin
      Destination[i].HasPatristicDists := True;
      Destination[i].PatristicDists.Assign(Source[i].PatristicDists);
    end;
  end;
end;

end.
