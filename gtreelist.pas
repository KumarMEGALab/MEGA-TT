unit gtreelist;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses

  Classes, SysUtils, Math,
  gtreedata, ttconst;

type

  TOutgroupNameValuePair = record
    TaxonName: String;
    IsOutgroupMember: Boolean;
  end;

  TOutgroupNameValuePairArray = array of TOutgroupNameValuePair;

  { TTimeTreeList }

  TTimeTreeList = class(TList)
  private
    FErrorMsg: String;
    FInformation: TStringList;
    FFreqName: AnsiString;
    FValueName: AnsiString;
    FValue2Name: AnsiString;
    FStatsName: AnsiString;
    FMaxStats: double;
    FDistanceMatrix: PDistanceMatrix;
    FOTUName: TStringList;
    FSpeciesNames: TStringList;
    FInternalNodeLbls: TStringList;
    FisRooted : boolean;

    function GetItems(index: integer):TTimeTreeData;
    procedure SetItems(index: integer; value: TTimeTreeData);

    function GetNoOfTrees:integer;
    function GetIsBLen:boolean;
    function GetIsSE:boolean;
    function GetIsStats:boolean;
    procedure SetIsBLen(value: boolean);
    procedure SetIsSE(value: boolean);
    procedure SetIsStats(value: boolean);
    function GetTotalFrequency:double;
    procedure SetTotalFrequency(value : double);
    function GetOTUName(index: integer):AnsiString;
    procedure SetOTUName(index: integer; value: AnsiString);
    function GetNoOfOTUs:integer;

    procedure SetInternalNodeLbl(index: integer; value: AnsiString);
    function GetHasInternalNodeLbls: Boolean;
    procedure SetInformation(source: TStringList);
    function WriteATreeToNewickFile(index: integer; var f: TextFile; bl,st:boolean; CutOff: double): boolean;
    function GetSpeciesName(Index: Integer): AnsiString;
    procedure SetSpeciesName(Index: Integer; const Value: AnsiString);
    function WriteSpeciesNamesToSession(var SessionFile: File): Boolean;
    function ReadSpeciesNamesFromSession(var SessionFile: File): Boolean;
  public
    function UpdateTimetreeIDs: Boolean;
    function HasUnwantedTaxa(WantedTaxa: TStringList): Boolean; { checks if the TreeList has any taxa names that are not in WantedTaxa}
    function GetOutgroupNameValuePairs(TreeIndex: Integer = 0): TOutgroupNameValuePairArray;
    function GetTrimmedTaxaNames: TStringList;
    function GetOtuToSpeciesMapList: TStringList; { gets a tstringlist where each item is 'otuname=speciesname'}
    procedure AssignSpeciesNames(AList: TStringList);
    procedure SetSpeciesNameForOtu(SpeciesName: AnsiString; OtuName: AnsiString);
    function HasSpeciesNames: Boolean;
    function GetSpeciesNameForOtu(OtuName: AnsiString): AnsiString;
    function GetInternalNodeLbl(index: integer):AnsiString; // public so TCalibrationDlg can get a reference to it
    property Items[Index: integer]: TTimeTreeData read GetItems write SetItems; default;
    property Information: TStringList read FInformation write SetInformation;
    property ValueName: AnsiString read FValueName write FValueName;
    property Value2Name: AnsiString read FValue2Name write FValue2Name;
    property FreqName: AnsiString read FFreqName write FFreqName;
    property StatsName: AnsiString read FStatsName write FStatsName;
    property MaxStats: Double read FMaxStats write FMaxStats;
    property DistanceMatrix: PDistanceMatrix read FDistanceMatrix write FDistanceMatrix;
    property OTUName[Index: integer]: AnsiString read GetOTUName write SetOTUName;
    property SpeciesName[Index: Integer]: AnsiString read GetSpeciesName write SetSpeciesName;
    property OTUNameList: TStringList read FOTUName write FOTUName;
    property SpeciesNamesList: TStringList read FSpeciesNames;
    property isRooted: boolean read FisRooted write FisRooted;
    property isBLen: boolean read GetIsBLen write SetIsBLen;
    property isSE: boolean read GetIsSE write SetIsSE;
    property isStats:boolean read GetIsStats write SetIsStats;
    property NoOfTrees: integer read GetNoOfTrees;
    property NoOfOTUs: integer read GetNoOfOTUs;
    property TotalFrequency: double read GetTotalFrequency write SetTotalFrequency;
    property InternalNodeLbl[Index: integer]: AnsiString read GetInternalNodeLbl write SetInternalNodeLbl;
    property InternalNodeLbls: TStringList read FInternalNodeLbls write FInternalNodeLbls;
    property HasInternalNodeLbls: boolean read GetHasInternalNodeLbls;
    procedure Add(tree: TTimeTreeData);
    procedure Insert(index: integer; tree: TTimeTreeData);
    function Remove(index: integer):TTimeTreeData;
    procedure Delete(index: integer);
    procedure DeleteAll;
    procedure Clear; override;
    procedure Assign(Source: TTimeTreeList);
    function ImportFromNewickFile(filename: String; NameList: TStringList): boolean;
    function ImportFromNewick(NewickTree: AnsiString; NameList: TStringList): boolean;
    function StripExtraneousParens(NewickString: AnsiString): AnsiString;
    function ExportToNewickFile(filename: String; bl,st:boolean; CutOff: double; MaxNumTrees:Integer = 0): boolean;
    function ExportATreeToNewickFile(index: integer; filename :String; bl,st:boolean; CutOff: double): boolean;
    function OutputNewickTree(index: integer; bl,st:boolean; CutOff: double): AnsiString;
    function OutputTable(index: integer; bl,st:boolean; CutOff: double): AnsiString;
    procedure ReadFromFile(var SessionFile: File; SessionFileVersion: integer);
    procedure WriteToFile(var data: File);
    function ToStringList: TStringList;
    procedure SetOutgroupMembers(Members: TBoolArray);
    constructor Create;
    destructor Destroy; override;
    function TranslateOtuNames(Translation: TStringList): Boolean; { Translation consist of key value pairs of the form 'oldName=newName'}
    function NodeNameToID(taxaName: AnsiString): Integer;
    function NodeLabelToID(NodeName: AnsiString): Integer;
    function FindMrca(TaxonAName: AnsiString; TaxonBName: AnsiString): Integer;
    property ErrorMsg: String read FErrorMsg;
  end;


implementation

uses

 StrUtils, gutils;

{ TTimeTreeList }

constructor TTimeTreeList.Create;
begin
  inherited Create;
  FInformation := TStringList.Create;
  FOTUName := TStringList.Create;
  FSpeciesNames := nil;
  FInternalNodeLbls := TStringList.Create;
  FMaxStats := -1.0;
end;

destructor TTimeTreeList.Destroy;
begin
  inherited Destroy;
  if Assigned(FInformation) then
    FInformation.Free;
  if Assigned(FOTUName) then
    FOTUName.Free;
  if Assigned(FInternalNodeLbls) then
    FInternalNodeLbls.Free;
  if Assigned(FSpeciesNames) then
    FSpeciesNames.Free;
end;

function TTimeTreeList.TranslateOtuNames(Translation: TStringList): Boolean;
var
  i: Integer;
  Index: Integer;
begin
  Result := False;
  FErrorMsg := EmptyStr;
  if not (FOTUName.Count = Translation.Count) then
  begin
    FErrorMsg := 'Found ' + IntToStr(FOTUName.Count) + ' OTUs in tree but there are ' + IntToStr(Translation.Count) + ' translations provided';
    Exit;
  end;
  for i := 0 to Translation.Count - 1 do
  begin
    Index := FOTUName.IndexOf(Translation.Names[i]);
    if Index >= 0 then
    begin
      FOTUName[Index] := Translation.ValueFromIndex[i];
    end;

  end;
  Result := True;
end;

function TTimeTreeList.GetItems(index: integer):TTimeTreeData;
begin
  result := inherited Items[index];
end;

function TTimeTreeList.GetOtuToSpeciesMapList: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  if Assigned(FOtuName) and Assigned(FSpeciesNames)and (FOtuName.Count = FSpeciesNames.Count) then
    if FOtuName.Count > 0 then
      for i := 0 to FOtuName.Count - 1 do
        Result.Add(FOtuName[i] + '=' + FSpeciesNames[i]);
end;

procedure TTimeTreeList.SetItems(index: integer; value: TTimeTreeData);
begin
  inherited Items[index] := value;
end;

function TTimeTreeList.GetOTUName(index: integer):AnsiString;
begin
  if index >= FOTUName.Count then
    result := ''
  else
    result := FOTUName[index];
end;

function TTimeTreeList.GetSpeciesName(Index: Integer): AnsiString;
begin
  if not Assigned(FSpeciesNames) then
    Result := EmptyStr
  else
  begin
    if index >= FSpeciesNames.Count then
      Result := EmptyStr
    else
      Result := FSpeciesNames[Index];
  end;
end;

function TTimeTreeList.GetSpeciesNameForOtu(OtuName: AnsiString): AnsiString;
var
  Index: Integer;
begin
  Result := EmptyStr;
  Index := OTUNameList.IndexOf(OtuName);
  if Index < FSpeciesNames.Count then
    FSpeciesNames[Index];
end;

procedure TTimeTreeList.SetOTUName(index: integer; value: AnsiString);
begin
  while index > FOTUName.Count-1 do
    FOTUName.Append('');
  FOTUName[index] := value;
end;

procedure TTimeTreeList.SetOutgroupMembers(Members: TBoolArray);
var
  i, j: Integer;
begin
  Assert(Length(Members) = (NoOfOtus));
  Assert(NoOfTrees > 0); { should never happen}
  if NoOfTrees = 0 then
    exit;
  for i := 0 to NoOfTrees - 1 do
  begin
    for j := 0 to NoOfOtus - 1 do
      Items[i].IsOutgroupMember[j] := Members[j];
  end;
end;

procedure TTimeTreeList.SetSpeciesName(Index: Integer; const Value: AnsiString);
var
  AStr: AnsiString;
begin
  if not Assigned(FSpeciesNames) then
    FSpeciesNames := TStringList.Create;
  while Index > FSpeciesNames.Count - 1 do
    FSpeciesNames.Append(EmptyStr);
  AStr := Value;
  TrimTaxaName(AStr);
  FSpeciesNames[Index] := AStr;
end;

procedure TTimeTreeList.SetSpeciesNameForOtu(SpeciesName: AnsiString;
  OtuName: AnsiString);
var
  Index: Integer;
  AName: AnsiString;
  BName: AnsiString;
begin
  if not Assigned(FSpeciesNames) then
    FSpeciesNames := TStringList.Create;
  AName := OtuName;
  TrimTaxaName(AName);
  Index := OTUNameList.IndexOf(AName);
  if Index < 0 then
    Index := OTUNameList.IndexOf(OtuName);
  while Index > FSpeciesNames.Count - 1 do
    FSpeciesNames.Append(EmptyStr);
  BName := SpeciesName;
  TrimTaxaName(BName);
  FSpeciesNames[Index] := BName;
end;

function TTimeTreeList.GetInternalNodeLbl(index: integer):AnsiString;
begin
  if index >= FInternalNodeLbls.Count then
    result := ''
  else
    result := FInternalNodeLbls[index];
end;

procedure TTimeTreeList.SetInternalNodeLbl(index: integer; value: AnsiString);
begin
  while index > FInternalNodeLbls.Count-1 do
    FInternalNodeLbls.Append('');
  FInternalNodeLbls[index] := value;
end;


function TTimeTreeList.GetHasInternalNodeLbls: Boolean;
begin
  result := (FInternalNodeLbls <> nil) and (FInternalNodeLbls.Count > 0)
end;

function TTimeTreeList.GetIsBLen:boolean;
begin
  if NoOfTrees = 0 then
    Result := false
  else
    Result := Items[0].isBLen;
end;

function TTimeTreeList.GetIsSE:boolean;
begin
  if NoOfTrees = 0 then
    Result := false
  else
    Result := Items[0].isSE;
end;

function TTimeTreeList.GetIsStats:boolean;
begin
  if NoOfTrees = 0 then
    Result := false
  else
    Result := (MaxStats > -0.000000001);
end;

procedure TTimeTreeList.SetIsBLen(value: boolean);
var
  i: integer;
begin
  if value = IsBLen then Exit;
  for i := 0 to Count-1 do
    Items[i].IsBLen := value;
end;

procedure TTimeTreeList.SetIsSE(value: boolean);
var
  i: integer;
begin
  if value = IsSE then Exit;
  for i := 0 to Count-1 do
    Items[i].IsSE := value;
end;

procedure TTimeTreeList.SetIsStats(value: boolean);
var
  i: integer;
begin
  if value = IsStats then Exit;
  if value then
    MaxStats := 0.0
  else
    MaxStats := -1.0;
end;

function TTimeTreeList.GetNoOfTrees:integer;
begin
  Result := Count;
end;

function TTimeTreeList.GetNoOfOTUs:integer;
begin
  if NoOfTrees = 0 then
    Result := 0
  else
    Result := Items[0].NoOfOTUs;
end;

procedure TTimeTreeList.Add(tree: TTimeTreeData);
begin
  if NoOfTrees = 0 then
  begin
    if tree.NoOfOTUs = 0 then Exit;
    inherited Add(tree);
  end
  else
  begin
    if tree.NoOfOTUs <> NoOfOTUs then Exit;
    if tree.isBLen <> isBLen then Exit;
    if tree.isSE <> isSE then Exit;
    inherited Add(tree);
  end;
end;

procedure TTimeTreeList.Insert(index: integer; tree: TTimeTreeData);
begin
  if NoOfTrees = 0 then
    Add(tree)
  else
  begin
    if tree.NoOfOTUs <> NoOfOTUs then Exit;
    if tree.isBLen <> isBLen then Exit;
    if tree.isSE <> isSE then Exit;
    inherited Insert(index, tree);
  end;
end;

// Iterates over the OTUName array to find the index (also is the NodeID) of the name.
function TTimeTreeList.NodeNameToID(taxaName: AnsiString): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to NoOfOTUs-1 do
    if compareText(OTUName[i], taxaName) = 0 then
    begin
      result := i;
      break;
    end;
end;

// Iterates over the Node label FNodeL array to find the index (also is the NodeID) of the node label.
function TTimeTreeList.NodeLabelToID(NodeName: AnsiString): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to NoOfOTUs-2 do
    if compareText(InternalNodeLbl[i], NodeName) = 0 then
    begin
      result := i;
      break;
    end;
end;

function TTimeTreeList.Remove(index: integer):TTimeTreeData;
begin
  Result := nil;
  if (index < 0) or (index >= NoOfTrees) then Exit;
  Result := Items[index];
  inherited Remove(Result);
end;

procedure TTimeTreeList.Delete(index: integer);
var
  tree: TTimeTreeData;
begin
  if (index < 0) or (index >= NoOfTrees) then Exit;
  tree := Items[index];
  inherited Remove(tree);
  tree.Free;
end;

procedure TTimeTreeList.DeleteAll;
begin
  if Self <> nil then
  begin
    while NoOfTrees > 0 do
      Delete(Count-1);
  end;
end;

procedure TTimeTreeList.Clear;
begin
  DeleteAll;

  FInformation.Clear;
  FOTUName.Clear;
  FInternalNodeLbls.Clear;
  FValueName  := '';
  FValue2Name := '';
  FFreqName   := '';
  FStatsName  := '';
  FMaxStats   := -1.0;
  FDistanceMatrix := nil;
  FisRooted := false;

  inherited;
end;

function TTimeTreeList.GetTotalFrequency:double;
var
  i: integer;
begin
  Result := 0.0;
  if NoOfTrees = 0 then Exit;
  for i := 0 to NoOfTrees-1 do
    Result := Result +Items[i].Freq;
end;

function TTimeTreeList.HasSpeciesNames: Boolean;
begin
  Result := (Assigned(FSpeciesNames)) and (FSpeciesNames.Count > 0);
end;

function TTimeTreeList.HasUnwantedTaxa(WantedTaxa: TStringList): Boolean;
var
  i: Integer;
begin
  Result := False;
  Assert((WantedTaxa.Count > 0) and (FOTUName.Count > 0));
  for i  := 0 to FOtuName.Count - 1 do
  begin
    if WantedTaxa.IndexOf(FOtuName[i]) < 0 then
    begin
      Result := True; { we found a taxon in our list that is not in WantedTaxa}
      break;
    end;
  end;
end;

procedure TTimeTreeList.SetTotalFrequency(value : double);
var
  i: integer;
  r: double;
begin
  if value = TotalFrequency then Exit;
  if NoOfTrees = 0 then Exit;
  r := value/TotalFrequency;
  for i := 0 to NoOfTrees-1 do
    Items[i].Freq := Items[i].Freq*r;
end;



procedure TTimeTreeList.ReadFromFile(var SessionFile: File; SessionFileVersion: integer);
var
  t: TTimeTreeData;
  i,j,ntree,notu: longint;
  flgBLen, flgSE, flgStats: boolean;
  buffer : array[0..8191] of AnsiChar;
  TempStr: AnsiString;
begin
  Clear;

  BlockRead(SessionFile, i, 4);     // for compatibility
  BlockRead(SessionFile, i, 4);     // for compatibility

  BlockRead(SessionFile, i, 4);
  if i > 0 then begin
      BlockRead(SessionFile, buffer, i);
      buffer[i] := #0;
      FInformation.CommaText := StrPas(buffer);
  end;

  BlockRead(SessionFile, i, 4);     // for compatibility
  BlockRead(SessionFile, i, 4);     // for compatibility
  BlockRead(SessionFile, i, 4);     // for compatibility
  BlockRead(SessionFile, i, 4);     // for compatibility

  BlockRead(SessionFile, i, 4);
  if i > 0 then begin
      BlockRead(SessionFile, buffer, i);
      buffer[i] := #0;
      FFreqName := StrPas(buffer);
  end;
  BlockRead(SessionFile, i, 4);
  if i > 0 then begin
      BlockRead(SessionFile, buffer, i);
      buffer[i] := #0;
      FValueName := StrPas(buffer);
  end;
  BlockRead(SessionFile, i, 4);
  if i > 0 then begin
      BlockRead(SessionFile, buffer, i);
      buffer[i] := #0;
      FValue2Name := StrPas(buffer);
  end;
  BlockRead(SessionFile, i, 4);
  if i > 0 then begin
      BlockRead(SessionFile, buffer, i);
      buffer[i] := #0;
      FStatsName := StrPas(buffer);
  end;

  BlockRead(SessionFile, i, 4);
  flgBLen := false;
  flgSE := false;
  flgStats := false;
  FisRooted := false;
  if (i and 1) = 1 then flgBLen := true;
  if (i and 2) = 2 then flgSE := true;
  if (i and 4) = 4 then flgStats := true;
  if (i and 8) = 8 then FisRooted := true;

  BlockRead(SessionFile, FMaxStats, 8);
  BlockRead(SessionFile, ntree, 4);
  BlockRead(SessionFile, notu, 4);
  for i := 0 to notu-1 do begin
    BlockRead(SessionFile, j, 4);
    BlockRead(SessionFile, buffer, j);
    buffer[j] := #0;
    OTUName[i] := StrPas(buffer);
    TempStr := OtuName[i];

    TrimTaxaName(TempStr);
    OTUName[i] := TempStr;
  end;
  if ntree > 0 then
    for i := 1 to ntree do
    begin
      t := TTimeTreeData.Create(notu, flgBLen, flgSE, flgStats);
      t.ReadFromFile(SessionFile, SessionFileVersion);
      Add(t);
    end;
end;

function TTimeTreeList.ReadSpeciesNamesFromSession(var SessionFile: File): Boolean;
var
  i, j: Integer;
  NameCount: Integer;
  NameLength: Integer;
  AChar: AnsiChar;
  AStr: AnsiString;
begin
  Result := False;
  try
    BlockRead(SessionFile, NameCount, SizeOf(Integer));
    if NameCount > 0 then
    begin
      if not Assigned(FSpeciesNames) then
        FSpeciesNames := TStringList.Create
      else
        FSpeciesNames.Clear;

      for i := 0 to NameCount - 1 do
      begin
        BlockRead(SessionFile, NameLength, SizeOf(Integer));
        if NameLength > 0 then
        begin
          SetLength(AStr, NameLength);
          for j := 1 to NameLength do
          begin
            BlockRead(SessionFile, AChar, SizeOf(AnsiChar));
            AStr[j] := AChar;
          end;
          FSpeciesNames.Add(AStr);
        end;
      end;
    end;
    Result := True;
  except
    on E: Exception do
      raise E;
  end;
end;

function TTimeTreeList.UpdateTimetreeIDs: Boolean;
begin
  Result := Items[0].UpdateTimeTreeIDs(FOTUName, FInternalNodeLbls);
end;

function TTimeTreeList.GetOutgroupNameValuePairs(TreeIndex: Integer = 0): TOutgroupNameValuePairArray;
var
  i: Integer;
  AName: AnsiString;
begin
  SetLength(Result, NoOfOTUs);
  for i := 0 to NoOfOtus - 1 do
  begin
    AName := GetOTUName(i);
    TrimTaxaName(AName);
    Result[i].TaxonName := AName;
    Result[i].IsOutgroupMember := Items[TreeIndex].IsOutgroupMember[i];
  end;
end;

function TTimeTreeList.GetTrimmedTaxaNames: TStringList;
var
  aName: AnsiString;
  i: Integer;
begin
  Result := TStringList.Create;
  if FOTUName.Count > 0 then
    for i:= 0 to FOTUName.Count - 1 do
    begin
      aName := FOTUName[i];
      TrimTaxaName(AName);
      Result.Add(AName);
    end;
end;

procedure TTimeTreeList.WriteToFile(var data: File);
var i,j: longint;
    buffer : array[0..8191] of AnsiChar;
begin
  i := 0;
  BlockWrite(data, i, 4);      // for compatibility
  BlockWrite(data, i, 4);      // for compatibility

  i := Length(FInformation.CommaText);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FInformation.CommaText);
    BlockWrite(data, buffer, i);
  end;

  i := 0;
  BlockWrite(data, i, 4);     // for compatibility
  BlockWrite(data, i, 4);     // for compatibility
  BlockWrite(data, i, 4);     // for compatibility
  BlockWrite(data, i, 4);     // for compatibility

  i := Length(FFreqName);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FFreqName);
    BlockWrite(data, buffer, i);
  end;
  i := Length(FValueName);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FValueName);
    BlockWrite(data, buffer, i);
  end;
  i := Length(FValue2Name);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FValue2Name);
    BlockWrite(data, buffer, i);
  end;
  i := Length(FStatsName);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FStatsName);
    BlockWrite(data, buffer, i);
  end;
  i := 0;
  if isBLen then i := i or 1;
  if isSE then i := i or 2;
  if isStats then i := i or 4;

  if isRooted then i := i or 8;
  BlockWrite(data, i, 4);
  BlockWrite(data, FMaxStats, 8);
  i := NoOfTrees;
  BlockWrite(data, i, 4);
  if NoOfTrees = 0 then
    i := FOTUName.Count
  else
    i := NoOfOTUs;
  BlockWrite(data, i, 4);
  for i := 0 to FOTUName.Count-1 do begin
    j := Length(OTUName[i]);
    StrPCopy(buffer, OTUName[i]);
    BlockWrite(data, j, 4);
    BlockWrite(data, buffer, j);
  end;
  for i := 0 to NoOfTrees-1 do
    Items[i].WriteToFile(data);
end;

procedure TTimeTreeList.SetInformation(source: TStringList);
begin
  FInformation.Assign(source);
end;

procedure TTimeTreeList.Assign(Source: TTimeTreeList);
var
  i : integer;
  newtree : TTimeTreeData;
begin
  Clear;
  FInformation.Assign(Source.FInformation );
  FOTUName.Assign(Source.FOTUName );
  if Assigned(Source.SpeciesNamesList) then
  begin
    if not Assigned(FSpeciesNames) then
      FSpeciesNames := TStringList.Create;
    FSpeciesNames.AddStrings(Source.SpeciesNamesList);
  end;
  FInternalNodeLbls.Assign(Source.FInternalNodeLbls);
  ValueName := Source.ValueName;
  Value2Name := Source.Value2Name;
  FreqName := Source.FreqName;
  StatsName := Source.StatsName;
  isRooted := Source.isRooted;
  isBLen := Source.isBLen;
  isSE := Source.isSE;
  isStats := Source.isStats;
  FMaxStats := Source.FMaxStats;

  TotalFrequency := Source.TotalFrequency;
  for i := 0 to Source.Count-1 do
  begin
    newtree := TTimeTreeData.Create(Source.NoOfOTUs, isBLen,isSE, true);
    newtree.Assign(Source[i]);
    Add(newtree);
  end;
end;

procedure TTimeTreeList.AssignSpeciesNames(AList: TStringList);
begin
  if not Assigned(FSpeciesNames) then
    FSpeciesNames := TStringList.Create;
  FSpeciesNames.Assign(AList);
end;

// This function strips out any matched parenthesis which contain only a single
// descendent so that newick parsing will not fail(PhyML will sometimes write
// newick strings in this way).
function TTimeTreeList.StripExtraneousParens(NewickString: AnsiString): AnsiString;
const
  SingleQuote = #39;
  DoubleQuote = #34;
var
  LeftParen: Integer;
  RightParen: Integer;
  CurrentPosition: Integer;
  NumQuotesToLeft: Integer;
  NumQuotesToRight: Integer;
  QuoteChar: AnsiChar;

  function InsideQuotedTaxaName(NString: AnsiString): Boolean;
  var
    MyLeft: Integer;
    MyRight: Integer;
    StopChars: Set of AnsiChar;
    InsideQuotes: Boolean;
  begin
    Result := True; // this is the safe case, so if we fail we won't modify the string
    StopChars := [SingleQuote, DoubleQuote];
    MyLeft := LeftParen;
    MyRight := RightParen;

    while (MyLeft > 0) and (not (NString[MyLeft] in StopChars))  do
      dec(MyLeft);
    while (MyRight < Length(NString)) and (not (NString[MyRight] in StopChars)) do
      inc(MyRight);

    // if we found the same quote character to the left and to the right, then we MIGHT be inside of a pair of matched quotes
    if ((NString[MyLeft] = SingleQuote) and (NString[MyRight] = SingleQuote)) or
       ((NString[MyLeft] = DoubleQuote) and (NString[MyRight] = DoubleQuote)) then
    begin
      QuoteChar := NString[MyLeft];
      InsideQuotes := False;
      MyLeft := 1;

      // starting from the beginning of the string, see if our left paren is inside of matched quotes
      while MyLeft < LeftParen do
      begin
        if NString[MyLeft] = QuoteChar then
          InsideQuotes := not InsideQuotes; // keep toggling in/out of quotes until we reach our destination
        inc(MyLeft);
      end;
      Result := InsideQuotes;
    end
    else
      Result := False;
  end;

  function MoveToLeftParen(StartFrom: Integer; Source: AnsiString): Boolean;
  var
    InsideQuotedString: Boolean;
  begin
    InsideQuotedString := False;
    Result := False;
    LeftParen := StartFrom;

    while (LeftParen > 0) and ((Source[LeftParen] <> '(') or (InsideQuotedString)) do
    begin
      dec(LeftParen);
      if (Source[LeftParen] = SingleQuote) or (Source[LeftParen] = DoubleQuote) then
        InsideQuotedString := not InsideQuotedString;
    end;

    if Source[LeftParen] = '(' then
      Result := True;
  end;

  function MoveToNextRightParen(Source: AnsiString): Boolean;
  var
    InsideQuotedString: Boolean;
  begin
    Result := False;
    InsideQuotedString := (Source[RightParen] = SingleQuote) or (Source[RightParen] = DoubleQuote);
    while (RightParen < Length(Source)) and ((Source[RightParen] <> ')') or InsideQuotedString) do
    begin
      inc(RightParen);
      if (Source[RightParen] = SingleQuote) or (Source[RightParen] = DoubleQuote) then
        InsideQuotedString := not InsideQuotedString;
    end;

    if Source[RightParen] = ')' then
      Result := True;
  end;

  function CommaFound(Source: AnsiString; StartFrom: Integer; RunTo: Integer): Boolean;
  var
    InsideQuotedString: Boolean;
    k: Integer;
  begin
    InsideQuotedString := False;
    Result := False;
    k := StartFrom;
    while k <= RunTo do
    begin
      if (Source[k] = SingleQuote) or (Source[k] = DoubleQuote) then
        InsideQuotedString := not InsideQuotedString;

      if (not InsideQuotedString) and (Source[k] = ',') then
      begin
        Result := True;
        Exit;
      end;
      inc(k);
    end;
  end;

begin
  Result := trim(NewickString);
  RightParen := 1;

  try
    while True do
    begin
      if MoveToNextRightParen(Result) then
      begin
        if MoveToLeftParen(RightParen, Result) then
        begin
          if (not CommaFound(Result, LeftParen, RightParen)) and (not InsideQuotedTaxaName(Copy(Result, LeftParen, RightParen - LeftParen))) then
          begin
            System.Delete(Result, RightParen, 1);
            System.Delete(Result, LeftParen, 1);
            dec(RightParen);
          end
          else
            inc(RightParen);
        end
        else
          Exit;
      end
      else
        Exit;
    end;
  Except
    Result := NewickString; // then just return the original string so the parser can do its thing
  end;
end;

function TTimeTreeList.ImportFromNewick(NewickTree : AnsiString; NameList: TStringList): boolean;
var
  filebuffer, buffer: AnsiString;
  CurTree, CurOTU, CurNode, i, j, CurPos, TotalPos, Progress: integer;
  temptree: TTimeTreeData;
  ErrorMsg: AnsiString; { try and give the user something usefull if parsing fails}
  ErrorIndex: Integer; { so we can try and let the user know at which character we failed}

   function GetOTUindex:integer;
    var
      str: AnsiString;
      k,n: integer;
    begin
      result := -1;
      n := length(buffer);
      if i >= n then
        exit;

      k := i;
      if buffer[i] = '''' then { then it is a quoted name}
      begin
        inc(i);
        repeat
          if buffer[i] = '''' then
            if (i < n-1) and (buffer[i+1] = '''') then
              inc(i,2)
            else
              break
          else
            inc(i);
        until i = n;
        inc(i);

        str := Copy(buffer, k+1, i-k-2);

        k := length(str);
        while k > 1 do
        begin
          if str[k] = '''' then
            if (k > 1) and (str[k-1] = '''') then
            begin
              system.Delete(str, k, 1);
              dec(k);
            end;
          dec(k);
        end;
      end
      else { the name is not quoted}
      begin
        repeat
          Inc(i);
        until (buffer[i] = ':') or (buffer[i] = AnsiChar(#9))
           or (buffer[i] = ',') or (buffer[i] = ')')
           or (i = length(buffer));
        str := Copy(buffer, k, i-k);
        TrimTaxaName(str);
      end;

      if NameList = nil then
      begin
        if FOTUName.Count = CurOTU then
        begin
          OTUName[CurOTU] := str;
          Result := CurOTU;
          Inc(CurOTU);
        end
        else
        begin
          for k := 0 to FOTUName.Count-1 do
            if OTUName[k] = str then
            begin
              result := k;
              Break;
            end;
        end
      end
      else
      begin
        for k := 0 to NameList.Count-1 do
          if NameList[k] = str then
          begin
            OTUName[k] := str;
            result := k;
            Break;
          end;
        if Result = -1 then { correct for discrepency when quoted labels are combined with underscores}
        begin
          TrimTaxaName(str);
          for k := 0 to NameList.Count-1 do
            if NameList[k] = str then
            begin
              OTUName[k] := str;
              result := k;
              Break;
            end;
        end;

        if result = -1 then
          raise Exception.Create('MEGA Error: Could not find entry in NameList = ' + str + '.  Ensure that the names in your tree exactly match the names in your analysis file.');

      end;
    end;

    function SetBranchData(bn: integer):boolean;
    var
      str: AnsiString;
      j,n,c,d: integer;
      k: Integer;
    begin
      // if buffer[i] = #9 that means we have a clustal type bootstrap/stats value.
      // If bufer[i] = ' then we may have a quoted node label'
      result := false;
      if i >= length(buffer) then
        exit;

      try
        n := length(buffer);
        while (i <= n) and (buffer[i] <> ')') and (buffer[i] <> ',') do
          if buffer[i] = '''' then
          begin
            inc(i);
            d := i;
            repeat
              if buffer[i] = '''' then
                if (i < n-1) and (buffer[i+1] = '''') then
                  inc(i,2)
                else
                  break
              else
                inc(i);
            until i = n;
            inc(i);
            str := Copy(buffer, d, i-d-1);
            SetInternalNodeLbl(bn-NoOfOTUs, str);
          end
          else if buffer[i] = ':' then
          begin
            j := i+1;
            repeat
              Inc(i);
            until (buffer[i] = AnsiChar(#9)) or (buffer[i] = ',') or (buffer[i] = ')') or (i = n);
            str := Copy(buffer, j, i-j);
            Items[CurTree].BLenArray[bn] := StrToFloat(str);
          end
          else if buffer[i] = AnsiChar(#9) then
          begin
            j := i+1;
            repeat
              Inc(i);
            until (buffer[i] = ':') or (buffer[i] = ',') or (buffer[i] = ')') or (i = n);
            str := Copy(buffer, j, i-j);
            Items[CurTree].StatsArray[bn] := StrToFloat(str);
            if Pos(FormatSettings.DecimalSeparator, str) > 0 then
              if StrToFloat(str) > MaxStats then
                if StrToFloat(str) < 1 then
                  MaxStats := 1
                else
                  MaxStats := power(10, ceil(log10(StrToFloat(str))));
          end
          else
            Inc(i);
      except
        exit;
      end;
      result := true;
    end;

    function DecodeNodeData:integer;
    var
      n1,n2: integer;
    begin
      result := -1;
      if i = length(buffer) then
        exit;

      if buffer[i] = ';' then
        exit;

      Inc(i);
      if buffer[i] = '(' then
        n1 := DecodeNodeData
      else
        n1 := GetOTUindex;
      if n1 = -1 then
        exit;

      if not SetBranchData(n1) then
        exit;

      if i >= length(buffer) then
        exit;


      Inc(i);
      if buffer[i] = '(' then
        n2 := DecodeNodeData
      else
        n2 := GetOTUindex;
      if n2 = -1 then
        exit;

      if not SetBranchData(n2) then
        exit;


      Items[CurTree].NodeArray[CurNode].des1 := n1;
      Items[CurTree].NodeArray[CurNode].des2 := n2;

      Result := CurNode +NoOfOTUs;
      Inc(CurNode);

      isRooted := true;

      while buffer[i] = ',' do
      begin
        result := -1;
        n1 := CurNode +NoOfOTUs -1;

        Inc(i);
        if buffer[i] = '(' then
          n2 := DecodeNodeData
        else
          n2 := GetOTUindex;
        if n2 = -1 then
          exit;

        if not SetBranchData(n2) then
          exit;

        Items[CurTree].NodeArray[CurNode].des1 := n1;
        Items[CurTree].NodeArray[CurNode].des2 := n2;
        Result := CurNode +NoOfOTUs;
        Inc(CurNode);

        Items[CurTree].StatsArray[n1] := -1.0;

        isRooted := false;
      end;
      Inc(i);
      Inc(CurPos);
    end;

    function CheckPhylipFormat:boolean;
    var
      i,j: integer;
      flag: boolean;
    begin
      Result := false;
      flag := true;
      i := 1;
      while i < Length(filebuffer) do
      begin
        if filebuffer[i] = '''' then
          flag := not flag;
        if not flag then
          inc(i)
        else if filebuffer[i] = ')' then
        begin
          j := 1;
          while ((i+j) < length(filebuffer)) and (filebuffer[i+j] <> ',') and (filebuffer[i+j] <> ')') do
          begin
            if filebuffer[i+j] = '[' then
            begin
              Result := true;
              exit;
            end;
            inc(j);
          end;
          i := i+j;
        end
        else
          inc(i);
      end;
    end;

    procedure ConvertPhylipFormat;
    var
      i,j : integer;
      flag: boolean;
    begin
      flag := true;
      i := 1;
      while i < Length(filebuffer) do
      begin
        if filebuffer[i] = '''' then
          flag := not flag;
        if not flag then
          inc(i)
        else if filebuffer[i] = ')' then
        begin
          j := 1;
          while ((i+j) < length(filebuffer)) and (filebuffer[i+j] <> ',') and (filebuffer[i+j] <> ')') do
            if filebuffer[i+j] = '[' then
            begin
              filebuffer[i+j] := AnsiChar(#9);
              inc(j);
            end
            else if filebuffer[i+j] = ']' then
            begin
              System.Delete(filebuffer, i+j, 1);
              break;
            end
            else
              inc(j);
          i := i+j;
        end
        else
          inc(i);
      end;
    end;

    function CheckClustalFormat:boolean;
    var
      i,j,n: integer;
      flag: boolean;
    begin
      n := Length(filebuffer);
      result := false;
      flag := true;
      for i := 1 to n-1 do
      begin
        if filebuffer[i] = '''' then
          flag := not flag;
        if not flag then
          continue
        else if filebuffer[i] = ')' then
          if (Ord(filebuffer[i+1]) >= 48) and (Ord(filebuffer[i+1]) <= 57) then
          begin
            result := true;
            j := i+2;
            while (j < n) and (filebuffer[j] <> ',') and (filebuffer[j] <> ':') and (filebuffer[j] <> ')') and (filebuffer[j] <> ';') do
            begin
              if (Ord(filebuffer[i+1]) < 48) or (Ord(filebuffer[i+1]) > 57) then
              begin
                result := false;
                break;
              end;
              j := j+1;
            end;
            if not result then
              break;
          end
          else if filebuffer[i + 1] <> ':' then
            break;
      end;
    end;

    function CheckPAUPFormat1:boolean;
    var
      i: integer;
      flag: boolean;
    begin
      result   := false;
      flag := true;
      for i := 1 to Length(filebuffer)-1 do
      begin
        if filebuffer[i] = '''' then
          flag := not flag;
        if not flag then
          continue
        else if filebuffer[i] = ')' then
        begin
          if filebuffer[i+1] = ':' then
            result := true;
        end
        else if filebuffer[i+1] = ':' then
        begin
          result := false;
          break;
        end;
      end;
    end;

    procedure CheckFormat2;
    var
      i,j: integer;
      s: double;
      flag: boolean;
    begin
      if not isBLen then
        exit;

      s := 0;
      for i := 0 to 2*NoOfOTUs-3 do
        s := s +Items[0].BLen[i];
      if s < 0.0000000000001 then
      begin
        isBLen := false;
        exit;
      end;

      flag := true;
      for i := NoOfOTUs to 2*NoOfOTUs-3 do
        if abs(Items[0].Stats[i]-Items[0].BLen[i]) > 0.0000000000001 then
        begin
          flag := false;
          break;
        end;
      if IsStudyTree then
        flag := False;
      if flag then
      begin
        if Items[0].BLen[0] > 0.00000000001 then
          MaxStats := Items[0].BLen[0];

        isBLen := false;
        exit;
      end;

      flag := true;
      for i := 1 to NoOfOTUs-1 do
      if abs(Items[0].BLen[i]-Items[0].BLen[0]) > 0.00000000000001 then
      begin
        flag := false;
        break;
      end;
      if IsStudyTree then
        flag := False;
      if flag then
      begin
        s := 0;
        for i := 0 to NoOfTrees-1 do
          for j := 0 to 2*NoOfOTUs-3 do
          begin
            if Items[i].Stats[j] > -0.0000000000001 then
              Items[i].Stats[j] := Items[i].BLen[j];
            if s < Items[i].Stats[j] then
              s := Items[i].Stats[j];
          end;

        if Items[0].BLen[0] > 0.00000000001 then
          MaxStats := Items[0].BLen[0]
        else if s < 1.00000000001 then
          MaxStats := 1.0
        else if s < 100.00000000001 then
          MaxStats := 100
        else
          MaxStats := 0;
        StatsName := '';
        isBLen := false;
      end;
    end;

    procedure ConvertPAUPFormat1;
    var
      i: integer;
      flag: boolean;
    begin
      flag := true;
      for i := 1 to Length(filebuffer) do
      begin
        if filebuffer[i] = '''' then
          flag := not flag;
        if not flag then
          continue
        else if filebuffer[i] = ':' then
          filebuffer[i] := AnsiChar(#9);
      end;
    end;

    procedure ConvertPAUPFormat2;
    var
      i,j: integer;
    begin
      for i := 0 to NoOfTrees-1 do
        for j := 0 to 2*NoOfOTUs-3 do
          if Items[i].Stats[j] > -0.0000000000001 then
            Items[i].Stats[j] := Items[i].BLen[j];
      isBLen := false;
      MaxStats := 0;
      StatsName := 'Unknown';
    end;

    procedure ConvertClustalFormat;
    var
      i : integer;
      flag: boolean;
    begin
      i := 1;
      flag := true;
      while i < Length(filebuffer) do
      begin
        if filebuffer[i] = '''' then
          flag := not flag;
        if flag and
           (filebuffer[i]=')') and
           (Ord(filebuffer[i+1]) >= 48) and
           (Ord(filebuffer[i+1]) <= 57) then
          System.Insert(AnsiChar(#9), filebuffer, i+1);
        inc(i);
      end;
    end;

    /// <summary>Performs some simple format checks on the given Newick buffer string.
    /// Tests that we have equal numbers of left and right parenthesis as well as the
    /// existence of multiple branches inside of parenthesis.</summary>
    function CheckFormat(buffer: AnsiString): boolean;
    var
      numLeftParens: Integer;
      numRightParens: Integer;
      numCommas: Integer;
      i,n: integer;
      insideComment: Boolean;
    begin
      result := true; // assume that we will be successful
      numLeftParens := 0;
      numRightParens := 0;
      numCommas := 0;  // number of commas (inside of the current pair of parenthesis)
      insideComment := false;
      n := length(buffer);
      i := 1;

      repeat
        if insideComment then
        begin
          if buffer[i] = ']' then
            insideComment := false; // we do not support nested comments
          inc(i);
          continue;
        end
        else if buffer[i] = '[' then
        begin
          insideComment := true;
          inc(i);
          continue;
        end;
        if (not insideComment) and (buffer[i] = '''') then
        begin   // keep going until we are no longer inside of a quoted string
          inc(i);
          repeat
            if buffer[i] = '''' then
              if (i < n-1) and (buffer[i+1] = '''') then
                inc(i,2)
              else
                break
            else
              inc(i);
          until i = n;
          inc(i);
        end;

        if buffer[i] = ';' then // reached the end of a tree
        begin
          if (numLeftParens = 0) then
          begin
            result := false;
            break;
          end;
          if numLeftParens <> numRightParens then
          begin
            result := false;
            break;
          end;
          numLeftParens := 0; // else, reset the counts for left and right parens because we are parsing another tree
          numRightParens := 0;
        end
        else if buffer[i] = '(' then
        begin
          numCommas := 0;
          inc(numLeftParens);
        end
        else if buffer[i] = ')' then
        begin
          if numCommas = 0 then // if there were no commas inside of the current pair of parens (not sure if this is correct, it is ok to just have a branch inside)
          begin
            result := false;
            break;
          end;
          inc(numRightParens);
        end
        else if buffer[i] = ',' then
          inc(numCommas);
        inc(i);
      until i >= n;

      if numLeftParens <> numRightParens then
        result := false;
    end;

var
  str: AnsiString;
  d,n, temp: integer;
  hasBranchLengths: Boolean;
  insideQuotedString: Boolean;
begin
  Result := false;  // assume that parsing will fail
  if NewickTree = EmptyStr then
  begin
    Raise Exception.Create('The newick tree file you specified was empty.  Please check that your tree file is valid.');
  end;

  CurPos := 0;
  Progress := 0;
  filebuffer := NewickTree;
  TotalPos := 0;
  for i := 1 to length(filebuffer) do   // Count total number of '(' in tree
    if filebuffer[i] = '(' then
      inc(TotalPos);

  Clear;

  try
    try
      while Pos(AnsiChar(#9), filebuffer) > 0 do  { remove all tab characters}
          System.Delete(filebuffer, Pos(AnsiChar(#9), filebuffer), 1);

        // Note: '''' = a string consisting of a single quote.
        i := length(filebuffer);
      //if not CheckFormat(filebuffer) then
      //  exit;

        MaxStats := 0;
        StatsName := 'Unkown';

        while Pos('[', filebuffer) > 0 do // remove comments from the buffer string
        begin
          i := Pos('[', filebuffer);
          j := Pos(']', filebuffer)-i+1;
          System.Delete(filebuffer, i, j);
        end;
      if (Pos('[', filebuffer) > 0) or (Pos(']', filebuffer) > 0) then // the case where we have unmatched square brackets
        exit;

      if Pos(':', filebuffer) > 0 then
        hasBranchLengths := true
      else
        hasBranchLengths := false;

      if MaxStats < 0.00000000001 then
        if Pos(AnsiChar(#9), filebuffer) > 0 then
          MaxStats := 0
        else
          MaxStats := -1.0;

      buffer := Copy(filebuffer, 1, Pos(';', filebuffer));

      n := 0;
      insideQuotedString := false;
      for i := 1 to Length(buffer)-1 do
      begin
        if filebuffer[i] = '''' then
        begin
          insideQuotedString := not insideQuotedString;
          continue;
        end;

        if insideQuotedString then continue;

        if (buffer[i] = '(') or (buffer[i] = ',') then
          if buffer[i+1] <> '(' then
            Inc(n);
      end;

      if n <= 1 then
        exit;

      CurTree := 0;
      while Pos(';', filebuffer) > 0 do
      begin
        buffer := Copy(filebuffer, 1, Pos(';', filebuffer));
        if Pos('(', buffer) > 1 then
          System.Delete(buffer, 1, Pos('(', buffer)-1); // delete anything before the first '('

        CurOTU := 0;
        CurNode := 0;

        temptree := TTimeTreeData.Create(n, hasBranchLengths, false, true);
        temptree.Freq := 1.0;
        Add(temptree);

        i := 1;
        temp := DecodeNodeData;
        if temp = -1 then
        begin
          Clear;
          Result := false;
          exit;
        end;

        // Handle any node label at the end (root) node.
        if buffer[i] = '''' then
        begin
          inc(i);
          d := i;
          repeat
            if buffer[i] = '''' then
              if (i < n-1) and (buffer[i+1] = '''') then
                inc(i,2)
              else
                break
            else
              inc(i);
          until i = n;
          inc(i);
          str := Copy(buffer, d, i-d-1);
          SetInternalNodeLbl(2 * (NoOfOtus - 1) - NoOfOTUs - 1, str);
        end;

        System.Delete(filebuffer, 1, Pos(';', filebuffer));
        Inc(CurTree);
      end;

      if CurTree = 0 then
        Result := false
      else
      begin
        CheckFormat2;
        Result := true;
      end;
    except
      on E:Exception do
      begin
        Clear;
        Result := false;
      end;
    end;
  finally

  end;
end;


function  TTimeTreeList.ImportFromNewickFile(filename : String; NameList: TStringList):boolean;
var
  data: TextFile;
  filebuffer, buffer: AnsiString;
begin
  Result := false;

  try
    filebuffer := '';
    if FileExists(filename) then
    begin
      AssignFile(data, filename);
      Reset(data);

      while not Eof(data) do
      begin
        Readln(data, buffer);
        filebuffer := filebuffer +buffer;
      end;
      Result := ImportFromNewick(filebuffer, NameList);
    end;
  finally
    if FileExists(filename) then
      CloseFile(data);
  end;
end;

function TTimeTreeList.OutputNewickTree(index: integer; bl,st:boolean; CutOff: double): AnsiString;
var
  bflag: boolean;

  function CheckOTUName(str: AnsiString): AnsiString;
  var
    i: integer;
  begin
    result := trim(str);
    if (Pos('(', result) > 0) or (Pos(')', result) > 0) or (Pos(':', result) > 0) or (Pos(';', result) > 0) or
       (Pos('''', result) > 0) or (Pos('_', result) > 0) then
    begin
      for i := length(result) downto 1 do
        if result[i] = '''' then
          system.Insert('''', result, i);
      result := '''' +result +'''';
    end
    else
    begin
      while Pos(' ', result) > 0 do
        result[Pos(' ', result)] := '_';
    end;
  end;

  procedure ExportNode(nodeindex: integer);
  var
    flag: boolean;
  begin
    flag := Items[index].Stats[Items[index][nodeindex].des1]+0.000000000001 >= CutOff;
    if Items[index][nodeindex].des1 < NoOfOTUs then
      result := result +CheckOTUName(OTUName[Items[index][nodeindex].des1])
    else if flag then
    begin
      result := result +'(';
      ExportNode(Items[index][nodeindex].des1-NoOfOTUs);
      if Trim(InternalNodeLbl[Items[index][nodeindex].des1-NoOfOTUs]) <> EmptyStr then
        result := result +')' + #39 + InternalNodeLbl[Items[index][nodeindex].des1-NoOfOTUs] + #39
      else
        result := result + ')';
    end
    else
      ExportNode(Items[index][nodeindex].des1-NoOfOTUs);
    if flag then
    begin
      if st and isStats and (Items[index][nodeindex].des1 >= NoOfOTUs) then
        if abs(MaxStats) < 0.000000000001 then
          result := result +IntToStr(Trunc(Items[index].Stats[Items[index][nodeindex].des1]+0.0000000000001))
        else
          result := result +FloatToStrF(Items[index].Stats[Items[index][nodeindex].des1]/MaxStats,ffFixed,8,4);  // Tamura change Jan / 15
      if bl and isBLen and bflag then
        result := result +':'+FloatToStrF(Items[index].BLen[Items[index][nodeindex].des1],ffFixed,15,8);
    end;

    result := result +',';

    flag := Items[index].Stats[Items[index][nodeindex].des2]+0.000000000001 >= CutOff;
    if Items[index][nodeindex].des2 < NoOfOTUs then
      result := result +CheckOTUName(OTUName[Items[index][nodeindex].des2])
    else if flag then
    begin
      result := result +'(';
      ExportNode(Items[index][nodeindex].des2-NoOfOTUs);
      if Trim(InternalNodeLbl[Items[index][nodeindex].des2-NoOfOTUs]) <> EmptyStr then
        result := result +')' + #39 + InternalNodeLbl[Items[index][nodeindex].des2-NoOfOTUs] + #39
      else
        result := result + ')';
    end
    else
      ExportNode(Items[index][nodeindex].des2-NoOfOTUs);
    if flag then
    begin
      if st and isStats and (Items[index][nodeindex].des2 >= NoOfOTUs) then
        if abs(MaxStats) < 0.000000000001 then
          result := result +IntToStr(Trunc(Items[index].Stats[Items[index][nodeindex].des2]+0.0000000000001))
        else
          result := result +FloatToStrF(Items[index].Stats[Items[index][nodeindex].des2]/MaxStats,ffFixed,8,4);  // Tamura change Jan / 15
      if isBLen and bflag then
        result := result +':'+FloatToStrF(Items[index].BLen[Items[index][nodeindex].des2],ffFixed,15,8);
    end;
  end;

var root, i, j: integer;
    flag: boolean;
begin
  result := '';
  try
    root := -1;
    bflag := bl and (Items[index].SBL > 0.0000000000001);

    for i := 2*NoOfOTUs-2 downto NoOfOTUs do
    begin
      flag := true;
      for j := NoOfOTUs-2 downto 0 do
      begin
        if Items[index][j].des1 = i then
        begin
          flag := false;
          Break;
        end;
        if Items[index][j].des2 = i then
        begin
          flag := false;
          Break;
        end;
      end;
      if flag then
      begin
        root := i-NoOfOTUs;
        Break;
      end;
    end;
    result := result +'(';
    if IsRooted then
      ExportNode(root)
    else if Items[index][root].des1 < NoOfOTUs then
    begin
      result := result +CheckOTUName(OTUName[Items[index][root].des1]);
      if isBLen and bflag then
        result := result +':'+FloatToStrF(Items[index].BLen[Items[index][root].des1]+Items[index].BLen[Items[index][root].des2],ffFixed,15,8);
      result := result +',';

      ExportNode(Items[index][root].des2-NoOfOTUs);
    end
    else if Items[index][root].des2 < NoOfOTUs then
    begin
      ExportNode(Items[index][root].des1-NoOfOTUs);
      result := result +','+CheckOTUName(OTUName[Items[index][root].des2]);
      if isBLen and bflag then
        result := result +':'+FloatToStrF(Items[index].BLen[Items[index][root].des1]+Items[index].BLen[Items[index][root].des2],ffFixed,15,8);
    end
    else
    begin
      ExportNode(Items[index][root].des1-NoOfOTUs);

      if Items[index].Stats[Items[index][root].des2]+0.000000000001 >= CutOff then
      begin
        result := result +',(';
        ExportNode(Items[index][root].des2-NoOfOTUs);
        result := result +')';
        if st and isStats  then
          if abs(MaxStats) < 0.000000000001 then
            result := result +IntToStr(Trunc(Items[index].Stats[Items[index][root].des2]+0.000000000001))
          else
            result := result +FloatToStrF(Items[index].Stats[Items[index][root].des2]/MaxStats,ffFixed,8,4);  // Tamura change Jan / 15
        if isBLen and bflag then
          result := result +':'+FloatToStrF(Items[index].BLen[Items[index][root].des1]+Items[index].BLen[Items[index][root].des2],ffFixed,15,8);
      end
      else
        ExportNode(Items[index][root].des2-NoOfOTUs);
    end;
    result := result +');';
  except
    result := '';
    exit;
  end;
end;

function TTimeTreeList.OutputTable(index: integer; bl,st:boolean; CutOff: double): AnsiString;
var
  bflag: boolean;

  function CheckOTUName(str: AnsiString): AnsiString;
  var
    i: integer;
  begin
    result := trim(str);
    if (Pos('(', result) > 0) or (Pos(')', result) > 0) or (Pos(':', result) > 0) or (Pos(';', result) > 0) or
       (Pos('''', result) > 0) or (Pos('_', result) > 0) then
    begin
      for i := length(result) downto 1 do
        if result[i] = '''' then
          system.Insert('''', result, i);
      result := '''' +result +'''';
    end
    else
    begin
      while Pos(' ', result) > 0 do
        result[Pos(' ', result)] := '_';
    end;
  end;

  procedure ExportNode(nodeindex: integer);
  var
    flag: boolean;
  begin
    flag := Items[index].Stats[Items[index][nodeindex].des1]+0.000000000001 >= CutOff;
    if Items[index][nodeindex].des1 < NoOfOTUs then
      result := result +CheckOTUName(OTUName[Items[index][nodeindex].des1])
    else if flag then
    begin
      result := result +'(';
      ExportNode(Items[index][nodeindex].des1-NoOfOTUs);
      result := result +')';
    end
    else
      ExportNode(Items[index][nodeindex].des1-NoOfOTUs);
    if flag then
    begin
      if st and isStats and (Items[index][nodeindex].des1 >= NoOfOTUs) then
        if abs(MaxStats) < 0.000000000001 then
          result := result +IntToStr(Trunc(Items[index].Stats[Items[index][nodeindex].des1]+0.0000000000001))
        else
          result := result +FloatToStrF(Items[index].Stats[Items[index][nodeindex].des1]/MaxStats,ffFixed,8,4);  // Tamura change Jan / 15
      if bl and isBLen and bflag then
        result := result +':'+FloatToStrF(Items[index].BLen[Items[index][nodeindex].des1],ffFixed,15,8);
    end;

    result := result +',';

    flag := Items[index].Stats[Items[index][nodeindex].des2]+0.000000000001 >= CutOff;
    if Items[index][nodeindex].des2 < NoOfOTUs then
      result := result +CheckOTUName(OTUName[Items[index][nodeindex].des2])
    else if flag then
    begin
      result := result +'(';
      ExportNode(Items[index][nodeindex].des2-NoOfOTUs);
      result := result +')';
    end
    else
      ExportNode(Items[index][nodeindex].des2-NoOfOTUs);
    if flag then
    begin
      if st and isStats and (Items[index][nodeindex].des2 >= NoOfOTUs) then
        if abs(MaxStats) < 0.000000000001 then
          result := result +IntToStr(Trunc(Items[index].Stats[Items[index][nodeindex].des2]+0.0000000000001))
        else
          result := result +FloatToStrF(Items[index].Stats[Items[index][nodeindex].des2]/MaxStats,ffFixed,8,4);  // Tamura change Jan / 15
      if isBLen and bflag then
        result := result +':'+FloatToStrF(Items[index].BLen[Items[index][nodeindex].des2],ffFixed,15,8);
    end;
  end;

var root, i, j: integer;
    flag: boolean;
begin
  result := '';
  try
    root := -1;
    bflag := bl and (Items[index].SBL > 0.0000000000001);

    for i := 2*NoOfOTUs-2 downto NoOfOTUs do
    begin
      flag := true;
      for j := NoOfOTUs-2 downto 0 do
      begin
        if Items[index][j].des1 = i then
        begin
          flag := false;
          Break;
        end;
        if Items[index][j].des2 = i then
        begin
          flag := false;
          Break;
        end;
      end;
      if flag then
      begin
        root := i-NoOfOTUs;
        Break;
      end;
    end;

    if isRooted then
      ExportNode(root)
    else if Items[index][root].des1 < NoOfOTUs then
    begin
      result := result +CheckOTUName(OTUName[Items[index][root].des1]);
      if isBLen and bflag then
        result := result +':'+FloatToStrF(Items[index].BLen[Items[index][root].des1]+Items[index].BLen[Items[index][root].des2],ffFixed,15,8);
      result := result +',';

      ExportNode(Items[index][root].des2-NoOfOTUs);
    end
    else if Items[index][root].des2 < NoOfOTUs then
    begin
      ExportNode(Items[index][root].des1-NoOfOTUs);
      result := result +','+CheckOTUName(OTUName[Items[index][root].des2]);
      if isBLen and bflag then
        result := result +':'+FloatToStrF(Items[index].BLen[Items[index][root].des1]+Items[index].BLen[Items[index][root].des2],ffFixed,15,8);
    end
    else
    begin
      ExportNode(Items[index][root].des1-NoOfOTUs);

      if Items[index].Stats[Items[index][root].des2]+0.000000000001 >= CutOff then
      begin
        result := result +',(';
        ExportNode(Items[index][root].des2-NoOfOTUs);
        result := result +')';
        if st and isStats  then
          if abs(MaxStats) < 0.000000000001 then
            result := result +IntToStr(Trunc(Items[index].Stats[Items[index][root].des2]+0.000000000001))
          else
            result := result +FloatToStrF(Items[index].Stats[Items[index][root].des2]/MaxStats,ffFixed,8,4);  // Tamura change Jan / 15
        if isBLen and bflag then
          result := result +':'+FloatToStrF(Items[index].BLen[Items[index][root].des1]+Items[index].BLen[Items[index][root].des2],ffFixed,15,8);
      end
      else
        ExportNode(Items[index][root].des2-NoOfOTUs);
    end;
    result := result +');';
  except
    result := '';
    exit;
  end;
end;


function TTimeTreeList.WriteATreeToNewickFile(index: integer; var f: TextFile; bl,st:boolean; CutOff: double): boolean;
var
  buffer: AnsiString;
begin
  buffer := OutputNewickTree(index, bl, st, CutOff);
  result := buffer <> '';
  Writeln(f,buffer);
end;


function TTimeTreeList.WriteSpeciesNamesToSession(var SessionFile: {$IFNDEF FPC}{$ENDIF}File): Boolean;
var
  i, j, NameLength: Integer;
  AChar: AnsiChar;
begin
  Result := False;
  try
    if Assigned(FSpeciesNames) then
    begin
      i := FSpeciesNames.Count;
      BlockWrite(SessionFile, i, SizeOf(Integer));
      if i > 0 then
      for i := 0 to FSpeciesNames.Count - 1 do
      begin
        NameLength := Length(FSpeciesNames[i]);
        BlockWrite(SessionFile, NameLength, SizeOf(Integer));
        if NameLength > 0 then
          for j := 1 to NameLength do
          begin
            AChar := AnsiString(FSpeciesNames[i])[j];
            BlockWrite(SessionFile, AChar, SizeOf(AnsiChar));
          end;
      end;
    end
    else
    begin
      i := 0;
      BlockWrite(SessionFile, i, SizeOf(Integer));
    end;
    Result := True;
  except
    on E: Exception do
      raise E;
  end;
end;

function TTimeTreeList.ExportATreeToNewickFile(index: integer; filename :String; bl,st:boolean; CutOff: double): boolean;
var
  f : TextFile;
begin
  try try
    AssignFile(f, filename);
    ReWrite(f);
    result := WriteATreeToNewickFile(index, f, bl, st, CutOff);
  except
    result := false;
    exit;
  end;
  finally
    CloseFile(f);
  end;
end;

function TTimeTreeList.ExportToNewickFile(filename : String; bl,st:boolean; CutOff: double; MaxNumTrees: Integer):boolean;
var
  f : TextFile;
  SBL: double;
  i: integer;
  NumTreesToPrint: Integer;
  MyMaxNumTrees : Integer;
begin
  NumTreesToPrint := 1;
  try try
    AssignFile(f, filename);
    ReWrite(f);

    for i := 0 to NumTreesToPrint - 1 do
    begin
      result := WriteATreeToNewickFile(i, f, bl, st, CutOff);
      if not result then
        break;
    end;
    result := true;
  except
    result := false;
    exit;
  end;
  finally
    CloseFile(f);
  end;
end;

function TTimeTreeList.FindMrca(TaxonAName: AnsiString; TaxonBName: AnsiString
  ): Integer;
var
  TaxonAId, TaxonBId: Integer;
begin
  TaxonAId := NodeNameToId(TaxonAName);
  TaxonBId := NodeNameToId(TaxonBName);
  Result := Items[0].MRCA(TaxonAId, TaxonBId);
end;

function TTimeTreeList.ToStringList: TStringList;
var
  i: Integer;
  j: Integer;
  TempStringList: TStringList;
begin
  Result := TStringList.Create;
  TempStringList := nil;

  Result.Add(#9 + 'Info');
  Result.Add('[isRooted]=' + BoolToStr(isRooted, True));
  Result.Add('[isBLen]=' + BoolToStr(isBLen, True));
  Result.Add('[isSE]=' + BoolToStr(isSE, True));
  Result.Add('[isStats]=' + BoolToStr(isStats, True));
  Result.Add('[NoOfTrees]=' + IntToStr(NoOfTrees));
  Result.Add('[NoOfOTUs]=' + IntToStr(NoOfOTUs));
  Result.Add('[ValueName]=' + ValueName);
  Result.Add('[Value2Name]=' + Value2Name);
  Result.Add('[FreqName]=' + FreqName);
  Result.Add('[StatsName]=' + StatsName);
  Result.Add('[MaxStats]=' + FloatToStrF(MaxStats, ffFixed, 5, 5));
  Result.Add('[TotalFrequency]=' + FloatToStrF(TotalFrequency, ffFixed, 5, 5));

  Result.Add(' ');
  Result.Add(#9 + 'Trees');
  for i := 0 to NoOfTrees - 1 do
  begin
    TempStringList := TTimeTreeData(Items[i]).StateToStringList('');
    Result.Add('[TreeData:[=' + IntToStr(i) + ']');
    for j := 0 to TempStringList.Count - 1 do
      Result.Add(TempStringList[j]);
  end;

  Result.Add(' ');
  Result.Add(#9 + 'OTUNames');
  for i := 0 to FOTUName.Count - 1 do
    Result.Add(IntToStr(i) + ' = ' + FOTUName[i]);


  if FInternalNodeLbls.Count > 0 then
  begin
    Result.Add(' ');
    Result.Add(#9 + 'Internal Labels');
    for i := 0 to FInternalNodeLbls.Count - 1 do
      Result.Add(IntToStr(i) + ' = ' + FInternalNodeLbls[i]);
  end;

  if Information.Count > 0 then
  begin
    Result.Add(' ');
    Result.Add(#9 + 'Information');
    for i := 0 to Information.Count - 1 do
      Result.Add('[Information_' + IntToStr(i) + ']=' + Information[i]);
  end;

  if TempStringList <> nil then
    FreeAndNil(TempStringList);
end;

end.

