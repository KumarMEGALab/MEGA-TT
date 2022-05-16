unit gtreenode;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, gtreeattrib, ttconst, gtaxonomicrank;

type

  TBranch = record
      length : double;
      SE : double;
      maxlen1, maxlen2 : double;
      bootnum : integer;
      stats : double;
      stat2 : integer;
  end;

  TpNode  = ^TNode;
  TNode   = record
      index : integer;
      anc : TpNode;
      des1, des2 : TpNode;
      size : integer;
      cursize : double;
      depth : integer;
      width : integer;
      height : double;
      h0     : double;
      vh     : double;
      maxh: double;
      minh: double;
      rate: double;

      branch : TBranch;
      minOTU : integer;
      position : TPoint;
      angle : double;
      sector: double;
      avglen: double;
      name : AnsiString;
      PrivateName: AnsiString;
      SpeciesName: AnsiString;
      oriName : AnsiString;
      marker : TNodeMarker;
      charstate : AnsiString;
      OTU : boolean;
      compressed : boolean;
      outgroup: boolean;
      hidden : boolean;
      flag: boolean;
      hilighted: boolean;
      emphasized: boolean; // then draw with emphasis
      //CustomHighlightColor: TColor; // If highlighted color differs from normal highlight color
      attrindex: integer;
      namesize: TPoint;
      speciesNameSize: TPoint;
      dx: integer;
      groupindex: integer;
      bracket: TRect;
      capdepth: integer;
      IsGeneDuplication: Boolean;
      IsSpeciationEvent: Boolean;
      dataCoverage: Double;
      timetreeId: Integer; { the node ID provided by timetree.org, used for MEGA-TT}
      ciLower: Double;
      ciUpper: Double;
      IsCI: Boolean;
      rank: TTaxonomicRank;
      putativeRank: TTaxonomicRank;
      ranksCount: PRanksCount;
      numLeaves: Integer;
      adjustedAge: Double;
      precomputedAge: Double;
      ciString: String;
  end;
  TpNodeArray = array[1..(2147483647 div SizeOf(TpNode))] of TpNode; {max size of a data structure is 2GB}

  function HasDescendentDeeperThanRank(aNode: TpNode; aRank: TTaxonomicRank): Boolean; { for a node whose rank is unknown, evaluates if the node has descendents less specific or equal to aRank }
  function NodeIsWithinRankRestriction(aNode: TpNode; aRank: TTaxonomicRank): Boolean;
  function NodeInfoAsCSV(aNode: TpNode): String;
  function NodeInfoCSVHeader: String;

implementation

uses
  gnamesmap;

function HasDescendentDeeperThanRank(aNode: TpNode; aRank: TTaxonomicRank): Boolean;
var
  TempNode: TpNode;

  function ProcessNode(descendent: TpNode): Boolean;
  begin
    Result := False;
    if (descendent.rank = aRank) or ((descendent.rank <> trNoRank) and (descendent.rank <> trUnknown) and (descendent.rank <= aRank)) then
    begin
      Result := True;
      Exit;
    end;
    if Assigned(descendent.des1) then
      Result := ProcessNode(descendent.des1);
    if (not Result) and Assigned(descendent.des2) then
      Result := ProcessNode(descendent.des2);
  end;

begin
  Result := False;
  if (not Assigned(aNode.des1)) or (not Assigned(aNode.des2)) then
  begin
    Result := False;
    Exit;
  end;
  Result := ProcessNode(aNode.des1);
  if not Result then
    Result := ProcessNode(aNode.des2);
end;

function NodeIsWithinRankRestriction(aNode: TpNode; aRank: TTaxonomicRank): Boolean;
begin
  if (aNode.rank <> trNoRank) and (aNode.rank <> trUnknown) then
    Result := (aNode.rank <= aRank)
  else
    Result := False;
end;

function NodeInfoAsCSV(aNode: TpNode): String;
var
  ttId: String;
  aName: String;
begin
  ttId := IntToStr(aNode.timetreeId);
  if NamesMap.Contains(ttId) then
    aName := TOtuName(NamesMap[ttId]).Name
  else
    aName := 'unnamed';
  Result := Format('%s,%d,%s,%d,', [aName, aNode.timetreeId, TaxonomicRankToString(aNode.Rank), aNode.numLeaves]);
  Result := Result + BoolToStr(aNode.flag, True) + ',' + BoolToStr(aNode.OTU, True) + ',';
  if aNode.des1 <> nil then
    Result := Result + Format('%d,', [aNode.des1.timetreeId])
  else
    Result := Result + 'nil,';
  if aNode.des2 <> nil then
    Result := Result + Format('%d,', [aNode.des2.timetreeId])
  else
    Result := Result + 'nil,';
  if aNode.anc <> nil then
    Result := Result + Format('%d,', [aNode.anc.timetreeId])
  else
    Result := Result + 'nil,';
  Result := Result + Format('%.3e,%d,%s', [aNode.branch.length, aNode.index, aNode.name]);
end;

function NodeInfoCSVHeader: String;
begin
  Result := 'name,ttid,rank,num_leaves,flag,is_otu,des1,des2,anc,blen,index,name';
end;

end.

