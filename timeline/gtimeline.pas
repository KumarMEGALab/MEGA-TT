unit gtimeline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gtimelineresult;

type

  { TTimeline }

  TTimeline = class(TObject)
    private
      FData: TList;
      FMaximum: Double;
      FMinimum: Double;
      function GetCount: Integer;
      function GetItem(Index: Integer): TTimelineResult;
      function GetKnownEventsCount: Integer;
      procedure ResolveAmbiguity(TimelineResult: TTimelineResult);
    public
      constructor Create;
      destructor Destroy; override;

      function GetNumVisibleNodes(UnknownEventsAreShown: Boolean): Integer;
      procedure Add(TimelineResult: TTimelineResult);
      procedure Clear;
      procedure Sort;
      procedure CheckUpdateAmbiguousNames;
      property Items[Index: Integer]: TTimelineResult read GetItem; default;
      property Count: Integer read GetCount;
      property KnownEventsCount: Integer read GetKnownEventsCount;
      property Minimum: Double read FMinimum;
      property Maximum: Double read FMaximum;
  end;

  function CompareTimelineResults(Item1: Pointer; Item2: Pointer): Integer;

implementation

uses
  math, gtaxonomicrank, ttconst;

function CompareTimelineResults(Item1: Pointer; Item2: Pointer): Integer;
var
  result1, result2: TTimelineResult;
begin
  result1 := TTimelineResult(Item1);
  result2 := TTimelineResult(Item2);
  Result := CompareValue(result1.BranchLength, result2.BranchLength, 0.00001);
  if Result = 0 then
    Result := CompareValue(ord(result1.TaxonomicRank), ord(result2.TaxonomicRank));

end;

{ TTimeline }

function TTimeline.GetCount: Integer;
begin
  Result := FData.Count;
end;

function TTimeline.GetItem(Index: Integer): TTimelineResult;
begin
  Assert((Index >= 0) and (FData.Count > 0) and (Index < FData.Count));
  Result := TTimelineResult(FData[Index]);
end;

function TTimeline.GetKnownEventsCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FData.Count > 0 then
    for i := 0 to FData.Count - 1 do
      if not TTimelineResult(FData[i]).IsUnknownEvent then
        inc(Result);
end;

procedure TTimeline.ResolveAmbiguity(TimelineResult: TTimelineResult);
var
  i: Integer;
  aResult: TTimelineResult;
  rankStr: String;
begin
  for i := 0 to Count - 1 do
  begin
    aResult := GetItem(i);
    if LowerCase(aResult.ScientificName) = UNKNOWN_TAXON_NAME then
      continue;
    if (aResult <> TimelineResult) and (aResult.ScientificName = TimelineResult.ScientificName) and (not aResult.IsAmbiguous) then
    begin
      rankStr := TaxonomicRankToString(TimelineResult.TaxonomicRank);
      TimelineResult.ScientificName := TimelineResult.ScientificName + ' (' + rankStr + ') ';
      TimelineResult.IsAmbiguous := True;
      rankStr := TaxonomicRankToString(aResult.TaxonomicRank);
      aResult.ScientificName := aResult.ScientificName + ' (' + rankStr + ') ';
      aResult.IsAmbiguous := True;
      Exit;
    end;
  end;
end;

constructor TTimeline.Create;
begin
  FData := TList.Create;
  FMinimum := 0.0;
  FMaximum := 0.0;
end;

destructor TTimeline.Destroy;
begin
  Clear;
  FData.Free;
  inherited Destroy;
end;

function TTimeline.GetNumVisibleNodes(UnknownEventsAreShown: Boolean): Integer;
begin
  if UnknownEventsAreShown then
    Result := FData.Count
  else
    Result := KnownEventsCount;
end;

procedure TTimeline.Add(TimelineResult: TTimelineResult);
var
  aResult: TTimelineResult;
begin
  aResult := TTimelineResult.Create;
  aResult.Assign(TimelineResult);
  FData.Add(aResult);
  if Count = 1 then
  begin
    FMinimum := aResult.BranchLength;
    FMaximum := aResult.BranchLength
  end
  else
  begin
    if aResult.BranchLength < FMinimum then
      FMinimum := aResult.BranchLength;
    if aResult.BranchLength > FMaximum then
      FMaximum := aResult.BranchLength;
  end;
end;

procedure TTimeline.Clear;
var
  i: Integer;
begin
  if Assigned(FData) and (FData.Count > 0) then
    for i := FData.Count - 1 downto 0 do
      TTimelineResult(FData[i]).Free;
  FData.Clear;
  FMinimum := 0.0;
  FMaximum := 0.0;
end;

procedure TTimeline.Sort;
begin
  //FData.Sort(@CompareTimelineResults);
end;

procedure TTimeline.CheckUpdateAmbiguousNames;
var
  i: Integer;
  TimelineResult: TTimelineResult;
  rankStr: String;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      TimelineResult := GetItem(i);
      ResolveAmbiguity(TimelineResult);
    end;
end;

end.

