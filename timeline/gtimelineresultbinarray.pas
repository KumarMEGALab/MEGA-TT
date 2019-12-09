unit gtimelineresultbinarray;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gtimelineresultbin, gtimelineresult, ttconst;

type

  { TTimelineResultBinArray }

  TTimelineResultBinArray = class(TObject)
    private
      FBins: array of TTimelineResultBin;
      FBinHeight: Integer;
      FDimensions: TRect;
      FNumBins: Integer;
      FNumResults: Integer;
      FCurrentPosition: Integer;
      function GetNumBins: Integer;
      procedure InitializeBins;
      procedure MoveResult(Target: Integer; Destination: Integer);
      function BinIsEmpty(Index: Integer): Boolean;
      function IsLastEmptyBin(Index: Integer): Boolean;
      procedure Finalize;
    public
      YCoordsMappingFunc: TMapMyaToCoordsFunc;
      constructor Create(Dimensions: TRect; BinHeight: Integer);
      destructor Destroy; override;

      procedure Add(aResult: TTimelineResult);
      function MoveResultsToBestFitBins: Boolean;
      property NumBins: Integer read GetNumBins;
  end;

implementation

{ TTimelineResultBinArray }

procedure TTimelineResultBinArray.InitializeBins;
var
  i: Integer;
  y: Integer;
begin
  FNumBins := ((FDimensions.Bottom - FDimensions.Top) div FBinHeight);
  SetLength(FBins, FNumBins);
  if FNumBins > 0 then
  begin
    y := FDimensions.Top;
    for i := 0 to FNumBins - 1 do
    begin
      FBins[i] := TTimelineResultBin.Create(i);
      FBins[i].TopCoord := y;
      y := y + FBinHeight;
      FBins[i].BottomCoord := y;
      FBins[i].LeftCoord := FDimensions.Left;
      FBins[i].RightCoord := FDimensions.Right;
      FBins[i].TimelineResult := nil;
    end;
  end;
end;

function TTimelineResultBinArray.GetNumBins: Integer;
begin
  Result := Length(FBins);
end;

procedure TTimelineResultBinArray.MoveResult(Target: Integer; Destination: Integer);
var
  aResult: TTimelineResult;
begin
  Assert(Assigned(FBins[Target].TimelineResult));
  aResult := FBins[Target].TimelineResult;
  FBins[Target].TimelineResult := nil;
  FBins[Destination].TimelineResult := aResult;
end;

function TTimelineResultBinArray.BinIsEmpty(Index: Integer): Boolean;
begin
  Result := (FBins[Index].TimelineResult = nil);
end;

function TTimelineResultBinArray.IsLastEmptyBin(Index: Integer): Boolean;
var
  i: Integer;
begin
  if not BinIsEmpty(Index) then
  begin
    Result := False;
    Exit;
  end;

  if Index = (FNumBins - 1) then
  begin
    Result := True;
    Exit;
  end;

  if BinIsEmpty(Index) and (Index < (FNumBins - 1)) and (not BinIsEmpty(Index + 1)) then
  begin
    Result := True;
    Exit;
  end;

  for i := Index to FNumBins - 1 do
  begin
    if BinIsEmpty(i) and (i < (FNumBins - 1)) and (BinIsEmpty(i + 1)) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TTimelineResultBinArray.Finalize;
var
  i: Integer;
  aResult: TTimelineResult;
begin
  for i := 0 to FNumBins - 1 do
  begin
    if Assigned(FBins[i].TimelineResult) then
    begin
      aResult := FBins[i].TimelineResult;
      aResult.FigurineYCoord := FBins[i].TopCoord - 7;
      aResult.LineYCoord := FBins[i].TopCoord + Round((FBins[i].BottomCoord - FBins[i].TopCoord) / 2) - 2*NAME_BOX_PADDING;
      aResult.NameYCoord := FBins[i].TopCoord;
      aResult.TimeTextYCoord := FBins[i].TopCoord;
    end;
  end;
end;

constructor TTimelineResultBinArray.Create(Dimensions: TRect; BinHeight: Integer);
begin
  FDimensions := Dimensions;
  FBinHeight := BinHeight;
  FCurrentPosition := 0;
  FNumResults := 0;
  InitializeBins;
end;

destructor TTimelineResultBinArray.Destroy;
begin
  inherited Destroy;
end;

procedure TTimelineResultBinArray.Add(aResult: TTimelineResult);
begin
  FBins[FCurrentPosition].TimelineResult := aResult;
  inc(FCurrentPosition);
  inc(FNumResults);
end;

function TTimelineResultBinArray.MoveResultsToBestFitBins: Boolean;
var
  i, j, y: Integer;
  aResult: TTimelineResult;
begin
  Result := False;
  Assert(Assigned(YCoordsMappingFunc));
  for i := FNumResults - 1 downto 0 do
  begin
    aResult := FBins[i].TimelineResult;
    y := YCoordsMappingFunc(aResult.BranchLength);
    if (y > FBins[i].BottomCoord) and (i < FNumBins) then
    begin
      for j := (i + 1) to FNumBins - 1 do
      begin
        if not BinIsEmpty(j) then
          break;
        if (y <= FBins[j].TopCoord) or IsLastEmptyBin(j) then
        begin
          if BinIsEmpty(j) then
            MoveResult(i, j);
          break;
        end;
      end;
    end;
  end;
  Result := True;
  Finalize;
end;

end.

