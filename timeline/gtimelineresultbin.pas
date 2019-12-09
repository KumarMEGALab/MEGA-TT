unit gtimelineresultbin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gtimelineresult;

type

  { TTimelineResultBin }

  TTimelineResultBin = class(TObject)
    private
      FBottomCoord: Integer;
      FIndex: Integer;
      FLeftCoord: Integer;
      FRightCoord: Integer;
      FTimelineResult: TTimelineResult;
      FTopCoord: Integer;
      procedure SetBottomCoord(AValue: Integer);
      procedure SetLeftCoord(AValue: Integer);
      procedure SetRightCoord(AValue: Integer);
      procedure SetTimelineResult(AValue: TTimelineResult);
      procedure SetTopCoord(AValue: Integer);
    public
      constructor Create(Index: Integer);
      destructor Destroy; override;

      property TopCoord: Integer read FTopCoord write SetTopCoord;
      property BottomCoord: Integer read FBottomCoord write SetBottomCoord;
      property LeftCoord: Integer read FLeftCoord write SetLeftCoord;
      property RightCoord: Integer read FRightCoord write SetRightCoord;
      property TimelineResult: TTimelineResult read FTimelineResult write SetTimelineResult;
      property Index: Integer read FIndex;
  end;

implementation

{ TTimelineResultBin }

procedure TTimelineResultBin.SetBottomCoord(AValue: Integer);
begin
  if FBottomCoord=AValue then Exit;
  FBottomCoord:=AValue;
end;

procedure TTimelineResultBin.SetLeftCoord(AValue: Integer);
begin
  if FLeftCoord=AValue then Exit;
  FLeftCoord:=AValue;
end;

procedure TTimelineResultBin.SetRightCoord(AValue: Integer);
begin
  if FRightCoord=AValue then Exit;
  FRightCoord:=AValue;
end;

procedure TTimelineResultBin.SetTimelineResult(AValue: TTimelineResult);
begin
  if FTimelineResult=AValue then Exit;
  FTimelineResult:=AValue;
end;

procedure TTimelineResultBin.SetTopCoord(AValue: Integer);
begin
  if FTopCoord=AValue then Exit;
  FTopCoord:=AValue;
end;

constructor TTimelineResultBin.Create(Index: Integer);
begin
  FIndex := Index;
end;

destructor TTimelineResultBin.Destroy;
begin
  inherited Destroy;
end;

end.

