unit gtimescalerenderer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
 {$M+}

interface

uses
  Classes, SysUtils, geologicaltimes, gsvgstrings, ttconst, gscaleticks;

type

  { TTimescaleRenderer }

  TTimescaleRenderer = class(TObject)
    private
      FScalingFactor: Integer;
      FNumTimeScaleTicks: Integer;
      FSvgStrings: TStringList;
      FIsLogScale: Boolean;
      FLinePoints: array[0..1] of TPoint;
      FBgAttribs: array[0..7] of TXmlAttribute;
      FTextAttribs: array[0..1] of TXmlAttribute;
      FBorderAttribs: array[0..2] of TXmlAttribute;
      FLineAttribs: array[0..3] of TXmlAttribute;
      FDrawingArea: TRect;
      FMargins: TRect;
      FScaleTicks: array of TScaleTick;
      FMaxTime: Double;
      FMinTime: Double;
      FGeologicTimes: TArrayOfGeologicTime;
      FTickHeight: Integer;
      FTickIncrement: Double;
      procedure InitBorderAttribs;
      procedure InitBgAttribs;
      procedure InitTextAttribs;
      procedure InitLineAttribs;
      procedure InitLinearScaleTicks;
      procedure InitLogScaleTicks;
      procedure FinalizeScaleTicks;
      procedure InitMargins;
      procedure SetIsLogScale(AValue: Boolean);
      procedure DrawBackground;
      procedure DrawScaleTicks;
      procedure DrawHorizLine;
      procedure DrawTextForTicks;
      procedure FindBestScaleTickIncrement;
      function FindBestLogScaleBase: Integer;
      procedure SetNumTimeScaleTicks(AValue: Integer);
      procedure SetTickHeight(AValue: Integer);
    public
      MapMyaToCoordsFunc: TMapMyaToCoordsFunc;
      constructor Create;
      destructor Destroy; override;

      function Render(aRect: TRect; maxTime: Double; times: TArrayOfGeologicTime; scalingFactor: Integer = 0): TStringList;

      property IsLogScale: Boolean read FIsLogScale write SetIsLogScale;
      property NumTimeScaleTicks: Integer read FNumTimeScaleTicks write SetNumTimeScaleTicks;
      property TickHeight: Integer read FTickHeight write SetTickHeight;
  end;

implementation

uses
  math, gtextwidth;

{ TTimescaleRenderer }

procedure TTimescaleRenderer.InitBorderAttribs;
begin
  FBorderAttribs[0].Name := 'fill';
  FBorderAttribs[0].Value := 'none';
  FBorderAttribs[1].Name := 'stroke';
  FBorderAttribs[1].Value := '#555';
  FBorderAttribs[2].Name := 'stroke-width';
  FBorderAttribs[2].Value := '1';
end;

procedure TTimescaleRenderer.InitBgAttribs;
begin
  FBgAttribs[0].Name := 'fill';
  FBgAttribs[0].Value := 'none';
  FBgAttribs[1].Name := 'stroke';
  FBgAttribs[1].Value := '#555555';
  FBgAttribs[2].Name := 'stroke-width';
  FBgAttribs[2].Value := '1';
  FBgAttribs[3].Name := 'full-name';
  FBgAttribs[4].Name := 'start';
  FBgAttribs[5].Name := 'end';
  FBgAttribs[6].Name := 'unit';
  FBgAttribs[7].Name := 'title';
end;

procedure TTimescaleRenderer.InitTextAttribs;
begin
  FTextAttribs[0].Name := 'fill';
  FTextAttribs[0].Value := '#555555';
  FTextAttribs[1].Name := 'font-size';
  FTextAttribs[1].Value := '12';
end;

procedure TTimescaleRenderer.InitLineAttribs;
begin
  FLineAttribs[0].Name := 'fill';
  FLineAttribs[0].Value := 'none';
  FLineAttribs[1].Name := 'stroke';
  FLineAttribs[1].Value := '#333';
  FLineAttribs[2].Name := 'stroke-width';
  FLineAttribs[2].Value := '1';
  FLineAttribs[3].Name := 'stroke-linecap';
  FLineAttribs[3].Value := 'square';
end;

procedure TTimescaleRenderer.InitLinearScaleTicks;
var
  i: Integer;
begin
  FindBestScaleTickIncrement;
  SetLength(FScaleTicks, FNumTimeScaleTicks + 2);
  for i := 0 to Length(FScaleTicks) - 1 do
    FScaleTicks[i] := TScaleTick.Create;
  FScaleTicks[0].Time := FMinTime;
  FScaleTicks[Length(FScaleTicks) - 1].Time := FMaxTime;
  for i := 1 to Length(FScaleTicks) - 2 do
    FScaleTicks[i].Time := Min(FMaxTime, FScaleTicks[i - 1].Time + FTickIncrement);

  for i := 0 to Length(FScaleTicks) - 1 do
  begin
    FScaleTicks[i].Points[0].Y := FDrawingArea.Top + FMargins.Top;
    FScaleTicks[i].Points[1].Y := FScaleTicks[i].Points[0].Y + FTickHeight;
    FScaleTicks[i].Points[0].X := MapMyaToCoordsFunc(FScaleTicks[i].Time);
    FScaleTicks[i].Points[1].X := FScaleTicks[i].Points[0].X;
  end;
  FinalizeScaleTicks;
end;

procedure TTimescaleRenderer.InitLogScaleTicks;
var
  i: Integer;
  TickValue: Double;
  aTick: TScaleTick;
  BaseValue: Integer;
begin
  BaseValue := FindBestLogScaleBase;
  SetLength(FScaleTicks, 1);
  FScaleTicks[0] := TScaleTick.Create(True);
  FScaleTicks[0].Time := FMinTime;
  TickValue := 0.0;
  i := 0;
  while TickValue < FMaxTime do
  begin
    TickValue := power(BaseValue, i);
    inc(i);
    SetLength(FScaleTicks, Length(FScaleTicks) + 1);
    aTick := TScaleTick.Create(True);
    aTick.Time := TickValue;
    FScaleTicks[Length(FScaleTicks) - 1] := aTick;
  end;

  SetLength(FScaleTicks, Length(FScaleTicks) + 1);
  aTick := TScaleTick.Create(True);
  aTick.Time := FMaxTime;
  FScaleTicks[Length(FScaleTicks) - 1] := aTick;

  for i := 0 to Length(FScaleTicks) - 1 do
  begin
    FScaleTicks[i].Points[0].Y := FDrawingArea.Top + FMargins.Top;
    FScaleTicks[i].Points[1].Y := FScaleTicks[i].Points[0].Y + FTickHeight;
    FScaleTicks[i].Points[0].X := MapMyaToCoordsFunc(FScaleTicks[i].Time);
    FScaleTicks[i].Points[1].X := FScaleTicks[i].Points[0].X;
  end;
  FScaleTicks[Length(FScaleTicks) -1].Points[0].X := FDrawingArea.Left;
  FScaleTicks[Length(FScaleTicks) -1].Points[1].X := FScaleTicks[Length(FScaleTicks) -1].Points[0].X;
  FinalizeScaleTicks;
end;

procedure TTimescaleRenderer.FinalizeScaleTicks;
var
  FirstTick, SecondTick, LastTick, SecondToLastTick: TScaleTick;
  DistanceBetween, i: Integer;
begin
  if Length(FScaleTicks) < 3 then
    Exit;
  LastTick := FScaleTicks[Length(FScaleTicks) - 1];
  SecondToLastTick := FScaleTicks[Length(FScaleTicks) -2];
  DistanceBetween := abs(LastTick.Points[0].X - SecondToLastTick.Points[0].X);
  if CustomTextWidth(SecondToLastTick.TimeText) >= DistanceBetween then
  begin
    SecondToLastTick.Free;
    FScaleTicks[Length(FScaleTicks) - 2] := LastTick;
    SetLength(FScaleTicks, Length(FScaleTicks) - 1);
  end;

  FirstTick := FScaleTicks[0];
  SecondTick := FScaleTicks[1];
  DistanceBetween := abs(FirstTick.Points[0].X - SecondTick.Points[0].X);
  if CustomTextWidth(SecondTick.TimeText) >= DistanceBetween then
  begin
    SecondTick.Free;
    for i := 2 to Length(FScaleTicks) - 1 do
      FScaleTicks[i - 1] := FScaleTicks[i];
    SetLength(FScaleTicks, Length(FScaleTicks) - 1);
  end;
end;

procedure TTimescaleRenderer.InitMargins;
begin
  FMargins.Top := 5;
  FMargins.Left := 0;
  FMargins.Bottom := 0;
  FMargins.Right := 0;
end;

procedure TTimescaleRenderer.SetIsLogScale(AValue: Boolean);
begin
  if FIsLogScale=AValue then Exit;
  FIsLogScale:=AValue;
end;

procedure TTimescaleRenderer.DrawBackground;
var
  mya: Double;
  i: Integer;
  Temp: String;
  aRect: TRect;
begin
  if Length(FGeologicTimes) > 0 then
  begin
    { draw the border}
    aRect.Top := FDrawingArea.Top + FMargins.Top;
    aRect.Bottom := aRect.Top + FTickHeight;
    aRect.Left := MapMyaToCoordsFunc(FGeologicTimes[Length(FGeologicTimes) - 1].StartMya)-1;
    aRect.Right := MapMyaToCoordsFunc(FMinTime);
    Temp := RectToSvgRect(aRect, FBorderAttribs);
    FSvgStrings.Add(Temp);


    aRect.Bottom := aRect.Bottom - 1;
    aRect.Top := aRect.Top + 1;

    { draw the rectangles}
    for i := Length(FGeologicTimes) - 1 downto 0 do
    begin
      aRect.Left := MapMyaToCoordsFunc(FGeologicTimes[i].StartMya);
      if i = 0 then
        aRect.Right := MapMyaToCoordsFunc(0)
      else
        aRect.Right := MapMyaToCoordsFunc(FGeologicTimes[i].EndMya);

      FBgAttribs[0].Value := FGeologicTimes[i].HexaDecimalColorStr;
      FBgAttribs[1].Value := FGeologicTimes[i].HexaDecimalColorStr;
      FBgAttribs[2].Value := '3';
      FBgAttribs[3].Value := FGeologicTimes[i].Name;
      FBgAttribs[4].Value := Format('%.2f', [FGeologicTimes[i].StartMya]);
      FBgAttribs[5].Value := Format('%.2f', [FGeologicTimes[i].EndMya]);
      FBgAttribs[6].Value := TimespanTypeString(FGeologicTimes[i].TimespanType);
      FBgAttribs[7].Value := FGeologicTimes[i].Name;
      Temp := RectToSvgRect(aRect, FBgAttribs);
      FSvgStrings.Add(Temp);
    end;

    { cover up the overflow on the right border due to the stroke width}
    aRect.Left := MapMyaToCoordsFunc(0);
    aRect.Right := aRect.Left + 3;
    aRect.Top := aRect.Top - 2;
    aRect.Bottom := aRect.Bottom + 2;
    FBorderAttribs[0].Value := 'white';
    FBorderAttribs[1].Value := 'white';
    FBorderAttribs[2].Value := '1';
    Temp := RectToSvgRect(aRect, FBorderAttribs);
    FSvgStrings.Add(Temp);
  end;
end;

procedure TTimescaleRenderer.DrawScaleTicks;
var
  i: Integer;
  Points: TArrayOfTPoint;
  SvgString: String;
begin
  if IsLogScale then
    InitLogScaleTicks
  else
   InitLinearScaleTicks;
  Assert(Length(FScaleTicks) > 1);
  for i := 0 to Length(FScaleTicks) - 1 do
  begin
    Points := FScaleTicks[i].Points;
    SvgString := PointsToSvgLine(Points, FLineAttribs);
    FSvgStrings.Add(SvgString);
  end;
end;

procedure TTimescaleRenderer.DrawHorizLine;
var
  SvgLine: String;
begin
  FLinePoints[0].X := FDrawingArea.Left;
  FLinePoints[0].Y := FDrawingArea.Top + FMargins.Top;
  FLinePoints[1].X := FDrawingArea.Right;
  FLinePoints[1].Y := FLinePoints[0].Y;
  SvgLine := PointsToSvgLine(FLinePoints, FLineAttribs);
  FSvgStrings.Add(SvgLine);
end;

procedure TTimescaleRenderer.DrawTextForTicks;
var
  i: Integer;
  SvgString: String;
  Points: TArrayOfTPoint;
  x, y: Integer;
  TimeText: String;
begin
  for i := 0 to Length(FScaleTicks) - 1 do
  begin
    Points := FScaleTicks[i].Points;
    TimeText := FScaleTicks[i].Timetext;
    x := Points[0].X - (CustomTextWidth(TimeText) div 3);
    y := Points[1].Y + FMargins.Top + CustomTextHeight(TimeText, 8);
    SvgString := TextToSvgText(x, y, TimeText, FTextAttribs, FScalingFactor);
    FSvgStrings.Add(SvgString);
  end;
end;

procedure TTimescaleRenderer.FindBestScaleTickIncrement;
var
  Range: Double;
begin
  Assert(CompareValue(FMaxTime, 0.0, 0.0000000001) > 0);
  Range := FMaxTime - FMinTime;
  FTickIncrement := Range / FNumTimeScaleTicks;
  if FTickIncrement > 1000 then
    FTickIncrement := RoundTo(FTickIncrement, 3)
  else if FTickIncrement > 100 then
    FTickIncrement := RoundTo(FTickIncrement, 2)
  else if FTickIncrement > 10 then
    FTickIncrement := RoundTo(FTickIncrement, 1);
end;

function TTimescaleRenderer.FindBestLogScaleBase: Integer;
begin
  if FMaxTime > 1000 then
    Result := 10
  else if FMaxTime > 600 then
    Result := 5
  else
    Result := 2;
end;

procedure TTimescaleRenderer.SetNumTimeScaleTicks(AValue: Integer);
begin
  if FNumTimeScaleTicks=AValue then Exit;
  FNumTimeScaleTicks:=AValue;
end;

procedure TTimescaleRenderer.SetTickHeight(AValue: Integer);
begin
  if FTickHeight=AValue then Exit;
  FTickHeight:=AValue;
end;

constructor TTimescaleRenderer.Create;
begin
  FScalingFactor := 0;
  MapMyaToCoordsFunc := nil;
  FIsLogScale := False;
  FSvgStrings := TStringList.Create;
  InitMargins;
  InitBorderAttribs;
  InitBgAttribs;
  InitLineAttribs;
  InitTextAttribs;
  FMaxTime := -1.0;
  FMinTime := 0.0;
  FNumTimeScaleTicks := 4;
  FTickHeight := 15;
  SetLength(FScaleTicks, 0);
end;

destructor TTimescaleRenderer.Destroy;
begin
  if Assigned(FSvgStrings) then
    FSvgStrings.Free;
  SetLength(FScaleTicks, 0);
  inherited Destroy;
end;

function TTimescaleRenderer.Render(aRect: TRect; maxTime: Double; times: TArrayOfGeologicTime; scalingFactor: Integer = 0): TStringList;
begin
  FScalingFactor := scalingFactor;
  Assert(Assigned(MapMyaToCoordsFunc));
  Result := TStringList.Create;
  FSvgStrings.Clear;
  FDrawingArea := aRect;
  FMaxTime := maxTime;
  FGeologicTimes := times;
  //DrawBackground;
  DrawHorizLine;
  DrawScaleTicks;
  DrawTextForTicks;
  Result.Assign(FSvgStrings);
end;

end.

