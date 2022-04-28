unit gtimelinesvgwriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gtimeline, geologicaltimes, ttconst, mearthimpactsrenderer, mearthimpacts,
  mgeodata, mgeodatachartformatter;

type

  { TTimelineSvgWriter }

  TTimelineSvgWriter = class(TObject)

    private
      FChartFormatter: TGeoDataChartFormatter;
      FDrawUnknownNodes: Boolean;
      FO2Data: TGeoData;
      FCO2Data: TGeoData;
      FLuminosityData: TGeoData;
      FCO2File: String;
      FImpactsFile: String;
      FEarthImpacts: TEarthImpacts;
      FGeoScale: TCompositeGeologicTime;
      FMargins: TRect;
      FName: String;
      FO2File: String;
      FSvgStrings: TStringList;
      FTimeline: TTimeline;
      FHeight: Integer;
      FWidth: Integer;
      FFontHeight: Integer;
      FTimescaleFontHeight: Integer;
      FGeoScaleRectAttribs: array[0..6] of TXMLAttribute;
      FGeoScaleFontAttribs: array[0..6] of TXMLAttribute;
      FTimescaleLineAttribs: array[0..2] of TXMLAttribute;
      FTimescaleFontAttribs: array[0..2] of TXMLAttribute;
      FResultCircleAttribs: array[0..11] of TXMLAttribute;
      FTimescaleTicks: TArrayOfTPoint;
      FNamesFontAttribs: array[0..3] of TXMLAttribute;
      FNamesBoxAttribs: array of TXMLAttribute;
      FUnnamedNodesBoxAttribs: array of TXMLAttribute;
      FLineAttribs: array[0..3] of TXMLAttribute;
      FUsedFigurines: TStringList;
      function MyaToYCoord(mya: Double): Integer;
      procedure SetCO2File(AValue: String);
      procedure SetDrawUnknownNodes(AValue: Boolean);
      procedure SetImpactsFile(AValue: String);
      procedure SetO2File(AValue: String);
      function YCoordToMya(y: Integer): Double;
      function GeoPanelCoords(aType: TTimespanType): TRect;
      function EarthImpactsCoords: TRect;
      function O2PanelCoords: TRect;
      function CO2PanelCoords: TRect;
      function LuminosityPanelCoords: TRect;
      function TimescaleCoords: TRect;
      function ResultsCoords: TRect;
      function PhanerozoicCoords: TRect;
      function AfterPhanerozoicCoords: TRect;
      procedure OpenSvg; overload;
      procedure OpenSvg(const aName: String; const x: Integer; const y: Integer; const aWidth: Integer; const aHeight: Integer; const id: String=''); overload;
      procedure CloseSvg;
      procedure DrawEarthImpacts;
      procedure DrawEarthImpactsScale(aRect: TRect);
      procedure DrawO2;
      procedure DrawCO2;
      procedure DrawLuminosity;
      procedure DrawGeoDataPanelAreaChart(aColor: String);
      procedure DrawGeoDataPanelScale(aData: TGeoData; aRect: TRect);
      procedure DrawGeologicTimescale;
      procedure DrawGeologicLevel(aType: TTimespanType);
      procedure DrawBorder;
      procedure DrawBackground;
      procedure DrawTimeScale;
      procedure DrawPoints;
      procedure DrawNames;
      procedure AddGradientDefs;
      procedure DrawTimeText;
      procedure DrawLines;
      procedure DrawFigurines;
      procedure InitDefaultMargins;
      procedure InitGeoScaleRectAttribs;
      procedure InitGeoScaleFontAttribs;
      procedure InitTimescaleLineAttribs;
      procedure InitTimescaleFontAttribs;
      procedure InitNamesBoxAttribs;
      procedure InitUnnamedNodesBoxAttribs;
      procedure InitNamesFontAttribs;
      procedure InitTimescaleTicks;
      procedure InitLineAttribs;
      procedure InitChartFormatter;
      function CalculateNumTimescaleTicks: Integer;
      procedure SetHeight(AValue: Integer);
      procedure SetWidth(AValue: Integer);
      procedure AddDefinitions;
      function UpdateCoordsForTimelineResultsStatic: Boolean;
      //function UpdateCoordsForTimelineResultsDynamic: Boolean;
      function UpdateCoordsForTimelineUsingBins: Boolean;
      function WidthOfLongestName: Integer;
      procedure AdjustHeightIfNeeded;
      function LoadEarthImpacts: Boolean;
      function LoadClimateData: Boolean;
    public
      constructor Create;
      destructor Destroy; override;

      function GenerateSvg(Timeline: TTimeline; Dimensions: TPoint): String;
      property Name: String read FName;
      property Margins: TRect read FMargins write FMargins;
      property Width: Integer read FWidth write SetWidth;
      property Height: Integer read FHeight write SetHeight;
      property ImpactsFile: String read FImpactsFile write SetImpactsFile;
      property CO2File: String read FCO2File write SetCO2File;
      property O2File: String read FO2File write SetO2File;
      property DrawUnknownNodes: Boolean read FDrawUnknownNodes write SetDrawUnknownNodes;
  end;

implementation

uses
  testconsts, gsvgstrings, math, gtimelineresult, gtextwidth, gtimelineresultbinarray,
  gtaxonomicrank, gutils;

{ TTimelineSvgWriter }

function TTimelineSvgWriter.MyaToYCoord(mya: Double): Integer;
var
  aRect: TRect;
begin
  Assert((mya >= MIN_MYA) and (mya <= MAX_MYA));
  if mya < START_OF_PHANEROZOIC then { the Phanerozoic eon should take up most of the vertical space}
  begin
    aRect := PhanerozoicCoords;
    Result := Min(aRect.Bottom, FMargins.Top + Round(mya / START_OF_PHANEROZOIC * (aRect.Bottom - aRect.Top)));
  end
  else { after the Phanerozoic eon we have many less evolutionary steps so compress it to use the last bit of vertical space}
  begin
    aRect := AfterPhanerozoicCoords;
    Result := Min(FHeight - FMargins.Bottom, aRect.Top + Round(mya / MAX_MYA * (aRect.Bottom - aRect.Top)));
  end;
end;

procedure TTimelineSvgWriter.SetCO2File(AValue: String);
begin
  if FCO2File=AValue then Exit;
  FCO2File:=AValue;
end;

procedure TTimelineSvgWriter.SetDrawUnknownNodes(AValue: Boolean);
begin
  if FDrawUnknownNodes=AValue then Exit;
  FDrawUnknownNodes:=AValue;
end;

procedure TTimelineSvgWriter.SetImpactsFile(AValue: String);
begin
  if FImpactsFile=AValue then Exit;
  FImpactsFile:=AValue;
end;

procedure TTimelineSvgWriter.SetO2File(AValue: String);
begin
  if FO2File=AValue then Exit;
  FO2File:=AValue;
end;

function TTimelineSvgWriter.YCoordToMya(y: Integer): Double;
var
  aRect: TRect;
  ratio: Double;
begin
  Assert((y >= FMargins.Top) and (y < (Height - FMargins.Bottom)));
  aRect := PhanerozoicCoords;
  if y <= aRect.Bottom then
  begin
    ratio := (y / (aRect.Bottom - aRect.Top));
    Result := START_OF_PHANEROZOIC * Ratio;
  end
  else
  begin
    aRect := AfterPhanerozoicCoords;
    ratio := ((y - aRect.Top) / (aRect.Bottom - aRect.Top));
    Result := ratio * (MAX_MYA - START_OF_PHANEROZOIC);
  end;
end;

function TTimelineSvgWriter.GeoPanelCoords(aType: TTimespanType): TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := (FHeight - FMargins.Bottom);

  case aType of
    tstEon:
      begin
        Result.Left := GEOLOGIC_PANEL_WIDTH * 8 + FMargins.Left*2;
      end;
    tstEra:
      begin
        Result.Left := GEOLOGIC_PANEL_WIDTH * 9 + FMargins.Left*2;
      end;
    tstPeriod:
      begin
        Result.Left := GEOLOGIC_PANEL_WIDTH * 10 + FMargins.Left*2;
      end;
    tstEpoch, tstAge: raise Exception.Create('drawing of epochs and ages is not supported for the timeline search'); // Result.Right := Result.Left + CustomTextWidth(FGeoScale.GetLongestName(MIN_MYA, MAX_MYA, aType), 6);
  end;
  Result.Right := Result.Left + GEOLOGIC_PANEL_WIDTH;
  //case aType of
  //  tstEon, tstEra, tstPeriod: Result.Right := Result.Left + GEOLOGIC_PANEL_WIDTH * 2;
  //  tstEpoch, tstAge: raise Exception.Create('drawing of epochs and ages is not supported for the timeline search'); // Result.Right := Result.Left + CustomTextWidth(FGeoScale.GetLongestName(MIN_MYA, MAX_MYA, aType), 6);
  //end;
end;

function TTimelineSvgWriter.EarthImpactsCoords: TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := FHeight - FMargins.Bottom;
  Result.Left := FMargins.Left + GEOLOGIC_PANEL_WIDTH*6;
  Result.Right := Result.Left + GEOLOGIC_PANEL_WIDTH*2;
end;

function TTimelineSvgWriter.O2PanelCoords: TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := FHeight - FMargins.Bottom;
  Result.Left := FMargins.Left + GEOLOGIC_PANEL_WIDTH*4;
  Result.Right := Result.Left + GEOLOGIC_PANEL_WIDTH*2;
end;

function TTimelineSvgWriter.CO2PanelCoords: TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := FHeight - FMargins.Bottom;
  Result.Left := FMargins.Left + GEOLOGIC_PANEL_WIDTH*2;
  Result.Right := Result.Left + GEOLOGIC_PANEL_WIDTH*2;
end;

function TTimelineSvgWriter.LuminosityPanelCoords: TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := FHeight - FMargins.Bottom;
  Result.Left := FMargins.Left;
  Result.Right := Result.Left + GEOLOGIC_PANEL_WIDTH*2;
end;

function TTimelineSvgWriter.TimescaleCoords: TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := FHeight - FMargins.Bottom;
  Result.Left := FMargins.Left + GEOLOGIC_PANEL_WIDTH*10;
  Result.Right := Result.Left + GEOLOGIC_PANEL_WIDTH*2;
end;

function TTimelineSvgWriter.ResultsCoords: TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := FHeight - FMargins.Bottom;
  Result.Left := FMargins.Left + GEOLOGIC_PANEL_WIDTH*12;
  Result.Right := FWidth - FMargins.Right;
end;

function TTimelineSvgWriter.PhanerozoicCoords: TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := Round((FHeight - FMargins.Bottom)*VERT_SPACE_FOR_PHANEROZOIC);
end;

function TTimelineSvgWriter.AfterPhanerozoicCoords: TRect;
begin
  Result.Top := Round((FHeight - FMargins.Bottom)*VERT_SPACE_FOR_PHANEROZOIC);
  Result.Bottom := (FHeight - FMargins.Bottom);
end;

procedure TTimelineSvgWriter.OpenSvg;
var
  Temp: String;
begin
  Temp := '<svg xmlns=' + DBLQ + 'http://www.w3.org/2000/svg' + DBLQ + ' xmlns:xlink=' + DBLQ + 'http://www.w3.org/1999/xlink' + DBLQ + ' version=' + DBLQ + '1.1' + DBLQ + ' ';
  Temp := Temp + 'width=' + DBLQ + IntToStr(FWidth) + DBLQ + ' height=' + DBLQ + IntToStr(FHeight) + DBLQ + ' ';
  Temp := Temp + 'name=' + dblq + FName + dblq + ' ';
  Temp := Temp + 'x=' + dblq + IntToStr(0) + dblq + ' ';
  Temp := Temp + 'y=' + dblq + IntToStr(0) + dblq + ' ';
  Temp := Temp + 'viewBox=' + DBLQ + '0 0 ' + IntToStr(FWidth) + ' ' + IntToStr(FHeight) + DBLQ + '>';
  FSvgStrings.Add(Temp);
end;

procedure TTimelineSvgWriter.OpenSvg(const aName: String; const x: Integer; const y: Integer; const aWidth: Integer; const aHeight: Integer; const id: String='');
var
  Temp: String;
begin
  Temp := '<svg xmlns=' + DBLQ + 'http://www.w3.org/2000/svg' + DBLQ + ' xmlns:xlink=' + DBLQ + 'http://www.w3.org/1999/xlink' + DBLQ + ' version=' + DBLQ + '1.1' + DBLQ + ' ';
  Temp := Temp + 'width=' + DBLQ + IntToStr(aWidth) + DBLQ + ' height=' + DBLQ + IntToStr(aHeight) + DBLQ + ' ';
  Temp := Temp + 'name=' + dblq + aName + dblq + ' ';
  if id <> EmptyStr then
    Temp := Temp + 'id=' + dblq + id + dblq + ' ';
  Temp := Temp + 'x=' + dblq + IntToStr(x) + dblq + ' ';
  Temp := Temp + 'y=' + dblq + IntToStr(y) + dblq + ' ';
  Temp := Temp + 'viewBox=' + DBLQ + '0 0 ' + IntToStr(aWidth) + ' ' + IntToStr(aHeight) + DBLQ + '>';
  FSvgStrings.Add(Temp);
end;

procedure TTimelineSvgWriter.CloseSvg;
begin
  FSvgStrings.Add('</svg>');
end;

procedure TTimelineSvgWriter.DrawEarthImpacts;
var
  coords: TRect;
  scaleCoords: TRect;
  renderer: TEarthImpactsRenderer=nil;
  svgStrings: TStringList=nil;
begin
  coords := EarthImpactsCoords;
  if not LoadEarthImpacts then
    raise Exception.Create('failed to load earth impact structure data');
  renderer := TEarthImpactsRenderer.Create(FEarthImpacts.GetEarthImpacts(4600, 0));
  renderer.MapMyaToCoordsFunc := @MyaToYCoord;
  try
    OpenSvg('timeline-impacts', coords.Left, 0, coords.Right - coords.Left, FHeight, 'timeline-impacts');
    coords.Left := 2;
    coords.Right := GEOLOGIC_PANEL_WIDTH*2;
    svgStrings := renderer.Render(tVertical, coords);
    FSvgStrings.AddStrings(svgStrings);
    scaleCoords := Rect(0, coords.Bottom + FMargins.Left, coords.Right - coords.Left, coords.Bottom + FMargins.Bottom);
    DrawEarthImpactsScale(scaleCoords);
    CloseSvg;
  finally
    if Assigned(renderer) then
      renderer.Free;
    if Assigned(svgStrings) then
      svgStrings.Free;
  end;
end;

procedure TTimelineSvgWriter.DrawEarthImpactsScale(aRect: TRect);
const
  TICK_HEIGHT = 5;
var
  LinePoints: array[0..1] of TPoint;
  x,y: Integer;
  aText: String;
  tempStr: String;
begin
  { draw the left tick}
  LinePoints[0].X := aRect.Left + 1;
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X := LinePoints[0].X;
  LinePoints[1].Y := aRect.Top + TICK_HEIGHT;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FSvgStrings.Add(tempStr);


  { draw the left value}
  aText := Format('%d', [200]);
  x := aRect.Left + 2;
  y :=  aRect.Top + TICK_HEIGHT * 2;
  tempStr := TextToVerticalSvgText(x, y, aText, FTimescaleFontAttribs);
  FSvgStrings.Add(tempStr);


  { draw the center tick}
  LinePoints[0].X := Round(aRect.Left + (aRect.Right - aRect.Left) / 2);
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X := LinePoints[0].X;
  LinePoints[1].Y := LinePoints[0].Y + TICK_HEIGHT;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FSvgStrings.Add(tempStr);

  { draw the center value}
  aText := Format('%d km', [0]);
  x := LinePoints[0].X - Round(FTimescaleFontHeight/3);
  y := LinePoints[1].Y + TICK_HEIGHT;
  tempStr := TextToVerticalSvgText(x, y, aText, FTimescaleFontAttribs);
  FSvgStrings.Add(tempStr);

  { draw the right tick}
  LinePoints[0].X := aRect.Right - 4;
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X := LinePoints[0].X;
  LinePoints[1].Y := LinePoints[0].Y + TICK_HEIGHT;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FSvgStrings.Add(tempStr);

  { draw the right value}
  aText := Format('%d',[200]);
  x := LinePoints[0].X - Round(FTimescaleFontheight/2) - 2;
  y := LinePoints[1].Y + TICK_HEIGHT;
  tempStr := TextToVerticalSvgText(x, y, aText, FTimescaleFontAttribs);
  FSvgStrings.Add(tempStr);

  { connect the ticks}
  LinePoints[0].X := aRect.Left + 2;
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X :=  aRect.Right - 4;
  LinePoints[1].Y := aRect.Top;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FSvgStrings.Add(tempStr);
end;

procedure TTimelineSvgWriter.DrawO2;
var
  coords: TRect;
  ScaleCoords: TRect;
begin
  coords := O2PanelCoords;
  FChartFormatter.BuildAreaChartVertical(FO2Data, MAX_MYA, MIN_MYA, Rect(0, 0, 0, 0));
  OpenSvg('timeline-o2', coords.Left, 0, GEOLOGIC_PANEL_WIDTH*2, FHeight, 'timeline-o2');
  //DrawGeoDataPanelAreaChart('#ffc300');
  DrawGeoDataPanelAreaChart(FO2Data.ChartColor);
  ScaleCoords := Rect(0, coords.Bottom + FMargins.Left, coords.Right - coords.Left, coords.Bottom + FMargins.Bottom);
  DrawGeoDataPanelScale(FO2Data, scaleCoords);
  CloseSvg;
end;

procedure TTimelineSvgWriter.DrawCO2;
var
  coords: TRect;
  ScaleCoords: TRect;
begin
  coords := CO2PanelCoords;
  FChartFormatter.BuildAreaChartVertical(FCO2Data, MAX_MYA, MIN_MYA, Rect(0, 0, 0, 0));
  OpenSvg('timeline-co2', coords.Left, 0, GEOLOGIC_PANEL_WIDTH*2, FHeight, 'timeline-co2');
  //DrawGeoDataPanelAreaChart('#ff5733');
  DrawGeoDataPanelAreaChart(FCO2Data.ChartColor);
  ScaleCoords := Rect(0, coords.Bottom + FMargins.Left, coords.Right - coords.Left, coords.Bottom + FMargins.Bottom);
  DrawGeoDataPanelScale(FCO2Data, scaleCoords);
  CloseSvg;
end;

procedure TTimelineSvgWriter.DrawLuminosity;
var
  coords: TRect;
  scaleCoords: TRect;
begin
  coords := LuminosityPanelCoords;
  FChartFormatter.BuildAreaChartVertical(FLuminosityData, MAX_MYA, MIN_MYA, Rect(0, 0, 0, 0));
  OpenSvg('timeline-luminosity', coords.Left, 0, GEOLOGIC_PANEL_WIDTH*2, FHeight, 'timeline-luminosity');
  //DrawGeoDataPanelAreaChart('#c70039');
  DrawGeoDataPanelAreaChart(FLuminosityData.ChartColor);
  ScaleCoords := Rect(0, coords.Bottom + FMargins.Left, coords.Right - coords.Left, coords.Bottom + FMargins.Bottom);
  DrawGeoDataPanelScale(FLuminosityData, scaleCoords);
  CloseSvg;
end;

procedure TTimelineSvgWriter.DrawGeoDataPanelAreaChart(aColor: String);
var
  PolygonCoords: TArrayOfTPoint;
  Attribs: array of TXmlAttribute;
  AreaChartStr: String;
begin
  PolygonCoords := FChartFormatter.PointsArrayForAreaChart;
  SetLength(Attribs, 4);
  Attribs[0].Name := 'fill';
  Attribs[0].Value := aColor;
  Attribs[1].Name := 'fill-opacity';
  Attribs[1].Value := '0.85';
  Attribs[2].Name := 'stroke';
  Attribs[2].Value := aColor;
  Attribs[3].Name := 'stroke-width';
  Attribs[3].Value := '1';
  AreaChartStr := PointsToSvgPolygon(PolygonCoords, Attribs);
  FSvgStrings.Add(AreaChartStr);
end;

procedure TTimelineSvgWriter.DrawGeoDataPanelScale(aData: TGeoData; aRect: TRect);
const
  TICK_HEIGHT = 5;
var
  MaxVal: Double;
  MinVal: Double;
  LinePoints: array[0..1] of TPoint;
  x,y: Integer;
  aText: String;
  tempStr: String;
begin
  MaxVal := aData.GetMaxValueInRange(MAX_MYA, MIN_MYA);
  MinVal := aData.GetMinValueInRange(MAX_MYA, MIN_MYA);
  { draw the left tick}
  LinePoints[0].X := aRect.Left + 1;
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X := LinePoints[0].X;
  LinePoints[1].Y := aRect.Top + TICK_HEIGHT;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FSvgStrings.Add(tempStr);


  { draw the left value}
  aText := Format('%6.1f', [MinVal]);
  x := aRect.Left +2;
  y :=  aRect.Top + TICK_HEIGHT * 2;
  tempStr := TextToVerticalSvgText(x, y, aText, FTimescaleFontAttribs);
  FSvgStrings.Add(tempStr);


  { draw the center tick}
  LinePoints[0].X := Round(aRect.Left + (aRect.Right - aRect.Left) / 2);
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X := LinePoints[0].X;
  LinePoints[1].Y := LinePoints[0].Y + TICK_HEIGHT;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FSvgStrings.Add(tempStr);

  { draw the center value}
  aText := Format('%6.1f' + aData.LegendKey, [MinVal + (MaxVal - MinVal) / 2]);
  x := LinePoints[0].X - Round(FTimescaleFontHeight/3);
  y := LinePoints[1].Y + TICK_HEIGHT;
  tempStr := TextToVerticalSvgText(x, y, aText, FTimescaleFontAttribs);
  FSvgStrings.Add(tempStr);

  { draw the right tick}
  LinePoints[0].X := aRect.Right - 4;
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X := LinePoints[0].X;
  LinePoints[1].Y := LinePoints[0].Y + TICK_HEIGHT;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FSvgStrings.Add(tempStr);

  { draw the right value}
  aText := Format('%8.1f',[MaxVal]);
  x := LinePoints[0].X - Round(FTimescaleFontheight/2) - 2;
  y := LinePoints[1].Y + TICK_HEIGHT;
  tempStr := TextToVerticalSvgText(x, y, aText, FTimescaleFontAttribs);
  FSvgStrings.Add(tempStr);

  { connect the ticks}
  LinePoints[0].X := aRect.Left + 2;
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X :=  aRect.Right - 4;
  LinePoints[1].Y := aRect.Top;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FSvgStrings.Add(tempStr);
end;

procedure TTimelineSvgWriter.DrawGeologicTimescale;
var
  coords: TRect;
begin
  coords := GeoPanelCoords(tstEon);
  OpenSvg('timeline-geo-timescales', coords.Left, 0, GEOLOGIC_PANEL_WIDTH*2, FHeight, 'timeline-geo-timescales');
  DrawGeologicLevel(tstEon);
  DrawGeologicLevel(tstEra);
  CloseSvg;
end;

procedure TTimelineSvgWriter.DrawGeologicLevel(aType: TTimespanType);
var
  aRect: TRect;
  i: Integer;
  times: TArrayOfGeologicTime;
  Temp: String;
begin
  aRect := GeoPanelCoords(aType);
  case aType of
    tstEon:
      begin
        aRect.Left := 0;
        aRect.Right := aRect.Left + GEOLOGIC_PANEL_WIDTH;
      end;
    tstEra:
      begin
        aRect.Left := GEOLOGIC_PANEL_WIDTH;
        aRect.Right := aRect.Left + GEOLOGIC_PANEL_WIDTH;
      end;
    tstPeriod, tstEpoch, tstAge: raise Exception.Create('these geologic levels not supported for timeline svg');
  end;
  times := FGeoScale.GetTimescale(MIN_MYA, MAX_MYA, aType);

  if Length(times) > 0 then
  begin
    { draw the border}
    InitGeoScaleRectAttribs;
    FSvgStrings.Add(RectToSvgRect(aRect, FGeoScaleRectAttribs));

    { draw the rectangles}
    for i := Length(times) - 1 downto 0 do
    begin
      aRect.Bottom := MyaToYCoord(times[i].StartMya);
      aRect.Top := MyaToYCoord(times[i].EndMya);
      FGeoScaleRectAttribs[0].Value := times[i].HexaDecimalColorStr;
      FGeoScaleRectAttribs[1].Value := times[i].HexaDecimalColorStr;
      FGeoScaleRectAttribs[3].Value := times[i].Name;
      FGeoScaleRectAttribs[4].Value := Format('%.2f', [times[i].StartMya]);
      FGeoScaleRectAttribs[5].Value := Format('%.2f', [times[i].EndMya]);
      FGeoScaleRectAttribs[6].Value := TimespanTypeString(times[i].TimespanType);
      Temp := RectToSvgRect(aRect, FGeoScaleRectAttribs);
      FSvgStrings.Add(Temp);
      FGeoScaleFontAttribs[0].Value := times[i].Name;
      FGeoScaleFontAttribs[4].Value := Format('%.2f', [times[i].StartMya]);
      FGeoScaleFontAttribs[5].Value := Format('%.2f', [times[i].EndMya]);
      FGeoScaleFontAttribs[6].Value := TimespanTypeString(times[i].TimespanType);

      case aType of
        tstEon, tstEra, tstPeriod:
          begin
            Temp := AddVerticalSvgTextToRect(aRect, times[i].GetBestFitText(aRect, tdVertical), '16', FGeoScaleFontAttribs);
          end;
        tstEpoch, tstAge:
          begin
            Assert(False, 'Drawing of epochs and ages is not supported for timeline search');
          end;
      end;

      if Temp <> EmptyStr then
        FSvgStrings.Add(Temp);
    end;
  end;
end;

procedure TTimelineSvgWriter.DrawBorder;
var
  aRect: TRect;
  attribs: array[0..4] of TXmlAttribute;
  Temp: String;
begin
  aRect.Top := 1;
  aRect.Bottom := FHeight-1;
  aRect.Left := 0;
  aRect.Right := FWidth-1;

  attribs[0].Name := 'fill';
  attribs[0].Value := 'none';
  attribs[1].Name := 'stroke';
  attribs[1].Value := '#555555';
  attribs[2].Name := 'stroke-width';
  attribs[2].Value := '1';
  attribs[3].Name := 'rx';
  attribs[3].Value := '2';
  attribs[4].Name := 'ry';
  attribs[4].Value := '2';
  Temp := RectToSvgRect(aRect, attribs);
  FSvgStrings.Add(Temp);
end;

procedure TTimelineSvgWriter.DrawBackground;
var
  Temp: String;
begin
  Temp := '<image width=' + dblq + IntToStr(FWidth) + dblq + ' ';
  Temp := Temp + 'height=' + dblq + IntToStr(FHeight) + dblq + ' ';
  Temp := Temp + 'xlink:href=' + dblq + 'data:image/png;base64,' + TIMELINE_BG_IMG + dblq + '></image>';
  FSvgStrings.Add(Temp);
end;

procedure TTimelineSvgWriter.DrawTimeScale;
var
  aRect: TRect;
  temp: String;
  points: array[0..1] of TPoint;
  i, x, y: Integer;
  mya: Double;
begin
  InitTimescaleTicks;
  aRect := TimescaleCoords;
  OpenSvg('timescale', aRect.Left, 0, aRect.Right - aRect.Left, FHeight);

  points[0].X := Round((aRect.Right - aRect.Left) / 4);
  points[1].X := points[0].X;
  points[0].Y := aRect.Top;
  points[1].Y := aRect.Bottom;
  Temp :=  PointsToSvgLine(points, FTimescaleLineAttribs);
  FSvgStrings.Add(Temp);


  points[1].X := points[1].X + TICK_LENGTH;
  for i := 0 to Length(FTimescaleTicks) - 1 do
  begin
    points[0].Y := FTimescaleTicks[i].Y;
    points[1].Y := points[0].Y;
    Temp := PointsToSvgLine(points, FTimescaleLineAttribs);
    FSvgStrings.Add(Temp);
    mya := FTimescaleTicks[i].X;
    Temp := TextToSvgText(points[1].X + 4, points[0].Y + Round(FTimescaleFontHeight / 3), Format('%.0f', [mya]), FTimescaleFontAttribs);
    FSvgStrings.Add(Temp);
  end;
  x := Round((aRect.Right - aRect.Left)/2 - (CustomTextHeight('MYA') div 4));
  y := aRect.Bottom + 10;
  Temp := TextToVerticalSvgText(x, y, 'MYA', FTimescaleFontAttribs);
  FSvgStrings.Add(Temp);
  CloseSvg;
end;

procedure TTimelineSvgWriter.DrawPoints;
var
  i, x, y, r: Integer;
  aResult: TTimelineResult;
  SvgTag: String;
  divTimeStr, ciLowStr, ciHighStr: String;
begin

  if not UpdateCoordsForTimelineUsingBins then
    UpdateCoordsForTimelineResultsStatic;
  AddDefinitions;

  { draw the points for the unknown events first so they are below the known events in the SVG}
  if FTimeline.Count > 0 then
  begin
    for i := 0 to FTimeline.Count - 1 do
    begin
      aResult := FTimeline[i];
      if not aResult.IsUnknownEvent then
        continue;
      x := aResult.TimelineXCoord;
      y := aResult.TimelineYCoord;
      r := TIME_MARKER_RADIUS;
      FormatTimeIntervalStrings(aResult.BranchLength, aResult.ConfidenceIntervalLow, aResult.ConfidenceIntervalHigh, divTimeStr, ciLowStr, ciHighStr);
      FResultCircleAttribs[0].Name := 'fill';
      FResultCircleAttribs[0].Value := 'white';
      FResultCircleAttribs[1].Name := 'id';
      FResultCircleAttribs[1].Value := IntToStr(aResult.Id);
      FResultCircleAttribs[2].Name := 'scientific-name';
      FResultCircleAttribs[2].Value := aResult.ScientificName;
      FResultCircleAttribs[3].Name := 'time';
      FResultCircleAttribs[3].Value := divTimeStr;
      FResultCircleAttribs[4].Name := 'ci_string';
      FResultCircleAttribs[4].Value := aResult.CiString;
      FResultCircleAttribs[5].Name := 'is_range';
      FResultCircleAttribs[5].Value := LowerCase(BoolToStr((aResult.CiString <> EmptyStr) and (Pos('Range', aResult.CiString) > 0), True));
      FResultCircleAttribs[6].Name := 'rank';
      FResultCircleAttribs[6].Value := TaxonomicRankToString(aResult.TaxonomicRank);
      FResultCircleAttribs[7].Name := 'isci';
      FResultCircleAttribs[7].Value := LowerCase(BoolToStr(aResult.IsConfidenceInterval, True));
      FResultCircleAttribs[8].Name := 'stroke';
      FResultCircleAttribs[8].Value := 'black';
      FResultCircleAttribs[9].Name := 'stroke-width';
      FResultCircleAttribs[9].Value := '1';
      FResultCircleAttribs[10].Name := 'tt_id';
      FResultCircleAttribs[10].Value := IntToStr(aResult.TimeTreeId);
      FResultCircleAttribs[11].Name := 'adjusted_age';
      FResultCircleAttribs[11].Value := FloatToStr(aResult.AdjustedAge);
      SvgTag := CircleToSvgCircle(x, y, r, FResultCircleAttribs);
      FSvgStrings.Add(SvgTag);
    end;

    for i := 0 to FTimeline.Count - 1 do
    begin
      aResult := FTimeline[i];
      if aResult.IsUnknownEvent then
        continue;
      x := aResult.TimelineXCoord;
      y := aResult.TimelineYCoord;
      r := TIME_MARKER_RADIUS;
      FormatTimeIntervalStrings(aResult.BranchLength, aResult.ConfidenceIntervalLow, aResult.ConfidenceIntervalHigh, divTimeStr, ciLowStr, ciHighStr);
      FResultCircleAttribs[0].Name := 'fill';
      FResultCircleAttribs[0].Value := 'url(#dotsGradient)';
      FResultCircleAttribs[1].Name := 'id';
      FResultCircleAttribs[1].Value := IntToStr(aResult.Id);
      FResultCircleAttribs[2].Name := 'scientific-name';
      FResultCircleAttribs[2].Value := aResult.ScientificName;
      FResultCircleAttribs[3].Name := 'time';
      FResultCircleAttribs[3].Value := divTimeStr;
      FResultCircleAttribs[4].Name := 'ci_string';
      FResultCircleAttribs[4].Value := aResult.CiString;
      FResultCircleAttribs[5].Name := 'is_range';
      FResultCircleAttribs[5].Value := LowerCase(BoolToStr((aResult.CiString <> EmptyStr) and (Pos('Range', aResult.CiString) > 0), True));
      FResultCircleAttribs[6].Name := 'rank';
      FResultCircleAttribs[6].Value := TaxonomicRankToString(aResult.TaxonomicRank);
      FResultCircleAttribs[7].Name := 'isci';
      FResultCircleAttribs[7].Value := LowerCase(BoolToStr(aResult.IsConfidenceInterval, True));
      FResultCircleAttribs[8].Name := 'stroke';
      FResultCircleAttribs[8].Value := 'none';
      FResultCircleAttribs[9].Name := 'stroke-width';
      FResultCircleAttribs[9].Value := '1';
      FResultCircleAttribs[10].Name := 'tt_id';
      FResultCircleAttribs[10].Value := IntToStr(aResult.TimeTreeId);
      FResultCircleAttribs[11].Name := 'adjusted_age';
      FResultCircleAttribs[11].Value := FloatToStr(aResult.AdjustedAge);
      SvgTag := CircleToSvgCircle(x, y, r, FResultCircleAttribs);
      FSvgStrings.Add(SvgTag);
    end;
  end;
end;

procedure TTimelineSvgWriter.DrawNames;
var
  i: Integer;
  aResult: TTimelineResult;
  aBoxWidth: Integer;
  boxCoords: TRect;
  SvgTag: String;
  aName: String;
begin
  if FTimeline.Count = 0 then
    Exit;

  aBoxWidth := WidthOfLongestName;
  for i := 0 to FTimeline.Count - 1 do
  begin
    aResult := FTimeline[i];
    if (not FDrawUnknownNodes) and aResult.IsUnknownEvent then
      continue;
    boxCoords.Left := aResult.NameXCoord;
    boxCoords.Top := aResult.NameYCoord - NAME_BOX_PADDING;
    boxCoords.Right := boxCoords.Left + aBoxWidth;
    boxCoords.Bottom := boxCoords.Top + FFontHeight + NAME_BOX_PADDING;
    if aResult.CommonName <> EmptyStr then
      aName := aResult.CommonName
    else
      aName := aResult.ScientificName;
    if not aResult.IsUnknownEvent then
    begin
      FNamesBoxAttribs[5].Value := aName;
      FNamesBoxAttribs[6].Value := TaxonomicRankToString(aResult.TaxonomicRank);
      FNamesBoxAttribs[8].Value := IntToStr(aResult.TimeTreeId);
      SvgTag := RectToSvgRect(boxCoords, FNamesBoxAttribs);
    end
    else
    begin
      FUnnamedNodesBoxAttribs[5].Value := aName;
      FUnnamedNodesBoxAttribs[6].Value := TaxonomicRankToString(aResult.TaxonomicRank);
      FUnnamedNodesBoxAttribs[8].Value := IntToStr(aResult.TimeTreeId);
      SvgTag := RectToSvgRect(boxCoords, FUnnamedNodesBoxAttribs)
    end;
    FSvgStrings.Add(SvgTag);

    boxCoords.Top := boxCoords.Top + 3;
    if not aResult.IsUnknownEvent then
      SvgTag := AddSvgTextToRect(boxCoords, aName, FNamesFontAttribs, FFontHeight);
    FSvgStrings.Add(SvgTag);
  end;
end;

procedure TTimelineSvgWriter.AddGradientDefs;
var
  Temp: String;
begin
  Temp := '<defs>';
  Temp := Temp + '<linearGradient id=' + dblq + 'boxGradient' + dblq + ' x1=' + dblq + '0' + dblq + ' x2=' + dblq + '0' + dblq + ' y1=' + dblq + '0' + dblq + ' y2=' + dblq + '1' + dblq + '>';
  Temp := Temp + '<stop offset=' + dblq + '0%' + dblq + ' stop-color=' + dblq + '#fefcea' + dblq + '/>';
  Temp := Temp + '<stop offset=' + dblq + '100%' + dblq + ' stop-color=' + dblq + '#f1da36' + dblq + '/>';
  Temp := Temp + '</linearGradient>';
  Temp := Temp + '<linearGradient id=' + dblq + 'grayBoxGradient' + dblq + ' x1=' + dblq + '0' + dblq + ' x2=' + dblq + '0' + dblq + ' y1=' + dblq + '0' + dblq + ' y2=' + dblq + '1' + dblq + '>';
  Temp := Temp + '<stop offset=' + dblq + '0%' + dblq + ' stop-color=' + dblq + '#eee' + dblq + '/>';
  Temp := Temp + '<stop offset=' + dblq + '100%' + dblq + ' stop-color=' + dblq + '#ccc' + dblq + '/>';
  Temp := Temp + '</linearGradient>';
  Temp := Temp + '</defs>';
  FSvgStrings.Add(Temp);
end;

procedure TTimelineSvgWriter.DrawTimeText;
var
  i: Integer;
  aResult: TTimelineResult;
  boxCoords: TRect;
  SvgTag: String;
begin
  if FTimeline.Count = 0 then
    Exit;
  SetLength(FNamesBoxAttribs, 8);
  SetLength(FUnnamedNodesBoxAttribs, 8);
  FUnnamedNodesBoxAttribs[5].Name := 'class';
  FUnnamedNodesBoxAttribs[5].Value := 'div-time';
  FNamesBoxAttribs[5].Name := 'class';
  FNamesBoxAttribs[5].Value := 'div-time';

  for i := 0 to FTimeline.Count - 1 do
  begin
    aResult := FTimeline[i];
    if (not FDrawUnknownNodes) and (aResult.IsUnknownEvent) then
      continue;
    if CompareValue(aResult.AdjustedAge, 0, FP_CUTOFF) > 0 then
    begin
      FNamesBoxAttribs[6].Name := 'adjusted_age';
      FNamesBoxAttribs[6].Value := FloatToStr(aResult.AdjustedAge);
      FUnnamedNodesBoxAttribs[6].Name := 'adjusted_age';
      FUnnamedNodesBoxAttribs[6].Value := FloatToStr(aResult.AdjustedAge);
    end
    else
    begin
      FNamesBoxAttribs[6].Name := EmptyStr;
      FNamesBoxAttribs[6].Value := EmptyStr;
      FUnnamedNodesBoxAttribs[6].Name := EmptyStr;
      FUnnamedNodesBoxAttribs[6].Value := EmptyStr;
    end;

    if Trim(aResult.CiString) <> EmptyStr then
    begin
      FNamesBoxAttribs[7].Name := 'ci_string';
      FNamesBoxAttribs[7].Value := aResult.CiString;
      FUnnamedNodesBoxAttribs[7].Name := 'ci_string';
      FUnnamedNodesBoxAttribs[7].Value := aResult.CiString;
    end
    else
    begin
      FNamesBoxAttribs[7].Name := EmptyStr;
      FNamesBoxAttribs[7].Value := EmptyStr;
      FUnnamedNodesBoxAttribs[7].Name := EmptyStr;
      FUnnamedNodesBoxAttribs[7].Value := EmptyStr;
    end;

    boxCoords.Left := aResult.TimeTextXCoord;
    boxCoords.Top := aResult.TimeTextYCoord - NAME_BOX_PADDING;
    boxCoords.Right := boxCoords.Left + TIME_BOX_WIDTH;
    boxCoords.Bottom := boxCoords.Top + FFontHeight + NAME_BOX_PADDING;
    if aResult.IsUnknownEvent then
      SvgTag := RectToSvgRect(boxCoords, FUnnamedNodesBoxAttribs)
    else
      SvgTag := RectToSvgRect(boxCoords, FNamesBoxAttribs);
    FSvgStrings.Add(SvgTag);

    boxCoords.Top := boxCoords.Top + 3;
    SvgTag := AddSvgTextToRect(boxCoords, Format('%0.0f', [aResult.BranchLength]), FNamesFontAttribs, FFontHeight);
    FSvgStrings.Add(SvgTag);
  end;
end;

procedure TTimelineSvgWriter.DrawLines;
var
  lineCoords: array[0..2] of TPoint;
  i: Integer;
  aResult: TTimelineResult;
  SvgTag: String;
begin
  if FTimeline.Count = 0 then
    Exit;

  for i := 0 to FTimeline.Count - 1 do
  begin
    aResult := FTimeline[i];
    if (not FDrawUnknownNodes) and (aResult.IsUnknownEvent) then
      continue;
    lineCoords[0].X := aResult.TimelineXCoord + Round(TIME_MARKER_RADIUS / 2) + 1;
    lineCoords[0].Y := aResult.TimelineYCoord;
    lineCoords[1].X := lineCoords[0].X + Round((aResult.FigurineXCoord - aResult.TimelineXCoord) / 2);
    lineCoords[1].Y := lineCoords[0].Y;
    lineCoords[2].X := aResult.NameXCoord - NAME_BOX_PADDING;
    lineCoords[2].Y := aResult.LineYCoord;
    SvgTag := PointsToSvgLine(lineCoords, FLineAttribs);
    FSvgStrings.Add(SvgTag);
  end;
end;

procedure TTimelineSvgWriter.DrawFigurines;
var
  i,j : Integer;
  aResult: TTimelineResult;
  SvgTag: String;
  uri: String;
begin
  if FTimeline.Count = 0 then
    Exit;

  for i := 0 to FTimeline.Count - 1 do
  begin
    aResult := FTimeline[i];
    if aResult.FigurinesList.Count <> 0 then
    begin
      j := aResult.FigurinesList.Count - 1;
      while j >= 0 do
      begin
        uri := aResult.FigurinesList[j];
        if FUsedFigurines.IndexOf(uri) < 0 then
        begin
          FUsedFigurines.Add(uri);
          {$IFDEF DEBUG}
          uri := 'http://www.kumarlab_dev.net' + aResult.FigurinesList[J];
          {$ENDIF}

          break;
        end
        else
        begin
          uri := EmptyStr;
        end;
        dec(j);
      end;
      SvgTag := SvgImageTag(aResult.FigurineXCoord, aResult.FigurineYCoord, FIGURINE_WIDTH, FIGURINE_WIDTH, uri);
      FSvgStrings.Add(SvgTag);
    end;
  end;
end;

procedure TTimelineSvgWriter.InitDefaultMargins;
begin
  FMargins.Top := 10;
  FMargins.Right := 5;
  FMargins.Bottom := 50;
  FMargins.Left := 5;
end;

procedure TTimelineSvgWriter.InitGeoScaleRectAttribs;
begin
  FGeoScaleRectAttribs[0].Name := 'fill';
  FGeoScaleRectAttribs[0].Value := 'none';
  FGeoScaleRectAttribs[1].Name := 'stroke';
  FGeoScaleRectAttribs[1].Value := 'black';
  FGeoScaleRectAttribs[2].Name := 'stroke-width';
  FGeoScaleRectAttribs[2].Value := '1';
  FGeoScaleRectAttribs[3].Name := 'full-name';
  FGeoScaleRectAttribs[3].Value := '';
  FGeoScaleRectAttribs[4].Name := 'start';
  FGeoScaleRectAttribs[4].Value := '';
  FGeoScaleRectAttribs[5].Name := 'end';
  FGeoScaleRectAttribs[5].Value := '';
  FGeoScaleRectAttribs[6].Name := 'unit';
  FGeoScaleRectAttribs[6].Value := '';
end;

procedure TTimelineSvgWriter.InitGeoScaleFontAttribs;
begin
  FGeoScaleFontAttribs[0].Name := 'full-name';
  FGeoScaleFontAttribs[1].Name := 'font-family';
  FGeoScaleFontAttribs[1].Value := 'Roboto Condensed';
  FGeoScaleFontAttribs[2].Name := 'font-size';
  FGeoScaleFontAttribs[2].Value := IntToStr(FFontHeight);
  FGeoScaleFontAttribs[3].Name := 'text-anchor';
  FGeoScaleFontAttribs[3].Value := 'middle';
  FGeoScaleFontAttribs[4].Name := 'start';
  FGeoScaleFontAttribs[4].Value := '';
  FGeoScaleFontAttribs[5].Name := 'end';
  FGeoScaleFontAttribs[5].Value := '';
  FGeoScaleFontAttribs[6].Name := 'unit';
  FGeoScaleFontAttribs[6].Value := '';
end;

procedure TTimelineSvgWriter.InitTimescaleLineAttribs;
begin
  FTimescaleLineAttribs[0].Name := 'stroke';
  FTimescaleLineAttribs[0].Value := 'black';
  FTimescaleLineAttribs[1].Name := 'stroke-width';
  FTimescaleLineAttribs[1].Value := '1';
  FTimescaleLineAttribs[2].Name := 'stroke-linecap';
  FTimescaleLineAttribs[2].Value := 'square';
end;

procedure TTimelineSvgWriter.InitTimescaleFontAttribs;
begin
  FTimescaleFontAttribs[0].Name := 'color';
  FTimescaleFontAttribs[0].Value := 'black';
  FTimescaleFontAttribs[1].Name := 'font-size';
  FTimescaleFontAttribs[1].Value := IntToStr(FTimescaleFontHeight);
  FTimescaleFontAttribs[2].Name := 'font-family';
  FTimescaleFontAttribs[2].Value := 'Roboto Condensed';
end;

procedure TTimelineSvgWriter.InitNamesBoxAttribs;
begin
  SetLength(FNamesBoxAttribs, 9);
  FNamesBoxAttribs[0].Name := 'fill';
  FNamesBoxAttribs[0].Value := 'url(#boxGradient)';
  FNamesBoxAttribs[1].Name := 'stroke';
  FNamesBoxAttribs[1].Value := '#bbbbbb';
  FNamesBoxAttribs[2].Name := 'stroke-width';
  FNamesBoxAttribs[2].Value := '1';
  FNamesBoxAttribs[3].Name := 'rx';
  FNamesBoxAttribs[3].Value := '3';
  FNamesBoxAttribs[4].Name := 'ry';
  FNamesBoxAttribs[4].Value := '3';
  FNamesBoxAttribs[5].Name := 'scientific-name';
  FNamesBoxAttribs[5].Value := EmptyStr;
  FNamesBoxAttribs[6].Name := 'title';
  FNamesBoxAttribs[6].Value := EmptyStr;
  FNamesBoxAttribs[7].Name := 'class';
  FNamesBoxAttribs[7].Value := 'rank-tooltip';
  FNamesBoxAttribs[8].Name := 'tt_id';
  FNamesBoxAttribs[8].Value := '-1';
end;

procedure TTimelineSvgWriter.InitUnnamedNodesBoxAttribs;
begin
  SetLength(FUnnamedNodesBoxAttribs, 9);
  FUnnamedNodesBoxAttribs[0].Name := 'fill';
  FUnnamedNodesBoxAttribs[0].Value := 'url(#grayBoxGradient)';
  FUnnamedNodesBoxAttribs[1].Name := 'stroke';
  FUnnamedNodesBoxAttribs[1].Value := '#bbbbbb';
  FUnnamedNodesBoxAttribs[2].Name := 'stroke-width';
  FUnnamedNodesBoxAttribs[2].Value := '1';
  FUnnamedNodesBoxAttribs[3].Name := 'rx';
  FUnnamedNodesBoxAttribs[3].Value := '3';
  FUnnamedNodesBoxAttribs[4].Name := 'ry';
  FUnnamedNodesBoxAttribs[4].Value := '3';
  FUnnamedNodesBoxAttribs[5].Name := 'scientific-name';
  FUnnamedNodesBoxAttribs[5].Value := EmptyStr;
  FUnnamedNodesBoxAttribs[6].Name := 'title';
  FUnnamedNodesBoxAttribs[6].Value := EmptyStr;
  FUnnamedNodesBoxAttribs[7].Name := 'class';
  FUnnamedNodesBoxAttribs[7].Value := 'rank-tooltip';
  FUnnamedNodesBoxAttribs[8].Name := 'tt_id';
  FUnnamedNodesBoxAttribs[8].Value := '-1';
end;

procedure TTimelineSvgWriter.InitNamesFontAttribs;
begin
  FNamesFontAttribs[0].Name := 'font-family';
  FNamesFontAttribs[0].Value := 'Roboto Condensed';
  FNamesFontAttribs[1].Name := 'font-size';
  FNamesFontAttribs[1].Value := IntToStr(FFontHeight);
  FNamesFontAttribs[2].Name := 'text-anchor';
  FNamesFontAttribs[2].Value := 'middle';
  FNamesFontAttribs[3].Name := 'fill';
  FNamesFontAttribs[3].Value := 'black';
end;

procedure TTimelineSvgWriter.InitTimescaleTicks;
var
  i, y: Integer;
  mya: Integer;
begin
  SetLength(FTimescaleTicks, 12);
  FTimescaleTicks[0].Y := FMargins.Top;
  FTimescaleTicks[0].X := Round(MIN_MYA);

  for i := 1 to 5 do
  begin
    mya := i * 100;
    y := Round(MyaToYCoord(mya));
    FTimescaleTicks[i].Y := y;
    FTimescaleTicks[i].X := mya;

  end;
  for i := 6 to 9 do
  begin
    mya := (i - 5) * 1000;
    y := Round(MyaToYCoord(mya));
    FTimescaleTicks[i].Y := y;
    FTimescaleTicks[i].X := Round(mya);
  end;
  FTimescaleTicks[10].Y := FHeight - FMargins.Bottom;
  FTimescaleTicks[10].X := Round(MAX_MYA);
  FTimescaleTicks[11].Y := Round(MyaToYCoord(START_OF_PHANEROZOIC));
  FTimescaleTicks[11].X := Round(START_OF_PHANEROZOIC);
end;

procedure TTimelineSvgWriter.InitLineAttribs;
begin
  FLineAttribs[0].Name := 'stroke';
  FLineAttribs[0].Value := 'black';
  FLineAttribs[1].Name := 'stroke-width';
  FLineAttribs[1].Value := '1';
  FLineAttribs[2].Name := 'fill';
  FLineAttribs[2].Value := 'none';
  FLineAttribs[3].Name := 'stroke-dasharray';
  FLineAttribs[3].Value := '2,2';
end;

procedure TTimelineSvgWriter.InitChartFormatter;
begin
  FChartFormatter := TGeoDataChartFormatter.Create;
  FChartFormatter.ChartHeight := FHeight;
  FChartFormatter.ChartWidth := GEOLOGIC_PANEL_WIDTH*2 - FMargins.Left;
  FChartFormatter.ChartMargins := Rect(2, FMargins.Top, 2, FMargins.Bottom);
  FChartFormatter.MapAgeToCoordFunc := @MyaToYCoord;
end;

function TTimelineSvgWriter.CalculateNumTimescaleTicks: Integer;
var
  i: Integer;
begin
  Result := FHeight - FMargins.Top - FMargins.Bottom;
  i := 1;
  while Result > MIN_TIME_TICK_SPACING do
  begin
    i := i * 2;
    Result := (FHeight - FMargins.Top - FMargins.Bottom) div i;
  end;
  Result := Result + 2;
end;

procedure TTimelineSvgWriter.SetHeight(AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

procedure TTimelineSvgWriter.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
end;

procedure TTimelineSvgWriter.AddDefinitions;
var
  Temp: String;
begin
  Temp := '<defs>';
  Temp := Temp + '<linearGradient id=' + dblq + 'dotsGradient' + dblq + ' x1=' + dblq + '0' + dblq + ' x2=' + dblq + '0' + dblq + ' y1=' + dblq + '0' + dblq + ' y2=' + dblq + '1' + dblq + '>';
  Temp := Temp + '<stop offset=' + dblq + '0%' + dblq + ' stop-color=' + dblq + 'dddddd' + dblq + '/>';
  Temp := Temp + '<stop offset=' + dblq + '100%' + dblq + ' stop-color=' + dblq + '#777777' + dblq + '/>';
  Temp := Temp + '</linearGradient>';
  Temp := Temp + '</defs>';
  FSvgStrings.Add(Temp);
end;

function TTimelineSvgWriter.UpdateCoordsForTimelineResultsStatic: Boolean;
var
  i, x, y: Integer;
  aResult: TTimelineResult;
  aRect: TRect;
  mya: Double;
  TextSpaceNeeded: Integer;
begin
  Result := False;
  if FTimeline.Count = 0 then
    Exit;

  aRect := ResultsCoords;
  TextSpaceNeeded := WidthOfLongestName;
  x := (aRect.Right - aRect.Left) - TextSpaceNeeded - TIME_BOX_WIDTH - TEXT_RECT_MARGIN - 3;
  y := aRect.Top + FFontHeight;
  for i := 0 to FTimeline.Count - 1 do
  begin
    aResult := FTimeline[i];
    mya := aResult.BranchLength;
    aResult.TimelineYCoord := MyaToYCoord(mya);
    aResult.TimelineXCoord := TIME_POINTS_HORIZ_OFFSET;
    aResult.FigurineXCoord := x - FIGURINE_WIDTH - TEXT_RECT_MARGIN;
    aResult.FigurineYCoord := y - 7;
    aResult.LineYCoord := y + Round((aRect.Bottom - aRect.Top) / 2);
    aResult.NameXCoord := x;
    aResult.NameYCoord := y;
    aResult.TimeTextXCoord := x + TextSpaceNeeded + TEXT_RECT_MARGIN;
    aResult.TimeTextYCoord := y;
    y := y + FFontHeight + NAME_VERT_SPACING;
  end;
  Result := ((FTimeline[FTimeline.Count - 1].NameYCoord - FFontHeight * 2) < FHeight);
end;

//function TTimelineSvgWriter.UpdateCoordsForTimelineResultsDynamic: Boolean;
//var
//  i, x, y, bestY: Integer;
//  aResult: TTimelineResult;
//  aRect: TRect;
//  mya: Double;
//  TextSpaceNeeded: Integer;
//begin
//  Result := False;
//  if FTimeline.Count = 0 then
//    Exit;
//
//  aRect := ResultsCoords;
//  TextSpaceNeeded := WidthOfLongestName;
//  x := (aRect.Right - aRect.Left) - TextSpaceNeeded - TIME_BOX_WIDTH - TEXT_RECT_MARGIN - 3;
//  y := aRect.Top + FFontHeight;
//  for i := 0 to FTimeline.Count - 1 do
//  begin
//    aResult := FTimeline[i];
//    mya := aResult.BranchLength;
//    bestY := MyaToYCoord(mya);
//    if bestY > y then
//      y := bestY;
//    aResult.TimelineYCoord := MyaToYCoord(mya);
//    aResult.TimelineXCoord := TIME_POINTS_HORIZ_OFFSET;
//    aResult.FigurineXCoord := x - FIGURINE_WIDTH - TEXT_RECT_MARGIN;
//    aResult.FigurineYCoord := y - 7;
//    aResult.LineYCoord := y + Round((aRect.Bottom - aRect.Top) / 2);
//    aResult.NameXCoord := x;
//    aResult.NameYCoord := y;
//    aResult.TimeTextXCoord := x + TextSpaceNeeded + TEXT_RECT_MARGIN;
//    aResult.TimeTextYCoord := y;
//    y := y + FFontHeight + NAME_VERT_SPACING;
//  end;
//  Result := ((aResult.NameYCoord - FFontHeight * 2) < (FHeight - FMargins.Bottom));
//end;

function TTimelineSvgWriter.UpdateCoordsForTimelineUsingBins: Boolean;
var
  BinHeight: Integer;
  i: Integer;
  x: Integer;
  aResult: TTimelineResult;
  aRect: TRect;
  mya: Double;
  TextSpaceNeeded: Integer;
  BinSorter: TTimelineResultBinArray=nil;
begin
  aRect := ResultsCoords;
  TextSpaceNeeded := WidthOfLongestName;
  x := (aRect.Right - aRect.Left) - TextSpaceNeeded - TIME_BOX_WIDTH - TEXT_RECT_MARGIN - 3;

  for i := 0 to FTimeline.Count - 1 do
  begin
    aResult := FTimeline[i];
    mya := aResult.BranchLength;
    aResult.TimelineYCoord := MyaToYCoord(mya);
    aResult.TimelineXCoord := TIME_POINTS_HORIZ_OFFSET;
    aResult.FigurineXCoord := x - FIGURINE_WIDTH - TEXT_RECT_MARGIN;
    aResult.NameXCoord := x;
    aResult.TimeTextXCoord := x + TextSpaceNeeded + TEXT_RECT_MARGIN;
  end;

  try
    BinHeight := FFontHeight + NAME_VERT_SPACING + NAME_BOX_PADDING;
    BinSorter := TTimelineResultBinArray.Create(aRect, BinHeight);
    BinSorter.YCoordsMappingFunc := @MyaToYCoord;
    Assert(BinSorter.NumBins >= FTimeline.GetNumVisibleNodes(FDrawUnknownNodes));
    for i := 0 to FTimeline.Count - 1 do
    begin
      if not FDrawUnknownNodes then
        if TTimelineResult(FTimeline[i]).IsUnknownEvent then
          continue;
      BinSorter.Add(FTimeline[i]);
    end;
    Result := BinSorter.MoveResultsToBestFitBins;
  finally
    if Assigned(BinSorter)then
      BinSorter.Free;
  end;
end;

function TTimelineSvgWriter.WidthOfLongestName: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FTimeline.Count = 0 then
    Exit;
  for i := 0 to FTimeline.Count - 1 do
  begin
    if CustomTextWidth(FTimeline[i].ScientificName) > Result then
      Result := CustomTextWidth(FTimeline[i].ScientificName);
  end;
  Result := Result + 10;
end;

procedure TTimelineSvgWriter.AdjustHeightIfNeeded;
var
  binHeight: Integer;
  numNodesShown: Integer;
begin
  numNodesShown := FTimeline.GetNumVisibleNodes(FDrawUnknownNodes);
  binHeight := FFontHeight + NAME_VERT_SPACING + NAME_BOX_PADDING;
  while ((FHeight div binHeight) <= (numNodesShown + 3)) do
    inc(FHeight, binHeight);
end;

function TTimelineSvgWriter.LoadEarthImpacts: Boolean;
begin
  Result := False;

  FEarthImpacts := TEarthImpacts.Create;
  try
    FEarthImpacts.LoadFromFile(ImpactsFile);
    Result := True;
  except
    on E:Exception do
    begin
      WriteLn('Failed to load earth impacts file: '  + E.Message);
      Halt(6);
    end;
  end;
end;

function TTimelineSvgWriter.LoadClimateData: Boolean;
begin
  Result := False;
  if Trim(O2File) <> EmptyStr then
  begin
    FO2Data := TGeoData.Create('Oxygen', 'o2', '%%');
    FO2Data.ChartColor := O2_CHART_COLOR;
    if not FO2Data.LoadFromFile(O2File) then
      raise Exception.Create('failed to load O2 data');
  end;

  if Trim(CO2File) <> EmptyStr then
  begin
    FCO2Data := TGeoData.Create('Carbon Dioxide', 'co2', '%%');
    FCO2Data.ChartColor := CO2_CHART_COLOR;
    if not FCO2Data.LoadFromFile(CO2File) then
      raise Exception.Create('failed to load CO2 data');
  end;

  //FLuminosityData := TGeoData.Create('Solar Luminosity', 'luminosity', 'L☉');
  FLuminosityData := TGeoData.Create('Solar Luminosity', 'luminosity', 'L');
  FLuminosityData.ChartColor := LUMINOSITY_CHART_COLOR;
  if not FLuminosityData.LoadFromFormula(True) then
    raise Exception.Create('failed to load Luminosity data');

  Result := True;
end;

constructor TTimelineSvgWriter.Create;
begin
  FDrawUnknownNodes := False;
  FSvgStrings := TStringList.Create;
  FName := TIMELINE_SVG_NAME;
  FGeoScale := TCompositeGeologicTime.Create;
  FFontHeight := 16;
  FTimescaleFontHeight := 10;
  FUsedFigurines := TStringList.Create;
  InitDefaultMargins;
  InitGeoScaleFontAttribs;
  InitGeoScaleRectAttribs;
  InitTimescaleFontAttribs;
  InitTimescaleLineAttribs;
  InitNamesBoxAttribs;
  InitUnnamedNodesBoxAttribs;
  InitNamesFontAttribs;
  InitLineAttribs;
end;

destructor TTimelineSvgWriter.Destroy;
begin
  if Assigned(FUsedFigurines) then
    FUsedFigurines.Free;
  if Assigned(FSvgStrings) then
    FSvgStrings.Free;
  if Assigned(FGeoScale) then
    FGeoScale.Free;
  if Assigned(FO2Data) then
    FO2Data.Free;
  if Assigned(FCO2Data) then
    FCO2Data.Free;
  if Assigned(FLuminosityData) then
    FLuminosityData.Free;
  if Assigned(FChartFormatter) then
    FChartFormatter.Free;
  inherited Destroy;
end;

function TTimelineSvgWriter.GenerateSvg(Timeline: TTimeline; Dimensions: TPoint): String;
var
  aRect: TRect;
begin
  FTimeline := Timeline;
  FWidth := Dimensions.X;
  FHeight := Dimensions.Y;
  AdjustHeightIfNeeded;
  OpenSvg;
  //DrawBackground;
  //DrawBorder;
  LoadClimateData;
  InitChartFormatter;
  DrawLuminosity;
  DrawCO2;
  DrawO2;
  DrawEarthImpacts;
  DrawGeologicTimescale;
  DrawTimescale;
  aRect := ResultsCoords;
  OpenSvg('results', aRect.Left, 0, aRect.Right - aRect.Left, FHeight, 'timeline-results');
  AddGradientDefs;
  DrawPoints;
  DrawNames;
  DrawTimeText;
  DrawLines;
  //DrawFigurines;
  CloseSvg;
  CloseSvg;
  Result := FSvgStrings.Text;
  {$IFDEF DEBUG}
  FSvgStrings.SaveToFile('/home/gstecher/Documents/megatt/timelinesearch/debug.svg');
  {$ENDIF}
end;

end.

