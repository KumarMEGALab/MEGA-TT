unit gsvgwriter;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, gtreelist, gutils, ttconst, gsvgtreebox, mearthimpacts,
  mgeodata, gtaxonomicrank, gpairwisesvg, geologicaltimes, MLongintList, mgeodatachartformatter,
  mearthimpactsrenderer;

type

  { TTimetreeSvgWriter }

  TTimetreeSvgWriter = class(TObject)
    private
      FCanScaleTree: Boolean;
      FDeepestRank: TTaxonomicRank;
      FDoLogScale: Boolean;
      FDoNewick: Boolean;
      FIsMobileFriendly: Boolean;
      FPanelHeight: Integer;
      FRanksFile: String;
      FVSpacing: Integer;
      FWidth: Integer;
      procedure SetCanScaleTree(AValue: Boolean);
      procedure SetDeepestRank(AValue: TTaxonomicRank);
      procedure SetDoLogScale(AValue: Boolean);
      procedure SetDoNewick(AValue: Boolean);
      procedure SetPanelHeight(AValue: Integer);
      procedure SetStudyTimeNodeHeights(AValue: TDoubleArray);
      procedure SetVSpacing(AValue: Integer);
      procedure SetWidth(AValue: Integer);
    protected
      FTreeBox: TMySvgTreeBox;
      FEarthImpacts: TEarthImpacts;
      FO2Data: TGeoData;
      FCO2Data: TGeoData;
      FLuminosityData: TGeoData;

    public

      constructor Create;
      destructor Destroy; override;
      procedure SetData(aList: TTimeTreeList; ImpactsFile: String=''; O2File: String=''; CO2File: String='');
      procedure GenerateSvgStrings(TreeFile: String; PanelsFile: String; Index: Integer=0); overload;
      procedure GenerateSvgStrings(TreeFile: String); overload;
      procedure ProgressCallback(AProgress: Integer); { a stub for TSvgTreeBox}
      procedure StatusCallback(AStatus: TTreeRebuildPhase); { a stub for TSvgTreeBox}
      procedure UpdateTaxaOrder(InputIDs: TLongIntList);
      property DeepestRank: TTaxonomicRank read FDeepestRank write SetDeepestRank;
      property RanksFile: String read FRanksFile write FRanksFile;
      property Width: Integer read FWidth write SetWidth;
      property DoLogScale: Boolean read FDoLogScale write SetDoLogScale;
      property DoNewick: Boolean read FDoNewick write SetDoNewick;
      property VSpacing: Integer read FVSpacing write SetVSpacing;
      property PanelHeight: Integer read FPanelHeight write SetPanelHeight;
      property CanScaleTree: Boolean read FCanScaleTree write SetCanScaleTree;
      property StudyTimeNodeHeights: TDoubleArray write SetStudyTimeNodeHeights;
      property IsMobileFriendly: Boolean read FIsMobileFriendly write FIsMobileFriendly;
  end;

  { TPairwiseSvgWriter }

  TPairwiseSvgWriter = class(TObject)
    private
      borderAttribs: array[0..2] of TXmlAttribute;
      lineAttribs: array[0..1] of TXmlAttribute;
      fontAttribs: array[0..1] of TXmlAttribute;
      FTimescaleLineAttribs: array[0..2] of TXMLAttribute;
      FTimescaleFontAttribs: array[0..2] of TXMLAttribute;
      FFontHeight: Integer;
      FGeoDataChartFormatter: TGeoDataChartFormatter;
      FFontName: String;
      FHeaderFontHeight: Integer;
      FHeight: Integer;
      FLinkColor: String;
      FSmallFontHeight: Integer;
      FTimeTicksPanelWidth: Integer;
      FWidth: Integer;
      FO2Data: TGeoData;
      FCO2Data: TGeoData;
      FLuminosityData: TGeoData;
      FCO2File: String;
      FImpactsFile: String;
      FEarthImpacts: TEarthImpacts;
      FO2File: String;
      function GetMaxTime: Double;
      function GetMinTime: Double;
      function GetTimespan: Double;
      procedure SetCO2File(AValue: String);
      procedure SetFontHeight(AValue: Integer);
      procedure SetFontName(AValue: String);
      procedure SetHeaderFontSize(AValue: Integer);
      procedure SetHeight(AValue: Integer);
      procedure SetImpactsFile(AValue: String);
      procedure SetLinkColor(AValue: String);
      procedure SetO2File(AValue: String);
      procedure SetSmallFontHeight(AValue: Integer);
      procedure SetTimeTicksPanelWidth(AValue: Integer);
      procedure SetWidth(AValue: Integer);
      procedure InitBorderAttribs;
      procedure InitLineAttribs;
      procedure InitFontAttribs;
      procedure InitTimescaleLineAttribs;
      procedure InitTimescaleFontAttribs;
      procedure InitXMLAttributes;
    protected
      FGeoPanelWidth: Integer;
      FMargins: TRect;
      FGeologicTimes: TCompositeGeologicTime;
      FStrings: TStringList;
      FPairwiseResult: TPairwiseResult;
      procedure InitChartFormatter;
      procedure DrawGeoDataPanelAreaChart(aColor: String);
      procedure DrawGeoDataPanelScale(aData: TGeoData; aRect: TRect);
      procedure DrawEarthImpacts;
      procedure HideImpactsOverflow(coords: TRect);
      procedure DrawEarthImpactsScale(aRect: TRect);
      procedure DrawO2;
      procedure DrawCO2;
      procedure DrawLuminosity;
      procedure DrawBorder;
      procedure DrawBackground;
      procedure DrawExtLink(x, y: Integer);
      procedure DrawGeologicTimescale;
      procedure DrawGeologicLevel(aType: TTimespanType);
      procedure DrawGeologicLevelTitle(aType: TTimespanType);
      procedure DrawTimeTicks;
      procedure AddDefs;
      procedure DrawHitRecords;
      procedure DrawResult;
      function GetTimeticksFormatString: String;
      function GetTickSpacing: Integer;
      function ShowEpochsAndAges: Boolean;
      function MyaToYCoord(mya: Double): Integer;
      function YCoordToMya(y: Integer): Double;
      function GeoPanelCoords(aType: TTimespanType): TRect;
      function GeoPanelWidth(aType: TTimespanType): Integer;
      function ResultsPanelCoords: TRect;
      function TimeTickCoords: TRect;
      function LuminosityCoords: TRect;
      function O2Coords: TRect;
      function CO2Coords: TRect;
      function ImpactCoords: TRect;
      procedure OpenSvg(const aName: String; const x: Integer; const y: Integer; const aWidth: Integer; const aHeight: Integer; const id: String='');
      procedure CloseSvg;
      function LoadEarthImpacts: Boolean;
      function LoadClimateData: Boolean;
    public
      constructor Create(aWidth: Integer; aHeight: Integer);
      destructor Destroy; override;

      function LoadPairwiseData(aData: String): Boolean;
      function GenerateSvg: TStringList;

      property Width: Integer read FWidth write SetWidth;
      property Height: Integer read FHeight write SetHeight;
      property FontHeight: Integer read FFontHeight write SetFontHeight;
      property SmallFontHeight: Integer read FSmallFontHeight write SetSmallFontHeight;
      property HeaderFontHeight: Integer read FHeaderFontHeight write SetHeaderFontSize;
      property FontName: String read FFontName write SetFontName;
      property MinTime: Double read GetMinTime;
      property MaxTime: Double read GetMaxTime;
      property TimeSpan: Double read GetTimespan;
      property TimeTicksPanelWidth: Integer read FTimeTicksPanelWidth write SetTimeTicksPanelWidth;
      property LinkColor: String read FLinkColor write SetLinkColor;
      property ImpactsFile: String read FImpactsFile write SetImpactsFile;
      property CO2File: String read FCO2File write SetCO2File;
      property O2File: String read FO2File write SetO2File;
  end;

implementation

uses
  math, gtextwidth, gsvgstrings;

{ TPairwiseSvgWriter }

procedure TPairwiseSvgWriter.SetHeight(AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

procedure TPairwiseSvgWriter.SetImpactsFile(AValue: String);
begin
  if FImpactsFile=AValue then Exit;
  FImpactsFile:=AValue;
end;

procedure TPairwiseSvgWriter.SetLinkColor(AValue: String);
begin
  if FLinkColor=AValue then Exit;
  FLinkColor:=AValue;
end;


procedure TPairwiseSvgWriter.SetO2File(AValue: String);
begin
  if FO2File=AValue then Exit;
  FO2File:=AValue;
end;

procedure TPairwiseSvgWriter.SetSmallFontHeight(AValue: Integer);
begin
  if FSmallFontHeight=AValue then Exit;
  FSmallFontHeight:=AValue;
end;

procedure TPairwiseSvgWriter.SetTimeTicksPanelWidth(AValue: Integer);
begin
  if FTimeTicksPanelWidth=AValue then Exit;
  FTimeTicksPanelWidth:=AValue;
end;

procedure TPairwiseSvgWriter.SetFontHeight(AValue: Integer);
begin
  if FFontHeight=AValue then Exit;
  FFontHeight:=AValue;
end;

procedure TPairwiseSvgWriter.SetFontName(AValue: String);
begin
  if FFontName=AValue then Exit;
  FFontName:=AValue;
end;

procedure TPairwiseSvgWriter.SetHeaderFontSize(AValue: Integer);
begin
  if FHeaderFontHeight=AValue then Exit;
  FHeaderFontHeight:=AValue;
end;

function TPairwiseSvgWriter.GetMaxTime: Double;
begin
  if CompareValue(FPairwiseResult.GetMaxTime - FPairwiseResult.GetMinTime, 1.0, FP_CUTOFF) > 0 then
    Result := Min(4600, FPairwiseResult.GetMaxTime + 1*log2(FPairwiseResult.GetMaxTime - FPairwiseResult.GetMinTime))
  else
    Result := Min(4600, FPairwiseResult.GetMaxTime*2);
end;

function TPairwiseSvgWriter.GetMinTime: Double;
begin
  if CompareValue(FPairwiseResult.GetMaxTime - FPairwiseResult.GetMinTime, 1.0, FP_CUTOFF) > 0 then
    Result := Max(0, FPairwiseResult.GetMinTime - 1*log2(FPairwiseResult.GetMaxTime - FPairwiseResult.GetMinTime))
  else
    Result := Max(0, FPairwiseResult.GetMinTime/2);
end;

function TPairwiseSvgWriter.GetTimespan: Double;
begin
  if CompareValue(MaxTime, MinTime, FP_CUTOFF) = 0 then
    Result := MinTime * 2
  else
    Result := (MaxTime - MinTime);
end;

procedure TPairwiseSvgWriter.SetCO2File(AValue: String);
begin
  if FCO2File=AValue then Exit;
  FCO2File:=AValue;
end;

procedure TPairwiseSvgWriter.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
end;

procedure TPairwiseSvgWriter.InitBorderAttribs;
begin
  borderAttribs[0].Name := 'fill';
  borderAttribs[0].Value := 'white';
  borderAttribs[1].Name := 'stroke';
  borderAttribs[1].Value := 'black';
  borderAttribs[2].Name := 'stroke-width';
  borderAttribs[2].Value := '1';
end;

procedure TPairwiseSvgWriter.InitLineAttribs;
begin
  lineAttribs[0].Name := 'stroke';
  lineAttribs[0].Value := 'black';
  lineAttribs[1].Name := 'stroke-width';
  lineAttribs[1].Value := '1';
end;

procedure TPairwiseSvgWriter.InitFontAttribs;
begin
  fontAttribs[0].Name := 'color';
  fontAttribs[0].Value := 'black';
  fontAttribs[1].Name := 'font-size';
  fontAttribs[1].Value := IntToStr(FFontHeight - 6);
end;

procedure TPairwiseSvgWriter.InitTimescaleLineAttribs;
begin
  FTimescaleLineAttribs[0].Name := 'stroke';
  FTimescaleLineAttribs[0].Value := 'black';
  FTimescaleLineAttribs[1].Name := 'stroke-width';
  FTimescaleLineAttribs[1].Value := '1';
  FTimescaleLineAttribs[2].Name := 'stroke-linecap';
  FTimescaleLineAttribs[2].Value := 'square';
end;

procedure TPairwiseSvgWriter.InitTimescaleFontAttribs;
begin
  FTimescaleFontAttribs[0].Name := 'color';
  FTimescaleFontAttribs[0].Value := 'black';
  FTimescaleFontAttribs[1].Name := 'font-size';
  FTimescaleFontAttribs[1].Value := IntToStr(FSmallFontHeight);
  FTimescaleFontAttribs[2].Name := 'font-family';
  FTimescaleFontAttribs[2].Value := 'Roboto Condensed';
end;

procedure TPairwiseSvgWriter.InitXMLAttributes;
begin
  InitBorderAttribs;
  InitLineAttribs;
  InitFontAttribs;
  InitTimescaleLineAttribs;
  InitTimescaleFontAttribs;
end;

procedure TPairwiseSvgWriter.InitChartFormatter;
begin
  FGeoDataChartFormatter.ChartHeight := FHeight;
  FGeoDataChartFormatter.ChartWidth := GEOLOGIC_PANEL_WIDTH*2 - FMargins.Left;
  FGeoDataChartFormatter.ChartMargins := Rect(2, FMargins.Top, 2, FMargins.Bottom);
  FGeoDataChartFormatter.MapAgeToCoordFunc := MyaToYCoord;
end;

procedure TPairwiseSvgWriter.DrawGeoDataPanelAreaChart(aColor: String);
var
  PolygonCoords: TArrayOfTPoint;
  Attribs: array of TXmlAttribute;
  AreaChartStr: String;
begin
  PolygonCoords := FGeoDataChartFormatter.PointsArrayForAreaChart;
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
  FStrings.Add(AreaChartStr);
end;

procedure TPairwiseSvgWriter.DrawGeoDataPanelScale(aData: TGeoData; aRect: TRect);
const
  TICK_HEIGHT = 5;
var
  MaxVal: Double;
  MinVal: Double;
  LinePoints: array[0..1] of TPoint;
  x,y: Integer;
  aText: String;
  tempStr: String;
  formatString: String;
begin
  MaxVal := aData.GetMaxValueInRange(MaxTime, MinTime, True);
  MinVal := aData.GetMinValueInRange(MaxTime, MinTime);
  if MinVal < 0.01 then
    formatString := '%6.3f'
  else if MinVal < 0.1 then
    formatString := '%6.2f'
  else
    formatString := '%6.1f';

  { draw the left tick}
  LinePoints[0].X := aRect.Left + 1;
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X := LinePoints[0].X;
  LinePoints[1].Y := aRect.Top + TICK_HEIGHT;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FStrings.Add(tempStr);


  { draw the left value}
  aText := Format(formatString, [MinVal]);
  x := aRect.Left +2;
  y :=  aRect.Top + TICK_HEIGHT * 2;
  tempStr := TextToVerticalSvgText(x, y, aText, FTimescaleFontAttribs);
  FStrings.Add(tempStr);


  { draw the center tick}
  LinePoints[0].X := Round(aRect.Left + (aRect.Right - aRect.Left) / 2);
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X := LinePoints[0].X;
  LinePoints[1].Y := LinePoints[0].Y + TICK_HEIGHT;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FStrings.Add(tempStr);

  { draw the center value}
  aText := Format(formatString + aData.LegendKey, [MinVal + (MaxVal - MinVal) / 2]);
  x := LinePoints[0].X - Round(FSmallFontHeight/3);
  y := LinePoints[1].Y + TICK_HEIGHT;
  tempStr := TextToVerticalSvgText(x, y, aText, FTimescaleFontAttribs);
  FStrings.Add(tempStr);

  { draw the right tick}
  LinePoints[0].X := aRect.Right - 4;
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X := LinePoints[0].X;
  LinePoints[1].Y := LinePoints[0].Y + TICK_HEIGHT;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FStrings.Add(tempStr);

  { draw the right value}
  aText := Format(formatString,[MaxVal]);
  x := LinePoints[0].X - Round(FSmallFontheight/2) - 2;
  y := LinePoints[1].Y + TICK_HEIGHT;
  tempStr := TextToVerticalSvgText(x, y, aText, FTimescaleFontAttribs);
  FStrings.Add(tempStr);

  { connect the ticks}
  LinePoints[0].X := aRect.Left + 2;
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X :=  aRect.Right - 4;
  LinePoints[1].Y := aRect.Top;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FStrings.Add(tempStr);
end;

procedure TPairwiseSvgWriter.DrawEarthImpacts;
var
  coords: TRect;
  scaleCoords: TRect;
  renderer: TEarthImpactsRenderer=nil;
  svgStrings: TStringList=nil;
begin
  coords := ImpactCoords;
  if not LoadEarthImpacts then
    raise Exception.Create('failed to load earth impact structure data');
  renderer := TEarthImpactsRenderer.Create(FEarthImpacts.GetEarthImpacts(MaxTime, MinTime));
  renderer.MapMyaToCoordsFunc := MyaToYCoord;
  try
    OpenSvg('pairwise-impacts', coords.Left, 0, coords.Right - coords.Left, FHeight, 'pairwise-impacts');
    coords.Left := 2;
    coords.Right := GEOLOGIC_PANEL_WIDTH*2-2;
    svgStrings := renderer.Render(tVertical, coords);
    FStrings.AddStrings(svgStrings);
    HideImpactsOverflow(coords);
    scaleCoords := Rect(0, coords.Bottom + 2, coords.Right - coords.Left, coords.Bottom + FMargins.Bottom);
    scaleCoords.Right := scaleCoords.Right + 2;
    DrawEarthImpactsScale(scaleCoords);
    CloseSvg;
  finally
    if Assigned(renderer) then
      renderer.Free;
    if Assigned(svgStrings) then
      svgStrings.Free;
  end;
end;

procedure TPairwiseSvgWriter.HideImpactsOverflow(coords: TRect);
var
  rectAttribs: array[0..2] of TXmlAttribute;
  overflowCoords: TRect;
  svgTag: String;
begin
  rectAttribs[0].Name := 'fill';
  rectAttribs[0].Value := 'white';
  rectAttribs[1].Name := 'stroke';
  rectAttribs[1].Value := 'white';
  rectAttribs[2].Name := 'stroke-width';
  rectAttribs[2].Value := '1';
  overflowCoords.Left := coords.Left - 2;
  overflowCoords.Right := coords.Right;
  overflowCoords.Bottom := coords.Top;
  overflowCoords.Top := coords.Top - FMargins.Top + 2;
  svgTag := RectToSvgRect(overflowCoords, rectAttribs);
  FStrings.Add(svgTag);
  overflowCoords.Top := coords.Bottom;
  overflowCoords.Bottom := coords.Bottom + FMargins.Bottom;
  svgTag := RectToSvgRect(overflowCoords, rectAttribs);
  FStrings.Add(svgTag);
end;

procedure TPairwiseSvgWriter.DrawEarthImpactsScale(aRect: TRect);
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
  FStrings.Add(tempStr);


  { draw the left value}
  aText := Format('%d', [200]);
  x := aRect.Left + 2;
  y :=  aRect.Top + TICK_HEIGHT * 2;
  tempStr := TextToVerticalSvgText(x, y, aText, FTimescaleFontAttribs);
  FStrings.Add(tempStr);


  { draw the center tick}
  LinePoints[0].X := Round(aRect.Left + (aRect.Right - aRect.Left) / 2);
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X := LinePoints[0].X;
  LinePoints[1].Y := LinePoints[0].Y + TICK_HEIGHT;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FStrings.Add(tempStr);

  { draw the center value}
  aText := Format('%d km', [0]);
  x := LinePoints[0].X - Round(FSmallFontHeight/3);
  y := LinePoints[1].Y + TICK_HEIGHT;
  tempStr := TextToVerticalSvgText(x, y, aText, FTimescaleFontAttribs);
  FStrings.Add(tempStr);

  { draw the right tick}
  LinePoints[0].X := aRect.Right;
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X := LinePoints[0].X;
  LinePoints[1].Y := LinePoints[0].Y + TICK_HEIGHT;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FStrings.Add(tempStr);

  { draw the right value}
  aText := Format('%d',[200]);
  x := LinePoints[0].X - Round(FSmallFontheight/2) - 2;
  y := LinePoints[1].Y + TICK_HEIGHT;
  tempStr := TextToVerticalSvgText(x, y, aText, FTimescaleFontAttribs);
  FStrings.Add(tempStr);

  { connect the ticks}
  LinePoints[0].X := aRect.Left + 2;
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X :=  aRect.Right;
  LinePoints[1].Y := aRect.Top;
  tempStr := PointsToSvgLine(LinePoints, FTimescaleLineAttribs);
  FStrings.Add(tempStr);
end;

procedure TPairwiseSvgWriter.DrawO2;
var
  coords: TRect;
  ScaleCoords: TRect;
begin
  coords := O2Coords;
  FGeoDataChartFormatter.BuildAreaChartVertical(FO2Data, MaxTime, MinTime, Rect(0, 0, 0, 0));
  OpenSvg('pairwise-o2', coords.Left, 0, GEOLOGIC_PANEL_WIDTH*2, FHeight, 'pairwise-o2');
  DrawGeoDataPanelAreaChart(FO2Data.ChartColor);
  ScaleCoords := Rect(0, coords.Bottom + 2, coords.Right - coords.Left, coords.Bottom + FMargins.Bottom);
  DrawGeoDataPanelScale(FO2Data, scaleCoords);
  CloseSvg;
end;

procedure TPairwiseSvgWriter.DrawCO2;
var
  coords: TRect;
  ScaleCoords: TRect;
begin
  coords := CO2Coords;
  FGeoDataChartFormatter.BuildAreaChartVertical(FCO2Data, MaxTime, MinTime, Rect(0, 0, 0, 0));
  OpenSvg('pairwise-co2', coords.Left, 0, GEOLOGIC_PANEL_WIDTH*2, FHeight, 'pairwise-co2');
  DrawGeoDataPanelAreaChart(FCO2Data.ChartColor);
  ScaleCoords := Rect(0, coords.Bottom + 2, coords.Right - coords.Left, coords.Bottom + FMargins.Bottom);
  DrawGeoDataPanelScale(FCO2Data, scaleCoords);
  CloseSvg;
end;

procedure TPairwiseSvgWriter.DrawLuminosity;
var
  coords: TRect;
  scaleCoords: TRect;
begin
  coords := LuminosityCoords;
  FGeoDataChartFormatter.BuildAreaChartVertical(FLuminosityData, MaxTime, MinTime, Rect(0, 0, 0, 0), (MaxTime < START_OF_PHANEROZOIC) or (MinTime > START_OF_PHANEROZOIC));
  OpenSvg('pairwise-luminosity', coords.Left, 0, GEOLOGIC_PANEL_WIDTH*2, FHeight, 'pairwise-luminosity');
  DrawGeoDataPanelAreaChart(FLuminosityData.ChartColor);
  ScaleCoords := Rect(0, coords.Bottom + 2, coords.Right - coords.Left, coords.Bottom + FMargins.Bottom);
  DrawGeoDataPanelScale(FLuminosityData, scaleCoords);
  CloseSvg;
end;

procedure TPairwiseSvgWriter.DrawBorder;
var
  aRect: TRect;
  attribs: array[0..4] of TXmlAttribute;
  Temp: String;
begin
  aRect.Top := 1;
  aRect.Bottom := Height-1;
  aRect.Left := 0;
  aRect.Right := Width-1;

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
  FStrings.Add(Temp);
end;

procedure TPairwiseSvgWriter.DrawBackground;
var
  Temp: String;
begin
  Temp := '<image width=' + dblq + '500' + dblq + ' ';
  Temp := Temp + 'height=' + dblq + '600' + dblq + ' ';
  Temp := Temp + 'xlink:href=' + dblq + 'data:image/png;base64,' + TIMELINE_BG_IMG + dblq + '></image>';
  FStrings.Add(Temp);
end;

procedure TPairwiseSvgWriter.DrawExtLink(x, y: Integer);
var
  img: String;
  Temp: String;
begin
  img := 'iVBORw0KGgoAAAANSUhEUgAAAAwAAAAMCAYAAABWdVznAAAAV0lEQVR4Xq2QwQ2AAAwC3cmd2Kk7sRP64CEJ9qOX8OPatMc/QKppnEPhTmJh23CLiwAqIw21CybKQ28qQi37WGFYBJcwfJQpP8LlEHKyZMF0IdmF13zlAjZ/6H4wb+mUAAAAAElFTkSuQmCC';
  Temp := '<image width=' + dblq + '12' + dblq + ' ';
  Temp := Temp + 'height=' + dblq + '12' + dblq + ' ';
  Temp := Temp + 'x=' + dblq + IntToStr(x) + dblq + ' ';
  Temp := Temp + 'y=' + dblq + IntToStr(y) + dblq + ' ';
  Temp := Temp + 'xlink:href=' + dblq + 'data:image/png;base64,' + img + dblq + '></image>';
  FStrings.Add(Temp);
end;

procedure TPairwiseSvgWriter.DrawGeologicTimescale;
begin
  OpenSvg('eons', GeoPanelCoords(tstEon).Left, 0, FGeoPanelWidth, Height, 'pairwise-eons');
  DrawGeologicLevel(tstEon);
  CloseSvg;
  OpenSvg('eras', GeoPanelCoords(tstEra).Left, 0, FGeoPanelWidth, Height, 'pairwise-eras');
  DrawGeologicLevel(tstEra);
  CloseSvg;
  OpenSvg('periods', GeoPanelCoords(tstPeriod).Left, 0, FGeoPanelWidth, Height, 'pairwise-periods');
  DrawGeologicLevel(tstPeriod);
  CloseSvg;
  if ShowEpochsAndAges then
  begin
    OpenSvg('epochs', GeoPanelCoords(tstEpoch).Left, 0, GeoPanelWidth(tstEpoch), Height, 'pairwise-epochs');
    DrawGeologicLevel(tstEpoch);
    CloseSvg;
    OpenSvg('ages', GeoPanelCoords(tstAge).Left, 0, GeoPanelWidth(tstAge), Height, 'pairwise-ages');
    DrawGeologicLevel(tstAge);
    CloseSvg;
  end;
end;

procedure TPairwiseSvgWriter.DrawGeologicLevel(aType: TTimespanType);
var
  aRect: TRect;
  i: Integer;
  times: TArrayOfGeologicTime;
  rectAttribs: array of TXMLAttribute;
  textAttribs: array of TXMLAttribute;
  Temp: String;
begin
  aRect.Left := 0;
  aRect.Right := aRect.Left + GeoPanelWidth(aType);
  aRect.Top := FMargins.Top;
  aRect.Bottom := Height - FMargins.Bottom;

  times := FGeologicTimes.GetTimescale(MinTime, MaxTime, aType);

  if Length(times) > 0 then
  begin
    { draw the border}
    SetLength(rectAttribs, 3);
    rectAttribs[0].Name := 'fill';
    rectAttribs[0].Value := 'none';
    rectAttribs[1].Name := 'stroke';
    rectAttribs[1].Value := 'black';
    rectAttribs[2].Name := 'stroke-width';
    rectAttribs[2].Value := '1';
    FStrings.Add(RectToSvgRect(aRect, rectAttribs));

    SetLength(textAttribs, 7);
    textAttribs[0].Name := 'full-name';
    textAttribs[1].Name := 'font-family';
    textAttribs[1].Value := 'Roboto Condensed';
    textAttribs[2].Name := 'font-size';
    textAttribs[2].Value := IntToStr(FFontHeight);
    textAttribs[3].Name := 'text-anchor';
    textAttribs[3].Value := 'middle';
    textAttribs[4].Name := 'start';
    textAttribs[5].Name := 'end';
    textAttribs[6].Name := 'unit';

    { draw the rectangles}
    SetLength(rectAttribs, 7);
    rectAttribs[3].Name := 'full-name';
    rectAttribs[4].Name := 'start';
    rectAttribs[5].Name := 'end';
    rectAttribs[6].Name := 'unit';
    for i := Length(times) - 1 downto 0 do
    begin
      aRect.Bottom := MyaToYCoord(times[i].StartMya);
      aRect.Top := MyaToYCoord(times[i].EndMya);
      rectAttribs[0].Value := times[i].HexaDecimalColorStr;
      rectAttribs[1].Value := times[i].HexaDecimalColorStr;
      rectAttribs[3].Value := times[i].Name;
      rectAttribs[4].Value := Format('%.2f', [times[i].StartMya]);
      rectAttribs[5].Value := Format('%.2f', [times[i].EndMya]);
      rectAttribs[6].Value := TimespanTypeString(times[i].TimespanType);
      Temp := RectToSvgRect(aRect, rectAttribs);
      FStrings.Add(Temp);

      textAttribs[0].Value := times[i].Name;
      textAttribs[4].Value := Format('%.2f', [times[i].StartMya]);
      textAttribs[5].Value := Format('%.2f', [times[i].EndMya]);
      textAttribs[6].Value := TimespanTypeString(times[i].TimespanType);
      case aType of
        tstEon, tstEra, tstPeriod:
          begin
            Temp := AddVerticalSvgTextToRect(aRect, times[i].GetBestFitText(aRect, tdVertical), '16', textAttribs);
          end;
        tstEpoch, tstAge:
          begin
            if ShowEpochsAndAges then
            begin
              if (aRect.Bottom - aRect.Top) < CustomTextHeight(times[i].Name, FSmallFontHeight) then
                continue;
              textAttribs[2].Value := IntToStr(FSmallFontHeight);
              Temp := AddSvgTextToRect(aRect, times[i].GetBestFitText(aRect, tdHorizontal), textAttribs);
            end;
          end;
      end;

      if Temp <> EmptyStr then
        FStrings.Add(Temp);
    end;
  end;
  DrawGeologicLevelTitle(aType);
end;

procedure TPairwiseSvgWriter.DrawGeologicLevelTitle(aType: TTimespanType);
var
  x, y: Integer;
  svgTag: String;
  GeologicLevelName: String;
begin
  GeologicLevelName := TimespanTypeString(aType) + 's';
  x := (GeoPanelWidth(aType) div 2) - (CustomTextHeight(GeologicLevelName) div 4);
  y := FHeight - FMargins.Bottom + 2 + 8;
  svgTag := TextToVerticalSvgText(x, y, GeologicLevelName, FTimescaleFontAttribs);
  FStrings.Add(svgTag);
end;

procedure TPairwiseSvgWriter.DrawTimeTicks;
var
  aRect: TRect;
  Temp: String;
  tickSpacing: Integer;
  i, x, y: Integer;
  p: array [0..1] of TPoint;
  mya: Double;
  ticksFormatString: String;
begin
  InitLineAttribs;
  InitFontAttribs;
  InitBorderAttribs;
  AddDefs; { draws gradient definitions}

  { first, draw the border}
  aRect := TimeTickCoords;
  aRect.Left := 0;
  aRect.Right := FTimeTicksPanelWidth-1;
  Temp := RectToSvgRect(aRect, borderAttribs);
  FStrings.Add(Temp);
  borderAttribs[1].Value := 'white';
  aRect.Right := aRect.Right - 1;
  Temp := RectToSvgRect(aRect, borderAttribs);
  FStrings.Add(Temp);

  { draw the time scale}
  p[0].X := 4;
  p[1].X := 8;
  tickSpacing := GetTickSpacing;
  ticksFormatString := GetTimeticksFormatString;
  i := FMargins.Top + FSmallFontHeight;
  while i < (Height - FMargins.Bottom) do
  begin
    p[0].Y := i;
    p[1].Y := i;
    Temp := PointsToSvgLine(p, lineAttribs);
    FStrings.Add(Temp);
    mya := YCoordToMya(i);
    Temp := TextToSvgText(p[1].X + 4, p[0].Y + (FFontHeight div 4), Format(ticksFormatString, [mya]), fontAttribs);
    FStrings.Add(Temp);
    inc(i, tickSpacing);
  end;
  x := Round((aRect.Right - aRect.Left)/2 - (CustomTextHeight('MYA') div 4));
  y := aRect.Bottom + 10;
  Temp := TextToVerticalSvgText(x, y, 'MYA', FTimescaleFontAttribs);
  FStrings.Add(Temp);
end;

procedure TPairwiseSvgWriter.AddDefs;
var
  Temp: String;
begin
  Temp := '<defs>';
  Temp := Temp + '<linearGradient id=' + dblq + 'dotsGradient' + dblq + ' x1=' + dblq + '0' + dblq + ' x2=' + dblq + '0' + dblq + ' y1=' + dblq + '0' + dblq + ' y2=' + dblq + '1' + dblq + '>';
  Temp := Temp + '<stop offset=' + dblq + '0%' + dblq + ' stop-color=' + dblq + 'dddddd' + dblq + '/>';
  Temp := Temp + '<stop offset=' + dblq + '100%' + dblq + ' stop-color=' + dblq + '#777777' + dblq + '/>';
  Temp := Temp + '</linearGradient>';
  Temp := Temp + '</defs>';
  FStrings.Add(Temp);

  Temp := '<defs>';
  Temp := Temp + '<linearGradient id=' + dblq + 'boxGradient' + dblq + ' x1=' + dblq + '0' + dblq + ' x2=' + dblq + '0' + dblq + ' y1=' + dblq + '0' + dblq + ' y2=' + dblq + '1' + dblq + '>';
  Temp := Temp + '<stop offset=' + dblq + '0%' + dblq + ' stop-color=' + dblq + '#fefcea' + dblq + '/>';
  Temp := Temp + '<stop offset=' + dblq + '100%' + dblq + ' stop-color=' + dblq + '#f1da36' + dblq + '/>';
  Temp := Temp + '</linearGradient>';
  Temp := Temp + '</defs>';
  FStrings.Add(Temp);
end;

procedure TPairwiseSvgWriter.DrawHitRecords;
var
  x,y,r, i: Integer;
  aRect: TRect;
  Temp: String;
  attribs: array[0..8] of TXmlAttribute;
  hitRecord: TTimetreeHitRecord;
begin


  { draw the hit records}
  if FPairwiseResult.HitRecords.Count > 0 then
  begin
    x := 4 + 8 + 4 + 30;
    r := 6;
    attribs[0].Name := 'fill';
    attribs[0].Value := 'url(#dotsGradient)';
    for i := 0 to FPairwiseResult.HitRecords.Count - 1 do
    begin
      hitRecord := TTimetreeHitRecord(FPairwiseResult.HitRecords[i]);
      y := MyaToYCoord(hitRecord.Time);
      attribs[1].Name := 'abstract';
      attribs[1].Value := hitRecord.Abstr;
      attribs[2].Name := 'author';
      attribs[2].Value := hitRecord.Author;
      attribs[3].Name := 'cit-number';
      attribs[3].Value := IntToStr(hitRecord.CitationNum);
      attribs[4].Name := 'pubmed-id';
      attribs[4].Value := IntToStr(hitRecord.PubmedId);
      attribs[5].Name := 'ref-id';
      attribs[5].Value := hitRecord.RefId;
      attribs[6].Name := 'time';
      attribs[6].Value := FloatToStr(hitRecord.Time);
      attribs[7].Name := 'year';
      attribs[7].Value := IntToStr(hitRecord.Year);
      attribs[8].Name := 'title';
      attribs[8].Value := hitRecord.Title;
      Temp := CircleToSvgCircle(x, y, r, attribs);
      FStrings.Add(Temp);
    end;
  end;
end;

procedure TPairwiseSvgWriter.DrawResult;
var
  aRect: TRect;
  RectStr, TextStr: String;
  TempRect: TRect;
  aWidth: Integer;
  rectAttribs: array of TXmlAttribute;
  textAttribs: array of TXmlAttribute;
  ciLowStr, ciHighStr, divTimeStr, medianTimeStr: String;
begin
  aRect := ResultsPanelCoords;
  TempRect.Left := 0;
  aWidth := (aRect.Right - aRect.Left - 4);
  TempRect.Right := aWidth;

  { draw the background rectangle and common name for taxon A}
  TempRect.Top := aRect.Top + 20;
  TempRect.Bottom := TempRect.Top + 30;

  SetLength(rectAttribs, 3);
  rectAttribs[0].Name := 'fill';
  rectAttribs[0].Value := 'url(#boxGradient)';
  rectAttribs[1].Name := 'stroke';
  rectAttribs[1].Value := '#cccccc';
  rectAttribs[2].Name := 'stroke-width';
  rectAttribs[2].Value := '1';

  SetLength(textAttribs, 4);
  textAttribs[0].Name := 'font-family';
  textAttribs[0].Value := 'Roboto Condensed';
  textAttribs[1].Name := 'font-size';
  textAttribs[1].Value := IntToStr(FHeaderFontHeight);
  textAttribs[2].Name := 'text-anchor';
  textAttribs[2].Value := 'middle';
  textAttribs[3].Name := 'fill';
  textAttribs[3].Value := 'black';

  if FPairwiseResult.TaxonA.CommonName <> EmptyStr then { only draw it if it is available}
  begin
    if CustomTextWidth(FPairwiseResult.TaxonA.CommonName, 10) >= (aWidth - 20) then
    begin
      TextStr := WrapTextInTSpan(FPairwiseResult.TaxonA.CommonName, (aWidth div 2), FHeaderFontHeight  + 1, 10);
      TempRect.Bottom := TempRect.Bottom + (NumOccurences(TextStr, '</tspan>', True) * FHeaderFontHeight);
      TextStr := AddWrappedSvgTextToRect(TempRect, TextStr, textAttribs, FHeaderFontHeight);
    end
    else
      TextStr := AddSvgTextToRect(TempRect, FPairwiseResult.TaxonA.CommonName, textAttribs, FHeaderFontHeight);
    RectStr := RectToSvgRect(TempRect, rectAttribs);
    FStrings.Add(RectStr);
    FStrings.Add(TextStr);
  end;

  { draw the link for taxon A}
  SetLength(textAttribs, 8);
  textAttribs[0].Value := 'Verdana';
  textAttribs[1].Value := IntToStr(FFontHeight);
  textAttribs[3].Value := FLinkColor;
  textAttribs[4].Name := 'text-decoration';
  textAttribs[4].Value := 'underline';
  textAttribs[5].Name := 'url';
  textAttribs[5].Value := FPairwiseResult.TaxonA.Link;
  textAttribs[6].Name := 'font-style';
  textAttribs[6].Value := 'italic';
  textAttribs[7].Name := 'font-weight';
  textAttribs[7].Value := 'bold';
  TempRect.Top :=  TempRect.Bottom + 10;
  TempRect.Bottom := TempRect.Top + 20;
  if CustomTextWidth(FPairwiseResult.TaxonA.ScientificName, 10) >= (aWidth - 20) then
  begin
    TextStr := WrapTextInTSpan(FPairwiseResult.TaxonA.ScientificName, aWidth div 2, FFontHeight  + 1, 10);
    TempRect.Bottom := TempRect.Bottom + (NumOccurences(TextStr, '</tspan>', True) * FFontHeight);
    TextStr := AddWrappedSvgTextToRect(TempRect, TextStr, textAttribs, FFontHeight);
  end
  else
    TextStr := AddSvgTextToRect(TempRect, FPairwiseResult.TaxonA.ScientificName, textAttribs, FFontHeight);
  FStrings.Add(TextStr);
  DrawExtLink(TempRect.Right - 14, TempRect.Top + ((TempRect.Bottom - TempRect.Top) div 2) -12 + (FFontHeight div 4));

  { draw 'versus'}
  TempRect.Top := TempRect.Bottom + 20;
  TempRect.Bottom := TempRect.Top + 30;
  SetLength(textAttribs, 3);
  TextStr := AddSvgTextToRect(TempRect, 'Versus', textAttribs, FFontHeight);
  FStrings.Add(TextStr);

  SetLength(textAttribs, 4);
  textAttribs[0].Name := 'font-family';
  textAttribs[0].Value := 'Roboto Condensed';
  textAttribs[1].Name := 'font-size';
  textAttribs[1].Value := IntToStr(FHeaderFontHeight);
  textAttribs[2].Name := 'text-anchor';
  textAttribs[2].Value := 'middle';
  textAttribs[3].Name := 'fill';
  textAttribs[3].Value := 'black';

  if FPairwiseResult.TaxonB.CommonName <> EmptyStr then
  begin
    { draw the rectangle and common name for taxon B}
    TempRect.Top := TempRect.Bottom + 20;
    TempRect.Bottom := TempRect.Top + 30;

    if CustomTextWidth(FPairwiseResult.TaxonB.CommonName, 10) >= (aWidth - 20) then
    begin
      TextStr := WrapTextInTSpan(FPairwiseResult.TaxonB.CommonName, (aWidth div 2), FHeaderFontHeight  + 1, 10);
      TempRect.Bottom := TempRect.Bottom + (NumOccurences(TextStr, '</tspan>', True) * FHeaderFontHeight);
      TextStr := AddWrappedSvgTextToRect(TempRect, TextStr, textAttribs, FHeaderFontHeight);
    end
    else
      TextStr := AddSvgTextToRect(TempRect, FPairwiseResult.TaxonB.CommonName, textAttribs, FHeaderFontHeight);
    RectStr := RectToSvgRect(TempRect, rectAttribs);
    FStrings.Add(RectStr);
    FStrings.Add(TextStr);
  end;

  { draw the link for taxon B}
  SetLength(textAttribs, 8);
  textAttribs[0].Value := 'Verdana';
  textAttribs[1].Value := IntToStr(FFontHeight);
  textAttribs[3].Value := FLinkColor;
  textAttribs[4].Name := 'text-decoration';
  textAttribs[4].Value := 'underline';
  textAttribs[5].Name := 'url';
  textAttribs[5].Value := FPairwiseResult.TaxonB.Link;
  textAttribs[6].Name := 'font-style';
  textAttribs[6].Value := 'italic';
  textAttribs[7].Name := 'font-weight';
  textAttribs[7].Value := 'bold';
  TempRect.Top :=  TempRect.Bottom + 10;
  TempRect.Bottom := TempRect.Top + 20;

  if CustomTextWidth(FPairwiseResult.TaxonB.ScientificName) >= (aWidth - 20) then
  begin
    TextStr := WrapTextInTSpan(FPairwiseResult.TaxonB.ScientificName, (aWidth div 2), FFontHeight  + 1, 10);
    TempRect.Bottom := TempRect.Bottom + (NumOccurences(TextStr, '</tspan>', True) * FFontHeight);
    TextStr := AddWrappedSvgTextToRect(TempRect, TextStr, textAttribs, FFontHeight);
  end
  else
    TextStr := AddSvgTextToRect(TempRect, FPairwiseResult.TaxonB.ScientificName, textAttribs, FFontHeight);
  FStrings.Add(TextStr);
  DrawExtLink(TempRect.Right - 14, TempRect.Top + ((TempRect.Bottom - TempRect.Top) div 2) -12 + (FFontHeight div 4));

  { draw the median time}
  FormatTimeIntervalStrings(FPairwiseResult.MedianTime, FPairwiseResult.MolecularTime, FPairwiseResult.CILow, FPairwiseResult.CIHigh, medianTimeStr, divTimeStr, ciLowStr, ciHighStr);
  TempRect.Top := TempRect.Bottom + 20;
  TempRect.Bottom := TempRect.Top + 20;
  SetLength(textAttribs, 4);
  textAttribs[0].Name := 'font-family';
  textAttribs[0].Value := 'Roboto Condensed';
  textAttribs[1].Name := 'font-size';
  textAttribs[1].Value := IntToStr(FFontHeight);
  textAttribs[2].Name := 'text-anchor';
  textAttribs[2].Value := 'middle';
  textAttribs[3].Name := 'fill';
  textAttribs[3].Value := 'black';
  TextStr := AddSvgTextToRect(TempRect, 'Median Time:', textAttribs, FFontHeight);
  FStrings.Add(TextStr);

  TempRect.Top := TempRect.Bottom;
  TempRect.Bottom := TempRect.Top + FFontHeight;

  TextStr := Format('%s MYA', [medianTimeStr]);
  if CustomTextWidth(TextStr) >= aWidth then
  begin
    TextStr := WrapTextInTSpan(TextStr, aWidth div 2, FFontHeight  + 1, 8);
    TempRect.Bottom := TempRect.Bottom + (NumOccurences(TextStr, '</tspan>', True) * FFontHeight);
    TextStr := AddWrappedSvgTextToRect(TempRect, TextStr, textAttribs, FFontHeight);
  end
  else
    TextStr := AddSvgTextToRect(TempRect, TextStr, textAttribs, FFontHeight);
  FStrings.Add(TextStr);

  { draw the div time}
  TempRect.Top := TempRect.Bottom + 20;
  TempRect.Bottom := TempRect.Top + FFontHeight;
  TextStr := AddSvgTextToRect(TempRect, 'Estimated Time:', textAttribs, FFontHeight);
  FStrings.Add(TextStr);

  TempRect.Top := TempRect.Bottom;
  TempRect.Bottom := TempRect.Top + FFontHeight;
  TextStr := Format('%s MYA', [divTimeStr]);
  if CustomTextWidth(TextStr) >= aWidth then
  begin
    TextStr := WrapTextInTSpan(TextStr, aWidth div 2, FFontHeight  + 1, 8);
    TempRect.Bottom := TempRect.Bottom + (NumOccurences(TextStr, '</tspan>', True) * FFontHeight);
    TextStr := AddWrappedSvgTextToRect(TempRect, TextStr, textAttribs, FFontHeight);
  end
  else
    TextStr := AddSvgTextToRect(TempRect, TextStr, textAttribs, FFontHeight);
  FStrings.Add(TextStr);

  TempRect.Top := TempRect.Bottom;
  TempRect.Bottom := TempRect.Top + FFontHeight + 10;
  if FPairwiseResult.CIHigh > 0 then
  begin
    if FPairwiseResult.IsCI then
      TextStr := Format('CI: (%s - %s MYA)', [ciLowStr, ciHighStr])
    else
      TextStr := Format('Range: (%s - %s MYA)', [ciLowStr, ciHighStr]);
  end
  else
    TextStr := 'CI: (n/a)';
  //textAttribs[1].Value := IntToStr(FFontHeight);
  if CustomTextWidth(TextStr) < aWidth then
    TextStr := AddWrappedSvgTextToRect(TempRect, TextStr, textAttribs, FFontHeight)
  else
    TextStr := AddSvgTextToRect(TempRect, TextStr, textAttribs, FFontHeight);
  FStrings.Add(TextStr);

  { draw the TTOL link}
  TempRect.Top := TempRect.Bottom;
  TempRect.Bottom := TempRect.Top + FFontHeight;
  SetLength(textAttribs, 7);
  textAttribs[1].Value := IntToStr(FFontHeight);
  textAttribs[3].Value := FLinkColor;
  textAttribs[4].Name := 'text-decoration';
  textAttribs[4].Value := 'underline';
  textAttribs[5].Name := 'url';
  textAttribs[5].Value := 'http://www.kumarlab.net/downloads/papers/Mol%20Biol%20Evol-2015-Hedges-835-45.pdf';
  textAttribs[6].Name := 'font-weight';
  textAttribs[6].Value := 'bold';
  TextStr := AddSvgTextToRect(TempRect, '(TTOL)', textAttribs, FFontHeight);
  FStrings.Add(TextStr);
  DrawExtLink(TempRect.Right - (aWidth div 2) + (CustomTextWidth('(TTOL)') div 2) + 5, TempRect.Top + ((TempRect.Bottom - TempRect.Top) div 2) -12 + (FFontHeight div 4));

  { draw the number of studies}
  SetLength(textAttribs, 4);
  textAttribs[0].Name := 'font-family';
  textAttribs[0].Value := 'Roboto Condensed';
  textAttribs[1].Name := 'font-size';
  textAttribs[1].Value := IntToStr(FFontHeight - 2);
  textAttribs[2].Name := 'text-anchor';
  textAttribs[2].Value := 'middle';
  textAttribs[3].Name := 'fill';
  textAttribs[3].Value := 'black';
  TempRect.Top := TempRect.Bottom + 30;
  TempRect.Bottom := TempRect.Bottom + FFontHeight;
  if FPairwiseResult.NumStudies = 1 then
    TextStr := Format('(Median and estimated times were derived from %d study)', [FPairwiseResult.NumStudies])
  else
    TextStr := Format('(Median and estimated times were derived from %d studies)', [FPairwiseResult.NumStudies]);
  TextStr := WrapTextInTSpan(TextStr, (aWidth div 2), FFontHeight  + 1, 10);
  TempRect.Bottom := TempRect.Bottom + (NumOccurences(TextStr, '</tspan>', True) * FFontHeight);

  TextStr := AddWrappedSvgTextToRect(TempRect, TextStr, textAttribs, FFontHeight);
  FStrings.Add(TextStr);
  if FPairwiseResult.NumOutliers > 0 then
  begin
    if FPairwiseResult.NumOutliers = 1 then
      TextStr := 'Note: 1 outlier study time is not displayed.'
    else
      TextStr := Format('Note: %d outlier study times are not displayed', [FPairwiseResult.NumOutliers]);
    TextStr := WrapTextInTSpan(TextStr, (aWidth div 2), FFontHeight + 1, 10);
    TempRect.Top := TempRect.Bottom + 30;
    TempRect.Bottom := TempRect.Bottom + (NumOccurences(TextStr, '</tspan>', True) * FFontHeight*2);
    TextStr := AddWrappedSvgTextToRect(TempRect, TextStr, textAttribs, FFontHeight);
    FStrings.Add(TextStr);
  end;
end;

function TPairwiseSvgWriter.GetTimeticksFormatString: String;
var
  i: Integer;
  tickSpacing: Integer;
  SameValsFound: Boolean;
  ValsLT1Found, ValsLT01Found: Boolean;
  mya: Double;
begin
  SameValsFound := False;
  ValsLT1Found := False;
  ValsLT01Found := False;
  Result := '%.0f';
  tickSpacing := GetTickSpacing;
  i := FMargins.Top + FSmallFontHeight;
  while i < (Height - FMargins.Bottom) do
  begin
    mya := YCoordToMya(i);
    if ((i - tickSpacing) > 0) and (Round(mya) = Round(YCoordToMya(i-tickSpacing))) then
      SameValsFound := True;
    if mya < 1.0 then
      ValsLT1Found := True;
    if mya < 0.1 then
      ValsLT01Found := True;
    inc(i, tickSpacing);
  end;

  if ValsLT01Found then
    Result := '%.3e'
  else if ValsLT1Found then
    Result := '%.2f'
  else if SameValsFound then
    Result := '%.1f';
end;

function TPairwiseSvgWriter.GetTickSpacing: Integer;
begin
  Result := Max((Height - FMargins.Top - FMargins.Bottom) div 20, 10);
end;

function TPairwiseSvgWriter.ShowEpochsAndAges: Boolean;
begin
  Result := (FPairwiseResult.GetMaxTime <= 542.0);
end;

function TPairwiseSvgWriter.MyaToYCoord(mya: Double): Integer;
begin
  if mya > MinTime then
  begin
    if TimeSpan > 0.0 then
      Result := Min(Height - FMargins.Bottom, FMargins.Top + Round((mya - MinTime) / TimeSpan * (Height - FMargins.Top - FMargins.Bottom)))
    else
      Result := FMargins.Top;
  end
  else
    Result := FMargins.Top;
end;

function TPairwiseSvgWriter.YCoordToMya(y: Integer): Double;
begin
  Assert((y >= FMargins.Top) and (y < (Height - FMargins.Bottom)));
  if CompareValue(TimeSpan, 0.0, FP_CUTOFF) > 0.0 then
    Result := (y - FMargins.Top)/(Height - FMargins.Top - FMargins.Bottom) * TimeSpan + MinTime
  else
  begin
    Result := MinTime;
  end;
end;

function TPairwiseSvgWriter.GeoPanelCoords(aType: TTimespanType): TRect;
const
  LEFT_PADDING = 2;

begin
  Result.Top := 0;
  Result.Bottom := (Height - FMargins.Bottom) + 8;
  case aType of
    tstEon:
      begin
        Result.Left := ImpactCoords.Right + LEFT_PADDING;
      end;
    tstEra:
      begin
        Result.Left := ImpactCoords.Right + LEFT_PADDING + GEOLOGIC_PANEL_WIDTH;
      end;
    tstPeriod:
      begin
        Result.Left := ImpactCoords.Right + LEFT_PADDING + GEOLOGIC_PANEL_WIDTH * 2;
      end;
    tstEpoch:
      begin
        Result.Left := ImpactCoords.Right + LEFT_PADDING + GEOLOGIC_PANEL_WIDTH * 3;
      end;
    tstAge:
      begin
        Result.Left := ImpactCoords.Right + LEFT_PADDING + GEOLOGIC_PANEL_WIDTH * 3 + CustomTextWidth(FGeologicTimes.GetLongestName(FPairwiseResult.GetMinTime, FPairwiseResult.GetMaxTime, tstEpoch), 6) + 8;
      end;
  end;
  Result.Right := Result.Left + GEOLOGIC_PANEL_WIDTH;
  case aType of
    tstEon, tstEra, tstPeriod: Result.Right := Result.Left + FGeoPanelWidth;
    tstEpoch, tstAge: Result.Right := Result.Left + CustomTextWidth(FGeologicTimes.GetLongestName(FPairwiseResult.GetMinTime, FPairwiseResult.GetMaxTime, aType), 6) + 8;
  end;
end;

function TPairwiseSvgWriter.GeoPanelWidth(aType: TTimespanType): Integer;
var
  aRect: TRect;
begin
  aRect := GeoPanelCoords(aType);
  Result := (aRect.Right - aRect.Left);
end;

function TPairwiseSvgWriter.ResultsPanelCoords: TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := (Height - FMargins.Bottom) + 8;
  Result.Left := GeoPanelCoords(tstPeriod).Right + TimeTicksPanelWidth + 4;
  if ShowEpochsAndAges then
    Result.Left := (Result.Left + GeoPanelWidth(tstEpoch) + GeoPanelWidth(tstAge));
  Result.Right := (Width - 1)
end;

function TPairwiseSvgWriter.TimeTickCoords: TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := (FHeight - FMargins.Bottom);
  Result.Left := GeoPanelCoords(tstPeriod).Right;
  if ShowEpochsAndAges then
    Result.Left := Result.Left + GeoPanelWidth(tstEpoch) + GeoPanelWidth(tstAge);
  Result.Right := Result.Left + FTimeTicksPanelWidth;
end;

function TPairwiseSvgWriter.LuminosityCoords: TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := FHeight - FMargins.Bottom;
  Result.Left := FMargins.Left + 5;
  Result.Right := Result.Left + GEOLOGIC_PANEL_WIDTH*2;
end;

function TPairwiseSvgWriter.O2Coords: TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := FHeight - FMargins.Bottom;
  Result.Left := FMargins.Left + 5 + GEOLOGIC_PANEL_WIDTH*4;
  Result.Right := Result.Left + GEOLOGIC_PANEL_WIDTH*2;
end;

function TPairwiseSvgWriter.CO2Coords: TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := FHeight - FMargins.Bottom;
  Result.Left := FMargins.Left +  5 + GEOLOGIC_PANEL_WIDTH*2;
  Result.Right := Result.Left + GEOLOGIC_PANEL_WIDTH*2;
end;

function TPairwiseSvgWriter.ImpactCoords: TRect;
begin
  Result.Top := FMargins.Top;
  Result.Bottom := FHeight - FMargins.Bottom;
  Result.Left := FMargins.Left + 5 + GEOLOGIC_PANEL_WIDTH*6;
  Result.Right := Result.Left + GEOLOGIC_PANEL_WIDTH*2;
end;

procedure TPairwiseSvgWriter.OpenSvg(const aName: String; const x: Integer; const y: Integer; const aWidth: Integer; const aHeight: Integer; const id: String='');
var
  Temp: String;
begin
  Temp := '<svg xmlns=' + DBLQ + 'http://www.w3.org/2000/svg' + DBLQ + ' xmlns:xlink=' + DBLQ + 'http://www.w3.org/1999/xlink' + DBLQ + ' version=' + DBLQ + '1.1' + DBLQ + ' ';
  Temp := Temp + 'width=' + DBLQ + IntToStr(aWidth) + DBLQ + ' height=' + DBLQ + IntToStr(aHeight) + DBLQ + ' ';
  Temp := Temp + 'name=' + dblq + aName + dblq + ' ';
  Temp := Temp + 'x=' + dblq + IntToStr(x) + dblq + ' ';
  Temp := Temp + 'y=' + dblq + IntToStr(y) + dblq + ' ';
  if id <> EmptyStr then
    Temp := Temp + 'id=' + dblq + id + dblq + ' ';
  Temp := Temp + 'viewBox=' + DBLQ + '0 0 ' + IntToStr(aWidth) + ' ' + IntToStr(aHeight) + DBLQ + '>';
  FStrings.Add(Temp);
end;

procedure TPairwiseSvgWriter.CloseSvg;
begin
  FStrings.Add('</svg>');
end;

function TPairwiseSvgWriter.LoadEarthImpacts: Boolean;
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

function TPairwiseSvgWriter.LoadClimateData: Boolean;
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
  if not FLuminosityData.LoadFromFormula then
    raise Exception.Create('failed to load Luminosity data');

  Result := True;
end;

constructor TPairwiseSvgWriter.Create(aWidth: Integer; aHeight: Integer);
begin
  FPairwiseResult := TPairwiseResult.Create;
  FStrings := TStringList.Create;
  FGeologicTimes := TCompositeGeologicTime.Create;
  FGeoDataChartFormatter := TGeoDataChartFormatter.Create;
  Width := aWidth;
  Height := aHeight;
  FFontHeight := 16;
  FSmallFontHeight := 14;
  FHeaderFontHeight := 20;
  FFontName := 'Roboto Condensed';
  FMargins.Top := 10;
  FMargins.Bottom := 60;
  FMargins.Left := 0;
  FMargins.Right := 0;
  FGeoPanelWidth := 25;
  FTimeTicksPanelWidth := 60;
  FLinkColor := '#194E84';
end;

destructor TPairwiseSvgWriter.Destroy;
begin
  if Assigned(FPairwiseResult) then
    FPairwiseResult.Free;
  if Assigned(FStrings) then
    FStrings.Free;
  if Assigned(FGeologicTimes) then
    FGeologicTimes.Free;
  if Assigned(FGeoDataChartFormatter) then
    FGeoDataChartFormatter.Free;
  inherited Destroy;
end;

function TPairwiseSvgWriter.LoadPairwiseData(aData: String): Boolean;
var
  aList: TStringList;
begin
  aList := nil;
  Result := False;
  if not FileExists(aData) then
    Exit;

  try
    try
      aList := TStringList.Create;
      aList.LoadFromFile(aData);
      Result := FPairwiseResult.LoadFromJson(aList.Text);
    except
      on E:Exception do
        WriteLn('SVG creation failed with error: ' + E.Message);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TPairwiseSvgWriter.GenerateSvg: TStringList;
var
  aRect: TRect;
begin
  OpenSvg('wrapper', 0, 0, Width, Height);
  LoadClimateData;
  InitChartFormatter;
  InitXMLAttributes;
  DrawLuminosity;
  DrawCO2;
  DrawO2;
  DrawEarthImpacts;
  //DrawBorder;
  //aRect := TimeTickCoords;
  //OpenSvg('background', 0, FMargins.Top, Width, Height);
  //DrawBackground;
  //CloseSvg; { background}
  DrawGeologicTimescale;
  aRect := TimeTickCoords;
  OpenSvg('hit-records', aRect.Left, 0, aRect.Right-aRect.left, Height, 'pairwise-hit-records');
  DrawTimeTicks;
  DrawHitRecords;
  CloseSvg; { hit records}
  aRect := ResultsPanelCoords;
  OpenSvg('results', aRect.Left, 0, aRect.Right - aRect.Left, Height,'pairwise-results');
  DrawResult;
  CloseSvg; { results}
  CloseSvg; { wrapper}
  Result := FStrings;
end;

{ TTimetreeSvgWriter }

procedure TTimetreeSvgWriter.SetDeepestRank(AValue: TTaxonomicRank);
begin
  if FDeepestRank=AValue then Exit;
  FDeepestRank:=AValue;
  FTreeBox.DeepestRank := AValue;
end;

procedure TTimetreeSvgWriter.SetCanScaleTree(AValue: Boolean);
begin
  if FCanScaleTree=AValue then Exit;
  FCanScaleTree:=AValue;
  if not AValue then
    FTreeBox.ShowTopologyOnly := True;
end;

procedure TTimetreeSvgWriter.SetDoLogScale(AValue: Boolean);
begin
  if FDoLogScale=AValue then Exit;
  FDoLogScale:=AValue;
  FTreeBox.DoLogScale := AValue;
end;

procedure TTimetreeSvgWriter.SetDoNewick(AValue: Boolean);
begin
  if FDoNewick=AValue then Exit;
  FDoNewick:=AValue;
  FTreeBox.DoNewick:=AValue;
end;

procedure TTimetreeSvgWriter.SetPanelHeight(AValue: Integer);
begin
  if FPanelHeight=AValue then Exit;
  FPanelHeight:=AValue;
  FTreeBox.PanelHeight:=AValue;
end;

procedure TTimetreeSvgWriter.SetStudyTimeNodeHeights(AValue: TDoubleArray);
begin
  FTreeBox.StudyTimeNodeHeights := AValue;
end;

procedure TTimetreeSvgWriter.SetVSpacing(AValue: Integer);
begin
  if FVSpacing=AValue then Exit;
  FVSpacing:=AValue;
  FTreeBox.NameSpacing:=AValue;
end;

procedure TTimetreeSvgWriter.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  FTreeBox.ImageWidth := FWidth;
end;

constructor TTimetreeSvgWriter.Create;
begin
  FIsMobileFriendly := True;
  FCanScaleTree := True;
  FTreeBox := TMySvgTreeBox.Create;
  FDeepestRank := trUnknown;
  FEarthImpacts := nil;
  FO2Data := nil;
  FCO2Data := nil;
  FLuminosityData := nil;
  FDoLogScale := False;
end;

destructor TTimetreeSvgWriter.Destroy;
begin
  if Assigned(FTreeBox) then
    FTreeBox.Free;
  if Assigned(FEarthImpacts) then
    FEarthImpacts.Free;
  if Assigned(FO2Data) then
    FO2Data.Free;
  if Assigned(FCO2Data) then
    FCO2Data.Free;
  if Assigned(FLuminosityData) then
    FLuminosityData.Free;
  inherited Destroy;
end;

procedure TTimetreeSvgWriter.SetData(aList: TTimeTreeList; ImpactsFile: String=''; O2File: String=''; CO2File: String='');
begin
  if FDoLogScale then
    FTreeBox.DoLogScale := True;

  FTreeBox.SetTimeTreeList(aList, False);
  if Trim(ImpactsFile) <> EmptyStr then
  begin
    FEarthImpacts := TEarthImpacts.Create;
    if not FEarthImpacts.LoadFromFile(ImpactsFile) then
      raise Exception.Create('failed to load impacts data');
    FTreeBox.SetEarthImpacts(FEarthImpacts);
    FEarthImpacts := nil; { relinquish ownership}
  end;
  if Trim(O2File) <> EmptyStr then
  begin
    FO2Data := TGeoData.Create('Oxygen', 'o2', '%%');
    FO2Data.ChartColor := O2_CHART_COLOR;
    if not FO2Data.LoadFromFile(O2File) then
      raise Exception.Create('failed to load O2 data');
    FTreeBox.SetO2Data(FO2Data);
    FO2Data := nil;
  end;
  if Trim(CO2File) <> EmptyStr then
  begin
    FCO2Data := TGeoData.Create('Carbon Dioxide', 'co2', '%%');
    FCO2Data.ChartColor := CO2_CHART_COLOR;
    if not FCO2Data.LoadFromFile(CO2File) then
      raise Exception.Create('failed to load CO2 data');
    FTreeBox.SetCO2Data(FCO2Data);
    FCO2Data := nil;
  end;

  //FLuminosityData := TGeoData.Create('Solar Luminosity', 'luminosity', 'L☉');
  FLuminosityData := TGeoData.Create('Solar Luminosity', 'luminosity', 'L');
  FLuminosityData.ChartColor := LUMINOSITY_CHART_COLOR;
  if not FLuminosityData.LoadFromFormula then
    raise Exception.Create('Failed to load luminosity data');
  FTreeBox.SetLuminosityData(FLuminosityData);
  FLuminosityData := nil;
end;

procedure TTimetreeSvgWriter.GenerateSvgStrings(TreeFile: String; PanelsFile: String; Index: Integer=0);
begin
  //FTreeBox.TreeIndex := Index +1;
  FTreeBox.RanksFile := FRanksFile;
  FTreeBox.GenerateSvgStrings(TreeFile, PanelsFile, FIsMobileFriendly);
end;

procedure TTimetreeSvgWriter.GenerateSvgStrings(TreeFile: String);
begin
  FTreeBox.GenerateSvgStrings(TreeFile, FIsMobileFriendly);
end;

procedure TTimetreeSvgWriter.ProgressCallback(AProgress: Integer);
begin

end;

procedure TTimetreeSvgWriter.StatusCallback(AStatus: TTreeRebuildPhase);
begin


end;

procedure TTimetreeSvgWriter.UpdateTaxaOrder(InputIDs: TLongIntList);
begin
  FTreeBox.UpdateTaxaOrder(InputIDs);
  FTreeBox.UserSuppliedATaxaList := True;
end;

end.

