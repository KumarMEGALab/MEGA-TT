unit mearthimpactsrenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mearthimpacts, ttconst;

type
  TOrientation = (tHorizontal, tVertical);

  { TEarthImpactsRenderer }

  TEarthImpactsRenderer = class(TObject)
    private
      FHeight: Integer;
      FImpacts: TEarthImpactsArray;
      FIsFullSvg: Boolean;
      FMaxImpactDiameter: Integer;
      FOrientation: TOrientation;
      FWidth: Integer;
      FRect: TRect;
      FSvgStrings: TStringList;
      procedure DrawBorder;
      procedure DrawCenterLine;
      procedure DrawScale;
      procedure DrawImpacts;
      procedure DrawTitle;
      procedure SetIsFullSvg(AValue: Boolean);
      procedure OpenSvg;
      procedure CloseSvg;
    public
      MapMyaToCoordsFunc: TMapMyaToCoordsFunc;
      constructor Create(aImpacts: TEarthImpactsArray);
      destructor Destroy; override;

      function Render(aOrientation: TOrientation; aRect: TRect; aMaxImpactHeight: Integer=-1): TStringList; overload;
      function Render: TStringList; overload;
      property Orientation: TOrientation read FOrientation;
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
      property MaxImpactDiameter: Integer read FMaxImpactDiameter;
      property IsFullSvg: Boolean read FIsFullSvg write SetIsFullSvg;
  end;

implementation

uses
  gsvgstrings, math;

{ TEarthImpactsRenderer }

procedure TEarthImpactsRenderer.DrawBorder;
var
  BorderStr: String;
  p: array of TPoint;
  attribs: array[0..2] of TXmlAttribute;
begin
  case FOrientation of
    tHorizontal:
      begin
        SetLength(p, 3);
        p[0].X := FRect.Left;
        p[0].Y := FRect.Bottom;
        p[1].X := FRect.Right;
        p[1].Y := FRect.Bottom;
        p[2].X := FRect.Right;
        p[2].Y := FRect.Top;
      end;
    tVertical:
      begin
        SetLength(p, 5);
        p[0].X := FRect.Left;
        p[0].Y := FRect.Bottom;
        p[1].X := FRect.Right;
        p[1].Y := FRect.Bottom;
        p[2].X := FRect.Right;
        p[2].Y := FRect.Top;
        p[3].X := FRect.Left;
        p[3].Y := FRect.Top;
        p[4].X := FRect.Left;
        p[4].Y := FRect.Bottom;
      end;
  end;
  attribs[0].name := 'stroke';
  attribs[0].value := 'white';
  attribs[1].name := 'stroke-width';
  attribs[1].value := '1';
  attribs[2].name := 'fill';
  attribs[2].value := 'white';
  BorderStr := PointsToSvgLine(p, attribs);
  FSvgStrings.Add(BorderStr);
end;

procedure TEarthImpactsRenderer.DrawCenterLine;
var
  i: Integer;
  Points: array[0..1] of TPoint;
  LineString: String;
begin
  case FOrientation of
    tHorizontal:
      begin
        Points[0].X := FRect.Left;
        Points[0].Y := FRect.Top + Round(FHeight * 0.5);
        Points[1].X := FRect.Right;
        Points[1].Y := Points[0].Y;
      end;
    tVertical:
      begin
        Points[0].X := FRect.Left + Round(FWidth * 0.5);
        Points[0].Y := FRect.Top;
        Points[1].X := Points[0].X;
        Points[1].Y := FRect.Bottom;
      end;
  end;

  LineString := '<polyline points=' + DBLQ;

  for i := 0 to 1 do
    LineString := LineString + IntToStr(Points[i].X) + ',' + IntToStr(Points[i].Y) + ' ';
  LineString := Trim(LineString) + DBLQ + ' ';
  LineString := LineString + 'fill=' + dblq + 'none' + dblq + ' ';
  LineString := LineString + 'stroke-dasharray=' + dblq + '5,5' + dblq + ' ';
  LineString := LineString + 'stroke=' + DBLQ + 'orange' + DBLQ + ' stroke-width=' + DBLQ + '1' + DBLQ + ' stroke-linecap=' + DBLQ + 'square' + DBLQ + '/>';
  FSvgStrings.Add(LineString);
end;

procedure TEarthImpactsRenderer.DrawScale;
begin
  case FOrientation of
  tHorizontal:
    begin
      Assert(False, 'not implemented');
    end;
  tVertical:
    begin
      { don't draw a scale in this case }
    end;
  end;
end;

procedure TEarthImpactsRenderer.DrawImpacts;
var
  i, r, x, y: Integer;
  ageStr: String;
  SvgTag: String;
  aRadius: Double;
begin
  case FOrientation of
    tHorizontal: y := FRect.Top + Round(FHeight * 0.5);
    tVertical: x := FRect.Left + Round(FWidth * 0.5);
  end;

  FSvgStrings.Add('<g class=' + dblq + 'earth-impacts' + dblq + '>');
  if Length(FImpacts) > 0 then
    for i := 0 to Length(FImpacts) - 1 do
    begin
      case FOrientation of
        tHorizontal:
          begin
            x := MapMyaToCoordsFunc(FImpacts[i].Age);
            aRadius := FImpacts[i].Diameter / (FMaxImpactDiameter + 10) * FHeight * 0.5;
          end;
        tVertical:
          begin
            y := MapMyaToCoordsFunc(FImpacts[i].Age);
            aRadius := FImpacts[i].Diameter / (FMaxImpactDiameter + 10) * FWidth * 0.5;
          end;
      end;

      r := Max(1, Round(aRadius));
      SvgTag := '<circle cx=' + dblq + IntToStr(x) + dblq + ' ';
      SvgTag := SvgTag + 'cy=' + dblq + IntToStr(y) + dblq + ' ';
      SvgTag := SvgTag + 'r=' + dblq + IntToStr(r) + dblq + ' ';
      SvgTag := SvgTag + 'diam=' + dblq + Format('%.2f', [FImpacts[i].Diameter]) + dblq + ' ';
      if FImpacts[i].Age >= 1 then
        ageStr := IntToStr(Round(FImpacts[i].Age))
      else
        ageStr := Format('%.4e', [FImpacts[i].Age]);
      SvgTag := SvgTag + 'age=' + dblq + ageStr + dblq + ' ';
      SvgTag := SvgTag + 'lat=' + dblq + Format('%.3f', [FImpacts[i].Latitude]) + dblq + ' ';
      SvgTag := SvgTag + 'lon=' + dblq + Format('%.3f', [FImpacts[i].Longitude]) + dblq + ' ';
      SvgTag := SvgTag + 'name=' + dblq + FImpacts[i].Name + dblq + ' ';
      SvgTag := SvgTag + 'loc=' + dblq + FImpacts[i].Location + dblq + ' ';
      SvgTag := SvgTag + 'stroke=' + dblq + 'red' + dblq + ' ';
      SvgTag := SvgTag + 'stroke-width=' + dblq + '1' + dblq + ' ';
      SvgTag := SvgTag + 'fill-opacity=' + dblq + '0.4' + dblq + ' ';
      SvgTag := SvgTag + 'fill=' + dblq + 'red' + dblq + ' />';
      FSvgStrings.Add(SvgTag);
    end;
  FSvgStrings.Add('</g>');
end;

procedure TEarthImpactsRenderer.DrawTitle;
var
  TitleTag: String;
  attribs: array[0..1] of TXmlAttribute;
  x, y: Integer;
begin
  attribs[0].Name := 'font-height';
  attribs[0].Value := '16';
  attribs[1].Name := 'fill';
  attribs[1].Value := '#555555';
  case FOrientation of
    tHorizontal:
      begin

        //TitleTag := TextToSvgText(FRect.Right + 60, ((aRect.Bottom - aRect.Top) div 2) + (FSvgFontHeight div 2), 'Earth Impacts', attribs);
        FSvgStrings.Add(TitleTag);
      end;
    tVertical:
      begin
        Assert(False, 'not implemented');
        { do NOT draw a title in this case}
      end;
  end;
end;

procedure TEarthImpactsRenderer.SetIsFullSvg(AValue: Boolean);
begin
  if FIsFullSvg=AValue then Exit;
  FIsFullSvg:=AValue;
end;

procedure TEarthImpactsRenderer.OpenSvg;
var
  Temp: String;
begin
  Temp := '<svg xmlns=' + DBLQ + 'http://www.w3.org/2000/svg' + DBLQ + ' xmlns:xlink=' + DBLQ + 'http://www.w3.org/1999/xlink' + DBLQ + ' version=' + DBLQ + '1.1' + DBLQ + ' ';
  Temp := Temp + 'width=' + DBLQ + IntToStr(FWidth) + DBLQ + ' height=' + DBLQ + IntToStr(FHeight) + DBLQ + ' ';
  Temp := Temp + 'name=' + dblq + 'Earth-Impacts' + dblq + ' ';
  Temp := Temp + 'x=' + dblq + IntToStr(0) + dblq + ' ';
  Temp := Temp + 'y=' + dblq + IntToStr(0) + dblq + ' ';
  Temp := Temp + 'viewBox=' + DBLQ + '0 0 ' + IntToStr(FWidth) + ' ' + IntToStr(FHeight) + DBLQ + '>';
  FSvgStrings.Add(Temp);
end;

procedure TEarthImpactsRenderer.CloseSvg;
begin
  FSvgStrings.Add('</svg>');
end;

constructor TEarthImpactsRenderer.Create(aImpacts: TEarthImpactsArray);
begin
  FIsFullSvg := False;
  FImpacts := aImpacts;
  FOrientation := tHorizontal;
  FRect.Left := 0;
  FRect.Right := DEFAULT_SVG_WIDTH;
  FRect.Top := 0;
  FRect.Right := DEFAULT_DATA_PANEL_HEIGHT;
  FMaxImpactDiameter := DEFAULT_MAX_IMPACT_HEIGHT;
  FSvgStrings := TStringList.Create;
end;

destructor TEarthImpactsRenderer.Destroy;
var
  i: Integer;
begin
  if Assigned(FSvgStrings) then
    FSvgStrings.Free;
  if Length(FImpacts) > 0 then
    for i := 0 to Length(FImpacts) - 1 do
      FImpacts[i].Free;
  SetLength(FImpacts, 0);
  inherited Destroy;
end;

function TEarthImpactsRenderer.Render(aOrientation: TOrientation; aRect: TRect; aMaxImpactHeight: Integer): TStringList;
begin
  FOrientation := aOrientation;
  FRect.Top := aRect.Top;
  FRect.Bottom := aRect.Bottom;
  FRect.Left := aRect.Left;
  FRect.Right := aRect.Right;
  FWidth := FRect.Right - FRect.Left;
  FHeight := FRect.Bottom - FRect.Top;
  if aMaxImpactHeight > 0 then
    FMaxImpactDiameter := aMaxImpactHeight;
  Result := nil;
  //Assert(Length(FImpacts) > 0);
  Result := Self.Render;
end;

function TEarthImpactsRenderer.Render: TStringList;
begin
  if not Assigned(MapMyaToCoordsFunc) then
    raise Exception.Create('missing MapMyaToCoordsFunc');
  if FIsFullSvg then
    OpenSvg;
  DrawBorder;
  DrawCenterLine;
  DrawImpacts;
  //if FOrientation = tHorizontal then
  //begin
  //  DrawScale;
  //  DrawTitle;
  //end;
  if FIsFullSvg then
    CloseSvg;
  Result := TStringList.Create;
  Result.AddStrings(FSvgStrings);
end;

end.

