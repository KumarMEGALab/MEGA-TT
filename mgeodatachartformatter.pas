unit mgeodatachartformatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mgeodata, ttconst;

type
  //TMapAgeToCoordFunc = function (mya: Double): Integer of object;

  { TGeoDataChartFormatter }

  TGeoDataChartFormatter = class(TObject)
    private
      FYAxisBottomAdd: Double; { when drawing horizontal, for the y-axis we will add to the lower bound so that min values are visible}
      FXAxisLeftAdd: Double; { when drawin vertical, for the x-axis, add to the lower bound so min values are visible}
      FBottomTickCoords: TArrayOfTPoint;
      FBottomTickText: String;
      FCenterTickCoords: TArrayOfTPoint;
      FCenterTickText: String;
      FChartHeight: Integer;
      FChartWidth: Integer;
      FChartMargins: TRect;
      FMaxVal: Double;
      FMinVal: Double;
      FPointsArrayForAreaChart: TArrayOfTPoint;
      FRange: Double;
      FTopTickCoords: TArrayOfTPoint;
      FTopTickText: String;
      FXAxisMaxValue: Double;
      FXAxisMinValue: Double;
      FYAxisMaxValue: Double;
      FYAxisMinValue: Double;
      procedure SetChartHeight(AValue: Integer);
      procedure SetChartMargins(AValue: TRect);
      procedure SetChartWidth(AValue: Integer);
      procedure FilterOutRedundantChartPoints;
      procedure FilterOutRedundantChartPointsVertical;
      procedure MakeLinearSmooth;
    protected
      FGeoData: TGeoData;
      FEarliestTime: Double;
      FLatestTime: Double;
      FRect: TRect;
      procedure InitPointsArrayForAreaChart(MakeSmooth:Boolean=False);
      procedure InitPointsArrayForAreaChartVertical(MakeSmooth:Boolean=False);

      procedure InitYAxisMinMaxValues;
      procedure InitCalculatedValues;
      procedure InitCalculatedValuesVertical;
    public
      MapAgeToCoordFunc: TMapAgeToCoordFunc;
      //MapAgeToYCoordFunc: TMapAgeToCoordFunc;
      constructor Create;
      destructor Destroy; override;

      procedure BuildAreaChart(aData: TGeoData; EarliestTime: Double; LatestTime: Double; aRect: TRect; MakeSmooth: Boolean=False);
      procedure BuildAreaChartVertical(aData: TGeoData; EarliestTime: Double; LatestTime: Double; aRect: TRect; MakeSmooth: Boolean=False);

      property EarliestTime: Double read FEarliestTime;
      property LatestTime: Double read FLatestTime;

      property PointsArrayForAreaChart: TArrayOfTPoint read FPointsArrayForAreaChart;
      property TopTickCoords: TArrayOfTPoint read FTopTickCoords;
      property CenterTickCoords: TArrayOfTPoint read FCenterTickCoords;
      property BottomTickCoords: TArrayOfTPoint read FBottomTickCoords;

      property TopTickText: String read FTopTickText;
      property CenterTickText: String read FCenterTickText;
      property BottomTickText: String read FBottomTickText;

      property YAxisMinValue: Double read FYAxisMinValue;
      property YAxisMaxValue: Double read FYAxisMaxValue;
      property XAxisMinValue: Double read FXAxisMinValue;
      property XAxisMaxValue: Double read FXAxisMaxValue;

      property ChartHeight: Integer read FChartHeight write SetChartHeight;
      property ChartWidth: Integer read FChartWidth write SetChartWidth;
      property ChartMargins: TRect read FChartMargins write SetChartMargins;
  end;

implementation

uses
  math;

{ TGeoDataChartFormatter }

constructor TGeoDataChartFormatter.Create;
begin
  FChartHeight := 115;
end;

destructor TGeoDataChartFormatter.Destroy;
begin
  inherited Destroy;
end;

procedure TGeoDataChartFormatter.BuildAreaChart(aData: TGeoData; EarliestTime: Double; LatestTime: Double; aRect: TRect; MakeSmooth: Boolean=False);
begin
  Assert(Assigned(MapAgeToCoordFunc));
  FGeoData := aData;
  FEarliestTime := EarliestTime;
  FLatestTime := LatestTime;
  FRect := aRect;
  InitCalculatedValues;
  InitPointsArrayForAreaChart(MakeSmooth);
end;

procedure TGeoDataChartFormatter.BuildAreaChartVertical(aData: TGeoData; EarliestTime: Double; LatestTime: Double; aRect: TRect; MakeSmooth: Boolean=False);
begin
  Assert(Assigned(MapAgeToCoordFunc));
  FGeoData := aData;
  FEarliestTime := EarliestTime;
  FLatestTime := LatestTime;
  FRect := aRect;
  InitCalculatedValuesVertical;
  InitPointsArrayForAreaChartVertical(MakeSmooth);
end;

procedure TGeoDataChartFormatter.SetChartHeight(AValue: Integer);
begin
  if FChartHeight=AValue then Exit;
  FChartHeight:=AValue;
end;

procedure TGeoDataChartFormatter.SetChartMargins(AValue: TRect);
begin
  FChartMargins:=AValue;
end;

procedure TGeoDataChartFormatter.SetChartWidth(AValue: Integer);
begin
  if FChartWidth=AValue then Exit;
  FChartWidth:=AValue;
end;

procedure TGeoDataChartFormatter.FilterOutRedundantChartPoints;
var
  tempPoints: TArrayOfTPoint;
  i: Integer;
begin
  SetLength(tempPoints, 2);
  tempPoints[0].X := FPointsArrayForAreaChart[0].X;
  tempPoints[0].Y := FPointsArrayForAreaChart[0].Y;
  tempPoints[1].X := FPointsArrayForAreaChart[1].X;
  tempPoints[1].Y := FPointsArrayForAreaChart[1].Y;
  if Length(FPointsArrayForAreaChart) > 2 then
  begin
    for i := 2 to Length(FPointsArrayForAreaChart) - 3 do
    begin
      if FPointsArrayForAreaChart[i].X <> FPointsArrayForAreaChart[i-1].X then
      begin
        SetLength(tempPoints, Length(tempPoints) + 1);
        tempPoints[Length(tempPoints) - 1].X := FPointsArrayForAreaChart[i].X;
        tempPoints[Length(tempPoints) - 1].Y := FPointsArrayForAreaChart[i].Y;
      end;
    end;
  end;
  SetLength(tempPoints, Length(tempPoints) + 2);
  tempPoints[Length(tempPoints) - 2].X := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 2].X;
  tempPoints[Length(tempPoints) - 2].Y := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 2].Y;
  tempPoints[Length(tempPoints) - 1].X := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 1].X;
  tempPoints[Length(tempPoints) - 1].Y := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 1].Y;
  SetLength(FPointsArrayForAreaChart, 0);
  FPointsArrayForAreaChart := tempPoints;
end;

procedure TGeoDataChartFormatter.FilterOutRedundantChartPointsVertical;
var
  tempPoints: TArrayOfTPoint;
  i: Integer;
begin
  SetLength(tempPoints, 2);
  tempPoints[0].X := FPointsArrayForAreaChart[0].X;
  tempPoints[0].Y := FPointsArrayForAreaChart[0].Y;
  tempPoints[1].X := FPointsArrayForAreaChart[1].X;
  tempPoints[1].Y := FPointsArrayForAreaChart[1].Y;
  if Length(FPointsArrayForAreaChart) > 2 then
  begin
    for i := 2 to Length(FPointsArrayForAreaChart) - 3 do
    begin
      if FPointsArrayForAreaChart[i].Y <> FPointsArrayForAreaChart[i-1].Y then
      begin
        SetLength(tempPoints, Length(tempPoints) + 1);
        tempPoints[Length(tempPoints) - 1].X := FPointsArrayForAreaChart[i].X;
        tempPoints[Length(tempPoints) - 1].Y := FPointsArrayForAreaChart[i].Y;
      end;
    end;
  end;
  SetLength(tempPoints, Length(tempPoints) + 2);
  tempPoints[Length(tempPoints) - 2].X := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 2].X;
  tempPoints[Length(tempPoints) - 2].Y := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 2].Y;
  tempPoints[Length(tempPoints) - 1].X := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 1].X;
  tempPoints[Length(tempPoints) - 1].Y := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 1].Y;
  SetLength(FPointsArrayForAreaChart, 0);
  FPointsArrayForAreaChart := tempPoints;
end;

procedure TGeoDataChartFormatter.MakeLinearSmooth;
var
  tempPoints: TArrayOfTPoint;
  i: Integer;
  debug1, debug2: Integer;
begin
  SetLength(tempPoints, 5);
  tempPoints[0].X := FPointsArrayForAreaChart[0].X;
  tempPoints[0].Y := FPointsArrayForAreaChart[0].Y;
  tempPoints[1].X := FPointsArrayForAreaChart[1].X;
  tempPoints[1].Y := FPointsArrayForAreaChart[1].Y;
  tempPoints[2].X := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 3].X;
  tempPoints[2].Y := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 3].Y;
  tempPoints[3].X := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 2].X;
  tempPoints[3].Y := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 2].Y;
  tempPoints[4].X := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 1].X;
  tempPoints[4].Y := FPointsArrayForAreaChart[Length(FPointsArrayForAreaChart) - 1].Y;


  debug1 := Length(tempPoints);
  debug2 := Length(FPointsArrayForAreaChart);
  SetLength(FPointsArrayForAreaChart, 0);
  FPointsArrayForAreaChart := tempPoints;
end;

procedure TGeoDataChartFormatter.InitPointsArrayForAreaChart(MakeSmooth:Boolean=False);
var
  i: Integer;
  GeoDataElements: TGeoDataElementArray;
  Delta: Double = 0.0;
  Ratio: Double = 0.0;
  NumElements: Integer = 0;
  DrawingAreaHeight: Integer;
begin
  GeoDataElements := FGeoData.GetElementsArray(FEarliestTime, FLatestTime);
  NumElements := Length(GeoDataElements);
  if NumElements > 0 then
  begin
    SetLength(FPointsArrayForAreaChart, NumElements + 3);
    FPointsArrayForAreaChart[0].X := FRect.Right;
    FPointsArrayForAreaChart[0].Y := FRect.Bottom-1; { y base}
    DrawingAreaHeight := Round(FChartHeight - FChartMargins.Top - FYAxisBottomAdd);
    for i := 0 to NumElements - 1 do
    begin
      if GeoDataElements[i].Age >= 0 then
      begin
        FPointsArrayForAreaChart[i + 1].X := MapAgeToCoordFunc(GeoDataElements[i].Age);
        Delta := GeoDataElements[i].Value - FMinVal;
        if CompareValue(FRange, 0.0, 0.00001) <> 0 then
          Ratio := Delta / FRange
        else
          Ratio := 1.0;
        //FPointsArrayForAreaChart[i + 1].Y := Min(FPointsArrayForAreaChart[0].Y, Round(FPointsArrayForAreaChart[0].Y - FYAxisBottomAdd) - Round(Ratio*DrawingAreaHeight)) ;
        FPointsArrayForAreaChart[i + 1].Y := Min(FPointsArrayForAreaChart[0].Y, Round((FPointsArrayForAreaChart[0].Y - FYAxisBottomAdd) - (Ratio*DrawingAreaHeight)));
      end
      else
      begin
        FPointsArrayForAreaChart[i + 1].X := FPointsArrayForAreaChart[i].X;
        FPointsArrayForAreaChart[i + 1].Y := FPointsArrayForAreaChart[i].Y;
      end;
    end;
    FPointsArrayForAreaChart[NumElements + 1].X := FPointsArrayForAreaChart[NumElements].X;
    FPointsArrayForAreaChart[NumElements + 1].Y := FPointsArrayForAreaChart[0].Y;
    FPointsArrayForAreaChart[NumElements + 2].X := FPointsArrayForAreaChart[0].X;
    FPointsArrayForAreaChart[NumElements + 2].Y := FPointsArrayForAreaChart[0].Y;
  end;
  if MakeSmooth then
    MakeLinearSmooth
  else
    FilterOutRedundantChartPoints;
end;

procedure TGeoDataChartFormatter.InitPointsArrayForAreaChartVertical(MakeSmooth: Boolean=False);
var
  i: Integer;
  GeoDataElements: TGeoDataElementArray;
  Delta: Double = 0.0;
  Ratio: Double = 0.0;
  NumElements: Integer = 0;
  DrawingAreaWidth: Integer;
begin
  GeoDataElements := FGeoData.GetElementsArrayExtrapollated(FEarliestTime, FLatestTime);
  NumElements := Length(GeoDataElements);
  if NumElements > 0 then
  begin
    SetLength(FPointsArrayForAreaChart, NumElements + 3);
    FPointsArrayForAreaChart[0].X := FRect.Left;
    FPointsArrayForAreaChart[0].Y := MapAgeToCoordFunc(GeoDataElements[0].Age);
    DrawingAreaWidth := Round(FChartWidth - FChartMargins.Left - FXAxisLeftAdd);

    for i := 0 to NumElements - 1 do
    begin
      if GeoDataElements[i].Age >= 0 then
      begin
        FPointsArrayForAreaChart[i + 1].Y := MapAgeToCoordFunc(GeoDataElements[i].Age);
        Delta := GeoDataElements[i].Value - FMinVal;
        if CompareValue(FRange, 0.0, 0.00001) <> 0 then
          Ratio := Delta / FRange
        else
          Ratio := 1.0;
        FPointsArrayForAreaChart[i + 1].X := Min(DrawingAreaWidth, Round(FPointsArrayForAreaChart[0].X + FXAxisLeftAdd) + Round(Ratio*DrawingAreaWidth)) ;
      end
      else
      begin
        FPointsArrayForAreaChart[i + 1].X := FPointsArrayForAreaChart[i].X;
        FPointsArrayForAreaChart[i + 1].Y := FPointsArrayForAreaChart[i].Y;
      end;
    end;
    FPointsArrayForAreaChart[NumElements + 1].X := FPointsArrayForAreaChart[0].X;
    FPointsArrayForAreaChart[NumElements + 1].Y := FPointsArrayForAreaChart[NumElements].Y;
    FPointsArrayForAreaChart[NumElements + 2].X := FPointsArrayForAreaChart[0].X;
    FPointsArrayForAreaChart[NumElements + 2].Y := FPointsArrayForAreaChart[0].Y;
  end;
  if MakeSmooth then
    MakeLinearSmooth
  else
    FilterOutRedundantChartPointsVertical;
end;

procedure TGeoDataChartFormatter.InitYAxisMinMaxValues;
begin

end;

procedure TGeoDataChartFormatter.InitCalculatedValues;
begin
  FMaxVal := FGeoData.GetMaxValueInRange(FEarliestTime, FLatestTime);
  FMinVal := FGeoData.GetMinValueInRange(FEarliestTime, FLatestTime);
  FRange := FMaxVal - FMinVal;
  if FMinVal > 0 then
  begin
    FYAxisBottomAdd := Max(1.0, FRange * 0.1);
    if FYAxisBottomAdd > FMinVal then
      FYAxisBottomAdd := FMinVal;
  end
  else
    FYAxisBottomAdd := 0.0;

  if FYAxisBottomAdd > 1 then
    FYAxisMinValue := FMinVal - FYAxisBottomAdd
  else
    FYAxisMinValue := FMinVal;
end;

procedure TGeoDataChartFormatter.InitCalculatedValuesVertical;
begin
  FMaxVal := FGeoData.GetMaxValueInRange(FEarliestTime, FLatestTime, True);
  FMinVal := FGeoData.GetMinValueInRange(FEarliestTime, FLatestTime, True);
  FRange := FMaxVal - FMinVal;
  if FMinVal > 0 then
  begin
    FXAxisLeftAdd := Max(1.0, FRange * 0.1);
    if FXAxisLeftAdd > FMinVal then
      FXAxisLeftAdd := FMinVal;
  end
  else
    FXAxisLeftAdd := 0.0;

  if FXAxisLeftAdd > 1 then
    FYAxisMinValue := FMinVal - FXAxisLeftAdd
  else
    FYAxisMinValue := FMinVal;
end;

end.

