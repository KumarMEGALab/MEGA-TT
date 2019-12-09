unit mgeodata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type


  { TGeoDataElement }

  TGeoDataElement = class(TObject)
  private
    FAge: Double;
    FValue: Double;
    protected

    public
      constructor Create;
      destructor Destroy; override;

      property Age: Double read FAge write FAge;
      property Value: Double read FValue write FValue;
  end;

  TGeoDataElementArray = array of TGeoDataElement;

  TGeoDataFormulaFunc = function(aTime: Integer): Double of object;

  { TGeoData }

  TGeoData = class(TList)
    private
      FBgColor: String;
      FChartColor: String;
      FLegendKey: String;
      FName: String;
      FShortName: String;
      function FindFirstElementIndexForRequestedRange(EarliestAge: Double): Integer;
      function FindLastElementIndexForRequestedRange(LatestAge: Double): Integer;
      procedure SetChartColor(AValue: String);
    protected

    public
      GeoDataFormulaFunc: TGeoDataFormulaFunc;
      constructor Create(aName: String; aShortName: String; aLegendKey: String);
      destructor Destroy; override;
      function GetElementsArray(EarliestAge: Double; LatestAge: Double): TGeoDataElementArray;
      function GetElementsArrayExtrapollated(EarliestAge: Double; LatestAge: Double): TGeoDataElementArray;
      function LoadFromFile(aFile: String): Boolean;
      function LoadFromFormula(MakeLinearSmooth: Boolean=False): Boolean;
      function GetMaxValue: Double;
      function GetMinValue: Double;
      function GetMaxValueInRange(EarliestAge: Double; LatestAge: Double; Extrapolate: Boolean=False): Double;
      function GetMinValueInRange(EarliestAge: Double; LatestAge: Double; Extrapolate: Boolean=False): Double;
      property Name: String read FName write FName;
      property ShortName: String read FShortName write FShortName;
      property LegendKey: String read FLegendKey write FLegendKey;
      property BGroundColor: String read FBgColor write FBgColor;
      property ChartColor: String read FChartColor write SetChartColor;
  end;

  function LuminosityFormula(aTime: Integer): Double;

implementation

uses
  ttconst;

function LuminosityFormula(aTime: Integer): Double;
begin
  Result := 1/(1 + 0.4*(1 - (AGE_OF_THE_SUN - aTime)/AGE_OF_THE_SUN)) * 100;
end;

{ TGeoDataElement }

constructor TGeoDataElement.Create;
begin

end;

destructor TGeoDataElement.Destroy;
begin
  inherited Destroy;
end;


{ TGeoData }

function TGeoData.FindFirstElementIndexForRequestedRange(EarliestAge: Double): Integer;
var
  i: Integer;
  aElement: TGeoDataElement;
begin
  Result := Count - 1;
  for i := 0 to Count - 1 do
  begin
    aElement := TGeoDataElement(inherited Items[i]);
    if (aElement.Age >= EarliestAge) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TGeoData.FindLastElementIndexForRequestedRange(LatestAge: Double): Integer;
var
  i: Integer;
  aElement: TGeoDataElement;
begin
  for i := 0 to Count - 1 do
  begin
    aElement := TGeoDataElement(inherited Items[i]);
    if (aElement.Age >= LatestAge) then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TGeoData.SetChartColor(AValue: String);
begin
  if FChartColor=AValue then Exit;
  FChartColor:=AValue;
end;

constructor TGeoData.Create(aName: String; aShortName: String; aLegendKey: String);
begin
  inherited Create;
  FName := aName;
  FShortName := aShortName;
  FLegendKey := aLegendKey;
  FBgColor := 'white';
  FChartColor := '#00b050';
end;

destructor TGeoData.Destroy;
var
  i: Integer;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
      TGeoDataElement(inherited Items[i]).Free;
  inherited Destroy;
end;

function TGeoData.GetElementsArray(EarliestAge: Double; LatestAge: Double): TGeoDataElementArray;
var
  i, startIndex, endIndex: Integer;
  aElement: TGeoDataElement;
begin
  SetLength(Result, 0);
  if Count > 0 then
  begin
    endIndex := FindFirstElementIndexForRequestedRange(EarliestAge);
    startIndex := FindLastElementIndexForRequestedRange(LatestAge);
    for i := startIndex to endIndex do
    begin
      aElement := TGeoDataElement(inherited Items[i]);
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := aElement;
      //if (aElement.Age <= EarliestAge) and (aElement.Age >= LatestAge) then
      //begin
      //  SetLength(Result, Length(Result) + 1);
      //  Result[Length(Result) - 1] := aElement;
      //end;
    end;
  end;
end;

function TGeoData.GetElementsArrayExtrapollated(EarliestAge: Double; LatestAge: Double): TGeoDataElementArray;
var
  i, startIndex, endIndex: Integer;
  aElement: TGeoDataElement;
begin
  SetLength(Result, 0);
  if Count > 0 then
  begin
    endIndex := FindFirstElementIndexForRequestedRange(EarliestAge);
    startIndex := FindLastElementIndexForRequestedRange(LatestAge);
    if (TGeoDataElement(inherited Items[startIndex]).Age > LatestAge) and (startIndex > 0) then
      dec(startIndex);
    for i := startIndex to endIndex do
    begin
      aElement := TGeoDataElement(inherited Items[i]);
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := aElement;
      //if (aElement.Age <= EarliestAge) and (aElement.Age >= LatestAge) then
      //begin
      //  SetLength(Result, Length(Result) + 1);
      //  Result[Length(Result) - 1] := aElement;
      //end;
    end;
  end;
end;

function TGeoData.LoadFromFile(aFile: String): Boolean;
var
  aList: TStringList;
  i: Integer;
  aData: TGeoDataElement;
begin
  aList := nil;
  Result := False;
  if not FileExists(aFile) then
    Exit;
  try
    try
      aList := TStringList.Create;
      aList.LoadFromFile(aFile);
      if aList.Count > 0 then
      begin
        for i := 0 to aList.Count - 1 do
        begin
          aData := TGeoDataElement.Create;
          aData.Age := StrToFloat(aList.Names[i]);
          aData.Value := StrToFloat(aList.ValueFromIndex[i]);
          inherited Add(aData);
        end;
        Result := (Count > 0);
      end;
    except
      Result := False;
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TGeoData.LoadFromFormula(MakeLinearSmooth: Boolean=False): Boolean;
var
  i, j: LongInt;
  aData: TGeoDataElement;
begin
  Result := False;

  try
    if MakeLinearSmooth then
    begin
      aData := TGeoDataElement.Create;
      aData.Age := MIN_MYA;
      aData.Value := LuminosityFormula(Round(MIN_MYA));
      inherited Add(aData);

      aData := TGeoDataElement.Create;
      aData.Age := START_OF_PHANEROZOIC;
      aData.Value := LuminosityFormula(Round(START_OF_PHANEROZOIC));
      inherited Add(aData);

      aData := TGeoDataElement.Create;
      aData.Age := MAX_MYA;
      aData.Value := LuminosityFormula(Round(MAX_MYA));
      inherited Add(aData);

      Result := True;
    end
    else
    begin
      j := Round(MAX_MYA);

      for i := 0 to j do
      begin
        if odd(i) then
          continue;
        aData := TGeoDataElement.Create;
        aData.Age := i;
        aData.Value := LuminosityFormula(i);
        inherited Add(aData);
      end;
    end;
    Result := True;
  except
    on E:Exception do
      raise Exception.Create('Error when loading geo data from a formula: ' + E.Message);
  end;
end;

function TGeoData.GetMaxValue: Double;
var
  i: Integer;
begin
  Result := 0.0;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if TGeoDataElement(inherited Items[i]).Value > Result then
        Result := TGeoDataElement(inherited Items[i]).Value;
end;

function TGeoData.GetMinValue: Double;
var
  i: Integer;
begin
  Result := MaxInt;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if TGeoDataElement(inherited Items[i]).Value < Result then
        Result := TGeoDataElement(inherited Items[i]).Value;
end;

function TGeoData.GetMaxValueInRange(EarliestAge: Double; LatestAge: Double; Extrapolate: Boolean=False): Double;
var
  i: Integer;
  Temp: TGeoDataElementArray;
begin
  Result := 0.0;
  if Extrapolate then
    Temp := GetElementsArrayExtrapollated(EarliestAge, LatestAge)
  else
    Temp := GetElementsArray(EarliestAge, LatestAge);
  if Length(Temp) > 0 then
    for i := 0 to Length(Temp) - 1 do
      if Temp[i].Value > Result then
        Result := Temp[i].Value;
end;

function TGeoData.GetMinValueInRange(EarliestAge: Double; LatestAge: Double; Extrapolate: Boolean=False): Double;
var
  i: Integer;
  Temp: TGeoDataElementArray;
begin
  Result := MaxInt;
  if Extrapolate then
    Temp := GetElementsArrayExtrapollated(EarliestAge, LatestAge)
  else
    Temp := GetElementsArray(EarliestAge, LatestAge);
  if Length(Temp) > 0 then
  begin
    for i := 0 to Length(Temp) - 1 do
      if Temp[i].Value < Result then
        Result := Temp[i].Value;
  end
  else
    Result := 0.0;
end;

end.

