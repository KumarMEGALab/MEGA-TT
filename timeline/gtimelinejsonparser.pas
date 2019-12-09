unit gtimelinejsonparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, gtimelineresult, gconfidenceintervals;

type

  { TTimelineJsonParser }

  TTimelineJsonParser = class(TObject)
    private
      FData: TList;
      FCIParser: TConfidenceIntervalParser;
      function GetItem(Index: Integer): TTimelineResult;
      function LoadSingleResult(JsonStr: String): Boolean;
      function GetScientificName(JsonStr: String): String;
      function GetCommonName(JsonStr: String): String;
      function GetBranchLength(JsonStr: String): Double;
      function GetCiLow(JsonStr: String): Double;
      function GetCiHigh(JsonStr: String): Double;
      function GetAdjustedAge(JsonStr: String): Double;
      function GetPreadjustedAge(JsonStr: String): Double;
      function GetTimeEstimatesStr(JsonStr: String): String;
      procedure Clear;
    public
      constructor Create;
      destructor Destroy; override;

      function Parse(JsonStr: String): Boolean;
      function Count: Integer;
      property Items[Index: Integer]: TTimelineResult read GetItem; default;

  end;

implementation

uses
  gtaxonomicrank;

{ TTimelineJsonParser }

function TTimelineJsonParser.GetItem(Index: Integer): TTimelineResult;
begin
  Assert((Index >= 0) and (FData.Count > 0) and (Index < FData.Count));
  Result := TTimelineResult(FData[Index]);
end;

function TTimelineJsonParser.LoadSingleResult(JsonStr: String): Boolean;
var
  AJson: TJSONObject=nil;
  AData: TJSONData=nil;
  AParser: TJSONParser=nil;
  aResult: TTimelineResult=nil;
  aArray: TJSONArray=nil;
  i: Integer;
  adjustedAge, preadjustedAge: Double;
  isCI: Boolean = False;
  ciLow, ciHigh: Double;
  timeEstimatesString: String;
begin
  Result := False;

  try
    try
      AParser := TJSONParser.Create(JsonStr);
      AData := AParser.Parse;
      if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
        raise Exception.Create('failed to parse JSON');
      AJson := TJSONObject(AData);
      aResult := TTimelineResult.Create;

      AData := AJson.Find('id', jtNumber);
      if Assigned(AData) then
        aResult.Id := AData.Value;

      AData := AJson.Find('timetree_id', jtNumber);
      if Assigned(AData) then
        aResult.TimeTreeId := AData.Value;

      AData := AJson.Find('name', jtObject);
      if Assigned(AData) then
      begin
        aResult.ScientificName := GetScientificName(AData.AsJSON);
        aResult.CommonName := GetCommonName(AData.AsJSON);
      end;

      AData := AJson.Find('rank', jtString);
      if Assigned(AData) then
        aResult.TaxonomicRank := StringToTaxonomicRank(AData.Value);

      AData := AJson.Find('branch', jtObject);
      if Assigned(AData) then
      begin
        aResult.BranchLength := GetBranchLength(AData.AsJSON);
        ciLow := GetCiLow(AData.AsJSON);
        ciHigh := GetCiHigh(AData.AsJSON);
      end;

      AData := AJson.Find('studies', jtObject);
      if Assigned(AData) then
      begin
        adjustedAge := GetAdjustedAge(AData.AsJSON);
        preadjustedAge := GetPreadjustedAge(AData.AsJSON);
        timeEstimatesString := GetTimeEstimatesStr(AData.AsJSON);
        if Trim(timeEstimatesString) = EmptyStr then
          raise Exception.Create('missing time estimates string');

        FCIParser.ProcessConfidenceInterval(timeEstimatesString, adjustedAge, preadjustedAge, ciLow, ciHigh, isCI);
        aResult.IsConfidenceInterval := isCI;
        aResult.ConfidenceIntervalLow := ciLow;
        aResult.ConfidenceIntervalHigh := ciHigh;
      end;

      AData := AJson.Find('figurines', jtArray);
      if Assigned(AData) then
      begin
        aArray := TJSONArray(AData);
        if aArray.Count > 0 then
          for i := 0 to aArray.Count - 1 do
          begin
            aData := aArray[i];
            aResult.FigurinesList.Add(AData.Value);
          end;
      end;

      FData.Add(aResult);
      Result := True;
    except
      on E:Exception do
        raise Exception.Create('Failed to load JSON record: ' + E.Message);
    end;
  finally
    if Assigned(AJson) then
      AJson.Free;
    if Assigned(AParser) then
      AParser.Free;
  end;
end;

function TTimelineJsonParser.GetScientificName(JsonStr: String): String;
var
  AJson: TJSONObject=nil;
  AData: TJSONData=nil;
  AParser: TJSONParser=nil;
begin
  Result := EmptyStr;

  try
    AParser := TJSONParser.Create(JsonStr);
    AData := AParser.Parse;
    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('failed to parse scientific name from JSON');
    AJson := TJSONObject(AData);

    AData := AJson.Find('scientific', jtString);
    if Assigned(AData) then
      Result := AData.Value
    else
      raise Exception.Create('missing scientific name');
  finally
    if Assigned(AJson) then
      AJson.Free;
    if Assigned(AParser) then
      AParser.Free;
  end;
end;

function TTimelineJsonParser.GetCommonName(JsonStr: String): String;
var
  AJson: TJSONObject=nil;
  AData: TJSONData=nil;
  AParser: TJSONParser=nil;
begin
  Result := EmptyStr;

  try
    AParser := TJSONParser.Create(JsonStr);
    AData := AParser.Parse;
    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('failed to parse common name from JSON');
    AJson := TJSONObject(AData);

    AData := AJson.Find('common', jtString);
    if Assigned(AData) then
      Result := AData.Value;
  finally
    if Assigned(AJson) then
      AJson.Free;
    if Assigned(AParser) then
      AParser.Free;
  end;
end;

function TTimelineJsonParser.GetBranchLength(JsonStr: String): Double;
var
  AJson: TJSONObject=nil;
  AData: TJSONData=nil;
  AParser: TJSONParser=nil;
begin
  Result := 0.0;

  try
    AParser := TJSONParser.Create(JsonStr);
    AData := AParser.Parse;
    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('failed to parse branch length from JSON');
    AJson := TJSONObject(AData);

    AData := AJson.Find('length', jtNumber);
    if Assigned(AData) then
      Result := AData.Value
    else
      raise Exception.Create('missing branch length');
  finally
    if Assigned(AJson) then
      AJson.Free;
    if Assigned(AParser) then
      AParser.Free;
  end;
end;

function TTimelineJsonParser.GetCiLow(JsonStr: String): Double;
var
  AJson: TJSONObject=nil;
  AData: TJSONData=nil;
  AParser: TJSONParser=nil;
begin
  Result := 0.0;

  try
    AParser := TJSONParser.Create(JsonStr);
    AData := AParser.Parse;
    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('failed to parse confidence interval from JSON');
    AJson := TJSONObject(AData);

    AData := AJson.Find('ci_low', jtNumber);
    if Assigned(AData) then
      Result := AData.Value
    else
      raise Exception.Create('missing CI lower bound');
  finally
    if Assigned(AJson) then
      AJson.Free;
    if Assigned(AParser) then
      AParser.Free;
  end;
end;

function TTimelineJsonParser.GetCiHigh(JsonStr: String): Double;
var
  AJson: TJSONObject=nil;
  AData: TJSONData=nil;
  AParser: TJSONParser=nil;
begin
  Result := 0.0;

  try
    AParser := TJSONParser.Create(JsonStr);
    AData := AParser.Parse;
    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('failed to parse confidence interval from JSON');
    AJson := TJSONObject(AData);

    AData := AJson.Find('ci_high', jtNumber);
    if Assigned(AData) then
      Result := AData.Value
    else
      raise Exception.Create('missing CI upper bound');
  finally
    if Assigned(AJson) then
      AJson.Free;
    if Assigned(AParser) then
      AParser.Free;
  end;
end;

function TTimelineJsonParser.GetAdjustedAge(JsonStr: String): Double;
var
  AJson: TJSONObject=nil;
  AData: TJSONData=nil;
  AParser: TJSONParser=nil;
begin
  Result := 0.0;

  try
    AParser := TJSONParser.Create(JsonStr);
    AData := AParser.Parse;
    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('failed to parse studies from JSON');
    AJson := TJSONObject(AData);

    AData := AJson.Find('precomputed_age', jtNumber);
    if Assigned(AData) then
      Result := AData.Value
    else
      raise Exception.Create('missing precomputed_age');
  finally
    if Assigned(AJson) then
      AJson.Free;
    if Assigned(AParser) then
      AParser.Free;
  end;
end;

function TTimelineJsonParser.GetPreadjustedAge(JsonStr: String): Double;
var
  AJson: TJSONObject=nil;
  AData: TJSONData=nil;
  AParser: TJSONParser=nil;
begin
  Result := 0.0;

  try
    AParser := TJSONParser.Create(JsonStr);
    AData := AParser.Parse;
    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('failed to parse studies from JSON');
    AJson := TJSONObject(AData);

    AData := AJson.Find('preadjusted_age', jtNumber);
    if Assigned(AData) then
      Result := AData.Value
    else
      raise Exception.Create('missing preadjusted_age');
  finally
    if Assigned(AJson) then
      AJson.Free;
    if Assigned(AParser) then
      AParser.Free;
  end;
end;

function TTimelineJsonParser.GetTimeEstimatesStr(JsonStr: String): String;
var
  AJson: TJSONObject=nil;
  AData: TJSONData=nil;
  AParser: TJSONParser=nil;
begin
  Result := EmptyStr;

  try
    AParser := TJSONParser.Create(JsonStr);
    AData := AParser.Parse;
    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('failed to parse studies from JSON');
    AJson := TJSONObject(AData);

    AData := AJson.Find('time_estimates', jtString);
    if Assigned(AData) then
      Result := AData.Value
    else
      raise Exception.Create('missing time_estimates');
  finally
    if Assigned(AJson) then
      AJson.Free;
    if Assigned(AParser) then
      AParser.Free;
  end;
end;

procedure TTimelineJsonParser.Clear;
var
  i: Integer;
begin
  if FData.Count > 0 then
  begin
    for i := 0 to FData.Count - 1 do
      TTimelineResult(FData[i]).Free;
  end;
  FData.Clear;
end;

constructor TTimelineJsonParser.Create;
begin
  FData := TList.Create;
  FCIParser := TConfidenceIntervalParser.Create;
end;

destructor TTimelineJsonParser.Destroy;
begin
  Clear;
  if Assigned(FData) then
    FData.Free;
  if Assigned(FCIParser) then
    FCIParser.Free;
  inherited Destroy;
end;

function TTimelineJsonParser.Parse(JsonStr: String): Boolean;
var
  AParser: TJSONParser;
  AData: TJSONData;
  aArray: TJSONArray;
  i: Integer;
 begin
  AParser := nil;
  AData := nil;
  Result := True;

  try
    try
      AParser := TJSONParser.Create(jsonStr);
      AData := AParser.Parse;
      if (not Assigned(AData)) or (not (AData.JSONType = jtArray)) then
        raise Exception.Create('failed to parse JSON');
      aArray := TJSONArray(AData);
      if aArray.Count > 0 then
        for i := 0 to aArray.Count - 1 do
        begin
          aData := aArray[i];
          Result := Result and LoadSingleResult(aData.AsJSON);
        end;
    except
      on E:Exception do
        raise Exception.Create('Failed to load JSON: ' + E.Message);
    end;
  finally
    if Assigned(AParser) then
      AParser.Free;
  end;
end;

function TTimelineJsonParser.Count: Integer;
begin
  Result := FData.Count;
end;

end.

