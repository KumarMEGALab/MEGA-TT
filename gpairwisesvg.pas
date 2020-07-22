unit gpairwisesvg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, gconfidenceintervals;

type

  { TTimetreeTaxon }

  TTimetreeTaxon = class(TObject)

  private
    FCommonName: String;
    FLink: String;
    FScientificName: String;
    FSearchString: String;
    FTimetreeId: Integer;
    procedure SetCommonName(AValue: String);
    procedure SetLink(AValue: String);
    procedure SetScientificName(AValue: String);
    procedure SetSearchString(AValue: String);
    procedure SetTimetreeId(AValue: Integer);
    protected

    public
      constructor Create;
      destructor Destroy; override;
      property CommonName: String read FCommonName write SetCommonName;
      property ScientificName: String read FScientificName write SetScientificName;
      property SearchString: String read FSearchString write SetSearchString;
      property TimetreeId: Integer read FTimetreeId write SetTimetreeId;
      property Link: String read FLink write SetLink;
  end;

  { TTimetreeHitRecord }

  TTimetreeHitRecord = class(TObject)
  private
    FAbstract: String;
    FAuthor: String;
    FCitationNum: Integer;
    FPubmedId: Integer;
    FRefId: String;
    FTime: Double;
    FTitle: String;
    FYear: Integer;
    procedure SetAbstract(AValue: String);
    procedure SetAuthor(AValue: String);
    procedure SetCitationNum(AValue: Integer);
    procedure SetPubmedId(AValue: Integer);
    procedure SetRefId(AValue: String);
    procedure SetTime(AValue: Double);
    procedure SetTitle(AValue: String);
    procedure SetYear(AValue: Integer);

  protected

  public
    constructor Create;
    destructor Destroy; override;

    property Abstr: String read FAbstract write SetAbstract;
    property Author: String read FAuthor write SetAuthor;
    property CitationNum: Integer read FCitationNum write SetCitationNum;
    property PubmedId: Integer read FPubmedId write SetPubmedId;
    property RefId: String read FRefId write SetRefId;
    property Time: Double read FTime write SetTime;
    property Year: Integer read FYear write SetYear;
    property Title: String read FTitle write SetTitle;
  end;

  { TPairwiseResult }

  TPairwiseResult = class(TObject)

    private
      FCIParser: TConfidenceIntervalParser;
      FAncestorId: Integer;
      FCIHigh: Double;
      FCILow: Double;
      FHitRecords: TList;
      FIsCI: Boolean;
      FNumStudies: Integer;
      FShowSummary: Boolean;
      FMedianTime: Double;
      FMolecularTime: Double;
      FShowExpert: Boolean;
      FTaxonA: TTimetreeTaxon;
      FTaxonB: TTimetreeTaxon;
      procedure SetAncestorId(AValue: Integer);
      procedure SetMedianTime(AValue: Double);
      procedure SetMolecularTime(AValue: Double);
      procedure SetShowExpert(AValue: Boolean);
      procedure SetShowSummary(AValue: Boolean);
      procedure SortRecords;
      procedure FilterOutliers;
      function GetCiLow(JsonStr: String): Double;
      function GetCiHigh(JsonStr: String): Double;
      function GetAdjustedAge(JsonStr: String): Double;
      function GetPreadjustedAge(JsonStr: String): Double;
      function GetTimeEstimatesStr(JsonStr: String): String;
      procedure HandleMissingConfidenceIntervalData;

    protected

    public
      constructor Create;
      destructor Destroy; override;

      function LoadFromJson(jsonData: String): Boolean;
      function LoadHitRecord(aJsonStr: String): Boolean;
      function GetMinTime: Double;
      function GetMaxTime: Double;
      property ShowExpert: Boolean read FShowExpert write SetShowExpert;
      property ShowSummary: Boolean read FShowSummary write SetShowSummary;
      property HitRecords: TList read FHitRecords write FHitRecords;
      property MolecularTime: Double read FMolecularTime write SetMolecularTime;
      property MedianTime: Double read FMedianTime write SetMedianTime;
      property CILow: Double read FCILow write FCILow;
      property CIHigh: Double read FCIHigh write FCIHigh;
      property IsCI: Boolean read FIsCI write FIsCI;
      property AncestorId: Integer read FAncestorId write SetAncestorId;
      property TaxonA: TTimetreeTaxon read FTaxonA;
      property TaxonB: TTimetreeTaxon read FTaxonB;
      property NumStudies: Integer read FNumStudies; { the actual number of studies used to generate a time estimate. Outliers get filtered out so we don't use HitRecords.Count}
  end;

  function CompareHitRecord(Item1: Pointer; Item2: Pointer): Integer;

implementation

uses
  math, gsvgstrings;

function CompareHitRecord(Item1: Pointer; Item2: Pointer): Integer;
var
  rec1, rec2: TTimetreeHitRecord;
begin
  rec1 := TTimetreeHitRecord(Item1);
  rec2 := TTimetreeHitRecord(Item2);
  Result := CompareValue(rec1.Time, rec2.Time, 0.0001);
end;

{ TTimetreeTaxon }

procedure TTimetreeTaxon.SetCommonName(AValue: String);
begin
  if FCommonName=AValue then Exit;
  FCommonName:=AValue;
end;

procedure TTimetreeTaxon.SetLink(AValue: String);
begin
  FLink := StringReplace(AValue, '&', '&amp;', [rfReplaceAll]);
end;

procedure TTimetreeTaxon.SetScientificName(AValue: String);
begin
  if FScientificName=AValue then Exit;
  FScientificName:=AValue;
end;

procedure TTimetreeTaxon.SetSearchString(AValue: String);
begin
  if FSearchString=AValue then Exit;
  FSearchString:=AValue;
end;

procedure TTimetreeTaxon.SetTimetreeId(AValue: Integer);
begin
  if FTimetreeId=AValue then Exit;
  FTimetreeId:=AValue;
end;

constructor TTimetreeTaxon.Create;
begin

end;

destructor TTimetreeTaxon.Destroy;
begin

  inherited Destroy;
end;

{ TPairwiseResult }

procedure TPairwiseResult.SetAncestorId(AValue: Integer);
begin
  if FAncestorId=AValue then Exit;
  FAncestorId:=AValue;
end;

procedure TPairwiseResult.SetMedianTime(AValue: Double);
begin
  if FMedianTime=AValue then Exit;
  FMedianTime:=AValue;
end;

procedure TPairwiseResult.SetMolecularTime(AValue: Double);
begin
  if FMolecularTime=AValue then Exit;
  FMolecularTime:=AValue;
end;

procedure TPairwiseResult.SetShowExpert(AValue: Boolean);
begin
  if FShowExpert=AValue then Exit;
  FShowExpert:=AValue;
end;

procedure TPairwiseResult.SetShowSummary(AValue: Boolean);
begin
  if FShowSummary=AValue then Exit;
  FShowSummary:=AValue;
end;

constructor TPairwiseResult.Create;
begin
  FTaxonA := TTimetreeTaxon.Create;
  FTaxonB := TTimetreeTaxon.Create;
  FHitRecords := TList.Create;
  FCIParser := TConfidenceIntervalParser.Create;
end;

destructor TPairwiseResult.Destroy;
var
  i: Integer;
begin
  if Assigned(FTaxonA) then
    FTaxonA.Free;
  if Assigned(FTaxonB) then
    FTaxonB.Free;
  if Assigned(FHitRecords) then
  begin
    if FHitRecords.Count > 0 then
      for i := 0 to FHitRecords.Count - 1 do
        TTimetreeHitRecord(FHitRecords[i]).Free;
    FHitRecords.Free;
  end;
  if Assigned(FCIParser) then
    FCIParser.Free;
  inherited Destroy;
end;

procedure TPairwiseResult.SortRecords;
begin
  FHitRecords.Sort(@CompareHitRecord);
end;

procedure TPairwiseResult.FilterOutliers;
var
  sigma, delta, stdev: Double;
  cConstant, mean: Double;
  dMax: Double;
  aRec: TTimetreeHitRecord;
  i: Integer;
begin
  if FHitRecords.Count > 5 then
  begin
    mean := 0.0;
    sigma := 0.0;
    stdev := 0.0;
    cConstant := 0.9969 + 0.4040 * ln(FHitRecords.Count);
    for i := 0 to FHitRecords.Count - 1 do
      mean := mean + TTimetreeHitRecord(FHitRecords[i]).Time;
    mean := mean / FHitRecords.Count;

    for i := 0 to FHitRecords.Count - 1 do
      sigma := sigma + (TTimetreeHitRecord(FHitRecords[i]).Time - mean) * (TTimetreeHitRecord(FHitRecords[i]).Time - mean);
    sigma := sigma / (FHitRecords.Count - 1);
    stdev := sqrt(sigma);
    dMax := cConstant * stdev;
    for i := FHitRecords.Count - 1 downto 0 do
    begin
      if (TTimetreeHitRecord(FHitRecords[i]).Time > (mean + dMax)) or (TTimetreeHitRecord(FHitRecords[i]).Time < (mean - dMax)) then
      begin
        aRec := TTimetreeHitRecord(FHitRecords[i]);
        FHitRecords.Delete(i);
        aRec.Free;
      end;
    end;
  end;
end;

function TPairwiseResult.GetCiLow(JsonStr: String): Double;
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

    AData := AJson.Find('precomputed_ci_low', jtNumber);
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

function TPairwiseResult.GetCiHigh(JsonStr: String): Double;
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

    AData := AJson.Find('precomputed_ci_high', jtNumber);
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

function TPairwiseResult.GetAdjustedAge(JsonStr: String): Double;
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

function TPairwiseResult.GetPreadjustedAge(JsonStr: String): Double;
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

function TPairwiseResult.GetTimeEstimatesStr(JsonStr: String): String;
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

procedure TPairwiseResult.HandleMissingConfidenceIntervalData;
begin
  if FCIHigh > 0 then
    Exit;
  if (FHitRecords.Count > 1) and (FHitRecords.Count < 5) then
  begin
    FCILow := TTimetreeHitRecord(FHitRecords[0]).Time;
    FCIHigh := TTimetreeHitRecord(FHitRecords[FHitRecords.Count - 1]).Time
  end;
end;

function TPairwiseResult.LoadFromJson(jsonData: String): Boolean;
var
  AParser: TJSONParser;
  AJson: TJSONObject;
  AData: TJSONData;
  aArray: TJSONArray;
  i: Integer;
  adjustedAge, preadjustedAge: Double;
  timeEstimatesString: String;
 begin
  AParser := nil;
  AJson := nil;
  AData := nil;
  Result := True;

  try
    try
      AParser := TJSONParser.Create(jsonData);
      AData := AParser.Parse;
      if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
        raise Exception.Create('failed to parse JSON');
      AJson := TJSONObject(AData);

      AData := AJson.Find('topology_ancestor_id', jtNumber);
      if Assigned(AData) then
        FAncestorId := AData.Value;

      AData := AJson.Find('show_expert', jtBoolean);
      if Assigned(AData) then
        FShowExpert := AData.Value;

      AData := AJson.Find('show_summary', jtBoolean);
      if Assigned(AData) then
        FShowSummary := AData.Value;

      AData := AJson.Find('sum_simple_mol_time', jtString);
      if Assigned(AData) then
        FMolecularTime := StrToFloat(AData.Value);

      AData := AJson.Find('sum_median_time', jtString);
      if Assigned(AData) then
        FMedianTime := StrToFloat(AData.Value);

      AData := AJson.Find('studies', jtObject);
      if Assigned(AData) then
      begin
        adjustedAge := GetAdjustedAge(AData.AsJSON);
        preadjustedAge := GetPreadjustedAge(AData.AsJSON);
        timeEstimatesString := GetTimeEstimatesStr(AData.AsJSON);
        FCILow := GetCiLow(AData.AsJSON);
        FCIHigh := GetCiHigh(AData.AsJSON);
        if Trim(timeEstimatesString) = EmptyStr then
          raise Exception.Create('missing time estimates string');

        FCIParser.ProcessConfidenceInterval(timeEstimatesString, adjustedAge, preadjustedAge, FCILow, FCIHigh, FIsCI);
      end;

      AData := AJson.Find('taxon_a', jtString);
      if Assigned(AData) then
        FTaxonA.SearchString := AData.Value
      else
        raise Exception.Create('missing taxon A');

      AData := AJson.Find('taxon_b', jtString);
      if Assigned(AData) then
        FTaxonB.SearchString := AData.Value
      else
        raise Exception.Create('missing taxon B');

      AData := AJson.Find('taxon_a_id', jtNumber);
      if Assigned(AData) then
        FTaxonA.TimetreeId := AData.Value;

      AData := AJson.Find('taxon_b_id', jtNumber);
      if Assigned(AData) then
        FTaxonB.TimetreeId := AData.Value;

      AData := AJson.Find('scientific_name_a', jtString);
      if Assigned(AData) then
        FTaxonA.ScientificName := AData.Value
      else
        raise Exception.Create('missing scientific name A');

      AData := AJson.Find('scientific_name_b', jtString);
      if Assigned(AData) then
        FTaxonB.ScientificName := AData.Value
      else
        raise Exception.Create('missing scientific name B');

      AData := AJson.Find('common_name_a', jtString);
      if Assigned(AData) then
        FTaxonA.CommonName := AData.Value
      else
        FTaxonA.CommonName := FTaxonA.ScientificName;

      AData := AJson.Find('common_name_b', jtString);
      if Assigned(AData) then
        FTaxonB.CommonName := AData.Value
      else
        FTaxonB.CommonName := FTaxonB.ScientificName;

      AData := AJson.Find('link_taxon_a', jtString);
      if Assigned(AData) then
        FTaxonA.Link := AData.Value
      else
        raise Exception.Create('missing taxon A link');

      AData := AJson.Find('link_taxon_b', jtString);
      if Assigned(AData) then
        FTaxonB.Link := AData.Value
      else
        raise Exception.Create('missing taxon B link');


      AData := AJson.Find('hit_records', jtArray);
      if Assigned(AData) then
      begin
        aArray := TJSONArray(AData);
        if aArray.Count > 0 then
          for i := 0 to aArray.Count - 1 do
          begin
            aData := aArray[i];
            Result := Result and LoadHitRecord(aData.AsJSON);
          end;
      end;
      FNumStudies := FHitRecords.Count; { need to count it here because outliers will not be drawn in the svg}

      FilterOutliers;
      SortRecords;
      HandleMissingConfidenceIntervalData
    except
      on E:Exception do
        raise Exception.Create('Failed to load JSON: ' + E.Message);
    end;
  finally
    if Assigned(AParser) then
      AParser.Free;
    if Assigned(AJson) then
      AJson.Free;
  end;
end;

function TPairwiseResult.LoadHitRecord(aJsonStr: String): Boolean;
var
  AJson: TJSONObject=nil;
  AData: TJSONData=nil;
  AParser: TJSONParser=nil;
  aRec: TTimetreeHitRecord=nil;
begin
  Result := False;

  try
    try
      AParser := TJSONParser.Create(aJsonStr);
      AData := AParser.Parse;
      if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
        raise Exception.Create('failed to parse JSON');
      AJson := TJSONObject(AData);
      aRec := TTimetreeHitRecord.Create;

      AData := AJson.Find('citation_num', jtNumber);
      if Assigned(AData) then
        aRec.CitationNum := AData.Value;

      AData := AJson.Find('pubmed_id', jtNumber);
      if Assigned(AData) then
        aRec.PubmedId := AData.Value;

      AData := AJson.Find('ref_id', jtString);
      if Assigned(AData) then
        aRec.RefId := AData.Value;

      AData := AJson.Find('abst', jtString);
      if Assigned(AData) then
        aRec.Abstr := AData.Value;

      AData := AJson.Find('author', jtString);
      if Assigned(AData) then
        aRec.Author := AData.Value;

      AData := AJson.Find('title', jtString);
      if Assigned(AData) then
        aRec.Title := AData.Value;

      AData := AJson.Find('year', jtNumber);
      if Assigned(AData) then
        aRec.Year := AData.Value;

      AData := AJson.Find('time', jtNumber);
      if Assigned(AData) then
        aRec.Time := AData.Value;
      FHitRecords.Add(aRec);
      Result := True;
    except
      on E:Exception do
        raise Exception.Create('Failed to load JSON: ' + E.Message);
    end;
  finally
    if Assigned(AJson) then
      AJson.Free;
    if Assigned(AParser) then
      AParser.Free;
  end;
end;

function TPairwiseResult.GetMinTime: Double;
var
  i: Integer;
begin
  Result := 4600.0; { 4.6 billion years ago}
  if FHitRecords.Count > 0 then
    for i := 0 to FHitRecords.Count - 1 do
      if TTimetreeHitRecord(FHitRecords[i]).Time < Result then
        Result := TTimetreeHitRecord(FHitRecords[i]).Time;
end;

function TPairwiseResult.GetMaxTime: Double;
var
  i: Integer;
begin
  Result := 0.0;
  if FHitRecords.Count > 0 then
    for i := 0 to FHitRecords.Count - 1 do
      if TTimetreeHitRecord(FHitRecords[i]).Time > Result then
        Result := TTimetreeHitRecord(FHitRecords[i]).Time;
end;

{ TTimetreeHitRecord }

procedure TTimetreeHitRecord.SetAbstract(AValue: String);
begin
  FAbstract:= HtmlEntities(AValue);
end;

procedure TTimetreeHitRecord.SetAuthor(AValue: String);
begin
  FAuthor := HtmlEntities(AValue);
end;

procedure TTimetreeHitRecord.SetCitationNum(AValue: Integer);
begin
  if FCitationNum=AValue then Exit;
  FCitationNum:=AValue;
end;

procedure TTimetreeHitRecord.SetPubmedId(AValue: Integer);
begin
  if FPubmedId=AValue then Exit;
  FPubmedId:=AValue;
end;

procedure TTimetreeHitRecord.SetRefId(AValue: String);
begin
  FRefId := HtmlEntities(AValue);
end;

procedure TTimetreeHitRecord.SetTime(AValue: Double);
begin
  if FTime=AValue then Exit;
  FTime:=AValue;
end;

procedure TTimetreeHitRecord.SetTitle(AValue: String);
begin
  FTitle := HtmlEntities(AValue);
end;

procedure TTimetreeHitRecord.SetYear(AValue: Integer);
begin
  if FYear=AValue then Exit;
  FYear:=AValue;
end;

constructor TTimetreeHitRecord.Create;
begin

end;

destructor TTimetreeHitRecord.Destroy;
begin
  inherited Destroy;
end;

end.

