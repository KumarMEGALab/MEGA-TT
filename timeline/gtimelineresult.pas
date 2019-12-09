unit gtimelineresult;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsonparser, gtaxonomicrank;

type

  { TTimelineResult }

  TTimelineResult = class(TObject)

    private
      FBranchLength: Double;
      FCommonName: String;
      FConfidenceIntervalHigh: Double;
      FConfidenceIntervalLow: Double;
      FFigurinesList: TStringList;
      FFigurineXCoord: Integer;
      FFigurineYCoord: Integer;
      FId: Integer;
      FIsAmbiguous: Boolean;
      FIsConfidenceInterval: Boolean;
      FLineYCoord: Integer;
      FScientificName: String;
      FNameXCoord: Integer;
      FNameYCoord: Integer;
      FTaxonomicRank: TTaxonomicRank;
      FTimelineXCoord: Integer;
      FTimelineYCoord: Integer;
      FTimeTextXCoord: Integer;
      FTimeTextYCoord: Integer;
      FTimeTreeId: Integer;
      function GetIsUnknownEvent: Boolean;
      procedure SetBranchLength(AValue: Double);
      procedure SetCommonName(AValue: String);
      procedure SetFigurinesList(AValue: TStringList);
      procedure SetId(AValue: Integer);
      procedure SetScientificName(AValue: String);
      procedure SetTimeTreeId(AValue: Integer);

    public
      constructor Create;

      function IsEqual(Other: TTimelineResult): Boolean;
      procedure Assign(Source: TTimelineResult);
      property Id: Integer read FId write SetId;
      property TimeTreeId: Integer read FTimeTreeId write SetTimeTreeId;
      property ScientificName: String read FScientificName write SetScientificName;
      property CommonName: String read FCommonName write SetCommonName;
      property BranchLength: Double read FBranchLength write SetBranchLength;
      property FigurinesList: TStringList read FFigurinesList write SetFigurinesList;
      property NameXCoord: Integer read FNameXCoord write FNameXCoord;
      property NameYCoord: Integer read FNameYCoord write FNameYCoord;
      property TimelineXCoord: Integer read FTimelineXCoord write FTimelineXCoord;
      property TimelineYCoord: Integer read FTimelineYCoord write FTimelineYCoord;
      property FigurineXCoord: Integer read FFigurineXCoord write FFigurineXCoord;
      property FigurineYCoord: Integer read FFigurineYCoord write FFigurineYCoord;
      property LineYCoord: Integer read FLineYCoord write FLineYCoord;
      property TimeTextXCoord: Integer read FTimeTextXCoord write FTimeTextXCoord;
      property TimeTextYCoord: Integer read FTimeTextYCoord write FTimeTextYCoord;
      property TaxonomicRank: TTaxonomicRank read FTaxonomicRank write FTaxonomicRank;
      property IsAmbiguous: Boolean read FIsAmbiguous write FIsAmbiguous;
      property IsUnknownEvent: Boolean read GetIsUnknownEvent;
      property ConfidenceIntervalLow: Double read FConfidenceIntervalLow write FConfidenceIntervalLow;
      property ConfidenceIntervalHigh: Double read FConfidenceIntervalHigh write FConfidenceIntervalHigh;
      property IsConfidenceInterval: Boolean read FIsConfidenceInterval write FIsConfidenceInterval;
  end;


implementation

uses
  math, ttconst;

{ TTimelineResult }

procedure TTimelineResult.SetBranchLength(AValue: Double);
begin
  if FBranchLength=AValue then Exit;
  FBranchLength:=AValue;
end;

function TTimelineResult.GetIsUnknownEvent: Boolean;
begin
  Result := (LowerCase(Trim(ScientificName)) = LowerCase(UNKNOWN_TAXON_NAME));
end;

procedure TTimelineResult.SetCommonName(AValue: String);
begin
  if FCommonName=AValue then Exit;
  FCommonName:=AValue;
end;

procedure TTimelineResult.SetFigurinesList(AValue: TStringList);
begin
  if FFigurinesList=AValue then Exit;
  FFigurinesList:=AValue;
end;

procedure TTimelineResult.SetId(AValue: Integer);
begin
  if FId=AValue then Exit;
  FId:=AValue;
end;

procedure TTimelineResult.SetScientificName(AValue: String);
begin
  if FScientificName=AValue then Exit;
  FScientificName:=AValue;
end;

procedure TTimelineResult.SetTimeTreeId(AValue: Integer);
begin
  if FTimeTreeId=AValue then Exit;
  FTimeTreeId:=AValue;
end;

constructor TTimelineResult.Create;
begin
  FBranchLength := 0.0;
  FConfidenceIntervalHigh := 0.0;
  FConfidenceIntervalLow := 0.0;
  FIsAmbiguous := False;
  FCommonName := EmptyStr;
  FFigurinesList := TStringList.Create;
  FId := -1;
  FTimeTreeId := -1;
  FScientificName := EmptyStr;
  FTaxonomicRank := trUnknown;
end;

function TTimelineResult.IsEqual(Other: TTimelineResult): Boolean;
var
  i: Integer;
begin
  Result := (FId = Other.Id) and
            (FScientificName = Other.ScientificName) and
            (FCommonName = Other.CommonName) and
            (CompareValue(FBranchLength, Other.BranchLength, 0.001) = 0) and
            (FFigurinesList.Count = Other.FigurinesList.Count);
  if FFigurinesList.Count > 0 then
    for i := 0 to FFigurinesList.Count - 1 do
      Result := Result and (FFigurinesList[i] = Other.FigurinesList[i]);
end;

procedure TTimelineResult.Assign(Source: TTimelineResult);
begin
  FBranchLength := Source.FBranchLength;
  FCommonName := Source.FCommonName;
  FFigurinesList.Assign(Source.FigurinesList);
  FId := Source.FId;
  FTimeTreeId := Source.FTimeTreeId;
  FScientificName := Source.FScientificName;
  FTaxonomicRank := Source.TaxonomicRank;
  FIsAmbiguous := Source.IsAmbiguous;
  FConfidenceIntervalLow := Source.FConfidenceIntervalLow;
  FConfidenceIntervalHigh := Source.FConfidenceIntervalHigh;
  FIsConfidenceInterval := Source.FIsConfidenceInterval;
end;


end.

