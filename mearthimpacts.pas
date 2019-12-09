unit mearthimpacts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TEarthImpact }

  TEarthImpact = class(TObject)
  private
    FDiameter: Double;
    FId: Integer;
    FLatitude: Double;
    FLocation: String;
    FLongitude: Double;
    FMaxAge: Double;
    FMinAge: Double;
    FName: String;
    function GetAge: Double;
    protected

    public
      constructor Create;
      destructor Destroy; override;
      function ToDebugString: String;
      property Id: Integer read FId write FId;
      property Name: String read FName write FName;
      property Location: String read FLocation write FLocation;
      property MaxAge: Double read FMaxAge write FMaxAge;
      property MinAge: Double read FMinAge write FMinAge;
      property Age: Double read GetAge;
      property Diameter: Double read FDiameter write FDiameter;
      property Latitude: Double read FLatitude write FLatitude;
      property Longitude: Double read FLongitude write FLongitude;
  end;

  TEarthImpactsArray = array of TEarthImpact;
  { TEarthImpacts }

  TEarthImpacts = class(TList)
    protected
      FTokenSplitter: TStringList; { for faster parsing of the tsv file, we have a persistent list}
      function EarthImpactFactory(aStr: String): TEarthImpact;
    public
      constructor Create;
      destructor Destroy; override;
      function GetMaxDiameter: Double;
      function GetEarthImpacts(StartMya: Double; EndMya: Double): TEarthImpactsArray;
      function LoadFromFile(aFile: String): Boolean;
  end;

  function CompareImpactsByDiameter(Item1: Pointer; Item2: Pointer): Integer;

implementation

uses
  gutils, math;

function CompareImpactsByDiameter(Item1: Pointer; Item2: Pointer): Integer;
begin
  Result := CompareValue(TEarthImpact(Item2).Diameter, TEarthImpact(Item1).Diameter);
end;

{ TEarthImpacts }

function TEarthImpacts.EarthImpactFactory(aStr: String): TEarthImpact;
var
  TempDbl: Double;
begin
  Result := nil;
  try
    FTokenSplitter.Clear;
    FTokenSplitter.CommaText := aStr;

    if FTokenSplitter.Count = 8 then
    begin
      Result := TEarthImpact.Create;
      Result.Id := StrToInt(FTokenSplitter[0]);
      Result.Name := FTokenSplitter[1];
      Result.Location := FTokenSplitter[2];
      if Trim(FTokenSplitter[3]) <> EmptyStr then
        Result.MaxAge := StrToFloat(FTokenSplitter[3]);
      if Trim(FTokenSplitter[4]) <> EmptyStr then
        Result.MinAge := StrToFloat(FTokenSplitter[4]);
      if not ((Result.MaxAge >= 0) or (Result.MinAge >= 0)) then
        raise Exception.Create('missing age data');
      Result.Diameter := StrToFloat(FTokenSplitter[5]);
      Result.Latitude := StrToFloat(FTokenSplitter[6]);
      Result.Longitude := StrToFloat(FTokenSplitter[7]);
    end;
  except
    if Assigned(Result) then
      Result.Free;
    Result := nil;
  end;
end;

constructor TEarthImpacts.Create;
begin
  inherited Create;
  FTokenSplitter := TStringList.Create;
end;

destructor TEarthImpacts.Destroy;
var
  i: Integer;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
      TEarthImpact(inherited Items[i]).Free;
  if Assigned(FTokenSplitter) then
    FTokenSplitter.Free;
  inherited Destroy;
end;

function TEarthImpacts.GetMaxDiameter: Double;
var
  i: Integer;
begin
  Result := 0.0;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      if TEarthImpact(inherited Items[i]).Diameter > Result then
        Result := TEarthImpact(inherited Items[i]).Diameter;
    end;
end;

function TEarthImpacts.GetEarthImpacts(StartMya: Double; EndMya: Double): TEarthImpactsArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do
      if (TEarthImpact(inherited Items[i]).Age <= StartMya) and (TEarthImpact(inherited Items[i]).Age >= EndMya) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := TEarthImpact(inherited Items[i]);
      end;
  end;
end;

function TEarthImpacts.LoadFromFile(aFile: String): Boolean;
var
  aList: TStringList;
  i: Integer;
  aImpact: TEarthImpact;
begin
  Result := False;
  aList := nil;
  if not FileExists(aFile) then
    Exit;
  try
    aList := TStringList.Create;
    aList.LoadFromFile(aFile);
    if aList.Count > 0 then
    begin
      for i := 1 to aList.Count - 1 do { skip the first line which is the header line}
      begin
        aImpact := EarthImpactFactory(aList[i]);
        if not Assigned(aImpact) then
          raise Exception.Create('Invalid earth impact data: ' + aList[i]);
        inherited Add(aImpact);
      end;
      inherited Sort(@CompareImpactsByDiameter);
      Result := True;
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

{ TEarthImpact }

function TEarthImpact.GetAge: Double;
begin
  if (FMaxAge > 0) and (FMinAge > 0) then
    Result := (FMaxAge + FMinAge) * 0.5
  else if MaxAge > 0 then
    Result := MaxAge
  else if MinAge > 0 then
    Result := MinAge
  else
    Result := 0.0;
end;

constructor TEarthImpact.Create;
begin
  FDiameter := -1;
  FId := -1;
  FLatitude := -1;
  FLocation := EmptyStr;
  FLongitude := -1;
  FMaxAge := -1;
  FMinAge := -1;
  FName := EmptyStr;
end;

destructor TEarthImpact.Destroy;
begin
  inherited Destroy;
end;

function TEarthImpact.ToDebugString: String;
begin
  Result := Format('%d %.3f %.3f %.3f %.3f %.3f %.3f %s %s', [FID, FDiameter, FLatitude, FLongitude, FMaxAge, FMinAge, Age, FName, FLocation]);
end;

end.

