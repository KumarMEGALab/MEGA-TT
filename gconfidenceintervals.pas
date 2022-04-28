unit gconfidenceintervals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TConfidenceInterval }

  TConfidenceInterval = record
    LowerBound: Double;
    TimetreeId: LongInt;
    UpperBound: Double;
    AdjustedAge: Double;
    IsConfidenceInterval: Boolean; { otherwise it is a range because we did not have enough data to calculate a reasonable confidence interval}
  end;

  TConfidenceIntervalArray = array of TConfidenceInterval;

  { TConfidenceIntervalParser }

  TConfidenceIntervalParser = class(TObject)
    private
      FTokenSplitter: TStringList; { for faster parsing of the csv file, we have a persistent list}
      FTimesTokenSplitter: TStringList; { for parsing just the time estimates}
      FTimeEstimates: TList;

    protected
      function ParseTimeEstimates(aStr: String): Integer;
      function ParseConfidenceInterval(const aText: String; var aId: LongInt; var aLower: Double; var aUpper: Double; var IsConfidenceInterval: Boolean; var aAdjustedAge: Double): Boolean; overload;
      //function ParseConfidenceInterval(const aText: String; var aLower: Double; var aUpper: Double; var IsConfidenceInterval: Boolean): Boolean; overload;
      function IsOutsideOfInterval(const adjustedAge: Double; const ciLow: Double; const ciHigh: Double): Boolean;
      procedure AdjustConfidenceInterval(var ciLow: Double; var ciHigh: Double; const adjustedAge: Double; const preadjustedAge: Double);
      procedure GetRange(var ciLow: Double; var ciHigh: Double);
    public
      constructor Create;
      destructor Destroy; override;

      function ProcessConfidenceInterval(const EstimatesStr: String; const adjustedAge: Double; const preAdjustedAge: Double;  var aLower: Double; var aUpper: Double; var IsConfidenceInterval: Boolean): Boolean; deprecated 'CIs are now handled completely on the server';
  end;

  { TLoadConfidenceIntervalsThread }

  TLoadConfidenceIntervalsThread = class(TThread)

    private
      FIntervalParser: TConfidenceIntervalParser;
      FConfidenceIntervalsFile: String;
      FEndTime: TDateTime;
      FIsSuccess: Boolean;
      FLogStrings: TStringList;
      FStartTime: TDateTime;
      procedure SetLogStrings(AValue: TStringList);

    protected
      procedure LoadConfidenceIntervals;
      //function ParseTimeEstimates(aStr: String): Integer;
      //function ParseConfidenceInterval(const aText: String; var aId: LongInt; var aLower: Double; var aUpper: Double; var IsConfidenceInterval: Boolean): Boolean;
      //function IsOutsideOfInterval(const adjustedAge: Double; const ciLow: Double; const ciHigh: Double): Boolean;
      //procedure AdjustConfidenceInterval(var ciLow: Double; var ciHigh: Double; const adjustedAge: Double; const preadjustedAge: Double);
      //procedure GetRange(var ciLow: Double; var ciHigh: Double);
    public
      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;
      procedure Execute; override;

      property ConfidenceIntervalsFile: String read FConfidenceIntervalsFile write FConfidenceIntervalsFile;
      property IsSuccess: Boolean read FIsSuccess;
      property StartTime: TDateTime read FStartTime;
      property EndTime: TDateTime read FEndTime;
      property LogStrings: TStringList read FLogStrings write SetLogStrings;
  end;

var
  ConfidenceIntervals: TConfidenceIntervalArray;

  function SortTimeEstimates(Item1: Pointer; Item2: Pointer): Integer;

implementation

uses
  ttconst, dateutils, math;

function SortTimeEstimates(Item1: Pointer; Item2: Pointer): Integer;
begin
  Result := CompareValue(Double(Item1), Double(Item2));
end;

{ TConfidenceIntervalParser }

function TConfidenceIntervalParser.ParseTimeEstimates(aStr: String): Integer;
var
  subString: String;
  i: Integer;
  aEstimate: Double;
begin
  subString := Copy(aStr, 2, Length(aStr) - 2);
  FTimesTokenSplitter.Clear;
  FTimesTokenSplitter.CommaText := subString;
  FTimeEstimates.Clear;
  if FTimesTokenSplitter.Count > 0 then
  begin
    for i := 0 to FTimesTokenSplitter.Count - 1 do
    begin
      aEstimate := StrToFloat(FTimesTokenSplitter[i]);
      FTimeEstimates.Add(Pointer(aEstimate));
    end;
  end;
  Result := FTimeEstimates.Count;
end;

function TConfidenceIntervalParser.ParseConfidenceInterval(const aText: String; var aId: LongInt; var aLower: Double; var aUpper: Double; var IsConfidenceInterval: Boolean; var aAdjustedAge: Double): Boolean;
var
  preadjustedAge, adjustedAge: Double;
begin
  Result := False;
  FTokenSplitter.Clear;
  FTokenSplitter.CommaText := aText;

  if FTokenSplitter.Count = 3 then
  begin
    try
      aId := StrToInt(FTokenSplitter[0]);
      aLower := StrToFloat(FTokenSplitter[1]);
      aUpper := StrToFloat(FTokenSplitter[2]);
    except
      aId := StrToInt(FTokenSplitter[0]);
      aLower := 0;
      aUpper := 0;
      adjustedAge := 0;
    end;
  end
  else
  if FTokenSplitter.Count = 6 then
  begin
    aId := StrToInt(FTokenSplitter[1]);
    adjustedAge := StrToFloat(FTokenSplitter[2]);
    aLower := StrToFloat(FTokenSplitter[3]);
    aUpper := StrToFloat(FTokenSplitter[4]);
    if Trim(FTokenSplitter[5]) <> EmptyStr then
      preadjustedAge := StrToFloat(FTokenSplitter[5])
    else
      preadjustedAge := 0.0;
    aAdjustedAge := preadjustedAge;
    Result := ProcessConfidenceInterval(FTokenSplitter[0], adjustedAge, preadjustedAge, aLower, aUpper,  IsConfidenceInterval);
  end
  else
  begin
    aId := StrToInt(FTokenSplitter[1]);
    aLower := 0;
    aUpper := 0;
    adjustedAge := 0;
    IsConfidenceInterval := False;
    //raise Exception.Create('failed to parse confidence interval, expected 3 tokens, got ' + IntToStr(FTokenSplitter.Count));
  end;
  Result := True;
end;

//function TConfidenceIntervalParser.ParseConfidenceInterval(const aText: String; var aLower: Double; var aUpper: Double; var IsConfidenceInterval: Boolean): Boolean;
//var
//  numEstimates: Integer;
//  adjustedAge, preadjustedAge: Double;
//begin
//  numEstimates := ParseTimeEstimates(FTokenSplitter[0]);
//  adjustedAge := StrToFloat(FTokenSplitter[2]);
//  if (numEstimates > 1) and (numEstimates < 5) then
//  begin
//    GetRange(aLower, aUpper);
//    IsConfidenceInterval := False;
//  end
//  else
//  begin
//    if NumEstimates <= 1 then
//      IsConfidenceInterval := False
//    else
//      IsConfidenceInterval := True;
//    aLower := StrToFloat(FTokenSplitter[3]);
//    aUpper := StrToFloat(FTokenSplitter[4]);
//    if (FTokenSplitter[5] <> EmptyStr) and (aUpper > 0) and IsOutsideOfInterval(adjustedAge, aLower, aUpper) then
//    begin
//      preadjustedAge := StrToFloat(FTokenSplitter[5]);
//      AdjustConfidenceInterval(aLower, aUpper, adjustedAge, preadjustedAge);
//    end;
//  end;
//end;

function TConfidenceIntervalParser.ProcessConfidenceInterval(const EstimatesStr: String; const adjustedAge: Double; const preAdjustedAge: Double;  var aLower: Double; var aUpper: Double; var IsConfidenceInterval: Boolean): Boolean;
var
  numEstimates: Integer;
begin
  //raise Exception.Create('Application Error: call to deprecated TConfidenceIntervalParser.ProcessConfidenceInterval function');
  numEstimates := ParseTimeEstimates(EstimatesStr);
  if (numEstimates > 1) and (numEstimates < 5) then
  begin
    GetRange(aLower, aUpper);
    IsConfidenceInterval := False;
  end
  else
  begin
    if NumEstimates <= 1 then
      IsConfidenceInterval := False
    else
      IsConfidenceInterval := True;
    if (preadjustedAge > 0) and (aUpper > 0) and IsOutsideOfInterval(adjustedAge, aLower, aUpper) then
      AdjustConfidenceInterval(aLower, aUpper, adjustedAge, preadjustedAge);
  end;
end;

function TConfidenceIntervalParser.IsOutsideOfInterval(const adjustedAge: Double; const ciLow: Double; const ciHigh: Double): Boolean;
begin
  Result := ((adjustedAge < ciLow) or (adjustedAge > ciHigh));
end;

procedure TConfidenceIntervalParser.AdjustConfidenceInterval(var ciLow: Double; var ciHigh: Double; const adjustedAge: Double; const preadjustedAge: Double);
var
  deltaLow, deltaHigh: Double;
begin
  deltaLow := preadjustedAge - ciLow;
  deltaHigh := ciHigh - preadjustedAge;
  if ciLow > 0 then
    ciLow := adjustedAge - deltaLow;
  if ciHigh > 0 then
    ciHigh := adjustedAge + deltaHigh;
end;

procedure TConfidenceIntervalParser.GetRange(var ciLow: Double; var ciHigh: Double);
begin
  FTimeEstimates.Sort(@SortTimeEstimates);
  ciLow := Double(FTimeEstimates[0]);
  ciHigh := Double(FTimeEstimates[FTimeEstimates.Count - 1]);
end;

constructor TConfidenceIntervalParser.Create;
begin
  FTokenSplitter := TStringList.Create;
  FTimesTokenSplitter := TStringList.Create;
  FTimeEstimates := TList.Create;
end;

destructor TConfidenceIntervalParser.Destroy;
begin
  if Assigned(FTokenSplitter) then
    FTokenSplitter.Free;
  if Assigned(FTimesTokenSplitter) then
    FTimesTokenSplitter.Free;
  if Assigned(FTimeEstimates) then
    FTimeEstimates.Free;
  inherited Destroy;
end;

{ TLoadConfidenceIntervalsThread }

procedure TLoadConfidenceIntervalsThread.SetLogStrings(AValue: TStringList);
begin
  if FLogStrings=AValue then Exit;
  FLogStrings:=AValue;
end;

procedure TLoadConfidenceIntervalsThread.LoadConfidenceIntervals;
var
  sList: TStringList;
  i: Integer;
  temp: String;
  aId: LongInt;
  aLower, aUpper, aAdjustedAge: Double;
  isConfidenceInterval: Boolean;
begin
  FStartTime := Now;
  FIsSuccess := False;
  sList := nil;
  if not FileExists(ConfidenceIntervalsFile) then
    Exit;

  try
    try
      sList := TStringList.Create;
      sList.LoadFromFile(ConfidenceIntervalsFile);
      if sList.Count > 0 then
      begin
        SetLength(ConfidenceIntervals, 5561270 + 10000); { there are IDs in the timetree DB that skip a number so just add extra, we don't want to search this list, just access items directly by their timetree ID}

        for i := 1 to sList.Count - 1 do { first line is a header so skip it}
        begin
          if Trim(sList[i]) = EmptyStr then
            continue;
          if i = 52164 then
            FIsSuccess := True;
          if not FIntervalParser.ParseConfidenceInterval(sList[i], aId, aLower, aUpper, isConfidenceInterval, aAdjustedAge) then
            raise Exception.Create('failed to parse confidence interval: ' + sList[i]);

          ConfidenceIntervals[aId].LowerBound := aLower;
          ConfidenceIntervals[aId].UpperBound := aUpper;
          ConfidenceIntervals[aId].AdjustedAge := aAdjustedAge;
          ConfidenceIntervals[aId].IsConfidenceInterval := isConfidenceInterval;
        end;
        FIsSuccess := True;
      end;
      FEndTime := Now;
      if Assigned(FLogStrings) then
      begin
        try
          LogfileCriticalSect.Acquire;
          temp := 'Load confidence intervals: execution time = ' + IntToStr(MilliSecondsBetween(FEndTime, FStartTime)) + ' ms';
          FLogStrings.Add(temp);
        finally
          LogfileCriticalSect.Release;
        end;
      end;
    except
      SetLength(ConfidenceIntervals, 0);
    end;
  finally
    if Assigned(sList) then
      sList.Free;
  end;
end;

//function TLoadConfidenceIntervalsThread.ParseTimeEstimates(aStr: String): Integer;
//var
//  subString: String;
//  i: Integer;
//  aEstimate: Double;
//begin
//  subString := Copy(aStr, 2, Length(aStr) - 2);
//  FTimesTokenSplitter.Clear;
//  FTimesTokenSplitter.CommaText := subString;
//  FTimeEstimates.Clear;
//  if FTimesTokenSplitter.Count > 0 then
//  begin
//    for i := 0 to FTimesTokenSplitter.Count - 1 do
//    begin
//      aEstimate := StrToFloat(FTimesTokenSplitter[i]);
//      FTimeEstimates.Add(Pointer(aEstimate));
//    end;
//  end;
//  Result := FTimeEstimates.Count;
//end;

//function TLoadConfidenceIntervalsThread.ParseConfidenceInterval(const aText: String; var aId: LongInt; var aLower: Double; var aUpper: Double; var IsConfidenceInterval: Boolean): Boolean;
//var
//  preadjustedAge, adjustedAge: Double;
//  numEstimates: Integer;
//begin
//  Result := False;
//  FTokenSplitter.Clear;
//  FTokenSplitter.CommaText := aText;
//
//  if FTokenSplitter.Count = 3 then
//  begin
//    aId := StrToInt(FTokenSplitter[0]);
//    aLower := StrToFloat(FTokenSplitter[1]);
//    aUpper := StrToFloat(FTokenSplitter[2]);
//  end
//  else
//  if FTokenSplitter.Count = 6 then
//  begin
//    numEstimates := ParseTimeEstimates(FTokenSplitter[0]);
//    aId := StrToInt(FTokenSplitter[1]);
//    adjustedAge := StrToFloat(FTokenSplitter[2]);
//    if (numEstimates > 1) and (numEstimates < 5) then
//    begin
//      GetRange(aLower, aUpper);
//      IsConfidenceInterval := False;
//    end
//    else
//    begin
//      if NumEstimates <= 1 then
//        IsConfidenceInterval := False
//      else
//        IsConfidenceInterval := True;
//      aLower := StrToFloat(FTokenSplitter[3]);
//      aUpper := StrToFloat(FTokenSplitter[4]);
//      if (FTokenSplitter[5] <> EmptyStr) and (aUpper > 0) and IsOutsideOfInterval(adjustedAge, aLower, aUpper) then
//      begin
//        preadjustedAge := StrToFloat(FTokenSplitter[5]);
//        AdjustConfidenceInterval(aLower, aUpper, adjustedAge, preadjustedAge);
//      end;
//    end;
//  end
//  else
//    raise Exception.Create('failed to parse confidence interval, expected 3 tokens, got ' + IntToStr(FTokenSplitter.Count));
//  Result := True;
//end;

//function TLoadConfidenceIntervalsThread.IsOutsideOfInterval( const adjustedAge: Double; const ciLow: Double; const ciHigh: Double): Boolean;
//begin
// Result := ((adjustedAge < ciLow) or (adjustedAge > ciHigh));
//end;

//procedure TLoadConfidenceIntervalsThread.AdjustConfidenceInterval(var ciLow: Double; var ciHigh: Double; const adjustedAge: Double; const preadjustedAge: Double);
//var
//  deltaLow, deltaHigh: Double;
//begin
//  deltaLow := preadjustedAge - ciLow;
//  deltaHigh := ciHigh - preadjustedAge;
//  if ciLow > 0 then
//    ciLow := adjustedAge - deltaLow;
//  if ciHigh > 0 then
//    ciHigh := adjustedAge + deltaHigh;
//end;

//procedure TLoadConfidenceIntervalsThread.GetRange(var ciLow: Double; var ciHigh: Double);
//begin
//  FTimeEstimates.Sort(@SortTimeEstimates);
//  ciLow := Double(FTimeEstimates[0]);
//  ciHigh := Double(FTimeEstimates[FTimeEstimates.Count - 1]);
//end;

constructor TLoadConfidenceIntervalsThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FIntervalParser := TConfidenceIntervalParser.Create;
end;

destructor TLoadConfidenceIntervalsThread.Destroy;
begin
  if Assigned(FIntervalParser) then
    FIntervalParser.Free;
  inherited Destroy;
end;

procedure TLoadConfidenceIntervalsThread.Execute;
begin
  LoadConfidenceIntervals;
end;

end.

