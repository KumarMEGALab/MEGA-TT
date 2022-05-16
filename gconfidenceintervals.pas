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
    PrecomputedAge: Double;
    AdjustedAge: Double;
    IsConfidenceInterval: Boolean; { otherwise it is a range because we did not have enough data to calculate a reasonable confidence interval}
    CiString: String;
  end;

  TConfidenceIntervalArray = array of TConfidenceInterval;

  { TConfidenceIntervalParser }

  TConfidenceIntervalParser = class(TObject)
    private
      FTokenSplitter: TStringList; { for faster parsing of the csv file, we have a persistent list}
      FTimesTokenSplitter: TStringList; { for parsing just the time estimates}
      FTimeEstimates: TList;
      function IsWhiteSpace(TheChar: String): Boolean;
      function SplitOnSingleCharFaster(TheString: String; Delimiter: Char; var Tokens: TStringList): Boolean;
    protected
      function ParseTimeEstimates(aStr: String): Integer;
      function ParseConfidenceInterval(const aText: String; var aId: LongInt; var aLower: Double; var aUpper: Double; var IsConfidenceInterval: Boolean; var aAdjustedAge: Double; var ciString: String; var precomputedAge: Double): Boolean; overload;
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

function TConfidenceIntervalParser.IsWhiteSpace(TheChar: String): Boolean;
begin
  Result := (TheChar = ' ') or
            (TheChar = #9) or
            (TheChar = #13#10) or
            (TheChar = #10) or
            (TheChar = #13) {$IFDEF FPC}or
            (TheChar = LineEnding){$ENDIF};
end;

function TConfidenceIntervalParser.SplitOnSingleCharFaster(TheString: String; Delimiter: Char; var Tokens: TStringList): Boolean;
var
  CurrentPosition: Integer;
  TempString: String;
  CleanString: String;
  i: Integer;
begin
  Tokens.Clear;
  Result := True;
  if TheString[Length(TheString)] = Delimiter then
    CleanString := Copy(TheString, 1, Length(TheString) - 1)
  else
    CleanString := TheString;

  try
    i := 1;
    CurrentPosition := 1;
    while i < Length(CleanString) do
    begin
      if (CleanString[i] = #34) and (i < Length(CleanString)) then
      begin
        inc(i);
        while (i < Length(CleanString)) and (CleanString[i] <> #34) do
          inc(i);
        inc(i);
        TempString := Trim(Copy(CleanString, CurrentPosition, i - CurrentPosition));
        Tokens.Add(TempString);
        while (i < Length(CleanString)) and  (CleanString[i] = #34) do
          inc(i);
        if CleanString[i] = Delimiter then
          inc(i);
        CurrentPosition := i;
      end;
      if (CleanString[i] = Delimiter) then
      begin
        TempString := Trim(Copy(CleanString, CurrentPosition, i - CurrentPosition));
        Tokens.Add(TempString);
        while (i < Length(CleanString)) and  (CleanString[i] = Delimiter) do
          inc(i);
        CurrentPosition := i;
      end
      else
        inc(i);
    end;
    TempString := Trim(Copy(CleanString, CurrentPosition, Length(CleanString)));
    if (TempString <> EmptyStr) then
      Tokens.Add(TempString);
  Except
    on E:Exception do
    begin
      Result := False;
    end;
  end;
end;

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

function TConfidenceIntervalParser.ParseConfidenceInterval(const aText: String; var aId: LongInt; var aLower: Double; var aUpper: Double; var IsConfidenceInterval: Boolean; var aAdjustedAge: Double; var ciString: String; var precomputedAge: Double): Boolean;
var
  preadjustedAge: Double;
  aToken: String = '';
begin
  Result := False;
  FTokenSplitter.Clear;
  //FTokenSplitter.CommaText := aText;
  SplitOnSingleCharFaster(aText, ',', FTokenSplitter);

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
      precomputedAge := 0;
    end;
  end
  else
  if FTokenSplitter.Count >= 6 then
  begin
    aId := StrToInt(FTokenSplitter[1]);
    precomputedAge := StrToFloat(FTokenSplitter[2]);
    Assert(CompareValue(precomputedAge, 0, FP_CUTOFF) > 0, 'invalid precomputed age');
    aLower := StrToFloat(FTokenSplitter[3]);
    aUpper := StrToFloat(FTokenSplitter[4]);
    aToken := Trim(FTokenSplitter[5]);
    if aToken <> EmptyStr then
    begin
      if (Pos('CI:', aToken) = 1) or (Pos('Range:', aToken) = 1) then
      begin
        ciString := aToken;
      end
      else
        preadjustedAge := StrToFloat(FTokenSplitter[5])
    end
    else
      preadjustedAge := 0.0;
    aAdjustedAge := preadjustedAge;
    if FTokenSplitter.Count > 6 then
      ciString := FTokenSplitter[6];
  end
  else
  begin
    aId := StrToInt(FTokenSplitter[1]);
    aLower := 0;
    aUpper := 0;
    if FTokenSplitter.Count > 2 then
      precomputedAge := StrToFloat(FTokenSplitter[2])
    else
      precomputedAge := 0;
    IsConfidenceInterval := False;
  end;
  Result := True;
end;

function TConfidenceIntervalParser.ProcessConfidenceInterval(const EstimatesStr: String; const adjustedAge: Double; const preAdjustedAge: Double;  var aLower: Double; var aUpper: Double; var IsConfidenceInterval: Boolean): Boolean;
var
  numEstimates: Integer;
begin
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
  aLower, aUpper, aAdjustedAge, aPrecomputedAge: Double;
  isConfidenceInterval: Boolean;
  ciString: String = '';
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
          if not FIntervalParser.ParseConfidenceInterval(sList[i], aId, aLower, aUpper, isConfidenceInterval, aAdjustedAge, ciString, aPrecomputedAge) then
            raise Exception.Create('failed to parse confidence interval: ' + sList[i]);
          ConfidenceIntervals[aId].LowerBound := aLower;
          ConfidenceIntervals[aId].UpperBound := aUpper;
          ConfidenceIntervals[aId].AdjustedAge := aAdjustedAge;
          ConfidenceIntervals[aId].IsConfidenceInterval := isConfidenceInterval;
          ConfidenceIntervals[aId].CiString := ciString;
          ConfidenceIntervals[aId].PrecomputedAge := aPrecomputedAge;
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

