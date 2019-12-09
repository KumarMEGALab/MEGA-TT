unit gutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


procedure FreeMemAndNil(var ptr; size: Integer = -1);
procedure TrimTaxaName(var S: AnsiString); overload;
procedure TrimTaxaName2(var S: String); overload;
function NextAvailableFilename(aName: String; NewExtension: String): String;
function SplitOnSingleChar(TheString: String; Delimiter: Char; var Tokens: TStringList; SplitOnWhiteSpace: Boolean = False): Boolean;
function IsWhiteSpace(TheChar: String): Boolean;
function DumpExceptionCallStack(E: Exception): TStringList;
function Mince(PathToMince: String; MaxNumChars: Integer): String;
function SplitOnWhiteSpace(TheString: String): TStringList;
function NumOccurences(FullString: String; SubString: String; IgnoreCase: Boolean=True): Integer;
function HorizontalCenterOfRect(aRect: TRect): Integer;
function VerticalCenterOfRect(aRect: TRect): Integer;
procedure FormatTimeIntervalStrings(const divTime: Double; const ciLow: Double; const ciHigh: Double; var divTimeStr: String; var ciLowStr: String; var ciHighStr: String); overload;
procedure FormatTimeIntervalStrings(const medianTime: Double; const divTime: Double; const ciLow: Double; const ciHigh: Double; var medianTimeStr: String; var divTimeStr: String; var ciLowStr: String; var ciHighStr: String); overload;
function AnyTwoStringsAreSame(str1, str2, str3: String): Boolean; overload;
function AnyTwoStringsAreSame(str1, str2, str3, str4: String): Boolean; overload;

implementation

uses
  ttconst, strutils, math;

function NumOccurences(FullString: String; SubString: String; IgnoreCase: Boolean=True): Integer;
var
  Offset: Integer;
  aFullString: String;
  aSubString: String;
begin
  if IgnoreCase then
  begin
    aFullString := LowerCase(FullString);
    aSubString := LowerCase(SubString);
  end
  else
  begin
    aFullString := FullString;
    aSubString := SubString;
  end;

  Result := 0;
  Offset := PosEx(aSubString, aFullString, 1);
  while Offset <> 0 do
  begin
    inc(Result);
    Offset := PosEx(aSubString, aFullString, Offset + length(aSubString));
  end;
end;

function HorizontalCenterOfRect(aRect: TRect): Integer;
begin
  Assert(aRect.Right >= aRect.Left);
  if aRect.Right = aRect.Left then
    Result := aRect.Left
  else
    Result := aRect.Left + Round((aRect.Right - aRect.Left) / 2)
end;

function VerticalCenterOfRect(aRect: TRect): Integer;
begin
  Assert(aRect.Bottom >= aRect.Top);
  if ARect.Bottom = aRect.Top then
    Result := aRect.Bottom
  else
    Result := aRect.Top + Round((aRect.Bottom - aRect.Top) / 2);
end;

procedure FormatTimeIntervalStrings(const divTime: Double; const ciLow: Double;const ciHigh: Double; var divTimeStr: String; var ciLowStr: String; var ciHighStr: String);
var
  formatStr: String;
  precision: Integer=0;
begin
  if divTime < 0.1 then
    precision := 3
  else if divTime < 1.0 then
    precision := 2
  else if divTime < 5.0 then
    precision := 1
  else
    precision := 0;

  formatStr := '%.' + IntToStr(precision) + 'f';
  divTimeStr := Format(formatStr, [divTime]);
  ciLowStr := Format(formatStr, [ciLow]);
  ciHighStr := Format(formatStr, [ciHigh]);

  if (ciLow > 0) and (ciHigh > 0) and (CompareValue(ciHigh, divTime, 0.0000001) <> 0) then
  begin
    while AnyTwoStringsAreSame(divTimeStr, ciLowStr, ciHighStr) and (precision < 8) do
    begin
      inc(precision);
      formatStr := '%.' + IntToStr(precision) + 'f';
      divTimeStr := Format(formatStr, [divTime]);
      ciLowStr := Format(formatStr, [ciLow]);
      ciHighStr := Format(formatStr, [ciHigh]);
    end;
  end
  else
  begin
    while (not (StrToFloat(divTimeStr) > 0)) and (precision < 8) do
    begin
      inc(precision);
      formatStr := '%.' + IntToStr(precision) + 'f';
      divTimeStr := Format(formatStr, [divTime]);
    end;
  end;
  if divTime < 30 then
  begin
    inc(precision);
    formatStr := '%.' + IntToStr(precision) + 'f';
    divTimeStr := Format(formatStr, [divTime]);
    ciLowStr := Format(formatStr, [ciLow]);
    ciHighStr := Format(formatStr, [ciHigh]);
  end;
end;

procedure FormatTimeIntervalStrings(const medianTime: Double; const divTime: Double; const ciLow: Double; const ciHigh: Double; var medianTimeStr: String; var divTimeStr: String; var ciLowStr: String; var ciHighStr: String);
var
  formatStr: String;
  precision: Integer=0;
  maxPrecision: Integer;
begin
  if divTime < 0.1 then
  begin
    precision := 3;
    maxPrecision := 4;
  end
  else if divTime < 1.0 then
  begin
    precision := 2;
    maxPrecision := 3;
  end
  else if divTime < 5.0 then
  begin
    precision := 1;
    maxPrecision := 2;
  end
  else
  begin
    precision := 0;
    maxPrecision := 1;
  end;

  formatStr := '%.' + IntToStr(precision) + 'f';
  divTimeStr := Format(formatStr, [divTime]);
  ciLowStr := Format(formatStr, [ciLow]);
  ciHighStr := Format(formatStr, [ciHigh]);
  medianTimeStr := Format(formatStr, [medianTime]);

  if (ciLow > 0) and (ciHigh > 0) and (CompareValue(ciHigh, divTime, 0.0000001) <> 0) then
  begin
    while AnyTwoStringsAreSame(medianTimeStr, divTimeStr, ciLowStr, ciHighStr)  and (precision < maxPrecision) do
    begin
      inc(precision);
      formatStr := '%.' + IntToStr(precision) + 'f';
      divTimeStr := Format(formatStr, [divTime]);
      ciLowStr := Format(formatStr, [ciLow]);
      ciHighStr := Format(formatStr, [ciHigh]);
      medianTimeStr := Format(formatStr, [medianTime]);
    end;
  end
  else
  begin
    while (not (StrToFloat(divTimeStr) > 0)) and (precision < maxPrecision) do
    begin
      inc(precision);
      formatStr := '%.' + IntToStr(precision) + 'f';
      divTimeStr := Format(formatStr, [divTime]);
      medianTimeStr := Format(formatStr, [medianTime]);
    end;
  end;
  if divTime < 30 then
  begin
    inc(precision);
    formatStr := '%.' + IntToStr(precision) + 'f';
    divTimeStr := Format(formatStr, [divTime]);
    ciLowStr := Format(formatStr, [ciLow]);
    ciHighStr := Format(formatStr, [ciHigh]);
    medianTimeStr := Format(formatStr, [medianTime]);
  end;
end;

function AnyTwoStringsAreSame(str1, str2, str3: String): Boolean;
begin
  Result := ((str1 = str2) or (str1 = str3) or (str2 = str3));
end;

function AnyTwoStringsAreSame(str1, str2, str3, str4: String): Boolean;
begin
  Result := ((str1 = str2) or (str1 = str3) or (str1 = str4) or (str2 = str3) or (str2 = str4) or (str3 = str4));
end;

function SplitOnWhiteSpace(TheString: String): TStringList;
var
  i: Integer;
  CleanString: String;
  Temp: String;
  Token: String;
begin
  Token := EmptyStr;
  CleanString := trim(TheString);
  Result := TStringList.Create;
  for i := 1 to Length(CleanString) do
  begin
    Temp := CleanString[i];
    if not IsWhiteSpace(Temp) then
    begin
      Token := Token + Temp;
    end
    else
    begin
      if Length(Token) > 0 then
        Result.Add(Token);
      Token := EmptyStr;
    end;
  end;
  if Length(Token) > 0 then
    Result.Add(Token);
end;

function Mince(PathToMince: String; MaxNumChars: Integer): String;
 var
   TotalLength, FLength: Integer;
 begin
   TotalLength := Length(PathToMince) ;
   if TotalLength > MaxNumChars then
   begin
    FLength := (MaxNumChars Div 2) - 1;
    Result := Copy(PathToMince, 0, fLength)
              + '..'
              + Copy(PathToMince,
                    TotalLength-fLength,
                    TotalLength) ;
   end
   else
     Result := PathToMince;
 end;

{$IFDEF FPC}
function DumpExceptionCallStack(E: Exception): TStringList;
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Result := TStringList.Create;
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  Result.Text := Report;
end;
{$ENDIF}

function IsWhiteSpace(TheChar: String): Boolean;
begin
  Result := (TheChar = ' ') or
            (TheChar = #9) or
            (TheChar = #13#10) or
            (TheChar = #10) or
            (TheChar = #13) {$IFDEF FPC}or
            (TheChar = LineEnding){$ENDIF};
end;

procedure FreeMemAndNil(var ptr; size: Integer = -1);
var
  p: Pointer;
begin
  p := Pointer(ptr);
  if p <> nil then
  begin
    if size > -1 then
      FreeMem(p, size)
    else
      FreeMem(p);
    Pointer(ptr) := nil;
  end;
end;

procedure TrimTaxaName(var S: AnsiString);
var
  I, K, L: Longint;
begin
  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  for I:= 1 to Length(S) do
    if not (S[I] in ValidOtuNameStartSet - ['_']) then
      S[I] := ' '
    else
      break;
  S := Trim(S);

  for I:= 2 to Length(S) do
    if not (S[I] in ValidOtuNameContinueSet -['_']) then
      S[I] := ' ';
    // change everthing in a blank

  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  // now L <> 0 then
  I := 1;
  while (I < L) do
  begin
    if ((S[I] <= ' ') and (S[I+1] <= ' ')) then
    begin
      for K := I+1 to L-1 do  // copies rest as it is
        S[K] := S[K+1];
      S[L] := #0;  // this helps us to trim right later
      Dec(L);
    end
    else
      Inc(I);
  end;

  if Length(S) > MMaxTaxaNameLen then
    S:= Copy(S,1,MMaxTaxaNameLen);
  S := TrimRight(S);
end;

procedure TrimTaxaName2(var S: String);
var
  I, K, L: Longint;
begin
  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  for I:= 1 to Length(S) do
    if not (S[I] in ValidOtuNameStartSet - ['_']) then
      S[I] := ' '
    else
      break;
  S := Trim(S);

  for I:= 2 to Length(S) do
    if not (S[I] in ValidOtuNameContinueSet -['_']) then
      S[I] := ' ';
    // change everthing in a blank

  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  // now L <> 0 then
  I := 1;
  while (I < L) do
  begin
    if ((S[I] <= ' ') and (S[I+1] <= ' ')) then
    begin
      for K := I+1 to L-1 do  // copies rest as it is
        S[K] := S[K+1];
      S[L] := #0;  // this helps us to trim right later
      Dec(L);
    end
    else
      Inc(I);
  end;

  if Length(S) > MMaxTaxaNameLen then
    S:= Copy(S,1,MMaxTaxaNameLen);
  S := TrimRight(S);
end;

function NextAvailableFilename(aName: String; NewExtension: String): String;
var
  TempDataFileName: String;
  TempFileExtension: String;
  NewFileIndex: Integer;
  NeedNewFileName: Boolean;
  FilenameWithNewExt: String;
  CurrentDataFile: String;
  ResultsDirectory: String;
begin
//  CurrentDataFile := aName;
//  ResultsDirectory := GetPathToDefaultResultsDirectory; //Get the default results directory and save it as results directory modified
//
//  if aName <> EmptyStr then  //If we have a user specified file name attempt to use it
//  begin
//    if (aName[length(aName)] = PathDelim) or DirectoryExists(aName)  then
//    begin    //The user specified a directory to save to
//      if  aName[length(aName)] <> PathDelim then
//        aName := a + PathDelim;
//      if DirectoryExistsCreate(aName) then
//        ResultsDirectory := aName;
//    end
//    else
//    begin   //The user specified a file to save to
//      if (ExtractFileExt(aName) = EmptyStr) then   //If the user didn't specify an extension, add the proper extension now
//        Result := aName + NewExtension
//      else
//        Result := ChangeFileExt(aName, NewExtension); // otherwise, force the correct file extension
//      if FileExistsUTF8(Result) { *Converted from FileExists*  } then
//      begin
//        NewFileIndex := 2;
//        NeedNewFileName := True;
//        while NeedNewFileName do
//        begin
//          TempFileExtension := ExtractFileExt(Result);
//          TempDataFileName := ChangeFileExt(Result, '');
//          TempDataFileName := TempDataFileName + '(' + IntToStr(NewFileIndex) + ')' + TempFileExtension;
//          if FileExistsUTF8(TempDataFileName) { *Converted from FileExists*  } then
//            inc(NewFileIndex)
//          else
//            NeedNewFileName := False;
//        end;
//        Result := TempDataFileName;
//        exit;
//      end
//      else   //the directory the file is in might not be there check to see if it exists and make it if necessary
//      begin
//        if DirectoryExistsCreate(ExtractFileDir(D_MegaMain.OutputFileName)) then
//        begin
//          Result := ChangeFileExt(D_MegaMain.OutputFileName, NewExtension); // GS - we must make sure that the correct file name is used, the user may specify it incorrectly
////          Result := D_MegaMain.OutputFileName;
//          if RunFromWebTOP then
//            SendToWebtop('extension: ' + ExtractFileExt(result));
//          exit;
//        end;
//      end;
//    end;
//  end;
//  NewExtension := '-' + IntToStr(GetCurrentProcessId) + NewExtension;
//  FilenameWithNewExt := ExtractFileName(ChangeFileExt(CurrentDataFile, NewExtension));  //i.e. determine filename for new file i.e. activefile.xls where NewExtension is .xls
//  if not DirectoryExistsUTF8(ResultsDirectory) { *Converted from DirectoryExists*  } then                                         //If the results directory doesn't exist, create the appropreate results directory
//    MkDir(ResultsDirectory);
//  result := NextAvailableFilename(ResultsDirectory + FilenameWithNewExt);               //See if the full path of the new file is available, and increment a number on the end until it is
end;

function SplitOnSingleChar(TheString: String; Delimiter: Char; var Tokens: TStringList; SplitOnWhiteSpace: Boolean): Boolean;
var
  CurrentPosition: Integer;
  TempString: String;
  CleanString: String;
  i: Integer;
begin
  Tokens.Clear;
  Result := True;
  if SplitOnWhiteSpace then
    CleanString := Trim(TheString)
  else
    CleanString := TheString;

  try
    i := 1;
    CurrentPosition := 1;
    while i < Length(CleanString) do
    begin
      if (CleanString[i] = Delimiter) or (SplitOnWhiteSpace and IsWhiteSpace(CleanString[i])) then
      begin
        TempString := Trim(Copy(CleanString, CurrentPosition, i - CurrentPosition));
        Tokens.Add(TempString);
        while (i < Length(CleanString)) and ((CleanString[i] = Delimiter) or (SplitOnWhiteSpace and IsWhiteSpace(CleanString[i]))) do
          inc(i);
        CurrentPosition := i;
      end
      else
        inc(i);
    end;
    TempString := Trim(Copy(CleanString, CurrentPosition, Length(CleanString)));
    if (TempString <> EmptyStr) or (not SplitOnWhiteSpace) then
      Tokens.Add(TempString);
  Except
    on E:Exception do
    begin
      Result := False;
    end;
  end;
end;



end.

