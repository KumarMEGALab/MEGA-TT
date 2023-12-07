program megattproj;

{$mode objfpc}{$H+}

uses
  {$DEFINE UseCThreads}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, LazUTF8,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, gtreedata, gtreelist, gtreedataadapter,
  MLongintList, gtreenode, ttconst, gutils, gtreeproc
  { you can add units after this }
  , gsvgwriter, gsvgtreebox, gtreeattrib, gtextwidth, geologicaltimes,
  mearthimpacts, mgeodata, gtaxonomicrank, gnamesmap, gpairwisesvg, syncobjs,
  gloadtreethread, dateutils, mgeodatachartformatter, testconsts, gsvgstrings,
  gtimeline, gtimelinesvgwriter, gtimelineresult, gtimelinejsonparser,
  gtimelineresultbin, gtimelineresultbinarray, mearthimpactsrenderer,
  gtimescalerenderer, gscaleticks, typinfo, gconfidenceintervals,
  gnamestoidsmapper, gleafnodecounts, gnewick_to_svg, mtreepruner;

type

  TActionType = (tatPruneTree, tatPairwise, tatCountRanks, tatTimeline, tatGenerateNameToIdsMap,
                 tatCountLeafNodes, tatRenderNewick, tatRenderNewickOnly, tatNone);
  { megatt }

  megatt = class(TCustomApplication)
  protected
    FNewickFile: String;
    FNewickString: String;
    FMapFileNames: TStringList;
    FTreeList: TTimeTreeList;
    LoadNamesThread: TLoadNamesMapThread;
    LoadTreeThread: TLoadNewickTreeThread;
    LoadRanksThread: TLoadRanksThread;
    LoadConfidenceIntervalsThread: TLoadConfidenceIntervalsThread;
    LoadLeafNodeCountsThread: TLeafNodeCountsLoaderThread;
    NamesAreLoaded: Boolean;
    ConfidenceIntervalsAreLoaded: Boolean;
    LeafCountsAreLoaded: Boolean;
    TreeIsLoaded: Boolean;
    RanksAreLoaded: Boolean;
    GeoBgColorsSet: Boolean;
    procedure DoRun; override;
    procedure DoPruneTree;
    procedure DoPairwiseSvg;
    procedure DoTimelineSvg;
    procedure DoRenderNewick;
    procedure DoRenderNewickOnly; { skips the rendering of everything else, i.e. timescale, geopanels, etc...}
    procedure NewickToTabularFormat;
    function ParseCommandLine: Boolean;
    procedure CountRanks;
    procedure OutputVersionInfo;
    procedure VerifyPruneTreeInputs;
    procedure VerifyCountRanksInputs;
    procedure VerifyTimelineInputs;
    procedure VerifyPairwiseInputs;
    procedure VerifyMapFileaNamesInputs;
    procedure VerifyCountLeavesInputs;
    procedure VerifyRenderNewickInputs;
    procedure DoGenerateNamesToIdsMap;
    procedure DoGenerateLeafNodeCounts;
    procedure DoConvertFile;

    procedure LaunchMapLoader;
    procedure LaunchTreeLoader;
    procedure LaunchRanksLoader;
    procedure LaunchConfidenceIntervalsLoader;
    procedure LaunchLeafNodeCountsLoader;
    procedure MapLoaded(aThread: TObject);
    procedure TreeLoaded(aThread: TObject);
    procedure RanksLoaded(aThread: TObject);
    procedure LeafCountsLoaded(aThread: TObject);
    procedure ConfidenceIntervalsLoaded(aThread: TObject);

    function EverythingIsLoaded: Boolean;
  public
    IsMobileFriendly: Boolean;
    DrawUnnamedTimelineNodes: Boolean;
    LogFile: String;
    LogStrings: TStringList;
    IsCountRanks: Boolean;
    ConvertFile: String;
    PairwiseJsonData: String;
    TimelineJsonData: String;
    ActionType: TActionType;
    VSpacing: Integer;
    PanelHeight: Integer;

    DoNewick: Boolean;
    DoLogScale: Boolean;
    Width: Integer;
    Height: Integer;
    Rank: String;
    DoPruneTreeSlow: Boolean;
    TreeFileName: AnsiString;
    NamesMapFile: AnsiString;
    NodeIdsFile: AnsiString;
    EarthImpactsFile: AnsiString;
    RanksFile: AnsiString;
    O2File: AnsiString;
    CO2File: AnsiString;
    ConfidenceIntervalsFile: AnsiString;
    LeafNodeCountsFile: String;
    OutputFileName: AnsiString;
    ExportPairwiseDistances: Boolean;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ megatt }

procedure megatt.DoRun;
begin
  Width := 600;
  Height := 800;
  LogStrings := nil;
  NamesAreLoaded:=False;
  TreeIsLoaded:=False;
  RanksAreLoaded:=False;
  LeafCountsAreLoaded:=False;
  GeoScaleForBgColors := gbcAuto;
  GeoBgColorsSet := False;
  TreeName := EmptyStr;
  FMapFileNames := nil;

  try
    if HasOption('h','help') then begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    { add your program here }
    ActionType := tatNone;
    if not ParseCommandLine then
    begin
      WriteLn('Failed to parse command line');
      Halt(9);
    end;
    case ActionType of
    tatPruneTree:
      begin
        VerifyPruneTreeInputs;
        DoPruneTree;
      end;
    tatPairwise:
      begin
        VerifyPairwiseInputs;
        DoPairwiseSvg;
      end;
    tatCountRanks:
      begin
        VerifyCountRanksInputs;
        CountRanks;
      end;
    tatTimeline:
      begin
        VerifyTimelineInputs;
        DoTimelineSvg;
      end;
    tatGenerateNameToIdsMap:
      begin
        VerifyMapFileaNamesInputs;
        DoGenerateNamesToIdsMap;
      end;
    tatCountLeafNodes:
      begin
        VerifyCountLeavesInputs;
        DoGenerateLeafNodeCounts;
      end;
    tatRenderNewick:
      begin
        VerifyRenderNewickInputs;
        DoRenderNewick;
      end;
    tatRenderNewickOnly:
      begin
        DoRenderNewickOnly;
      end;
    tatNone:
      begin
        WriteLn('invalid command line params: nothing to do');
        Halt(101);
      end;
    end;


  except
    on E:Exception do
    begin
      WriteLn('Dammit Jim! He' + #34 + 's dead!: ' + E.Message);
      Halt(1);
    end;
  end;

  // stop program loop
  Terminate;
end;

procedure megatt.DoPruneTree;
var
  aPruner: TTreePruner = nil;
  startTime, endTime: TDateTime;
  temp: String;
begin
  try
    try
      startTime := Now;
      if LogFile <> EmptyStr then
      begin
        LogfileCriticalSect := TCriticalSection.Create;
        LogStrings := TStringList.Create;
      end;
      LaunchTreeLoader;
      LaunchMapLoader;
      LaunchRanksLoader;
      LaunchConfidenceIntervalsLoader;
      LaunchLeafNodeCountsLoader;

      LoadRanksThread.WaitFor;
      LoadTreeThread.WaitFor;

      aPruner := TTreePruner.Create;
      if DoLogScale then
        aPruner.DoLogScale := True;
      aPruner.IsMobileFriendly := IsMobileFriendly;
      aPruner.Width := Width;
      aPruner.VSpacing := VSpacing;
      aPruner.DoNewick := DoNewick;
      aPruner.PanelHeight := PanelHeight;
      aPruner.RanksFile := RanksFile;
      if Assigned(LogStrings) then
        aPruner.LogStrings := LogStrings;
      //if Rank <> EmptyStr then
      //begin
      //  aPruner.DeepestRank := StringToTaxonomicRank(Rank);
      //  if not IsValidRankForSearch(aPruner.DeepestRank) then
      //    raise Exception.Create('invalid taxonomic rank for tree search');
      //end;
      if not aPruner.PruneTree(FTreeList, NodeIdsFile, DoPruneTreeSlow) then
        raise Exception.Create('Prune tree command failed');
      LoadNamesThread.WaitFor;
      LoadConfidenceIntervalsThread.WaitFor;
      LoadLeafNodeCountsThread.WaitFor;

      if aPruner.DoMapNames then
        if not aPruner.MapOtuNames then
          raise Exception.Create('failed to map otu names');

      if not aPruner.WritePrunedTreeToSvg(OutputFilename, EarthImpactsFile, O2File, CO2File) then
        raise Exception.Create('Failed to create SVG file for pruned tree');
      if ExportPairwiseDistances and (not aPruner.WritePairwiseDistancesToHtml(OutputFilename)) then
        raise Exception.Create('Failed to export pairwise distances');
      endTime := Now;
      if Assigned(LogStrings) then
      begin
        temp := 'total execution time: ' + IntToStr(MilliSecondsBetween(endTime, StartTime)) + ' ms';
        LogStrings.Add(temp);
        LogStrings.SaveToFile(LogFile);
      end;
    except
      on E:Exception do
      begin
        WriteLn('FATAL ERROR in DoPruneTree: ' + E.Message);
        ShowException(E);
      end;
    end;
  finally
    if Assigned(aPruner) then
      aPruner.Free;
    if LogFile <> EmptyStr then
      LogfileCriticalSect.Free;
    if Assigned(LogStrings) then
      LogStrings.Free;
  end;
end;

procedure megatt.DoPairwiseSvg;
var
  SvgWriter: TPairwiseSvgWriter;
  aList: TStringList;
begin
  SvgWriter := TPairwiseSvgWriter.Create(Width, Height);
  if SvgWriter.LoadPairwiseData(PairwiseJsonData) then
  begin
    SvgWriter.ImpactsFile := EarthImpactsFile;
    SvgWriter.CO2File := CO2File;
    SvgWriter.O2File := O2File;
    aList := SvgWriter.GenerateSvg;
    aList.SaveToFile(OutputFileName);
  end
  else
  begin
    WriteLn('Failed to parse Json data');
    Halt(1);
  end;
end;

procedure megatt.DoTimelineSvg;
var
  Parser: TTimelineJsonParser=nil;
  aWriter: TTimelineSvgWriter=nil;
  aList: TStringList=nil;
  Timeline: TTimeline=nil;
  i: Integer;
  Svg: String;
  Dimensions: TPoint;
begin
  Dimensions.X := Width;
  Dimensions.Y := DEFAULT_TIMELINE_HEIGHT;
  try
    try
      aList := TStringList.Create;
      aList.LoadFromFile(TimelineJsonData);
      Parser := TTimelineJsonParser.Create;
      if not Parser.Parse(aList.Text) then
        raise Exception.Create('Failed to parse json file');
      Timeline := TTimeline.Create;
      if Parser.Count > 0 then
      begin
        for i := 0 to Parser.Count - 1 do
          Timeline.Add(Parser[i]);
        Timeline.Sort;
        Timeline.CheckUpdateAmbiguousNames;
        aWriter := TTimelineSvgWriter.Create;
        aWriter.DrawUnknownNodes := DrawUnnamedTimelineNodes;
        aWriter.ImpactsFile := EarthImpactsFile;
        aWriter.CO2File := CO2File;
        aWriter.O2File := O2File;
        Svg := aWriter.GenerateSvg(Timeline, Dimensions);
        if Svg = EmptyStr then
          raise Exception.Create('Generation of the timeline svg failed');
        aList.Text := Svg;
        aList.SaveToFile(OutputFileName);
        if not FileExists(OutputFileName) then
          raise Exception.Create('Timeline SVG was not saved correctly. Is there a permissions issue?');
      end
      else
        raise Exception.Create('No valid timeline data found in the json file');
    except
      on E:Exception do
      begin
        WriteLn('Error doing timeline results to svg: ' + E.Message);
        Halt(3);
      end;
    end;
  finally
    if Assigned(aList) then
      aList.Free;
    if Assigned(Parser) then
      Parser.Free;
    if Assigned(aWriter) then
      aWriter.Free;
    if Assigned(Timeline) then
      Timeline.Free;
  end;
end;

procedure megatt.DoRenderNewick;
var
  aPruner: TTreePruner = nil;
  startTime, endTime: TDateTime;
  temp: String;
begin
  try
    try
      startTime := Now;
      if LogFile <> EmptyStr then
      begin
        LogfileCriticalSect := TCriticalSection.Create;
        LogStrings := TStringList.Create;
      end;
      LaunchTreeLoader;
      LaunchMapLoader;
      LaunchRanksLoader;
      LoadRanksThread.WaitFor;
      LoadTreeThread.WaitFor;
      aPruner := TTreePruner.Create;
      aPruner.IsMobileFriendly := IsMobileFriendly;
      aPruner.Width := Width;
      aPruner.VSpacing := VSpacing;
      aPruner.DoNewick := DoNewick;
      aPruner.PanelHeight := PanelHeight;
      aPruner.RanksFile := RanksFile;
      if Assigned(LogStrings) then
        aPruner.LogStrings := LogStrings;
      LoadNamesThread.WaitFor;
      if not aPruner.WriteUserTreeToSvg(FTreeList, OutputFilename, EarthImpactsFile, O2File, CO2File) then
        raise Exception.Create('Failed to create SVG file for pruned tree');
      endTime := Now;
      if Assigned(LogStrings) then
      begin
        temp := 'total execution time: ' + IntToStr(MilliSecondsBetween(endTime, StartTime)) + ' ms';
        LogStrings.Add(temp);
        LogStrings.SaveToFile(LogFile);
      end;
    except
      on E:Exception do
      begin
        WriteLn('FATAL ERROR in DoPruneTree: ' + E.Message);
        ShowException(E);
      end;
    end;
  finally
    if Assigned(aPruner) then
      aPruner.Free;
    if LogFile <> EmptyStr then
      LogfileCriticalSect.Free;
    if Assigned(LogStrings) then
      LogStrings.Free;
  end;
end;

procedure megatt.DoRenderNewickOnly;
var
  startTime, endTime: TDateTime;
  temp: String;
  newickToSvg: TNewickToSvg = nil;
begin
  try
    try
      startTime := Now;

      if LogFile <> EmptyStr then
      begin
        LogfileCriticalSect := TCriticalSection.Create;
        LogStrings := TStringList.Create;
      end;
      if Trim(FNewickString) <> EmptyStr then
        newickToSvg := TNewickToSvg.Create(FNewickString)
      else if FileExists(FNewickFile) then
        newickToSvg := TNewickToSvg.CreateFromFile(FNewickFile)
      else
        raise Exception.Create('no valid inputs for SVG generation were found');
      if not newickToSvg.DrawSvg(OutputFileName) then
        raise Exception.Create(newickToSvg.Log.Text);
      if not newickToSvg.WriteTreeInTabularFormat(ChangeFileExt(OutputFileName, '.table')) then
        raise Exception.Create(newickToSvg.Log.Text);
      endTime := Now;
      if Assigned(LogStrings) then
      begin
        temp := 'total execution time: ' + IntToStr(MilliSecondsBetween(endTime, StartTime)) + ' ms';
        LogStrings.Add(temp);
        LogStrings.SaveToFile(LogFile);
      end;
    except
      on E:Exception do
      begin
        WriteLn('FATAL ERROR in DoRenderNewickOnly: ' + E.Message);
        ShowException(E);
      end;
    end;
  finally
    if LogFile <> EmptyStr then
      LogfileCriticalSect.Free;
    if Assigned(LogStrings) then
      LogStrings.Free;
  end;
end;

procedure megatt.NewickToTabularFormat;
begin

end;

function megatt.ParseCommandLine: Boolean;
var
  TempStr: AnsiString = '';
  i: Integer;
  Index: Integer;
  tempBool: Boolean = False;
begin
  DrawUnnamedTimelineNodes := False;
  LogFile := EmptyStr;
  IsCountRanks := False;
  PairwiseJsonData := EmptyStr;
  VSpacing := 2;
  PanelHeight := 100;
  DoNewick := False;
  DoLogScale := False;
  Width := 960;
  Height := 480;
  Result := false;
  TreeFileName := EmptyStr;
  RanksFile := EmptyStr;
  Rank := EmptyStr;
  for i := 0 to ParamCount do
  begin
    TempStr := AnsiLowerCase(ParamStr(i));
    if (TempStr = '-t') or (TempStr = '--tree') then
      TreeFileName := ParamStr(i+1)
    else if (TempStr = '-names-map') or (TempStr = '--names-map') then
      NamesMapFile := Trim(ParamStr(i+1))
    else if (TempStr = '-ids') or (TempStr = '--ids') then
    begin
      NodeIdsFile := ParamStr(i+1);
    end
    else if (TempStr = '-cf') or (TempStr = '--convert-file') then
    begin
      ConvertFile := ParamStr(i+1);
      DoConvertFile;
      Halt(0);
    end
    else if (TempStr = '-vspacing') or (TempStr = '--vspacing') then
    begin
      VSpacing := StrToInt(ParamStr(i+1));
      if not ((VSpacing >= 1) and (VSpacing <= 3)) then
        raise Exception.Create('invalid value for vertical spacing');
    end
    else if (TempStr = '-panel-height') or (TempStr = '--panel-height') then
    begin
      PanelHeight := StrToInt(ParamStr(i+1));
      if not ((PanelHeight >= 1) and (PanelHeight <= 3)) then
        raise Exception.Create('invalid value for panel height');
      case PanelHeight of
      1: PanelHeight := 50; { small}
      2: PanelHeight := 75; { medium}
      3: PanelHeight := 100; { large}
      end;
    end
    else if (TempStr = '-geo-bg-colors') or (TempStr = '--geo-bg-colors') then
    begin
      GeoScaleForBgColors := StringToGeoBgColors(ParamStr(i+1));
      GeoBgColorsSet := True;
    end
    else if (TempStr = '-newick') or (TempStr = '--newick') then
    begin
      DoNewick := True;
    end
    else if (TempStr = '-o') or (TempStr = '--outfile') then
    begin
      OutputFileName := ExpandFileName(ParamStr(i+1));
      if ExtractFilePath(OutputFileName) = EmptyStr then  //If they specified an absolute path
        OutputFileName := ExtractFilePath(ExeName) + OutputFileName;
    end
    else if (ParamStr(i) = '-logfile') or (ParamStr(i) = '--logfile') then
    begin
      LogFile := ParamStr(i+1);
    end
    else if (ParamStr(i) = '-tree-name') or (ParamStr(i) = '--tree-name') then
    begin
      TreeName := ParamStr(i+1);
    end
    else if (ParamStr(i) = '-h') OR (ParamStr(i) = '--help') OR (ParamStr(i) = '-help') then
    begin
      WriteHelp;
      exit;
    end
    else if (ParamStr(i) = '-earth-impacts') or (ParamStr(i) = '--earth-impacts') then
    begin
      EarthImpactsFile := ParamStr(i + 1);
    end
    else if (ParamStr(i) = '-o2') or (ParamStr(i) = '--o2') then
    begin
      O2File := ParamStr(i + 1);
    end
    else if (ParamStr(i) = '-co2') or (ParamStr(i) = '--co2') then
    begin
      CO2File := ParamStr(i + 1);
    end
    else if (ParamStr(i) = '-leaf-counts') or (ParamStr(i) = '--leaf-counts') then
    begin
      LeafNodeCountsFile := ParamStr(i + 1);
    end
    else if (ParamStr(i) = '-ci') or (ParamStr(i) = '--confidence-intervals') then
      ConfidenceIntervalsFile := ParamStr(i + 1)
    else if (ParamStr(i) = '-log-scale') or (ParamStr(i) = '--log-scale') then
    begin
      DoLogScale := True;
    end
    else if (ParamStr(i) = '-p') or (ParamStr(i) = '--prune') then
    begin
      ActionType := tatPruneTree;
      DoPruneTreeSlow := False;
    end
    else if (ParamStr(i) = '-r') or (ParamStr(i) = '--render') then
    begin
      ActionType := tatRenderNewick;
    end
    else if (ParamStr(i) = '-i') or (ParamStr(i) = '--input') then
    begin
      FNewickString := ParamStr(i + 1);
      ActionType := tatRenderNewickOnly;
    end
    else if (ParamStr(i) = '-if') or (ParamStr(i) = '--input-file') then
    begin
      FNewickFile := ParamStr(i + 1);
      ActionType := tatRenderNewickOnly;
    end
    else if (ParamStr(i) = '-ps') or (ParamStr(i) = '--prune-slow') then
    begin
      ActionType := tatPruneTree;
      DoPruneTreeSlow := True;
    end
    else if (ParamStr(i) = '-pairwise') or (ParamStr(i) = '--pairwise') then
    begin
      ActionType := tatPairwise;
      PairwiseJsonData := ParamStr(i+1);
    end
    else if (ParamStr(i) = '-map-names') or (ParamStr(i) = '--map-names') then
    begin
      ActionType := tatGenerateNameToIdsMap;
      Index := i + 1;
      FMapFileNames := TStringList.Create;
      while Index <= ParamCount do
      begin
        TempStr := ParamStr(Index);
        FMapFileNames.Add(ParamStr(Index));
        inc(Index);
      end;
    end
    else if (ParamStr(i) = '-count-leaves') or (ParamStr(i) = '--count-leaves') then
    begin
      ActionType := tatCountLeafNodes;
    end
    else if (ParamStr(i) = '-names-map') or (ParamStr(i) = '--names-map') then
    begin
      NamesMapFile := ParamStr(i+1);
    end
    else if (ParamStr(i) = '-ranks') or (ParamStr(i) = '--ranks') then
    begin
      RanksFile := ParamStr(i+1);
    end
    else if (ParamStr(i) = '-timeline') or (ParamStr(i) = '--timeline') then
    begin
      ActionType := tatTimeline;
      TimelineJsonData := ParamStr(i+1);
    end
    else if (ParamStr(i) = '-draw-unnamed') or (ParamStr(i) = '--draw-unnamed') then
    begin
      DrawUnnamedTimelineNodes := True;
    end
    else if (ParamStr(i) = '-target-rank') or (ParamStr(i) = '--target-rank') then
    begin
      Rank := ParamStr(i+1);
    end
    else if (ParamStr(i) = '-count-ranks') or (ParamStr(i) = '--count-ranks') then
    begin
      ActionType := tatCountRanks;
    end
    else if (ParamStr(i) = '-width') or (ParamStr(i) = '--width') then
    begin
      Width := StrToInt(ParamStr(i+1));
    end
    else if (ParamStr(i) = '-height') or (ParamStr(i) = '--height') then
    begin
      Height := StrToInt(ParamStr(i+1));
    end
    else if (ParamStr(i) = '-v') or (ParamStr(i) = '--version') then
    begin
     OutputVersionInfo;
     exit;
    end
    else if (ParamStr(i) = '-mf') or (ParamStr(i) = '--mobile-friendly') then
    begin
      TempStr := LowerCase(Trim(ParamStr(i + 1)));
      if SameText(TempStr, 'true') then
        IsMobileFriendly := True
      else if SameText(TempStr, 'false') then
        IsMobileFriendly := False
      else
      begin
        WriteLn(Format('invalid value given for %s parameter. Got %s but expected either "true" or "false"', [ParamStr(i), ParamStr(i + 1)]));
      end;
    end
    else if (ParamStr(i) = '-st') or (ParamStr(i) = '--studytree') then
      IsStudyTree := True
    else if (ParamStr(i) = '-xpw') or (ParamStr(i) = '--export-pairwise') then
      ExportPairwiseDistances := True;
  end;
  Result := true;
end;

procedure megatt.CountRanks;
var
  aPruner: TTreePruner = nil;
begin
  try
    try
      LaunchMapLoader;
      LaunchTreeLoader;
      LaunchRanksLoader;
      LoadRanksThread.WaitFor;
      LoadTreeThread.WaitFor;
      LoadNamesThread.WaitFor;
      aPruner := TTreePruner.Create;
      aPruner.RanksFile := RanksFile;
      if not aPruner.CountRanks(FTreeList, OutputFileName) then
        raise Exception.Create('Count ranks command failed');
    except
      on E:Exception do
      begin
        WriteLn('FATAL ERROR in CountRanks: ' + E.Message);
        ShowException(E);
      end;
    end;
  finally
    if Assigned(aPruner) then
      aPruner.Free;
  end;
end;

procedure megatt.OutputVersionInfo;
begin
  WriteLn('MEGA-TT (MEGA TimeTree )');
  WriteLn('Version 2.0.7');
  WriteLn('copyright 2023');
  WriteLn('Authors: Glen Stecher <gstecher@bd3software.com>');
end;

procedure megatt.VerifyPruneTreeInputs;
begin
  if not FileExists(Trim(TreeFileName)) then
  begin
   WriteLn('The necessary newick file was not provided or does not exist');
   Halt(1);
  end;
  if not FileExists(Trim(NodeIdsFile)) then
  begin
   WriteLn('The necessary node IDs file was not provided or does not exist');
   Halt(2);
  end;
  if not FileExists(Trim(NamesMapFile)) then
  begin
    WriteLn('The necessary IDs to names map file was not provided or does not exist');
    Halt(3);
  end;
  if not FileExists(Trim(EarthImpactsFile)) then
  begin
    WriteLn('The required earth impacts file was not provided or does not exist');
    Halt(4);
  end;
  if not FileExists(Trim(O2File)) then
  begin
    WriteLn('The required O2 file was not provided or does not exist');
    Halt(5);
  end;
  if not FileExists(Trim(CO2File)) then
  begin
    WriteLn('The required CO2 file was not provided or does not exist');
    Halt(6);
  end;
  if not FileExists(RanksFile) then
  begin
    WriteLn('The required ranks file was not found');
    Halt(8);
  end;
  if not GeoBgColorsSet then
  begin
    WriteLn('The required geo-bg-colors parameter was not given');
    Halt(9);
  end;
  if TreeName = EmptyStr then
  begin
    WriteLn('The required tree-name parameter was not given');
    Halt(10);
  end;
  if not FileExists(ConfidenceIntervalsFile) then
  begin
    WriteLn('The required confidence intervals file was not found');
    Halt(11);
  end;
  if not FileExists(LeafNodeCountsFile) then
  begin
    WriteLn('The required leaf node counts file was not found');
    Halt(12);
  end;
end;

procedure megatt.VerifyCountRanksInputs;
begin
  if not FileExists(Trim(TreeFileName)) then
  begin
   WriteLn('The necessary newick file was not provided or does not exist');
   Halt(1);
  end;
  if not FileExists(RanksFile) then
  begin
    WriteLn('The required ranks file was not found');
    Halt(2);
  end;
  if OutputFileName = EmptyStr then
  begin
    WriteLn('The required output file name is missing');
    Halt(3);
  end;
  if not FileExists(Trim(NamesMapFile)) then
  begin
    WriteLn('The necessary IDs to names map file was not provided or does not exist');
    Halt(4);
  end;
end;

procedure megatt.VerifyTimelineInputs;
begin
  if not FileExists(TimelineJsonData) then
  begin
    WriteLn('The specified timeline json file was not found or could not be opened');
    Halt(2);
  end;
  if OutputFileName = EmptyStr then
  begin
    WriteLn('The required output file name is missing');
    Halt(3);
  end;
  if Width < 400 then
  begin
    WriteLn('Minimum width for the timeline svg is 400');
    Halt(4);
  end;
  if Height < 600 then
  begin
    WriteLn('Minimum height for the timeline svg is 600');
    Halt(5);
  end;
  if not FileExists(Trim(EarthImpactsFile)) then
  begin
    WriteLn('The required earth impacts file was not provided or does not exist');
    Halt(6);
  end;
  if not FileExists(Trim(O2File)) then
  begin
    WriteLn('The required O2 file was not provided or does not exist');
    Halt(7);
  end;
  if not FileExists(Trim(CO2File)) then
  begin
    WriteLn('The required CO2 file was not provided or does not exist');
    Halt(8);
  end;
end;

procedure megatt.VerifyPairwiseInputs;
begin
  if not FileExists(PairwiseJsonData) then
  begin
    WriteLn('The specified pairwise json file was not found or could not be opened');
    Halt(2);
  end;
  if OutputFileName = EmptyStr then
  begin
    WriteLn('The required output file name is missing');
    Halt(3);
  end;
  if Width < 400 then
  begin
    WriteLn('Minimum width for the pairwise svg is 400');
    Halt(4);
  end;
  if Height < 600 then
  begin
    WriteLn('Minimum height for the pairwise svg is 600');
    Halt(5);
  end;
  if not FileExists(Trim(EarthImpactsFile)) then
  begin
    WriteLn('The required earth impacts file was not provided or does not exist');
    Halt(6);
  end;
  if not FileExists(Trim(O2File)) then
  begin
    WriteLn('The required O2 file was not provided or does not exist');
    Halt(7);
  end;
  if not FileExists(Trim(CO2File)) then
  begin
    WriteLn('The required CO2 file was not provided or does not exist');
    Halt(8);
  end;
end;

procedure megatt.VerifyMapFileaNamesInputs;
var
  i: Integer;
begin
  if not FMapFileNames.Count = 6 then
  begin
    WriteLn('expected 6 map files, found: ' + IntToStr(FMapFileNames.Count));
    Halt(1);
  end;

  for i:= 0 to FMapFileNames.Count - 1 do
    if not FileExists(FMapFileNames[i]) then
    begin
      WriteLn('missing names to IDs map file: ' + FMapFileNames[i]);
      Halt(2);
    end;
end;

procedure megatt.VerifyCountLeavesInputs;
begin
  if not FileExists(RanksFile) then
  begin
    WriteLn('The required ranks file is missing');
    Halt(1);
  end;
  if not FileExists(NamesMapFile) then
  begin
    WriteLn('The required names map file is missing');
    Halt(2);
  end;
end;

procedure megatt.VerifyRenderNewickInputs;
begin
  if not FileExists(Trim(TreeFileName)) then
  begin
   WriteLn('The necessary newick file was not provided or does not exist');
   Halt(1);
  end;
  if not FileExists(Trim(NamesMapFile)) then
  begin
    WriteLn('The necessary IDs to names map file was not provided or does not exist');
    Halt(3);
  end;
  if not FileExists(Trim(EarthImpactsFile)) then
  begin
    WriteLn('The required earth impacts file was not provided or does not exist');
    Halt(4);
  end;
  if not FileExists(Trim(O2File)) then
  begin
    WriteLn('The required O2 file was not provided or does not exist');
    Halt(5);
  end;
  if not FileExists(Trim(CO2File)) then
  begin
    WriteLn('The required CO2 file was not provided or does not exist');
    Halt(6);
  end;
  if not FileExists(RanksFile) then
  begin
    WriteLn('The required ranks file was not found');
    Halt(8);
  end;
  if not GeoBgColorsSet then
  begin
    WriteLn('The required geo-bg-colors parameter was not given');
    Halt(9);
  end;
  if TreeName = EmptyStr then
  begin
    WriteLn('The required tree-name parameter was not given');
    Halt(10);
  end;
end;

procedure megatt.DoGenerateNamesToIdsMap;
var
  aMapper: TNamesToIdsMapper;
  NamesToIdsMap: TStringList;
begin
  aMapper := TNamesToIdsMapper.Create;
  aMapper.NamesMapFiles := FMapFileNames;
  NamesToIdsMap := TStringList.Create;
  if not aMapper.GenerateFullNamesToIdsMap(NamesToIdsMap) then
  begin
    WriteLn(aMapper.MessageLog.Text);
  end
  else
    NamesToIdsMap.SaveToFile('namesToIdsMap.csv');
end;

procedure megatt.DoGenerateLeafNodeCounts;
var
  aMapper: TNamesToIdsMapper;
  LeafNodeCounts: TStringList = nil;
begin
  try
    try
      aMapper := TNamesToIdsMapper.Create;
      LeafNodeCounts := TStringList.Create;
      if not aMapper.GenerateLeafNodesCount(LeafNodeCounts, NamesMapFile, RanksFile) then
        raise Exception.Create('failed to generate leaf node counts - ' + aMapper.MessageLog.Text);
      LeafNodeCounts.SaveToFile('leafNodeCounts.csv');
    except
      on E:Exception do
      begin
        WriteLn('Exception encountered in DoGenerateLeafNodeCounts: ' + E.Message);
        Halt(1);
      end;
    end;
  finally
    if Assigned(aMapper) then
      aMapper.Free;
    if Assigned(LeafNodeCounts) then
      LeafNodeCounts.Free;
  end;
end;

procedure megatt.DoConvertFile;
var
  aList: TStringList;
  ranks: TStringList;
  ids: TStringList;
  temp: TStringList;
  i: Integer;
  maxValue: LongInt;
  minValue: LongInt;
  aValue: LongInt;
  aRank: TTaxonomicRank;
begin
  maxValue := 0;
  minValue := MaxInt;
  aList := nil;
  ranks := nil;
  ids := nil;
  temp := nil;

  try
    temp := TStringList.Create;
    ranks := TStringList.Create;
    ids := TStringList.Create;
    aList := TStringList.Create;
    aList.LoadFromFile(ConvertFile);
    if aList.Count > 1 then
    begin
      for i:= 1 to aList.Count - 1 do
      begin
        if SplitOnSingleChar(aList[i], #9, temp, False) then
        begin
          //aValue := StrToInt(temp[0]);
          //if aValue > maxValue then
          //  maxValue := aValue;
          //if aValue < minValue then
          //  minValue := aValue;
          //if temp.Count = 3 then
          //try
          //  temp[2] := StringReplace(temp[2], ' ', '_', [rfReplaceAll]);
          //  aRank := StringToTaxonomicRank(temp[2]);
          //except
          //  ranks.Add(temp[2]);
          //end;

          if temp.Count < 2 then
            raise Exception.Create('invalid line in input file');
          ids.Add(temp[0] + '=' + temp[1]);
          if temp.Count = 3 then
          begin
            temp[2] := StringReplace(temp[2], ' ', '_', [rfReplaceAll]);
            ranks.Add(temp[0] + '=' + temp[2])
          end
          else
            ranks.Add(temp[0] + '=');
        end
        else
          raise Exception.Create('failed to split string');
      end;
      //ranks.Add('minval=' + IntToStr(minValue));
      //ranks.Add('maxval=' + IntToStr(maxVAlue));
      ranks.SaveToFile(ChangeFileExt(ConvertFile, '_ranks.txt'));
      //ids.SaveToFile(ChangeFileExt(ConvertFile, '_ids_map.txt'));
    end;
  finally
    if Assigned(aList) then
      aList.Free;
    if Assigned(ranks) then
      ranks.Free;
    if Assigned(ids) then
      ids.Free;
    if Assigned(temp) then
      temp.Free;
  end;
end;

procedure megatt.LaunchMapLoader;
begin
  NamesMapCriticalSect := TCriticalSection.Create;
  LoadNamesThread := nil;
  if not FileExists(NamesMapFile) then
    raise Exception.Create('missing names map file: ' + NamesMapFile);
  LoadNamesThread := TLoadNamesMapThread.Create(True);
  LoadNamesThread.MapFile:=NamesMapFile;
  LoadNamesThread.OnTerminate:=@MapLoaded;
  if Assigned(LogStrings) then
    LoadNamesThread.LogStrings := LogStrings;
  LoadNamesThread.Start;
end;

procedure megatt.LaunchTreeLoader;
begin
  LoadTreeThread := nil;
  LoadTreeCriticalSect := TCriticalSection.Create;
  if not FileExists(TreeFileName) then
    raise Exception.Create('missing newick tree file');
  LoadTreeThread := TLoadNewickTreeThread.Create;
  LoadTreeThread.Filename := TreeFileName;
  LoadTreeThread.OnTerminate := @TreeLoaded;
  if Assigned(LogStrings) then
    LoadTreeThread.LogStrings := LogStrings;
  LoadTreeThread.Start;
end;

procedure megatt.LaunchRanksLoader;
begin
  LoadRanksCriticalSect := TCriticalSection.Create;
  LoadRanksThread := TLoadRanksThread.Create;
  LoadRanksThread.RanksFile := RanksFile;
  LoadRanksThread.OnTerminate := @RanksLoaded;
  if Assigned(LogStrings) then
    LoadRanksThread.LogStrings := LogStrings;
  LoadRanksThread.Start;
end;

procedure megatt.LaunchConfidenceIntervalsLoader;
begin
  LoadConfidenceIntervalsCriticalSect := TCriticalSection.Create;
  LoadConfidenceIntervalsThread := TLoadConfidenceIntervalsThread.Create(True);
  LoadConfidenceIntervalsThread.ConfidenceIntervalsFile := ConfidenceIntervalsFile;
  LoadConfidenceIntervalsThread.OnTerminate := @ConfidenceIntervalsLoaded;
  if Assigned(LogStrings) then
    LoadConfidenceIntervalsThread.LogStrings := LogStrings;
  LoadConfidenceIntervalsThread.Start;
end;

procedure megatt.LaunchLeafNodeCountsLoader;
begin
  LoadLeafNodeCountsCriticalSect := TCriticalSection.Create;
  LoadLeafNodeCountsThread := TLeafNodeCountsLoaderThread.Create(True);
  LoadLeafNodeCountsThread.TargetRank := StringToTaxonomicRank(Rank);
  LoadLeafNodeCountsThread.LeafNodeCountsFile := LeafNodeCountsFile;
  LoadLeafNodeCountsThread.OnTerminate := @LeafCountsLoaded;
  LoadLeafNodeCountsThread.Start;
end;

procedure megatt.MapLoaded(aThread: TObject);
begin
  with (aThread as TLoadNamesMapThread) do
    if not IsSuccess then
    begin
      WriteLn('failed to load names map');
      Halt(1);
    end;

  try
    NamesMapCriticalSect.Acquire;
    NamesAreLoaded:=True;
  finally
    NamesMapCriticalSect.Release;
  end;
end;

procedure megatt.TreeLoaded(aThread: TObject);
begin
  if not LoadTreeThread.IsSuccess then
  begin
    WriteLn('failed to tree file');
    Halt(1);
  end;
  FTreeList := LoadTreeThread.TreeList;


  try
    LoadTreeCriticalSect.Acquire;
    TreeIsLoaded:=True;
  finally
    LoadTreeCriticalSect.Release;
  end;
end;

procedure megatt.RanksLoaded(aThread: TObject);
begin
  with (aThread as TLoadRanksThread) do
    if not IsSuccess then
    begin
      WriteLn('failed to load taxonomic ranks');
      Halt(2);
    end;

  try
    LoadRanksCriticalSect.Acquire;
    RanksAreLoaded := True;
  finally
    LoadRanksCriticalSect.Release;
  end;
end;

procedure megatt.LeafCountsLoaded(aThread: TObject);
begin
  with (aThread as TLeafNodeCountsLoaderThread) do
    if not IsSuccess then
    begin
      WriteLn('failed to load leaf node counts');
      Halt(2);
    end;

  try
    LoadLeafNodeCountsCriticalSect.Acquire;
    LeafCountsAreLoaded := True;
  finally
    LoadLeafNodeCountsCriticalSect.Release;
  end;
end;

procedure megatt.ConfidenceIntervalsLoaded(aThread: TObject);
begin
  with (aThread as TLoadConfidenceIntervalsThread) do
    if not IsSuccess then
    begin
      WriteLn('failed to load confidence intervals');
      Halt(2);
    end;

  try
    LoadConfidenceIntervalsCriticalSect.Acquire;
    ConfidenceIntervalsAreLoaded := True;
  finally
    LoadConfidenceIntervalsCriticalSect.Release;
  end;
end;

function megatt.EverythingIsLoaded: Boolean;
begin
  Result := (NamesAreLoaded and TreeIsLoaded and RanksAreLoaded and ConfidenceIntervalsAreLoaded and LeafCountsAreLoaded);
end;

constructor megatt.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IsMobileFriendly := True;
  StopOnException:=True;
  DoPruneTreeSlow := False;
  O2File := EmptyStr;
  CO2File := EmptyStr;
  EarthImpactsFile := EmptyStr;
  ExportPairwiseDistances := False;
end;

destructor megatt.Destroy;
begin
  inherited Destroy;
end;

procedure megatt.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: megatt;
begin
  Application:=megatt.Create(nil);
  Application.Run;
  Application.Free;
end.

