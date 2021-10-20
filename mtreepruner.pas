unit mtreepruner;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, gtreelist, gtreedata, MLongintList, gtreedataadapter,
  gsvgwriter, StrHashMap, gtaxonomicrank, gnamesmap, ttconst, fpspreadsheet;

type



  { TTreePruner }

  TTreePruner = class(TObject)
    private
      FDeepestRank: TTaxonomicRank;
      FDoLogScale: Boolean;
      FDoMapNames: Boolean;
      FDoNewick: Boolean;
      FIsMobileFriendly: Boolean;
      FLogStrings: TStringList;
      FPanelHeight: Integer;
      //FNamesMapFile: String;
      //FNamesMap: TStringHashMap;
      FRanksFile: String;
      FVSpacing: Integer;
      FWidth: Integer;
      procedure SetDoLogScale(AValue: Boolean);
      procedure SetDoMapNames(AValue: Boolean);
      procedure SetDoNewick(AValue: Boolean);
      procedure SetLogStrings(AValue: TStringList);
      procedure SetPanelHeight(AValue: Integer);
      procedure SetVSpacing(AValue: Integer);
      procedure SetWidth(AValue: Integer);
      function GetNodeHeights: TDoubleArray;
      //procedure SetNamesMapFile(AValue: String);
      //procedure SetNamesMap(AValue: TStringHashMap);

    protected
      FNodeIdsFile: String;
      //FNewickFile: String;
      FTreeList: TTimeTreeList;
      FNodeIds: TLongIntList;
      FTree: TFpNodeTreeDataAdapter;
      FSvgWriter: TTimetreeSvgWriter;
      FBestNumDecimals: Integer;
      procedure SetNodeIdsFile(AValue: String);
      //procedure SetNewickFile(AValue: String);
      function LoadNodeIds: Boolean;
      function BestNumDecimalsForHtmlTable(d: T2DArrayOfExtended): Integer;
      function WritePairwiseDistancesToMatrixHtmlFile(d: T2DArrayOfExtended; baseFileName: String): Boolean;
      function WritePairwiseDistancesToColumnHtmlFile(d: T2DArrayOfExtended; baseFileName: String): Boolean;
    public
      constructor Create;
      destructor Destroy; override;
      function CanScaleStudyTree(studyTree: TTimeTreeList): Boolean;
      function MapOtuNames: Boolean;
      function CountRanks(aTreeList: TTimeTreeList; outFile: String): Boolean;
      function PruneTree(aList: TTimeTreeList; aNodeIds: String; DoSlowVersion: Boolean=False): Boolean;
      function WritePrunedTreeToSvg(SvgFile: String; ImpactsFile: String=''; O2File: String=''; CO2File: String=''): Boolean;
      function WritePairwiseDistanceToCsvFile(SvgFile: String): Boolean; overload;
      function WritePairwiseDistanceToCsvFile(d: T2DArrayOfExtended; SvgFile: String; isMatrixFormat: Boolean = True): Boolean; overload;
      function WritePairwiseDistancesToHtml(htmlFile: String): Boolean;
      function WritePairwiseDistancesToSpreadsheet(d: T2DArrayOfExtended; baseName: String; isMatrixFormat: Boolean): Boolean;
      function WriteUserTreeToSvg(aList: TTimeTreeList; SvgFile: String; ImpactsFile: String=''; O2File: String=''; CO2File: String=''): Boolean;
      property NodeIdsFile: String read FNodeIdsFile write SetNodeIdsFile;
      property DoMapNames: Boolean read FDoMapNames write SetDoMapNames;
      property RanksFile: String read FRanksFile write FRanksFile;
      property DeepestRank: TTaxonomicRank read FDeepestRank write FDeepestRank;
      property Width: Integer read FWidth write SetWidth;
      property IsMobileFriendly: Boolean read FIsMobileFriendly write FIsMobileFriendly;
      property DoLogScale: Boolean read FDoLogScale write SetDoLogScale;
      property DoNewick: Boolean read FDoNewick write SetDoNewick;
      property VSpacing: Integer read FVSpacing write SetVSpacing;
      property PanelHeight: Integer read FPanelHeight write SetPanelHeight;
      property LogStrings: TStringList read FLogStrings write SetLogStrings;
  end;

implementation

uses
  dateutils, gconfidenceintervals, fpsTypes, fpsallformats;

{ TTreePruner }

procedure TTreePruner.SetDoMapNames(AValue: Boolean);
begin
  if FDoMapNames=AValue then Exit;
  FDoMapNames:=AValue;
end;

procedure TTreePruner.SetDoNewick(AValue: Boolean);
begin
  if FDoNewick=AValue then Exit;
  FDoNewick:=AValue;
  FSvgWriter.DoNewick := AValue;
end;

procedure TTreePruner.SetLogStrings(AValue: TStringList);
begin
  if FLogStrings=AValue then Exit;
  FLogStrings:=AValue;
end;

procedure TTreePruner.SetPanelHeight(AValue: Integer);
begin
  if FPanelHeight=AValue then Exit;
  FPanelHeight:=AValue;
  FSvgWriter.PanelHeight:=AValue;
end;

procedure TTreePruner.SetVSpacing(AValue: Integer);
begin
  if FVSpacing=AValue then Exit;
  FVSpacing:=AValue;
  FSvgWriter.VSpacing:=AValue;
end;

procedure TTreePruner.SetDoLogScale(AValue: Boolean);
begin
  if FDoLogScale=AValue then Exit;
  FDoLogScale:=AValue;
  FSvgWriter.DoLogScale := AValue;
end;

procedure TTreePruner.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  FSvgWriter.Width := FWidth;
end;

function TTreePruner.GetNodeHeights: TDoubleArray;
begin
  Result := FTree.GetNodeHeights;
end;

procedure TTreePruner.SetNodeIdsFile(AValue: String);
begin
  if FNodeIdsFile=AValue then Exit;
  FNodeIdsFile:=AValue;
end;

function TTreePruner.LoadNodeIds: Boolean;
var
  aFile: TextFile;
  TempId: Integer;
begin
  Result := False;
  if not FileExists(FNodeIdsFile) then
    Exit;

  try
    try
      AssignFile(aFile, FNodeIdsFile);
      Reset(aFile);
      while not EOF(aFile) do
      begin
        Read(aFile, TempId);
        if (TempId > 0) or (not EOF(aFile)) then
        begin
          FNodeIds.Add(TempId);
        end;
      end;
      Result := (FNodeIds.Count > 0);
    except
      on E:Exception do
        raise E;
    end;
  finally
    CloseFile(aFile);
  end;
end;

function TTreePruner.BestNumDecimalsForHtmlTable(d: T2DArrayOfExtended): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Length(d) > 1 then
    for i := 1 to Length(d) - 1 do
    begin
      if d[i][0] < 0.1 then
        Result := 2
      else if d[i][0] < 1.0 then
        Result := 1
    end;
end;

function TTreePruner.WritePairwiseDistancesToMatrixHtmlFile(d: T2DArrayOfExtended; baseFileName: String): Boolean;
var
  matrixFile: Text;
  i, j: Integer;
  filename: String;
begin
  try
    filename := ChangeFileExt(baseFileName, '_distances_matrix.html');
    AssignFile(matrixFile, filename);
    Rewrite(matrixFile);
    WriteLn(matrixFile, '<table>');
    WriteLn(matrixFile, '<thead>');
    WriteLn(matrixFile, '<tr>');
    Write(matrixFile, '<th>#</th><th>Name</th>');
    for i := 0 to FTreeList.NoOfOTUs - 1 do
      Write(matrixFile, Format('<th title="%s">%d</th>', [FTreeList.OTUName[i], i + 1]));
    WriteLn(matrixFile, EmptyStr);
    WriteLn(matrixFile, '</tr>');
    WriteLn(matrixFile, '</thead>');
    WriteLn(matrixFile, '<tbody>');
    WriteLn(matrixFile, '<tr>');
    WriteLn(matrixFile, '<td>1</td>');
    WriteLn(matrixFile, Format('<td>%s</td>' , [FTreeList.OTUName[0]]));
    for j := 0 to Length(d) - 1 do
      WriteLn(matrixFile, '<td class="emptyCell"></td>');
    WriteLn(matrixFile, '</tr>');
    for i := 1 to Length(d) - 1 do
    begin
      WriteLn(matrixFile, '<tr>');
      Write(matrixFile, Format('<td>%d</td>', [i + 1]));
      Write(matrixFile, Format('<td>%s</td>', [FTreeList.OTUName[i]]));
      for j := 0 to i - 1 do
        Write(matrixFile, Format('<td title="%s - %s">%.' + IntToStr(FBestNumDecimals) + 'f</td>', [FTreeList.OTUName[i], FTreeList.OTUName[j], d[i][j]]));
      for j := i to Length(d) - 1 do
        Write(matrixFile, '<td class="emptyCell"></td>');
      WriteLn(matrixFile, '</tr>');
    end;
    WriteLn(matrixFile, '</tbody>');
    WriteLn(matrixFile, '</table>');
  finally
    CloseFile(matrixFile);
  end;
  Result := FileExists(filename);
end;

function TTreePruner.WritePairwiseDistancesToColumnHtmlFile(d: T2DArrayOfExtended; baseFileName: String): Boolean;
var
  tableFile: Text;
  i, j: Integer;
  filename: String;
begin
  try
    filename := ChangeFileExt(baseFileName, '_distances_columns.html');
    AssignFile(tableFile, filename);
    Rewrite(tableFile);
    WriteLn(tableFile, '<table>');
    WriteLn(tableFile, '<thead>');
    WriteLn(tableFile, '<tr>');
    Write(tableFile, '<th>Taxon A</th><th>Taxon B</th><th>Time</th>');
    WriteLn(tableFile, EmptyStr);
    WriteLn(tableFile, '</tr>');
    WriteLn(tableFile, '</thead>');
    WriteLn(tableFile, '<tbody>');
    for i := 0 to Length(d) - 1 do
    begin
      for j := 0 to i - 1 do
      begin
        if i <> j then
        begin
          WriteLn(tableFile, '<tr>');
          Write(tableFile, Format('<td>%s</td>', [FTreeList.OTUName[i]]));
          Write(tableFile, Format('<td>%s</td>', [FTreeList.OTUName[j]]));
          Write(tableFile, Format('<td>%.' + IntToStr(FBestNumDecimals) + 'f</td>', [d[i][j]]));
          WriteLn(tableFile, '</tr>');
        end;
      end;
    end;
    WriteLn(tableFile, '</tbody>');
    WriteLn(tableFile, '</table>');
  finally
    CloseFile(tableFile);
  end;
  Result := FileExists(filename);
end;

function TTreePruner.MapOtuNames: Boolean;
var
  i: Integer;
  aList: TStringList;
  aString: String;
begin
  Result := False;
  aList := FTreeList.OTUNameList;
  for i := (aList.Count - 1) downto 0 do
  begin
    if NamesMap.Contains(aList[i]) then
    begin
      aString := TOtuName(NamesMap[aList[i]]).Name;
      aList[i] := aString;
    end
    else
    begin
      Assert(False, 'unnamed taxon found in pruned tree. Either the source newick file is bad or a name is missing from the map list');
      FTreeList[0].RemoveOTU(i);
      aList.Delete(i);
    end;
  end;
  Result := True;
end;

constructor TTreePruner.Create;
begin
  FTreeList := TTimeTreeList.Create;
  FNodeIds := TLongIntList.Create;
  FTree := TFpNodeTreeDataAdapter.Create;
  FSvgWriter := TTimetreeSvgWriter.Create;
  FRanksFile := EmptyStr;
  FDeepestRank := trUnknown;
  FDoLogScale := False;
  FLogStrings := nil;
  FIsMobileFriendly := True;
end;

destructor TTreePruner.Destroy;
var
  i: Integer;
begin
  //if Assigned(FTreeList) then
  //  FTreeList.Free;
  if Assigned(FNodeIds) then
    FNodeIds.Free;
  if Assigned(FTree) then
    FTree.Free;
  if Assigned(FSvgWriter) then
    FSvgWriter.Free;
  inherited Destroy;
end;

function TTreePruner.CanScaleStudyTree(studyTree: TTimeTreeList): Boolean;
begin
  Assert(Assigned(studyTree[0]));
  Result := False;
  if not studyTree.isBLen then
    Exit;
  FTree.SetTreeData(studyTree[0], False);
  Result := (FTree.GetPercentMissingBlensData < (1 - BLENS_DATA_COVERAGE_NEEDED_FOR_SCALING));
  if not Result then
    studyTree.isBLen := False;
end;

function TTreePruner.CountRanks(aTreeList: TTimeTreeList; outFile: String): Boolean;
var
  aData: TTimeTreeData = nil;
  startTime, endTime: TDateTime;
  aList: TStringList;
begin
  aList := nil;
  Result := False;
  FTreeList := aTreeList;

  try
    try
      aList := TStringList.Create;
      if not FTreeList.UpdateTimetreeIDs then
        raise Exception.Create('Failed to update timetree node IDs');
      aData := FTreeList.Remove(0); { we will take temporary ownership}
      FTree.SetTreeData(aData, False);
      if not FTree.CountRanks(aList) then
        raise Exception.Create('failed to count taxonomic ranks');
      aList.SaveToFile(outFile);
      Result := True;
    except
      on E:Exception do
      begin
        writeLn('Error when counting ranks: ' + E.Message);
        raise E;
      end;
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TTreePruner.PruneTree(aList: TTimeTreeList; aNodeIds: String; DoSlowVersion: Boolean=False): Boolean;
var
  aData: TTimeTreeData = nil;
  startTime, endTime: TDateTime;
  temp: String;
begin
  startTime := Now;
  Result := False;
  NodeIdsFile:= aNodeIds;
  try
    FTreeList := aList;
    if not LoadNodeIds then
      raise Exception.Create('Invalid or missing node IDs');
    if not FTreeList.UpdateTimetreeIDs then
      raise Exception.Create('Failed to update timetree node IDs');
    aData := FTreeList.Remove(0); { we will take temporary ownership}
    FTree.SetTreeData(aData, False);
    if FDeepestRank <> trUnknown then
    begin
      DoMapNames := False;
      Result := FTree.PruneTree(FNodeIds, FTreeList.OTUNameList, FDeepestRank);
    end
    else
    begin
      DoMapNames := True;
      Result := FTree.PruneTree(FNodeIds, FTreeList.OTUNameList, DoSlowVersion); { the real meat and potatoes}
    end;
    if Result then
    begin
      //if (DoMapNames) and (not MapOtuNames) then { converts the otunames from IDs to actual names}
      //  raise Exception.Create('Failed to map leaf node IDs to names');
      FTreeList.Add(aData);
      endTime := Now;
      if Assigned(FLogStrings) then
      begin
        temp := 'Prune tree execution time: ' + IntToStr(MilliSecondsBetween(endTime, startTime)) + ' ms';
        FLogStrings.Add(temp);
      end;
      //FTreeList.ExportToNewickFile('/home/gstecher/Downloads/prunedTree.nwk', True, False, 0.0);
      //FTreeList.ExportToNewickFile('C:\Users\bd3 software\Documents\MEGA7\pruneTree\prunedTree.nwk', True, False, 0.0);
      //debug := FTreeList[0].StateToStringList('pruned tree');
      //debug.SaveToFile('/home/gstecher/Downloads/prunedTree.txt');
      //debug.Free;
      //FTreeList.OtuNameList.SaveToFile('/home/gstecher/Downloads/otuNames.txt');
    end
    else
      raise Exception.Create('Tree pruning function failed mysteriously');
  except
    on E:Exception do
    begin
      WriteLn('Exception raised: ' + E.Message);
      raise E;
    end;
  end;
end;

function TTreePruner.WritePrunedTreeToSvg(SvgFile: String; ImpactsFile: String=''; O2File: String=''; CO2File: String=''): Boolean;
var
  aList: TStringList = nil;
  NewickFile: String;
  NodeCountFile: Text;
  NodeCount: String;
  startTime: TDateTime;
  temp: String;
begin
  Result := True;
  startTime := Now;

  try
    //NodeCount := IntToStr(2 * FTreeList.NoOfOTUs - 1);
    NodeCount := IntToStr(FTreeList.NoOfOtus);
    AssignFile(NodeCountFile, ChangeFileExt(SvgFile, '.nodecount'));
    ReWrite(NodeCountFile);
    WriteLn(NodeCountFile, NodeCount);
    if DoNewick then
    begin
      NewickFile := ChangeFileExt(SvgFile, '.nwk');
      FTreeList.isRooted :=True;
      Result := FTreeList.ExportToNewickFile(NewickFile, True, False, 0.0);
      FTreeList.isRooted := False;
      Result := Result and FileExists(NewickFile);
    end;
    FSvgWriter.SetData(FTreeList, ImpactsFile, O2File, CO2File);
    FSvgWriter.DeepestRank := FDeepestRank;
    FSvgWriter.RanksFile := FRanksFile;
    if FNodeIds.Count >= 3 then
      FSvgWriter.UpdateTaxaOrder(FNodeIds);
    FSvgWriter.IsMobileFriendly := FIsMobileFriendly;
    FSvgWriter.GenerateSvgStrings(ChangeFileExt(SvgFile, '.tree.svg'), ChangeFileExt(SvgFile, '.panels.svg'), 0);
    if Assigned(FLogStrings) then
    begin
      temp := 'Write SVG execution time: ' + IntToStr(MilliSecondsBetween(Now, startTime)) + ' ms';
      FLogStrings.Add(temp);
    end;    Result := (Result and FileExists(ChangeFileExt(SvgFile, '.tree.svg')) and FileExists(ChangeFileExt(SvgFile, '.panels.svg')));
    {$IFDEF DEBUG}
     //debug := TStringList.Create;
     //debug.Add('<html><body>');
     //aList := FSvgWriter.GetTreeStrings;
     //debug.AddStrings(aList);
     //aList := FSvgWriter.GetPanelsStrings;
     //debug.AddStrings(aList);
     //debug.Add('</body></html>');
     //debug.SaveToFile(ChangeFileExt(SvgFile, '.html'));
     //debug.Free;
    {$ENDIF}
  finally
    CloseFile(NodeCountFile);
  end;
end;

function TTreePruner.WritePairwiseDistanceToCsvFile(SvgFile: String): Boolean;
var
  d: T2DArrayOfExtended;
  aTree: TFpNodeTreeDataAdapter = nil;
begin
  try
    aTree := TFpNodeTreeDataAdapter.Create;
    aTree.SetTreeData(FTreeList[0], False);
    d := aTree.GetPairwiseDistances;
    Result := WritePairwiseDistanceToCsvFile(d, SvgFile);
  finally
    if Assigned(aTree) then
      aTree.Free;
  end;
  FTreeList := nil; { it will be freed downstream, done this way to avoid making copies in order to improve performance}
end;

function TTreePruner.WritePairwiseDistanceToCsvFile(d: T2DArrayOfExtended; SvgFile: String; isMatrixFormat: Boolean = True): Boolean;
var
  target: Text;
  i, j: Integer;
  filename: String;
begin
  try
    if isMatrixFormat then
      filename := ChangeFileExt(SvgFile, '_distances_matrix.csv')
    else
      filename := ChangeFileExt(SvgFile, '_distances_column.csv');
    AssignFile(target, filename);
    Rewrite(target);
    if isMatrixFormat then
    begin
      WriteLn(target, FTreeList.OTUName[0]);
      for i := 1 to Length(d) - 1 do
      begin
        Write(target, FTreeList.OTUName[i], ',');
        for j := 0 to i - 1 do
          Write(target, Format('%.' + IntToStr(FBestNumDecimals) + 'f', [d[i][j]]), ',');
        WriteLn(target, EmptyStr);
      end;
    end
    else
    begin
      for i := 1 to Length(d) - 1 do
      begin
        for j := 0 to i - 1 do
          WriteLn(target, Format('%s,%s,%.' + IntToStr(FBestNumDecimals) + 'f', [FTreeList.OtuName[i], FTreeList.OtuName[j], d[i][j]]));
      end;
    end;

  finally
    CloseFile(target);
  end;
  Result := FileExists(filename);
end;

function TTreePruner.WritePairwiseDistancesToHtml(htmlFile: String): Boolean;
var
  d: T2DArrayOfExtended;
  aTree: TFpNodeTreeDataAdapter = nil;
begin
  try
    aTree := TFpNodeTreeDataAdapter.Create;
    aTree.SetTreeData(FTreeList[0], False);
    d := aTree.GetPairwiseDistances;
    FBestNumDecimals := BestNumDecimalsForHtmlTable(d);
    Result := WritePairwiseDistancesToColumnHtmlFile(d, htmlFile);
    if Result then
      Result := WritePairwiseDistancesToMatrixHtmlFile(d, htmlFile)
    else
      raise Exception.Create('failed to generate pairwise distances columns table file');
    if Result then
      Result := WritePairwiseDistanceToCsvFile(d, htmlFile, True)
    else
      raise Exception.Create('failed to generate pairwise distances matrix xlsx file');
    if Result then
      Result := WritePairwiseDistanceToCsvFile(d, htmlFile, False)
    else
      raise Exception.Create('failed to generate pairwise distances column xlsx file');
  finally
    if Assigned(aTree) then
      aTree.Free;
  end;
  FTreeList := nil; { it will be freed downstream, done this way to avoid making copies in order to improve performance}
end;

function TTreePruner.WritePairwiseDistancesToSpreadsheet(d: T2DArrayOfExtended; baseName: String; isMatrixFormat: Boolean): Boolean;
var
  wb: TsWorkbook = nil;
  ws: TsWorksheet = nil;
  i,j, index: Integer;
  filename: String;
begin
  try
    wb := TsWorkbook.Create;
    ws := wb.AddWorksheet('pairwise distances');
    if isMatrixFormat then
    begin
      filename := ChangeFileExt(baseName, '_distances_matrix.xlsx');
      ws.WriteBlank(0, 0);
      ws.WriteBlank(0, 1);
      for i := 0 to Length(d) - 1 do
        ws.WriteNumber(0, i + 2, i + 1);
      for i := 0 to Length(d) - 1 do
      begin
        ws.WriteNumber(i + 1, 0, i + 1);
        ws.WriteText(i + 1, 1, FTreeList.OTUName[i]);
        for j := 0 to i - 1 do
          if j <> i then
            ws.WriteNumber(i + 1, j + 2, d[i][j]);
      end;
    end
    else
    begin
      filename := ChangeFileExt(baseName, '_distances_column.xlsx');
      ws.WriteText(0, 0, 'Taxon A');
      ws.WriteText(0, 1, 'Taxon B');
      ws.WriteText(0, 2, 'Divergence Time');
      index := 1;
      for i := 1 to Length(d) - 1 do
      begin
        for j := 0 to i - 1 do
        begin
          ws.WriteText(index, 0, FTreeList.OTUName[i]);
          ws.WriteText(index, 1, FTreeList.OTUName[j]);
          ws.WriteNumber(index, 2, d[i][j]);
          inc(index);
        end;
      end;
    end;
    wb.WriteToFile(filename, sfOOXML, True);
    Result := FileExists(filename);
  finally
    if Assigned(wb) then
      wb.Free;
  end;
end;

function TTreePruner.WriteUserTreeToSvg(aList: TTimeTreeList; SvgFile: String; ImpactsFile: String; O2File: String; CO2File: String): Boolean;
var
  NewickFile: String;
  NodeCountFile: Text;
  NodeCount: String;
  startTime: TDateTime;
  temp: String;
begin
  Result := True;
  startTime := Now;

  try
    FTreeList := aList;
    NodeCount := IntToStr(FTreeList.NoOfOtus);
    AssignFile(NodeCountFile, ChangeFileExt(SvgFile, '.nodecount'));
    ReWrite(NodeCountFile);
    WriteLn(NodeCountFile, NodeCount);
    if DoNewick then
    begin
      NewickFile := ChangeFileExt(SvgFile, '.nwk');
      FTreeList.isRooted :=True;
      Result := FTreeList.ExportToNewickFile(NewickFile, True, False, 0.0);
      Result := Result and FileExists(NewickFile);
    end;
    FTreeList.isRooted := False;
    if IsStudyTree then
      if not CanScaleStudyTree(aList) then
      begin
        FSvgWriter.StudyTimeNodeHeights := GetNodeHeights;
        FSvgWriter.CanScaleTree := False;
        aList.isBLen := False;
      end;
    FSvgWriter.SetData(aList, ImpactsFile, O2File, CO2File);
    FSvgWriter.RanksFile := FRanksFile;
    FTreeList := nil; { it will be freed downstream, done this way to avoid making copies in order to improve performance}
    FSvgWriter.GenerateSvgStrings(ChangeFileExt(SvgFile, '.tree.svg'), ChangeFileExt(SvgFile, '.panels.svg'), 0);
    if Assigned(FLogStrings) then
    begin
      temp := 'Write SVG execution time: ' + IntToStr(MilliSecondsBetween(Now, startTime)) + ' ms';
      FLogStrings.Add(temp);
    end;    Result := (Result and FileExists(ChangeFileExt(SvgFile, '.tree.svg')) and FileExists(ChangeFileExt(SvgFile, '.panels.svg')));
  finally
    CloseFile(NodeCountFile);
  end;
end;

end.

