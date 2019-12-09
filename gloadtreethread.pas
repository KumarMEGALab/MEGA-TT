unit gloadtreethread;

interface

uses
  Classes, SysUtils, gtreelist;

type

  { TLoadTreeThread }

  TLoadTreeThread = class(TThread)
    private
      FEndTime: TDateTime;
      FLogStrings: TStringList;
      FStartTime: TDateTime;
      procedure SetLogStrings(AValue: TStringList);

    protected
      FIsSuccess: Boolean;
      function LoadTree: Boolean; virtual; abstract;
      function GetTreeList: TTimeTreeList;
      procedure SetTreeList(const Value: TTimeTreeList);
    public
      Messages: TStringList;
      FTreeList: TTimeTreeList;
      Filename: String;
      constructor Create;
      destructor Destroy; override;
      procedure Execute; override;

      property TreeList: TTimeTreeList read GetTreeList write SetTreeList; // TreeList should always be owned by some other object so there is no wastful copying
      property IsSuccess: Boolean read FIsSuccess;
      property LogStrings: TStringList read FLogStrings write SetLogStrings;
      property StartTime: TDateTime read FStartTime;
      property EndTime: TDateTime read FEndTime;
  end;

  TLoadNewickTreeThread = class(TLoadTreeThread)

    protected
      function LoadTree: Boolean; override;
    public

  end;



implementation

uses
  ttconst, dateutils;

{ TLoadNewickTreeThread }

function TLoadNewickTreeThread.LoadTree: Boolean;
var
  temp: String;
begin
  FStartTime := Now;
  Result := False;
  Messages.Clear;
  if not FileExists(Filename) then
  begin
    Messages.Add('The specified newick file does not exist: ' + Filename);
    Exit;
  end;

  try
    FTreeList := TTimeTreeList.Create;
    if not TreeList.ImportFromNewickFile(Filename, nil) then
      Messages.Add('Failed to load the newick file')
    else
      Result := True;
    FEndTime := Now;
    if Assigned(FLogStrings) then
    begin
      try
        LogfileCriticalSect.Acquire;
        temp := 'Load tree: execution time = ' + IntToStr(MilliSecondsBetween(FEndTime, FStartTime)) + ' ms';
        FLogStrings.Add(temp);
      finally
        LogfileCriticalSect.Release;
      end;
    end;
  except
    on E:Exception do
    begin
      Messages.Add('Error when loading the newick tree: ' + E.Message);
    end;
  end;
end;

{ TLoadTreeThread }

constructor TLoadTreeThread.Create;
begin
  inherited Create(True);
  FLogStrings := nil;
  FreeOnTerminate := True;
  FTreeList := nil;
  Messages := TStringList.Create;
end;

destructor TLoadTreeThread.Destroy;
begin
  if Assigned(Messages) then
    Messages.Free;
  // do not free the TreeList, just let some other object take ownership so we don't have to copy it
  inherited;
end;

procedure TLoadTreeThread.Execute;
begin
  //inherited;
  FIsSuccess := LoadTree;
end;

procedure TLoadTreeThread.SetLogStrings(AValue: TStringList);
begin
  if FLogStrings=AValue then Exit;
  FLogStrings:=AValue;
end;

function TLoadTreeThread.GetTreeList: TTimeTreeList;
begin
  Result := FTreeList;
end;

procedure TLoadTreeThread.SetTreeList(const Value: TTimeTreeList);
begin
  FTreeList := Value;
end;

end.
