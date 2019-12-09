unit gscaleticks;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
 {$M+}

interface

uses
  Classes, SysUtils, ttconst;

type

  { TScaleTick }

  TScaleTick = class(TObject)
    private
      FPoints: TArrayOfTPoint;
      FTime: Double;
      FIsLogScale: Boolean;
      function GetTimeText: String;

    public
      constructor Create(IsLogScale: Boolean=False);
      destructor Destroy; override;

      property Time: Double read FTime write FTime;
      property Timetext: String read GetTimeText;
      property Points: TArrayOfTPoint read FPoints write FPoints;
  end;

implementation

uses
  math;

{ TScaleTick }

function TScaleTick.GetTimeText: String;
begin
  Assert(CompareValue(FTime, 0.0, 0.0000001) >= 0);
  if FIsLogScale then
    Result := IntToStr(Round(FTime))
  else if CompareValue(FTime, 0.0, 0.0000001) = 0 then
    Result := '0'
  else if FTime < 0.01 then
    Result := Format('%.2e', [FTime])
  else if FTime < 5 then
    Result := Format('%.2f', [FTime])
  else
    Result := IntToStr(Round(FTime));
end;

constructor TScaleTick.Create(IsLogScale: Boolean=False);
begin
  SetLength(FPoints, 2);
  FTime := -1;
  FIsLogScale := IsLogScale;
end;

destructor TScaleTick.Destroy;
begin
  SetLength(FPoints, 0);
  inherited Destroy;
end;

end.

