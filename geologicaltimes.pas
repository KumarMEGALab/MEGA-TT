unit geologicaltimes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ttconst;

type
  TTimespanType = (tstEon, tstEra, tstPeriod, tstEpoch, tstAge);
  TGeoBgColors = (gbcEon, gbcEra, gbcPeriod, gbcEpoch, gbcAge, gbcAuto, gbcNone);

  { TGeologicTime }

  TGeologicTime = class(TObject)
    protected
      FAbbr: String;
      FBlue: Integer;
      FGreen: Integer;
      FName: String;
      FRed: Integer;
      FEndMya: Double;
      FStartMya: Double;
      FTimespanType: TTimespanType;
      function GetHexaDecimalColorStr: String;

    public
      constructor Create(aType: TTimespanType; aName: String; aStart, aEnd: Double; aRed, aGreen, aBlue: Integer; Abbr: String='');
      destructor Destroy; override;
      function GetBestFitText(aRect: TRect; Orientation: TTextOrientation=tdHorizontal): String;
      property Red: Integer read FRed write FRed;  { colors are done this way because we need to run sometimes in a headless environment for timetree and can't use graphics classes}
      property Green: Integer read FGreen write FGreen;
      property Blue: Integer read FBlue write FBlue;
      property Name: String read FName write FName;
      property StartMya: Double read FStartMya write FStartMya; {Millions of Years Ago that the geological time ended}
      property EndMya: Double read FEndMya write FEndMya; {Millions of Years Ago that the geological time ended}
      property TimespanType: TTimespanType read FTimespanType;
      property HexaDecimalColorStr: String read GetHexaDecimalColorStr;
      property Abbreviation: String read FAbbr write FAbbr;
  end;

  TArrayOfGeologicTime = array of TGeologicTime;

  { TCompositeGeologicTime }

  TCompositeGeologicTime = class(TGeologicTime)
    private
    protected
      FEons: TList;
      FEras: TList;
      FPeriods: TList;
      FEpochs: TList;
      FAges: TList;
      procedure InitTimescales;
      procedure InitEons;
      procedure InitEras;
      procedure InitPeriods;
      procedure InitEpochs;
      procedure InitAges;
      function FindFirstAndLast(var First: Integer; var Last: Integer; const min: Double; const max: Double; const aType: TTimespanType): Boolean;
    public
      constructor Create;
      destructor Destroy; override;

      function GetTimescale(aStartTime: Double; aType: TTimespanType): TArrayOfGeologicTime; overload;
      function GetTimescale(minTime, maxTime: Double; aType: TTimespanType): TArrayOfGeologicTime; overload;
      function GetLongestName(aStartTime: Double; aType: TTimespanType): String; overload;
      function GetLongestName(minTime, maxTime: Double; aType: TTimespanType): String; overload;
  end;

  function TimespanTypeString(aType: TTimespanType): String;
  function StringToTimespanType(aString: String): TTimespanType;
  function StringToGeoBgColors(aString: String): TGeoBgColors;
  function GeoBgColorsToString(bgColors: TGeoBgColors): String;
  function MapGeoBgColorsToTimespanType(bgColors: TGeoBgColors): TTimespanType;

  var
    GeoScaleForBgColors: TGeoBgColors;


implementation

uses
  gtextwidth, math;

function TimespanTypeString(aType: TTimespanType): String;
begin
  case aType of
    tstEon: Result := 'Eon';
    tstEra: Result := 'Era';
    tstPeriod: Result := 'Period';
    tstEpoch: Result := 'Epoch';
    tstAge: Result := 'Age';
    else
    begin
      Assert(False, 'missing handler for TTimespanType');
      Result := 'unknown';
    end;
  end;
end;

function StringToTimespanType(aString: String): TTimespanType;
begin
  if LowerCase(aString) = 'eon' then
    Result := tstEon
  else if LowerCase(aString) = 'era' then
    Result := tstEra
  else if LowerCase(aString) = 'period' then
    Result := tstPeriod
  else if LowerCase(aString) = 'epoch' then
    Result := tstEpoch
  else if LowerCase(aString) = 'age' then
    Result := tstAge
  else
    raise Exception.Create('invalid geologic level parameter given. Must be one of: eon, era, period, epoch, age');
end;

function StringToGeoBgColors(aString: String): TGeoBgColors;
begin
  if LowerCase(aString) = 'eons' then
    Result := gbcEon
  else if LowerCase(aString) = 'eras' then
    Result := gbcEra
  else if LowerCase(aString) = 'periods' then
    Result := gbcPeriod
  else if LowerCase(aString) = 'epochs' then
    Result := gbcEpoch
  else if LowerCase(aString) = 'ages' then
    Result := gbcAge
  else if LowerCase(aString) = 'auto' then
    Result := gbcAuto
  else if LowerCase(aString) = 'none' then
    Result := gbcNone
  else
    raise Exception.Create('invalid geo-bg-colors parameter given. Must be one of: eons, eras, periods, epochs, ages, auto, none');
end;

function GeoBgColorsToString(bgColors: TGeoBgColors): String;
begin
  case bgColors of
    gbcEon: Result := 'eons';
    gbcEra: Result := 'eras';
    gbcPeriod: Result := 'periods';
    gbcEpoch: Result := 'epochs';
    gbcAge: Result := 'ages';
    gbcAuto: Result := 'auto';
    gbcNone: Result := 'none';
  end;
end;

function MapGeoBgColorsToTimespanType(bgColors: TGeoBgColors): TTimespanType;
begin
  case bgColors of
    gbcEon: Result := tstEon;
    gbcEra: Result := tstEra;
    gbcPeriod: Result := tstPeriod;
    gbcEpoch: Result := tstEpoch;
    gbcAge: Result := tstAge;
    gbcAuto, gbcNone:
      begin
        Assert(False, 'invalid conversion bg colors to timespan type');
        Result := tstPeriod;
      end;
  end;
end;

{ TCompositeGeologicTime }

procedure TCompositeGeologicTime.InitEons;
begin
  FEons.Add(TGeologicTime.Create(tstEon, 'Phanerozoic', 542.0, 0.0, 154, 217, 221, 'Ph'));
  FEons.Add(TGeologicTime.Create(tstEon, 'Proterozoic', 2500.0, 542.0, 247, 53, 99, 'Pr'));
  FEons.Add(TGeologicTime.Create(tstEon, 'Archean', 4000.0, 2500.0, 240, 4, 127, 'A'));
  FEons.Add(TGeologicTime.Create(tstEon, 'Hadeon', 4600.0, 4000.0, 255, 255, 255, 'HDE'));
end;

procedure TCompositeGeologicTime.InitEras;
begin
  FEras.Add(TGeologicTime.Create(tstEra, 'Cenozoic', 65.5, 0.0, 242, 249, 29, 'Cz'));
  FEras.Add(TGeologicTime.Create(tstEra, 'Mesozoic', 251.0, 65.5, 103, 197, 202, 'Mz'));
  FEras.Add(TGeologicTime.Create(tstEra, 'Paleozoic', 542.0, 251.0, 153, 192, 141, 'Pz'));
  FEras.Add(TGeologicTime.Create(tstEra, 'Neo-Proterozoic', 1000.0, 542.0, 254, 179, 66, 'Z'));
  FEras.Add(TGeologicTime.Create(tstEra, 'Meso-Proterozoic', 1600.0, 1000.0, 253, 180, 98, 'Y'));
  FEras.Add(TGeologicTime.Create(tstEra, 'Paleo-Proterozoic', 2500.0, 1600.0, 247, 67, 12, 'X'));
  FEras.Add(TGeologicTime.Create(tstEra, 'Neoarchean', 2800.0, 2500.0, 249, 155, 193, 'W'));
  FEras.Add(TGeologicTime.Create(tstEra, 'Mesoarchean', 3200.0, 2800.0, 247, 104, 169, 'V'));
  FEras.Add(TGeologicTime.Create(tstEra, 'Paleoarchean', 3600.0, 3200.0, 244, 68, 159, 'U'));
  FEras.Add(TGeologicTime.Create(tstEra, 'Eoarchean', 4600.0, 3600.0, 218, 3, 127, 'EA'));
end;

procedure TCompositeGeologicTime.InitPeriods;
begin
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Neogene', 23.03, 0.0, 255, 230, 25, 'Ng'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Paleogene', 65.5, 23.03, 253,154,82, 'Pg'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Cretaceous', 145.5, 65.5, 127,198,78, 'K'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Jurassic', 199.6, 145.5, 52,178,201, 'J'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Triassic', 251.0, 199.6, 129,43,146, 'Tr'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Permian', 299.0, 251.0, 240,64,40, 'P'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Pennsylvanian', 318.1, 299.0, 153,194,181, 'PEN'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Mississipian', 359.2, 318.1, 103,143,102, 'MIS'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Devonian', 416, 359.2, 203,140,55, 'D'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Silurian', 443.7, 416, 179,225,82, 'S'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Ordovician', 488.3, 443.7, 0,146,112, 'O'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Cambrian', 542.0, 488.3, 127,160,86, 'C'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Ediacaran', 630.0, 542.0, 254,217,106, 'EDI'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Cryogenian', 850, 630.0, 254,204,92, 'CRI'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Tonian', 1000, 850.0, 254,191,78, 'TON'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Stenian', 1200, 1000.0, 254,217,154, 'STE'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Ectasian', 1400, 1200.0, 253,204,138, 'ECT'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Calymmian', 1600, 1400.0, 253, 192, 122, 'CLY'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Statherian', 1800, 1600.0, 248,117,167, 'STA'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Orosirian', 2050, 1800.0, 247,104,152, 'ORO'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Ryacian', 2300, 2050.0, 247,91,137, 'RYA'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Syderian', 2500, 2300.0, 247,79,124, 'SYD'));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, ' ', 2800, 2500.0, 250,167,200));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, ' ', 3200, 2800.0, 248,129,181));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, ' ', 3600, 3200.0, 246,104,178));
  FPeriods.Add(TGeologicTime.Create(tstPeriod, 'Not defined', 4600, 3600.0, 230,29,140, 'ND'));
end;

procedure TCompositeGeologicTime.InitEpochs;
begin
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Holocene', 0.0118, 0.0, 254,242,224, 'HOL'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Pleistocene', 1.806, 0.0118, 255,242,174, 'PLE'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Pliocene', 5.332, 1.806, 255,255,153, 'PLI'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Miocene', 23.03, 5.332, 255,255,0, 'MI'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Oligocene', 33.9, 23.03, 253,192,122, 'OLI'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Eocene', 55.8, 33.9, 253,180,108, 'EOC'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Paleocene', 65.5, 55.8, 253,167,95, 'PAL'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Upper', 99.6, 65.5, 166,216,74, 'u'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Lower', 145.5, 99.6, 140,205,87, 'l'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Upper', 161.2, 145.5, 179,227,238, 'u'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Middle', 175.6, 161.2, 128,207,216, 'm'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Lower', 199.6, 175.6, 66,174,208,' l'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Upper', 228, 199.6, 189,140,195, 'u'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Middle', 245, 228.0, 177,104,177, 'm'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Lower', 251, 245.0, 152,57,153, 'l'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Lopingian', 260.4, 251.0, 251,167,148, 'LOP'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Guadalupian', 270.6, 260.4, 251,116,92, 'GUA'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Cisuralian', 299, 270.6, 239,88,69, 'CIS'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Upper', 306.5, 299.0, 191,208,186, 'u'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Middle', 311.7, 306.5, 166,199,183, 'u'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Lower', 318.1, 311.7, 140,190,180, 'l'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Upper', 326.4, 318.1, 179,190,108, 'u'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Middle', 345.3, 326.4,153,180,108, 'm'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Lower', 359.2, 345.3, 128,171,108, 'l'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Upper', 385.3, 359.2, 241,225,157, 'u'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Middle', 397.5, 385.3, 241,200,104, 'm'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Lower', 416, 397.5, 229,172,77, 'l'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Pridoli', 418.7, 416, 230,245,225, 'PRD'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Ludlow', 422.9, 418.7, 191,230,207, 'LUD'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Wenlock', 428.2, 422.9, 179,225,194, 'WEN'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Llandovery', 443.7, 428.2, 153,215,179, 'LLY'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Upper', 460.9, 443.7, 127,202,147, 'u'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Middle', 471.8, 460.9, 77,180,126, 'm'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Lower', 488.3, 471.8, 26,157,111, 'l'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Furongian', 501, 488.3, 179,224,149, 'FUR'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Series 3', 510, 501.0, 166,207,134, 'SE3'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Series 2', 521, 510.0, 153,192,120, 'SE2'));
  FEpochs.Add(TGeologicTime.Create(tstEpoch, 'Terreneuvian', 542, 521.0, 140,176,108, 'TER'));
end;

procedure TCompositeGeologicTime.InitAges;
begin
  FAges.Add(TGeologicTime.Create(tstAge, ' ', 0.0118, 0.0, 254,242,236));
  FAges.Add(TGeologicTime.Create(tstAge, 'Upper', 0.126, 0.0118, 255,242,211, 'u'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Middle', 0.781, 0.126, 255,242,199, 'm'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Lower', 1.806, 0.781, 255,242,186, 'l'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Gelasian', 2.588, 1.806, 255,255,204, 'GEL'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Piacenzian', 3.6, 2.588, 255,255,191, 'PIA'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Zanclean', 5.332, 3.6, 255,255,179, 'ZAN'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Messinian', 7.246, 5.332, 255,255,115, 'MES'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Tortonian', 11.608, 7.246, 255,255,102, 'TOR'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Serravallian', 13.82, 11.608, 255,255,89, 'SER'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Langhian', 15.97, 13.82, 255,255,77, 'LAN'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Burdigalian', 20.43, 15.97, 255,255,65, 'BUR'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Aquitanian', 23.03, 20.43, 255,255,51, 'AQT'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Chattian', 28.4, 23.03, 254,230,170, 'CHT'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Rupelian', 33.9, 28.4, 254,217,154, 'RUP'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Priabonian', 37.2, 33.9, 253,205,161, 'PRB'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Bartonian', 40.4, 37.2, 253,192,145, 'BRT'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Lutetian', 48.6, 40.4, 252,180,130, 'LUT'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Ypresian', 55.8, 48.6, 252,167,115, 'YPR'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Thanetian', 58.7, 55.8, 253,191,111, 'THA'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Selandian', 61.7, 58.7, 254,191,101, 'SEL'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Danian', 65.5, 61.7, 253,180,98, 'DAN'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Maastrichtian', 70.6, 65.5, 242,250,140, 'MAA'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Campanian', 83.5, 70.6, 230,244,127, 'CMP'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Santonian', 85.8, 83.5, 217,239,116, 'SAN'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Coniacian', 89.3, 85.8, 204,233,104, 'CON'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Turonian', 93.5, 89.3, 191,227,93, 'TUR'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Cenomanian', 99.6, 93.5, 179,222,83, 'CEN'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Albian', 112, 99.6, 204,234,151, 'ALB'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Aptian', 125, 112, 191,228,138, 'APT'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Barremian', 130, 125, 179,223,127, 'BRM'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Hauterivian', 136.4, 130, 166,217,117, 'HAU'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Valanginian', 140.2, 136.4, 153,211,106, 'VAL'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Berriasian', 145.5, 140.2, 140,205,96, 'BER'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Tithonian', 150.8, 145.5, 217,241,247, 'TTH'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Kimmeridgian', 155.7, 150.8, 204,236,244, 'KIM'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Oxfordian', 161.2, 155.7, 191,231,241, 'OXF'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Callovian', 164.7, 161.2, 191,231,229, 'CAL'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Bathonian', 167.7, 164.7, 179,226,227, 'BTH'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Bajocian', 171.6, 167.7, 166,221,224, 'BAJ'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Aalenian', 175.6, 171.6, 154,217,221, 'AAL'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Toarcian', 183, 175.6, 153,206,227, 'TOA'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Pliensbachian', 189.6, 183, 128,197,221, 'PLB'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Sinemurian', 196.5, 189.6, 103,188,216, 'SIN'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Hettangian', 199.6, 196.5, 78,179,211, 'HET'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Rhaetian', 203.6, 199.6, 227,185,219, 'RHT'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Norian', 216.5, 203.6, 214,170,211, 'NOR'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Carnian', 228, 216.5, 201,155,203, 'CRN'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Ladinian', 237, 228.0, 201,131,191, 'LAD'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Anisian', 245, 237.0, 188,117,183, 'ANS'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Olenekian', 249.7, 245.0, 176,81,165, 'OLK'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Induan', 251, 249.7, 164,70,159, 'IND'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Changhsingian', 253.8, 251.0, 252,192,178, 'CHA'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Wuchiapingian', 260.4, 253.8, 252,180,162, 'WUC'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Capitanian', 265.8, 260.4, 251,154,133, 'CAP'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Wordian', 268, 265.8, 251,141,118, 'WOR'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Roadian', 270.6, 268.0, 251,128,105, 'ROD'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Kungurian', 275.6, 270.6, 227,135,118, 'KUN'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Artinskian', 284.4, 275.6, 227,123,104, 'ART'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Sakmarian', 294.6, 284.4, 227,111,92, 'SAK'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Asselian', 299, 294.6, 227,99,80, 'ASS'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Gzhelian', 303.9, 299.0, 204,212,199, 'GZE'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Kasimovian', 306.5, 303.9, 191,208,197, 'KAS'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Moscovian', 311.7, 306.5, 199,203,185, 'MOS'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Bashkirian', 318.1, 311.7, 153,194,181, 'BSK'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Serpukhovian', 326.4, 318.1, 191,194,107, 'SPK'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Visean', 345.3, 326.4, 166,185,108, 'VIS'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Tournaisian', 359.2, 345.3, 140,176,108, 'TOU'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Famennian', 374.5, 359.2, 242,237,197, 'FAM'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Frasnian', 385.3, 374.5, 242,237,173, 'FRS'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Givetian', 391.8, 385.3, 241,225,133, 'GIV'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Eifelian', 397.5, 391.8, 241,213,118, 'EIF'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Emsian', 407, 397.5, 229,208,117, 'EMS'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Pragian', 411.2, 407.0, 229,196,104, 'PRA'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Lochkovian', 416, 411.2, 229,183,90, 'LOK'));
  FAges.Add(TGeologicTime.Create(tstAge, ' ', 418.7, 416.0, 230,245,225, ''));
  FAges.Add(TGeologicTime.Create(tstAge, 'Ludfordian', 421.3, 418.7,  217,240,223, 'LDF'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Gorstian', 422.9, 421.3, 204,236,221, 'GOR'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Homerian', 426.2, 422.9, 204,235,209, 'HOM'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Sheinwoodian', 428.2, 426.2, 191,230,195, 'SHE'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Telychian', 436, 428.2, 191,230,207, 'TEL'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Aeronian', 439, 436.0, 179,225,194, 'AER'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Rhuddanian', 443.7, 439.0, 166,220,181, 'RHU'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Hirnantian', 445.6, 443.7, 166,219,171, 'HIR'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Katian', 455.8, 445.6, 153,214,159, 'KAT'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Sandbian', 460.9, 455.8, 140,208,148, 'SND'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Darriwilian', 468.1, 460.9, 116,198,156, 'DAR'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Stage 3', 471.8, 468.1, 102,192,146, 'ST3'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Floian', 478.6, 471.8, 65,176,135, 'FLO'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Tremadocian', 488.3, 478.6, 51,169,126, 'TRE'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Stage 10', 492, 488.3, 230,245,201, 'ST10'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Stage 9', 496, 492.0, 217,240,187, 'ST9'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Paibian', 501, 496.0, 204,235,174, 'PAI'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Stage 7', 503, 501.0, 204,223,170, 'ST7'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Drumian', 506.5, 503.0, 191,217,157, 'DRU'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Stage 5', 510, 506.5, 179,212,146, 'ST5'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Stage 4', 517, 510.0, 179,202,142, 'ST4'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Stage 3', 521, 517.0, 166,197,131, 'ST3'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Stage 2', 534.6, 521.0, 166,186,128, 'ST2'));
  FAges.Add(TGeologicTime.Create(tstAge, 'Fortunian', 542, 534.6, 153,181,117, 'FOR'));
end;

function TCompositeGeologicTime.FindFirstAndLast(var First: Integer; var Last: Integer; const min: Double; const max: Double; const aType: TTimespanType): Boolean;
var
  i: Integer;
  aList: TList;
  aTime: TGeologicTime;
begin
  Result := False;

  { grab the appropriate list to search}
  case aType of
    tstEon: aList := FEons;
    tstEra: aList := FEras;
    tstPeriod: aList := FPeriods;
    tstEpoch: aList := FEpochs;
    tstAge: aList := FAges;
  end;
   { find the first one}
  First := 0;
  for i := 0 to aList.Count - 1 do
  begin
    aTime := TGeologicTime(aList[i]);
    if (min >= aTime.EndMya) and (min < aTime.StartMya) then
    begin
      Result := True;
      break;
    end
    else
      inc(First);
    //if aTime.EndMya > min then
    //  inc(First)
    //else
    //begin
    //  Result := True;
    //  break;
    //end;
  end;

  { find the last one}
  Last := aList.Count - 1;
  for i := aList.Count - 1 downto 0 do
  begin
    aTime := TGeologicTime(aList[i]);
    if aTime.EndMya > max then
      dec(Last)
    else
    begin
      Result := Result and True;
      break;
    end;
  end;
  if Last < 0 then
    Last := 0;
end;

constructor TCompositeGeologicTime.Create;
begin
  FEons:= TList.Create;
  FEras:= TList.Create;
  FPeriods:= TList.Create;
  FEpochs:= TList.Create;
  FAges:= TList.Create;
  InitTimescales;
end;

destructor TCompositeGeologicTime.Destroy;
var
  i: Integer;
begin
  if Assigned(FEons) then
  begin
    if FEons.Count > 0 then
      for i := 0 to FEons.Count - 1 do
        TGeologicTime(FEons[i]).Free;
    FEons.Free;
  end;

  if Assigned(FEras) then
  begin
    if FEras.Count > 0 then
      for i := 0 to FEras.Count - 1 do
        TGeologicTime(FEras[i]).Free;
    FEras.Free;
  end;

  if Assigned(FPeriods) then
  begin
    if FPeriods.Count > 0 then
      for i := 0 to FPeriods.Count - 1 do
        TGeologicTime(FPeriods[i]).Free;
    FPeriods.Free;
  end;

  if Assigned(FEpochs) then
  begin
    if FEpochs.Count > 0 then
      for i := 0 to FEpochs.Count - 1 do
        TGeologicTime(FEpochs[i]).Free;
    FEpochs.Free;
  end;

  if Assigned(FAges) then
  begin
    if FAges.Count > 0 then
      for i := 0 to FAges.Count - 1 do
        TGeologicTime(FAges[i]).Free;
    FAges.Free;
  end;

  inherited Destroy;
end;

procedure TCompositeGeologicTime.InitTimescales;
begin
  InitEons;
  InitEras;
  InitPeriods;
  InitEpochs;
  InitAges;
end;

function TCompositeGeologicTime.GetTimescale(aStartTime: Double; aType: TTimespanType): TArrayOfGeologicTime;
var
  aList: TList;
  i: Integer;
  aGeoTime: TGeologicTime;
begin
  case aType of
  tstEon: aList := FEons;
  tstEra: aList := FEras;
  tstPeriod: aList := FPeriods;
  tstEpoch: aList := FEpochs;
  tstAge: aList := FAges;
  else
    raise Exception.Create('invalid geologic timescale');
  end;

  SetLength(Result, 0);
  for i := 0 to aList.Count - 1 do
  begin
    aGeoTime := TGeologicTime(aList[i]);
    if aGeoTime.EndMya <= aStartTime then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := aGeoTime;
    end
    else
      break;
  end;
end;

function TCompositeGeologicTime.GetTimescale(minTime, maxTime: Double; aType: TTimespanType): TArrayOfGeologicTime;
var
  aList: TList;
  i: Integer;
  aTime: TGeologicTime;
  first, last: Integer;
begin
  case aType of
  tstEon: aList := FEons;
  tstEra: aList := FEras;
  tstPeriod: aList := FPeriods;
  tstEpoch: aList := FEpochs;
  tstAge: aList := FAges;
  else
    raise Exception.Create('invalid geologic timescale');
  end;

  SetLength(Result, 0);
  if not FindFirstAndLast(first, last, minTime, maxTime, aType) then
    raise Exception.Create('invalid geologic times given');

  for i := first to last do
  begin
    aTime := TGeologicTime(aList[i]);
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := aTime;
  end;
end;

function TCompositeGeologicTime.GetLongestName(aStartTime: Double; aType: TTimespanType): String;
var
  i: Integer;
  aGeoTime: TGeologicTime;
  aList: TList;
begin
  case aType of
  tstEon: aList := FEons;
  tstEra: aList := FEras;
  tstPeriod: aList := FPeriods;
  tstEpoch: aList := FEpochs;
  tstAge: aList := FAges;
  end;

  Result := EmptyStr;
  for i := 0 to aList.Count - 1 do
  begin
    aGeoTime := TGeologicTime(aList[i]);
    if aGeoTime.EndMya <= aStartTime then
    begin
      if Length(aGeoTime.Name) > Length(Result) then
        Result:= aGeoTime.Name;
    end
    else
      break;
  end;
end;

function TCompositeGeologicTime.GetLongestName(minTime, maxTime: Double; aType: TTimespanType): String;
var
  aList: TList;
  i: Integer;
  aTime: TGeologicTime;
  first, last: Integer;
begin
  Result := EmptyStr;
  case aType of
  tstEon: aList := FEons;
  tstEra: aList := FEras;
  tstPeriod: aList := FPeriods;
  tstEpoch: aList := FEpochs;
  tstAge: aList := FAges;
  else
    raise Exception.Create('invalid geologic timescale');
  end;

  if not FindFirstAndLast(first, last, minTime, maxTime, aType) then
    raise Exception.Create('invalid geologic times given');

  for i := first to last do
  begin
    aTime := TGeologicTime(aList[i]);
    if Length(aTime.Name) > Length(Result) then
      Result := aTime.Name;
  end;
end;

{ TGeologicTime }

function TGeologicTime.GetBestFitText(aRect: TRect; Orientation: TTextOrientation=tdHorizontal): String;
var
  aWidth: Integer;
begin
  if FAbbr = EmptyStr then
  begin
    Result := Name;
    Exit;
  end;
  case Orientation of
    tdVertical:
      begin
        aWidth := aRect.Bottom - aRect.Top
      end;
    tdHorizontal:
      begin
        aWidth := aRect.Right - aRect.Left;
      end
    else
      begin
        Assert(False, 'missing text direction handler');
      end;
  end;
  if CustomTextWidth(Name) < aWidth then
    Result := Name
  else
    Result := FAbbr;
end;

function TGeologicTime.GetHexaDecimalColorStr: String;
var
  gray: Integer;
begin
  if IsStudyTree then
  begin
    gray := Round(0.21*FRed + 0.72*FGreen + 0.07*FBlue);
    Result := '#' + IntToHex(gray, 2) + IntToHex(gray, 2) + IntToHex(gray, 2);
  end
  else
    Result := '#' + IntToHex(FRed, 2) + IntToHex(FGreen, 2) + IntToHex(FBlue, 2);
end;

constructor TGeologicTime.Create(aType: TTimespanType; aName: String;
  aStart, aEnd: Double; aRed, aGreen, aBlue: Integer; Abbr: String='');
begin
  FTimespanType := aType;
  FName := aName;
  FStartMya := aStart;
  FEndMya := aEnd;
  FRed := aRed;
  FBlue := aBlue;
  FGreen := aGreen;
  FAbbr := Abbr;
end;

destructor TGeologicTime.Destroy;
begin
  inherited Destroy;
end;

end.

