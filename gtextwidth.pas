unit gtextwidth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function CustomTextWidth(aText: String; CharWidth: Integer=8): Integer;
function CustomTextHeight(aText: String; CharHeight: Integer=16): Integer;

implementation

function CustomTextWidth(aText: String; CharWidth: Integer=8): Integer;
begin
  Result := Length(aText) * CharWidth; { TODO 1 -oglen -cmegatt : implement customtextwidth }
end;

function CustomTextHeight(aText: String; CharHeight: Integer=16): Integer;
begin
  Result := CharHeight; { TODO 1 -oglen -cmegatt : implement CustomTextHeight }
end;

end.

