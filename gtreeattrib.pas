unit gtreeattrib;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses

  Types, SysUtils, Classes,
  Math, ttconst;

type
  TBranchOption = (boFullBranch, boHalfBranch, boNoBranch, boBranchOnly);
  TBracketStyle = (brsSquare, brsLine, brsNone);
  TGraphicAlign = (gaLeft, gaRight, gaTop, gaBottom);
  TPenStyle = (psSolid);


  TNodeMarker = record
    Shape: TNodeMarkerShape;
    //Color: TColor;
  end;

  TNodeMarkerArray = array of TNodeMarker;


  { TNodeAttrib }

  TNodeAttrib = class
    NodeIndex: integer;
    Compressed: boolean;

    Caption: AnsiString;
    Marker: TNodeMarker;
    OverwriteMarker: boolean;
    OverwriteDownstream: boolean;
    GraphicAlign: TGraphicAlign;
    Name: AnsiString;
  private
    FCaptionFontHeight: Integer;
    FFontHeight: Integer;
    FLineWidth: Integer;
    FBracketLineWidth: Integer;
    //FLinePen: TPen;
    //FBracketPen: TPen;
    //FFont: TFont;
    //FCaptionFont: TFont;
    //FBrush: TBrush;

    FShowTaxonName: boolean;
    FShowSpeciesName: boolean;
    FShowTaxonMarker: boolean;
    FShowNodeMarker:  boolean;
    FShowCaption: boolean;
    FShowBracket: boolean;
    FShowImage: boolean;
    FBranchOption: TBranchOption;
    FBracketStyle: TBracketStyle;


    FImageScaled: boolean;

    //procedure SetFont(newfont: TFont);
    //procedure SetCaptionFont(newfont: TFont);
    //procedure SetLineColor(value: TColor);
    //function GetLineColor: TColor;
    procedure SetLineWidth(value: integer);
    function GetLineWidth:integer;
    //procedure SetLineStyle(value: TPenStyle);
    //function GetLineStyle: TPenStyle;
    procedure SetBracketLineWidth(value: integer);
    function GetBracketLineWidth:integer;
    //procedure SetBracketLineColor(value: TColor);
    //function GetBracketLineColor: TColor;
    //procedure SetFillStyle(value: TBrushStyle);
    //function GetFillStyle: TBrushStyle;
    //function GetBoldFont: TFont;

  public

    IsJPEG: boolean;
    CaptionSize: integer;
    //property LinePen: TPen read FLinePen write FLinePen;
    //property Brush: TBrush read FBrush write FBrush;
    //property Font: TFont read FFont write SetFont;
    //property BoldFont: TFont read GetBoldFont;
    //property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionFontHeight: Integer read FCaptionFontHeight write FCaptionFontHeight;
    property FontHeight: Integer read FFontHeight write FFontHeight;

    //property LineColor: TColor read GetLineColor write SetLineColor;
    property LineWidth: integer read FLineWidth write SetLineWidth;
    //property LineStyle: TPenStyle read GetLineStyle write SetLineStyle;
    //property FillStyle: TBrushStyle read GetFillStyle write SetFillStyle;

    property BracketStyle: TBracketStyle read FBracketStyle write FBracketStyle;
    property BracketLineWidth: integer read FBracketLineWidth write SetBracketLineWidth;
    //property BracketColor: TColor read GetBracketLineColor write SetBracketLineColor;
    property BranchOption: TBranchOption read FBranchOption write FBranchOption;

    property ShowTaxonName: boolean read FShowTaxonName write FShowTaxonName;
    property ShowSpeciesName: boolean read FShowSpeciesName write FShowSpeciesName;
    property ShowTaxonMarker: boolean read FShowTaxonMarker write FShowTaxonMarker;

    property ShowCaption: boolean read FShowCaption write FShowCaption;
    property ShowBracket: boolean read FShowBracket write FShowBracket;
    property ShowNodeMarker: boolean read FShowNodeMarker write FShowNodeMarker;
    property ShowImage: boolean read FShowImage write FShowImage;
    property ImageScaled: boolean read FImageScaled write FImageScaled;
    //property Image: TBitmap read Bitmap;

    //procedure AssignBitmap(image: TBitmap);
    //procedure AssignJPEG(image: TJPEGImage);
    function IsImage: boolean;

    procedure Assign(Source: TNodeAttrib);
    procedure AssignGraphAttrib(Source: TNodeAttrib);
    function ShowLabel: boolean;

    constructor Create;
    destructor Destroy; override;
  end;

  TNodeAttribList = class(TList)
  private
    function GetItems(Index:integer): TNodeAttrib;
    procedure SetItems(Index:integer; AItem: TNodeAttrib);
  public
    property Items[index: integer]:TNodeAttrib read GetItems write SetItems; default;
  end;

  TArrayOfNodeAttribList = array[0..(MaxInt div SizeOf(Pointer))-1] of TNodeAttribList;
  PArrayOfNodeAttribList = ^TArrayOfNodeAttribList;


implementation


////////////////////////////////////
//// TNodeAttrib
////////////////////////////////////

constructor TNodeAttrib.Create;
begin
  inherited Create;
  FLineWidth := 1;
  //FLinePen := TPen.Create;
  //FBracketPen := TPen.Create;
  //FFont := TFont.Create;
  //FCaptionFont := TFont.Create;
  //FFont.Size := 8;
  //FCaptionFont.Size := 8;
  //FBrush := TBrush.Create;
  //Bitmap := TBitmap.Create;
  //JPEGImage   := TJPEGImage.Create;
  FCaptionFontHeight := 8;
  FFontHeight := 8;
  GraphicAlign := gaRight;
  NodeIndex := 0;
  Compressed := false;
  FShowTaxonName   := true;
  FShowSpeciesName := False;
  FShowTaxonMarker := true;
  FShowNodeMarker  := true;
  FShowCaption     := true;
  FShowBracket     := true;
  FShowImage       := true;
  FBranchOption    := boHalfBranch;
  Marker.Shape := msNone;
  //Marker.Color := clBlack;
  IsJPEG := false;
end;

destructor TNodeAttrib.Destroy;
begin
  //JPEGImage.Free;
  //Bitmap.Free;
  //FBrush.Free;
  //FFont.Free;
  //FCaptionFont.Free;
  //FBracketPen.Free;
  //FLinePen.Free;
  inherited;
end;

//procedure TNodeAttrib.SetFont(newfont: TFont);
//begin
//  FFont.Assign(newfont);
//end;
//
//procedure TNodeAttrib.SetCaptionFont(newfont: TFont);
//begin
//  FCaptionFont.Assign(newfont);
//end;
//
//procedure TNodeAttrib.SetLineColor(value: TColor);
//begin
//  FLinePen.Color := value;
//  FBrush.Color := value;
//end;
//
//function TNodeAttrib.GetLineColor: TColor;
//begin
//  Result := FLinePen.Color;
//end;

procedure TNodeAttrib.SetLineWidth(value: integer);
begin
  FLineWidth := value;
  //FLinePen.Width := value;
end;

function TNodeAttrib.GetLineWidth:integer;
begin
  Result := FLineWidth;
  //Result := FLinePen.Width;
end;

//procedure TNodeAttrib.SetLineStyle(value: TPenStyle);
//begin
//  FLinePen.Style := value;
//end;
//
//function TNodeAttrib.GetLineStyle: TPenStyle;
//begin
//  Result := FLinePen.Style;
//end;

procedure TNodeAttrib.SetBracketLineWidth(value: integer);
begin
  FBracketLineWidth := value;
end;

function TNodeAttrib.GetBracketLineWidth:integer;
begin
  Result := FBracketLineWidth;
end;

//procedure TNodeAttrib.SetBracketLineColor(value: TColor);
//begin
//  FBracketPen.Color := value;
//end;
//
//function TNodeAttrib.GetBracketLineColor: TColor;
//begin
//  Result := FBracketPen.Color;
//end;

//procedure TNodeAttrib.SetFillStyle(value: TBrushStyle);
//begin
//  FBrush.Style := value;
//end;
//
//function TNodeAttrib.GetFillStyle: TBrushStyle;
//begin
//  Result := FBrush.Style;
//end;

procedure TNodeAttrib.Assign(Source: TNodeAttrib);
begin
    NodeIndex    := Source.NodeIndex;
    Compressed   := Source.Compressed;
    Caption      := Source.Caption;
    Marker       := Source.Marker;
    IsJPEG       := Source.IsJPEG;
    GraphicAlign := Source.GraphicAlign;
    //Bitmap.Assign(Source.Bitmap);
    //JPEGImage.Assign(Source.JPEGImage);

    //FLinePen.Assign(Source.FLinePen);
    //FBracketPen.Assign(Source.FBracketPen);
    //FFont.Assign(Source.FFont);
    //FCaptionFont.Assign(Source.FCaptionFont);
    //FBrush.Assign(Source.FBrush);

    FShowTaxonName   := Source.FShowTaxonName;
    FShowSpeciesName := Source.FShowSpeciesName;
    FShowTaxonMarker := Source.FShowTaxonMarker;
    FShowNodeMarker  := Source.FShowNodeMarker;
    FShowCaption     := Source.FShowCaption;
    FShowBracket     := Source.FShowBracket;
    FShowImage       := Source.FShowImage;
    FBranchOption    := Source.FBranchOption;
    FBracketStyle    := Source.FBracketStyle;

    OverwriteMarker  := Source.OverwriteMarker;
    OverwriteDownstream  := Source.OverwriteDownstream;
end;

procedure TNodeAttrib.AssignGraphAttrib(Source: TNodeAttrib);
begin
    GraphicAlign := Source.GraphicAlign;

    //FLinePen.Assign(Source.FLinePen);
    //FBracketPen.Assign(Source.FBracketPen);
    //FFont.Assign(Source.FFont);
    //FCaptionFont.Assign(Source.FCaptionFont);
    //FBrush.Assign(Source.FBrush);

    FShowTaxonName   := Source.FShowTaxonName;
    FShowSpeciesName := Source.FShowSpeciesName;
    FShowTaxonMarker := Source.FShowTaxonMarker;
    FShowNodeMarker  := Source.FShowNodeMarker;
    FShowCaption     := Source.FShowCaption;
    FShowBracket     := Source.FShowBracket;
    FShowImage       := Source.FShowImage;
    FBranchOption    := Source.FBranchOption;
    FBracketStyle    := Source.FBracketStyle;
end;

function TNodeAttrib.IsImage: boolean;
begin
  Result := False;
end;

function TNodeAttrib.ShowLabel: boolean;
begin
  if Caption = '' then
    result := false
  else if Compressed then
    result := FShowTaxonName
  else
    result := FShowCaption;
end;

//////////////////////
//  TNodeAttribList
//////////////////////

function TNodeAttribList.GetItems(Index:integer): TNodeAttrib;
begin
  result := inherited Items[Index];
end;

procedure TNodeAttribList.SetItems(Index:integer; AItem: TNodeAttrib);
begin
  inherited Items[Index] := AItem;
end;

end.
