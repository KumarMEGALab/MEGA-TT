unit gsvgtreebox;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
 {$M+}
interface

uses

  SysUtils, Classes,
  Math, gtreeattrib, gtreenode, gtextwidth, mgeodatachartformatter,
  ttconst, gutils, gtreedata, gtreelist, geologicaltimes, mearthimpacts,
  mgeodata, gtaxonomicrank,MLongintList;

const
  MAX_BITMAP_DIMENSION = 4194304;
  MAX_TAXA_STRING = '(100,000)';
  FX_BASE_DEFAULT = 20;

type
  TFontTarget = (tftOtu, tftCharState, tftScale, tftStats, tftimes, tftCaption, tftNode);
  TCustomSvgTree = class;

  TSvgNodeInfo = class
  private
    Tree: TCustomSvgTree;

    function GetIndex: integer;
    function GetAncIndex: integer;
    function GetDes1Index: integer;
    function GetDes2Index: integer;
    function GetNodeType: TNodeType;
    function GetHeight: double;
    function GetIsOtu: Boolean;
    function GetIsOutgroup: Boolean;
  public
    property Index: integer read GetIndex;
    property AncIndex: integer read GetAncIndex;
    property Des1Index: integer read GetDes1Index;
    property Des2Index: integer read GetDes2Index;
    property NodeType: TNodeType read GetNodeType;
    property Height: double read GetHeight;
    property IsOtu: Boolean read GetIsOtu;
    property IsOutgroup: Boolean read GetIsOutgroup;
    constructor Create(Source: TCustomSvgTree);
    destructor Destroy; override;
  end;

  TSvgBranchInfo = class
  private
    Tree: TCustomSvgTree;

    function GetNodeIndex: integer;
    function GetAncNodeIndex: integer;
    function GetBranchType : TBranchType;
    function GetLength : double;
    function GetTotalLength : double;
    function GetSE : double;
    function GetMaxLen1 : double;
    function GetMaxLen2 : double;
    function GetStats : double;
  public
    property NodeIndex: integer read GetNodeIndex;
    property AncNodeIndex: integer read GetAncNodeIndex;
    property BranchType : TBranchType read GetBranchType;
    property Length : double read GetLength;
    property TotalLength : double read GetTotalLength;
    property SE : double read GetSE;
    property MaxLen1 : double read GetMaxLen1;
    property MaxLen2 : double read GetMaxLen2;
    property Stats : double read GetStats;

    constructor Create(Source: TCustomSvgTree);
    destructor Destroy; override;
  end;



  TLinearizeFunc = function(tree: TTimeTreeData):double of object;
  THeightSEFunc = function(NodeIndex: integer):double of object;

  { TCustomSvgTree }

  TCustomSvgTree = class(TObject)
  public
    AttribList: TNodeAttribList;
    GroupAttrib: TNodeAttribList;
  private
    FSvgTreeImageWidth: Integer;
    FDeepestRank: TTaxonomicRank;
    FDoLogScale: Boolean;
    FGeoTimescaleHeight: Integer;
    FBLensFontHeight: Integer;
    FBranchPenWidth: Integer;
    FCharStateFontHeight: Integer;
    FNameSpacing: Integer;
    FRanksFile: String;
    FShowDivergenceTimes: Boolean;
    FShowNodeIds: Boolean;
    FStatsFontHeight: Integer;
    FTimesFontHeight: Integer;
    FRoot : TpNode;
    FNode : ^TpNodeArray;
    FSBL : double;
    FScale : double;
    FTimeScale : double;

    FAutoSize: Boolean;
    FTreeExist: boolean;
    FTopoflag : boolean;
    FMarkedPos : TPoint;

    FMaxDepth : integer;
    FMinTreeWidth: integer;
    FUserSuppliedATaxaList: Boolean;
    Fxmax : double;
    Fxunit : double;
    Fyunit: Integer;
    Fgunit: Integer;
    Fxbase: Integer;
    Fybase: Integer;
    FMaxCharState : integer;
    FOrigin : TPoint;

    FNoOfOTUs : integer;
    FMaxStats : double;
    FValue : double;
    FValue2 : double;
    FStatsCutoff : integer;
    FBLenCutoff : double;
    FCondenseValue : integer;

    FNameEditingEnabled: Boolean;
    FShowMappedTreeNames: Boolean;

    FBranchFocused : boolean;
    FNodeFocused : boolean;
    FFocusedIndex : integer;
    FBranchInfo: TSvgBranchInfo;
    FNodeInfo: TSvgNodeInfo;

    FFocusedNameIndex: integer;

    FisRooted : boolean;
    FisBranchFixed : boolean;
    FisSE : boolean;
    FisStats : boolean;
    FisTimes : boolean;
    FisValue : boolean;
    FisValue2 : boolean;
    FShowStats : boolean;
    FShowSelection: boolean;
    FFillSubtreeDelta : boolean;
    FStatsPosition : TBranchInfoPosition;
    FTimesPosition : TBranchInfoPosition;
    FStatsMargin : TPoint;
    FTimesMargin : TPoint;
    FShowBLen : boolean;
    FBLenPosition : TBranchInfoPosition;
    FBLenDecimals : integer;
    FValueDecimals : integer;
    FValue2Decimals : integer;
    FShowCharState : boolean;
    FShowScale : boolean;
    FShowTimeScale : boolean;
    FStartAngle : integer;
    FCenterMargin : integer;
    FShowRoot: boolean;

    FShowHeightErrBar: boolean;

    FForceLinearized: boolean;
    FIsLinearized: boolean;
    FIsCondensed: boolean;
    FTopologyOnly: boolean;

    FTreeStyle : TTreeStyle;
    FBranchStyle : TBranchStyle;
    FTreeWidth : integer;
    FMinWidth, FMinHeight: integer;
    FRadius : integer;
    FPixelsPerUnit : double;

    FHorzTaxonName: boolean;
    FAlignCaption: boolean;

    FScaleText : AnsiString;
    FTimeText : AnsiString;
    FScaleUnit : AnsiString;
    FTimeUnit : AnsiString;
    FScaleTick : double;
    FTimeTick : double;
    FSource : AnsiString;
    FTitle : AnsiString;
    FDescription : AnsiString;
    FStatsName : AnsiString;
    FValueName : AnsiString;
    FValue2Name : AnsiString;

    FDistance : AnsiString;
    FGap: AnsiString;
    FMethod : AnsiString;
    FTestMethod : AnsiString;

    FDistanceMatrix : PDistanceMatrix;

    FUseSubtreeAttrib: boolean;
    FUseGroupAttrib: boolean;

    FLinearizeFunc: TLinearizeFunc;
    FHeightSEFunc:  THeightSEFunc;

    procedure InitMem; dynamic;
    procedure ResetMem; dynamic;
    procedure InitTree; dynamic;
    procedure SetCharStateFontHeight(AValue: Integer);
    procedure SetDoLogScale(AValue: Boolean);
    procedure SetFUserSuppliedATaxaList(AValue: Boolean);
    procedure SetNameSpacing(AValue: Integer);
    procedure SetStats; dynamic;
    procedure SetRootAtLast;
    procedure SetPosition;
    procedure SetTreeSize;
    procedure SetClusterHeight;
    procedure DrawPolygon(Points: array of TPoint; NumPoints: Integer);

    procedure DrawTree(HMF : THandle); virtual;
    procedure MoveRoot(p: TpNode; midpoint: boolean); dynamic;
    function TaxaNamesColumnWidth: Integer;
    function GetNoOfNodes:integer;
    function SearchMidPoint:integer;
    function GetIsOutgroup:boolean;
    function GetOutgroup(index : integer):boolean;
    procedure SetOutgroup(index : integer; value : boolean);
    function GetCurrentOutgroup(index : integer):boolean;
    procedure SetDistanceMatrix(value: PDistanceMatrix);

    function GetTreeWidth:integer;
    procedure SetTreeWidth(w : integer);
    function TrySetTreeWidth(w: Integer): Boolean;

    procedure SetClusterWidth(p: TpNode);
    function GetTreeHeight:integer;
    function GetCurNoOfOTUs:integer;

    function GetLongestPath:double;
    function GetMinBranchLength:double;
    function GetMaxBranchLength:double;

    procedure SetIsCondensed(value: boolean);
    procedure SetCondenseValue(value : integer);
    procedure SetTopologyOnly(value : boolean); virtual;
    function GetShowOTUName: boolean;
    procedure SetShowOTUName(b : boolean);
    function GetShowOTUMarker: boolean;
    procedure SetShowOTUMarker(b : boolean); virtual;
    procedure SetShowBLen(b : boolean);
    procedure SetStatsCutoff(value : integer);
    procedure SetBLenCutoff(value : double);
    procedure InitScale;
    procedure SetScaleText(text : AnsiString);
    procedure SetTimeText(text : AnsiString);
    function GetScaleDecimals: integer;
    function GetNameFocused:boolean;
    function GetOTUName(i : integer):AnsiString;
    function GetAncestorName(i : integer):AnsiString;  // Used in Ancestral Sequences, supplies a name if applicable or the two decendants of the node. "(des1, des2)"
    function GetOTUOrigionalName(i: integer): AnsiString;
    function GetIndexOfOrigionalName(OrigName: AnsiString): Integer;
    function GetIndexOfName(Name: AnsiString): Integer;
    function GetClusterName(i : integer):AnsiString;
    function GetGroupName(i : integer):AnsiString;
    procedure SetClusterName(index: integer; name:AnsiString);
    procedure ChangeOTUName(Sender : TObject); virtual;
    function GetCharState(Index : integer):AnsiString;
    procedure SetClusterHeightFromDistances;
    procedure SetClusterHeightByBranchLengths;
    procedure SetClusterHeightByLinearizeFunc;
    procedure SetStatsPosition(position : TBranchInfoPosition);
    procedure SetTimesPosition(Position : TBranchInfoPosition);
    procedure SetStatsMargin(margin : TPoint);
    procedure SetTimesMargin(Margin: TPoint);
    procedure SetBLenPosition(position : TBranchInfoPosition);
    procedure SetBLenDecimals(value : integer);
    procedure SetPixelsPerUnit(value : double);
    procedure SetPixelsPerOTU(value : integer);
    function GetPixelsPerOTU:integer;
    procedure SetPixelsPerGroupMember(value : integer);
    function GetPixelsPerGroupMember:integer;
    procedure SetBranchStyle(value : TBranchStyle);
    procedure SetTreeStyle(value : TTreeStyle);
    procedure SetMarker(index : integer; newstyle : TNodeMarker{TMarkerItem});
    function GetMarker(index : integer):TNodeMarker; //TMarkerItem;
    function GetTopologyOnly: boolean;
    function GetIsCondensed: boolean;

    function GetIsDistanceMatrix:boolean;
    function GetIsScale:boolean;
    function GetIsTimeScale:boolean;
    function GetIsBranchLength:boolean; virtual;
    function GetIsSE:boolean; virtual;
    function GetIsStats:boolean; virtual;
    function GetIsTimes: Boolean; virtual;
    function GetShowBLen:boolean;
    procedure SetFillSubtreeDelta(value: boolean);

    procedure Draw; virtual;

    function  FocusNode(x, y : Int64):boolean;
    function  FocusBranch(x, y : integer):boolean;
    function  FocusName(x, y : integer):boolean;
    procedure ClearAttrib;

    procedure SetGroupIndex;

    procedure SetUseSubtreeAttrib(value: boolean);
    procedure SetUseGroupAttrib(value: boolean);

    procedure SetForceLinearized(value: boolean);
    function GetLineWidth(p: TpNode): integer;
    function GetNodeAttrib(p: TpNode): TNodeAttrib;
    function GetTaxonMarker(p: TpNode): TNodeMarker;

    procedure SetCenterMargin(value: integer);

    procedure SetIsLinearized(value: boolean);
    function GetCoords(i: integer): AnsiString;  // Gives the "name" in a coordinate way "(des1, des2)"
    function GetAncestorNodeNumber(i: integer): integer;
    function GetNumCalibrations: Integer;
    function HasCalibrations: Boolean;
    function FindAncestorName(aNode: TpNode): String;
    procedure MegaPolyline(Points: array of TPoint; NumPoints: Integer; aNode: TpNode=nil);
    procedure MegaPolygon(Points: array of TPoint; NumPoints: Integer; aNode: TpNode=nil);
    procedure MegaTextOutA(aX: Integer; aY: Integer; aText: PAnsiChar); overload;
    procedure MegaTextOutA(aX: Integer; aY: Integer; aText: String); overload;
    function SvgOtuName(aX: Integer; aY: Integer; aText: String; aNode: TpNode): String;
  protected
    FIsMobileFriendly: Boolean;
    FCircleTags: TStringList;
    FTaxaNameTags: TStringList;
    FRenderNewickOnly: Boolean;
    FTreeSvgFile: TextFile;
    FPanelsSvgFile: TextFile;
    FSvgLineWidth: Integer;
    FSvgLineColor: String;
    FSvgDisabledNodeColor: String;
    FSvgTimedNodeColor: String;
    FSvgBgColor: String;
    FSvgFontColor: String;
    FSvgFontHeight: Integer;
    FSvgFont: String;
    FFontTarget: TFontTarget;
    FScaleFontHeight: Integer;
    procedure CompressCluster(index: integer);
    procedure ExpandCluster(index: integer);
    procedure ExpandAllCluster(index: integer);
    function GetShowCharState:boolean;
    procedure SetFontTarget(aTarget: TFontTarget);
    function CurrentFontHeight: Integer;
    procedure CountLeaves;
  public
    StudyTimeNodeHeights: TDoubleArray;
    HideOverlappingTaxa: Boolean;
    DivTimeDecimals: Integer;
    function ShowEpochsAndAges: Boolean;
    procedure SetIsRooted(const AValue: Boolean);
    function GetTaxonName(Taxon: Integer): String;
    { When the user applies a calibration, we make a copy of the TCalibrationTime
      instance for saving to a session file. SetDivergenceTime makes the copy}
    procedure GetIntCoords(i: integer; var des1, des2: integer);
    procedure SetAttrIndex; // Moved to public since we need to call this after any font change and when simply changing font size the SetOTUFont does NOT get called, thus we must call it manually.
    Function OTUCoords(Index: Integer): TRect;  // Moved for accessability.
    function NodeCoords(NodeIndex: Integer): TRect;
    property Source : AnsiString read FSource;
    property Title : AnsiString read FTitle;
    property Description : AnsiString read FDescription;
    property Distance : AnsiString read FDistance;
    property Gap : AnsiString read FGap;
    property Method : AnsiString read FMethod;
    property TestMethod : AnsiString read FTestMethod;
    property IsLinearized : boolean read FIsLinearized write SetIsLinearized;
    property IsCondensed : boolean read GetIsCondensed write SetIsCondensed;
    property AncestorNodeNumber[index : integer] : integer read GetAncestorNodeNumber;
    property TreeExist: boolean read FTreeExist;
    property DeepestRank: TTaxonomicRank read FDeepestRank write FDeepestRank;
    property RanksFile: String read FRanksFile write FRanksFile;

    property NoOfOTUs : integer read FNoOfOTUs;
    property CurrentNoOfOTUs : integer read GetCurNoOfOTUs;
    property NoOfNodes: integer read GetNoOfNodes;

    property StatsName : AnsiString read FStatsName;
    property MaxStats : double read FMaxStats;
    property ValueName : AnsiString read FValueName;
    property Value: double read FValue write FValue;
    property ValueDecimals: integer read FValueDecimals write FValueDecimals;
    property IsValue: boolean read FisValue;
    property Value2Name: AnsiString read FValue2Name;
    property Value2: double read FValue2;
    property Value2Decimals: integer read FValue2Decimals write FValue2Decimals;
    property IsValue2: boolean read FisValue2 write FisValue2;
    property IsRooted: boolean read FisRooted write FIsRooted;
    property IsBranchFixed: boolean read FisBranchFixed;
    property IsBranchLength: boolean read GetIsBranchLength;
    property IsSE: boolean read GetIsSE;
    property IsStats: boolean read GetIsStats;
    property IsScale: boolean read GetIsScale;
    property IsTimeScale: boolean read GetIsTimeScale;
    property IsOutgroup: boolean read GetIsOutgroup;
    property IsDistanceMatrix : boolean read GetIsDistanceMatrix;
    property Outgroup[Index: integer] : boolean read GetOutgroup write SetOutgroup;
    property CurrentOutgroup[Index: integer] : boolean read GetCurrentOutgroup;
    property LongestPath : double read GetLongestPath;
    property MinBranchLength : double read GetMinBranchLength;
    property MaxBranchLength : double read GetMaxBranchLength;
    property CondenseValue : integer read FCondenseValue write SetCondenseValue;
    property DistanceMatrix : PDistanceMatrix read FDistanceMatrix write SetDistanceMatrix;

    property OTUName[index : integer] : AnsiString read GetOTUName;
    property AncestorName[index : integer] : AnsiString read GetAncestorName;
    property CoordsName[index : integer] : AnsiString read GetCoords;
    property OTUOrigionalName[index : integer] : AnsiString read GetOTUOrigionalName;
    property IndexOfOrigionalName[OrigionalName: AnsiString] : integer read GetIndexOfOrigionalName;
    property IndexOfName[Name: AnsiString] : integer read GetIndexOfName;

    property GroupName[index : integer] : AnsiString read GetGroupName;
    property ClusterName[index : integer] : AnsiString read GetClusterName write SetClusterName;
    property Marker[index : integer] : TNodeMarker read GetMarker write SetMarker;

    property PixelsPerUnit : double read FPixelsPerUnit write SetPixelsPerUnit;
    property TreeHeight : integer read GetTreeHeight;
    property MinHeight : integer read FMinHeight;
    property MinWidth : integer read FMinWidth;

    property StatsCutoff : integer read FStatsCutoff write SetStatsCutoff;
    property BLenCutoff : double read FBLenCutoff write SetBLenCutoff;

    property ScaleText : AnsiString read FScaleText write SetScaleText;
    property ScaleDecimals : integer read GetScaleDecimals;
    property ScaleTick : double read FScaleTick write FScaleTick;
    property ScaleUnit : AnsiString read FScaleUnit write FScaleUnit;
    property TimeText : AnsiString read FTimeText write SetTimeText;
    property TimeTick : double read FTimeTick write FTimeTick;
    property TimeUnit : AnsiString read FTimeUnit write FTimeUnit;

    property NodeFocused : boolean read FNodeFocused;
    property BranchFocused : boolean read FBranchFocused;
    property FocusedIndex : integer read FFocusedIndex;
    property BranchInfo: TSvgBranchInfo read FBranchInfo;
    property NodeInfo: TSvgNodeInfo read FNodeInfo;

    property NameFocused : boolean read GetNameFocused;
    property FocusedNameIndex : integer read FFocusedNameIndex;

    property UseSubtreeAttrib: boolean read FUseSubtreeAttrib write SetUseSubtreeAttrib;
    property UseGroupAttrib: boolean read FUseGroupAttrib write SetUseGroupAttrib;

    property LinearizeFunc: TLinearizeFunc read FLinearizeFunc write FLinearizeFunc;
    property HeightSEFunc:  THeightSEFunc read FHeightSEFunc write FHeightSEFunc;
    procedure FocusOnRoot;
    function IsFocusedOnRoot: Boolean;
    procedure MakeRootOnBranch; dynamic;
    procedure MakeRootOnMidPoint; dynamic;
    procedure MakeRootByOutgroup(OutgroupOnBottom: Boolean=False); dynamic;
    function GetOutgroupAncestorIndex: Integer;
    function GetFirstOutgroupMemberIndex: Integer;
    procedure AddOutgroup(index : integer);
    procedure RemoveOutgroup(index : integer);
    procedure SortClusterInOrder;
    procedure SortClusterForShape;
    procedure FlipCluster; dynamic;
    procedure FlipAllCluster; dynamic;
    procedure Build;
    procedure BuildNoDraw;
    procedure ApplyLogScale;
    function HeightScaledToLog10(h: Double): Integer;
    procedure MapInternalNodeNames;
    procedure MapConfidenceIntervals;
    procedure Refresh; virtual;
    function FindNextFocus(candidates: TList): Integer;
    procedure FocusOnNode(index: integer);
    procedure FocusOnBranch(nodeindex: integer);
    procedure FocusOnName(index: integer);
    procedure MoveFocus(direction : integer);
    function FocusOnPoint(Point: TPoint): Boolean;
    procedure ClearFocus;
    procedure AssignTreeAttrib(Source: TCustomSvgTree); dynamic;
    procedure Clear;
    procedure GetTreeData(tree: TTimeTreeData); virtual;
    procedure GetDescName(nodeindex: integer; names: TStringList);
    function StrLength(TheStr: AnsiString): Integer;
    function StrHeight(TheStr: AnsiString): Integer;
    constructor Create;
    destructor Destroy; override;
    property SvgLineColor: String read FSvgLineColor; { a hexadecimal rgb value as a string}
    property SvgLineWidth: Integer read FSvgLineWidth;
    property StatsMargin : TPoint read FStatsMargin write SetStatsMargin;
    property TimesMargin : TPoint read FTimesMargin write SetTimesMargin;
    property UserSuppliedATaxaList: Boolean read FUserSuppliedATaxaList write SetFUserSuppliedATaxaList;
  published
    property ForceLinearized : boolean read FForceLinearized write SetForceLinearized;
                 // For consistency of UPGMA tree.

    property NameSpacing: Integer read FNameSpacing write SetNameSpacing;
    property Radius : integer read FRadius write FRadius;
    property StartAngle : integer read FStartAngle write FStartAngle;
    property CenterMargin : integer read FCenterMargin write SetCenterMargin;

    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property TreeWidth : integer read GetTreeWidth write SetTreeWidth;
    property PixelsPerGroupMember : integer read GetPixelsPerGroupMember write SetPixelsPerGroupMember;
    property PixelsPerOTU : integer read GetPixelsPerOTU write SetPixelsPerOTU;

    property TreeStyle : TTreeStyle read FTreeStyle write SetTreeStyle;
    property BranchStyle : TBranchStyle read FBranchStyle write SetBranchStyle;
    property FillSubtreeDelta : boolean read FFillSubtreeDelta write SetFillSubtreeDelta;
    property StatsPosition : TBranchInfoPosition read FStatsPosition write SetStatsPosition;

    property BLenPosition : TBranchInfoPosition read FBLenPosition write SetBLenPosition;
    property BLenDecimals : integer read FBLenDecimals write SetBLenDecimals;
    property ShowHeightErrBar: Boolean read FShowHeightErrBar write FShowHeightErrBar;
    property ShowOTUName : boolean read GetShowOTUName write SetShowOTUName;
    property ShowOTUMarker : boolean read GetShowOTUMarker write SetShowOTUMarker;
    property ShowStats : boolean read FShowStats write FShowStats;
    property ShowBLen : boolean read GetShowBLen write SetShowBLen;
    property ShowScale : boolean read FShowScale write FShowScale;
    property ShowTimeScale : boolean read FShowTimeScale write FShowTimeScale;
    property ShowRoot : boolean read FShowRoot write FShowRoot;
    property ShowNodeIds: Boolean read FShowNodeIds write FShowNodeIds;
    property ShowTopologyOnly : boolean read GetTopologyOnly write SetTopologyOnly;
    property HorzTaxonName: boolean read FHorzTaxonName write FHorzTaxonName;
    property AlignCaption: boolean read FAlignCaption write FAlignCaption;
    property ShowCharState: Boolean read GetShowCharState write FShowCharState;

    property CharStateFontHeight: Integer read FCharStateFontHeight write SetCharStateFontHeight;
    property StatsFontHeight: Integer read FStatsFontHeight write FStatsFontHeight;
    property TimesFontHeight: Integer read FTimesFontHeight write FTimesFontHeight;
    property BLensFontHeight: Integer read FBLensFontHeight write FBLensFontHeight;
    property BranchPenWidth: Integer read FBranchPenWidth write FBranchPenWidth;
    property ShowDivergenceTimes: Boolean read FShowDivergenceTimes write FShowDivergenceTimes;
    property IsTimes: Boolean read GetIsTimes write FIsTimes;
    property DoLogScale: Boolean read FDoLogScale write SetDoLogScale;
end;


//////////////////
//  TSvgTreeBox
//////////////////

  TSvgBranchInfoFunc = function(tree: TTimeTreeData):double of object;

  PString = ^AnsiString;
  TArrayOfString = array[0..(MaxInt div SizeOf(PString)-1)] of PString; { max size of a data structure is 2GB}
  PArrayOfString = ^TArrayOfString;

  TAncStateProc = procedure(AncState: PArrayOfString; SiteIndex : integer; tree: TTimeTreeData) of object;
  TAncProbFunc = function(NodeIndex, SiteIndex: integer; tree: TTimeTreeData; Probs: TStringList):double of object;

  TSvgTreeBox = class(TCustomSvgTree)
  private
    FNoOfTrees: integer;
    FTreeIndex: integer;
    FSiteIndex: integer;
    FMaxSiteIndex: integer;
    FConsensusValue: integer;
    FFreqName: AnsiString;
    FFreqDecimals: integer;
    ConsTreeIsValue: boolean;
    ConsTreeIsValue2: boolean;
    ConsCondenseValue: integer;

    TreeList : TTimeTreeList;
    ConsTree : TTimeTreeData;
    ConsCondensed, ConsTopoOnly, ConsRooted: boolean;
    Initialized: PArrayOfBool;
    FDataInitialized: PArrayOfBool;

    FBlenFunc: TSvgBranchInfoFunc;
    FSEFunc: TSvgBranchInfoFunc;
    FStatsFunc: TSvgBranchInfoFunc;
    FValueFunc: TSvgBranchInfoFunc;
    FValue2Func: TSvgBranchInfoFunc;

    FAncStateProc: TAncStateProc;
    FAncProbFunc: TAncProbFunc;
    AncState: PArrayOfString;

    AttribListArray: PArrayOfNodeAttribList;

    function GetIsSE: boolean;
    function GetIsStats: boolean;
    procedure InitMem; override;
    procedure ResetMem; override;
    procedure InitTree; override;
    procedure SetStats; override;
    procedure SetCondenseValue(value : integer);
    function GetCondenseValue: integer;
    procedure SetTreeIndex(index : integer);
    procedure SetOutgroups(outgroup : boolean);
    procedure LoadTreeData(index : integer);
    //procedure InitDivTimes;
    procedure InitTreeData(index : integer);
    procedure SaveTreeData(index : integer);
    procedure SetConsensusValue(value : integer);
    function GetValue(index:integer):double;
    function GetValue2(index:integer):double;
    function GetFrequency(index:integer):double;
    function GetStatsName:AnsiString;
    procedure SetMaxSiteIndex(value: integer);
    function GetIsValue:boolean;
    function GetIsValue2:boolean;
    function GetIsFreq:boolean;
    procedure SetValueFunc(f: TSvgBranchInfoFunc);
    procedure SetValue2Func(f: TSvgBranchInfoFunc);
    function GetIsBranchLength:boolean; override;
    function GetMaxStats:double;
    function GetDataInitialized(index: integer):boolean;
    function GetInformation: TStringList;
    procedure SetTopologyOnly(value : boolean); override;

    procedure MoveRoot(p: TpNode; midpoint: boolean); override;
    procedure SetAttribList;
    procedure DeleteAttrib(index: integer);

    procedure SetShowOTUMarker(b : boolean); override;

    procedure SetValue(index: integer; value: double);
    procedure SetValue2(index: integer; value: double);
  published
    property FreqDecimals: integer read FFreqDecimals write FFreqDecimals;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetInternalNodeLabels(var NodeLabels: TStringList);
    procedure GetTreeData(tree: TTimeTreeData); override;
    procedure AssignTreeList(ATreeList: TTimeTreeList);
    function GetOTUNamesList: TStringlist;
    procedure GetOutgroupTaxa(var AList: TStringList);
    function GetOutgroupInfo: TStringList;
    procedure ClearOutgroups;
    function LargestWidthOfOTUNames: Integer;
    property Value[Index:integer] : double read GetValue write SetValue;
    property Value2[Index:integer] : double read GetValue2 write SetValue2;
    property Frequency[Index:integer] : double read GetFrequency;
    property DataInitialized[index: integer]: boolean read GetDataInitialized;
    procedure AssignTreeAttrib(Source: TCustomSvgTree); override;
    property Information: TStringList read GetInformation;
  published
    property FreqName: AnsiString read FFreqName write FFreqName;
    property NoOfTrees: integer read FNoOfTrees;
    property TreeIndex: integer read FTreeIndex write SetTreeIndex;
    property isValue : boolean read GetIsValue;
    property isValue2 : boolean read GetIsValue2;
    property isFreq: boolean read GetIsFreq;
    property isBranchLength : boolean read GetIsBranchLength;
    property isSE : boolean read GetIsSE;
    property isStats : boolean read GetIsStats;
    property StatsName : AnsiString read GetStatsName;
    property MaxStats: double read GetMaxStats;
    property ConsensusValue: integer read FConsensusValue write SetConsensusValue;
    property CondenseValue: integer read GetCondenseValue write SetCondenseValue;
    property BLenFunc: TSvgBranchInfoFunc read FBLenFunc write FBLenFunc;
    property SEFunc: TSvgBranchInfoFunc read FSEFunc write FSEFunc;
    property StatsFunc: TSvgBranchInfoFunc read FStatsFunc write FStatsFunc;

    property ValueFunc: TSvgBranchInfoFunc read FValueFunc write SetValueFunc;
    property Value2Func: TSvgBranchInfoFunc read FValue2Func write SetValue2Func;

    function GetSubtreeAttrib(NodeAttrib: TNodeAttrib; nodeindex: integer): boolean;
      // return if FNode[nodeindex] is the attribute holder
    procedure SetSubtreeAttrib(NodeAttrib: TNodeAttrib; nodeindex: integer);
    procedure ClearSubtreeAttrib(nodeindex: integer; recursive: boolean);
    procedure ClearAllSubtreeAttrib;
    procedure OverwriteAttribDownstream(nodeindex: integer);
    procedure SetTaxonMarkerOfSubtree(nodeindex: integer; marker: TNodeMarker);

    procedure SetGroupInfo(groupinfo: TStringList);
    procedure GetGroupAttrib(Attrib: TNodeAttrib; GroupName: AnsiString);
    procedure SetGroupAttrib(Attrib: TNodeAttrib; GroupName: AnsiString);
    procedure SetTaxonMarkerOfGroup(GroupName: AnsiString; marker: TNodeMarker);


    function  ImportFromNewickStandard(filename : String; Unroot: Boolean=False):boolean;
    function  ImportFromNewickString(NewickTree : AnsiString):boolean;
    procedure GetSubtree(subtree: TSvgTreeBox);

    function SeTTimeTreeList(Tree: TTimeTreeList; optimize: boolean):boolean;
    procedure SeTTimeTreeListRooted(const AValue: Boolean);

    procedure ExportAllTreesToNewickFile(filename : String);
    procedure ExportCurrentTreeToNewickFile(filename : String);
    function GetCurrentTree: TTimeTreeData;

    function GetNewickTree: AnsiString;
  end;

  { TSvgTreeBox }

  { TMySvgTreeBox }

  TMySvgTreeBox = class(TSvgTreeBox)
    private
      FDoNewick: Boolean;
      FO2Data: TGeoData;
      FCO2Data: TGeoData;
      FLuminosityData: TGeoData;
      FEarthImpacts: TEarthImpacts;
      FTimescaleHeight: Integer;
      FGeoDataPanelHeight: Integer;
      FGeoScaleFontHeight: Integer;
      FPanelHeaderHeight: Integer; { space for writing the panel name}
      FScaleValFontHeight: Integer;
      FPanelsMargin: Integer;
      FGeoDataPanelMargin: Integer;
      FUserSuppliedATaxaList: Boolean;
      FXofRootNode: Integer;
      FXofTaxaNodes: Integer;
      CurAttrib: TNodeAttrib;
      FImageHeight: Integer;
      FImageWidth: Integer;
      dwPenWidth : DWORD;
      FGeoTimescale: TCompositeGeologicTime;
      FGeoDataChartFormatter: TGeoDataChartFormatter;
      FPanelTitleTextAttribs: array[0..1] of TXMLAttribute;
      FDrawingCoords : array[0..600] of TPoint;
      FTaxonName : array[0..1023] of AnsiChar;
      FOTULocations: Array of TRect;

      procedure InitPanelTitleTextAttribs;
      procedure DrawBackground;
      procedure DrawBranches(aWidth: Integer);
      procedure DrawBranchStraight(p : TpNode);
      procedure DrawOTUName(p: TpNode);
      procedure DrawCompressedArea(p: TpNode);
      procedure AddTreeSvgUseTags;
      function DrawGeologicLevel(aLevel: TTimespanType; AddBottomBorder: Boolean = False): Integer;
      function DrawGeologicLevelVert(aLevel: TTimespanType): Integer;
      procedure DrawGeologicLevelTopBorder;
      procedure DrawGeologicTimescaleTitle;
      function GetGeoLevelVertHeight(aLevel: TTimespanType): Integer;
      function GetGeoScaleForBgColors(mya: Double): TArrayOfGeologicTime;
      procedure DrawNodeHeights;
      procedure DrawEarthImpacts;
      procedure DrawEarthImpactsPanelBorder;
      procedure DrawEarthImpactsScale;
      procedure DrawGeoDataPanel(aRect: TRect; aData: TGeoData);
      procedure DrawGeoDataPanelScale(aRect: TRect; aData: TGeoData);
      procedure DrawGeoDataPanelData(aRect: TRect; aData: TGeoData);
      procedure DrawGeoDataPanelDataAreaChart(aRect: TRect; aData: TGeoData);

      procedure DrawTimescale;
      function MyaToX(mya: Double): Integer;
      function XToMya(x: Integer): Double;
      function ImpactsPanelCoords: TRect;
      procedure SetDoNewick(AValue: Boolean);
      procedure SetPanelHeight(AValue: Integer);
      function TimescaleCoords: TRect;
      function O2PanelCoords: TRect;
      function CO2PanelCoords: TRect;
      function LuminosityPanelCoords: TRect;
      function GeoTimescaleTitleCoords: TPoint;
      function TimescaleTitleCoords: TPoint;
      procedure ChangeAttribute(i: Integer);
      procedure SetImageHeight(AValue: Integer);
      procedure SetImageWidth(AValue: Integer);
      procedure UpdateFXCoordinates; { finds FXofRootNode and FXofTaxaNodes}

      function PointsToSvgLine(Points: array of TPoint; aColor: String; aWidth: String): String; overload;
      function PointsToSvgLine(Points: array of TPoint; Attributes: array of TXmlAttribute; aColor: String; aWidth: String): String; overload;
      function PointsToSvgPolygon(Points: array of TPoint; Attributes: array of TXmlAttribute): String;
      function TextToSvgText(x, y: Integer; aText: String; doScaleWidth: Boolean; aHeight: Integer=16; aColor: String='black'): String; overload;
      function TextToSvgText(x, y: Integer; aText: String; Attributes: array of TXMLAttribute; aScalingFactor: Integer = 0): String; overload;
      function AddSvgTextToRect(aRect: TRect; aText: String; Attributes: array of TXMLAttribute; aScalingFactor: Integer = 0): String;
      function AddVerticalSvgTextToRect(aRect: TRect; aText: String; fHeight: String; Attributes: array of TXMLAttribute): String;
      function GeoTimescaleHeight: Integer;
    public
      constructor Create;
      destructor Destroy; override;

      procedure SetEarthImpacts(Impacts: TEarthImpacts);
      procedure SetO2Data(aData: TGeoData);
      procedure SetCO2Data(aData: TGeoData);
      procedure SetLuminosityData(aData: TGeoData);
      procedure DrawTree(HMF : THandle); override;
      procedure DrawNewickOnly;
      procedure GenerateSvgStrings(TreeFileName: String; PanelsFileName: String; mobileFriendly: Boolean); overload;
      procedure GenerateSvgStrings(TreeFileName: String; mobileFriendly: Boolean); overload;
      procedure OpenSVG(aName: String; aWidth, aHeight: Integer; y: Integer; var aFile: TextFile; id: String=''); { writes the header tags}
      procedure CloseSVG(var aFile: TextFile); { closes the end tags for FTreeSVGFile}
      procedure OpenGroupDefs(var aFile: TextFile; groupId: String; className: String = '');
      procedure CloseGroupDefs(var aFile: TextFile; aWidth: Integer; aHeight: Integer; groupId: String);
      procedure UpdateTaxaOrder(InputIds: TLongIntList);
      property ImageWidth: Integer read FImageWidth write SetImageWidth;
      property ImageHeight: Integer read FImageHeight write SetImageHeight;
      property DoNewick: Boolean read FDoNewick write SetDoNewick;
      property PanelHeight: Integer read FGeoDataPanelHeight write SetPanelHeight;
      property IsMobileFriendly: Boolean read FIsMobileFriendly write FIsMobileFriendly;
  end;

var
  TreeName: String;

implementation

uses

  dateutils, gnamesmap, gsvgstrings, mearthimpactsrenderer,
  gtreeproc, StrUtils, gtimescalerenderer, gconfidenceintervals,
  gleafnodecounts;


{ TSvgTreeBox }

procedure TMySvgTreeBox.InitPanelTitleTextAttribs;
begin
  FPanelTitleTextAttribs[0].Name := 'font-height';
  FPanelTitleTextAttribs[0].Value := '16';
  FPanelTitleTextAttribs[1].Name := 'fill';
  FPanelTitleTextAttribs[1].Value := '#1c6bc9';
end;

procedure TMySvgTreeBox.DrawBackground;
var
  mya: Double;
  GeologicUnits: TArrayOfGeologicTime;
  i, j: Integer;
  Points: array[0..4] of TPoint;
  Temp: String;
  fillOpacity: String;
begin
  if GeoScaleForBgColors = gbcNone then
    Exit;
  if IsStudyTree then
    fillOpacity := '0.3'
  else
    fillOpacity := '0.2';
  mya := FRoot.height;
  GeologicUnits := GetGeoScaleForBgColors(mya);

  if Length(GeologicUnits) > 0 then
  begin
    Temp := '<g class=' + dblq + 'background' + dblq + ' id=' + dblq + TreeName + '-background-colors' + dblq + ' bg-colors=' + dblq + GeoBgColorsToString(GeoScaleForBgColors) + dblq + ' >';
    WriteLn(FTreeSvgFile, Temp);
    { draw the rectangles}
    for i := Length(GeologicUnits) - 1 downto 0 do
    begin
      Points[0].X := MyaToX(GeologicUnits[i].StartMya);
      Points[0].Y := MinHeight*4;
      Points[1].X := MyaToX(GeologicUnits[i].EndMya);
      Points[1].Y := MinHeight*4;
      Points[2].X := Points[1].X;
      Points[2].Y := FGeoTimescaleHeight;
      Points[3].X := Points[0].X;
      Points[3].Y := FGeoTimescaleHeight;
      Points[4].X := Points[0].X;
      Points[4].Y := MinHeight*4;
      Temp := '<polyline points=' + DBLQ;
      for j := 0 to 4 do
        Temp := Temp + IntToStr(Points[j].X) + ',' + IntToStr(Points[j].Y) + ' ';
      Temp := Trim(Temp) + DBLQ + ' ';
      Temp := Temp + 'class=' + dblq + 'background' + dblq + ' ';
      Temp := Temp + 'fill=' + DBLQ + GeologicUnits[i].HexaDecimalColorStr + DBLQ + ' ';

      Temp := Temp + 'fill-opacity=' + dblq + fillOpacity + dblq + ' ';
      Temp := Temp + 'stroke=' + DBLQ + '#aaaaaa' + DBLQ + ' stroke-width=' + DBLQ + '0' + DBLQ + ' stroke-linecap=' + DBLQ + 'square' + DBLQ + '/>';
      WriteLn(FTreeSvgFile, Temp);
    end;
    Temp := '</g>';
    WriteLn(FTreeSvgFile, Temp);
  end;
end;

procedure TMySvgTreeBox.DrawBranches(aWidth: Integer);
var
  topologyOnlyTag: String = '';
  revflg: boolean;
  ai: integer;
  temp: String;
  i: Integer;

  procedure DrawBranch(p : TpNode);
  var
    q, p2 : TpNode;

      procedure DrawBranchRectangle(p : TpNode);
      var
        d: integer;
        a1,a2: TNodeAttrib;

        procedure DrawHeightErrBar;
        var
          HTempPen,HTempBrush: THandle;
          err: double;
          dy: integer;
        begin
          if p.OTU or (p.outgroup and HasCalibrations) then exit;
          err := 2*HeightSEFunc(p.index);

          dwPenWidth   := CurAttrib.LineWidth*2;
          dy := CurAttrib.LineWidth*3 +8;
          if dy > Fyunit div 4 then
            dy := Fyunit div 4;

          FDrawingCoords[0].x := p.position.x - trunc(err*Fxunit) +Fxbase;
          FDrawingCoords[0].y := p.position.y +dy +Fybase;
          FDrawingCoords[1].x := FDrawingCoords[0].x;
          FDrawingCoords[1].y := p.position.y -dy +Fybase;
          FDrawingCoords[2].x := p.position.x + trunc(err*Fxunit) +Fxbase;
          FDrawingCoords[2].y := FDrawingCoords[1].y;
          FDrawingCoords[3].x := FDrawingCoords[2].x;
          FDrawingCoords[3].y := FDrawingCoords[0].y;
          MegaPolygon(FDrawingCoords, 4);
        end;

        procedure DrawVerticalLine;
        begin
          q := p.des1;
          while q.hidden do q := q.des1;

          if p.des1.hidden or ((q.attrindex <> p.attrindex) and
                               ((GetNodeAttrib(q).BranchOption = boHalfBranch) or
                                (GetNodeAttrib(q).BranchOption = boBranchOnly))) then
          begin
            q := p.des1;
            while q.hidden do q := q.des1;
            //if p.attrindex <> ai then
            //  ChangeAttrib(ai);
            d := GetLineWidth(q)*2;
            FDrawingCoords[0].x := p.position.x +Fxbase;
            FDrawingCoords[0].y := q.position.y +Fybase -d +CurAttrib.LineWidth*2;
            FDrawingCoords[1].x := FDrawingCoords[0].x;
            FDrawingCoords[1].y := p.position.y +Fybase -CurAttrib.LineWidth*2;
            MegaPolyLine(FDrawingCoords, 2);
            //if p.attrindex <> ai then
            //  ChangeAttrib(p.attrindex);
          end;
          q := p.des2;
          while q.hidden do q := q.des2;
          if p.des2.hidden or ((q.attrindex <> p.attrindex) and
                               ((GetNodeAttrib(q).BranchOption = boHalfBranch) or
                                (GetNodeAttrib(q).BranchOption = boBranchOnly))) then
          begin
            //if p.attrindex <> ai then
            //  ChangeAttrib(ai);
            d := GetLineWidth(q)*2;
            FDrawingCoords[0].x := p.position.x +Fxbase;
            FDrawingCoords[0].y := p.position.y +Fybase +CurAttrib.LineWidth*2;
            FDrawingCoords[1].x := FDrawingCoords[0].x;
            FDrawingCoords[1].y := q.position.y +Fybase +d -CurAttrib.LineWidth*2;
            MegaPolyLine(FDrawingCoords, 2);
            //if p.attrindex <> ai then
            //  ChangeAttrib(p.attrindex);
          end;
        end;

      begin
        if p = FRoot then
        begin
          if (isRooted and ShowRoot) then
          begin
            FDrawingCoords[0].x := Fxbase -100;
            FDrawingCoords[0].y := p.position.y +Fybase;
            FDrawingCoords[1].x := Fxbase;
            FDrawingCoords[1].y := FDrawingCoords[0].y;
            MegaPolyLine(FDrawingCoords, 2, p);
          end;
          DrawVerticalLine;
        end
        else
        begin
          if not p.hidden then
          begin
            if p.OTU or p.compressed then
            begin
              DrawOTUName(p);
              if (not p.OTU) and p.compressed and (not FTopoflag) then
                DrawCompressedArea(p);
            end
            else
              DrawVerticalLine;

            if (not (p.OTU or p.compressed)) and
               (GetNodeAttrib(p.des1).BranchOption = boFullBranch) then
              a1 := GetNodeAttrib(p.des1)
            else if GetNodeAttrib(p).BranchOption = boBranchOnly then
              if ai < 0 then
                a1 := GroupAttrib[ai]
              else
                a1 := AttribList[ai]
            else
              a1 := GetNodeAttrib(p);
            if (not (p.OTU or p.compressed)) and
               (GetNodeAttrib(p.des2).BranchOption = boFullBranch) then
              a2 := GetNodeAttrib(p.des2)
            else if GetNodeAttrib(p).BranchOption = boBranchOnly then
              if ai < 0 then
                a2 := GroupAttrib[ai]
              else
                a2 := AttribList[ai]
            else
              a2 := GetNodeAttrib(p);

            if (p.attrindex <> p.anc.attrindex) and ((GetNodeAttrib(p).BranchOption = boHalfBranch) or
                                                     (GetNodeAttrib(p).BranchOption = boBranchOnly)) then
            begin
              if p.compressed then
                d := 0
              else if a1.LineWidth = GetNodeAttrib(p).LineWidth then
                d := a1.LineWidth*2
              else if a2.LineWidth = GetNodeAttrib(p).LineWidth then
                d := a2.LineWidth*2
              else if a1.LineWidth > a2.LineWidth then
                d := a1.LineWidth*2
              else
                d := a2.LineWidth*2;
              FDrawingCoords[0].x := p.anc.position.x +Fxbase +GetNodeAttrib(p).LineWidth*2;
              FDrawingCoords[0].y := p.position.y +Fybase;
              if p.position.x = p.anc.position.x then
                FDrawingCoords[1].x := FDrawingCoords[0].x
              else
                FDrawingCoords[1].x := p.position.x +Fxbase +d -GetNodeAttrib(p).LineWidth*2;
              FDrawingCoords[1].y := FDrawingCoords[0].y;
              MegaPolyLine(FDrawingCoords, 2, p);

              if IsLinearized and (not FTopoflag) and assigned(HeightSEFunc) and ShowHeightErrBar then
                DrawHeightErrBar;
            end
            else
            begin
              //if ((p.attrindex <> ai) and (GetNodeAttrib(p).BranchOption = boNoBranch)) or
              //   ((p.anc.attrindex = p.attrindex) and (GetNodeAttrib(p).BranchOption = boBranchOnly)) then
              //  ChangeAttrib(ai);
              if p.compressed then
                d := 0
              else if a1.LineWidth = CurAttrib.LineWidth then
                d := a1.LineWidth*2
              else if a2.LineWidth = CurAttrib.LineWidth then
                d := a2.LineWidth*2
              else if a1.LineWidth > a2.LineWidth then
                d := a1.LineWidth*2
              else
                d := a2.LineWidth*2;
              FDrawingCoords[0].x := p.anc.position.x +Fxbase;
              if p = p.anc.des1 then
                FDrawingCoords[0].y := p.anc.position.y +Fybase -CurAttrib.LineWidth*2
              else
                FDrawingCoords[0].y := p.anc.position.y +Fybase +CurAttrib.LineWidth*2;
              FDrawingCoords[1].x := FDrawingCoords[0].x;
              FDrawingCoords[1].y := p.position.y +Fybase;
              if p.position.x = p.anc.position.x then
                FDrawingCoords[2].x := FDrawingCoords[1].x
              else
                FDrawingCoords[2].x := p.position.x +Fxbase +d -CurAttrib.LineWidth*2;
              FDrawingCoords[2].y := p.position.y +Fybase;
              MegaPolyLine(FDrawingCoords, 3, p);

              if IsLinearized and(not p.outgroup) and (not FTopoflag) and assigned(HeightSEFunc) and ShowHeightErrBar then
                DrawHeightErrBar;
            end;
            //if GetNodeAttrib(p) <> CurAttrib then
            //    ChangeAttrib(p.attrindex);
          end;
        end;
      end;

  begin
    if p <> FRoot then
      if TreeStyle = tsCircle then
      begin
        if (p.anc.cursize > (CurrentNoOfOTUs div 4)) and
          ((p.cursize <= (CurrentNoOfOTUs div 4)) or p.OTU or p.compressed) then
          if cos(p.angle) < 0 then
            revflg := true
          else
            revflg := false;
      end
      else if TreeStyle = tsRadiation then
        if (p.anc.sector > PI/2) and
          ((p.sector <= PI/2) or p.OTU or p.compressed) then
          if cos(p.angle) < 0 then
            revflg := true
          else
            revflg := false;

    if not (p.OTU or p.compressed) then
    begin
      DrawBranch(p.des1);
      DrawBranch(p.des2);
    end;

    CurAttrib := GetNodeAttrib(p);
    if p = FRoot then
      ai := 0
    else if (GetNodeAttrib(p).BranchOption = boBranchOnly) or
            (GetNodeAttrib(p).BranchOption = boNoBranch) then
    begin
      q := p.anc;
      while (q <> FRoot) and ((GetNodeAttrib(q).BranchOption = boBranchOnly) or q.hidden) do
        q := q.anc;
      ai := q.attrindex;
    end
    else
      ai := p.attrindex;

    case BranchStyle of
      bsRectangular : DrawBranchRectangle(p);
      bsStraight    : DrawBranchStraight(p);
    end;
  end;

begin
  FSvgTreeImageWidth := aWidth;
  if IsStudyTree and ShowTopologyOnly then
    topologyOnlyTag := ' scaled="false"';
  temp := '<g id=' + DBLQ + TreeName + '-branches' + DBLQ + ' ' + 'class=' + DBLQ + 'branches' + DBLQ + ' ' + 'depth=' + DBLQ +  TaxonomicRankToString(FDeepestRank) + DBLQ + topologyOnlyTag + '>';
  WriteLn(FTreeSvgFile, temp);
  FCircleTags.Add('<g id=' + DBLQ + TreeName + '-nodes' + DBLQ + ' ' + 'class=' + DBLQ + 'branches' + DBLQ + ' ' + 'depth=' + DBLQ +  TaxonomicRankToString(FDeepestRank) + DBLQ + '>');
  FTaxaNameTags.Add('<g id=' + DBLQ + TreeName + '-names' + DBLQ + ' ' + 'class=' + DBLQ + 'branches' + DBLQ + ' ' + 'depth=' + DBLQ +  TaxonomicRankToString(FDeepestRank) + DBLQ + '>');
  DrawBranch(FRoot);
  FCircleTags.Add('</g>');
  FTaxaNameTags.Add('</g>');
  temp := '</g>';
  WriteLn(FTreeSvgFile, temp);
  for i := 0 to FCircleTags.Count - 1 do
    WriteLn(FTreeSvgFile, FCircleTags[i]);
  for i := 0 to FTaxaNameTags.Count - 1 do
    WriteLn(FTreeSvgFile, FTaxaNameTags[i]);
end;

procedure TMySvgTreeBox.DrawBranchStraight(p: TpNode);
var
  q: TpNode = nil;
begin
  if p = FRoot then
  begin
    if (isRooted and ShowRoot) then
    begin
      FDrawingCoords[0].x := Fxbase -100;
      FDrawingCoords[1].y := p.position.y +Fybase;
      FDrawingCoords[1].x := p.position.x +Fxbase;
      if ShowTopologyOnly then
      begin
        q := p;
        if p.des1.cursize > p.des2.cursize then
          while not q.OTU do q := q.des1
        else
          while not q.OTU do q := q.des2;
        FDrawingCoords[0].y := FDrawingCoords[1].y -Round(100*(q.position.y-p.position.y)/(q.position.x-p.position.x));
       end
       else
         FDrawingCoords[0].y := FDrawingCoords[1].y;

      MegaPolyLine(FDrawingCoords, 2, p);
    end;
  end
  else
  begin
    if not p.hidden then
    begin
      if p.OTU or p.compressed then
      begin
        DrawOTUName(p);
      end;
      if (not p.OTU) and p.compressed and (not FTopoflag) then
        DrawCompressedArea(p);
      FDrawingCoords[0].x := p.anc.position.x +Fxbase;
      FDrawingCoords[0].y := p.anc.position.y +Fybase;
      FDrawingCoords[1].x := p.position.x +Fxbase;
      FDrawingCoords[1].y := p.position.y +Fybase;
      MegaPolyLine(FDrawingCoords, 2, p);
    end;
  end;
end;

procedure TMySvgTreeBox.DrawOTUName(p: TpNode);
var
  x,y,d : integer;
  pName: PAnsiChar;

  function ptInOTULoc(x, y: Integer): Boolean;
  var
    i: Integer;
  begin
    result := false;
    if HideOverlappingTaxa then
      for i := 0 to Length(FOTULocations)-1 do
      begin
        //if (y > FOTULocations[i].Top) and (y < FOTULocations[i].Bottom) then
        if (y > FOTULocations[i].Top) and (y < (FOTULocations[i].Top+FOTULocations[i].Bottom*2) div 3) then // KT 4/6/2011 previous version was hiding taxa when they weren't actually touching yet.
          result := true;
      end;
  end;

  procedure addOTULoc(r: TRect);
  begin
    SetLength(FOTULocations, Length(FOTULocations)+1);
    FOTULocations[Length(FOTULocations)-1] := r;
  end;

begin
  if not CurAttrib.ShowTaxonName then Exit;
  try
    d := GetLineWidth(p)*2;
    if (TreeStyle = tsRadiation) or (TreeStyle = tsCircle) then
    begin
      if (not p.OTU) and p.compressed then
        x := (p.des1.position.x + p.des2.position.x) div 2
      else
        x := p.position.x;
      if cos(p.angle) < 0.0 then
        x := x -p.namesize.x;

      if GetTaxonMarker(p).Shape <> msNone then
      begin
        d := d +Round((1+abs(cos(p.angle)))*Abs(FSVGFontHeight*2.5));
        if cos(p.angle) < 0.0 then
          x := x -d
        else
          x := x +d
      end
      else
      begin
        if abs(cos(p.angle)) < sqrt(2)/2 then
          if cos(p.angle) < 0.0 then
            x := x +round((1-abs(cotan(p.angle)))*p.namesize.x/2) -d
          else
            x := x -round((1-abs(cotan(p.angle)))*p.namesize.x/2) +d
        else if cos(p.angle) < 0.0 then
          x := x -d
        else
          x := x +d;
      end
    end
    else
    begin
      if GetTaxonMarker(p).Shape <> msNone then
        d := d +Abs(FSVGFontHeight*5);
      if ShowCharState then
        d := d +CustomTextWidth(p.charstate)*4 +Abs(CharStateFontHeight)*2;
      if (not p.OTU) and p.compressed and (not FTopoflag) then
        x := p.des1.position.x +d
      else
        x := p.position.x +d;
    end;
    if (TreeStyle = tsRadiation) or (TreeStyle = tsCircle) then
    begin
      if (not p.OTU) and p.compressed then
        y := (p.des1.position.y + p.des2.position.y) div 2
      else
        y := p.position.y;
      if GetTaxonMarker(p).Shape <> msNone then
        if sin(p.angle) < 0.0 then
          y := y +Abs(FSVGFontHeight*2) +Round(Abs(sin(p.angle)*FSVGFontHeight*3))
        else
          y := y +Abs(FSVGFontHeight*2) -Round(Abs(sin(p.angle)*FSVGFontHeight*3))
      else if abs(cos(p.angle)) < sqrt(2)/2 then
        if sin(p.angle) < 0.0 then
          y := y +abs(FSVGFontHeight*2) +round((2-abs(cotan(p.angle)))*abs(FSVGFontHeight)*2.5)
        else
          y := y +abs(FSVGFontHeight*2) -round((2-abs(cotan(p.angle)))*abs(FSVGFontHeight)*2.5)
      else
        if sin(p.angle) < 0.0 then
          y := y +abs(FSVGFontHeight*2) +round(abs(tan(p.angle)*FSVGFontHeight)*2.5)
        else
          y := y +abs(FSVGFontHeight*2) -round(abs(tan(p.angle)*FSVGFontHeight)*2.5)
    end
    else if (not p.OTU) and p.compressed then
      y := (p.des1.position.y + p.des2.position.y) div 2 +Abs(FSVGFontHeight*2)
    else
      y := p.position.y +Abs(FSVGFontHeight*2);

    if TreeStyle = tsTraditional then
      if p.compressed then
        pname := StrPCopy(FTaxonName, ' '+p.PrivateName)
      else
        pname := StrPCopy(FTaxonName, ' '+p.name)

    else if cos(p.angle) < 0.0 then
      pname := StrPCopy(FTaxonName, p.name)
    else
      pname := StrPCopy(FTaxonName, ' '+p.name);

    if p.PrivateName <> EmptyStr then
    begin
      FTaxaNameTags.Add(SvgOtuName(x+Fxbase, y+Fybase, PAnsiChar(p.PrivateName), p));
      //WriteLn(FTreeSvgFile, SvgOtuName(x+Fxbase, y+Fybase, PAnsiChar(p.PrivateName), p));
    end
    else
    begin
      if ptInOTULoc(x+Fxbase, y+Fybase) then
        exit;
      FTaxaNameTags.Add(SvgOtuName(x+Fxbase, y+Fybase, pname, p));
      //WriteLn(FTreeSvgFile, SvgOtuName(x+Fxbase, y+Fybase, pname, p));
    end;
    addOTULoc(Rect(x+Fxbase, y+Fybase, x+Fxbase+StrLength(pname), y+Fybase+(StrHeight(pname)*4)));
  Except on E: Exception do
    raise Exception.Create('Error in DrawOTUName: ' + E.Message);
  end;
end;

procedure TMySvgTreeBox.DrawCompressedArea(p: TpNode);
begin
  FDrawingCoords[0].x := p.position.x +Fxbase;
  FDrawingCoords[0].y := p.position.y +CurAttrib.LineWidth +Fybase;
  FDrawingCoords[1].x := p.position.x +Fxbase;
  FDrawingCoords[1].y := p.position.y -CurAttrib.LineWidth +Fybase;
  FDrawingCoords[2].x := p.des1.position.x +Fxbase;
  FDrawingCoords[2].y := p.des1.position.y +Fybase;
  FDrawingCoords[3].x := p.des2.position.x +Fxbase;
  FDrawingCoords[3].y := p.des2.position.y +Fybase;
  MegaPolygon(FDrawingCoords, 4, p);
end;

procedure TMySvgTreeBox.AddTreeSvgUseTags;
begin
  WriteLn(FTreeSvgFile, Format('<svg viewBox="0 0 %d %d" preserveAspectRatio="none">', [FSvgTreeImageWidth, minHeight*4]));
  WriteLn(FTreeSvgFile, Format('<use href="#%s-background-colors" width="1" height="1"/>', [TreeName]));
  WriteLn(FTreeSvgFile, '</svg>');
  WriteLn(FTreeSvgFile, Format('<svg viewBox="0 0 %d %d" preserveAspectRatio="none">', [FSvgTreeImageWidth, minHeight*4]));
  WriteLn(FTreeSvgFile, Format('<use href="#%s-branches"/>', [TreeName]));
  WriteLn(FTreeSvgFile, '</svg>');
  WriteLn(FTreeSvgFile, Format('<svg preserveAspectRatio="none">', []));
  WriteLn(FTreeSvgFile, Format('<use href="#%s-nodes"/>', [TreeName]));
  WriteLn(FTreeSvgFile, '</svg>');
  WriteLn(FTreeSvgFile, Format('<svg preserveAspectRatio="none">', []));
  WriteLn(FTreeSvgFile, Format('<use href="#%s-names"/>', [TreeName]));
  WriteLn(FTreeSvgFile, '</svg>');
  //WriteLn(FTreeSvgFile, Format('', []));
  //
  //WriteLn(FTreeSvgFile, '</svg>');
end;

function TMySvgTreeBox.DrawGeologicLevel(aLevel: TTimespanType; AddBottomBorder: Boolean = False): Integer;
var
  mya: Double;
  times: TArrayOfGeologicTime;
  i, x, y: Integer;
  Temp: String;
  aRect: TRect;
  rectAttribs: array of TXmlAttribute;
  textAttribs: array of TXmlAttribute;
  linePoints: array [0..1] of TPoint;
begin

  mya := FRoot.height;
  times := FGeoTimescale.GetTimescale(mya, aLevel);
  if Length(times) > 0 then
  begin

    Result := FGeoTimescaleHeight;
    { draw the border}
    aRect.Top := 0;
    aRect.Bottom := FGeoTimescaleHeight;
    aRect.Left := MyaToX(times[Length(times) - 1].StartMya)-1;
    aRect.Right := MyaToX(0);
    SetLength(rectAttribs, 3);
    rectAttribs[0].Name := 'fill';
    rectAttribs[0].Value := 'none';
    rectAttribs[1].Name := 'stroke';
    rectAttribs[1].Value := '#555555';
    rectAttribs[2].Name := 'stroke-width';
    rectAttribs[2].Value := '1';
    if FIsMobileFriendly then
      Temp := RectToSvgRect(aRect, rectAttribs, FSvgTreeImageWidth)
    else
      Temp := RectToSvgRect(aRect, rectAttribs);
    WriteLn(FPanelsSvgFile, Temp);

    SetLength(rectAttribs, 8);
    SetLength(textAttribs, 4);
    rectAttribs[3].Name := 'full-name';
    rectAttribs[4].Name := 'start';
    rectAttribs[5].Name := 'end';
    rectAttribs[6].Name := 'unit';
    rectAttribs[7].Name := 'title';

    textAttribs[0].Name := 'full-name';
    textAttribs[1].Name := 'start';
    textAttribs[2].Name := 'end';
    textAttribs[3].Name := 'unit';
    { draw the rectangles}
    for i := Length(times) - 1 downto 0 do
    begin
      aRect.Left := MyaToX(times[i].StartMya);
      aRect.Bottom := FGeoTimescaleHeight-2;
      if i = 0 then
        aRect.Right := MyaToX(0)
      else
        aRect.Right := MyaToX(times[i].EndMya);
      if ShowEpochsAndAges then
      begin
        if aLevel = tstAge then
          aRect.Top := 2
        else
          aRect.Top := 1;
      end
      else
        if aLevel = tstPeriod then
          aRect.Top := 2
        else
          aRect.Top := 1;
      rectAttribs[0].Value := times[i].HexaDecimalColorStr;
      rectAttribs[1].Value := times[i].HexaDecimalColorStr;
      rectAttribs[2].Value := '3';
      rectAttribs[3].Value := times[i].Name;
      rectAttribs[4].Value := Format('%.2f', [times[i].StartMya]);
      rectAttribs[5].Value := Format('%.2f', [times[i].EndMya]);
      rectAttribs[6].Value := TimespanTypeString(times[i].TimespanType);
      rectAttribs[7].Value := times[i].Name;
      if FIsMobileFriendly then
        Temp := RectToSvgRect(aRect, rectAttribs, FSvgTreeImageWidth)
      else
        Temp := RectToSvgRect(aRect, rectAttribs);
      WriteLn(FPanelsSvgFile, Temp);

      textAttribs[0].Value := times[i].Name;
      textAttribs[1].Value := Format('%.2f', [times[i].StartMya]);
      textAttribs[2].Value := Format('%.2f', [times[i].EndMya]);
      textAttribs[3].Value := TimespanTypeString(times[i].TimespanType);
      if FIsMobileFriendly then
        Temp := AddSvgTextToRect(aRect, times[i].GetBestFitText(aRect), textAttribs, FSvgTreeImageWidth)
      else
        Temp := AddSvgTextToRect(aRect, times[i].GetBestFitText(aRect), textAttribs);
      if Temp <> EmptyStr then
        WriteLn(FPanelsSvgFile, Temp);
    end;

    { draw the left and right borders}
    linePoints[0].Y := 0;
    linePoints[1].Y := FGeoTimescaleHeight;
    linePoints[0].X := MyaToX(times[Length(times) - 1].StartMya)-1;
    linePoints[1].X := linePoints[0].X;
    Temp := PointsToSvgLine(linePoints, '#555555', '1');
    WriteLn(FPanelsSvgFile, Temp);
    linePoints[0].X := MyaToX(0);
    linePoints[1].X := linePoints[0].X;
    Temp := PointsToSvgLine(linePoints, '#555555', '1');
    WriteLn(FPanelsSvgFile, Temp);

    { handle bottom border if needed}
    if AddBottomBorder then
    begin
      linePoints[0].Y := FGeoTimescaleHeight-1;
      linePoints[1].Y := linePoints[0].Y;
      linePoints[0].X := MyaToX(times[Length(times) - 1].StartMya)-1;
      linePoints[1].X := MyaToX(0);
      Temp := PointsToSvgLine(linePoints, '#555555', '1');
      WriteLn(FPanelsSvgFile, Temp);
    end;

    { cover up the overflow on the right border due to the stroke width}
    aRect.Left := MyaToX(0);
    aRect.Right := aRect.Left + 3;
    aRect.Top := aRect.Top - 2;
    aRect.Bottom := aRect.Bottom + 2;
    SetLength(rectAttribs, 3);
    rectAttribs[0].Name := 'fill';
    rectAttribs[0].Value := 'white';
    rectAttribs[1].Name := 'stroke';
    rectAttribs[1].Value := 'white';
    rectAttribs[2].Name := 'stroke-width';
    rectAttribs[2].Value := '1';
    if FIsMobileFriendly then
      WriteLn(FPanelsSvgFile, RectToSvgRect(aRect, rectAttribs, FSvgTreeImageWidth))
    else
      WriteLn(FPanelsSvgFile, RectToSvgRect(aRect, rectAttribs));

    { write the name of the geologic level}
    x := aRect.Left + 5;
    y :=  (aRect.Top + Round((aRect.Bottom - aRect.Top) / 2) + (FScaleValFontHeight div 2));
    WriteLn(FPanelsSvgFile, TextToSvgText(x, y, TimespanTypeString(aLevel) + 's', FIsMobileFriendly, FScaleValFontHeight));
  end;
end;

function TMySvgTreeBox.DrawGeologicLevelVert(aLevel: TTimespanType): Integer;
var
  mya: Double;
  times: TArrayOfGeologicTime;
  i: Integer;
  Temp: String;
  aRect: TRect;
  LongestName: String;
  aHeight: Integer;
  textAttribs: array[0..3] of TXmlAttribute;
  rectAttribs: array of TXmlAttribute;
begin
  Result := 0;
  mya := FRoot.height;
  times := FGeoTimescale.GetTimescale(mya, aLevel);
  LongestName := FGeoTimescale.GetLongestName(mya, aLevel);
  aHeight := CustomTextWidth(LongestName);
  Result := aHeight;
  if Length(times) > 0 then
  begin
    { draw the border}
    aRect.Top := 0;
    aRect.Bottom := aHeight;
    aRect.Left := MyaToX(times[Length(times) - 1].StartMya)-1;
    aRect.Right := MyaToX(0)+1;
    SetLength(rectAttribs, 3);
    rectAttribs[0].Name := 'fill';
    rectAttribs[0].Value := 'none';
    rectAttribs[1].Name := 'stroke';
    rectAttribs[1].Value := 'black';
    rectAttribs[2].Name := 'stroke-width';
    rectAttribs[2].Value := '1';
    WriteLn(FPanelsSvgFile, RectToSvgRect(aRect, rectAttribs));

    { draw the rectangles}
    textAttribs[0].Name := 'full-name';
    textAttribs[1].Name := 'start';
    textAttribs[2].Name := 'end';
    textAttribs[3].Name := 'unit';

    SetLength(rectAttribs, 7);
    rectAttribs[3].Name := 'full-name';
    rectAttribs[4].Name := 'start';
    rectAttribs[5].Name := 'end';
    rectAttribs[6].Name := 'unit';

    for i := Length(times) - 1 downto 0 do
    begin
      aRect.Left := MyaToX(times[i].StartMya);
      aRect.Bottom := aHeight-2;
      aRect.Right := MyaToX(times[i].EndMya);

      if aLevel = tstAge then
        aRect.Top := 2
      else
        aRect.Top := 1;
      rectAttribs[0].Value := times[i].HexaDecimalColorStr;
      rectAttribs[1].Value := times[i].HexaDecimalColorStr;
      rectAttribs[2].Value := '2';
      rectAttribs[3].Value := times[i].Name;
      rectAttribs[4].Value := Format('%.2f', [times[i].StartMya]);
      rectAttribs[5].Value := Format('%.2f', [times[i].EndMya]);
      rectAttribs[6].Value := TimespanTypeString(times[i].TimespanType);
      Temp := RectToSvgRect(aRect, rectAttribs);
      WriteLn(FPanelsSvgFile, Temp);

      textAttribs[0].Value := times[i].Name;
      textAttribs[1].Value := Format('%.2f', [times[i].StartMya]);
      textAttribs[2].Value := Format('%.2f', [times[i].EndMya]);
      textAttribs[3].Value := TimespanTypeString(times[i].TimespanType);
      Temp := AddVerticalSvgTextToRect(aRect, times[i].Name, '14', textAttribs);
      WriteLn(FPanelsSvgFile, Temp);
    end;
  end;
end;

procedure TMySvgTreeBox.DrawGeologicLevelTopBorder;
var
  lineCoords: array[0..1] of TPoint;
begin
    lineCoords[0].Y := 0;
    lineCoords[1].Y := 0;
    lineCoords[0].X := MyaToX(FRoot.height)-1;
    lineCoords[1].X := MyaToX(0);
    WriteLn(FPanelsSvgFile, PointsToSVGLine(lineCoords, '#555555', '1'));
end;

procedure TMySvgTreeBox.DrawGeologicTimescaleTitle;
var
  coords: TPoint;
  titleSvg: String;
begin
  coords := GeoTimescaleTitleCoords;
  if FIsMobileFriendly then
    titleSvg := TextToSvgText(coords.x, coords.y, 'Geologic Timescale', FPanelTitleTextAttribs, FSvgTreeImageWidth)
  else
    titleSvg := TextToSvgText(coords.x, coords.y, 'Geologic Timescale', FPanelTitleTextAttribs);
  WriteLn(FPanelsSvgFile, titleSvg);
end;

function TMySvgTreeBox.GetGeoLevelVertHeight(aLevel: TTimespanType): Integer;
var
  LongestName: String;
  mya: Double;
begin
  mya := FRoot.Height;
  LongestName := FGeoTimescale.GetLongestName(mya, aLevel);
  Result := CustomTextWidth(LongestName);
end;

function TMySvgTreeBox.GetGeoScaleForBgColors(mya: Double): TArrayOfGeologicTime;
begin
  if GeoScaleForBgColors <> gbcAuto then {user specifed}
  begin
    if (not ShowEpochsAndAges) and ((GeoScaleForBgColors = gbcEpoch) or (GeoScaleForBgColors = gbcAge)) then
    begin
      Result := FGeoTimescale.GetTimescale(mya, tstPeriod); { epochs and ages are not applicable so fallback to period}
      GeoScaleForBgColors := gbcPeriod;
    end
    else
      Result := FGeoTimescale.GetTimescale(mya, MapGeoBgColorsToTimespanType(GeoScaleForBgColors))
  end
  else if ShowEpochsAndAges then
  begin
    Result := FGeoTimescale.GetTimescale(mya, tstAge);
    GeoScaleForBgColors := gbcAge;
  end
  else
  begin
    Result := FGeoTimescale.GetTimescale(mya, tstPeriod);
    GeoScaleForBgColors := gbcPeriod;
  end;
end;

procedure TMySvgTreeBox.DrawNodeHeights;
var
  Text : array[0..15] of AnsiChar;
  PText : PAnsiChar;
  x,y,i, dh, dv : integer;
  TextSize : integer;
  DivTime: Double;
  DivTimeStr: String;
  d: Double;
begin

  SetFontTarget(tftimes);
  for i := NoOfOTUs+1 to NoOfNodes do
  begin
    if FNode[i] = FRoot then
      continue;
    DivTime := FNode[i]^.Height;
    DivTimeStr := FloatToStrF(DivTime, ffFixed, 8, DivTimeDecimals);
    with FNode[i]^ do
    begin

      textsize := CustomTextWidth(DivTimeStr);
      dh := FSvgLineWidth * 2;
      if TimesMargin.X < -(textsize + dh + 8) then
          dh := -(textsize + dh +8)
      else if TimesMargin.X > (position.x - anc.position.x + dh) then
          dh := position.x - anc.position.x + dh
      else
          dh := TimesMargin.X + dh;
      x := position.x - textsize - dh;

      dv := TimesMargin.Y + GetLineWidth(FNode[i]) * 2;
      //if (TimesPosition = bipAboveBranch)
      //or ((TimesPosition = bipAutomatic) and (isBranchLength and ShowBLen)) then
          y := position.y - dv;
      //else if TimesPosition = bipBelowBranch then
      //    y := position.y + Abs(TimesFontHeight) * 5 + dv
      //else
      //    if position.y <= anc.position.y then
      //        y := position.y - dv
      //    else
      //        y := position.y +Abs(TimesFontHeight) * 5 + dv;

      ptext := StrPCopy(text, DivTimeStr);
      MegaTextOutA(x + Fxbase, y + Fybase, ptext);
    end;
  end;
  WriteLn(FTreeSvgFile, '</g>');
end;

procedure TMySvgTreeBox.DrawEarthImpacts;
var
  aImpacts: TEarthImpactsArray;
  i: Integer;
  aRect: TRect;
  MaxDiam: Integer;
  attribs: array[0..1] of TXMLAttribute;
  renderer: TEarthImpactsRenderer=nil;
  svgStrings: TStringList=nil;
begin
    attribs[0].Name := 'font-height';
    attribs[0].Value := '16';
    attribs[1].Name := 'fill';
    attribs[1].Value := '#555555';
  aRect := ImpactsPanelCoords;

  aImpacts := FEarthImpacts.GetEarthImpacts(FRoot.height, 0.0);
  MaxDiam := Trunc(RoundTo(FEarthImpacts.GetMaxDiameter, 2));
  if Length(aImpacts) > 0 then
  begin
    WriteLn(FPanelsSvgFile, '<g class=' + dblq + 'earth-impacts' + dblq + '>');
    renderer :=  TEarthImpactsRenderer.Create(aImpacts);
    renderer.MapMyaToCoordsFunc:= MyaToX;
    svgStrings := renderer.Render(tHorizontal, aRect, MaxDiam);
    for i := 0 to svgStrings.Count - 1 do
      WriteLn(FPanelsSvgFile, svgStrings[i]);
    DrawEarthImpactsScale;
    WriteLn(FPanelsSvgFile, TextToSvgText(aRect.Right + 60, ((aRect.Bottom - aRect.Top) div 2) + (FSvgFontHeight div 2), 'Earth Impacts', FPanelTitleTextAttribs));
    WriteLn(FPanelsSvgFile, '</g>');
  end;
  DrawEarthImpactsPanelBorder;
end;

procedure TMySvgTreeBox.DrawEarthImpactsPanelBorder;
var
  aRect: TRect;
  p: array [0..2] of TPoint;
begin
  aRect := ImpactsPanelCoords;
  p[0].X := aRect.Left;
  p[0].Y := aRect.Bottom;
  p[1].X := aRect.Right;
  p[1].Y := aRect.Bottom;
  p[2].X := aRect.Right;
  p[2].Y := aRect.Top;
  WriteLn(FPanelsSvgFile, PointsToSvgLine(p, '#aaaaaa', '1'));
end;

procedure TMySvgTreeBox.DrawEarthImpactsScale;
const
  TICK_WIDTH = 5;
var
  MaxDiam: Integer;
  aRect: TRect;
  Temp: String;
  LinePoints: array[0..1] of TPoint;
  x, y: Integer;
  aText: String;
begin
  aRect := ImpactsPanelCoords;
  { clean up any circle that extended over the right edge of the box}
  Temp := '<rect x=' + dblq + IntToStr(aRect.Right + 1) + dblq + ' ';
  Temp := Temp + 'y=' + dblq + IntToStr(aRect.Top) + dblq + ' ';
  Temp := Temp + 'width=' + dblq + '60' + dblq + ' ';
  Temp := Temp + 'height=' + dblq + IntToStr(FGeoDataPanelHeight) + dblq + ' ';
  Temp := Temp + 'fill=' + dblq + 'white' + dblq + ' ';
  Temp := Temp + 'stroke=' + dblq + 'white' + dblq + ' ';
  Temp := Temp + 'stroke-width=' + dblq + '2' + dblq + ' ';
  Temp := Temp + ' />';
  WriteLn(FPanelsSvgFile, Temp);

  { clean up any circle that extended over the left edge of the box}
  Temp := '<rect x=' + dblq + '1' + dblq + ' ';
  Temp := Temp + 'y=' + dblq + IntToStr(aRect.Top) + dblq + ' ';
  Temp := Temp + 'width=' + dblq + IntToStr(aRect.Left - 2) + dblq + ' ';
  Temp := Temp + 'height=' + dblq + IntToStr(FGeoDataPanelHeight) + dblq + ' ';
  Temp := Temp + 'fill=' + dblq + 'white' + dblq + ' ';
  Temp := Temp + 'stroke=' + dblq + 'white' + dblq + ' ';
  Temp := Temp + 'stroke-width=' + dblq + '2' + dblq + ' ';
  Temp := Temp + ' />';
  WriteLn(FPanelsSvgFile, Temp);

  MaxDiam := Trunc(RoundTo(FEarthImpacts.GetMaxDiameter, 2));

  { draw the top tick}
  LinePoints[0].X := aRect.Right + 2;
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X := aRect.Right + 2 + TICK_WIDTH;
  LinePoints[1].Y := aRect.Top;
  WriteLn(FPanelsSvgFile, PointsToSvgLine(LinePoints, 'black', '1'));

  { draw the top value}
  aText := IntToStr(MaxDiam);
  x := aRect.Right + 14;
  y := aRect.Top + (FScaleValFontHeight div 2) +1;
  WriteLn(FPanelsSvgFile, TextToSvgText(x, y, aText, False, FScaleValFontHeight, 'black'));

  if FGeoDataPanelHeight > 50 then
  begin
    { draw the 1/4 tick}
    LinePoints[0].X := aRect.Right + 2;
    LinePoints[0].Y := aRect.Top + Round(FGeoDataPanelHeight / 4);
    LinePoints[1].X := aRect.Right + 2 + TICK_WIDTH;
    LinePoints[1].Y := LinePoints[0].Y;
    WriteLn(FPanelsSvgFile, PointsToSvgLine(LinePoints, 'black', '1'));

    { draw the 1/4 value}
    aText := IntToStr(Trunc(RoundTo((MaxDiam / 2), 2)));
    y := aRect.Top + Round(FGeoDataPanelHeight / 4) +(FScaleValFontHeight div 4);
    WriteLn(FPanelsSvgFile, TextToSvgText(x, y, aText, False, FScaleValFontHeight, 'black'));
  end;


  { draw the center tick}
  LinePoints[0].X := aRect.Right + 2;
  LinePoints[0].Y := aRect.Top + Round(FGeoDataPanelHeight / 2);
  LinePoints[1].X := aRect.Right + 2 + TICK_WIDTH;
  LinePoints[1].Y := LinePoints[0].Y;
  WriteLn(FPanelsSvgFile, PointsToSvgLine(LinePoints, 'black', '1'));

  { draw the center value}
  aText := '0 km';
  y := aRect.Top + Round(FGeoDataPanelHeight / 2) +(FScaleValFontHeight div 4);
  WriteLn(FPanelsSvgFile, TextToSvgText(x, y, aText, False, FScaleValFontHeight));

  if FGeoDataPanelHeight > 50 then
  begin
    { draw the 3/4 tick}
    LinePoints[0].X := aRect.Right + 2;
    LinePoints[0].Y := aRect.Top + Round(FGeoDataPanelHeight * 0.75);
    LinePoints[1].X := aRect.Right + 2 + TICK_WIDTH;
    LinePoints[1].Y := LinePoints[0].Y;
    WriteLn(FPanelsSvgFile, PointsToSvgLine(LinePoints, 'black', '1'));

    { draw the 3/4 value}
    aText := IntToStr(Trunc(RoundTo((MaxDiam / 2), 2)));
    y := aRect.Top + Round(FGeoDataPanelHeight * 0.75) +(FScaleValFontHeight div 4);
    WriteLn(FPanelsSvgFile, TextToSvgText(x, y, aText, False, FScaleValFontHeight));
  end;

  { draw the bottom tick}
  LinePoints[0].X := aRect.Right + 2;
  LinePoints[0].Y := aRect.Bottom;
  LinePoints[1].X := aRect.Right + 2 + TICK_WIDTH;
  LinePoints[1].Y := aRect.Bottom;
  WriteLn(FPanelsSvgFile, PointsToSvgLine(LinePoints, 'black', '1'));

  { draw the bottom value}
  aText := IntToStr(MaxDiam);
  y := aRect.Bottom + (FScaleValFontHeight div 4);
  WriteLn(FPanelsSvgFile, TextToSvgText(x, y, aText, False, FScaleValFontHeight));
end;

procedure TMySvgTreeBox.DrawGeoDataPanel(aRect: TRect; aData: TGeoData);
var
  Temp: String;
  p: array [0..2] of TPoint;
begin
  WriteLn(FPanelsSvgFile, '<g class=' + dblq + 'geo-data' + dblq + ' name=' + dblq + aData.ShortName + dblq + ' >');
  p[0].X := aRect.Left;
  p[0].Y := aRect.Bottom;
  p[1].X := aRect.Right;
  p[1].Y := aRect.Bottom;
  p[2].X := aRect.Right;
  p[2].Y := aRect.Top;
  WriteLn(FPanelsSvgFile, PointsToSvgLine(p, '#aaaaaa', '1'));
  WriteLn(FPanelsSvgFile, TextToSvgText(aRect.Right + 60, ((aRect.Bottom - aRect.Top) div 2) + (FSvgFontHeight div 2) +1, aData.Name, FPanelTitleTextAttribs));
  FGeoDataChartFormatter.ChartHeight := FGeoDataPanelHeight;
  FGeoDataChartFormatter.ChartMargins := Rect(0, FGeoDataPanelMargin, 0, 0);
  FGeoDataChartFormatter.BuildAreaChart(aData, FRoot.height, 0.0, aRect, (aData = FLuminosityData) and (not FDoLogScale));
  DrawGeoDataPanelScale(aRect, aData);
  DrawGeoDataPanelDataAreaChart(aRect, aData);
  WriteLn(FPanelsSvgFile, '</g>');
end;

procedure TMySvgTreeBox.DrawGeoDataPanelScale(aRect: TRect; aData: TGeoData);
const
  TICK_WIDTH = 5;
var
  MaxVal: Double;
  MinVal: Double;
  LinePoints: array[0..1] of TPoint;
  x,y: Integer;
  aText: String;
begin
  MaxVal := aData.GetMaxValueInRange(FRoot.height, 0.0);
  MinVal := FGeoDataChartFormatter.YAxisMinValue;
  { draw the top tick}
  LinePoints[0].X := aRect.Right + 2;
  LinePoints[0].Y := aRect.Top;
  LinePoints[1].X := aRect.Right + 2 + TICK_WIDTH;
  LinePoints[1].Y := aRect.Top;
  WriteLn(FPanelsSvgFile, PointsToSvgLine(LinePoints, '#000', '1'));

  { draw the top value}
  aText := Format('%6.2f', [MaxVal]);
  x := aRect.Right + 14;
  y :=  aRect.Top + (FScaleValFontHeight div 2);
  WriteLn(FPanelsSvgFile, TextToSvgText(x, y, aText, False, FScaleValFontHeight));


  { draw the center tick}
  LinePoints[0].X := aRect.Right + 2;
  LinePoints[0].Y := aRect.Top + Round(FGeoDataPanelHeight / 2);
  LinePoints[1].X := aRect.Right + 2 + TICK_WIDTH;
  LinePoints[1].Y := aRect.Top + Round(FGeoDataPanelHeight / 2);
  WriteLn(FPanelsSvgFile, PointsToSvgLine(LinePoints, 'black', '1'));

  { draw the center value}
  aText := Format('%6.2f' + aData.LegendKey, [MinVal + (MaxVal - MinVal) / 2]);
  y := aRect.Top + Round(FGeoDataPanelHeight / 2) +(FScaleValFontHeight div 4);
  WriteLn(FPanelsSvgFile, TextToSvgText(x, y, aText, False, FScaleValFontHeight));

    { draw the bottom tick}
  LinePoints[0].X := aRect.Right + 2;
  LinePoints[0].Y := aRect.Bottom;
  LinePoints[1].X := aRect.Right + 2 + TICK_WIDTH;
  LinePoints[1].Y := aRect.Bottom;
  WriteLn(FPanelsSvgFile, PointsToSvgLine(LinePoints, 'black', '1'));

  { draw the bottom value}
  aText := Format('%8.2f',[MinVal]);
  y := aRect.Bottom + (FScaleValFontHeight div 4);
  WriteLn(FPanelsSvgFile, TextToSvgText(x, y, aText, False, FScaleValFontHeight));
end;

procedure TMySvgTreeBox.DrawGeoDataPanelData(aRect: TRect; aData: TGeoData);
var
  i: Integer;
  GeoData: TGeoDataElementArray;
  MaxVal: Double;
  Points: array[0..1] of TPoint;
  Attribs: array of TXmlAttribute;
begin

  GeoData := aData.GetElementsArray(FRoot.height, 0.0);
  if Length(GeoData) > 0 then
  begin
    SetLength(Attribs, 2);
    Attribs[0].Name := 'val';
    Attribs[1].Name := 'fill-opacity';
    Attribs[1].Value := '0.5';
    MaxVal := aData.GetMaxValueInRange(FRoot.height, 0.0);
    Points[0].Y := aRect.Bottom-1; { y base}
    for i := 0 to Length(GeoData) - 1 do
    begin
      if GeoData[i].Age > 0 then
      begin
        Points[0].X := MyaToX(GeoData[i].Age);
        Points[1].X := Points[0].X;
        Points[1].Y := Points[0].Y - Round(GeoData[i].Value / MaxVal * (FGeoDataPanelHeight - FGeoDataPanelMargin));
        Attribs[0].Value := Format('%.6f', [GeoData[i].Value]);
        WriteLn(FPanelsSvgFile, PointsToSvgLine(Points, Attribs, 'blue', '1.5'));
      end;
    end;
  end;
end;

procedure TMySvgTreeBox.DrawGeoDataPanelDataAreaChart(aRect: TRect; aData: TGeoData);
var
  PolygonCoords: TArrayOfTPoint;
  Attribs: array of TXmlAttribute;
begin
  PolygonCoords := FGeoDataChartFormatter.PointsArrayForAreaChart;
  SetLength(Attribs, 4);
  Attribs[0].Name := 'fill';
  Attribs[0].Value := aData.ChartColor;
  Attribs[1].Name := 'fill-opacity';
  Attribs[1].Value := '0.75';
  Attribs[2].Name := 'stroke';
  Attribs[2].Value := aData.ChartColor;
  Attribs[3].Name := 'stroke-width';
  Attribs[3].Value := '1';
  WriteLn(FPanelsSvgFile, PointsToSvgPolygon(PolygonCoords, Attribs));
end;

procedure TMySvgTreeBox.DrawTimescale;
var
  aRect: TRect;
  Renderer: TTimescaleRenderer=nil;
  FGeologicTimes: TArrayOfGeologicTime;
  SvgStrings: TStringList=nil;
  i: Integer;
  titleCoords: TPoint;
begin
  try
    Renderer := TTimescaleRenderer.Create;
    Renderer.MapMyaToCoordsFunc := MyaToX;
    if DoLogScale then
      Renderer.IsLogScale := True;
    aRect := TimescaleCoords;
    if ShowEpochsAndAges then
      FGeologicTimes := FGeoTimescale.GetTimescale(FRoot.height, tstAge)
    else
      FGeologicTimes := FGeoTimescale.GetTimescale(FRoot.height, tstPeriod);
    if FIsMobileFriendly then
      SvgStrings := Renderer.Render(aRect, FRoot.height, FGeologicTimes, FSvgTreeImageWidth)
    else
      SvgStrings := Renderer.Render(aRect, FRoot.height, FGeologicTimes);
    if SvgStrings.Count > 0 then
      for i := 0 to SvgStrings.Count - 1 do
        WriteLn(FPanelsSvgFile, SvgStrings[i]);
    titleCoords := TimescaleTitleCoords;
    if FIsMobileFriendly then
      WriteLn(FPanelsSvgFile, TextToSvgText(titleCoords.X, titleCoords.Y, 'Time (MYA)', FPanelTitleTextAttribs, FSvgTreeImageWidth))
    else
      WriteLn(FPanelsSvgFile, TextToSvgText(titleCoords.X, titleCoords.Y, 'Time (MYA)', FPanelTitleTextAttribs));
  finally
    if Assigned(Renderer) then
      Renderer.Free;
    if Assigned(SvgStrings) then
      SvgStrings.Free;
  end;
end;

function TMySvgTreeBox.MyaToX(mya: Double): Integer;
begin
  if mya > FRoot.height then
  begin
    Result := FRoot.Position.x + Fxbase;
    Exit;
  end;
  if DoLogScale then
  begin
    Result := HeightScaledToLog10(mya+1) + fxbase;
    if Result < FRoot.Position.x + FxBase then
      Result := FRoot.Position.x + Fxbase;
  end
  else
  begin
    if mya*Fxunit < FXofTaxaNodes then
      Result := Round(FXofTaxaNodes - mya*Fxunit) + Fxbase
    else
      Result := FRoot.position.x + Fxbase;
  end;
end;

function TMySvgTreeBox.XToMya(x: Integer): Double;
var
  aWidth: Integer;
  Proportion: Double;
begin
  aWidth := (FNode[1].position.x - FRoot.position.x);
  if x <= FRoot.position.x + fxbase then
  begin
    Result := FRoot.height;
    Exit;
  end
  else if x >= (aWidth + fxbase) then
  begin
    Result := 0.0;
    Exit;
  end;

  if DoLogScale then
    Result := power(10, ((-1*log10(FRoot.height)*(x-fxbase-aWidth+1))/aWidth))
  else
  begin
    Assert(False, 'not implemented');
    Proportion := x/aWidth;
    Result := aWidth - Proportion*aWidth;
  end;
end;

function TMySvgTreeBox.ImpactsPanelCoords: TRect;
begin
  Assert(Assigned(FRoot));
  Result.Left := FRoot.position.x + Fxbase;
  Result.Right := FXofTaxaNodes + Fxbase;
  Result.Top := FPanelHeaderHeight;
  Result.Bottom := Result.Top + FGeoDataPanelHeight;
end;

procedure TMySvgTreeBox.SetDoNewick(AValue: Boolean);
begin
  if FDoNewick=AValue then Exit;
  FDoNewick:=AValue;
end;

procedure TMySvgTreeBox.SetPanelHeight(AValue: Integer);
begin
  if FGeoDataPanelHeight=AValue then Exit;
  FGeoDataPanelHeight:=AValue;
end;

function TMySvgTreeBox.TimescaleCoords: TRect;
begin
  Result.Left := FRoot.position.x + Fxbase;
  Result.Right := FXofTaxaNodes + Fxbase;
  Result.Top := 0;
  Result.Bottom := Result.Top + FTimescaleHeight;
end;

function TMySvgTreeBox.O2PanelCoords: TRect;
begin
  Result.Left := FRoot.position.x + Fxbase;
  Result.Right := FXofTaxaNodes + Fxbase;
  Result.Top := FPanelHeaderHeight;
  Result.Bottom := Result.Top + FGeoDataPanelHeight;
end;

function TMySvgTreeBox.CO2PanelCoords: TRect;
begin
  Result.Left := FRoot.position.x + Fxbase;
  Result.Right := FXofTaxaNodes + Fxbase;
  Result.Top := FPanelHeaderHeight;
  Result.Bottom := Result.Top + FGeoDataPanelHeight;
end;

function TMySvgTreeBox.LuminosityPanelCoords: TRect;
begin
  Result.Left := FRoot.position.x + Fxbase;
  Result.Right := FXofTaxaNodes + Fxbase;
  Result.Top := FPanelHeaderHeight;
  Result.Bottom := Result.Top + FGeoDataPanelHeight;
end;

function TMySvgTreeBox.GeoTimescaleTitleCoords: TPoint;
begin
  Result.X := FXofTaxaNodes + Fxbase + 60;
  if ShowEpochsAndAges then
    Result.Y := Round(FGeoTimescaleHeight *5 / 2 + FScaleValFontHeight / 2)
  else
    Result.Y := Round(FGeoTimescaleHeight*3 / 2 + FScaleValFontHeight / 2)
end;

function TMySvgTreeBox.TimescaleTitleCoords: TPoint;
begin
  Result.X := FXofTaxaNodes + FxBase + 60;
  Result.Y := Round(FTimescaleHeight / 2 + FScaleValFontHeight / 2);
end;

procedure TMySvgTreeBox.ChangeAttribute(i: Integer);
begin
  if i < 0 then
    CurAttrib := GroupAttrib[-(i+1)]
  else
    CurAttrib := AttribList[i];
end;

procedure TMySvgTreeBox.SetImageHeight(AValue: Integer);
begin
  if FImageHeight=AValue then Exit;
  FImageHeight:=AValue;
end;

procedure TMySvgTreeBox.SetImageWidth(AValue: Integer);
begin
  if FImageWidth=AValue then Exit;
  FImageWidth:=AValue;
  SetTreeWidth(AValue);
end;

procedure TMySvgTreeBox.UpdateFXCoordinates;
var
  i: Integer;
begin
  FXofTaxaNodes := 0;
  FXofRootNode := FRoot.position.X;
  if NoOfOTUs > 0 then
    for i := 1 to NoOfNodes do
      if FNode[i].OTU then
        if FNode[i].position.X > FXofTaxaNodes then
          FXOfTaxaNodes := FNode[i].position.X;
end;

function TMySvgTreeBox.PointsToSvgLine(Points: array of TPoint; aColor: String; aWidth: String): String;
var
  i: Integer;
begin
  Result := '<polyline points=' + DBLQ;
  for i := 0 to Length(Points) - 1 do
    Result := Result + IntToStr(Points[i].X) + ',' + IntToStr(Points[i].Y) + ' ';
  Result := Trim(Result) + DBLQ + ' ';
  Result := Result + 'fill=' + dblq + 'none' + dblq + ' ';
  Result := Result + 'stroke=' + DBLQ + aColor + DBLQ + ' stroke-width=' + DBLQ + aWidth + DBLQ + ' stroke-linecap=' + DBLQ + 'square' + DBLQ + '/>';
end;

function TMySvgTreeBox.PointsToSvgLine(Points: array of TPoint; Attributes: array of TXmlAttribute; aColor: String; aWidth: String): String;
var
  i: Integer;
begin
  Result := '<polyline points=' + DBLQ;
  for i := 0 to Length(Points) - 1 do
    Result := Result + IntToStr(Points[i].X) + ',' + IntToStr(Points[i].Y) + ' ';
  Result := Trim(Result) + DBLQ + ' ';
  if Length(Attributes) > 0 then
  for i := 0 to Length(Attributes) - 1 do
    Result := Result + Attributes[i].Name + '=' + dblq + Attributes[i].Value + dblq + ' ';
  Result := Result + 'fill=' + dblq + 'none' + dblq + ' ';
  Result := Result + 'stroke=' + DBLQ + aColor + DBLQ + ' stroke-width=' + DBLQ + aWidth + DBLQ + ' stroke-linecap=' + DBLQ + 'square' + DBLQ + '/>';
end;

function TMySvgTreeBox.PointsToSvgPolygon(Points: array of TPoint; Attributes: array of TXmlAttribute): String;
var
  i: Integer;
begin
  Result := '<polygon points=' + DBLQ;
  for i := 0 to Length(Points) - 1 do
    Result := Result + IntToStr(Points[i].X) + ',' + IntToStr(Points[i].Y) + ' ';
  Result := Trim(Result) + DBLQ + ' ';
  if Length(Attributes) > 0 then
  for i := 0 to Length(Attributes) - 1 do
    Result := Result + Attributes[i].Name + '=' + dblq + Attributes[i].Value + dblq + ' ';
  Result := Result + '/>';
end;

function TMySvgTreeBox.TextToSvgText(x, y: Integer; aText: String; Attributes: array of TXMLAttribute; aScalingFactor: Integer = 0): String;
var
  i: Integer;
  tempStr: String = '';
begin
  if aScalingFactor <> 0 then
    tempStr := Format('%s%.1f%%%s', [DBLQ, x/aScalingFactor*100, DBLQ])
  else
    tempStr := Format('%s%d%s', [DBLQ, x, DBLQ]);
  Result := '<text x=' + tempStr + ' ';
  Result := Result + 'y=' + DBLQ + IntToStr(y) + DBLQ + ' ';
  if Length(Attributes) > 0 then
  begin
    for i := 0 to Length(Attributes) - 1 do
      Result := Result + Attributes[i].Name + '=' + dblq + Attributes[i].Value + dblq + ' ';
  end;
  Result := Result + 'font-family=' + DBLQ + FSvgFont + DBLQ + '>';
  Result := Result + HtmlEntities(aText) + '</text>';
end;

function TMySvgTreeBox.TextToSvgText(x, y: Integer; aText: String; doScaleWidth: Boolean; aHeight: Integer=16; aColor: String='black'): String;
var
  tempStr: String = '';
begin
  if FIsMobileFriendly then
    tempStr := Format('%s%.1f%%%s', [DBLQ, x/FSvgTreeImageWidth*100, DBLQ])
  else
    tempStr := Format('%s%d%s', [DBLQ, x, DBLQ]);
  Result := '<text x=' + tempStr + ' ';
  Result := Result + 'y=' + DBLQ + IntToStr(y) + DBLQ + ' ';
  Result := Result + 'fill=' + DBLQ + aColor + DBLQ + ' ';
  Result := Result + 'font-size=' + DBLQ + IntToStr(aHeight) + DBLQ + ' ';
  Result := Result + 'font-family=' + DBLQ + FSvgFont + DBLQ + '>';
  Result := Result + HtmlEntities(aText) + '</text>';
end;

function TMySvgTreeBox.AddSvgTextToRect(aRect: TRect; aText: String; Attributes: array of TXMLAttribute; aScalingFactor: Integer =  0): String;
var
  aWidth: Integer;
  tWidth: Integer;
  MaxWidth: Integer;
  Temp: String;
  x, y: Integer;
begin
  Result := EmptyStr;
  aWidth := aRect.Right - aRect.Left;
  if aWidth < 24 then
    Exit;
  if CustomTextWidth(aText) > aWidth then
  begin
    MaxWidth := Round(aWidth / 8);
    Temp := Mince(aText, MaxWidth);
  end
  else
    Temp := aText;
  tWidth := CustomTextWidth(Temp);
  x := aRect.Left + Round((aWidth - tWidth) * 0.5);
  y := Round(aRect.Bottom - FGeoTimescaleHeight *0.125);
  if (Temp[1] <> '.') and (aWidth > tWidth) then
    Result := TextToSvgText(x, y, Temp, Attributes, aScalingFactor)
  else
    Result := EmptyStr;
end;

function TMySvgTreeBox.AddVerticalSvgTextToRect(aRect: TRect; aText: String;
  fHeight: String; Attributes: array of TXMLAttribute): String;
var
  aWidth, aHeight: Integer;
  tHeight, tWidth: Integer;
  i, x, y: Integer;
begin
  Result := EmptyStr;
  aWidth := aRect.Right - aRect.Left;
  aHeight := aRect.Bottom - aRect.Top;
  tHeight := CustomTextHeight(aText, StrToInt(fHeight));
  if tHeight > aWidth then
    Exit;
  tWidth := CustomTextWidth(aText);
  x := aRect.Left + (aWidth - Round((aWidth - tHeight) * 0.5));
  y := Min(aRect.Top + aHeight - 2, aRect.Bottom - Round((aHeight - tWidth) *0.5));

  Result := '<text x=' + DBLQ + IntToStr(x) + DBLQ + ' ';
  Result := Result + 'y=' + DBLQ + IntToStr(y) + DBLQ + ' ';
  Result := Result + 'transform=' + dblq + 'rotate(270 ' + IntToStr(x) + ',' + IntToStr(y) + ')' + dblq + ' ';
  if Length(Attributes) > 0 then
    for i := 0 to Length(Attributes) - 1 do
      Result := Result + Attributes[i].Name + '=' + dblq + Attributes[i].Value + dblq + ' ';
  Result := Result + 'font-size=' + DBLQ + fHeight + DBLQ + ' ';
  Result := Result + 'font-family=' + DBLQ + FSvgFont + DBLQ + '>';
  Result := Result + aText + '</text>';
end;

function TMySvgTreeBox.GeoTimescaleHeight: Integer;
begin
  if ShowEpochsAndAges then
    Result := 5 * FGeoTimescaleHeight
  else
    Result := 3 * FGeoTimescaleHeight;
end;

constructor TMySvgTreeBox.Create;
begin
  inherited Create;
  FGeoTimescale := TCompositeGeologicTime.Create;
  FGeoDataChartFormatter := TGeoDataChartFormatter.Create;
  FGeoDataChartFormatter.MapAgeToCoordFunc := MyaToX;
  FScaleValFontHeight := 12;
  FPanelsMargin := 15;
  FGeoDataPanelMargin:= 2;
  FGeoTimescaleHeight := 20;
  FGeoScaleFontHeight:= 10;
  FPanelHeaderHeight := 5;
  FGeoDataPanelHeight := 100 + FPanelHeaderHeight;
  FTimescaleHeight := 35;
  FEarthImpacts := nil;
  FO2Data := nil;
  FCO2Data := nil;
  FLuminosityData := nil;
end;

destructor TMySvgTreeBox.Destroy;
begin
  if Assigned(FGeoTimescale) then
    FGeoTimescale.Free;
  if Assigned(FGeoDataChartFormatter) then
    FGeoDataChartFormatter.Free;
  if Assigned(FEarthImpacts) then
    FEarthImpacts.Free;
  if Assigned(FO2Data) then
    FO2Data.Free;
  if Assigned(FCO2Data) then
    FCO2Data.Free;
  if Assigned(FLuminosityData) then
    FLuminosityData.Free;
  inherited Destroy;
end;

procedure TMySvgTreeBox.SetEarthImpacts(Impacts: TEarthImpacts);
begin
  FEarthImpacts := Impacts;
end;

procedure TMySvgTreeBox.SetO2Data(aData: TGeoData);
begin
  FO2Data := aData;
end;

procedure TMySvgTreeBox.SetCO2Data(aData: TGeoData);
begin
  FCO2Data := aData;
end;

procedure TMySvgTreeBox.SetLuminosityData(aData: TGeoData);
begin
  FLuminosityData := aData;
end;

procedure TMySvgTreeBox.DrawTree(HMF: THandle);
var
  aList: TStringList=nil;
  aWidth: Integer=0;
  treeHeight: Integer=0;
  panelsHeight: Integer=0;
  y: Integer=0;
  agesHeight: Integer=0;
  epochsHeight: Integer=0;
  taxonColWidth: Integer;
  tag: String = '';
begin
  if FRenderNewickOnly then
  begin
    DrawNewickOnly;
    Exit;
  end;

  InitPanelTitleTextAttribs;
  if (not ShowTopologyOnly) and ShowEpochsAndAges then
  begin
    agesHeight := FGeoTimescaleHeight;
    epochsHeight := FGeoTimescaleHeight;
  end;
  taxonColWidth := TaxaNamesColumnWidth;

  if TrySetTreeWidth(Round((ImageWidth - taxonColWidth)/ 4)) then
    aWidth := ImageWidth
  else
    aWidth := treeWidth * 4 + taxonColWidth + 20;
  ShowScale := False;
  if (not IsStudyTree) or (not ShowTopologyOnly) then
    IsLinearized := True;
  BuildNoDraw;
  if DoLogScale then
    ApplyLogScale;
  treeHeight := MinHeight * 4;
  panelsHeight := FPanelsMargin + 5 *(FGeoDataPanelHeight + FPanelsMargin) + GeoTimescaleHeight;
  if ShowTopologyOnly and IsStudyTree then
  begin
    y := 40;
    OpenSvg('wrapper', aWidth, 200, y, FPanelsSvgFile);
    OpenSvg('panels', aWidth, 200, 20, FPanelsSvgFile, 'scale');
    tag := Format('<text x="%d" y="%d" class="topology_only">Tree is not drawn to scale due to some nodes not having time estimates</text>', [Fxbase + 30, 10]);
    WriteLn(FPanelsSvgFile, tag);
    tag := Format('<circle cx="%d" cy="%d" r="4" fill="%s" stroke="#000" stroke-width="1" />%s<text class="topology_only" x="%d" y="%d">denotes timed nodes (click a timed node to view time)</text>', [Fxbase + 20, 30, FSvgTimedNodeColor, LineEnding, Fxbase + 30, 35]);
    WriteLn(FPanelsSVgFile, tag);
    CloseSvg(FPanelsSvgFile);
    CloseSvg(FPanelsSvgFile);
  end;

  try
    UpdateFXCoordinates;
    OpenSvg('tree', aWidth, treeHeight, y, FTreeSVGFile);
    WriteLn(FTreeSVGFile, '<defs>');
    DrawBackground;
    DrawBranches(aWidth);
    WriteLn(FTreeSVGFile, '</defs>');
    AddTreeSvgUseTags;
    CloseSvg(FTreeSVGFile);

    if not (ShowTopologyOnly and IsStudyTree) then
    begin
      OpenSvg('wrapper', aWidth, panelsHeight, y, FPanelsSvgFile);
      OpenSvg('timetree-geo-timescales', aWidth, agesHeight + epochsHeight + 3*FGeoTimescaleHeight, 0, FPanelsSvgFile);
      if ShowEpochsAndAges then
      begin
        OpenSvg('ages', aWidth, agesHeight, y, FPanelsSvgFile, TreeName + '-ages');
        OpenGroupDefs(FPanelsSvgFile, 'timescale-ages', 'geo-timescale');
        y := y + DrawGeologicLevel(tstAge);
        DrawGeologicLevelTopBorder;
        CloseGroupDefs(FPanelsSvgFile, aWidth, agesHeight, 'timescale-ages');
        CloseSvg(FPanelsSvgFile);

        OpenSvg('epochs', aWidth, epochsHeight, y, FPanelsSvgFile, TreeName + '-epochs');
        OpenGroupDefs(FPanelsSvgFile, 'timescale-epochs', 'geo-timescale');
        y := y + DrawGeologicLevel(tstEpoch);
        CloseGroupDefs(FPanelsSvgFile, aWidth, epochsHeight, 'timescale-epochs');
        CloseSvg(FPanelsSvgFile);
      end;
      OpenSvg('periods', aWidth, FGeoTimescaleHeight, y, FPanelsSvgFile, TreeName + '-periods');
      OpenGroupDefs(FPanelsSvgFile, 'timescale-periods', 'geo-timescale');
      DrawGeologicLevel(tstPeriod);
      if not ShowEpochsAndAges then
        DrawGeologicLevelTopBorder;
      CloseGroupDefs(FPanelsSvgFile, aWidth, FGeoTimescaleHeight, 'timescale-periods');
      CloseSvg(FPanelsSvgFile);

      y := y + FGeoTimescaleHeight;
      OpenSvg('eras', aWidth, FGeoTimescaleHeight, y, FPanelsSvgFile, TreeName + '-eras');
      OpenGroupDefs(FPanelsSvgFile, 'timescale-eras', 'geo-timescale');
      DrawGeologicLevel(tstEra);
      CloseGroupDefs(FPanelsSvgFile, aWidth, FGeoTimescaleHeight, 'timescale-eras');
      CloseSvg(FPanelsSvgFile);

      y := y + FGeoTimescaleHeight;
      OpenSvg('eons', aWidth, FGeoTimescaleHeight, y, FPanelsSvgFile, TreeName + '-eons');
      OpenGroupDefs(FPanelsSvgFile, 'timescale-eons', 'geo-timescale');
      DrawGeologicLevel(tstEon, True);
      CloseGroupDefs(FPanelsSvgFile, aWidth, FGeoTimescaleHeight, 'timescale-eons');
      CloseSvg(FPanelsSvgFile);
      DrawGeologicTimescaleTitle;
      CloseSvg(FPanelsSvgFile);

      y := y + FGeoTimescaleHeight;
      OpenSvg('timescale', aWidth, FTimescaleHeight, y, FPanelsSvgFile);
      OpenGroupDefs(FPanelsSvgFile, 'timescale', 'timescale');
      DrawTimescale;
      CloseGroupDefs(FPanelsSvgFile, aWidth, FTimescaleHeight, 'timescale');
      CloseSvg(FPanelsSvgFile);

      y := y + FTimescaleHeight + (FPanelsMargin);
      OpenSvg('impacts', aWidth, FGeoDataPanelHeight + FPanelsMargin, y, FPanelsSvgFile);
      OpenGroupDefs(FPanelsSvgFile, 'earth-impacts', 'earth-impacts');
      DrawEarthImpacts;
      CloseGroupDefs(FPanelsSvgFile, aWidth, FGeoDataPanelHeight + FPanelsMargin, 'earth-impacts');
      CloseSvg(FPanelsSvgFile);

      y := y +FGeoDataPanelHeight + FPanelsMargin;
      OpenSvg('o2', aWidth, FGeoDataPanelHeight + FPanelsMargin, y, FPanelsSvgFile);
      OpenGroupDefs(FPanelsSvgFile, 'o2', 'geo-data');
      DrawGeoDataPanel(O2PanelCoords, FO2Data);
      CloseGroupDefs(FPanelsSvgFile, aWidth, FGeoDataPanelHeight + FPanelsMargin, 'o2');
      CloseSvg(FPanelsSvgFile);

      y := y + FGeoDataPanelHeight + FPanelsMargin;
      OpenSvg('co2', aWidth, FGeoDataPanelHeight + FPanelsMargin, y, FPanelsSvgFile);
      OpenGroupDefs(FPanelsSvgFile, 'co2', 'geo-data');
      DrawGeoDataPanel(CO2PanelCoords, FCO2Data);
      CloseGroupDefs(FPanelsSvgFile, aWidth, FGeoDataPanelHeight + FPanelsMargin, 'co2');
      CloseSvg(FPanelsSvgFile);

      y := y + FGeoDataPanelHeight + FPanelsMargin;
      OpenSvg('luminosity', aWidth, FGeoDataPanelHeight + FPanelsMargin, y, FPanelsSvgFile);
      OpenGroupDefs(FPanelsSvgFile, 'luminosity', 'geo-data');
      DrawGeoDataPanel(LuminosityPanelCoords, FLuminosityData);
      CloseGroupDefs(FPanelsSvgFile, aWidth, FGeoDataPanelHeight + FPanelsMargin, 'luminosity');
      CloseSvg(FPanelsSvgFile);
      CloseSvg(FPanelsSvgFile);
    end;
  except
    on E:Exception do
    begin
      WriteLn('Dammit man!: '+ E.Message);
      aList := DumpExceptionCallStack(E);
      WriteLn(aList.Text);
      aList.Free;
    end;
  end;
end;

procedure TMySvgTreeBox.DrawNewickOnly;
var
  aList: TStringList=nil;
  aWidth: Integer=0;
  treeHeight: Integer=0;
  y: Integer=0;
  taxonColWidth: Integer;
begin
  taxonColWidth := TaxaNamesColumnWidth;
  GeoScaleForBgColors := gbcNone;
  if TrySetTreeWidth(Round((ImageWidth - taxonColWidth)/ 4)) then
    aWidth := ImageWidth
  else
    aWidth := treeWidth * 4 + taxonColWidth + 20;
  ShowScale := False;
  BuildNoDraw;
  treeHeight := MinHeight * 4;
  try
    UpdateFXCoordinates;
    OpenSvg('tree', aWidth, treeHeight, y, FTreeSVGFile);
    WriteLn(FTreeSVGFile, '<defs>');
    DrawBackground;
    DrawBranches(aWidth);
    WriteLn(FTreeSVGFile, '</defs>');
    AddTreeSvgUseTags;
    CloseSvg(FTreeSVGFile);
  except
    on E:Exception do
    begin
      WriteLn('Dammit man!: '+ E.Message);
      aList := DumpExceptionCallStack(E);
      WriteLn(aList.Text);
      aList.Free;
    end;
  end;
end;

procedure TMySvgTreeBox.GenerateSvgStrings(TreeFileName: String; PanelsFileName: String; mobileFriendly: Boolean);
begin
  try
    FIsMobileFriendly := mobileFriendly;
    AssignFile(FTreeSvgFile, TreeFileName);
    Rewrite(FTreeSvgFile);
    AssignFile(FPanelsSvgFile, PanelsFileName);
    Rewrite(FPanelsSvgFile);
    DrawTree(0);
  finally
    CloseFile(FTreeSvgFile);
    CloseFile(FPanelsSvgFile);
  end;
end;

procedure TMySvgTreeBox.GenerateSvgStrings(TreeFileName: String; mobileFriendly: Boolean);
begin
  try
    FIsMobileFriendly := mobileFriendly;
    FRenderNewickOnly := True;
    AssignFile(FTreeSvgFile, TreeFileName);
    Rewrite(FTreeSvgFile);
    DrawTree(0);
  finally
    CloseFile(FTreeSvgFile);
  end;
end;

procedure TMySvgTreeBox.OpenSVG(aName: String; aWidth, aHeight: Integer; y: Integer; var aFile: TextFile; id: String='');
var
  Temp: String;
  widthStr: String = '';
begin
  if FIsMobileFriendly then
    widthStr := '100%'
  else
    widthStr := IntToStr(aWidth);
  Temp := '<svg xmlns=' + DBLQ + 'http://www.w3.org/2000/svg' + DBLQ + ' xmlns:xlink=' + DBLQ + 'http://www.w3.org/1999/xlink' + DBLQ + ' version=' + DBLQ + '1.1' + DBLQ + ' ';
  Temp := Temp + 'width=' + DBLQ + widthStr + DBLQ + ' height=' + DBLQ + IntToStr(aHeight) + DBLQ + ' ';
  Temp := Temp + 'name=' + dblq + aName + dblq + ' ';
  Temp := Temp + 'y=' + dblq + IntToStr(y) + dblq + ' ';
  if id <> EmptyStr then
    Temp := Temp + 'id=' + dblq + id + dblq + ' ';
  if not FIsMobileFriendly then
    Temp := Temp + 'viewBox=' + DBLQ + '0 0 ' + IntToStr(aWidth) + ' ' + IntToStr(aHeight) + DBLQ;
  Temp := Temp + '>';
  WriteLn(aFile, Temp);
end;

procedure TMySvgTreeBox.CloseSVG(var aFile: TextFile);
begin
  WriteLn(aFile, '</svg>');
end;

procedure TMySvgTreeBox.OpenGroupDefs(var aFile: TextFile; groupId: String; className: String);
begin
  WriteLn(aFile, '<defs>');
  if className <> EmptyStr then
    WriteLn(aFile, Format('<g id="%s" class="%s">', [groupId, className]))
  else
    WriteLn(aFile, Format('<g id="%s">', [groupId]));
end;

procedure TMySvgTreeBox.CloseGroupDefs(var aFile: TextFile; aWidth: Integer; aHeight: Integer; groupId: String);
begin
  WriteLn(aFile, '</g>');
  WriteLn(aFile, '</defs>');
  WriteLn(aFile, Format('<svg viewBox="0 0 %d %d" preserveAspectRatio="none">', [aWidth, aHeight]));
  WriteLn(aFile, Format('<use href="#%s"/>', [groupId]));
  WriteLn(aFile, '</svg>');
end;

procedure TMySvgTreeBox.UpdateTaxaOrder(InputIds: TLongIntList);
var
  i: Integer;
  TimetreeId: String;
  TaxonName: String;
  TaxonIndex: Integer;
  Index: Integer;

  procedure ProcessNode(aNode: TpNode);
  begin
    if not aNode.OTU then
    begin
      ProcessNode(aNode.des1);
      ProcessNode(aNode.des2);
      aNode.minOtu := min(aNode.des1.minOtu, aNode.des2.minOtu);
    end;

  end;

begin
  Index := 0;
  for i := 0 to InputIds.Count - 1 do
  begin
    TimetreeId := IntToStr(InputIds[i]);
    if NamesMap.Contains(TimetreeId) then
    begin
      TaxonName := TOtuName(NamesMap[TimetreeId]).Name;
      TaxonIndex := TreeList.OTUNameList.IndexOf(TaxonName);
      if TaxonIndex >= 0 then
      begin
        FNode[TaxonIndex + 1].minOtu := Index;
        inc(Index)
      end;
    end;
  end;
  ProcessNode(FRoot);
end;

///////////////////////////////////////////////////////
// TCustomSvgTree
///////////////////////////////////////////////////////

constructor TCustomSvgTree.Create;
begin
    inherited Create;
    FIsMobileFriendly := True;
    FCircleTags := TStringList.Create;
    FTaxaNameTags := TStringList.Create;
    FRenderNewickOnly := False;
    FUserSuppliedATaxaList := False;
    FDoLogScale := False;
    FDeepestRank := trUnknown;
    FShowDivergenceTimes := False;
    FBranchPenWidth := 1;
    FScaleFontHeight := 8;
    FStatsFontHeight := 8;
    FTimesFontHeight := 8;
    FBLensFontHeight := 8;
    FFontTarget := tftOtu;
    FSvgLineColor := '#000';
    FSvgFontColor := '#000';
    FSvgDisabledNodeColor := 'none';
    FSvgTimedNodeColor := '#2ecc71';
    FSvgFontHeight := 16;
    //FSvgFont := 'Helvetica';
    FSvgFont := 'Roboto Condensed';
    FSvgLineWidth := 2;
    FSvgBgColor := 'white';
    HideOverlappingTaxa := False;
    FNodeInfo := TSvgNodeInfo.Create(Self);
    FBranchInfo := TSvgBranchInfo.Create(Self);

    AttribList := TNodeAttribList.Create;
    AttribList.Add(TNodeAttrib.Create);

    FAutoSize := false;
    FTreeExist := false;
    FNoOfOTUs := 0;
    FStatsCutOff := 0;
    FCondenseValue := 50;
    FMaxStats := 0.0;
    FFocusedIndex := 0;
    FFocusedNameIndex := 0;
    FNodeFocused := false;
    FBranchFocused := false;
    FDistanceMatrix := nil;
    FStatsPosition := bipAutomatic;
    FBLenPosition := bipAutomatic;
    FShowStats := true;
    FShowBLen := false;
    FPixelsPerUnit := 0.0;
    FShowScale := true;
    FShowTimeScale := true;
    FFillSubtreeDelta := true;
    FHorzTaxonName := false;
    FAlignCaption := false;
    FShowRoot := true;
    FTreeWidth := 400;
    FMinTreeWidth := 400;
    FRadius := 300;
    FCenterMargin := 20;
    FStartAngle := 0;
    FNameSpacing := 2;
    Fyunit := CustomTextHeight('Abc', 12)*FNameSpacing;
    Fgunit := 8;
    FBLenCutoff := 0.0;
    FBLenDecimals := 0;
    FStatsMargin.X := 12;
    FStatsMargin.Y := 4;
    FTimesMargin.X := 80;
    FTimesMargin.Y := 16;
    GroupAttrib := TNodeAttribList.Create;
    FUseSubtreeAttrib := true;
    FUseGroupAttrib   := true;
    FLinearizeFunc := nil;
end;

destructor TCustomSvgTree.Destroy;
var
  i: integer;
begin
    ResetMem;
    if AttribList.Count > 0 then
      for i := AttribList.Count-1 downto 0 do
        AttribList[i].Free;
    AttribList.Free;
    if GroupAttrib.Count > 0 then
      for i := GroupAttrib.Count-1 downto 0 do
        GroupAttrib[i].Free;
    GroupAttrib.Free;
    FBranchInfo.Free;
    FNodeInfo.Free;
    if Assigned(FCircleTags) then
      FCircleTags.Free;
    if Assigned(FTaxaNameTags) then
      FTaxaNameTags.Free;
    inherited Destroy;
end;

function TCustomSvgTree.ShowEpochsAndAges: Boolean;
begin
  Result := (FRoot.Height <= 542); { for epochs and ages, there is nothing defined past 542 mya}
end;

procedure TCustomSvgTree.GetIntCoords(i: integer; var des1,
  des2: integer);
begin
  des1 := FNode[i].des1.index;
  des2 := FNode[i].des2.index;
end;


function TCustomSvgTree.GetNoOfNodes:integer;
begin
  Result := 2*NoOfOTUs -1;
end;


function TCustomSvgTree.FocusOnPoint(Point: TPoint): Boolean;
begin
  Result := true;
  if not FocusNode(Point.X, Point.Y) then
    if not FocusBranch(Point.X, Point.Y) then
      if not FocusName(Point.X, Point.Y) then
      begin
        Result := false;
        inherited;
      end;
end;

procedure TCustomSvgTree.FocusOnRoot;
begin
  FocusOnNode(FRoot.Index);
end;

procedure TCustomSvgTree.AssignTreeAttrib(Source: TCustomSvgTree);
begin
    if Source.isBranchLength then
      if isBranchLength then
        PixelsPerUnit := Source.PixelsPerUnit
      else
        TreeWidth := Source.TreeWidth
    else
      TreeWidth := Source.TreeWidth;
    PixelsPerOTU := Source.PixelsPerOTU;

    FRadius := Source.FRadius;
    FStartAngle := Source.FStartAngle;
    FCenterMargin := Source.FCenterMargin;

    FBLenCutoff := Source.FBLenCutoff;
    FBLenPosition := Source.FBLenPosition;

    FStatsCutoff := Source.FStatsCutoff;
    FCondenseValue := Source.FCondenseValue;
    FStatsPosition := Source.FStatsPosition;
    FTimesPosition := Source.FTimesPosition;
    FStatsMargin := Source.FStatsMargin;
    FTimesMargin := Source.FTimesMargin;

    ShowOTUName := Source.ShowOTUName;
    ShowOTUMarker := Source.ShowOTUMarker;
    FShowBLen := Source.FShowBLen;
    FShowStats := Source.FShowStats;
    FShowScale := Source.FShowScale;
    FShowTimeScale := Source.FShowTimeScale;
    FShowCharState := Source.FShowCharState;

    FForceLinearized := Source.FForceLinearized;
    if IsDistanceMatrix and Source.IsDistanceMatrix then
      FIsLinearized := Source.FIsLinearized;

    FLinearizeFunc := Source.FLinearizeFunc;

    SetTopologyOnly(Source.FTopologyOnly);
    IsCondensed  := Source.IsCondensed;


    FTreeStyle := Source.FTreeStyle;
    FBranchStyle := Source.FBranchStyle;

    FHorzTaxonName := Source.FHorzTaxonName;
    FAlignCaption := Source.AlignCaption;

    FFillSubtreeDelta := Source.FillSubtreeDelta;

    FUseSubtreeAttrib := Source.FUseSubtreeAttrib;
    FUseGroupAttrib   := Source.FUseGroupAttrib;

    SetAttrIndex;
end;

procedure TCustomSvgTree.SetMarker(index : integer; newstyle : TNodeMarker{TMarkerItem});
begin
    if (index < 1) or (index > NoOfNodes) then Exit;
    FNode[index].marker := newstyle;
end;

function TCustomSvgTree.GetMarker(index : integer):TNodeMarker;//TMarkerItem;
begin
    if (index < 1) or (index > NoOfNodes) then begin
        Result.Shape := msNone;
        Exit;
    end;
    Result := FNode[index].marker;
end;

procedure TCustomSvgTree.SetIsCondensed(value: boolean);
begin
  if not TreeExist then Exit;
  if not IsStats then Exit;
  if value = FIsCondensed then Exit;
  FIsCondensed := value;
  if FIsCondensed then
  begin
    FTopoflag := true;
  end
  else
  begin
    FTopoflag := FTopologyOnly;
  end;
end;

function TCustomSvgTree.GetIsCondensed: boolean;
begin
    Result := FIsCondensed;
end;

procedure TCustomSvgTree.SetTopologyOnly(value : boolean);
begin
  if value = ShowTopologyOnly then Exit;
  FTopologyOnly := value;
  if FTopologyOnly then
  begin
//    Multflag := IsCondensed;
    FTopoflag := true;
  end
  else
  begin
    FTopoflag := false;
    FIsCondensed := false;
//    Multflag := true;
  end;
end;

function TCustomSvgTree.GetTopologyOnly: boolean;
begin
  if IsCondensed then
    Result := true
  else
    Result := FTopologyOnly;
end;

procedure TCustomSvgTree.SetForceLinearized(value: boolean);
begin
  FForceLinearized := value;
end;

function TCustomSvgTree.GetShowOTUName: boolean;
begin
  result := AttribList[0].ShowTaxonName;
end;


procedure TCustomSvgTree.SetShowOTUName(b : boolean);
var
  i: integer;
begin
  if b = ShowOTUName then Exit;
  for i := 0 to AttribList.Count-1 do
    AttribList[i].ShowTaxonName := b;
  if GroupAttrib.Count > 0 then
    for i := 0 to GroupAttrib.Count-1 do
      GroupAttrib[i].ShowTaxonName := b;
end;

function TCustomSvgTree.GetShowOTUMarker: boolean;
begin
  result := AttribList[0].ShowTaxonMarker;
end;

procedure TCustomSvgTree.SetShowOTUMarker(b : boolean);
var
  i: integer;
begin
  if b = ShowOTUMarker then Exit;
  for i := 0 to AttribList.Count-1 do
    AttribList[i].ShowTaxonMarker := b;
  if GroupAttrib.Count > 0 then
    for i := 0 to GroupAttrib.Count-1 do
      GroupAttrib[i].ShowTaxonMarker := b;
end;

procedure TCustomSvgTree.SetBranchStyle(value : TBranchStyle);
begin
    if value = FBranchStyle then Exit;
    FBranchStyle := value;
end;

procedure TCustomSvgTree.SetTreeStyle(value : TTreeStyle);
begin
  if value = FTreeStyle then Exit;
  if (value = tsRadiation) and (not IsBranchLength) then Exit;
  FTreeStyle := value;
end;

procedure TCustomSvgTree.SetFillSubtreeDelta(value: boolean);
begin
  if value = FillSubtreeDelta then Exit;
  FFillSubtreeDelta := value;
end;

procedure TCustomSvgTree.Draw;
begin

end;

function TCustomSvgTree.GetIsStats:boolean;
begin
    Result := FisStats;
end;

function TCustomSvgTree.GetIsTimes: Boolean;
begin
  Result := FIsTimes;
end;

procedure TCustomSvgTree.SetStatsCutoff(value : integer);
begin
    if (value < 0) or (value > 100) then Exit;
    FStatsCutoff := value;
end;

function TCustomSvgTree.GetIsBranchLength:boolean;
begin
    Result := {FisBranchLength and }(FSBL > 0.00000000000001);
end;

procedure TCustomSvgTree.SetBLenCutoff(value : double);
begin
    if (value < 0) or (value > Fxmax) then Exit;
    FBLenCutoff := value;
end;

procedure TCustomSvgTree.SetShowBLen(b : boolean);
begin
    if b = ShowBLen then Exit;
    FShowBLen := b;
end;

function TCustomSvgTree.GetShowBLen:boolean;
begin
    if not (TreeStyle = tsTraditional) then
        Result := false
    else
        Result := FShowBLen;
end;

function TCustomSvgTree.GetIsSE:boolean;
begin
    Result := FisSE;
end;

function TCustomSvgTree.GetShowCharState:boolean;
begin
  if TreeStyle <> tsTraditional then
    Result := false
  else if BranchStyle <> bsRectangular then
    Result := false
  else
    Result := FShowCharState;
end;

procedure TCustomSvgTree.SetFontTarget(aTarget: TFontTarget);
begin
  FFontTarget := aTarget;
end;

procedure TCustomSvgTree.SetStatsPosition(position : TBranchInfoPosition);
begin
    if position = FStatsPosition then Exit;
    FStatsPosition := position;
end;

procedure TCustomSvgTree.SetTimesPosition(Position: TBranchInfoPosition);
begin

end;

procedure TCustomSvgTree.SetStatsMargin(margin : TPoint);
begin
    if (margin.X = FStatsMargin.X) and (margin.Y = FStatsMargin.Y) then Exit;
    FStatsMargin.X := margin.X;
    FStatsMargin.Y := margin.Y;
end;

procedure TCustomSvgTree.SetTimesMargin(Margin: TPoint);
begin

end;

procedure TCustomSvgTree.SetBLenPosition(position : TBranchInfoPosition);
begin
  if position = FBLenPosition then Exit;
  FBLenPosition := position;
end;

procedure TCustomSvgTree.SetBLenDecimals(value : integer);
begin
    if value = BLenDecimals then Exit;
    FBLenDecimals := value;
end;

function TCustomSvgTree.GetTreeWidth:integer;
begin
    if FTopoflag then
        Result := FMinTreeWidth
    else
        Result := FTreeWidth;
end;

procedure TCustomSvgTree.SetTreeWidth(w : integer);
begin
  if not TreeExist then Exit;
  if w = TreeWidth then Exit;
  if w < 80 then
  begin
    w := 80;
  end
  else if w > 32767 then
  begin
    w := 32767;
  end;
  if isBranchLength then
      FPixelsPerUnit := FPixelsPerUnit*w/FTreeWidth;

  if TreeWidth = FMinTreeWidth then
  begin
      FTreeWidth := w;
      FMinTreeWidth := w;
  end
  else
  begin
      FMinTreeWidth := Round(w/FTreeWidth*FMinTreeWidth);
      FTreeWidth := w;
  end;
end;

function TCustomSvgTree.TrySetTreeWidth(w: Integer): Boolean;
begin
  Result := True;
  if not TreeExist then Exit;
  if w = TreeWidth then Exit;
  if w < 80 then
  begin
    w := 80;
    Result := False;
  end
  else if w > 32767 then
  begin
    w := 32767;
    Result := False;
  end;
  if isBranchLength then
      FPixelsPerUnit := FPixelsPerUnit*w/FTreeWidth;

  if TreeWidth = FMinTreeWidth then
  begin
      FTreeWidth := w;
      FMinTreeWidth := w;
  end
  else
  begin
      FMinTreeWidth := Round(w/FTreeWidth*FMinTreeWidth);
      FTreeWidth := w;
  end;
end;

procedure TCustomSvgTree.SetPixelsPerUnit(value : double);
begin
    if not TreeExist then Exit; // Exit if the tree does not exist
    if not isBranchLength then Exit;
    if FPixelsPerUnit = value then Exit; // Exit if we are setting the same value as is currently set
    if value < 0 then exit; // Exit if the input is a negative number
    if value/FPixelsPerUnit*FTreeWidth > 8000 then exit;
    FTreeWidth := Round(value/FPixelsPerUnit*FTreeWidth);
    FMinTreeWidth := Round(value/FPixelsPerUnit*FMinTreeWidth);
    FPixelsPerUnit := value;
end;

procedure TCustomSvgTree.SetClusterWidth(p: TpNode);
begin
  if not TreeExist then Exit;
  if p.OTU then
    p.width := Fyunit
  else if p.compressed then
    if Fgunit*p.size < Fyunit then
      p.width := Fyunit
    else
      p.width := Fgunit*p.size
  else
  begin
    SetClusterWidth(p.des1);
    SetClusterWidth(p.des2);
    p.width := p.des1.width + p.des2.width;
  end;
end;

function TCustomSvgTree.GetTreeHeight:integer;
begin
  Result := FRoot.width div 4;
end;


function TCustomSvgTree.GetCurNoOfOTUs:integer;

  procedure SearchCurOTUs(p: TpNode);
  begin
    if p.OTU or p.compressed then
      Inc(Result)
    else begin
      SearchCurOTUs(p.des1);
      SearchCurOTUs(p.des2);
    end;
  end;

begin
  Result := 0;
  SearchCurOTUs(FRoot);
end;

procedure TCustomSvgTree.SetPixelsPerOTU(value : integer);
begin
  if value < 0 then Exit;
  if value >= 2048 then exit;
  if value = Fyunit div 4 then Exit;
  Fyunit := value*4;
  SetClusterWidth(FRoot);
end;

function TCustomSvgTree.GetPixelsPerOTU:integer;
begin
    Result := Fyunit div 4;
end;

procedure TCustomSvgTree.SetPixelsPerGroupMember(value : integer);
begin
  if value < 0 then Exit;
  if value >= 8192 then exit;
  if value = Fgunit div 4 then Exit;
  Fgunit := value*4;
  SetClusterWidth(FRoot);
end;

function TCustomSvgTree.GetPixelsPerGroupMember:integer;
begin
    Result := Fgunit div 4;
end;

function order(r:double):integer;
begin
    Result := Floor(log10(r));
    if r < 1.0 then
        Dec(Result)
    else
        Inc(Result);
end;

procedure TCustomSvgTree.SetCondenseValue(value : integer);
begin
    if value = CondenseValue then Exit;
    FCondenseValue := value;
end;

function TCustomSvgTree.GetOTUName(i : integer):AnsiString;
begin
    if i = 0 then
        if FocusedIndex = 0 then
            Result := ''
        else
            Result := FNode[FocusedIndex].name
    else
        Result := FNode[i].name;
end;

function TCustomSvgTree.GetCoords(i : integer):AnsiString;
begin
    if (i <> 0) and (FNode[i].des1 <> nil) and (FNode[i].des2 <> nil) then
     Result := '(' + IntToStr(FNode[i].des1.index) + ' . ' + IntToStr(FNode[i].des2.index) + ')';
end;

function TCustomSvgTree.GetOTUOrigionalName(i: integer): AnsiString;
begin
    if i = 0 then
        if FocusedIndex = 0 then
            Result := ''
        else
            Result := FNode[FocusedIndex].oriName
    else
        Result := FNode[i].oriName;
end;

function TCustomSvgTree.GetIndexOfName(Name: AnsiString): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=1 to NoOfOTUs do
  begin
      if AnsiCompareStr(Name, GetOTUName(i)) = 0 then
    begin
      result := i;
      break;
    end;
  end;
end;

function TCustomSvgTree.GetIndexOfOrigionalName(OrigName: AnsiString): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=1 to NoOfOTUs do
  begin
      if AnsiCompareStr(OrigName, GetOTUOrigionalName(i)) = 0 then
    begin
      result := i;
      break;
    end;
  end;
end;

function TCustomSvgTree.GetClusterName(i : integer):AnsiString;
var
  p: TpNode;
begin
  Result := '';
  if i = 0 then
    if FocusedIndex = 0 then
    begin
      Result := '';
      exit;
    end
    else
      i := FocusedIndex;

  if (i > NoOfOTUs) and (FNode[i].anc <> nil) then
  begin
    if (FNode[i].attrindex <> FNode[i].anc.attrindex) and (FNode[i].attrindex > 0) then
      Result := AttribList[FNode[i].attrindex].Caption;
    exit;
  end;
  p := FNode[i].anc;
  while p <> nil do
  begin
    if p.attrindex > 0 then
    begin
      result := AttribList[p.attrindex].Caption;
      break;
    end;
    p := p.anc;
  end;
end;

function TCustomSvgTree.GetGroupName(i : integer):AnsiString;
begin
  result := '';
  if not TreeExist then Exit;
  if GroupAttrib.Count = 0 then Exit;
  if (i > 0) and (i <= NoOfOTUs) then
    if FNode[i].groupindex >= 0 then
      Result := GroupAttrib[FNode[i].groupindex].Name;
end;



procedure TCustomSvgTree.SetClusterName(index: integer; name:AnsiString);
begin
  if not TreeExist then Exit;
  if (index > NoOfOTUs) and (index <= NoOfNodes) then
    FNode[index].name := name;
end;

procedure TCustomSvgTree.ChangeOTUName(Sender: TObject);
begin

end;

function TCustomSvgTree.GetCharState(Index: integer): AnsiString;
begin

end;

function TCustomSvgTree.GetNameFocused:boolean;
begin
  Result := (FocusedNameIndex > 0);
end;


procedure TCustomSvgTree.SetStats;
var i : integer;
begin
  if MaxStats = 0.0 then
  begin
    for i := 1 to NoOfNodes-1 do
      if FNode[i].branch.stats < -0.0000000000001 then
        FNode[i].branch.stat2 := -1
      else if (isRooted and ShowRoot) or (FNode[i] <> FRoot) then
        FNode[i].branch.stat2 := Trunc(FNode[i].branch.stats+0.000000000001);
  end
  else
    for i := 1 to NoOfNodes-1 do
      if (isRooted and ShowRoot) or (FNode[i] <> FRoot) then
        if FNode[i].branch.stats < -0.0000000000001 then
          FNode[i].branch.stat2 := -1
        else
          FNode[i].branch.stat2 := Trunc(FNode[i].branch.stats*100/MaxStats+0.000000000001);
end;

procedure TCustomSvgTree.FlipCluster;
begin
    if (not (NodeFocused or BranchFocused)) or (FNode[FocusedIndex].OTU) then Exit;
    SwapNode(FNode[FocusedIndex].des1, FNode[FocusedIndex].des2);
    Refresh;
end;

procedure TurnNode(p : TpNode);
begin
    if not p.OTU then begin
        SwapNode(p.des1,p.des2);
        TurnNode(p.des1);
        TurnNode(p.des2);
    end;
end;

procedure TCustomSvgTree.FlipAllCluster;
begin
    if (not (NodeFocused or BranchFocused)) or (FocusedIndex <= NoOfOTUs) then Exit;
    TurnNode (FNode[FocusedIndex]);
    Refresh;
end;

procedure TCustomSvgTree.MoveRoot(p: TpNode; midpoint: boolean);
begin
  ChangeRoot(FRoot, p, midpoint);
end;

function TCustomSvgTree.TaxaNamesColumnWidth: Integer;
var
  i: Integer;
  temp: Integer;
begin
  Result := CustomTextWidth('Periods Geologic Timescale---', 10);
  for i := 1 to NoOfOTUs do
  begin
    if not FRenderNewickOnly then
    begin
      if FNode[i].namesize.x > Result then
        Result := FNode[i].Namesize.x + 50;
    end
    else
    begin
      temp := CustomTextWidth(FNode[i].name, 10);
      if temp > Result then
        Result := temp + 50;
    end;
  end;
end;


procedure TCustomSvgTree.MakeRootOnBranch;
var dLen, x0, x1, y0, y1 : double;
begin
    if FocusedIndex = 0 then Exit;
    if FocusedIndex = FRoot.index then Exit;
    if isRooted then Exit;
    if FNode[FocusedIndex].Anc = FRoot then
    begin
      x0 := FRoot.position.x;
      y0 := FRoot.position.y;
      x1 := FNode[FocusedIndex].position.x;
      y1 := FNode[FocusedIndex].position.y;
      if TreeStyle = tsRadiation then
        if (x1 = x0) and (y1 = y0) then
          Exit
        else begin
          dLen := FNode[FocusedIndex].branch.length
                 *sqrt((FMarkedPos.x -x0)*(FMarkedPos.x -x0)+(FMarkedPos.y -y0)*(FMarkedPos.y -y0))
                 /sqrt((x1 -x0)*(x1 -x0)+(y1 -y0)*(y1 -y0));
          if dLen > FNode[FocusedIndex].branch.length then
            dLen := FNode[FocusedIndex].branch.length;
        end
      else
        dLen := FNode[FocusedIndex].branch.length*(FMarkedPos.x -x0)/(x1 -x0);

      if FNode[FocusedIndex] = FRoot.des1 then begin
        FRoot.des2.branch.length := FRoot.des2.branch.length +dLen;
        FRoot.des2.branch.maxlen2 := FRoot.des2.branch.maxlen2 +dLen;
      end
      else if FNode[FocusedIndex] = FRoot.des2 then begin
        FRoot.des1.branch.length := FRoot.des1.branch.length +dLen;
        FRoot.des1.branch.maxlen2 := FRoot.des1.branch.maxlen2 +dLen;
      end;
      FNode[FocusedIndex].branch.length := FNode[FocusedIndex].branch.length -dLen;
      FNode[FocusedIndex].branch.maxlen2 := FNode[FocusedIndex].branch.maxlen2 -dLen;
    end
    else
    begin
      MoveRoot(FNode[FocusedIndex], false);
      SetClusterSize(FRoot);
      SetClusterWidth(FRoot);
//      SortBranchByFigure(FRoot);
    end;
    SetClusterHeight;
    SetAttrindex;

    FocusOnNode(FRoot.Index);
end;

function TCustomSvgTree.SearchMidPoint:integer;
var i : integer;
    r : double;
begin
    Result := FRoot.des1.index;
    for i := 1 to NoOfNodes do begin
        if FNode[i] = FRoot then Continue;
        with FNode[i].branch do begin
            r := length -Abs(maxlen1 - maxlen2);
            if r >= 0.0 then
                Result := i;
        end;
    end;
end;

procedure TCustomSvgTree.MakeRootOnMidPoint;
var bnum : integer;
begin
    if isRooted then Exit;
    if not isBranchLength then Exit;

    ClearFocus;                              // new

    bnum := SearchMidPoint;
    MoveRoot(FNode[bnum],true);
    SetClusterSize(FRoot);
    SetClusterWidth(FRoot);
//    SortBranchByFigure(FRoot);
    SetClusterHeight;
    SetAttrindex;
end;

procedure TCustomSvgTree.MakeRootByOutgroup(OutgroupOnBottom: Boolean);
begin

end;

procedure TCustomSvgTree.MegaPolyline(Points: array of TPoint; NumPoints: Integer; aNode: TpNode=nil);
var
  Temp: String;
  i: Integer;
  aRank: TTaxonomicRank;
  IdStr: String;
  timeStr, ciLowStr, ciHighStr: String;
  dof: String;
  fillColor: String;
  aHeight: Double;

  function CircleXCoordString(aX: Integer): String;
  begin
    if FIsMobileFriendly then
      Result := Format('%s%.1f%%%s', [DBLQ, aX/FSvgTreeImageWidth*100, DBLQ])
    else
      Result := Format('%s%d%s', [DBLQ, aX, DBLQ]);
  end;

begin
  Assert(NumPoints >= 2);
  if NumPoints = 2 then
  begin
    Temp := '<line x1=' + DBLQ + IntToStr(Points[0].X) + DBLQ + ' ';
    Temp := Temp + 'y1=' + DBLQ + IntToStr(Points[0].Y) + DBLQ + ' ';
    Temp := Temp + 'x2=' + DBLQ + IntToStr(Points[1].X) + DBLQ + ' ';
    Temp := Temp + 'y2=' + DBLQ + IntToStr(Points[1].Y) + DBLQ + ' ';
    Temp := Temp + 'stroke=' + DBLQ + FSvgLineColor + DBLQ + ' stroke-width=' + DBLQ + IntToStr(FSvgLineWidth) + DBLQ + {' stroke-linecap=' + DBLQ + 'square' + DBLQ + }'/>';
  end
  else
  begin
    Temp := '<polyline points=' + DBLQ;
    for i := 0 to NumPoints - 1 do
      Temp := Temp + IntToStr(Points[i].X) + ',' + IntToStr(Points[i].Y) + ' ';
    Temp := Trim(Temp) + DBLQ + ' ';
    Temp := Temp + 'fill=' + DBLQ + 'none' + DBLQ + ' ';
    Temp := Temp + 'stroke=' + DBLQ + FSvgLineColor + DBLQ + ' stroke-width=' + DBLQ + IntToStr(FSvgLineWidth) + DBLQ + {' stroke-linecap=' + DBLQ + 'square' + DBLQ +} '/>';
  end;
  WriteLn(FTreeSvgFile, Temp);

  if Assigned(aNode) then
  begin
    aHeight := aNode.height;
    if (aNode.timetreeId >= 0) and (not FRenderNewickOnly) then
      aRank := TaxonomicRanks[aNode.timetreeId]
    else
      aRank := trUnknown;

    Temp := '<circle cx=' + CircleXCoordString(Points[NumPoints - 1].X) + ' ';
    Temp := Temp + 'cy=' + DBLQ + IntToStr(Points[NumPoints - 1].Y) + DBLQ + ' ';
    Temp := Temp + 'r=' + DBLQ + '4' + DBLQ + ' ';
    Temp := Temp + 'stroke=' + DBLQ + FSvgLineColor + DBLQ + ' ';
    Temp := Temp + 'stroke-width=' + DBLQ + '1' + DBLQ + ' ';
    if IsStudyTree and ShowTopologyOnly and (CompareValue(StudyTimeNodeHeights[aNode.index - 1], 0.0, FP_CUTOFF) > 0) then
    begin
      fillColor := FSvgTimedNodeColor;
      aHeight := StudyTimeNodeHeights[aNode.index - 1];
    end
    else if (aRank = trUnknown) and (not IsStudyTree) and (not FRenderNewickOnly) then
      fillColor := FSvgDisabledNodeColor
    else
      fillColor := FSvgLineColor;
    Temp := Temp + 'fill=' + DBLQ + fillColor + DBLQ + ' />';
    FCircleTags.Add(Temp);
    //WriteLn(FTreeSvgFile, Temp);

    { draw a large circle that is not visible but will have the on-click handler}
    Temp := '<circle cx=' + CircleXCoordString(Points[NumPoints - 1].X) + ' ';
    Temp := Temp + 'cy=' + DBLQ + IntToStr(Points[NumPoints - 1].Y) + DBLQ + ' ';
    Temp := Temp + 'r=' + DBLQ + '12' + DBLQ + ' ';
    Temp := Temp + 'class=' + DBLQ + 'node' + DBLQ + ' ';

    Temp := Temp + 'rank=' + dblq + TaxonomicRankToString(aRank) + dblq + ' ';
    if aNode.OTU then
    begin
      Temp := Temp + 'otu=' + dblq + 'true' + dblq + ' ';
      Temp := Temp + 'name=' + dblq + aNode.name + dblq + ' ';
    end
    else
    begin
      FormatTimeIntervalStrings(aHeight, aNode.ciLower, aNode.ciUpper, timeStr, ciLowStr, ciHighStr);
      Temp := Temp + 'cilo=' + dblq + ciLowStr + dblq + ' ';
      Temp := Temp + 'cihi=' + dblq + ciHighStr + dblq + ' ';

      if aNode.IsCI then
        Temp := Temp + 'isci=' + dblq + 'true' + dblq + ' '
      else
        Temp := Temp + 'isci=' + dblq + 'false' + dblq + ' ';
    end;

    Temp := Temp + 'time=' + dblq + timeStr + dblq + ' ';
    Temp := Temp + 'lncount=' + dblq + IntToStr(Max(1, aNode.numLeaves)) + dblq + ' ';
    IdStr := IntToStr(aNode.timetreeId);
    if (not aNode.OTU) and (not FRenderNewickOnly) then
    begin
      if NamesMap.Contains(IdStr) then
        Temp := Temp + 'name=' + dblq + HtmlEntities(TOtuName(NamesMap[IdStr]).Name) + dblq + ' '
      else
      begin
        dof := FindAncestorName(ANode);
        Temp := Temp + 'dof=' + dblq + dof + dblq + ' ';
      end;
    end;

    Temp := Temp + 'id=' + DBLQ + IdStr + DBLQ + '/>';
    if not FRenderNewickOnly then
      FCircleTags.Add(Temp);
      //WriteLn(FTreeSvgFile, Temp);

    if (not FRenderNewickOnly) and (not (aNode = FRoot)) and (aNode.anc.anc = nil) and (aNode = aNode.anc.des1) then { this is descendent 1 of the root node}
    begin
      FormatTimeIntervalStrings(aNode.anc.height, aNode.anc.ciLower, aNode.anc.ciUpper, timeStr, ciLowStr, ciHighStr);
      Temp := '<circle cx=' + CircleXCoordString(Points[0].X) + ' ';
      Temp := Temp + 'cy=' + DBLQ + IntToStr(Points[0].Y) + DBLQ + ' ';
      Temp := Temp + 'r=' + DBLQ + '4' + DBLQ + ' ';
      Temp := Temp + 'fill=' + DBLQ + FSvgLineColor + DBLQ + ' />';
      FCircleTags.Add(Temp);
      //WriteLn(FTreeSvgFile, Temp);

      Temp := '<circle cx=' + CircleXCoordString(Points[0].X) + ' ';
      Temp := Temp + 'cy=' + DBLQ + IntToStr(Points[0].Y) + DBLQ + ' ';
      Temp := Temp + 'r=' + DBLQ + '12' + DBLQ + ' ';
      Temp := Temp + 'class=' + DBLQ + 'node' + DBLQ + ' ';
      aRank := TaxonomicRanks[aNode.anc.timetreeId];
      Temp := Temp + 'rank=' + dblq + TaxonomicRankToString(aRank) + dblq + ' ';
      Temp := Temp + 'time=' + dblq + timeStr + dblq + ' ';
      Temp := Temp + 'cilo=' + dblq + ciLowStr + dblq + ' ';
      Temp := Temp + 'cihi=' + dblq + ciHighStr + dblq + ' ';

      if aNode.anc.IsCI then
        Temp := Temp + 'isci=' + dblq + 'true' + dblq + ' '
      else
        Temp := Temp + 'isci=' + dblq + 'false' + dblq + ' ';

      Temp := Temp + 'lncount=' + dblq + IntToStr(aNode.anc.numLeaves) + dblq + ' ';
      IdStr := IntToStr(aNode.anc.timetreeId);
      if NamesMap.Contains(IdStr) then
        Temp := Temp + 'name=' + dblq + HtmlEntities(TOtuName(NamesMap[IdStr]).Name) + dblq + ' '
      else
      begin
        Temp := Temp + 'name=' + dblq +  dblq + ' ';
      end;
      Temp := Temp + 'id=' + DBLQ + IntToStr(aNode.anc.timetreeId) + DBLQ + '/>';
      FCircleTags.Add(Temp);
      //WriteLn(FTreeSvgFile, Temp);
    end;
  end;
end;

procedure TCustomSvgTree.MegaPolygon(Points: array of TPoint; NumPoints: Integer; aNode: TpNode=nil);
var
  Temp: String;
  i: Integer;
begin
  Assert(NumPoints >= 2);

  if NumPoints = 2 then
  begin
    Temp := '<line x1=' + DBLQ + IntToStr(Points[0].X) + DBLQ + ' ';
    Temp := Temp + 'y1=' + DBLQ + IntToStr(Points[0].Y) + DBLQ + ' ';
    Temp := Temp + 'x2=' + DBLQ + IntToStr(Points[1].X) + DBLQ + ' ';
    Temp := Temp + 'y2=' + DBLQ + IntToStr(Points[1].Y) + DBLQ + ' ';
    Temp := Temp + 'stroke=' + DBLQ + FSvgLineColor + DBLQ + ' stroke-width=' + DBLQ + IntToStr(FSvgLineWidth) + DBLQ + ' stroke-linecap=' + DBLQ + 'square' + DBLQ + '/>';
  end
  else
  begin
    Temp := '<polyline points=' + DBLQ;
    for i := 0 to NumPoints - 1 do
      Temp := Temp + IntToStr(Points[i].X) + ',' + IntToStr(Points[i].Y) + ' ';
    Temp := Trim(Temp) + DBLQ + ' ';
    Temp := Temp + 'fill=' + DBLQ + FSvgLineColor + DBLQ + ' ';
    Temp := Temp + 'stroke=' + DBLQ + FSvgLineColor + DBLQ + ' stroke-width=' + DBLQ + IntToStr(FSvgLineWidth) + DBLQ + ' stroke-linecap=' + DBLQ + 'square' + DBLQ + '/>';
  end;
  WriteLn(FTreeSvgFile, Temp);
end;

procedure TCustomSvgTree.MegaTextOutA(aX: Integer; aY: Integer; aText: PAnsiChar);
var
  Temp: String;
begin
  Temp := '<text x=' + DBLQ + IntToStr(aX + FSvgFontHeight) + DBLQ + ' ';
  Temp := Temp + 'y=' + DBLQ + IntToStr(aY - FSvgFontHeight - (FSvgFontHeight div 2) - 3) + DBLQ + '>';
  Temp := Temp + HtmlEntities(AnsiString(aText)) + '</text>';
  WriteLn(FTreeSvgFile, Temp);
end;

procedure TCustomSvgTree.MegaTextOutA(aX: Integer; aY: Integer; aText: String);
var
  Temp: String;
begin
  Temp := '<text x=' + DBLQ + IntToStr(aX + FSvgFontHeight) + DBLQ + ' ';
  Temp := Temp + 'y=' + DBLQ + IntToStr(aY - FSvgFontHeight - (FSvgFontHeight div 2) - 3) + DBLQ + '>';
  Temp := Temp + HtmlEntities(aText) + '</text>';
  WriteLn(FTreeSvgFile, Temp);
end;

function TCustomSvgTree.SvgOtuName(aX: Integer; aY: Integer; aText: String; aNode: TpNode): String;

  function XCoordAttr: String;
  begin
    if FIsMobileFriendly then
      Result := Format('%s%.1f%%%s', [DBLQ, (aX + FSvgFontHeight)/FSvgTreeImageWidth*100, DBLQ])
    else
      Result := Format('%s%d%s', [DBLQ, aX, DBLQ]);
  end;

begin
  if (aNode.timetreeId >= 0) and (not FRenderNewickOnly) then
    aNode.rank := TaxonomicRanks[aNode.timetreeId];
  Result := '<text x=' + XCoordAttr + ' ';
  Result := Result + 'y=' + DBLQ + IntToStr(aY - FSvgFontHeight - (FSvgFontHeight div 2) - 3) + DBLQ + ' ';
  if aNode.rank >= trSpecies then
    Result := Result + 'font-style=' + dblq + 'italic' + dblq + ' ';
  Result := Trim(Result) + '>';
  Result := Result + HtmlEntities(aText) + '</text>';
end;

procedure TCustomSvgTree.AddOutgroup(index : integer);
begin
    if (index > 0) and (index <= NoOfOTUs) then
        FNode[index].outgroup := true;
end;

procedure TCustomSvgTree.RemoveOutgroup(index : integer);
begin
    if (index > 0) and (index <= NoOfOTUs) then
        FNode[index].outgroup := false;
end;

procedure TCustomSvgTree.SetOutgroup(index : integer; value : boolean);
begin
    if not TreeExist then Exit;
    if (index > 0) and (index <= NoOfOTUs) then
        FNode[index].outgroup := value;
end;

function TCustomSvgTree.GetIsOutgroup:boolean;
var i : integer;
begin
    Result := false;
    for i := 1 to NoOfOTUs do
        if FNode[i].outgroup then begin
            Result := true;
            Break;
        end;
end;

function TCustomSvgTree.GetOutgroup(index : integer):boolean;
begin
    if (index < 1) or (index > NoOfOTUs) then
        Result := false
    else
        Result := FNode[index].outgroup;
end;

function TCustomSvgTree.GetOutgroupAncestorIndex: Integer;
var
  Mrca : TpNode;
  i : integer;
begin
  for i := 1 to NoOfOTUs do
      FNode[i].flag := FNode[i].outgroup;
  Mrca := SearchCommonAncestor(FRoot);
  Result := Mrca.index;
end;

function TCustomSvgTree.GetFirstOutgroupMemberIndex: Integer;
var
  Mrca: TpNode;
  i: Integer;
begin
  Result := FNode[1].index;
  if not isOutgroup then
    Exit;
  for i := 1 to NoOfOTUs do
      if FNode[i].outgroup then
      begin
        Result := FNode[i].index;
        break;
      end;
end;

function TCustomSvgTree.GetCurrentOutgroup(index : integer):boolean;
var p, a : TpNode;
begin
    p := FNode[index];
    a := p.anc;
    while a <> FRoot do begin
        p := a;
        a := p.anc;
    end;
    if p = a.des2 then
        Result := true
    else
        Result := false;
end;


procedure TCustomSvgTree.InitScale;
var
    r : double;
begin
    FScale := 2*FRoot.height;

    if FScale = 0.0 then
      Exit;

    FScale := FScale / 10;
    r := FScale/Power(10,Floor(log10(FScale)));
    if Trunc(r+0.5) >= 8 then
        FScale := Power(10,Floor(log10(FScale))+1)
    else if Trunc(r) >= 3 then
        FScale := 5*Power(10,Floor(log10(FScale)))
    else if (Trunc(r) = 2) or (Trunc(r+0.5) = 2) then
        FScale := 2*Power(10,Floor(log10(FScale)))
    else
        FScale := Power(10,Floor(log10(FScale)));
    FScaleTick := FScale;
    FTimeTick := 0.0;
    FTimeText := '';
    FTimeScale := 0.0;

    if order(FScale) >= 1 then
        ScaleText := FloatToStrF(FScale,ffFixed,15, 0)
    else
        ScaleText := FloatToStrF(FScale,ffFixed, 15, Abs(order(FScale)));
end;

procedure TCustomSvgTree.GetTreeData(tree: TTimeTreeData);
var i: integer;
begin
    tree.NoOfOTUs := NoOfOTUs;
    tree.isBLen := isBranchLength;
    if IsLinearized then
      tree.isSE := false
    else
      tree.isSE := isSE;
    tree.isStats := isStats;
    for i := 0 to NoOfOTUs-2 do begin
      tree.NodeArray[i].des1 := FNode[NoOfOTUs+i+1].des1.index-1;
      tree.NodeArray[i].des2 := FNode[NoOfOTUs+i+1].des2.index-1;
      Tree.DataCoverage[i] := FNode[NoOfOTUs+i+1].dataCoverage;
    end;
    for i := 1 to NoOfOtus do
      tree.IsOutgroupMember[i-1] := FNode[i].outgroup;

    for i := 0 to NoOfNodes-2 do
    begin
      if tree.isBLen then
        if IsLinearized then
        begin
          if FNode[i+1] <> FRoot then
            tree.BLenArray[i] := FNode[FNode[i+1].anc.index].height-FNode[i+1].height;
        end
        else
          tree.BLenArray[i] := FNode[i+1].branch.length;
      if tree.isSE then
        tree.SE[i] := FNode[i+1].branch.SE;
      if tree.isStats then
        tree.Stats[i] := FNode[i+1].branch.stats;
    end;
end;

function TCustomSvgTree.GetNodeAttrib(p: TpNode): TNodeAttrib;
begin
  if p.attrindex < 0 then
    result := GroupAttrib[p.groupindex]
  else
    result := AttribList[p.attrindex];
end;

function TCustomSvgTree.GetTaxonMarker(p: TpNode): TNodeMarker;

  //function CompMarker(m1,m2: TNodeMarker): boolean;
  //begin
  //  result := (m1.Shape = m2.Shape) and (m1.Color = m2.Color);
  //end;

var
  q: TpNode;
begin
  result.Shape := msNone;

  //result.Color := clBlack;
  //if (p.index > NoOfOTUs) and not p.compressed then
  //  exit;
  //if p.marker.Shape = msNone then
  //  exit;
  //
  //if UseGroupAttrib then
  //  if p.attrindex < 0 then
  //    if GroupAttrib[p.groupindex].ShowTaxonMarker and
  //       CompMarker(p.marker, GroupAttrib[p.groupindex].Marker) then
  //    begin
  //      result := p.marker;
  //      exit;
  //    end;
  //
  //q := p;
  //while (q <> FRoot) and (q.attrindex < 0) do
  //  q := q.anc;
  //if q.attrindex >= 0 then
  //  if AttribList[q.attrindex].ShowTaxonMarker then
  //  begin
  //    result := p.marker;
  //    exit;
  //  end;
end;

function TCustomSvgTree.GetTaxonName(Taxon: Integer): String;
begin
  Result := EmptyStr;
  if Taxon <= FNoOfOtus then
    Result := FNode[Taxon].name;
end;

function TCustomSvgTree.GetLineWidth(p: TpNode): integer;
var
  q: TpNode;
  a: TNodeAttrib;
begin
  if p = FRoot then
    result := AttribList[0].LineWidth
  else
  begin
    a := GetNodeAttrib(p);

    if (a.BranchOption = boFullBranch) or
       (a.BranchOption = boHalfBranch) or
       ((a.BranchOption = boBranchOnly) and (p.attrindex <> p.anc.attrindex)) then
      result := a.LineWidth
    else
    begin
      q := p.anc;
      while (q <> FRoot) do
      begin
        a := GetNodeAttrib(q);
        if not ((a.BranchOption = boBranchOnly) or q.hidden) then
          break;
        q := q.anc;
      end;
      result := a.LineWidth;
    end;
  end;
end;



procedure TCustomSvgTree.SetPosition;
var yy, n : integer;
    q1,q2 : TpNode;

    procedure SetX(p : TpNode);
    begin
        with p^ do
        begin
            if p = FRoot then
                position.x := 0
            else if FTopoflag then
                if OTU or compressed then
                    position.x := Round(FMaxDepth*Fxunit)
                else
                    position.x := Round((FMaxDepth -depth)*Fxunit)
            else if IsLinearized or ForceLinearized then
                    position.x := Round((Fxmax -height)*Fxunit)
            else
                position.x := anc.position.x + Round(branch.length*Fxunit);

            if (anc <> nil) and (position.x < anc.position.x) then
                position.x := anc.position.x;
            if OTU or compressed then
            begin
              if (not OTU) and (not FTopoflag) then
                if IsLinearized or ForceLinearized then begin
                  des1.position.x := Round(Fxmax*Fxunit);
                  des2.position.x := des1.position.x;
                end
                else begin
                  des1.position.x := position.x + Round(height*Fxunit);
                  des2.position.x := des1.position.x;
                end;
            end
            else
            begin
              if {Multflag and} (p <> FRoot) then
                if position.x = anc.position.x then
                  hidden := true;
              SetX(des1);
              SetX(des2);
            end;
        end;
    end; { SetX }

    procedure SetCurSizeStraight(p : TpNode);
    begin
        if p.OTU or (FTopoflag and p.compressed) then
            p.cursize := 1
        else if p.compressed then
          p.cursize := p.width/Fyunit
        else
        begin
            SetCurSizeStraight(p.des1);
            SetCurSizeStraight(p.des2);
            p.cursize := p.des1.cursize +p.des2.cursize;
        end;
    end;

    procedure SetXStraight(p : TpNode);
    begin
        if p = FRoot then
            p.position.x := 0
        else if p.OTU or p.compressed then
        begin
            p.position.x := Round(FMaxDepth*Fxunit);
            if not p.OTU then
            begin
                p.des1.position.x := p.position.x;
                p.des2.position.x := p.position.x;
            end;
        end
        else begin

                q1 := p;
                while (q1 <> FRoot) and (q1.anc.depth = p.depth) do q1 := q1.anc;
                p.position.x := Round((1-(q1.cursize-1)/(n-1))*FMaxDepth*Fxunit)
        end;
        if not (p.OTU or p.compressed) then
        begin
          if {Multflag and} (p <> FRoot) then
            if p.position.x = p.anc.position.x then
              p.hidden := true;
          SetXStraight(p.des1);
          SetXStraight(p.des2);
        end;
    end;

    procedure SetXCurve(p : TpNode);
    var x0,x1,y0,y1: integer;
        a: double;
    begin
      if p.OTU or p.compressed then Exit;
      if p <> FRoot then
        with p.position do
        begin
          q1 := p;
          if p = p.anc.des1 then
            if p.position.y < p.anc.position.y then
              while (q1 <> FRoot)
              and (   (q1.position.y < q1.anc.position.y)
                   or ((q1.position.y = q1.anc.position.y) and (q1 = q1.anc.des1))) do
                q1 := q1.anc
            else
              q1 := q1.anc
          else
            if p.position.y > p.anc.position.y then
              while (q1 <> FRoot)
              and (   (q1.position.y > q1.anc.position.y)
                   or ((q1.position.y = q1.anc.position.y) and (q1 = q1.anc.des2))) do
                q1 := q1.anc
            else
              q1 := q1.anc;

          x0 := q1.position.x;
          y0 := q1.position.y;

          q2 := p;
          if y < y0 then
            while not (q2.OTU or q2.compressed) do q2 := q2.des1
          else
            while not (q2.OTU or q2.compressed) do q2 := q2.des2;
          x1 := q2.position.x;
          y1 := q2.position.y;

          if p.depth = p.anc.depth then
            x := p.anc.position.x
          else begin
            a := abs(y1-y0)*sqrt(3)/(x1-x0);
            if abs(y-y0) > abs(y1-y0) then begin
            end
            else
              if 2*abs(y-y0) < abs(y-y1) then
                x := round(x0 +1/a*sqrt(4/9*(y1-y0)*(y1-y0) -(1/3*y0+ 2/3*y1 -y)*(1/3*y0+ 2/3*y1 -y)))
              else
                x := round(x1 -1/a*sqrt(16/9*(y1-y0)*(y1-y0) -(4/3*y0 -1/3*y1 -y)*(4/3*y0 -1/3*y1 -y)));
          end;
        end;
      if {Multflag and} (p <> FRoot) then
        if p.position.x = p.anc.position.x then
          p.hidden := true;
      SetXCurve(p.des1);
      SetXCurve(p.des2);
    end;

    procedure SetY(p : TpNode);
    begin
      if p.OTU then begin
        p.position.y := yy +(Fyunit div 2);
        yy := yy +Fyunit;
      end
      else if p.compressed then
        if FTopoflag then begin
          p.position.y := yy +(Fyunit div 2);
          yy := yy +Fyunit;
          p.des1.position.y := p.position.y -(Fyunit div 2);
          p.des2.position.y := p.position.y +(Fyunit div 2);
        end
        else begin
          p.position.y := yy +((p.width+Fgunit) div 2);
          yy := yy +p.width+Fgunit;
          p.des1.position.y := p.position.y -(Fgunit*p.size div 2);
          p.des2.position.y := p.position.y +(Fgunit*p.size div 2);
        end
      else
      begin
          SetY(p.des1);
          SetY(p.des2);
              q1 := p;
              q2 := p;
              repeat
                  q1 := q1.des1;
              until (q1.position.x > p.position.x) or (q1.OTU or q1.compressed);
              repeat
                  q2 := q2.des2;
              until (q2.position.x > p.position.x) or (q2.OTU or q2.compressed);
          p.position.y := (q1.position.y + q2.position.y) div 2;
      end;
    end; { SetY }

    procedure SetYStraight(p: TpNode);

        procedure SetYStraightFromBottom(p: TpNode);
        begin
            with p^ do begin
                if p.OTU then begin
                  p.position.y := yy +(Fyunit div 2);
                  yy := yy +Fyunit;
                end
                else if p.compressed then
                  if FTopoflag then begin
                    p.position.y := yy +(Fyunit div 2);
                    yy := yy +Fyunit;
                    p.des1.position.y := p.position.y -(Fyunit div 2);
                    p.des2.position.y := p.position.y +(Fyunit div 2);
                  end
                  else begin
                    p.position.y := yy +(p.width div 2);
                    yy := yy +p.width;
                    p.des1.position.y := p.position.y -(Fgunit*p.size div 2);
                    p.des2.position.y := p.position.y +(Fgunit*p.size div 2);
                  end
                else begin
                    SetYStraightFromBottom(des1);
                    SetYStraightFromBottom(des2);
                    if FTopoflag then begin
                        q1 := p;
                        while not (q1.OTU or q1.compressed) do q1 := q1.des1;
                        q2 := p;
                        while not (q2.OTU or q2.compressed) do q2 := q2.des2;
                    end
                    else begin
                        q1 := p;
                        while (q1.position.x = p.position.x) and (not(q1.OTU or q1.compressed)) do
                          q1 := q1.des1;
                        while not (q1.OTU or q1.compressed) do
                          q1 := q1.des2;
                        q2 := p;
                        while (q2.position.x = p.position.x) and (not(q2.OTU or q2.compressed)) do
                          q2 := q2.des2;
                        while not (q2.OTU or q2.compressed) do q2 := q2.des1;
                    end;
                    position.y := (q1.position.y + q2.position.y) div 2;
                end;
            end;
        end;

        procedure SetYStraightFromTop(p: TpNode);
        begin
          with p^ do
            if not OTU and compressed then
              position.y := anc.position.y -(anc.position.y-position.y)*(position.x-anc.position.x) div (des1.position.x-anc.position.x)
            else if not OTU then begin
              SetYStraightFromTop(des1);
              SetYStraightFromTop(des2);
            end;
        end;

    begin
      SetYStraightFromBottom(p);
      if (BranchStyle = bsStraight) and (not FTopoflag) then
        SetYStraightFromTop(p);
    end;

    procedure SetYForMultifurcation(p : TpNode);
    begin
        if p <> FRoot then
            if (TreeStyle = tsCircle) then begin
                if p.avglen = p.anc.avglen then begin
                  if FTopoflag or (not p.compressed) then begin
                    p.angle := p.anc.angle;
                    p.position.x := p.anc.position.x;
                    p.position.y := p.anc.position.y;
                  end;
                end
            end
            else if p.position.x = p.anc.position.x then
                p.position.y := p.anc.position.y;
        if not p.des1.OTU then SetYForMultifurcation(p.des1);
        if not p.des2.OTU then SetYForMultifurcation(p.des2);
    end;

    procedure SetCurSizeAngle(p : TpNode);
    begin
        if p.OTU or (FTopoflag and p.compressed) then
            p.cursize := 1
        else if p.compressed then
        begin
          if IsLinearized or ForceLinearized then
            p.cursize := p.size*Fgunit/Fyunit +1
          else if p.height < 0.00000000001 then  // added on April 25, 2007
            p.cursize := 1
          else
            p.cursize := p.width/Fyunit*FRoot.height/p.height;
//            p.cursize := ArcTan(p.avglen*Fxunit/(p.width-Fyunit))/ArcTan(p.avglen*Fxunit/(p.width))+1
          if p.cursize < 1 then
            p.cursize := 1
        end
        else
        begin
            SetCurSizeAngle(p.des1);
            SetCurSizeAngle(p.des2);
            p.cursize := p.des1.cursize +p.des2.cursize;
        end;
    end;

    procedure SetAnglePosition;

      procedure SetAvgLen(p : TpNode);
      begin
        if p.OTU then
        begin
          if p.branch.length > 0.0 then
            p.avglen := p.branch.length +p.namesize.x/Fxunit
          else
            p.avglen := p.namesize.x/Fxunit;
        end
        else if p.compressed then
        begin
          if p.branch.length > 0.0 then
            p.avglen := p.height +p.branch.length
          else
            p.avglen := p.height;
          if p.width < Fyunit then
            p.avglen := p.avglen +p.namesize.x/Fxunit;
        end
        else
        begin
          SetAvgLen(p.des1);
          SetAvgLen(p.des2);
          p.avglen := (p.des1.avglen + p.des2.avglen)/2;
          if p.branch.length > 0.0 then
            p.avglen := p.avglen +p.branch.length;
        end;
      end;

      procedure SetNodeAngle(p : TpNode);
      var h, s0, s1, b1, b2, n1, n2: double;
      begin
        if p.branch.length <= 0.0 then
		begin
          s0 := p.sector;
          s1 := s0;
        end
        else
		begin
          b1 := p.branch.length;
          h := p.avglen;
          s0 := p.sector;
          s1 := arcsin(h*sin(s0)/sqrt(h*h +b1*b1 -2*b1*h*cos(s0)));
          if s1 < s0 then
            s1 := PI -s1;
        end;
        n1 := p.des1.cursize;// p.des1.position.y;
        n2 := p.des2.cursize;// p.des2.position.y;
        if (n1+n2) < 0.00000000001 then
          if (p.des1.avglen <= 0.0) or (p.des2.avglen <= 0.0) then
          begin
            p.des1.sector := s1/2;
            p.des2.sector := s1/2;
          end
          else
          begin
            b1 := p.des1.avglen;
            b2 := p.des2.avglen;
            p.des1.sector := s1/b1/(1/b1 + 1/b2);
            p.des2.sector := s1/b2/(1/b1 + 1/b2);
          end
        else if (p.des1.avglen <= 0.0) or (p.des2.avglen <= 0.0) then
        begin
          p.des1.sector := s1*n1/(n1 + n2);
          p.des2.sector := s1*n2/(n1 + n2);
        end
        else
        begin
          b1 := p.des1.avglen;
          b2 := p.des2.avglen;
          p.des1.sector := s1*n1/b1/(n1/b1 + n2/b2);
          p.des2.sector := s1*n2/b2/(n1/b1 + n2/b2);
        end;
        p.des1.angle := p.angle +s1 -p.des1.sector;
        p.des2.angle := p.angle -s1 +p.des2.sector;
        if p.des1.branch.length > 0.0 then
          b1 := p.des1.branch.length*Fxunit
        else
          b1 := 0.0;
        if p.des2.branch.length > 0.0 then
          b2 := p.des2.branch.length*Fxunit
        else
          b2 := 0.0;
        p.des1.position.x := p.position.x +round(cos(p.des1.angle)*b1);
        p.des1.position.y := p.position.y -round(sin(p.des1.angle)*b1);
        p.des2.position.x := p.position.x +round(cos(p.des2.angle)*b2);
        p.des2.position.y := p.position.y -round(sin(p.des2.angle)*b2);
        if not(p.des1.OTU or p.des1.compressed) then
          SetNodeAngle(p.des1);
        if not(p.des2.OTU or p.des2.compressed) then
          SetNodeAngle(p.des2);
      end;

    var i : integer;
        n1, n2: double;
    begin
      SetAvgLen(FRoot);
      SetCurSizeAngle(FRoot);

      FRoot.position.x := 0;
      FRoot.position.y := 0;

      n1 := FRoot.des1.cursize; //FRoot.des1.position.y;
      n2 := FRoot.des2.cursize; //FRoot.des2.position.y;
      if FTopoflag or (FRoot.des1.avglen <= 0.0) or (FRoot.des2.avglen <= 0.0) then begin
        FRoot.des1.sector := PI*n1/(n1 + n2);
        FRoot.des2.sector := PI*n2/(n1 + n2);
      end
      else begin
        FRoot.des1.sector := PI*n1/FRoot.des1.avglen/(n1/FRoot.des1.avglen + n2/FRoot.des2.avglen);
        FRoot.des2.sector := PI*n2/FRoot.des2.avglen/(n1/FRoot.des1.avglen + n2/FRoot.des2.avglen);
      end;
      if (isRooted and ShowRoot) then begin
        FRoot.des1.sector := FRoot.des1.sector/3;
        FRoot.des2.sector := FRoot.des2.sector/3;
        FRoot.des1.angle := FRoot.des1.sector;
        FRoot.des2.angle := -FRoot.des2.sector;
      end
      else begin
        FRoot.des1.angle := (1/2 -StartAngle/6)*PI;
        FRoot.des2.angle := (3/2 -StartAngle/6)*PI;
      end;
      if FRoot.des1.compressed then begin
        FRoot.des1.position.x :=  round(cos(FRoot.des1.angle)*(FRoot.des1.branch.length+FRoot.des1.height)*Fxunit);
        FRoot.des1.position.y := -round(sin(FRoot.des1.angle)*(FRoot.des1.branch.length+FRoot.des1.height)*Fxunit);
      end
      else begin
        FRoot.des1.position.x :=  round(cos(FRoot.des1.angle)*FRoot.des1.branch.length*Fxunit);
        FRoot.des1.position.y := -round(sin(FRoot.des1.angle)*FRoot.des1.branch.length*Fxunit);
      end;
      if FRoot.des2.compressed then begin
        FRoot.des2.position.x :=  round(cos(FRoot.des2.angle)*(FRoot.des2.branch.length+FRoot.des2.height)*Fxunit);
        FRoot.des2.position.y := -round(sin(FRoot.des2.angle)*(FRoot.des2.branch.length+FRoot.des2.height)*Fxunit);
      end
      else begin
        FRoot.des2.position.x :=  round(cos(FRoot.des2.angle)*FRoot.des2.branch.length*Fxunit);
        FRoot.des2.position.y := -round(sin(FRoot.des2.angle)*FRoot.des2.branch.length*Fxunit);
      end;
      SetFontTarget(tftOtu);
      if not FRoot.des1.OTU then
        SetNodeAngle(FRoot.des1);
      if not FRoot.des2.OTU then
        SetNodeAngle(FRoot.des2);
      for i := NoOfOTUs+1 To NoOfNodes do
      begin
//        if FNode[i].hidden then continue;
        if FNode[i].compressed then
        begin
          FNode[i].des1.position.x := FNode[i].position.x +round(cos(FNode[i].angle)*FNode[i].height*Fxunit)
                                                        +round(cos(FNode[i].angle+PI/2)*FNode[i].size*Fgunit/2);
          FNode[i].des1.position.y := FNode[i].position.y -round(sin(FNode[i].angle)*FNode[i].height*Fxunit)
                                                        -round(sin(FNode[i].angle+PI/2)*FNode[i].size*Fgunit/2);
          FNode[i].des2.position.x := FNode[i].position.x +round(cos(FNode[i].angle)*FNode[i].height*Fxunit)
                                                        -round(cos(FNode[i].angle+PI/2)*FNode[i].size*Fgunit/2);
          FNode[i].des2.position.y := FNode[i].position.y -round(sin(FNode[i].angle)*FNode[i].height*Fxunit)
                                                        +round(sin(FNode[i].angle+PI/2)*FNode[i].size*Fgunit/2);
        end;
      end;
    end;

    procedure SetCirclePosition;
    var q1, q2: TpNode;
        OriAngle, CurrAngle, dAngle: double;

      procedure SetAvgLen(p : TpNode);
      begin
        with p^ do begin
          if p = FRoot then
            avglen := Radius*4*CenterMargin/100
          else if FTopoflag then
          begin
            if OTU or compressed then
            begin
              avglen := Radius*4;
              if not OTU then begin
                des1.avglen := Radius*4;
                des2.avglen := Radius*4;
              end
            end
            else
              avglen := Radius*4 -depth*Fxunit;
            if p.depth = p.anc.depth then
              p.hidden := true;
          end
          else if IsLinearized or ForceLinearized then
          begin
            if OTU then
              avglen := Radius *4
            else
              avglen := Radius*4 -height*Fxunit;
            if avglen <= anc.avglen then
            begin
              avglen := anc.avglen;
              if not compressed and not OTU then
                hidden := true;
            end;
            if not OTU and compressed then
            begin
              des1.avglen := Radius*4;
              des2.avglen := Radius*4;
            end;
          end
          else
          begin
            //if branch.length > 0 then
            if branch.length > 0.0000000000001 then
              avglen := anc.avglen +branch.length*Fxunit
            else
            begin
              avglen := anc.avglen;
              if not compressed and not OTU then
                hidden := true;
            end;
            if not OTU and compressed then
            begin
              des1.avglen := avglen +height*Fxunit;
              des2.avglen := avglen +height*Fxunit;
            end;
          end;
          if not (OTU or compressed) then
          begin
            SetAvgLen(des1);
            SetAvgLen(des2);
          end;
        end;
      end;

      procedure SetAngle(p: TpNode);
      begin
        if p.OTU or p.compressed then
          if Abs(OriAngle - CurrAngle) < 0.00000000001 then
          begin
            p.angle := CurrAngle;
            CurrAngle := CurrAngle -dAngle*p.cursize/2;
          end
          else
          begin
            p.angle := CurrAngle -dAngle*p.cursize/2;
            CurrAngle := CurrAngle -dAngle*p.cursize;
          end
        else
        begin
          SetAngle(p.des1);
          SetAngle(p.des2);
//          if Multflag then
//          begin
            q1 := p;
            q2 := p;
            repeat
              q1 := q1.des1;
            until (q1.avglen > p.avglen) or q1.OTU or q1.compressed;
            repeat
              q2 := q2.des2;
            until (q2.avglen > p.avglen) or q2.OTU or q2.compressed;
            p.angle := (q1.angle + q2.angle)/2;
//          end
//          else
//            p.angle := (p.des1.angle + p.des2.angle)/2;
        end;
      end;

    var i: integer;
    begin
      if FTopoflag then
          Fxunit := Fxunit*Radius/FMinTreeWidth*(1.0 -CenterMargin/100)
      else
          Fxunit := Fxunit*Radius/FTreeWidth*(1.0 -CenterMargin/100);

      SetAvgLen(FRoot);
      SetCurSizeAngle(FRoot);
      if CurrentNoOfOTUs > 18 then
          dAngle := 17/9*PI/(FRoot.cursize-1)
      else
          dAngle := 2*PI/FRoot.cursize;
      OriAngle := (1/2 -StartAngle/6)*PI;
      CurrAngle := OriAngle;
      SetAngle(FRoot);

      for i := 1 to NoOfNodes do
        with FNode[i]^ do
        begin
          if hidden then Continue;
          position.x :=  round(cos(angle)*avglen);
          position.y := -round(sin(angle)*avglen);
          if not OTU and compressed then
          begin
            des1.position.x :=  round(cos(angle)*des1.avglen) +round(cos(angle+PI/2)*Fgunit*size/2);
            des1.position.y := -round(sin(angle)*des1.avglen) -round(sin(angle+PI/2)*Fgunit*size/2);
            des2.position.x :=  round(cos(angle)*des2.avglen) -round(cos(angle+PI/2)*Fgunit*size/2);
            des2.position.y := -round(sin(angle)*des2.avglen) +round(sin(angle+PI/2)*Fgunit*size/2);
          end;
        end;
    end;

    //procedure ResetHidden(p: TpNode);
    //begin
    //  p.hidden := false;
    //  if not (p.compressed or p.OTU) then
    //  begin
    //    ResetHidden(p.des1);
    //    ResetHidden(p.des2);
    //  end;
    //end;

    procedure SetCaptionPosition;

      procedure SetCaptionX(p: TpNode);
      var
        a: TNodeAttrib;
      begin
        if not (p.OTU or p.compressed) then
        begin
          SetCaptionX(p.des1);
          SetCaptionX(p.des2);
        end;
        if p <> FRoot then
        begin
          a  := GetNodeAttrib(p);

          if p.compressed then
            if p.OTU or FTopoflag then
              p.Bracket.Left := p.position.X
            else
              p.Bracket.Left := p.des1.position.X
          else if p.OTU then
          begin
            p.Bracket.Left := p.position.X +GetLineWidth(p)*2 +CurrentFontHeight*4;
            if a.ShowTaxonName then
              p.Bracket.Left := p.Bracket.Left +p.namesize.x
            else if a.ShowSpeciesName then
              p.Bracket.Left := p.Bracket.left + P.speciesNameSIze.x;
            if GetTaxonMarker(p).Shape <> msNone then
              p.Bracket.Left := p.Bracket.Left +CurrentFontHeight*5;
            if ShowCharState then
            begin
              SetFontTarget(tftCharState);
              p.Bracket.Left := p.Bracket.Left +CustomTextWidth(p.charstate)*4 +Abs(CharStateFontHeight)*2;
            end;
          end;

          if p.attrindex = p.anc.attrindex then
          begin
            if p.anc.capdepth < p.capdepth then
              p.anc.capdepth := p.capdepth;
            if p.anc.Bracket.Left < p.bracket.Left then
              p.anc.bracket.Left := p.bracket.Left;
          end
          else
          begin
            if p.compressed then
            begin
              if p.anc.capdepth = 0 then
                p.anc.capdepth := 1;
            end
            else if a.CaptionSize > 0 then
            begin
              if p.capdepth = 0 then
                p.capdepth := 1;
              if p.anc.capdepth <= p.capdepth then
                p.anc.capdepth := p.capdepth +1;
            end
            else
            begin
              if p.anc.capdepth < p.capdepth then
                p.anc.capdepth := p.capdepth;
            end;
            if p.anc.Bracket.Left < p.bracket.Left +a.CaptionSize then
              p.anc.Bracket.Left := p.bracket.Left +a.CaptionSize;
          end;
        end;
      end;

      procedure SetCaptionY(p: TpNode);
      var
        d: integer;
        q: TpNode;
      begin
        if not (p.OTU or p.compressed) then
        begin
          SetCaptionY(p.des1);
          SetCaptionY(p.des2);
        end;

        if p = FRoot then exit;
        if p.attrindex = p.anc.attrindex then exit;

        q := p;
        while not (q.OTU or q.compressed) do
          q := q.des1;
        if q.compressed and not q.OTU and not FTopoflag then
          q := q.des1;
        if q.compressed then
          d := CurrentFontHeight*2
        else
          d := CurrentFontHeight*2;
        if d > (Fyunit div 2) then
          d := Fyunit div 2;
        p.Bracket.Top := q.position.y -d;

        q := p;
        while not (q.OTU or q.compressed) do
          q := q.des2;
        if q.compressed and not q.OTU and not FTopoflag then
          q := q.des2;

        if q.compressed then
          d := CurrentFontHeight*2
        else
          d := CurrentFontHeight*2;
        if d > (Fyunit div 2) then
          d := Fyunit div 2;
        p.Bracket.Bottom := q.position.y +d;
      end;

      procedure AlignCaptions;
      var
        max: integer;

        procedure SetXAlign(p: TpNode; d: integer);
        var
          a: TNodeAttrib;
        begin
          if not (p.OTU or p.compressed) then
          begin
            SetXAlign(p.des1, d);
            SetXAlign(p.des2, d);
          end;
          if p = FRoot then exit;
          if p.capdepth <> d then exit;
          if p.attrindex = p.anc.attrindex then
          begin
            if p.anc.Bracket.Left < p.bracket.Left then
              p.anc.Bracket.Left := p.bracket.Left;
          end
          else
          begin
            a := GetNodeAttrib(p);
            if p.anc.bracket.Left < p.bracket.Left+a.CaptionSize then
              p.anc.bracket.Left := p.bracket.Left+a.CaptionSize;

          end;
        end;

        procedure SetGroupDepth(p: TpNode; gd: integer);
        var
          a: TNodeAttrib;
        begin
          if not (p.OTU or p.compressed) then
          begin
            SetGroupDepth(p.des1, gd);
            SetGroupDepth(p.des2, gd);
          end;
          if p = FRoot then exit;
          if p.attrindex < 0 then
            p.capdepth := gd;
          if p.attrindex = p.anc.attrindex then
          begin
            if p.anc.capdepth < p.capdepth then
              p.anc.capdepth := p.capdepth;
          end
          else
          begin
            a := GetNodeAttrib(p);
            if a.ShowLabel or a.ShowBracket then
              if p.anc.capdepth <= p.capdepth then
                p.anc.capdepth := p.capdepth +1;
          end;
        end;

      var
        d,i,j: integer;
        a: TNodeAttrib;
      begin
        if UseGroupAttrib and (GroupAttrib.Count > 0) then
        begin
          d := 0;
          for i := 1 to NoOfNodes do
            if (FNode[i].attrindex < 0) and (FNode[i].capdepth > d) then
              d := FNode[i].capdepth;
          SetGroupDepth(FRoot, d);
        end;

        d := 0;
        for i := 1 to NoOfNodes do
          if FNode[i].capdepth > d then
            d := FNode[i].capdepth;

        for i := 1 to d do
        begin
          max := 0;
          for j := 1 to NoOfNodes do
          begin
            if FNode[j] = FRoot then continue;
            if FNode[j].capdepth <> i then continue;
            if FNode[j].attrindex = 0 then continue;
            if (FNode[j].attrindex = FNode[j].anc.attrindex) and
               (FNode[j].groupindex = FNode[j].anc.groupindex) then
              continue;

            a := GetNodeAttrib(FNode[j]);
            if a.ShowLabel or a.ShowBracket then
              if FNode[j].Bracket.Left +CurrentFontHeight > max then
                max := FNode[j].Bracket.Left +CurrentFontHeight;
          end;
          for j := 1 to NoOfNodes do
          begin
            if FNode[j] = FRoot then continue;
            if FNode[j].capdepth <> i then continue;
            if FNode[j].attrindex = 0 then continue;
            if (FNode[j].attrindex = FNode[j].anc.attrindex) and
               (FNode[j].groupindex = FNode[j].anc.groupindex) then
              continue;

            a := GetNodeAttrib(FNode[j]);
            if a.ShowLabel or a.ShowBracket then
              FNode[j].Bracket.Left := max -abs(a.CaptionFontHeight);
          end;
          SetXAlign(FRoot, i);
        end;
      end;

    var i: integer;
    begin
      if (AttribList.Count <= 1) and (GroupAttrib.Count = 0) then Exit;

      for i := 1 to NoOfNodes do
      begin
        if (FNode[i].attrindex = 0) and (FNode[i].groupindex = -1) then
          FNode[i].capdepth := 0
        else if FNode[i].compressed then
          FNode[i].capdepth := 0
        else
          FNode[i].capdepth := 1;
        FNode[i].Bracket.Top := 0;
        FNode[i].Bracket.Bottom := 0;
        FNode[i].Bracket.Left := 0;
      end;
      SetFontTarget(tftCharState);
      SetCaptionX(FRoot);
      SetCaptionY(FRoot);

      if AlignCaption then
        AlignCaptions;
    end;

begin
    if not TreeExist then Exit;
    //ResetHidden(FRoot);
    if IsBranchLength or ((IsLinearized or ForceLinearized) and (FRoot.Height > 0.000000000001)) then
    begin
      if (IsLinearized or ForceLinearized) and (FRoot.Height > 0.000000000001) then
        Fxmax := FRoot.Height
      else
        Fxmax := Max(FRoot.des1.branch.maxlen2, FRoot.des2.branch.maxlen2);
      if Fxmax < 0.000000000001 then begin
        FTopoflag := true;
      end;
    end
    else
    begin
      FTopoflag := true;
    end;

    if FTopoflag then
    begin
        if IsCondensed then
            SetDepth(FRoot, FCondenseValue)
        else
            SetDepth(FRoot, 0);

        FMaxDepth := Max(FRoot.depth, 1); // bug fix for div by zero error
        Fxunit := FMinTreeWidth*4 div FMaxDepth;
        if Fxunit = 0.0 then
            Fxunit := 1.0;
    end
    else if (abs(PixelsPerUnit) < 0.00000000000001) then
    begin
        Fxunit := FTreeWidth*4/Fxmax;
        FPixelsPerUnit := Fxunit/4;
    end
    else
    begin
        Fxunit := PixelsPerUnit*4;
        FTreeWidth := Ceil(Fxmax*PixelsPerUnit);
    end;

    yy := 0;

    if TreeStyle = tsCircle then
        SetCirclePosition
    else if TreeStyle = tsRadiation then
        SetAnglePosition
    else
    begin
      if (BranchStyle = bsStraight) and FTopoflag then begin
          SetCurSizeStraight(FRoot);
          n := Round(FRoot.cursize);
          SetXStraight(FRoot);
      end
      else
          SetX(FRoot);
      case BranchStyle of
         bsRectangular : SetY(FRoot);
         bsCurved :     if FTopoflag then
                          SetY(FRoot)
                        else
                          SetYStraight(FRoot);
         bsStraight :  SetYStraight(FRoot);
      else
         SetY(FRoot);
      end;
    end;

    if (TreeStyle = tsCircle) or ((TreeStyle = tsTraditional) and (BranchStyle <> bsRectangular)) then
      SetYForMultifurcation(FRoot);
    if (TreeStyle = tsTraditional) and (BranchStyle = bsCurved) and FTopoflag then
       SetXCurve(FRoot);
    if TreeStyle = tsTraditional then
     SetCaptionPosition;
end; { SetPosition }

procedure TCustomSvgTree.SetTreeSize;
var
  max, min : TPoint;
  a: TNodeAttrib;
  w,h,x,y,i : integer;
  Stat: Double;
  StatStr: String;
begin
    if not TreeExist then Exit;
    if (TreeStyle = tsCircle) or (TreeStyle = tsRadiation) then
    begin
        Fxbase := Abs(FSvgFontHeight)*4;
        Fybase := Abs(FSvgFontHeight)*4;
        min.x := 0;
        min.y := 0;
        max.x := 0;
        max.y := 0;

        for i := 1 to NoOfNodes do
        begin
          if not (FNode[i].OTU or FNode[i].compressed) then Continue;
          if FNode[i].hidden then Continue;

          a := GetNodeAttrib(FNode[i]);
          w := abs(a.FontHeight) +GetLineWidth(FNode[i])*2;

          if a.ShowTaxonName  then
            w := w +FNode[i].namesize.x
          else if a.ShowSpeciesName then
            w := w + FNode[i].speciesNameSize.x; { TODO 1 -oglen -cgenedups : handle namesize for species names }

          if GetTaxonMarker(FNode[i]).Shape <> msNone then
            if FNode[i].Compressed then
              w := w +Abs(a.CaptionFontHeight)*5
            else
              w := w +Abs(a.FontHeight)*5;

          h := Abs(a.FontHeight)*6;
          if HorzTaxonName then
            if (i > NoOfOTUs) and FNode[i].compressed then
            begin
              if cos(FNode[i].angle) < 0.0 then
                x := (FNode[i].des1.position.x +FNode[i].des2.position.x) div 2 -w
              else
                x := (FNode[i].des1.position.x +FNode[i].des2.position.x) div 2 +w;
              if sin(FNode[i].angle) < 0.0 then
                y := (FNode[i].des1.position.y +FNode[i].des2.position.y) div 2 +h
              else
                y := (FNode[i].des1.position.y +FNode[i].des2.position.y) div 2 -h;
            end
            else
            begin
              if cos(FNode[i].angle) < 0.0 then
                x := FNode[i].position.x -w
              else
                x := FNode[i].position.x +w;
              if sin(FNode[i].angle) < 0.0 then
                y := FNode[i].position.y +h
              else
                y := FNode[i].position.y -h;
            end
          else if (i > NoOfOTUs) and FNode[i].compressed then
          begin
            x := (FNode[i].des1.position.x +FNode[i].des2.position.x) div 2 +round(cos(FNode[i].angle)*w);
            y := (FNode[i].des1.position.y +FNode[i].des2.position.y) div 2 -round(sin(FNode[i].angle)*w);
          end
          else
          begin
            x := FNode[i].position.x +round(cos(FNode[i].angle)*w);
            y := FNode[i].position.y -round(sin(FNode[i].angle)*w);
          end;
          if x < min.x then
            min.x := x
          else if x > max.x then
            max.x := x;
          if y < min.y then
            min.y := y
          else if y > max.y then
            max.y := y;
        end;
        for i := 1 to NoOfNodes do begin
            FNode[i].position.x := FNode[i].position.x -min.X;
            FNode[i].position.y := FNode[i].position.y -min.Y;
        end;

        FOrigin.X := -min.X;
        FOrigin.Y := -min.Y;
        max.x := max.x -min.x +2*Fxbase;
        max.y := max.y -min.y +2*Fybase;
    end
    else
    begin
      repeat { there is no good way to calculate progress in this case}
        max.x := 0;
        max.y := 0;

        for i := 1 to NoOfOTUs do
        begin
            if FNode[i].hidden then Continue;
            if FNode[i].position.x > max.x then
              max.x := FNode[i].position.x;
            if FNode[i].position.y > max.y then
              max.y := FNode[i].position.y;
        end;
        SetFontTarget(tftCharState);
        for i := 1 to NoOfNodes do
        begin
            if FNode[i].hidden then Continue;
            if FNode[i].compressed and (not FNode[i].OTU) and (not FTopoflag) then
              w := FNode[i].des1.position.x
            else
              w := FNode[i].position.x;
            w := w +GetLineWidth(FNode[i])*2;

            if FNode[i].compressed or FNode[i].OTU then
            begin
              a := GetNodeAttrib(FNode[i]);
              if a.ShowTaxonName then
                w := w +FNode[i].namesize.x
              else if a.ShowSpeciesName then
                w := w + FNode[i].speciesNameSize.x;
              if GetTaxonMarker(FNode[i]).Shape <> msNone then
                if FNode[i].compressed then
                  w := w +Abs(a.CaptionFontHeight)*5
                else
                  w := w +Abs(a.FontHeight)*5;
              if FNode[i].OTU and ShowCharState then
                w := w +CustomTextWidth(FNode[i].charstate)*4 +Abs(CharStateFontHeight)*2;
            end;

            if w > max.x then
              max.x := w;

            h := FNode[i].position.y;
            if not FTopoflag and FNode[i].compressed and (not FNode[i].OTU) then
              if h < FNode[i].des2.position.Y then
                h := FNode[i].des2.position.Y;
            if h > max.y then
              max.y := h;
        end;
        for i := 1 to NoOfNodes do
        begin
            if FNode[i] <> FRoot then
            begin
              a := GetNodeAttrib(FNode[i]);
              if (not FNode[i].compressed) and
                 ((FNode[i].attrindex <> FNode[i].anc.attrindex) or (FNode[i].groupindex <> FNode[i].anc.groupindex))then
              begin
                w := FNode[i].bracket.Left + a.CaptionSize;
                if w > max.x then
                  max.x := w;
              end;
            end;
        end;
        if IsLinearized  then
        begin
            SetFontTarget(tftScale);
            i := -1; //@SK mega2b4 to shut up compiler
            if IsScale and ShowScale then
            begin
                i := Pos('.', FScaleText);
                if i > 0 then
                    i := Length(FScaleText)-i;
                w := Round(Fxmax*Fxunit)
                    +CustomTextWidth(FloatToStrF(0.0, ffFixed, 15, i))*2
                    +CustomTextWidth(FScaleUnit+'XXX')*4;
               if w > max.x then
                 max.x := w;
            end;
            if IsTimeScale and ShowTimeScale then
            begin
                if not ShowScale then
                begin
                    i := Pos('.', FTimeText);
                    if i > 0 then
                        i := Length(FTimeText)-i;
                end;
                w := Round(Fxmax*Fxunit)
                    +CustomTextWidth(FloatToStrF(0.0, ffFixed, 15, i))*2
                    +CustomTextWidth(FTimeUnit+'XXX')*4;
                if w > max.x then max.x := w;
            end;
        end;

        if (isRooted and ShowRoot) then
            Fxbase := FX_BASE_DEFAULT + 100
        else
            Fxbase := FX_BASE_DEFAULT;

        if ShowNodeIds then
        begin
          w := 0;
          SetFontTarget(tftStats);
          for i := NoOfOTUs+1 to NoOfNodes do
          begin
            if FNode[i].hidden then
              Continue;
            if FNode[i] = FRoot.des1 then
            begin
              if FRoot.des2.OTU or (FRoot.des2.position.x > FNode[i].position.x) then
                Continue;
            end
            else if FNode[i] = FRoot.des2 then
            begin
              if FRoot.des1.OTU or (FRoot.des1.position.x > FNode[i].position.x) then
                Continue;
            end;
            if w > FNode[i].position.x -(CustomTextWidth(IntToStr(FNode[i].branch.stat2))*4 +BranchPenWidth*2 +10) then
                w := FNode[i].position.x -(CustomTextWidth(IntToStr(FNode[i].branch.stat2))*4 +BranchPenWidth*2 +10);
          end;
          if Fxbase +w < FX_BASE_DEFAULT then
            Fxbase := FX_BASE_DEFAULT -w;
        end
        else if ShowStats then begin
            w := 0;
            SetFontTarget(tftStats);
            for i := NoOfOTUs+1 to NoOfNodes do
            begin
              if FNode[i].hidden then Continue;
              if FNode[i] = FRoot then
                Continue
              else if FNode[i] = FRoot.des1 then begin
                if FRoot.des2.OTU or (FRoot.des2.position.x > FNode[i].position.x) then Continue;
              end
              else if FNode[i] = FRoot.des2 then begin
                if FRoot.des1.OTU or (FRoot.des1.position.x > FNode[i].position.x) then Continue;
              end;
              if w > FNode[i].position.x -(CustomTextWidth(IntToStr(FNode[i].branch.stat2))*4 +BranchPenWidth*2 +10) then
                  w := FNode[i].position.x -(CustomTextWidth(IntToStr(FNode[i].branch.stat2))*4 +BranchPenWidth*2 +10);

            end;
            if Fxbase +w < FX_BASE_DEFAULT then
              Fxbase := FX_BASE_DEFAULT -w;
        end
        else if ShowDivergenceTimes then
        begin
            w := 0;
            SetFontTarget(tftimes);
            for i := NoOfOTUs+1 to NoOfNodes do
            begin
              if FNode[i].hidden then Continue;
              if FNode[i] = FRoot then
                Continue
              else if FNode[i] = FRoot.des1 then begin
                if FRoot.des2.OTU or (FRoot.des2.position.x > FNode[i].position.x) then Continue;
              end
              else if FNode[i] = FRoot.des2 then begin
                if FRoot.des1.OTU or (FRoot.des1.position.x > FNode[i].position.x) then Continue;
              end;
              Stat := FNode[i].Height;
              StatStr := FloatToStrF(Stat, ffFixed, 8, DivTimeDecimals);

              if w > (FNode[i].position.x - (CustomTextWidth(StatStr)*4 +BranchPenWidth*2 +10)) then
                  w := FNode[i].position.x -(CustomTextWidth(StatStr)*4 +BranchPenWidth*2 +10);
            end;
            if (Fxbase + w) < FX_BASE_DEFAULT then
              Fxbase := FX_BASE_DEFAULT - w;
        end;

        if isLinearized and (not FTopoflag) and ShowHeightErrbar and assigned(HeightSEFunc) then
          for i := NoOfOTUs+1 to NoOfNodes do
          begin
            if FNode[i] = FRoot then continue;
            if FNode[i].position.X = FNode[i].anc.position.X then continue;
            w := FNode[i].position.X -round(2*HeightSEFunc(FNode[i].index)*Fxunit);
            if Fxbase +w < FX_BASE_DEFAULT then
              Fxbase := FX_BASE_DEFAULT -w;
            w := FNode[i].position.X +round(2*HeightSEFunc(FNode[i].index)*Fxunit) +10;
            if w > max.x then
              max.x := w;
          end;

        Fybase := FSvgFontHeight*2;
        if isStats then
        begin
            for i := NoOfOTUs+1 to NoOfNodes do
            begin
                if FNode[i] = FRoot then Continue;
                if FNode[i].hidden then Continue;
                h := FNode[i].position.y +Abs(StatsFontHeight)*4 +StatsMargin.Y +2;
                if h > max.y then max.y := h;
                h := FNode[i].position.y -Abs(StatsFontHeight)*4 -StatsMargin.Y -2;
                if h < 0  then
                    if Fybase < Abs(h) then
                        Fybase := Abs(h);
            end;
        end
        else if isTimes then
        begin
          for i := NoOfOTUs+1 to NoOfNodes do
          begin
              if FNode[i] = FRoot then Continue;
              if FNode[i].hidden then Continue;
              h := FNode[i].position.y +Abs(TimesFontHeight)*4 + TimesMargin.Y +2;
              if h > max.y then max.y := h;
              h := FNode[i].position.y -Abs(TimesFontHeight)*4 - TimesMargin.Y -2;
              if h < 0  then
                  if Fybase < Abs(h) then
                      Fybase := Abs(h);
          end;
        end;

        if isBranchLength and ShowBLen then
        begin
          if Fybase < (Abs(BLensFontHeight) + 5) * 4 then
            Fybase := (Abs(BLensFontHeight) + 5) * 4;
        end;
        Inc(Fybase, 15);

        max.x := max.x +Fxbase +FX_BASE_DEFAULT;

        if IsLinearized then
            if ShowScale and IsTimeScale and ShowTimeScale then
                max.y := max.y +Fybase*2 +Fyunit +FScaleFontHeight*10 +16
            else if ShowScale or (IsTimeScale and ShowTimeScale) then
                max.y := max.y +Fybase*2 +Fyunit +FScaleFontHeight*6 +16
            else
                max.y := max.y +Fybase*2
        else if IsScale and ShowScale and (not FTopoflag) then
            max.y := max.y +Fybase*2 +Fyunit +FScaleFontHeight*6 +16
        else
            max.y := max.y +Fybase*1 +20;

        if max.Y >= MAX_BITMAP_DIMENSION then
        begin
          SetPixelsPerOTU(math.max(trunc((Fyunit-(max.Y-MAX_BITMAP_DIMENSION)/NoOfOTUs)/4), 1));
          SetPosition;
        end;
      until (Fyunit = 4) or (max.Y < MAX_BITMAP_DIMENSION);  // Because of the upper limit of MinHeight is MAX_BITMAP_DIMENSION * 4
    end;
    FMinWidth := max.X div 4;
    FMinHeight := max.Y div 4;
end;

procedure TCustomSvgTree.Build;
begin
    if not TreeExist then Exit;
    SetPosition;
    SetTreeSize;
    if IsScale and ShowScale then
      if FScale < 0.0000000000001 then
        InitScale;
end;

procedure TCustomSvgTree.BuildNoDraw;
begin
  if not TreeExist then
    Exit;
  if not FRenderNewickOnly then
  begin
    MapInternalNodeNames;
    if Length(ConfidenceIntervals) > 0 then
      MapConfidenceIntervals;
  end;
  SortClusterInOrder;
  SetPosition;
  SetTreeSize;
  if (Length(LeafNodeCounts) > 0) or IsStudyTree then
    CountLeaves;
  if IsScale and ShowScale then
    if FScale < 0.0000000000001 then
      InitScale;
end;

procedure TCustomSvgTree.ApplyLogScale;
var
  i: Integer;
begin
  for i := NoOfOtus +1 to NoOfNodes-1 do
  begin
    //FNode[i].position.x := HeightScaledToLog10(FNode[i].height + log10(1+FNode[i].height)+1);
    FNode[i].position.x := HeightScaledToLog10(FNode[i].height +1);
    //if CompareValue(FNode[i].Height, 1.0, 0.01) > 0 then
    //  FNode[i].position.x := Min(FNode[1].position.x, HeightScaledToLog10(FNode[i].height))
    //else
    //  FNode[i].position.x := FNode[1].position.x;
  end;
end;

function TCustomSvgTree.HeightScaledToLog10(h: Double): Integer;
var
  aWidth: Integer;
  Proportion: Double;
begin
  aWidth := FNode[1].position.x - FRoot.position.x - fxbase;
  if h > 1.0 then
  begin
    Proportion := log10(h) / log10(FRoot.height);
    Result := Round(aWidth - Proportion*aWidth) + fxbase; { subtract the proportion because height goes right-to-left but x coords go left-to-right}
  end
  else
    Result := aWidth + fxbase;
  Assert(Result <= FNode[1].position.x);
end;

procedure TCustomSvgTree.MapInternalNodeNames;
var
  i: Integer;
  aString: String;
  ttId: String;
begin
  for i := (NoOfOTUs + 1) to NoOfNodes do
  begin
    ttId := IntToStr(FNode[i].timetreeId);
    if NamesMap.Contains(ttId) then
    begin
      aString := TOtuName(NamesMap[ttId]).Name;
      FNode[i].PrivateName := aString;
    end
    else
      FNode[i].PrivateName := 'unnamed';
  end;
end;

procedure TCustomSvgTree.MapConfidenceIntervals;
var
  i: Integer;
  ttId: Integer;
begin
  for i := (NoOfOTUs + 1) to NoOfNodes do
  begin
    ttId := FNode[i].timetreeId;
    if ttId >= 0 then
    begin
      FNode[i].ciLower := ConfidenceIntervals[ttId].LowerBound;
      FNode[i].ciUpper := ConfidenceIntervals[ttId].UpperBound;
      FNode[i].IsCI := ConfidenceIntervals[ttId].IsConfidenceInterval;
    end
    else
      FNode[i].IsCI := False;
  end;
end;

procedure TCustomSvgTree.Refresh;
begin

end;

function TCustomSvgTree.FindNextFocus(candidates: TList): Integer;
begin

end;

function TCustomSvgTree.StrLength(TheStr: AnsiString): Integer;
begin
  Result := CustomTextWidth(TheStr);
end;

function TCustomSvgTree.StrHeight(TheStr: AnsiString): Integer;
begin
  Result := CustomTextHeight(TheStr);
end;

procedure TCustomSvgTree.DrawTree(HMF :THandle);
begin

end;

procedure TCustomSvgTree.FocusOnNode(index: integer);
begin
  if (index <= 0) or (index > NoOfNodes) then Exit;
  if (index = FocusedIndex) and NodeFocused then Exit;

  ClearFocus;

  FFocusedIndex := index;
  FNodeFocused := true;

  FMarkedPos.X := FNode[index].position.x;
  FMarkedPos.Y := FNode[index].position.y;

end;

function TCustomSvgTree.CurrentFontHeight: Integer;
var
  i: Integer;
begin
  case FFontTarget of
  tftOtu: Result := 16;
  tftCharState: Result := 16;
  tftScale: Result := 16;
  tftStats: Result := 16;
  tftimes: Result := 16;
  tftCaption: Result := 16;
  tftNode: Result := 16;
  end;
end;

procedure TCustomSvgTree.CountLeaves;
var
  i: Integer;
  timetreeId: Integer;

  procedure CountLeavesRecursive(var aNode: TpNode);
  begin
    if not aNode.OTU then
    begin
      CountLeavesRecursive(aNode.des1);
      CountLeavesRecursive(aNode.des2);
      if aNode.des1.OTU then
        aNode.numLeaves := 1
      else
        aNode.numLeaves := aNode.numLeaves + aNode.des1.numLeaves;

      if aNode.des2.OTU then
        aNode.NumLeaves := aNode.numLeaves + 1
      else
        aNode.numLeaves := aNode.numLeaves + aNode.des2.numLeaves;
    end;
  end;

begin
  CountLeavesRecursive(FRoot);
  if not IsStudyTree then
  begin
    for i := 1 to NoOfOTUs do
    begin
      timeTreeId := FNode[i].timetreeId;
      FNode[i].numLeaves := GetLeafNodeCount(timeTreeId);
    end;
  end;
end;

procedure TCustomSvgTree.FocusOnBranch(nodeindex: integer);
begin
  if (nodeindex <= 0) or (nodeindex > NoOfNodes) then Exit;
  if (nodeindex = FocusedIndex) and BranchFocused then Exit;
  if FNode[nodeindex] = FRoot then Exit;

  ClearFocus;

  FFocusedIndex := nodeindex;
  FBranchFocused := true;

  FMarkedPos.X := (FNode[nodeindex].anc.position.x +FNode[nodeindex].position.x) div 2;
  FMarkedPos.Y := (FNode[nodeindex].anc.position.y +FNode[nodeindex].position.y) div 2;


end;

procedure TCustomSvgTree.FocusOnName(index: integer);
begin
  if (index <= 0) and (index > NoOfOTUs) then  Exit;
  //if FocusName((FNode[index].position.x +Fxbase) div 4, (FNode[index].position.y +Fybase) div 4) then
  //  EditName;
end;

procedure TCustomSvgTree.MoveFocus(direction: integer);
begin

end;

procedure TCustomSvgTree.ClearFocus;
begin
  FNodeFocused := false;
  FBranchFocused := false;
  FFocusedIndex := 0;

  FFocusedNameIndex := 0;
end;

function  TCustomSvgTree.FocusNode(x, y : Int64):boolean;
var
  i, ni,x0,y0,d : Integer;
begin
    Result := false;
    if NoOfNodes = 0 then Exit;
    if (x <= 0) and (y <= 0) then
    begin
        ClearFocus;
        Exit;
    end;
    d := BranchPenWidth div 2 +5;
    ni := 0;

    for i := NoOfOTUs+1 to NoOfNodes do
    begin
      if FNode[i].hidden then Continue;
      if (TreeStyle = tsCircle) then
      begin
        if (FNode[i] <> FRoot) and (FNode[i].avglen = FNode[i].anc.avglen) then
          Continue
      end
      else if (FNode[i] <> FRoot) then
        if FNode[i].position.x = FNode[i].anc.position.x then
          Continue;

      with FNode[i]^ do
      begin
        x0 := Round((position.x +Fxbase)/4);
        y0 := Round((position.y +Fybase)/4);
//        if (abs(y-y0) < 46340) and (Sqrt((x-x0)*(x-x0)+(y-y0)*(y-y0)) < d) then  // DAN: Bug fix, The addition of (abs(y-y0) > 46340) prevents integer overflow errors, because 46340^2 = 2,147,395,600.
																				   // KT: The maximum of integer is not 46340 but 32768, because 2^16 = 65536 = -32768..+32767. 65536^2 = 4,294,967,200 = -2,147,395,600..+2,147,395,600
//        if (abs(y-y0) < 32768) and (Sqrt((x-x0)*(x-x0)+(y-y0)*(y-y0)) < d) then        // DAN: Not sure what Koichiro means here, I still think 46340 is correct.

        if (abs(y-y0) < (MAX_BITMAP_DIMENSION div 4)) and (Sqrt((x-x0)*(x-x0)+(y-y0)*(y-y0)) < d) then
          begin
              ni := i;
              FMarkedPos.X := 4*x -Fxbase;
              FMarkedPos.Y := 4*y -Fybase;
              Break;
          end;
      end;
    end;

    if (ni = FocusedIndex) and (not BranchFocused) then Exit;
    ClearFocus;
    if ni > 0 then
    begin
      FFocusedIndex := ni;
      FNodeFocused := true;


    end;
    Result := NodeFocused;
end;

function TCustomSvgTree.FocusBranch(x,y : integer):boolean;
var i,bn,x0,x1,x2,x3,y0,y1,y2,y3,dx,dy,d,r,r0 : integer;
    q1,q2: TpNode;

    procedure GetCrossPoint;
    var a,b: double;
    begin
      a := (y1-y0)/(x1-x0);
      b := (x1*y0-x0*y1)/(x1-x0);
      x0 := round((y +x/a -b)/(a +1/a));
      y0 := round((a*y +x +b/a)/(a +1/a));
    end;

var a, a1, a2: double;
begin
    Result := false;
    if NoOfNodes = 0 then Exit;
    if (x <= 0) and (y <= 0) then begin
        ClearFocus;
        Exit;
    end;
    d := BranchPenWidth div 2 +5;
    bn := 0;

    for i := 1 to NoOfNodes do begin
        if FNode[i] = FRoot then Continue;
        if FNode[i].hidden then Continue;
        if TreeStyle = tsCircle then begin
            if (not FNode[i].OTU) and (FNode[i].avglen = FNode[i].anc.avglen) then
                Continue;
        end
        else if TreeStyle <> tsRadiation then
            if (not FNode[i].OTU) and (FNode[i].position.x = FNode[i].anc.position.x) then
                Continue;
        with FNode[i]^ do begin
            if (TreeStyle = tsTraditional) and (BranchStyle = bsRectangular) then
            begin
                x0 := round((anc.position.x +position.x +2*Fxbase)/8);
                dx := round((position.x -anc.position.x)/8);
                if dx < d then dx := d;
                y0 := round((position.y +Fybase)/4);
                dy := d;
            end
            else if (TreeStyle = tsTraditional) and (BranchStyle = bsCurved) then
            begin
                if FTopoflag then
                begin
                  q1 := FNode[i];
                  if q1 = q1.anc.des1 then
                    if q1.position.y < q1.anc.position.y then
                      while (q1 <> FRoot) and (q1.position.y <= q1.anc.position.y) do
                      begin
                        if q1 = q1.anc.des2 then
                        begin
                          q1 := q1.anc;
                          Break;
                        end;
                        q1 := q1.anc;
                      end
                    else
                      q1 := q1.anc
                  else
                    if q1.position.y > q1.anc.position.y then
                      while (q1 <> FRoot) and (q1.position.y >= q1.anc.position.y) do
                      begin
                        if q1 = q1.anc.des1 then
                        begin
                          q1 := q1.anc;
                          Break;
                        end;
                        q1 := q1.anc;
                      end
                    else
                      q1 := q1.anc;
                  q2 := FNode[i];
                  if q2.position.y < q1.position.y then
                    while not (q2.OTU or q2.compressed) do q2 := q2.des1
                  else
                    while not (q2.OTU or q2.compressed) do q2 := q2.des2;
                end
                else
                begin
                  q1 := FNode[i].anc;
                  q2 := FNode[i];
                end;
                x0 := round((q1.position.x +Fxbase)/4);
                y0 := round((q1.position.y +Fybase)/4);
                x1 := round((q2.position.x +Fxbase)/4);
                y1 := round((q2.position.y +Fybase)/4);
                x2 := round((anc.position.x +Fxbase)/4);
                y2 := round((anc.position.y +Fxbase)/4);
                x3 := round((position.x +Fxbase)/4);
                y3 := round((position.y +Fxbase)/4);
                if (x3-x2) < 2*d then
                begin
                    x0 := round((x2+x3)/2);
                    dx := d;
                    y0 := round((y2+y3)/2);
                    dy := round(abs(y3-y2)/2);
                    if dy < d then dy := d;
                end
                else if (x >= x2) and (x <= x3) then
                begin
                    a := abs(y1-y0)*sqrt(3)/(x1-x0);
                    if 2*(x-x0) < (x1-x) then
                      if y0 > y1 then
                        y0 := round(1/3*y0 +2/3*y1 +sqrt(4/9*(y1-y0)*(y1-y0)-a*a*(x-x0)*(x-x0)))
                      else
                        y0 := round(1/3*y0 +2/3*y1 -sqrt(4/9*(y1-y0)*(y1-y0)-a*a*(x-x0)*(x-x0)))
                    else
                      if y0 > y1 then
                        y0 := round(4/3*y0 -1/3*y1 -sqrt(16/9*(y1-y0)*(y1-y0) -a*a*(x-x1)*(x-x1)))
                      else
                        y0 := round(4/3*y0 -1/3*y1 +sqrt(16/9*(y1-y0)*(y1-y0) -a*a*(x-x1)*(x-x1)));
                    if abs(y3-y2) div (x3-x2) >= 5 then
                    begin
                      dy := d*abs(y3-y2) div (x3-x2);
                      if y2 < y3 then
                      begin
                        if y0-dy < y2 then
                          dy := y0 -y2;
                        if y0+dy > y3 then
                          dy := y3 -y0;
                      end
                      else
                      begin
                        if y0-dy < y3 then
                          dy := y0-y3;
                        if y0+dy > y2 then
                          dy := y2-y0;
                      end;
                    end
                    else
                      dy := d;
                    x0 := round((x2+x3)/2);
                    dx := round((x3-x2)/2);
                    if dx < d then dx := d;
                end
                else
                begin
                    dy := -d;
                    dx := -d;
                end;
            end
            else begin
                if TreeStyle = tsCircle then
                begin
                  x0 := round((position.x +Fxbase)/4);
                  y0 := round((position.y +Fybase)/4);
                  x1 := round((position.x -round(cos(angle)*(avglen-anc.avglen)) +Fxbase)/4);
                  y1 := round((position.y +round(sin(angle)*(avglen-anc.avglen)) +Fybase)/4);
                end
                else
                begin
                  x0 := round((position.x +Fxbase)/4);
                  y0 := round((position.y +Fybase)/4);
                  x1 := round((anc.position.x +Fxbase)/4);
                  y1 := round((anc.position.y +Fybase)/4);
                end;
                if abs(x0-x1) <= 2*d then
                begin
                  dx := d;
                  dy := round(abs(y0-y1)/2);
                  x0 := round((x0+x1)/2);
                  y0 := round((y0+y1)/2);
                end
                else if abs(y0-y1) <= 2*d then
                begin
                  dx := round(abs(x0-x1)/2);
                  dy := d;
                  x0 := round((x0+x1)/2);
                  y0 := round((y0+y1)/2);
                end
                else
                begin
                  x2 := round((x0+x1)/2);
                  y2 := round((y0+y1)/2);
                  dx := round(abs(x0-x1)/2) +d;
                  dy := round(abs(y0-y1)/2) +d;
                  if (x >= x2-dx) and (x <= x2+dx)
                  and (y >= y2-dy) and (y <= y2+dy) then
                    GetCrossPoint;
                  dx := d;
                  dy := d;
                end;
            end;

            if (y >= y0-dy) and (y <= y0+dy) then
                if (x >= x0-dx) and (x <= x0+dx) then
                    begin
                        bn := i;
                        FMarkedPos.X := 4*x -Fxbase;
                        FMarkedPos.Y := 4*y -Fybase;
                        Break;
                    end;
        end;
    end;

    if (bn = FocusedIndex) and (not NodeFocused) then Exit;
    ClearFocus;

    if bn <> FocusedIndex then begin
      FFocusedIndex := bn;
      FBranchFocused := true;
    end;
    Result := BranchFocused;
end;

function TCustomSvgTree.FocusName(x, y: integer): boolean;
begin

end;


function TCustomSvgTree.GetScaleDecimals: integer;
begin
  result := Length(FScaleText) -Pos('.', FScaleText);
end;

procedure TCustomSvgTree.SetScaleText(text : AnsiString);
begin
    FScaleText := text;
    FScale := StrToFloat(ScaleText);
end;

function TCustomSvgTree.GetIsScale:boolean;
begin
  Result := False;
  //Result := (Fxmax > 0.000000000000001) and (not FTopoflag);
end;

function TCustomSvgTree.GetIsTimeScale:boolean;
begin
  Result := IsScale and (abs(FTimeScale) >= 0.000000000000001);
end;

procedure TCustomSvgTree.SetTimeText(text : AnsiString);
begin
    FTimeText := text;
    FTimeScale := StrToFloat(TimeText);
end;

procedure TCustomSvgTree.CompressCluster(index: integer);

    procedure MarkNode(p : TpNode);
    begin
        p.hidden := true;
        if p.OTU then Exit;
        MarkNode(p.des1);
        MarkNode(p.des2);
    end;

begin
    if index = 0 then Exit;
    if index = FRoot.index then Exit;
    if FNode[index].hidden then Exit;
    if FNode[index].compressed then Exit;
    FNode[index].compressed := true;
    if FNode[index].OTU then Exit;
    MarkNode(FNode[index].des1);
    MarkNode(FNode[index].des2);

end;

procedure UnMarkNode(p : TpNode);
begin
    p.hidden := false;
    if p.OTU then Exit;
    if p.compressed then Exit;
    UnMarkNode(p.des1);
    UnMarkNode(p.des2);
end;

procedure TCustomSvgTree.ExpandCluster(index: integer);
begin
    if index <= 0 then Exit;
    if FNode[index].hidden then Exit;
    if not FNode[index].compressed then Exit;
    FNode[index].compressed := false;
    if FNode[index].OTU then Exit;
    UnMarkNode(FNode[index].des1);
    UnMarkNode(FNode[index].des2);
end;

procedure TCustomSvgTree.ExpandAllCluster(index: integer);

    procedure ExpandClusters(p : TpNode);
    begin
        p.compressed := false;
        if not p.OTU then
        begin
          ExpandClusters(p.des1);
          ExpandClusters(p.des2);
        end;
    end;

begin
    if index <= 0 then begin
        ExpandClusters(FRoot);
        UnMarkNode(FRoot.des1);
        UnMarkNode(FRoot.des2);
    end
    else if index > NoOfOTUs then begin
        ExpandClusters(FNode[index]);
        UnMarkNode(FNode[index].des1);
        UnMarkNode(FNode[index].des2);
    end;
//    Refresh;
end;

procedure TCustomSvgTree.SortClusterInOrder;
begin
    SortBranchByOrder(FRoot);
end;

procedure TCustomSvgTree.SortClusterForShape;
begin
    SortBranchByFigure(FRoot);
end;

procedure TCustomSvgTree.SetDistanceMatrix(value: PDistanceMatrix);
begin
  if value = FDistanceMatrix then Exit;
  FDistanceMatrix := value;
  if (value <> nil) and TreeExist then
     SetClusterHeight;
end;

procedure TCustomSvgTree.SetClusterHeightFromDistances;
var n1, n2 : PArrayOfInt;
    i, j, k : integer;
    h : double;

    procedure SearchOTU(p : TpNode; n : PArrayOfInt);
    begin
        if p.OTU then begin
            Inc(n[0]);
            n[n[0]] := p.Index;
        end
        else begin
            SearchOTU(p.des1, n);
            SearchOTU(p.des2, n);
        end;
    end;

    procedure SetNum(p : TpNode);
    begin
        n1[0] := 0;
        SearchOTU(p.des1, n1);
        n2[0] := 0;
        SearchOTU(p.des2, n2);
    end;

begin
  n1 := nil;
  n2 := nil;
  try
    if not TreeExist then Exit;
    GetMem(n1, SizeOf(Integer)*NoOfOTUs);
    GetMem(n2, SizeOf(Integer)*NoOfOTUs);
    for k := NoOfOTUs+1 to NoOfNodes do begin
        SetNum(FNode[k]);
        h := 0.0;
        for i := 1 to n1[0] do
          for j := 1 to n2[0] do
            h := h + DistanceMatrix[n1[i]-1][n2[j]-1];
        h := h/n1[0]/n2[0]/2;
        FNode[k].Height := h;
    end;
  finally
    FreeMemAndNil(n1);
    FreeMemAndNil(n2);
  end;
end;

procedure TCustomSvgTree.SetClusterHeightByBranchLengths;

  procedure SetEachHeight(p : TpNode);
  begin
    with p^ do
      if OTU then
      begin
        height := 0.0;
        h0     := 0;
      end
      else
      begin
        SetEachHeight(des1);
        SetEachHeight(des2);

        h0     := ((des1.h0+des1.branch.length)*des1.size
                  +(des2.h0+des2.branch.length)*des2.size)/size;


        height := (des1.h0+des1.branch.length
                  +des2.h0+des2.branch.length)/2;
      end;
    end;

begin
  SetEachHeight(FRoot);
end;

procedure TCustomSvgTree.SetClusterHeightByLinearizeFunc;
var
  tree: TTimeTreeData;

  procedure SetRootHeight;
  var
    h: double;

    procedure GetNodeHeight(n: TpNode);
    begin
      h := h +tree.BLen[n.index-1];
      if n.OTU then
      begin
        if h > FRoot.height then
          FRoot.height := h;
      end
      else
      begin
        GetNodeHeight(n.des1);
        GetNodeHeight(n.des2);
      end;
      h := h -tree.BLen[n.index-1];
    end;

  begin
    h := 0;
    GetNodeHeight(FRoot.des1);
    GetNodeHeight(FRoot.des2);
  end;

  procedure SetEachClusterHeight(n: TpNode);
  begin
    n.height := n.anc.height -tree.BLen[n.index-1];
    if not n.OTU then
    begin
      SetEachClusterHeight(n.des1);
      SetEachClusterHeight(n.des2);
    end;
  end;

var
  i,r: integer;
  h: array of boolean;
begin
  if (not IsLinearized) and (not IsTimes)  then
  begin
    SetClusterHeightByBranchLengths;
    exit;
  end;

  tree := TTimeTreeData.Create(NoOfOTUs,true,isSE,false);

  for i := 1 to NoOfOTUs-1 do
  begin
    tree.NodeArray[i-1].des1 := FNode[NoOfOTUs+i].des1.index-1;
    tree.NodeArray[i-1].des2 := FNode[NoOfOTUs+i].des2.index-1;
  end;
  for i := 0 to NoOfNodes-2 do
  begin
    tree.BLen[i] := FNode[i+1].branch.length;
    if isSE then
      tree.SE[i] := FNode[i+1].branch.SE;
  end;

  LinearizeFunc(tree);

  SetRootHeight;
  SetEachClusterHeight(FRoot.des1);
  SetEachClusterHeight(FRoot.des2);

  tree.Free;
end;

procedure TCustomSvgTree.SetClusterHeight;
begin
  if not TreeExist then Exit;
  if IsLinearized then
    SetClusterHeightByBranchLengths
  else if assigned(LinearizeFunc) then
    SetClusterHeightByLinearizeFunc
  else
    SetClusterHeightByBranchLengths;
end;

procedure TCustomSvgTree.DrawPolygon(Points: array of TPoint; NumPoints: Integer
  );
begin

end;

procedure TCustomSvgTree.SetIsLinearized(value: boolean);
begin
  FIsLinearized := value;

end;

procedure TCustomSvgTree.SetIsRooted(const AValue: Boolean);
begin
  FIsRooted := AValue;
end;

procedure TCustomSvgTree.SetGroupIndex;

  procedure SetGroupIndexOnNode(p: TpNode);
  begin
    if p.OTU then exit;

    SetGroupIndexOnNode(p.des1);
    SetGroupIndexOnNode(p.des2);

    if p.des1.groupindex = p.des2.groupindex then
      p.groupindex := p.des1.groupindex
    else
      p.groupindex := -1;
  end;

begin
  SetGroupIndexOnNode(FRoot);
end;

procedure TCustomSvgTree.SetUseSubtreeAttrib(value: boolean);
begin
  if value = FUseSubtreeAttrib then exit;
  FUseSubtreeAttrib :=value;

  if not value then
    ExpandAllCluster(FRoot.index);

  SetAttrindex;
end;

procedure TCustomSvgTree.SetUseGroupAttrib(value: boolean);
begin
  if value = FUseGroupAttrib then exit;
  FUseGroupAttrib := value;

  if not value then
    ExpandAllCluster(FRoot.index);

  SetAttrindex;
end;

procedure TCustomSvgTree.SetAttrIndex;

  procedure SetDescend(p: TpNode);
  var
    a: TNodeAttrib;
    size: integer;
  begin
    if p <> FRoot then
    begin
      if p.attrindex = 0 then
        if (p.anc.attrindex <= 0) or not AttribList[p.anc.attrindex].OverwriteDownstream then
          if UseGroupAttrib and (p.groupindex <> p.anc.groupindex) then
            p.attrindex := -(p.groupindex+1);

      if p.attrindex = 0 then
      begin
        p.attrindex := p.anc.attrindex;
        if p.name = '' then
        begin
          p.namesize.x := 0;
          p.namesize.y := 0;
        end
        else
        begin
          a := GetNodeAttrib(p);
          SetFontTarget(tftNode);
          p.namesize.x := CustomTextWidth(p.name+' ');
          if p.PrivateName <> EmptyStr then
          begin
            p.namesize.x := p.namesize.x + (StrLength(p.PrivateName+' ') * 4);
          end;
            //p.namesize.x := p.namesize.x + (Canvas.TextWidth(p.PrivateName+' ') * 4);
          p.namesize.y := CurrentFontHeight*4;
        end;
        if p.speciesName = '' then
        begin
          p.speciesNameSize.x := 0;
          p.speciesNameSize.y := 0;
        end
        else
        begin
          a := GetNodeAttrib(p);
          SetFontTarget(tftNode);
          p.speciesNameSize.x := CustomTextWidth(p.speciesName+' ')*4;
          if p.PrivateName <> EmptyStr then
          begin
            p.speciesNameSize.x := p.speciesNameSize.x + (StrLength(p.PrivateName+' ') * 4);
          end;
          p.speciesNameSize.y := CurrentFontHeight*4;
        end;
      end
      else if p.attrindex <> p.anc.attrindex then
      begin
        a := GetNodeAttrib(p);

        if a.Compressed then
          CompressCluster(p.index);

//        a.CaptionSize := Canvas.TextWidth(a.Caption)*4;
        if p.OTU then
        begin
          if p.compressed then
          begin
            SetFontTarget(tftCaption);
            p.namesize.x := CustomTextWidth(a.Caption+' ')*4;
            p.speciesNameSize.x := CustomTextWidth(a.Caption + ' ') * 4;
          end
          else
          begin
            SetFontTarget(tftNode);
            p.namesize.x := CustomTextWidth(p.name+' ')*4;
            if p.SpeciesName <> EmptyStr then
            begin
              p.speciesNameSize.x := CustomTextWidth(p.SpeciesName + ' ') * 4;
              if p.PrivateName <> EmptyStr then
                p.speciesNameSize.x := p.speciesNameSize.x + (StrLength(p.PrivateName+' ') * 4);
            end;
            if p.PrivateName <> EmptyStr then
            begin
              p.namesize.x := p.namesize.x + (StrLength(p.PrivateName+' ') * 4);
            end;
          end;
        end
        else
        begin
          p.name := a.Caption;
          SetFontTarget(tftCaption);
          p.namesize.x := CustomTextWidth(a.Caption+' ')*4;
          p.speciesNamesize.x := CustomTextWidth(p.SpeciesName + ' ') * 4;
        end;
        p.namesize.y := CurrentFontHeight*4;
        p.speciesNameSize.y := CurrentFontHeight * 4;
        SetFontTarget(tftCaption);
        a.CaptionSize := 0;

        if a.Compressed then
        begin
          if a.ShowTaxonMarker and (a.Marker.Shape <> msNone) then
            a.CaptionSize := a.CaptionSize +abs(a.CaptionFontHeight)*5;
        end
        else if a.ShowBracket then
          a.CaptionSize := a.CaptionSize +abs(a.CaptionFontHeight)*2 +a.BracketLineWidth*4;

        size := CustomTextWidth(a.Caption+' ')*4;
        if a.ShowLabel then
            a.CaptionSize := a.CaptionSize +size;

        if a.CaptionSize > 0 then
          a.CaptionSize := a.CaptionSize +abs(a.CaptionFontHeight)*2;
      end;
    end;
    if not p.OTU then
    begin
      SetDescend(p.des1);
      SetDescend(p.des2);
    end;
  end;

var i: integer;
begin
  if not Assigned(FRoot) then  // in case somehow the topology editor got us here without being setup properly
    Exit;
  for i := 1 to NoOfNodes do
    FNode[i].attrindex := 0;

  ExpandAllCluster(FRoot.index);

  if UseGroupAttrib then
  begin
    SetGroupIndex;
    for i := 1 to NoOfNodes do
      if (FNode[i] <> FRoot) and (FNode[i].groupindex <> FNode[i].anc.groupindex) then
      begin
        if (i > NoOfOTUs) or (FNode[i].marker.Shape = msNone) then
          FNode[i].marker := GroupAttrib[FNode[i].groupindex].Marker;
      end;
  end;

  if UseSubtreeAttrib then
  begin
    if AttribList.Count > 1 then
      for i := 1 to AttribList.Count-1 do
      begin
        FNode[AttribList[i].nodeindex].attrindex := i;
        FNode[AttribList[i].nodeindex].marker := AttribList[i].marker;
      end;
  end;

  if UseGroupAttrib or UseSubtreeAttrib then
    SetDescend(FRoot);

  SetClusterWidth(FRoot);
end;

function TCustomSvgTree.OTUCoords(Index: Integer): TRect;
begin

end;

function TCustomSvgTree.NodeCoords(NodeIndex: Integer): TRect;
begin

end;

procedure TCustomSvgTree.ClearAttrib;
var i: integer;
begin
  if AttribList.Count <= 1 then Exit;
  for i := AttribList.Count-1 to 1 do
  begin
    AttribList[i].Free;
    AttribList.delete(i);
  end;
end;

procedure TCustomSvgTree.InitMem;
var i : integer;
begin
    GetMem(FNode,SizeOf(TpNode)*NoOfNodes);
    for i := 1 to NoOfOTUs do
    begin
        New(FNode[i]);
        FNode[i].index:= i;
        FNode[i].minOTU := i;
        FNode[i].size := 1;
        FNode[i].depth := 0;
        FNode[i].height := 0.0;
        FNode[i].h0 := 0;
        FNode[i].maxh := 0;
        FNode[i].minh := 0;
        FNode[i].rate := 0;
        FNode[i].width := 0;
        FNode[i].anc := nil;
        FNode[i].des1 := nil;
        FNode[i].des2 := nil;
        FNode[i].OTU := true;
        FNode[i].compressed := false;
        FNode[i].marker.shape := msNone;
        //FNode[i].marker.color := clBlack;
        FNode[i].outgroup := false;
        FNode[i].hidden := false;
        FNode[i].flag := false;
        FNode[i].hilighted := false;
        FNode[i].emphasized := false;
        //FNode[i].CustomHighlightColor := clWhite;
        FNode[i].angle := 0.0;
        FNode[i].branch.length := 0.0;
        FNode[i].branch.SE := 0.0;
        FNode[i].branch.maxlen1 := 0.0;
        FNode[i].branch.maxlen2 := 0.0;
        FNode[i].branch.bootnum := 0;
        FNode[i].branch.stats := 0.0;
        FNode[i].branch.stat2 := 0;
        FNode[i].attrindex := 0;
        FNode[i].namesize.x := 0;
        FNode[i].namesize.y := 0;
        FNode[i].groupindex := -1;
        FNode[i].capdepth   := 0;
        FNode[i].bracket.Left   := 0;
        FNode[i].bracket.Right  := 0;
        FNode[i].bracket.Top    := 0;
        FNode[i].bracket.Bottom := 0;
        FNode[i].numLeaves := 0;
    end;
    for i := NoOfOTUs+1 to NoOfNodes do
    begin
        NEW(FNode[i]);
        FNode[i].index:= i;
        FNode[i].size := 0;
        FNode[i].depth := 0;
        FNode[i].height := 0.0;
        FNode[i].h0 := 0;
        FNode[i].maxh := 0;
        FNode[i].minh := 0;
        FNode[i].rate := 0;
        FNode[i].width := 0;
        FNode[i].anc := nil;
        FNode[i].des1 := nil;
        FNode[i].des2 := nil;
        FNode[i].name := '';
        FNode[i].SpeciesName := EmptyStr;
        FNode[i].PrivateName := '';
        FNode[i].OTU := false;
        FNode[i].compressed := false;
        FNode[i].marker.shape := msNone;
        //FNode[i].marker.color := clBlack;
        FNode[i].outgroup := false;
        FNode[i].hidden := false;
        FNode[i].flag := false;
        FNode[i].hilighted := false;
        FNode[i].emphasized := false;
        FNode[i].angle := 0.0;
        FNode[i].branch.length := 0.0;
        FNode[i].branch.SE := 0.0;
        FNode[i].branch.maxlen1 := 0.0;
        FNode[i].branch.maxlen2 := 0.0;
        FNode[i].branch.bootnum := 0;
        FNode[i].branch.stats := 0.0;
        FNode[i].branch.stat2 := 0;
        FNode[i].attrindex := 0;
        FNode[i].namesize.x := 0;
        FNode[i].namesize.y := 0;
        FNode[i].groupindex := -1;
        FNode[i].capdepth   := 0;
        FNode[i].bracket.Left   := 0;
        FNode[i].bracket.Right  := 0;
        FNode[i].bracket.Top    := 0;
        FNode[i].bracket.Bottom := 0;
        FNode[i].numLeaves := 0;
    end;
    FRoot := nil;
    ClearAttrib;
end;

procedure TCustomSvgTree.ResetMem;
var i : integer;
begin
    if NoOfOTUs = 0 then Exit;

    if FNode <> nil then
      for i := 1 to NoOfNodes do
        Dispose(FNode[i]);
    FreeMemAndNil(FNode,SizeOf(TpNode)*NoOfNodes);
    FNode := nil;
    FreeDistMatrix(FDistanceMatrix, NoOfOTUs);
    FDistanceMatrix := nil;
    ClearAttrib;

    FFocusedIndex := 0;
    FFocusedNameIndex := 0;
    FNodeFocused := false;
    FBranchFocused := false;
    FTitle := '';
    FDescription := '';
    FSource := '';
    FDistance := '';
    FGap := '';
    FMethod := '';
    FisValue := false;
    FisValue2 := false;
    FisStats := false;
    FisSE := false;
    FisRooted := false;
    FValueName := '';
    FValue2Name := '';
    FStatsName := '';
    FValue := 0.0;
    FValue2 := 0.0;
    FMaxStats := 0.0;
    FNoOfOTUs := 0;
    FPixelsPerUnit := 0;
    FSBL := 0.0;
    FScale := 0.0;
    FScaleTick := 0.0;
    FScaleText := '';
    FTimeScale := 0.0;
    FTimeTick := 0.0;
    FTimeText := '';
    FTreeExist := false;
end;

procedure TCustomSvgTree.Clear;
begin
    if NoOfOTUs > 0 then ResetMem;
end;

function TCustomSvgTree.GetLongestPath:double;
var
  i: integer;
begin
  result := 0.0;
  if ForceLinearized and (FRoot.height > 0.0000000000001) then
    result := 2*FRoot.height
  else if isBranchLength then
    for i := 1 to NoOfOTUs do
      if result < FNode[i].branch.maxlen1 then
        result := FNode[i].branch.maxlen1;
end;

function TCustomSvgTree.GetMinBranchLength:double;
var
  i: integer;
begin
  result := 0.0;
  if not isBranchLength then
    Exit;
  result := LongestPath;
  if (IsLinearized or ForceLinearized) then
    for i := 1 to NoOfNodes do
    begin
      if FNode[i] = FRoot then Continue;
      if result > (FNode[i].anc.height-FNode[i].height) then
        result := FNode[i].anc.height-FNode[i].height;
    end
  else
    for i := 1 to NoOfNodes do
    begin
      if FNode[i] = FRoot then Continue;
      if result > FNode[i].branch.length then
        result := FNode[i].branch.length;
    end;

end;

function TCustomSvgTree.GetMaxBranchLength:double;
var
  i: integer;
begin
  result := 0.0;
  if not isBranchLength then
    Exit;
  for i := 1 to NoOfNodes do begin
    if FNode[i] = FRoot then Continue;
    if result < FNode[i].branch.length then
      result := FNode[i].branch.length;
  end;
end;

procedure TCustomSvgTree.SetRootAtLast;
var d1,d2 : TpNode;
    b : TBranch;
begin
    if FNode[NoOfNodes] = FNode[NoOfNodes].anc.des1 then
        FNode[NoOfNodes].anc.des1 := FRoot
    else
        FNode[NoOfNodes].anc.des2 := FRoot;
    FRoot.anc := FNode[NoOfNodes].anc;
    FNode[NoOfNodes].anc := nil;

    d1 := FRoot.des1;
    d2 := FRoot.des2;

    FRoot.des1 := FNode[NoOfNodes].des1;
    FRoot.des2 := FNode[NoOfNodes].des2;
    FNode[NoOfNodes].des1.anc := FRoot;
    FNode[NoOfNodes].des2.anc := FRoot;

    FNode[NoOfNodes].des1 := d1;
    FNode[NoOfNodes].des2 := d2;
    d1.anc := FNode[NoOfNodes];
    d2.anc := FNode[NoOfNodes];

    b := FRoot.branch;
    FRoot.branch := FNode[NoOfNodes].branch;
    FNode[NoOfNodes].branch := b;

    FRoot := FNode[NoOfNodes];
end;

procedure TCustomSvgTree.InitTree;

    function MinBLenDecimals:integer;
    begin
      result := 0;
      if LongestPath > 0 then
        if floor(log10(LongestPath)) > 0 then
          result := 0
        else
          result := -floor(log10(LongestPath)) +1;
    end;

var
  i,j : integer;
begin
    for i := NoOfOTUs+1 to NoOfNodes do
        if FNode[i].anc = nil then begin
            FRoot := FNode[i];
            Break;
        end;

    if FRoot <> FNode[NoOfNodes] then SetRootAtLast;

    FSBL := 0.0;
    for i := 1 to NoOfNodes-1 do
      FSBL := FSBL +FNode[i].branch.length;

    SetStats;
    if isBranchLength then
    begin
      SetMaxLength(FRoot);
      if FRoot.des1.branch.maxlen2 > FRoot.des2.branch.maxlen2 then
          Fxmax := FRoot.des1.branch.maxlen2
      else
          Fxmax := FRoot.des2.branch.maxlen2;
    end;
    if isBranchLength then
    begin
      if FBLenDecimals < MinBLenDecimals then
        FBLenDecimals := MinBLenDecimals;
    end;

    if isStats then
    begin
      FStatsMargin.X := BranchPenWidth*2 +4*4;
      i := (Fyunit div 2) -CurrentFontHeight*2;
      j := BranchPenWidth*2 +2*4;
      if i < j then
        FStatsMargin.Y := i
      else
        FStatsMargin.Y := j;
    end;

    if isBranchLength then
    begin
      FTopoflag := FTopologyOnly;
    end
    else
    begin
      FTopoflag := true;
    end;
    ShowOTUName := true;
    ShowOTUMarker := true;
    FTreeStyle := tsTraditional;
    FBranchStyle := bsRectangular;
    SetClusterSize(FRoot);
    SetClusterWidth(FRoot);
    if not isBranchFixed then
    begin
      if not isRooted then
      begin
        if isOutgroup then
        begin
          for i := 1 to NoOfOTUs do
            FNode[i].flag := FNode[i].outgroup;
          MoveRoot(SearchCommonAncestor(FRoot),true);
        end
        else if isBranchLength then
          MoveRoot(FNode[SearchMidPoint],true);
        SetClusterSize(FRoot);
        SetClusterWidth(FRoot);
      end;
    end;
    SetClusterHeight;

    SetAttrindex;

    SetPosition;
    SetTreeSize;

    InitScale;

    FTreeExist := true;
end;

procedure TCustomSvgTree.SetCharStateFontHeight(AValue: Integer);
begin
  if FCharStateFontHeight=AValue then Exit;
  FCharStateFontHeight:=AValue;
end;

procedure TCustomSvgTree.SetDoLogScale(AValue: Boolean);
begin
  if FDoLogScale=AValue then Exit;
  FDoLogScale:=AValue;
  //if AValue = true then
  //  FForceLinearized := True;
  ShowTopologyOnly := FDoLogScale;
end;

procedure TCustomSvgTree.SetFUserSuppliedATaxaList(AValue: Boolean);
begin
  if FUserSuppliedATaxaList=AValue then Exit;
  FUserSuppliedATaxaList:=AValue;
end;

procedure TCustomSvgTree.SetNameSpacing(AValue: Integer);
begin
  if FNameSpacing=AValue then Exit;
  FNameSpacing:=AValue;
  Fyunit := CustomTextHeight('Abc', 12)*FNameSpacing;
end;

function TCustomSvgTree.IsFocusedOnRoot: Boolean;
begin
  Result := (FocusedIndex = FRoot.Index);
end;

function TCustomSvgTree.GetIsDistanceMatrix:boolean;
begin
  Result := (FDistanceMatrix <> nil);
end;

procedure TCustomSvgTree.SetCenterMargin(value: integer);
begin
  if (value >= 20) and (value <= 80) then
    FCenterMargin := value;
end;


procedure TCustomSvgTree.GetDescName(nodeindex: integer; names: TStringList);

  procedure GetDescNameRecursive(node: TpNode);
  begin
    if node.OTU then
      names.Add(node.name)
    else
    begin
      GetDescNameRecursive(node.des1);
      GetDescNameRecursive(node.des2);
    end;
  end;


begin
  names.Clear;
  GetDescNameRecursive(FNode[nodeindex]);
end;

///////////////////////////////////////////////////////
// TSvgTreeBox
///////////////////////////////////////////////////////

constructor TSvgTreeBox.Create;
begin
    inherited;
    FNoOfTrees := 0;
    TreeList := nil;
end;

destructor TSvgTreeBox.Destroy;
begin
    if Self <> nil then
      inherited Destroy;
    //if Assigned(TreeList) then
    //  FreeAndNil(TreeList);
    //if Assigned(PartitionList) then
    //  PartitionList.Free;
    if Assigned(ConsTree) then
      ConsTree.Free;
end;

procedure TSvgTreeBox.AssignTreeAttrib(Source: TCustomSvgTree);
var
  OtherTree: TSvgTreeBox;
  i: Integer;
begin
  inherited;

  if Source is TSvgTreeBox then
  begin
    FConsensusValue  := TSvgTreeBox(Source).FConsensusValue;
    FFreqDecimals := TSvgTreeBox(Source).FFreqDecimals;

    if (TSvgTreeBox(Source).TreeIndex = 0) and (TreeIndex <> 0) then
      CondenseValue := TSvgTreeBox(Source).ConsCondenseValue
    else if (TreeIndex = 0) and ((TSvgTreeBox(Source).TreeIndex <> 0)) then
    begin
      ConsCondenseValue := TSvgTreeBox(Source).FCondenseValue;
      FCondenseValue    := ConsensusValue;
    end;
    OtherTree := TSvgTreeBox(Source);
    FShowDivergenceTimes := OtherTree.ShowDivergenceTimes;
    FShowHeightErrBar := OtherTree.ShowHeightErrBar;
    FIsLinearized := OtherTree.IsLinearized;
    FTimeText := OtherTree.TimeText;
  end;
end;

procedure TSvgTreeBox.SaveTreeData(index : integer);
var i : integer;
    tree : TTimeTreeData;
begin
    if index = 0 then
        tree := ConsTree
    else
        tree := TreeList[index-1];
    for i := 1 to NoOfOTUs-1 do begin
        tree.NodeArray[i-1].des1 := FNode[NoOfOTUs+i].des1.index-1;
        tree.NodeArray[i-1].des2 := FNode[NoOfOTUs+i].des2.index-1;
        tree.IsOutgroupMember[i-1] := FNode[i].outgroup;
        treelist.InternalNodeLbl[i-1] := FNode[NoOfOtus+i].name;
    end;
    if tree.isBLen then
        for i := 0 to NoOfNodes-2 do
            tree.BLen[i] := FNode[i+1].branch.length;
    if tree.isSE then
        for i := 0 to NoOfNodes-2 do
            tree.SE[i] := FNode[i+1].branch.SE;
        for i := 0 to NoOfNodes-2 do
            tree.Stats[i] := FNode[i+1].branch.stats;
end;


procedure TSvgTreeBox.LoadTreeData(index : integer);
var i,j,k : integer;
    tree : TTimeTreeData;
begin
    if index = 0 then
      tree := ConsTree
    else
      tree := TreeList[index-1];

    if (not ShowTopologyOnly) and (@BLenFunc <> nil) and (tree.SBL < 0.000000000001) then
    begin
      if not tree.isBLen then
        tree.isBLen := true;
      BLenFunc(tree);
      if @SEFunc <> nil then
      begin
        if not tree.isSE then
          tree.isSE := true;
        SEFunc(tree);
      end;
      Initialized[index] := false;
    end;

    FTopoflag := (not tree.isBLen) or ShowTopologyOnly or IsCondensed;

    FValue := tree.Value;
    FValue2 := tree.Value2;
    for i := 1 to NoOfNodes do begin
        FNode[i].anc := nil;
        FNode[i].compressed := false;
        FNode[i].hidden := false;
        FNode[i].flag := false;
    end;

    for i := 1 to NoOfOTUs-1 do
    begin
        j := tree.NodeArray[i-1].des1+1;
        k := tree.NodeArray[i-1].des2+1;
        FNode[NoOfOTUs+i].des1 := FNode[j];
        FNode[NoOfOTUs+i].des2 := FNode[k];
        FNode[j].anc := FNode[NoOfOTUs+i];
        FNode[k].anc := FNode[NoOfOTUs+i];
        if Index <> 0 then
          FNode[NoOfOTUs+i].name := TreeList.InternalNodeLbl[i-1]
        else
          FNode[NoOfOTUs+i].name := EmptyStr;
        FNode[NoOfOTUs+i].charstate := EmptyStr;
    end;

    for i := NoOfNodes downto NoOfOTUs+1 do
        if FNode[i].anc = nil then
        begin
            FRoot := FNode[i];
            Break;
        end;

    for i := 1to NoOfOtus do
      FNode[i].numLeaves := tree.GetChildNodeCount(i-1);

    if tree.isBLen then
        for i := NoOfNodes downto 1 do
        begin
          FNode[i].timetreeID := tree.GetTimetreeID(i-1);
          if FNode[i] = FRoot then Continue;
          FNode[i].branch.length := tree.BLen[i-1];
        end;
    //InitDivTimes;
    //if DoLogScale then
    //begin
    //
    //  for i := 1 to NoOfNodes do
    //  begin
    //    if FNode[i].branch.length > 0.0000000001 then
    //      FNode[i].branch.length := log10(FNode[i].branch.length+1);
    //  end;
    //end;

    if tree.isSE then
        for i := 1 to NoOfNodes do begin
            if FNode[i] = FRoot then Continue;
            FNode[i].branch.SE := tree.SE[i-1];
        end;
      for i := 1 to NoOfNodes do
      begin
        if FNode[i] = FRoot then Continue;
          FNode[i].branch.stats := tree.Stats[i-1];
      end;
    FTreeExist := true;
end;

//procedure TSvgTreeBox.InitDivTimes;
//
//  procedure ProcessNode(aNode: TpNode);
//  begin
//    if Assigned(aNode.des1) then
//      ProcessNode(aNode.des1);
//    if Assigned(aNode.des2) then
//      ProcessNode(aNode.des2);
//    if not aNode.OTU then
//    begin
//      if aNode.des1.OTU then
//        aNode.divtime := aNode.des1.branch.length
//      else
//        aNode.divtime := (aNode.des1.divtime + aNode.des2.divtime) * 0.5;
//    end;
//  end;
//
//begin
//  ProcessNode(FRoot);
//end;

procedure TSvgTreeBox.SetOutgroups(outgroup : boolean);
var i : integer;

    procedure MarkOutgroup(n: TpNode);
    begin
        if n.OTU then
            n.flag := true
        else begin
            MarkOutgroup(n.des1);
            MarkOutgroup(n.des2);
        end;
    end;

begin
    if outgroup then
        for i := 1 to NoOfOTUs do
            FNode[i].flag := FNode[i].outgroup
    else begin
        for i := 1 to NoOfOTUs do
            FNode[i].flag := false;
        MarkOutgroup(FRoot.des2);
    end;
end;

procedure TSvgTreeBox.SetStats;
var i : integer;
begin
  if MaxStats > 0.0 then
    for i := 1 to NoOfNodes do
    begin
      if FNode[i] = FRoot then Continue;
      if FNode[i].branch.stats < -0.0000000000001 then
        FNode[i].branch.stat2 := -1
       else
        FNode[i].branch.stat2 := Trunc(FNode[i].branch.stats*100/MaxStats+0.000000000001);
    end
  else
    for i := 1 to NoOfNodes do
    begin
      if FNode[i] = FRoot then Continue;
      if FNode[i].branch.stats < -0.0000000000001 then
        FNode[i].branch.stat2 := -1
      else
        FNode[i].branch.stat2 := Trunc(FNode[i].branch.stats+0.000000000001);
    end;
end;

function TSvgTreeBox.GetDataInitialized(index: integer):boolean;
begin
  result := false;
  if FDataInitialized = nil then Exit;
  result := FDataInitialized[index];
end;

procedure TSvgTreeBox.SetTopologyOnly(value : boolean);
begin
  inherited;

  if FDataInitialized = nil then Exit;
  if (not value) and (@BLenFunc <> nil) then
  begin
    SetTreeIndex(TreeIndex);
    if FScale = 0.0 then
      InitScale;
    Refresh;
  end;
end;

procedure TSvgTreeBox.InitTreeData(index : integer);
var
  tree, tmptree : TTimeTreeData;
begin
  if index = 0 then
  begin
    Assert(False, 'not implemented');
  end
  else
  begin
    tree := TreeList[index-1];
  end;
  FDataInitialized[index] := true;
end;

procedure TSvgTreeBox.SetTreeIndex(index : integer);
var p : TpNode;
begin
    if (index < 0) or (index > NoOfTrees) then Exit;

    if NodeFocused and BranchFocused then
      ClearFocus;

    SaveTreeData(TreeIndex);

    if not FDataInitialized[index] then
      InitTreeData(index);

    if (TreeIndex = 0) and (index <> 0) then
    begin
      IsCondensed      := ConsCondensed;
      ShowTopologyOnly := ConsTopoOnly;
      FIsRooted        := ConsRooted;
      FCondenseValue   := ConsCondenseValue;
    end
    else if (TreeIndex <> 0) and (index = 0) then
    begin
      ConsCondensed := IsCondensed;
      ConsTopoOnly  := ShowTopologyOnly;
      ConsRooted    := FIsRooted;
      FIsCondensed  := true;
      FTopologyOnly := true;
      FIsRooted     := false;
      ConsCondenseValue := FCondenseValue;
      FCondenseValue    := ConsensusValue;
    end;

    LoadTreeData(index);
    FTreeIndex := index;

    if not Initialized[index] then
      if isOutgroup then
        SetOutgroups(true)
      else
        SetOutgroups(false);

    SetStats;
    if isBranchLength then SetMaxLength(FRoot);
    SetClusterSize(FRoot);
    SetClusterWidth(FRoot);
    SetAttribList;
    SetAttrindex;
    if not Initialized[index] then
    begin
      if isOutgroup and (not isRooted) then
      begin
        p := SearchCommonAncestor(FRoot);
        if p.anc <> FRoot then
        begin
          MoveRoot(p, true);
          SetClusterSize(FRoot);
          SetClusterWidth(FRoot);
        end;
      end
      else if index = 0 then
      begin
        if not isRooted then
        begin
          if isBranchLength then
              MoveRoot(FNode[SearchMidPoint], true)
          else
              MoveRoot(SearchCommonAncestor(FRoot), false);
          SetClusterSize(FRoot);
          SetClusterWidth(FRoot);
        end;
      end
      else if not isBranchFixed then
      begin
        if (not isRooted) and isBranchLength then
        begin
          MoveRoot(FNode[SearchMidPoint], true);
          SetClusterSize(FRoot);
          SetClusterWidth(FRoot);
        end;
//        SortBranchByFigure(FNode^[NoOfNodes]);
      end;
      SetAttribList;
      SetAttrindex;
    end;
    SetClusterHeight;

    Initialized[index] :=  FDataInitialized[index];
end;

function TSvgTreeBox.GetStatsName:AnsiString;
begin
    if TreeIndex = 0 then
        Result := 'Consensus value'
    else
        Result := FStatsName;
end;

function TSvgTreeBox.GetCondenseValue: integer;
begin
    if TreeIndex = 0 then
        Result := ConsCondenseValue
    else
        Result := FCondenseValue;
end;

procedure TSvgTreeBox.SetCondenseValue(value : integer);
begin
    if value = CondenseValue then Exit;
    if value < 0 then
        value := 0
    else if value > 100 then
        value := 100;
    if TreeIndex = 0 then
        ConsCondenseValue := value
    else
        FCondenseValue := value;
end;

procedure TSvgTreeBox.SetConsensusValue(value : integer);
begin
    if value = ConsensusValue then Exit;
    if value < 0 then
        FConsensusValue :=  0
    else if value > 100 then
        FConsensusValue :=  100
    else
        FConsensusValue :=  value;
    if TreeIndex = 0 then
        FCondenseValue := FConsensusValue;
end;

procedure TSvgTreeBox.InitTree;
begin
    if (NoOfTrees = 0) and (Source = 'PartitionList') then
    begin
      FTreeIndex := 0;
      ConsCondensed := IsCondensed;
      ConsTopoOnly  := ShowTopologyOnly;
      ConsRooted    := FIsRooted;
      FIsCondensed  := true;
      FTopologyOnly := true;
      FIsRooted     := false;
      ConsCondenseValue := FCondenseValue;
      FCondenseValue    := ConsensusValue;
    end
    else
      FTreeIndex := 1;

    InitTreeData(TreeIndex);
    LoadTreeData(TreeIndex);
    Initialized[TreeIndex] := FDataInitialized[TreeIndex];
    FConsensusValue := 50;
    ConsCondenseValue := FCondenseValue;
    inherited InitTree;
end;

procedure TSvgTreeBox.SetValue(index: integer; value: double);
var
  tree : TTimeTreeData;
begin
  if (index < 0) or (index > NoOfTrees) then Exit;
  if index = 0 then
    Assert(False, 'not implemented');
  tree.Value := value;
end;

procedure TSvgTreeBox.SetValue2(index: integer; value: double);
var
  tree : TTimeTreeData;
begin
  if (index < 0) or (index > NoOfTrees) then Exit;
  if index = 0 then
    Assert(False, 'not implemented');
  tree.Value2 := value;
end;

function TSvgTreeBox.GetValue(index:integer):double;
var tree : TTimeTreeData;
begin
    Result := 0.0;
    if (index < 0) or (index > NoOfTrees) then Exit;
    if not FDataInitialized[index] then InitTreeData(index);
    if index = 0 then
      Assert(False, 'not implemented');
    Result := tree.Value;
end;

function TSvgTreeBox.GetValue2(index:integer):double;
var tree : TTimeTreeData;
begin
    Result := 0.0;
    if (index < 0) or (index > NoOfTrees) then Exit;
    if not FDataInitialized[index] then InitTreeData(index);
    if index = 0 then
      Assert(False, 'not implemented');
    Result := tree.Value2;
end;


function TSvgTreeBox.GetFrequency(index:integer):double;
var tree : TTimeTreeData;
begin
    Result := 0.0;
    if (index < 0) or (index > NoOfTrees) then Exit;
    if not FDataInitialized[index] then InitTreeData(index);
    if index = 0 then
      Assert(False, 'not implemented');
    Result := tree.Freq;
end;

procedure TSvgTreeBox.InitMem;
var i : integer;
begin
    inherited;
    GetMem(AncState, SizeOf(PString)*NoOfNodes);
    for i := 1 to NoOfNodes do
        AncState[i-1] := Addr(FNode[i].charstate);
    GetMem(FDataInitialized, SizeOf(Boolean)*(NoOfTrees+1));
    GetMem(Initialized, SizeOf(Boolean)*(NoOfTrees+1));
    GetMem(AttribListArray, SizeOf(TNodeAttribList)*(NoOfTrees+1));
    for i := 0 to NoOfTrees do
    begin
      FDataInitialized[i] := false;
      Initialized[i] := false;
      AttribListArray[i] := nil;
    end;
end;

function TSvgTreeBox.GetIsSE: boolean;
begin

end;

function TSvgTreeBox.GetIsStats: boolean;
begin

end;

procedure TSvgTreeBox.ResetMem;
var
  i: integer;
begin
    if ConsTree <> nil then
      ConsTree.Free;
    ConsTree := nil;
    if Assigned(TreeList) then
      FreeAndNil(TreeList);

    if FDistanceMatrix <> nil then
    begin
      FreeDistMatrix(FDistanceMatrix, NoOfOTUs);
      FDistanceMatrix := nil;
    end;

    FreeMemAndNil(AncState);
    FreeMemAndNil(FDataInitialized);
    FreeMemAndNil(Initialized);

    if AttribListArray <> nil then
    begin
      if AttribList.Count > 1 then
        for i := AttribList.Count-1 downto 1 do
          AttribList.Delete(i);
      for i := 0 to NoOfTrees do
        if AttribListArray[i] <> nil then
        begin
          while AttribListArray[i].Count > 0 do
          begin
            AttribListArray[i][0].Free;
            AttribListArray[i].Delete(0);
          end;
          FreeAndNil(AttribListArray[i]);
        end;
      FreeMemAndNil(AttribListArray);
    end;

    FNoOfTrees := 0;
    FSiteIndex := 0;
    FMaxSiteIndex := 0;

    inherited ResetMem;
end;


function TSvgTreeBox.SeTTimeTreeList(Tree: TTimeTreeList; optimize: boolean):boolean;
var i : integer;
begin
    result := false;
    if Tree.NoOfTrees = 0 then Exit;

    if NoOfTrees > 0 then ResetMem;
    FSource := 'TreeList';
    FisSE := Tree.isSE;
    FisStats := Tree.isStats;
    FisRooted := Tree.isRooted;
    FisBranchFixed := not optimize;
    if Tree.ValueName = '' then
        FisValue := false
    else begin
        FisValue := true;
        FValueName := Tree.ValueName;
    end;
    if Tree.Value2Name = '' then
        FisValue2 := false
    else begin
        FisValue2 := true;
        FValue2Name := Tree.Value2Name;
    end;
    FFreqName := Tree.FreqName;
    if FFreqName <> '' then begin
        FFreqDecimals := 0;
        for i := 0 to Tree.NoOfTrees-1 do
            if Frac(Tree[i].Freq) <> 0.0 then begin
                FFreqDecimals := 8;
                Break;
            end;
    end;
    FStatsName := Tree.StatsName;
    FMaxStats := Tree.MaxStats;
    FNoOfTrees := Tree.NoOfTrees;
    FNoOfOTUs := Tree.NoOfOTUs;

    TreeList := Tree;
    if @BLenFunc <> nil then
    begin
        TreeList.isBLen := true;
{}      FTopologyOnly := true;
    end;
    if @SEFunc <> nil then
        TreeList.isSE := true;
    if @StatsFunc <> nil then
        TreeList.isStats := true;

    if (Tree.NoOfTrees > 1) and (Tree.TotalFrequency > 0.0) then
        ConsTree := TTimeTreeData.Create(NoOfOTUs, false, false, true);

    try
      InitMem;

      for i := 1 to NoOfOTUs do begin
          FNode[i].name := Tree.OTUname[i-1];
          FNode[i].oriname := FNode[i].name;
          FNode[i].SpeciesName := Tree.SpeciesName[i - 1];
          FNode[i].PrivateName := '';
          FNode[i].outgroup := Tree.Items[0].IsOutgroupMember[i - 1];
      end;

      if FisValue then
        if @ValueFunc = nil then begin
          FValueDecimals := 0;
          for i := 0 to NoOfTrees-1 do
              if Frac(TreeList[i].Value) <> 0.0 then begin
                  FValueDecimals := 8;
                  Break;
              end;
        end
        else
          FValueDecimals := 8;

      if FisValue2 then
        if @Value2Func = nil then begin
          FValue2Decimals := 0;
          for i := 0 to NoOfTrees-1 do
              if Frac(TreeList[i].Value2) <> 0.0 then begin
                  FValue2Decimals := 8;
                  Break;
              end;
        end
        else
          FValue2Decimals := 8;

      if Tree.DistanceMatrix <> nil then
          FDistanceMatrix := Tree.DistanceMatrix;

      InitTree;
      Result := true;
    except
      ResetMem;
      Result := false;
    end;
end;

procedure TSvgTreeBox.SeTTimeTreeListRooted(const AValue: Boolean);
begin
  if TreeList.isRooted = AValue then
    Exit;
  TreeList.IsRooted := AValue;
end;

function TSvgTreeBox.GetIsValue:boolean;
begin
  if @FValueFunc <> nil then
    Result := true
  else if TreeIndex = 0 then
    Result := ConsTreeIsValue
  else
    Result := FisValue;
end;

function TSvgTreeBox.GetIsValue2:boolean;
begin
  if @FValue2Func <> nil then
    Result := true
  else if TreeIndex = 0 then
    Result := ConsTreeIsValue2
  else
    Result := FisValue2;
end;

function TSvgTreeBox.GetIsFreq:boolean;
begin
  if (NoOfTrees <= 1) or (FreqName = '') then
      Result := false
  else if TreeList.TotalFrequency = 0.0 then
      Result := false
  else
      Result := true;
end;

procedure TSvgTreeBox.SetValueFunc(f: TSvgBranchInfoFunc);
begin
    if @f = @FValueFunc then Exit;
    FValueFunc := f;
end;


procedure TSvgTreeBox.SetValue2Func(f: TSvgBranchInfoFunc);
begin
    if @f = @FValue2Func then Exit;
    FValue2Func := f;
end;

procedure TSvgTreeBox.AssignTreeList(ATreeList: TTimeTreeList);
begin
  ATreeList.Assign(TreeList);
end;

procedure TSvgTreeBox.SetMaxSiteIndex(value: integer);
begin
    if value = FMaxSiteIndex then Exit;
    FMaxSiteIndex := value;
    if FMaxSiteIndex < FSiteIndex then
        FSiteIndex := FMaxSiteIndex;
end;

procedure TSvgTreeBox.GetSubtree(subtree: TSvgTreeBox);
var tree, oritree: TTimeTreeData;
    TL : TTimeTreeList;
    h, k: PArrayOfInt;
    n, i, j : integer;
    AIndex: Integer;

    procedure GeTSvgNodeInfo(node: TpNode);
    begin
      if node.OTU then begin
        TL.OTUName[i] := node.Name;
        if isBranchLength then
          tree.BLenArray[i] := oritree.BLen[node.index-1];
        if isSE then
          tree.SEArray[i] := oritree.SE[node.index-1];
          tree.StatsArray[i] := oritree.Stats[node.index-1];
        h[node.index] := i;
        Inc(i);
      end
      else begin
        GeTSvgNodeInfo(node.des1);
        GeTSvgNodeInfo(node.des2);
        tree.NodeArray[j].des1 := h[node.des1.index];
        tree.NodeArray[j].des2 := h[node.des2.index];
        if isBranchLength then
          tree.BLenArray[n+j] := oritree.BLen[node.index-1];
        if isSE then
          tree.SEArray[n+j] := oritree.SE[node.index-1];
          tree.StatsArray[n+j] := oritree.Stats[node.index-1];
        h[node.index] := n+j;
        Inc(j);
      end;
    end;

    procedure SetSubtreeNodeAttrib(p: TpNode);
    var a: TNodeAttrib;
    begin
      if (p.attrindex <> p.anc.attrindex) and (p.attrindex > 0) then
      begin
        a := TNodeAttrib.Create;
        a.Assign(AttribList[p.attrindex]);
        subtree.SetSubtreeAttrib(a, h[p.index]+1);
        a.Free;
      end;

      if p.OTU then
        subtree.FNode[h[p.index]+1].marker := p.marker
      else
      begin
        SetSubtreeNodeAttrib(p.des1);
        SetSubtreeNodeAttrib(p.des2);
      end;
    end;

    procedure SetSubtreeGroupInfo;
    var
      sl: TStringList;
      i,j: integer;
    begin
      sl := TStringList.Create;
      for i := 1 to NoOfOTUs do
      begin
        if FNode[i].groupindex > -1 then
          for j := 1 to subtree.NoOfOTUs do
            if subtree.OTUName[j] = OTUName[i] then
            begin
              sl.Add(OTUName[i]+'='+GroupAttrib[FNode[i].groupindex].Name);
              break;
            end;
      end;
      if sl.Count > 0 then
      begin
        subtree.SetGroupInfo(sl);
        for i := 0 to subtree.GroupAttrib.Count-1 do
          for j := 0 to GroupAttrib.Count-1 do
            if subtree.GroupAttrib[i].Name = GroupAttrib[j].Name then
            begin
              subtree.GroupAttrib[i].Assign(GroupAttrib[j]);
              break;
            end;
      end;
      sl.Free;
    end;

begin
    if (FocusedIndex = 0) or (FocusedIndex = FRoot.index) then exit;
    GetMem(h, SizeOf(Integer)*(NoOfNodes+1));
    for i := 0 to NoOfNodes do
      h[i] := NoOfNodes;

    TL := TTimeTreeList.Create;

    TL.isRooted := true;
    TL.ValueName := '';
    TL.Value2Name := '';
    TL.FreqName := '';
    TL.StatsName := StatsName;
    TL.MaxStats := MaxStats;

    SaveTreeData(TreeIndex);
    if TreeIndex = 0 then
      oritree := ConsTree
    else
      oritree := TreeList[TreeIndex-1];
    tree := TTimeTreeData.Create(FNode[FocusedIndex].size, isBranchLength, isSE, true);

    i := 0;
    j := 0;
    n := tree.NoOfOTUs;
    GeTSvgNodeInfo(FNode[FocusedIndex].des1);
    GeTSvgNodeInfo(FNode[FocusedIndex].des2);
    tree.NodeArray[j].des1 := h[FNode[FocusedIndex].des1.index];
    tree.NodeArray[j].des2 := h[FNode[FocusedIndex].des2.index];
    TL.Add(tree);

    if isDistanceMatrix then begin
      TL.DistanceMatrix := NewDistMatrix(n, true);
      GetMem(k, SizeOf(Integer)*n);
      for i := 1 to NoOfOTUs do
        if h[i] < n then
          k[h[i]] := i-1;
      for i := 1 to n-1 do begin
        for j := 0 to i-1 do begin
          TL.DistanceMatrix[i][j] := DistanceMatrix[k[i]][k[j]];
          TL.DistanceMatrix[j][i] := TL.DistanceMatrix[i][j];
        end;
      end;
      FreeMemAndNil(k);
    end
    else
      TL.DistanceMatrix := nil;

    subtree.SeTTimeTreeList(TL, false);
    subtree.SortClusterInOrder;

    SetSubtreeGroupInfo;

    subtree.AssignTreeAttrib(Self);
    subtree.AttribList[0].Assign(AttribList[FNode[FocusedIndex].Attrindex]);

    SetSubtreeNodeAttrib(FNode[FocusedIndex].des1);
    SetSubtreeNodeAttrib(FNode[FocusedIndex].des2);

    subtree.SetAttrIndex;

    FreeMemAndNil(h);
end;

function TSvgTreeBox.GetIsBranchLength:boolean;
begin
  Result := TreeList.isBLen;
end;

function TSvgTreeBox.GetMaxStats:double;
begin
  Result := FMaxStats;
end;

function  TSvgTreeBox.ImportFromNewickStandard(filename : String; Unroot: Boolean=False):boolean;
var
  trees: TTimeTreeList;
begin
  trees := TTimeTreeList.Create;
  try
    result := trees.ImportFromNewickFile(filename, nil);
    if Unroot and trees.isRooted then
      trees.isRooted := False;

    if result then
    begin
      SeTTimeTreeList(trees, false);
      trees := nil;
    end;

    FSource := 'File: '+filename;
  finally
    if trees <> nil then
      trees.Free;
  end;
end;

function  TSvgTreeBox.ImportFromNewickString(NewickTree : AnsiString):boolean;
var
  trees: TTimeTreeList;
begin
  trees := TTimeTreeList.Create;
  try
    result := trees.ImportFromNewick(NewickTree, nil);
    if result then
    begin
      SeTTimeTreeList(trees, false);
      trees := nil;
    end;

    FSource := 'Tree: '+NewickTree;
  finally
    if trees <> nil then
      trees.Free;
  end;
end;

function TSvgTreeBox.GetInformation:TStringList;
begin
  Result := TreeList.Information;
end;

procedure TSvgTreeBox.GetInternalNodeLabels(var NodeLabels: TStringList);
var
  i: Integer;
begin
  NodeLabels.Clear;
  for i := 0 to TreeList.InternalNodeLbls.Count - 1 do
    NodeLabels.Add(TreeList.InternalNodeLbl[i]);
end;

function TSvgTreeBox.GetNewickTree: AnsiString;
begin
  SaveTreeData(TreeIndex);
  if ShowTopologyOnly and isCondensed then
    result := TreeList.OutputNewickTree(TreeIndex-1, false, TreeList.isStats, CondenseValue/100*MaxStats)
  else
    result := TreeList.OutputNewickTree(TreeIndex-1, TreeList.isBLen, TreeList.isStats, 0);
end;

function TSvgTreeBox.GetOTUNamesList: TStringlist;
begin
  Result := TStringList.Create;
  Result.AddStrings(TreeList.OTUNameList);
end;

function TSvgTreeBox.GetOutgroupInfo: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 1 to NoOfOTUs do
  begin
    if FNode[i].groupindex >= 0 then
    begin
      Result.Add(FNode[i].name + '=' + GroupAttrib[FNode[i].groupindex].Name);
    end;
  end;
end;

procedure TSvgTreeBox.GetOutgroupTaxa(var AList: TStringList);
var
  i: Integer;
begin
  for i := 1 to NoOfOtus do
    if FNode[i].outgroup then
      AList.Add(FNode[i].name);
end;

procedure TSvgTreeBox.ExportCurrentTreeToNewickFile(filename : String);
var
  tempTreeList: TTimeTreeList;
  treedata: TTimeTreeData;
  i: integer;
begin
  SaveTreeData(TreeIndex);

  if TreeIndex = 0 then
  begin
    tempTreeList := TTimeTreeList.Create;
    tempTreeList.MaxStats := MaxStats;
    for i := 0 to NoOfOTUs-1 do
      tempTreeList.OTUName[i] := OTUName[i+1];
    treedata := TTimeTreeData.Create(NoOfOTUs, (not ShowTopologyOnly), false, ShowStats);
    treedata.Assign(ConsTree);
    tempTreeList.Add(treedata);
    if IsCondensed then
      tempTreeList.ExportATreeToNewickFile(0, filename, (not ShowTopologyOnly), ShowStats, ConsensusValue/100*MaxStats)
    else
      tempTreeList.ExportATreeToNewickFile(0, filename, (not ShowTopologyOnly), ShowStats, 0);
    tempTreeList.Delete(0);
    tempTreeList.Free;
  end
  else if ShowTopologyOnly and isCondensed then
    TreeList.ExportATreeToNewickFile(TreeIndex-1, filename, (not ShowTopologyOnly), ShowStats, CondenseValue/100*MaxStats)
  else
    TreeList.ExportATreeToNewickFile(TreeIndex-1, filename, (not ShowTopologyOnly), ShowStats, 0);
end;

function TSvgTreeBox.GetCurrentTree: TTimeTreeData;
var
  treedata: TTimeTreeData;
begin
  SaveTreeData(TreeIndex);

  if TreeIndex = 0 then
  begin
    treedata := TTimeTreeData.Create(NoOfOTUs, (not ShowTopologyOnly), false, ShowStats);
    treedata.Assign(ConsTree);
    result := treedata;
  end
  else
    result := TreeList[TreeIndex-1];
end;

procedure TSvgTreeBox.ExportAllTreesToNewickFile(filename : String);
begin
  SaveTreeData(TreeIndex);
  if ShowTopologyOnly and isCondensed then
    TreeList.ExportToNewickFile(filename, (not ShowTopologyOnly), ShowStats, CondenseValue/100*MaxStats)
  else
    TreeList.ExportToNewickFile(filename, (not ShowTopologyOnly), ShowStats, 0);
end;

procedure TSvgTreeBox.SetTaxonMarkerOfSubtree(nodeindex: integer; marker: TNodeMarker);
var
  flag: boolean;

  procedure SetMarkerOnTaxon(p: TpNode);
  begin
    if (p <> FRoot) and (p.attrindex > 0) then
    begin
      AttribList[p.attrindex].ShowTaxonMarker := flag;
      AttribListArray[TreeIndex][p.attrindex-1].ShowTaxonMarker := flag;
    end;
    if p.OTU then
      p.marker := marker
    else
    begin
      SetMarkerOnTaxon(p.des1);
      SetMarkerOnTaxon(p.des2);
    end;
  end;

begin
  if (nodeindex <= 0) or (nodeindex > NoOfNodes) then
    exit;

  flag := marker.Shape <> msNone;
  if FNode[nodeindex].attrindex >= 0 then
    flag := AttribList[FNode[nodeindex].attrindex].ShowTaxonMarker;

  SetMarkerOnTaxon(FNode[nodeindex]);
end;

function TSvgTreeBox.GetSubtreeAttrib(NodeAttrib: TNodeAttrib; nodeindex: integer): boolean;
var i: integer;
begin
  if nodeindex <= 0 then
    i := 0
  else if nodeindex > NoOfNodes then
    i := 0
  else if FNode[nodeindex] = FRoot then
    i := 0
  else
    i := FNode[nodeindex].attrindex;
  if i < 0 then
    NodeAttrib.Assign(GroupAttrib[-(i+1)])
  else
    NodeAttrib.Assign(AttribList[i]);
  if FNode[nodeindex] = FRoot then
    result := true
  else
    result := (FNode[nodeindex].attrindex >= 0) and (FNode[nodeindex].attrindex <> FNode[nodeindex].anc.attrindex);
end;

procedure TSvgTreeBox.SetSubtreeAttrib(NodeAttrib: TNodeAttrib; nodeindex: integer);

  procedure SetDescendant(p: TpNode);
  begin
    if (p.attrindex > 0) and (p.attrindex <> p.anc.attrindex) then
    begin
      AttribList[p.attrindex].ShowTaxonName   := NodeAttrib.ShowTaxonName;
      AttribList[p.attrindex].ShowSpeciesName := NodeAttrib.ShowSpeciesName;
      AttribList[p.attrindex].ShowTaxonMarker := NodeAttrib.ShowTaxonMarker;
      AttribListArray[TreeIndex][p.attrindex-1].ShowTaxonName   := NodeAttrib.ShowTaxonName;
      AttribListArray[TreeIndex][p.attrindex-1].ShowSpeciesName   := NodeAttrib.ShowSpeciesName;
      AttribListArray[TreeIndex][p.attrindex-1].ShowTaxonMarker := NodeAttrib.ShowTaxonMarker;
      AttribListArray[TreeIndex][p.attrindex-1].OverwriteDownstream := NodeAttrib.OverwriteDownstream;
      AttribListArray[TreeIndex][p.attrindex-1].OverwriteDownstream := NodeAttrib.OverwriteDownstream;
    end;

    if p.OTU then exit;

    SetDescendant(p.des1);
    SetDescendant(p.des2);
  end;

var
  i: integer;
begin
  if (nodeindex = 0) or (FNode[nodeindex] = FRoot) then
  begin
    AttribList[0].Assign(NodeAttrib);
    Exit;
  end;
  if (FNode[nodeindex].attrindex < 0) or (FNode[nodeindex].attrindex = FNode[nodeindex].anc.attrindex) then
  begin
    if AttribListArray[TreeIndex] = nil then
      AttribListArray[TreeIndex] := TNodeAttribList.Create;
    AttribListArray[TreeIndex].Add(TNodeAttrib.Create);
    i := AttribListArray[TreeIndex].Count-1;
  end
  else
    i := FNode[nodeindex].attrindex -1;

  NodeAttrib.NodeIndex := nodeindex;
  AttribListArray[TreeIndex][i].Assign(NodeAttrib);

  if not FNode[nodeindex].OTU then
  begin
    SetDescendant(FNode[nodeindex].des1);
    SetDescendant(FNode[nodeindex].des2);
  end;

  SetAttribList;
  SetAttrindex;
end;

procedure TSvgTreeBox.ClearSubtreeAttrib(nodeindex: integer; recursive: boolean);

  procedure ClearDescendant(node: TpNode);
  begin
    if not node.OTU then
    begin
      ClearDescendant(node.des1);
      ClearDescendant(node.des2);
    end;
    if (node.attrindex > 0) and (node.attrindex <> node.anc.attrindex) then
    begin
      DeleteAttrib(node.attrindex);
      SetAttrindex;
    end;
  end;

begin
  if (nodeindex < 1) or (nodeindex > NoOfNodes) then exit;
  if recursive and (not FNode[nodeindex].OTU) then
  begin
    ClearDescendant(FNode[nodeindex].des1);
    ClearDescendant(FNode[nodeindex].des2);
  end;

  if FNode[nodeindex] = FRoot then exit;

  if (FNode[nodeindex].attrindex > 0) and (FNode[nodeindex].attrindex <> FNode[nodeindex].anc.attrindex) then
  begin
    DeleteAttrib(FNode[nodeindex].attrindex);
    FNode[nodeindex].compressed := false;
    SetAttrindex;
  end;
end;



procedure TSvgTreeBox.OverwriteAttribDownstream(nodeindex: integer);
var
  a: TNodeAttrib;

  procedure Overwrite(node: TpNode);
  begin
    if not node.OTU then
    begin
      Overwrite(node.des1);
      Overwrite(node.des2);
    end;
    if (node.attrindex > 0) and (node.attrindex <> node.anc.attrindex) then
      AttribListArray[TreeIndex][node.attrindex-1].AssignGraphAttrib(a);
  end;

begin
  if (nodeindex < 1) or (nodeindex > NoOfNodes) then exit;
  if FNode[nodeindex].OTU then exit;

  a := TNodeAttrib.Create;
  a.AssignGraphAttrib(AttribList[FNode[nodeindex].attrindex]);

  Overwrite(FNode[nodeindex].des1);
  Overwrite(FNode[nodeindex].des2);

  a.Free;

  SetAttribList;
  SetAttrindex;
end;

procedure TSvgTreeBox.ClearAllSubtreeAttrib;
begin
  ClearSubtreeAttrib(FRoot.index, true);
end;

procedure TSvgTreeBox.ClearOutgroups;
var
  i: Integer;
begin
  for i := 1 to NoOfOTUs do
  begin
    FNode[i].outgroup := False;
    FNode[i].emphasized := False;
  end;
end;

procedure TSvgTreeBox.SetGroupInfo(groupinfo: TStringList);

var
  i,j,k: integer;
  flag : boolean;
begin
  if groupinfo = nil then exit;
  if groupinfo.Count = 0 then exit;

  if GroupAttrib.Count > 0 then
    for i := GroupAttrib.Count-1 downto 0 do
    begin
      GroupAttrib[i].Free;
      GroupAttrib.Delete(i);
    end;

  for j := 1 to NoOfOTUs do
  begin
    FNode[j].groupindex := -1;
  end;

  for i := 0 to groupInfo.Count - 1 do
    for j := 1 to NoOfOtus do
    begin
      if (groupinfo.Names[i] = FNode[j].oriName) and (groupinfo.Values[groupinfo.Names[i]] <> '') then
        if SameText('outgroup', groupinfo.Values[groupinfo.Names[i]]) then
          FNode[j].outgroup := True;
    end;

  for i := 0 to groupinfo.Count-1 do
    for j := 1 to NoOfOTUs do
      if (groupinfo.Names[i] = FNode[j].oriName) and (groupinfo.Values[groupinfo.Names[i]] <> '') then
        if GroupAttrib.Count = 0 then
        begin
          GroupAttrib.Add(TNodeAttrib.Create);
          GroupAttrib[0].Assign(AttribList[0]);
          GroupAttrib[0].Name := groupinfo.Values[FNode[j].oriName];
          GroupAttrib[0].Caption := GroupAttrib[0].Name;
          FNode[j].groupindex := 0;
        end
        else
        begin
          flag := true;
          for k := 0 to GroupAttrib.Count-1 do
            if groupinfo.Values[FNode[j].oriName] = GroupAttrib[k].Name then
            begin
              FNode[j].groupindex := k;
              flag := false;
              break;
            end;
          if flag then
          begin
            GroupAttrib.Add(TNodeAttrib.Create);
            GroupAttrib[GroupAttrib.Count-1].Assign(AttribList[0]);
            GroupAttrib[GroupAttrib.Count-1].Name := groupinfo.Values[FNode[j].oriName];
            GroupAttrib[GroupAttrib.Count-1].Caption := GroupAttrib[GroupAttrib.Count-1].Name;
            FNode[j].groupindex := GroupAttrib.Count-1;
          end;
          break;
        end;

  SetAttrindex;
end;

procedure TSvgTreeBox.GetGroupAttrib(Attrib: TNodeAttrib; GroupName: AnsiString);
var
  i: integer;
begin
  if GroupAttrib.Count = 0 then exit;
  for i := 0 to GroupAttrib.Count-1 do
    if GroupAttrib[i].Name = GroupName then
    begin
      Attrib.Assign(GroupAttrib[i]);
      break;
    end;
end;

procedure TSvgTreeBox.SetGroupAttrib(Attrib: TNodeAttrib; GroupName: AnsiString);
var
  i: integer;
begin
  if GroupAttrib.Count = 0 then exit;
  for i := 0 to GroupAttrib.Count-1 do
    if GroupAttrib[i].Name = GroupName then
    begin
      GroupAttrib[i].Assign(Attrib);
      GroupAttrib[i].Name := GroupName;
      break;
    end;
  SetAttrindex;
end;

procedure TSvgTreeBox.SetTaxonMarkerOfGroup(GroupName: AnsiString; marker: TNodeMarker);
var
  i,n: integer;
begin
  if GroupAttrib.Count = 0 then exit;
  n := -1;
  for i := 0 to GroupAttrib.Count-1 do
    if GroupAttrib[i].Name = GroupName then
    begin
      n := i;
      break;
    end;
  if n >= 0 then
    for i := 1 to NoOfOTUs do
      if FNode[i].groupindex = n then
        FNode[i].marker := marker;
end;

procedure TSvgTreeBox.SetAttribList;
var i: integer;
begin
  while AttribList.Count > 1 do
    AttribList.Delete(AttribList.Count-1);
  if Assigned(AttribListArray[TreeIndex]) then
    for i := 0 to AttribListArray[TreeIndex].Count-1 do
      AttribList.Add(AttribListArray[TreeIndex][i]);
end;

procedure TSvgTreeBox.SetShowOTUMarker(b : boolean);
var
  i: integer;
begin
  inherited;

  if Assigned(AttribListArray) and Assigned(AttribListArray[TreeIndex]) then
    for i := 0 to AttribListArray[TreeIndex].Count-1 do
      AttribListArray[TreeIndex][i].ShowTaxonMarker := b;
end;

procedure TSvgTreeBox.DeleteAttrib(index: integer);
begin
  if (index = 0) or (index >= AttribList.Count) then Exit;
  AttribList[index].Free;
  AttribList.delete(index);
  AttribListArray[TreeIndex].delete(index-1);
  if AttribListArray[TreeIndex].Count = 0 then
  begin
    AttribListArray[TreeIndex].Free;
    AttribListArray[TreeIndex] := nil;
  end;
end;

procedure TSvgTreeBox.MoveRoot(p: TpNode; midpoint: boolean);
var q: TpNode;
    i: integer;
begin
  if AttribList.Count > 1 then
    for i := AttribList.Count-1 downto 1 do begin
      q := p.anc;
      while q <> FRoot do begin
        if (q.attrindex = i) and (q.attrindex <> q.anc.attrindex) then begin
          DeleteAttrib(q.attrindex);
          q.name := '';
          Break;
        end;
        q := q.anc;
      end;
    end;
  SetAttrindex;
  inherited;
end;

constructor TSvgNodeInfo.Create(Source: TCustomSvgTree);
begin
  inherited Create;
  Tree := Source;
end;

destructor TSvgNodeInfo.Destroy;
begin
  inherited;
end;

function TSvgNodeInfo.GetIndex: integer;
begin
  Result := Tree.FocusedIndex;
end;

function TSvgNodeInfo.GetIsOtu: Boolean;
begin
  Result := Tree.FNode[Tree.FocusedIndex].OTU;
end;

function TSvgNodeInfo.GetIsOutgroup: Boolean;
begin
  Result := Tree.FNode[Tree.FocusedIndex].outgroup;
end;

function TSvgNodeInfo.GetAncIndex: integer;
begin
  with Tree do
    if FocusedIndex = FRoot.index then
      Result  := 0
    else
      Result  := FNode[FocusedIndex].anc.index;
end;


function TSvgNodeInfo.GetDes1Index: integer;
begin
  with Tree do
    if FNode[FocusedIndex].OTU then
      Result := 0
    else
      Result := FNode[FocusedIndex].des1.index;
end;

function TSvgNodeInfo.GetDes2Index: integer;
begin
  with Tree do
    if FNode[FocusedIndex].OTU then
      Result := 0
    else
      Result := FNode[FocusedIndex].des2.index;
end;

function TSvgNodeInfo.GetHeight: double;
begin
  with Tree do
    if FocusedIndex = 0 then
      Result := 0.0
    else
      Result := FNode[FocusedIndex].height;
end;

function TSvgNodeInfo.GetNodeType: TNodeType;
begin
  with Tree do
    if FocusedIndex = 0 then
      Result := ntNone
    else if FNode[FocusedIndex].anc = nil then
      Result := ntRoot
    else if FNode[FocusedIndex].compressed then
      Result := ntCompressed
    else if FNode[FocusedIndex].hidden then
      Result := ntHidden
    else
      Result := ntInterior;
end;

{ TSvgBranchInfo }

constructor TSvgBranchInfo.Create;
begin
  inherited Create;
  Tree := Source;
end;

destructor TSvgBranchInfo.Destroy;
begin
  inherited;
end;

function TSvgBranchInfo.GetNodeIndex: integer;
begin
  with Tree do
//    if BranchFocused then
      Result := FocusedIndex
//    else
//      Result := 0;
end;

function TSvgBranchInfo.GetAncNodeIndex: integer;
begin
  with Tree do
    if FocusedIndex = 0 then
      Result := 0
    else if FocusedIndex = FRoot.index then
      Result := 0
    else
      Result := FNode[FocusedIndex].anc.index;
end;

function TSvgBranchInfo.GetBranchType: TBranchType;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = FRoot.index) then
      Result := btNone
    else if FNode[FocusedIndex].Anc = FRoot then
      if FRoot.des1.OTU or FRoot.des2.OTU then
        Result := btRootedExterior
      else
        Result := btRootedInterior
    else if FNode[FocusedIndex].OTU then
      Result := btExterior
    else
      Result := btInterior;
end;

function TSvgBranchInfo.GetLength: double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = FRoot.index) then
      Result := 0.0
    else if (IsLinearized or ForceLinearized) then
      Result := FNode[FocusedIndex].anc.height-FNode[FocusedIndex].height
    else
      Result := FNode[FocusedIndex].branch.length;
end;

function TSvgBranchInfo.GetTotalLength : double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = FRoot.index) then
      Result := 0.0
    else if (FNode[FocusedIndex].anc = FRoot) and (not isRooted) then
      Result := FRoot.des1.branch.length + FRoot.des2.branch.length
    else
      Result := FNode[FocusedIndex].branch.length;
end;

function TSvgBranchInfo.GetMaxLen1: double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = FRoot.index) then
      Result := 0.0
    else
      Result := FNode[FocusedIndex].branch.maxlen1;
end;

function TSvgBranchInfo.GetMaxLen2: double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = FRoot.index) then
      Result := 0.0
    else
      Result := FNode[FocusedIndex].branch.maxlen2;
end;

function TSvgBranchInfo.GetSE: double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = FRoot.index) then
      Result := 0.0
    else
      Result := Tree.FNode[FocusedIndex].branch.SE;
end;

function TSvgBranchInfo.GetStats: double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = FRoot.index) then
      Result := 0.0
    else
      Result := FNode[FocusedIndex].branch.stats;
end;

procedure TSvgTreeBox.GetTreeData(tree: TTimeTreeData);
begin
  SaveTreeData(TreeIndex);
  inherited GetTreeData(tree);
end;

function TSvgTreeBox.LargestWidthOfOTUNames: Integer;
var
  W, i, TempW: integer;
begin
  for i:=1 to NoOfOTUs do
  begin
    TempW := CustomTextWidth(' ' + OTUName[i]);
    if TempW > W then
      W := TempW;
  end;
  Result := W;
end;

function TCustomSvgTree.GetAncestorName(i: integer): AnsiString;
begin
  Result := GetOTUName(FNode[i].anc.index);
  if Result = EmptyStr then  // When there is no name, return the direct decendant numbers as a method of identifying the FNode.
    Result := GetCoords(FNode[i].anc.index);
end;


function TCustomSvgTree.GetAncestorNodeNumber(i: integer): integer;
begin
  Result  := -1;
  if (i <> 0) and (FNode[i] <> nil) and (FNode[i].anc <> nil) then
    result := FNode[i].anc.index;
end;

function TCustomSvgTree.GetNumCalibrations: Integer;
begin

end;

function TCustomSvgTree.HasCalibrations: Boolean;
begin

end;

function TCustomSvgTree.FindAncestorName(aNode: TpNode): String;
var
  tempNode: TpNode;
  idStr: String;
begin
  Result := EmptyStr;
  if not Assigned(aNode.anc) then
    Exit;
  tempNode := aNode;
  while Assigned(tempNode.anc) do
  begin
    tempNode := tempNode.anc;
    idStr := IntToStr(tempNode.timetreeId);
    if NamesMap.Contains(idStr) then
    begin
      Result := TOtuName(NamesMap[idStr]).name;
      break;
    end;
  end;
end;

end.
