unit UDeviceData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTypes, DOM, XMLWrite, XMLRead, lazUTF8;

const
  VistTypFirstId = 100;
  VistTypSId = VistTypFirstId;
  VistTypVId = 101;
  VistTypAId = 102;
  VistTypeLastId = VistTypAId;

  VistObjGeneralId     = 100;
  VistObjShakerTableId = 101;
  RootNodeName: DOMString          = 'INCVIST-3.0';
  DescriptionNodeName: DOMString   = 'DESCRIPTION';
  IncNodeName: DOMString           = 'INC';
  VistNodeName: DOMString          = 'VIST';
  ItemNodeName: DOMString          = 'ITEM';
  DateFieldName: DOMString         = 'DATE';
  TimeFieldName: DOMString         = 'TIME';
  NumberFieldName: DOMString       = 'NUMBER';
  NoiseFieldName: DOMString        = 'NOISE';
  MeasureTFieldName: DOMString     = 'MEASURE_T';
  CommentFieldName: DOMString      = 'COMMENTS';
  DiameterFieldName: DOMString     = 'DIAMETER';
  LengthFieldName: DOMString       = 'LENGTH';
  SigmaFieldName: DOMString        = 'SIGMA';
  EpsilonFieldName: DOMString      = 'EPSILON';
  DeltaLFieldName: DOMString       = 'DELTA_L';
  ObjFieldName: DOMString          = 'OBJECT';
  TypFieldName: DOMString          = 'TYPE';
  MeasureSppFieldName: DOMString   = 'MEASURE_S_PP';
  MeasureVrmsFieldName: DOMString  = 'MEASURE_V_RMS';
  MeasureAampFieldName: DOMString  = 'MEASURE_A_AMP';

type
  TDevDataType = (ddtNone, ddtDescription, ddtInc, ddtVist);

  { TCustomItem }
  TCustomItem = class
  private
    FComments: TWString;
    FDate: TWDate;
    FTime: TWTime;
    FNumber: TWInt;
    FMeasureT: TWFloat32;
    FMeasureF: TWFloat32;
    FNoise: TWFloat32;
    function GetMeasureF: TWFloat32;
  protected
    class function Compare(Item1, Item2: TCustomItem): Integer;
    procedure SaveFields(Doc: TDOMDocument; Root: TDOMElement); virtual;
    function LoadField(FieldName: DOMString; Txt: UTF8String): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Compare(Item: TCustomItem): Integer; virtual;
    procedure Save(Root: TDOMElement);
    procedure Load(Root: TDOMNode);
    property Comments: TWString read FComments write FComments;
    property Date: TWDate read FDate write FDate;
    property Time: TWTime read FTime write FTime;
    property Number: TWInt read FNumber write FNumber;
    property MeasureT: TWFloat32 read FMeasureT write FMeasureT;
    property MeasureF: TWFloat32 read GetMeasureF;
    property Noise: TWFloat32 read FNoise write FNoise;
  end;

  { TIncItem }
  TIncItem = class(TCustomItem)
  private
    FDiameter: TWInt;
    FLen: TWFloat32;
    FSigma: TWFloat32;
    FEpsilon: TWFloat32;
    FDeltaL: TWFloat32;
  protected
    procedure SaveFields(Doc: TDOMDocument; Root: TDOMElement); override;
    function LoadField(FieldName: DOMString; Txt: UTF8String): Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Diameter: TWInt read FDiameter write FDiameter;
    property Len: TWFloat32 read FLen write FLen;
    property Sigma: TWFloat32 read FSigma write FSigma;
    property Epsilon: TWFloat32 read FEpsilon write FEpsilon;
    property DeltaL: TWFloat32 read FDeltaL write FDeltaL;
  end;

  { TVistItem }
  TVistItem = class(TCustomItem)
  private
    FObj: TWInt;
    FTyp: TWInt;
    FMeasureSpp:  TWFloat32;
    FMeasureVrms: TWFloat32;
    FMeasureAamp: TWFloat32;
  protected
    procedure SaveFields(Doc: TDOMDocument; Root: TDOMElement); override;
    function LoadField(FieldName: DOMString; Txt: UTF8String): Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Obj: TWInt read FObj write FObj;
    property Typ: TWInt read FTyp write FTyp;
    property MeasureSpp:  TWFloat32 read FMeasureSpp  write FMeasureSpp;
    property MeasureVrms: TWFloat32 read FMeasureVrms write FMeasureVrms;
    property MeasureAamp: TWFloat32 read FMeasureAamp write FMeasureAamp;
  end;

  { TBase }
  TBase = class
  private
    FAutoDelete: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Save(Root: TDOMElement); virtual; abstract;
    procedure Load(Root: TDOMNode); virtual; abstract;
    procedure FreeMembers; virtual;
    property AutoDelete: Boolean read FAutoDelete write FAutoDelete;
  end;

  { TDescription }
  TDescription = class(TBase)
  private
    FText: TWString;
  public
    constructor Create;
    procedure FreeMembers; override;
    procedure Clear;
    procedure Save(Root: TDOMElement); override;
    procedure Load(Root: TDOMNode); override;
    property Text: TWString read FText write FText;
  end;

  { TCustomData }
  TCustomData = class(TBase)
  private
    FList: TList;
    FSorted: Boolean;
    function GetCount: Integer;
  protected
    function GetCustomItem(Index: Integer): TCustomItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FreeMembers; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Delete(Item: TCustomItem);
    procedure Add(NewItem: TCustomItem);
    procedure Sort; virtual;
    function IndexOf(Item: TCustomItem): Integer;
    function Find(Dt: TDate; Tm: TTime): Integer;
    property Count: Integer read GetCount;
    property Sorted: Boolean read FSorted;
  end;

  { TIncData }
  TIncData = class(TCustomData)
  private
    function GetItem(Index: Integer): TIncItem;
  public
    procedure Add(NewItem: TIncItem);
    procedure Save(Root: TDOMElement); override;
    procedure Load(Root: TDOMNode); override;
    property Items[Index: Integer]: TIncItem read GetItem;
  end;

  { TVistData }
  TVistData = class(TCustomData)
  private
    function GetItem(Index: Integer): TVistItem;
  public
    procedure Add(NewItem: TVistItem);
    procedure Save(Root: TDOMElement); override;
    procedure Load(Root: TDOMNode); override;
    property Items[Index: Integer]: TVistItem read GetItem;
  end;

  { TDeviceData }
  TDeviceData = class
  private
    FDescription: TDescription;
    FIncData: TIncData;
    FVistData: TVistData;
  protected
    procedure LoadImpl(Root: TDOMNode);
  public
    constructor Create;
    destructor Destroy; override;
    function Save(FileName: UTF8String): Boolean;
    function Load(FileName: UTF8String): Boolean; virtual;
    procedure Sort;
    procedure Clear;
    property Description: TDescription read FDescription;
    property IncData: TIncData read FIncData;
    property VistData: TVistData read FVistData;
  end;

  { TDeviceQuery }
  TDeviceQuery = class(TDeviceData)
  private
  public
    constructor Create;
  end;

  function DateToStr(Item: TCustomItem; DefFS: Boolean = True): String;
  function TimeToStr(Item: TCustomItem; DefFS: Boolean = True): String;
  function NumberToStr(Item: TCustomItem; DefFS: Boolean = True): String;
  function DiameterToStr(Item: TIncItem; DefFS: Boolean = True): String;
  function LengthToStr(Item: TIncItem; DefFS: Boolean = True): String;
  function MeasureTtoStr(Value: TWFloat32; DefFS: Boolean = True): String;
  function MeasureTtoStr(Item: TCustomItem; DefFS: Boolean = True): String;
  function MeasureFtoStr(Value: TWFloat32; DefFS: Boolean = True): String;
  function MeasureFtoStr(Item: TCustomItem; DefFS: Boolean = True): String;
  function NoiseToStr(Value: TWFloat32; DefFS: Boolean = True): String;
  function NoiseToStr(Item: TCustomItem; DefFS: Boolean = True): String;
  function EpsilonToStr(Value: TWFloat32; DefFS: Boolean = True): String;
  function EpsilonToStr(Item: TIncItem; DefFS: Boolean = True): String;
  function DeltaLtoStr(Value: TWFloat32; DefFS: Boolean = True): String;
  function DeltaLtoStr(Item: TIncItem; DefFS: Boolean = True): String;
  function SigmaGet(Value: Float32; Units: Integer): Float32;
  function SigmaToStr(Value: TWFloat32; Units: Integer;
   DefFS: Boolean = True): String;
  function SigmaToStr(Item: TIncItem; Units: Integer;
   DefFS: Boolean = True): String;
  function ObjToStr(Item: TVistItem): String;
  function MeasureStoStr(Value: TWFloat32; DefFS: Boolean = True): String;
  function MeasureSppToStr(Item: TVistItem; DefFS: Boolean = True): String;
  function MeasureVtoStr(Value: TWFloat32; DefFS: Boolean = True): String;
  function MeasureVrmsToStr(Item: TVistItem; DefFS: Boolean = True): String;
  function MeasureAtoStr(Value: TWFloat32; DefFS: Boolean = True): String;
  function MeasureAampToStr(Item: TVistItem; DefFS: Boolean = True): String;

implementation

uses
  UGlobal;

const
  DomLCID = $0809;

var
  DomFormatSettings: TFormatSettings;

{ TCustomItem }
constructor TCustomItem.Create;
begin
  FComments := nil;
  FDate := nil;
  FTime := nil;
  FNumber := nil;
  FMeasureT := nil;
  FNoise := nil;
end;

destructor TCustomItem.Destroy;
begin
  if Assigned(FComments) then
    FComments.Free;
  if Assigned(FDate) then
    FDate.Free;
  if Assigned(FTime) then
    FTime.Free;
  if Assigned(FNumber) then
    FNumber.Free;
  if Assigned(FMeasureT) then
    FMeasureT.Free;
  if Assigned(FMeasureF) then
    FMeasureF.Free;
  if Assigned(FNoise) then
    FNoise.Free;

  inherited;
end;

function TCustomItem.GetMeasureF: TWFloat32;
var
  F: Boolean;
begin
  F := False;
  if not Assigned(FMeasureT)
    then F := True
    else if FMeasureT.Value = 0.0 then
      F := True;

  if F then
  begin
    if Assigned(FMeasureF) then
    begin
      FMeasureF.Free;
      FMeasureF := nil;
    end;
  end
  else
  begin
    if not Assigned(FMeasureF) then
      FMeasureF := TWFloat32.Create;
    FMeasureF.Value := 1.0/FMeasureT.Value;
  end;

  Result := FMeasureF;
end;

class function TCustomItem.Compare(Item1, Item2: TCustomItem): Integer;
label
  OnMore, OnLess, OnEqual;
var
  Dt1,Dt2: TWDate;
  Tm1,Tm2: TWTime;
begin
  if Item1 = Item2 then
    goto OnEqual;

  if not Assigned(Item1) then
    goto OnLess;

  if not Assigned(Item2) then
    goto OnMore;

  Dt1 := Item1.Date;
  Dt2 := Item2.Date;
  Tm1 := Item1.Time;
  Tm2 := Item2.Time;

  if Dt1 = Dt2 then
  begin
    if Tm1 = Tm2 then
      goto OnEqual;

    if not Assigned(Tm1) then
      goto OnLess;

    if not Assigned(Tm2) then
      goto OnMore;

    if Tm1.Value = Tm2.Value then
      goto OnEqual;

    if Tm1.Value < Tm2.Value
      then goto OnLess
      else goto OnMore;
  end;

  if not Assigned(Dt1) then
    goto OnLess;

  if not Assigned(Dt2) then
    goto OnMore;

  if Dt1.Value = Dt2.Value then
    goto OnEqual;

  if Dt1.Value < Dt2.Value
    then goto OnLess
    else goto OnMore;

  OnMore:
  begin
    Result := 1;
    Exit;
  end;

  OnLess:
  begin
    Result := -1;
    Exit;
  end;

  OnEqual:
  begin
    Result := 0;
  end;
end;

function TCustomItem.Compare(Item: TCustomItem): Integer;
begin
  Result := Compare(Self, Item);
end;

procedure TCustomItem.Save(Root: TDOMElement);
var
  Doc: TDOMDocument;
  SubNode: TDOMElement;
begin
  Doc := Root.OwnerDocument;
  SubNode := Doc.CreateElement(ItemNodeName);
  Root.AppendChild(SubNode);
  SaveFields(Doc, SubNode);
end;

procedure TCustomItem.Load(Root: TDOMNode);
var
  Child: TDOMNode;
  Grandson: TDomNode;
  Txt: UTF8String;
begin
  Child := Root.FirstChild;
  while Assigned(Child) do
  begin
    Txt := '';
    Grandson := Child.FirstChild;
    while Assigned(Grandson) do
    begin
      if Grandson is TDOMText then
      begin
        Txt := UTF8String((Grandson as TDOMText).Data);
        Break;
      end;
      Grandson := Grandson.NextSibling;
    end;
    LoadField(Child.NodeName, Txt);

    Child := Child.NextSibling;
  end;
end;

procedure TCustomItem.SaveFields(Doc: TDOMDocument; Root: TDOMElement);
var
  El: TDOMElement;

procedure Add(Key: DOMString; Val: UTF8String);
begin
  El := Doc.CreateElement(WideString(Key));
  El.AppendChild(Doc.CreateTextNode(WideString(Val)));
  Root.AppendChild(El);
end;

begin
  if Assigned(FDate) then
    Add(DateFieldName, DateToStr(Self, False));
  if Assigned(FTime) then
    Add(TimeFieldName, TimeToStr(Self, False));
  if Assigned(FNumber) then
    Add(NumberFieldName, NumberToStr(Self, False));
  if Assigned(FComments) then
    Add(CommentFieldName, FComments.Value);
  if Assigned(FNoise) then
    Add(NoiseFieldName, NoiseToStr(Self, False));
  if Assigned(FMeasureT) then
    Add(MeasureTFieldName, MeasureTtoStr(Self, False));
end;

function TCustomItem.LoadField(FieldName: DOMString; Txt: UTF8String): Boolean;
var
  NewDate: TWDate;
  NewTime: TWTime;
  NewInt: TWInt;
  NewFloat: TWFloat32;
begin
  Result := True;

  try
    if FieldName = DateFieldName then
    begin
      NewDate := TWDate.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FDate) then
        FDate.Free;
      FDate := NewDate;
    end

    else if FieldName = TimeFieldName then
    begin
      NewTime := TWTime.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FTime) then
        FTime.Free;
      FTime := NewTime;
    end

    else if FieldName = NumberFieldName then
    begin
      NewInt := TWInt.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FNumber) then
        FNumber.Free;
      FNumber := NewInt;
    end

    else if FieldName = CommentFieldName then
    begin
      if not Assigned(FComments) then
        FComments := TWString.Create;
      FComments.Value := Txt;
    end

    else if FieldName = NoiseFieldName then
    begin
      NewFloat := TWFloat32.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FNoise) then
        FNoise.Free;
      FNoise := NewFloat;
    end

    else if FieldName = MeasureTFieldName then
    begin
      NewFloat := TWFloat32.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FMeasureT) then
        FMeasureT.Free;
      FMeasureT := NewFloat;
    end

    else
      Result := False;

  except
  end;
end;

{ TIncItem }
constructor TIncItem.Create;
begin
  inherited;
  FDiameter := nil;
  FLen := nil;
  FSigma := nil;
  FEpsilon := nil;
  FDeltaL := nil;
  FSigma := nil;
end;

destructor TIncItem.Destroy;
begin
  if Assigned(FDiameter) then
    FDiameter.Free;
  if Assigned(FLen) then
    FLen.Free;
  if Assigned(FSigma) then
    FSigma.Free;
  if Assigned(FEpsilon) then
    FEpsilon.Free;
  if Assigned(FDeltaL) then
    FDeltaL.Free;
  inherited;
end;

procedure TIncItem.SaveFields(Doc: TDOMDocument; Root: TDOMElement);
var
  El: TDOMElement;

procedure Add(Key: DOMString; Val: UTF8String);
begin
  El := Doc.CreateElement(WideString(Key));
  El.AppendChild(Doc.CreateTextNode(WideString(Val)));
  Root.AppendChild(El);
end;

begin
  inherited SaveFields(Doc, Root);

  if Assigned(FDiameter) then
    Add(DiameterFieldName, DiameterToStr(Self, False));
  if Assigned(FLen) then
    Add(LengthFieldName, LengthToStr(Self, False));
  if Assigned(FSigma) then
    Add(SigmaFieldName, SigmaToStr(Self, 0, False));
  if Assigned(FEpsilon) then
    Add(EpsilonFieldName, EpsilonToStr(Self, False));
  if Assigned(FDeltaL) then
    Add(DeltaLFieldName, DeltaLToStr(Self, False));
end;

function TIncItem.LoadField(FieldName: DOMString; Txt: UTF8String): Boolean;
var
  NewFloat: TWFloat32;
  NewInt: TWInt;
begin
  Result := True;

  if inherited LoadField(FieldName, Txt) then
    Exit;

  try
    if FieldName = DiameterFieldName then
    begin
      NewInt := TWInt.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FDiameter) then
        FDiameter.Free;
      FDiameter := NewInt;

    end
    else if FieldName = LengthFieldName then
    begin
      NewFloat := TWFloat32.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FLen) then
        FLen.Free;
      FLen := NewFloat;
    end

    else if FieldName = SigmaFieldName then
    begin
      NewFloat := TWFloat32.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FSigma) then
        FSigma.Free;
      FSigma := NewFloat;
    end

    else if FieldName = EpsilonFieldName then
    begin
      NewFloat := TWFloat32.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FEpsilon) then
        FEpsilon.Free;
      FEpsilon := NewFloat;
    end

    else if FieldName = DeltaLFieldName then
    begin
      NewFloat := TWFloat32.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FDeltaL) then
        FDeltaL.Free;
      FDeltaL := NewFloat;
    end

    else
      Result := False;

  except
  end;
end;

{ TVistItem }
constructor TVistItem.Create;
begin
  inherited;
  FObj := nil;
  FTyp := nil;
  FMeasureSpp := nil;
  FMeasureVrms := nil;
  FMeasureAamp := nil;
end;

destructor TVistItem.Destroy;
begin
  if Assigned(FObj) then
    FObj.Free;
  if Assigned(FTyp) then
    FTyp.Free;
  if Assigned(FMeasureSpp) then
    FMeasureSpp.Free;
  if Assigned(FMeasureVrms) then
    FMeasureVrms.Free;
  if Assigned(FMeasureAamp) then
    FMeasureAamp.Free;
  inherited;
end;

procedure TVistItem.SaveFields(Doc: TDOMDocument; Root: TDOMElement);
var
  El: TDOMElement;

procedure Add(Key: DOMString; Val: UTF8String);
begin
  El := Doc.CreateElement(WideString(Key));
  El.AppendChild(Doc.CreateTextNode(WideString(Val)));
  Root.AppendChild(El);
end;

begin
  inherited SaveFields(Doc, Root);
  if Assigned(FObj)
    then Add(ObjFieldName, FObj.ToStr(DomFormatSettings));
  if Assigned(FTyp)
    then Add(TypFieldName, FTyp.ToStr(DomFormatSettings));
  if Assigned(MeasureSpp)
    then Add(MeasureSppFieldName, MeasureSppToStr(Self, False));
  if Assigned(MeasureVrms)
    then Add(MeasureVrmsFieldName, MeasureVrmsToStr(Self, False));
  if Assigned(MeasureAamp)
    then Add(MeasureAampFieldName, MeasureAampToStr(Self, False));
end;

function TVistItem.LoadField(FieldName: DOMString; Txt: UTF8String): Boolean;
var
  NewFloat: TWFloat32;
  NewInt: TWInt;
begin
  Result := True;

  if inherited LoadField(FieldName, Txt) then
    Exit;

  try
    if FieldName = ObjFieldName then
    begin
      NewInt := TWInt.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FObj) then
        FObj.Free;
      FObj := NewInt;
    end

    else if FieldName = TypFieldName then
    begin
      NewInt := TWInt.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FTyp) then
        FTyp.Free;
      FTyp := NewInt;
    end

    else if FieldName = MeasureSppFieldName then
    begin
      NewFloat := TWFloat32.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FMeasureSpp) then
        FMeasureSpp.Free;
      FMeasureSpp := NewFloat;
    end

    else if FieldName = MeasureVrmsFieldName then
    begin
      NewFloat := TWFloat32.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FMeasureVrms) then
        FMeasureVrms.Free;
      FMeasureVrms := NewFloat;
    end

    else if FieldName = MeasureAampFieldName then
    begin
      NewFloat := TWFloat32.Parse(UTF8Trim(Txt), DomFormatSettings);
      if Assigned(FMeasureAamp) then
        FMeasureAamp.Free;
      FMeasureAamp := NewFloat;
    end

    else
      Result := False;

  except
  end;
end;

{ TBase }
constructor TBase.Create;
begin
  FAutoDelete := True;
end;

destructor TBase.Destroy;
begin
  if FAutoDelete then
    FreeMembers;
  inherited;
end;

procedure TBase.FreeMembers;
begin
end;

{ TDescription }
constructor TDescription.Create;
begin
  inherited;
  FText := nil;
end;

procedure TDescription.FreeMembers;
begin
  if Assigned(FText) then
  begin
    FText.Free;
    FText := nil;
  end;

  inherited;
end;

procedure TDescription.Clear;
begin
  if AutoDelete and Assigned(FText) then
    FText.Free;
  FText := nil;
end;

procedure TDescription.Save(Root: TDOMElement);
var
  Doc: TDOMDocument;
  SubNode: TDomElement;
begin
  Doc := Root.OwnerDocument;

  SubNode := Doc.CreateElement(DescriptionNodeName);
  Root.AppendChild(SubNode);
  if Assigned(FText) then
  begin
    SubNode.AppendChild(Doc.CreateTextNode(WideString(FText.Value)));
  end;
end;

procedure TDescription.Load(Root: TDOMNode);
var
  Child: TDOMNode;
  TextNode: TDOMText;
begin
  Child := Root.FirstChild;
  while Assigned(Child) do
  begin
    if Child is TDOMText then
    begin
      TextNode := Child as TDOMText;
      if not Assigned(FText) then
        FText := TWString.Create(UTF8String(TextNode.Data))
      else
        FText.Value := FText.Value + UTF8String(TextNode.Data);
    end;
    Child := Child.NextSibling;
  end;
end;

{ TCustomData }
constructor TCustomData.Create;
begin
  inherited;
  FList := TList.Create;
  FSorted := False;
end;

destructor TCustomData.Destroy;
var
  SavedList: TList;
begin
  SavedList := FList;
  inherited;
  SavedList.Free;
end;

function TCustomData.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TCustomData.FreeMembers;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    TCustomItem(FList.Items[I]).Free;
  FList.Clear;
end;

procedure TCustomData.Clear;
begin
  if AutoDelete
    then FreeMembers
    else FList.Clear;
end;

procedure TCustomData.Delete(Index: Integer);
var
  Item: TCustomItem;
begin
  Item := TCustomItem(FList.Items[Index]);
  FList.Delete(Index);
  if AutoDelete then
    Item.Free;
end;

procedure TCustomData.Delete(Item: TCustomItem);
var
  Index: Integer;
begin
  Index := IndexOf(Item);
  if Index >= 0 then
    Delete(Index);
end;

function TCustomData.GetCustomItem(Index: Integer): TCustomItem;
begin
  Result := TCustomItem(FList.Items[Index]);
end;

procedure TCustomData.Add(NewItem: TCustomItem);
begin
  FSorted := False;
  FList.Add(NewItem);
end;

procedure TCustomData.Sort;
var
  I,J,N: Integer;
  ItemI,ItemJ: TCustomItem;
begin
  N := Count - 1;
  for I := 0 to N - 1 do
  begin
    for J := I + 1 to N do
    begin
      ItemI := TCustomItem(FList.Items[I]);
      ItemJ := TCustomItem(FList.Items[J]);
      if TCustomItem.Compare(ItemI, ItemJ) > 0 then
        FList.Exchange(I, J);
    end;
  end;

  FSorted := True;
end;

function TCustomData.IndexOf(Item: TCustomItem): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := Count - 1 downto 0 do
  begin
    if TCustomItem(FList.Items[I]) = Item then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TCustomData.Find(Dt: TDate; Tm: TTime): Integer;
var
  I: Integer;
  Item: TCustomItem;
  WDt: TWDate;
  WTm: TWTime;
begin
  Result := -1;

  for I := Count - 1 downto 0 do
  begin
    Item := TCustomItem(FList.Items[I]);
    WDt := Item.Date;
    if not Assigned(WDt) then
      Continue;
    WTm := Item.Time;
    if not Assigned(WTm) then
      Continue;
    if (WDt.Value = Dt) and (WTm.Value = Tm) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{ TIncData }
function TIncData.GetItem(Index: Integer): TIncItem;
begin
  Result := TIncItem(GetCustomItem(Index));
end;

procedure TIncData.Add(NewItem: TIncItem);
begin
  inherited Add(NewItem);
end;

procedure TIncData.Save(Root: TDOMElement);
var
  Doc: TDOMDocument;
  SubNode: TDOMElement;
  I,N: Integer;
begin
  Doc := Root.OwnerDocument;

  SubNode := Doc.CreateElement(IncNodeName);
  Root.AppendChild(SubNode);

  N := Count - 1;
  for I := 0 to N do
  begin
    Items[I].Save(SubNode);
  end;
end;

procedure TIncData.Load(Root: TDOMNode);
var
  Child: TDOMNode;
  NewItem: TIncItem;
begin
  Child := Root.FirstChild;
  while Assigned(Child) do
  begin
    if Child.NodeName = ItemNodeName then
    begin
      NewItem := TIncItem.Create;
      try
        NewItem.Load(Child);
        Add(NewItem);
      except
        NewItem.Free;
      end;
    end;
    Child := Child.NextSibling;
  end;
end;

{ TVistData }
function TVistData.GetItem(Index: Integer): TVistItem;
begin
  Result := TVistItem(GetCustomItem(Index));
end;

procedure TVistData.Add(NewItem: TVistItem);
begin
  inherited Add(NewItem);
end;

procedure TVistData.Save(Root: TDOMElement);
var
  Doc: TDOMDocument;
  SubNode: TDOMElement;
  I,N: Integer;
begin
  Doc := Root.OwnerDocument;

  SubNode := Doc.CreateElement(VistNodeName);
  Root.AppendChild(SubNode);

  N := Count - 1;
  for I := 0 to N do
  begin
    Items[I].Save(SubNode);
  end;
end;

procedure TVistData.Load(Root: TDOMNode);
var
  Child: TDOMNode;
  NewItem: TVistItem;
begin
  Child := Root.FirstChild;
  while Assigned(Child) do
  begin
    if Child.NodeName = ItemNodeName then
    begin
      NewItem := TVistItem.Create;
      try
        NewItem.Load(Child);
        Add(NewItem);
      except
        NewItem.Free;
      end;
    end;
    Child := Child.NextSibling;
  end;
end;

{ TDeviceData }
constructor TDeviceData.Create;
begin
  FDescription := TDescription.Create;
  FIncData := TIncData.Create;
  FVistData := TVistData.Create;
end;

destructor TDeviceData.Destroy;
begin
  FDescription.Free;
  FIncData.Free;
  FVistData.Free;
  inherited;
end;

function TDeviceData.Save(FileName: UTF8String): Boolean;
var
  Doc: TXMLDocument;
  Root: TDOMElement;
begin
  Doc := TXMLDocument.Create;

  try
    Root := Doc.CreateElement(RootNodeName);
    Doc.AppendChild(Root);
    FDescription.Save(Root);
    FIncData.Save(Root);
    FVistData.Save(Root);
    WriteXMLFile(Doc, FileName);
    Result := True;
  except
    Result := False;
  end;

  Doc.Free;
end;

function TDeviceData.Load(FileName: UTF8String): Boolean;
var
  Doc: TXmlDocument;
  Child: TDOMNode;
begin
  Clear;

  Doc := nil;

  try
    ReadXMLFile(Doc, FileName);
    Child := Doc.FirstChild;
    while Assigned(Child) do
    begin
      if Child.NodeName = RootNodeName then
        LoadImpl(Child);
      Child := Child.NextSibling;
    end;
    Result := True;
  except
    Result := False;
  end;

  if Assigned(Doc) then
    Doc.Free;

  Sort;
end;

procedure TDeviceData.LoadImpl(Root: TDOMNode);
var
  Child: TDOMNode;
  NodeName: DOMString;
begin
  Child := Root.FirstChild;
  while Assigned(Child) do
  begin
    NodeName := Child.NodeName;
    if NodeName = DescriptionNodeName then
      FDescription.Load(Child)
    else if NodeName = IncNodeName then
      FIncData.Load(Child)
    else if NodeName = VistNodeName then
      FVistData.Load(Child);
    Child := Child.NextSibling;
  end;
end;

procedure TDeviceData.Sort;
begin
  FIncData.Sort;
  FVistData.Sort;
end;

procedure TDeviceData.Clear;
begin
  FDescription.Clear;
  FIncData.Clear;
  FVistData.Clear;
end;

{ TDeviceQuery }
constructor TDeviceQuery.Create;
begin
  inherited;
  FDescription.AutoDelete := False;
  FIncData.AutoDelete := False;
  FVistData.AutoDelete := False;
end;

{ --- }
function DateToStr(Item: TCustomItem; DefFS: Boolean): String;
begin
  if DefFS
    then Result := ToStr(Item.Date)
    else Result := ToStr(Item.Date, DomFormatSettings);
end;

function TimeToStr(Item: TCustomItem; DefFS: Boolean): String;
begin
  if DefFS
    then Result := ToStr(Item.Time)
    else Result := ToStr(Item.Time, DomFormatSettings);
end;

function TimeToStr(Item: TVistItem): String;
begin
  Result := ToStr(Item.Time);
end;

function NumberToStr(Item: TCustomItem; DefFS: Boolean): String;
begin
  if DefFS
    then Result := ToStr(Item.Number)
    else Result := ToStr(Item.Number, DomFormatSettings);
end;

function DiameterToStr(Item: TIncItem; DefFS: Boolean): String;
begin
  if DefFS
    then Result := ToStr(Item.Diameter)
    else Result := ToStr(Item.Diameter, DomFormatSettings);
end;

function LengthToStr(Item: TIncItem; DefFS: Boolean): String;
begin
  if DefFS
    then Result := TWFloat32.ToStr(Item.Len, 15, 2)
    else Result := TWFloat32.ToStr(Item.Len, 15, 2, DomFormatSettings);
end;

function MeasureTtoStr(Value: TWFloat32; DefFS: Boolean): String;
begin
  if DefFS
    then Result := TWFloat32.ToStr(Value, 15, 3)
    else Result := TWFloat32.ToStr(Value, 15, 3, DomFormatSettings);
end;

function MeasureTtoStr(Item: TCustomItem; DefFS: Boolean): String;
begin
  Result := MeasureTtoStr(Item.MeasureT, DefFS);
end;

function MeasureFtoStr(Value: TWFloat32; DefFS: Boolean): String;
begin
  if DefFS
    then Result := TWFloat32.ToStr(Value, 15, 3)
    else Result := TWFloat32.ToStr(Value, 15, 3, DomFormatSettings);
end;

function MeasureFtoStr(Item: TCustomItem; DefFS: Boolean): String;
begin
  Result := MeasureFtoStr(Item.MeasureF, DefFS);
end;

function NoiseToStr(Value: TWFloat32; DefFS: Boolean): String;
begin
  if DefFS
    then Result := TWFloat32.ToStr(Value, 15, 0)
    else Result := TWFloat32.ToStr(Value, 15, 0, DomFormatSettings);
end;

function NoiseToStr(Item: TCustomItem; DefFS: Boolean): String;
begin
  Result := NoiseToStr(Item.Noise, DefFS);
end;

function EpsilonToStr(Value: TWFloat32; DefFS: Boolean): String;
begin
  if DefFS
    then Result := TWFloat32.ToStr(Value, 15, 0)
    else Result := TWFloat32.ToStr(Value, 15, 0, DomFormatSettings);
end;

function EpsilonToStr(Item: TIncItem; DefFS: Boolean): String;
begin
  Result := EpsilonToStr(Item.Epsilon, DefFS);
end;

function DeltaLtoStr(Value: TWFloat32; DefFS: Boolean): String;
begin
  if DefFS
    then Result := TWFloat32.ToStr(Value, 15, 3)
    else Result := TWFloat32.ToStr(Value, 15, 3, DomFormatSettings);
end;

function DeltaLtoStr(Item: TIncItem; DefFS: Boolean): String;
begin
  Result := DeltaLtoStr(Item.DeltaL, DefFS);
end;

function SigmaGet(Value: Float32; Units: Integer): Float32;
begin
  if Units = 1
    then Result := Value*(1.0/0.0980665)
    else Result := Value;
end;

function SigmaToStr(Value: TWFloat32; Units: Integer; DefFS: Boolean): String;
var
  V: Float32;
begin
  if not Assigned(Value) then
    Result := ''
  else
  begin
    V := SigmaGet(Value.Value, Units);
    if DefFS
      then Result := FloatToStrF(V, ffFixed, 15, 3)
      else Result := FloatToStrF(V, ffFixed, 15, 3, DomFormatSettings);
  end;
end;

function SigmaToStr(Item: TIncItem;  Units: Integer; DefFS: Boolean): String;
begin
  Result := SigmaToStr(Item.Sigma, Units, DefFS);
end;

function ObjToStr(Item: TVistItem): String;
begin
  Result := '';
  if Assigned(Item.Obj) then
  begin
    case Item.Obj.Value of
      VistObjGeneralId:     Result := Gti(1172);
      VistObjShakerTableId: Result := Gti(1173);
    end;
  end;
end;

function MeasureStoStr(Value: TWFloat32; DefFS: Boolean): String;
begin
  if DefFS
    then Result := TWFloat32.ToStr(Value, 15, 0)
    else Result := TWFloat32.ToStr(Value, 15, 0, DomFormatSettings);
end;

function MeasureSppToStr(Item: TVistItem; DefFS: Boolean): String;
begin
  if Assigned(Item.MeasureSpp)
    then Result := MeasureStoStr(Item.MeasureSpp, DefFS)
    else Result := '';
end;

function MeasureVtoStr(Value: TWFloat32; DefFS: Boolean): String;
begin
  if DefFS
    then Result := TWFloat32.ToStr(Value, 15, 3)
    else Result := TWFloat32.ToStr(Value, 15, 3, DomFormatSettings);
end;

function MeasureVrmsToStr(Item: TVistItem; DefFS: Boolean): String;
begin
  if Assigned(Item.MeasureVrms)
    then Result := MeasureVtoStr(Item.MeasureVrms, DefFS)
    else Result := '';
end;

function MeasureAtoStr(Value: TWFloat32; DefFS: Boolean = True): String;
begin
  if DefFS
    then Result := TWFloat32.ToStr(Value, 15, 3)
    else Result := TWFloat32.ToStr(Value, 15, 3, DomFormatSettings);
end;

function MeasureAampToStr(Item: TVistItem; DefFS: Boolean = True): String;
begin
  if Assigned(Item.MeasureAamp)
    then Result := MeasureFtoStr(Item.MeasureAamp, DefFS)
    else Result := '';
end;

procedure Init;
begin
  {%H-}GetLocaleFormatSettings(DomLCID, DomFormatSettings);
end;

initialization
  Init;
end.

