unit SimpleDiff;

interface

uses
  SysUtils,
  Classes,
  JclBase,
  JclContainerIntf;

type
  TSimpleDiffKind = (dkModify, dkInsert, dkDelete);

  TStringSimpleDiff = record
    LeftIndex: Integer;
    RightIndex: Integer;
    LeftValue: string;
    RightValue: string;
    Kind: TSimpleDiffKind;
  end;
  TStringSimpleDiffs = array of TStringSimpleDiff;

  TStringsSimpleDiff = class
  private
    FHashConvert: TStrHashConvert;
    FStrCompare: TStrEqualityCompare;
    FStringDiffs: TStringSimpleDiffs;
    FCount: Integer;
    FLeftName: string;
    FLeftVersion: string;
    FRightName: string;
    FRightVersion: string;
    procedure Grow(ACapacity: Integer = 0);
    function GetStringDiff(Index: Integer): TStringSimpleDiff;
    procedure InternalDiff(LeftStrings: TStrings; const LeftHashes: TDynIntegerArray;
      RightStrings: TStrings; const RightHashes: TDynIntegerArray);
    procedure SetStringDiff(Index: Integer; const Value: TStringSimpleDiff);
  public
    constructor Create(HashConvert: TStrHashConvert = nil;
                       StrCompare: TStrEqualityCompare = nil);
    destructor Destroy; override;
    procedure Clear;
    procedure Diff(LeftStrings, RightStrings: TStrings);
    property Count: Integer read FCount;
    property StringDiffs[Index: Integer]: TStringSimpleDiff read GetStringDiff write SetStringDiff;
    property LeftName: string read FLeftName write FLeftName;
    property LeftVersion: string read FLeftVersion write FLeftVersion;
    property RightName: string read FRightName write FRightName;
    property RightVersion: string read FRightVersion write FRightVersion;
  end;

implementation

uses
  SysConst,
  JclAlgorithms;

//=== { TStringsDiff } =======================================================

constructor TStringsSimpleDiff.Create(HashConvert: TStrHashConvert; StrCompare: TStrEqualityCompare);
begin
  inherited Create;
  if Assigned(HashConvert) then
    FHashConvert := HashConvert
  else
    FHashConvert := StrSimpleHashConvert;
  if Assigned(StrCompare) then
    FStrCompare := StrCompare
  else
    FStrCompare := StrSimpleEqualityCompare;
end;

destructor TStringsSimpleDiff.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TStringsSimpleDiff.Clear;
begin
  FCount := 0;
  SetLength(FStringDiffs, 0);
end;

procedure TStringsSimpleDiff.Diff(LeftStrings, RightStrings: TStrings);
var
  I, LeftCount, RightCount: Integer;
  LeftHashes, RightHashes: TDynIntegerArray;
begin
  Clear;
  LeftCount := LeftStrings.Count;
  SetLength(LeftHashes, LeftCount);
  for I := 0 to LeftCount - 1 do
    LeftHashes[I] := FHashConvert(LeftStrings.Strings[I]);
  RightCount := RightStrings.Count;
  SetLength(RightHashes, RightCount);
  for I := 0 to RightCount - 1 do
    RightHashes[I] := FHashConvert(RightStrings.Strings[I]);
  InternalDiff(LeftStrings, LeftHashes, RightStrings, RightHashes);
end;

function TStringsSimpleDiff.GetStringDiff(Index: Integer): TStringSimpleDiff;
begin
  if (Index >= Low(FStringDiffs)) and (Index <= High(FStringDiffs)) then
    Result := FStringDiffs[Index]
  else
    raise ERangeError.CreateRes(@SRangeError);
end;

procedure TStringsSimpleDiff.Grow(ACapacity: Integer);
var
  Len: Integer;
begin
  if ACapacity = 0 then
  begin
    Len := Length(FStringDiffs);
    if Len = 0 then
      SetLength(FStringDiffs, 16)
    else
      SetLength(FStringDiffs, Len * 2);
  end
  else
    SetLength(FStringDiffs, ACapacity);
end;

procedure TStringsSimpleDiff.InternalDiff(LeftStrings: TStrings; const LeftHashes: TDynIntegerArray;
  RightStrings: TStrings; const RightHashes: TDynIntegerArray);
var
  LeftStart, LeftStop, RightStart, RightStop: Integer;
  LeftContinue, RightContinue: Boolean;
  C, I: Integer;
begin
  LeftStart := 0;
  LeftStop := LeftStrings.Count - 1;
  RightStart := 0;
  RightStop := RightStrings.Count - 1;
  LeftContinue := LeftStart <= LeftStop;
  RightContinue := RightStart <= RightStop;
  while LeftContinue or RightContinue do
  begin
    // remove matching strings at the beginning
    while LeftContinue and RightContinue and
          (LeftHashes[LeftStart] = RightHashes[RightStart]) and
          (LeftStrings.Strings[LeftStart] = RightStrings.Strings[RightStart]) do
    begin
      Inc(LeftStart);
      Inc(RightStart);
      LeftContinue := LeftStart <= LeftStop;
      RightContinue := RightStart <= RightStop;
    end;
    // no change left
    if (not LeftContinue) and (not RightContinue) then
      Break;
    // some lines are added
    if not LeftContinue then
    begin
      C := RightStop - RightStart + 1;
      if Length(FStringDiffs) <= (FCount + C) then
        Grow(FCount + C);
      for I := 0 to C - 1 do
      begin
        FStringDiffs[FCount].LeftIndex := LeftStart;
        FStringDiffs[FCount].RightIndex := RightStart;
        FStringDiffs[FCount].LeftValue := '';
        FStringDiffs[FCount].RightValue := RightStrings.Strings[RightStart];
        FStringDiffs[FCount].Kind := dkInsert;
        Inc(FCount);
        Inc(RightStart);
      end;
      Break;
    end;
    // some lines were deleted
    if not RightContinue then
    begin
      C := LeftStop - LeftStart + 1;
      if Length(FStringDiffs) <= (FCount + C) then
        Grow(FCount + C);
      for I := 0 to C - 1 do
      begin
        FStringDiffs[FCount].LeftIndex := LeftStart;
        FStringDiffs[FCount].RightIndex := RightStart;
        FStringDiffs[FCount].LeftValue := LeftStrings.Strings[LeftStart];
        FStringDiffs[FCount].RightValue := '';
        FStringDiffs[FCount].Kind := dkDelete;
        Inc(FCount);
        Inc(LeftStart);
      end;
      Break;
    end;
    // multiple changes
    C := 1;
    while LeftContinue and RightContinue do
    begin
      if (RightStart + C) <= RightStop then
      begin
        if (LeftHashes[LeftStart] = RightHashes[RightStart + C]) and
           (LeftStrings.Strings[LeftStart] = RightStrings.Strings[RightStart + C]) then
        begin
          for I := 0 to C - 1 do
          begin
            if Length(FStringDiffs) <= FCount then
              Grow;
            FStringDiffs[FCount].LeftIndex := LeftStart;
            FStringDiffs[FCount].RightIndex := RightStart;
            FStringDiffs[FCount].LeftValue := '';
            FStringDiffs[FCount].RightValue := RightStrings.Strings[RightStart];
            FStringDiffs[FCount].Kind := dkInsert;
            Inc(FCount);
            Inc(RightStart);
          end;
          Break;
        end;
      end
      else
        RightContinue := False;
      if (LeftStart + C) <= LeftStop then
      begin
        if (LeftHashes[LeftStart + C] = RightHashes[RightStart]) and
           (LeftStrings.Strings[LeftStart + C] = RightStrings.Strings[RightStart]) then
        begin
          for I := 0 to C - 1 do
          begin
            if Length(FStringDiffs) <= FCount then
              Grow;
            FStringDiffs[FCount].LeftIndex := LeftStart;
            FStringDiffs[FCount].RightIndex := RightStart;
            FStringDiffs[FCount].LeftValue := LeftStrings.Strings[LeftStart];
            FStringDiffs[FCount].RightValue := '';
            FStringDiffs[FCount].Kind := dkDelete;
            Inc(FCount);
            Inc(LeftStart);
          end;
          Break;
        end;
      end
      else
        LeftContinue := False;
      Inc(C);
    end;
    LeftContinue := LeftStart <= LeftStop;
    RightContinue := RightStart <= RightStop;
    if LeftContinue and RightContinue and
      (LeftHashes[LeftStart] <> RightHashes[RightStart]) then
    begin
      if Length(FStringDiffs) <= FCount then
        Grow;
      FStringDiffs[FCount].LeftIndex := LeftStart;
      FStringDiffs[FCount].RightIndex := RightStart;
      FStringDiffs[FCount].LeftValue := LeftStrings.Strings[LeftStart];
      FStringDiffs[FCount].RightValue := RightStrings.Strings[RightStart];
      FStringDiffs[FCount].Kind := dkModify;
      Inc(FCount);
      Inc(LeftStart);
      Inc(RightStart);
    end;
    LeftContinue := LeftStart <= LeftStop;
    RightContinue := RightStart <= RightStop;
  end;
end;

procedure TStringsSimpleDiff.SetStringDiff(Index: Integer; const Value: TStringSimpleDiff);
begin
  if (Index >= Low(FStringDiffs)) and (Index <= High(FStringDiffs)) then
    FStringDiffs[Index] := Value
  else
    raise ERangeError.CreateRes(@SRangeError);
end;

end.

