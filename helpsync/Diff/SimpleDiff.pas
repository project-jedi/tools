{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI                                                                                     }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is SimpleDiff.pas                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright Florent Ouchet. All rights reserved.            }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}

unit SimpleDiff;

interface

uses
  SysUtils,
  Classes,
  JclBase,
  JclContainerIntf;

type
  ESimpleDiffError = class(Exception);
  
  TSimpleDiffFlag = (dfDelete, dfInsert, dfConflict);
  TSimpleDiffFlags = set of TSimpleDiffFlag;

  TStringSimpleDiff = record
    LeftIndex: Integer;
    RightIndex: Integer;
    LeftValue: string;
    RightValue: string;
    Flags: TSimpleDiffFlags;
  end;
  TStringSimpleDiffs = array of TStringSimpleDiff;

  TMergePosition = (mpSame, mpAfter, mpBefore);

  TStringsSimpleDiff = class;

  TConflictResolution = (crError, crAutomatic, crPostpone, crDone);

  TMergeConflictEvent = procedure (Sender: TStringsSimpleDiff;
    MyIndex: Integer; var NewStringDiff: TStringSimpleDiff;
    var MergePosition: TMergePosition; var Resolution: TConflictResolution) of object;

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
    FDefaultConflictResolution: TConflictResolution;
    FOnMergeConflict: TMergeConflictEvent;
    procedure Grow(ACapacity: Integer = 0);
    function GetStringDiff(Index: Integer): TStringSimpleDiff;
    procedure InternalAdd(var NewStringDiff: TStringSimpleDiff; Position: TMergePosition);
    procedure InternalDiff(LeftStrings: TStrings; const LeftHashes: TDynIntegerArray;
      RightStrings: TStrings; const RightHashes: TDynIntegerArray);
    function MergeConflict(MyIndex: Integer; var NewStringDiff: TStringSimpleDiff;
      MergePosition: TMergePosition): Boolean;
    procedure SetStringDiff(Index: Integer; const Value: TStringSimpleDiff);
    function GetConflictCount: Integer;
  public
    constructor Create(HashConvert: TStrHashConvert = nil;
                       StrCompare: TStrEqualityCompare = nil);
    destructor Destroy; override;
    procedure Add(StringsDiff: TStringsSimpleDiff; Position: TMergePosition = mpSame); overload;
    procedure Add(StringDiff: TStringSimpleDiff; Position: TMergePosition = mpSame); overload;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Diff(LeftStrings, RightStrings: TStrings);
    procedure Check(LeftStrings: TStrings);
    procedure Merge(LeftStrings, RightStrings: TStrings); overload;
    procedure Merge(Strings: TStrings); overload;
    procedure Reverse;
    property Count: Integer read FCount;
    property StringDiffs[Index: Integer]: TStringSimpleDiff read GetStringDiff write SetStringDiff;
    property LeftName: string read FLeftName write FLeftName;
    property LeftVersion: string read FLeftVersion write FLeftVersion;
    property RightName: string read FRightName write FRightName;
    property RightVersion: string read FRightVersion write FRightVersion;
    property ConflictCount: Integer read GetConflictCount;
    property DefaultConflictResolution: TConflictResolution read FDefaultConflictResolution write FDefaultConflictResolution;
    property OnMergeConflict: TMergeConflictEvent read FOnMergeConflict write FOnMergeConflict;
  end;

function IsDelete(Flags: TSimpleDiffFlags): Boolean; inline;
function IsInsert(Flags: TSimpleDiffFlags): Boolean; inline;
function IsModify(Flags: TSimpleDiffFlags): Boolean; inline;
function IsNone(Flags: TSimpleDiffFlags): Boolean; inline;

procedure IncOffset(var Offset: Integer; Flags: TSimpleDiffFlags);

implementation

uses
  SysConst,
  JclAlgorithms;

function IsDelete(Flags: TSimpleDiffFlags): Boolean; inline;
begin
  Result := Flags * [dfDelete, dfInsert] = [dfDelete];
end;

function IsInsert(Flags: TSimpleDiffFlags): Boolean; inline;
begin
  Result := Flags * [dfDelete, dfInsert] = [dfInsert];
end;

function IsModify(Flags: TSimpleDiffFlags): Boolean; inline;
begin
  Result := Flags * [dfDelete, dfInsert] = [dfDelete, dfInsert];
end;

function IsNone(Flags: TSimpleDiffFlags): Boolean; inline;
begin
  Result := Flags * [dfDelete, dfInsert] = [];
end;

procedure IncOffset(var Offset: Integer; Flags: TSimpleDiffFlags);
begin
  if dfDelete in Flags then
    Dec(Offset);
  if dfInsert in Flags then
    Inc(Offset);
end;

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

procedure TStringsSimpleDiff.Add(StringsDiff: TStringsSimpleDiff;
  Position: TMergePosition);
var
  NewStringDiff: TStringSimpleDiff;
  Index: Integer;
begin
  for Index := 0 to StringsDiff.Count - 1 do
  begin
    NewStringDiff := StringsDiff.StringDiffs[Index];
    InternalAdd(NewStringDiff, Position);
  end;
end;

procedure TStringsSimpleDiff.Add(StringDiff: TStringSimpleDiff;
  Position: TMergePosition);
begin
  InternalAdd(StringDiff, Position);
end;

procedure TStringsSimpleDiff.Check(LeftStrings: TStrings);
var
  LeftCount, RightCount, Index: Integer;
begin
  LeftCount := LeftStrings.Count;
  RightCount := LeftStrings.Count;
  for Index := 0 to Count - 1 do
  begin
    if IsModify(FStringDiffs[Index].Flags) then
    begin
      if (FStringDiffs[Index].LeftIndex >= LeftCount) or
         (FStringDiffs[Index].RightIndex > RightCount) or
         not FStrCompare(FStringDiffs[Index].LeftValue, LeftStrings.Strings[FStringDiffs[Index].LeftIndex]) then
        raise ESimpleDiffError.Create('inconsistent source');
    end
    else
    if IsInsert(FStringDiffs[Index].Flags) then
    begin
      if FStringDiffs[Index].RightIndex > RightCount then
        raise ESimpleDiffError.Create('inconsistent source');
      Inc(RightCount);
    end
    else
    if IsDelete(FStringDiffs[Index].Flags) then
    begin
      if FStringDiffs[Index].RightIndex >= RightCount then
        raise ESimpleDiffError.Create('inconsistent source');
      Dec(RightCount);
    end;
  end;
end;

procedure TStringsSimpleDiff.Clear;
begin
  FCount := 0;
  SetLength(FStringDiffs, 0);
end;

procedure TStringsSimpleDiff.Delete(Index: Integer);
var
  StringDiff: TStringSimpleDiff;
  RightOffset: Integer;
begin
  StringDiff := GetStringDiff(Index);
  RightOffset := 0;
  IncOffset(RightOffset, StringDiff.Flags);
  // offset is opposed since the item is deleted
  RightOffset := -RightOffset;
  Dec(FCount);
  while Index < FCount do
  begin
    FStringDiffs[Index] := FStringDiffs[Index + 1];
    Inc(FStringDiffs[Index].RightIndex, RightOffset);
    Inc(Index);
  end;
  Finalize(FStringDiffs[FCount]);
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

function TStringsSimpleDiff.GetConflictCount: Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to Count - 1 do
    if dfConflict in FStringDiffs[Index].Flags then
      Inc(Result);
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

procedure TStringsSimpleDiff.InternalAdd(var NewStringDiff: TStringSimpleDiff;
  Position: TMergePosition);
  function CheckIndexes(MyIndex, NewIndex: Integer; var I: Integer): Boolean;
  var
    OldPosition: TMergePosition;
  begin
    Result := False;
    if MyIndex > NewIndex then
      // insertion point is found
      Result := True
    else
    if MyIndex = NewIndex then
    begin
      // merge possible conflicts at the same position
      OldPosition := Position;
      Result := MergeConflict(I, NewStringDiff, Position);
      // the insertion order changed, restart from the beginning
      if OldPosition <> Position then
        I := -1;
    end;
  end;
var
  I, J, NewOffset, SelfOffset: Integer;
begin
  I := 0;
  while I < Count do
  begin
    case Position of
      mpSame:
        if CheckIndexes(FStringDiffs[I].LeftIndex, NewStringDiff.LeftIndex, I) then
          Break;
      mpAfter:
        if CheckIndexes(FStringDiffs[I].RightIndex, NewStringDiff.LeftIndex, I) then
          Break;
      mpBefore:
        if CheckIndexes(FStringDiffs[I].LeftIndex, NewStringDiff.RightIndex, I) then
          Break;
    end;
    Inc(I);
  end;
  if not IsNone(NewStringDiff.Flags) then
  begin
    if Length(FStringDiffs) <= FCount then
      Grow;
    NewOffset := 0;
    IncOffset(NewOffset, NewStringDiff.Flags);
    if I > 0 then
    begin
      SelfOffset := FStringDiffs[I - 1].RightIndex - FStringDiffs[I - 1].LeftIndex;
      IncOffset(SelfOffset, FStringDiffs[I - 1].Flags);
    end
    else
      SelfOffset := 0;
    case Position of
      mpAfter:
        NewStringDiff.LeftIndex := NewStringDiff.RightIndex - SelfOffset;
      mpSame,
      mpBefore:
        NewStringDiff.RightIndex := NewStringDiff.LeftIndex + SelfOffset;
    end;

    J := FCount;
    Inc(FCount);
    while J > I do
    begin
      FStringDiffs[J + 1] := FStringDiffs[J];
      case Position of
        mpAfter:
          Inc(FStringDiffs[J + 1].RightIndex, NewOffset);
        mpSame,
        mpBefore:
          Dec(FStringDiffs[J + 1].LeftIndex, NewOffset);
      end;
    end;
    FStringDiffs[I] := NewStringDiff;
  end;
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
        FStringDiffs[FCount].Flags := [dfInsert];
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
        FStringDiffs[FCount].Flags := [dfDelete];
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
            FStringDiffs[FCount].Flags := [dfInsert];
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
            FStringDiffs[FCount].Flags := [dfDelete];
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
      FStringDiffs[FCount].Flags := [dfDelete, dfInsert];
      Inc(FCount);
      Inc(LeftStart);
      Inc(RightStart);
    end;
    LeftContinue := LeftStart <= LeftStop;
    RightContinue := RightStart <= RightStop;
  end;
end;

procedure TStringsSimpleDiff.Merge(Strings: TStrings);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    if IsModify(FStringDiffs[Index].Flags) then
      Strings.Strings[FStringDiffs[Index].RightIndex] := FStringDiffs[Index].RightValue
    else
    if IsInsert(FStringDiffs[Index].Flags) then
      Strings.Insert(FStringDiffs[Index].RightIndex, FStringDiffs[Index].RightValue)
    else
    if IsDelete(FStringDiffs[Index].Flags) then
      Strings.Delete(FStringDiffs[Index].RightIndex);
  end;
end;

function TStringsSimpleDiff.MergeConflict(MyIndex: Integer; var NewStringDiff: TStringSimpleDiff;
  MergePosition: TMergePosition): Boolean;
var
  Resolution: TConflictResolution;
  I: Integer;
begin
  Resolution := DefaultConflictResolution;

  if Assigned(FOnMergeConflict) then
    FOnMergeConflict(Self, MyIndex, NewStringDiff, MergePosition, Resolution);

  if Resolution = crAutomatic then
  begin
    Resolution := crError;
    case MergePosition of
      mpSame:
        begin
          // diffs that exactly do the same operations can be merged (the new one is removed)
          if ((FStringDiffs[MyIndex].Flags * [dfDelete,dfInsert]) = (NewStringDiff.Flags * [dfDelete,dfInsert])) and
             FStrCompare(FStringDiffs[MyIndex].LeftValue, NewStringDiff.LeftValue) and
             FStrCompare(FStringDiffs[MyIndex].RightValue, NewStringDiff.RightValue) then
          begin
            NewStringDiff.Flags := []; // change to nop
            Resolution := crDone;
          end;
        end;
      mpAfter: ; // TODO
      mpBefore: ; // TODO
    end;
  end
  else
  if Resolution = crPostpone then
  begin
    Include(FStringDiffs[MyIndex].Flags, dfConflict);
    Include(NewStringDiff.Flags, dfConflict);
  end;

  if Assigned(FOnMergeConflict) then
    FOnMergeConflict(Self, MyIndex, NewStringDiff, MergePosition, Resolution);

  if Resolution = crError then
    raise ESimpleDiffError.CreateFmt('the diff at index %d conflicts', [MyIndex]);
  Result := Resolution in [crDone, crPostpone];
end;

procedure TStringsSimpleDiff.Reverse;
var
  Index, TmpIndex: Integer;
  TmpValue: string;
begin
  for Index := 0 to Count - 1 do
  begin
    if IsInsert(FStringDiffs[Index].Flags) then
      FStringDiffs[Index].Flags := [dfDelete]
    else
    if IsDelete(FStringDiffs[Index].Flags) then
      FStringDiffs[Index].Flags := [dfInsert];
    TmpIndex := FStringDiffs[Index].LeftIndex;
    FStringDiffs[Index].LeftIndex := FStringDiffs[Index].RightIndex;
    FStringDiffs[Index].RightIndex := TmpIndex;
    TmpValue := FStringDiffs[Index].LeftValue;
    FStringDiffs[Index].LeftValue := FStringDiffs[Index].RightValue;
    FStringDiffs[Index].RightValue := TmpValue;
  end;
end;

procedure TStringsSimpleDiff.Merge(LeftStrings, RightStrings: TStrings);
var
  I, J, LeftCount, DiffCount: Integer;
  AddLine: Boolean;
begin
  J := 0;
  LeftCount := LeftStrings.Count;
  DiffCount := Count;
  RightStrings.Clear;
  RightStrings.BeginUpdate;
  try
    for I := 0 to LeftCount - 1 do
    begin
      AddLine := (J >= DiffCount) or (I < FStringDiffs[J].LeftIndex);
      while (J < DiffCount) and (FStringDiffs[J].LeftIndex = I) do
      begin
        if IsModify(FStringDiffs[J].Flags) then
        begin
          AddLine := False;
          RightStrings.Add(FStringDiffs[J].RightValue);
        end
        else
        if IsInsert(FStringDiffs[J].Flags) then
        begin
          AddLine := True;
          RightStrings.Add(FStringDiffs[J].RightValue);
        end;
        Inc(J);
      end;
      if AddLine then
        RightStrings.Add(LeftStrings.Strings[I]);      
    end;
  finally
    RightStrings.EndUpdate;
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

