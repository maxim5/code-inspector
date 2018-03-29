unit ICore.Stream;

interface
{$IFDEF ICORE}
uses ICore.Utils, ICore.Math;
{$ENDIF}

type
{$REGION 'TStrem = class'}
  TStream = class
    constructor Create;
  protected
    FSize: LongInt;
    FPos: LongInt;
    FValid: Boolean;

    procedure SetPosition(const Value: LongInt); virtual;
  public
    // read
    function Read(out Buf; Count: LongInt): LongWord; virtual;

    function ReadByte: Byte;          // UInt8
    function ReadWord: Word;          // UInt16
    function ReadLongWord: LongWord;  // UInt32
    function ReadShortInt: ShortInt;  // Int8
    function ReadSmallInt: SmallInt;  // Int16
    function ReadLongInt: LongInt;    // Int32
    function ReadSingle: Single;      // Float
    function ReadBoolean: Boolean;    // Boolean

    function ReadAnsiStr: AnsiString;
    function ReadWideStr: {$IFDEF VER200} UnicodeString {$ELSE} WideString {$ENDIF};
    function ReadStr: string;
    {$IFDEF ICORE}
      function ReadV2f: TVec2f;
      function ReadV3f: TVec3f;
      function ReadV4f: TVec4f;
    {$ENDIF}

    // write
    function Write(const But; Count: LongInt): LongWord; virtual;

    function WriteByte(const Value: Byte): LongWord; inline;          // UInt8
    function WriteWord(const Value: Word): LongWord; inline;          // UInt16
    function WriteLongWord(const Value: LongWord): LongWord; inline;  // UInt32
    function WriteShortInt(const Value: ShortInt): LongWord; inline;  // Int8
    function WriteSmallInt(const Value: SmallInt): LongWord; inline;  // Int16
    function WriteLongInt(const Value: LongInt): LongWord; inline;    // Int32
    function WriteSingle(const Value: Single): LongWord; inline;      // Float
    function WriteBoolean(const Value: Boolean): LongWord; inline;    // Boolean

    function WriteAnsiStr(const Value: AnsiString): LongWord; inline;
    function WriteWideStr(const Value: {$IFDEF VER200} UnicodeString {$ELSE} WideString {$ENDIF}): LongWord; inline;
    function WriteTextWide(const Value: string): LongWord;
    function WriteTextAnsi(const Value: AnsiString): LongWord;
    function WriteStr(const Value: string): LongWord; inline;
    {$IFDEF ICORE}
      function WriteV2f(const Value: TVec2f): LongWord; inline;
      function WriteV3f(const Value: TVec3f): LongWord; inline;
      function WriteV4f(const Value: TVec4f): LongWord; inline;
    {$ENDIF}

    //other
    function Seek(const Value: LongInt): LongWord; virtual; //offset

    property Valid: Boolean read FValid;
    property Size: LongInt read FSize;
    property Position: LongInt read FPos write SetPosition;
  end;
  { TODO -oStream -cIgroman : ???????? ?????-???????? TStream }
{$ENDREGION}

{$REGION 'TStreamFile = class}
  TStreamFile = class(TStream)
    constructor Create;
    destructor Destroy; override;
  protected
    procedure SetPosition(const Value: LongInt); override;
  private
    F: File;
    FStart: LongWord;
    RW: Boolean;
  public
    procedure Open(const FileName: string; const Rewrite: Boolean = False);
    procedure Close; inline;

    function Read(out Data; Count: LongInt): LongWord; override;
    function Write(const Data; Count: LongInt): LongWord; override;

    function Seek(const Value: LongInt): LongWord; override;
    procedure SetBlock(const Start, Size: LongWord); inline;
    {$IFDEF ICORE_CRC32}
    function GetCRC32: LongWord; inline;
    {$ENDIF}

    property Valid: Boolean read FValid;
    property Position: LongInt read FPos write SetPosition;
  end;
{$ENDREGION}

implementation
{$IFDEF ICORE_CRC32}
uses ICore.CRC32;
{$ENDIF}

{$REGION 'System'}

{$IFNDEF ICORE}
  function Max(const x, y: LongInt): LongInt; inline;
  begin
    Result := x;
    if x <= y then
     Result := y;
  end;
{$ENDIF}

{$ENDREGION}

{$REGION 'TStream'}
constructor TStream.Create;
begin
  inherited;
  FSize := 0;
  FPos := 0;
  FValid := False;
end;

procedure TStream.SetPosition(const Value: LongInt);
begin
  {$IFDEF ICORE}
  FPos := TMath.Max(0, Value);
  {$ELSE}
  FPos := Max(0, Value);
  {$ENDIF}
end;

function TStream.Read(out Buf; Count: LongInt): LongWord;
begin
  Result := 0;
end;

function TStream.ReadByte: Byte;
begin
  Read(Result, SizeOf(Result));
end;

function TStream.ReadWord: Word;
begin
  Read(Result, SizeOf(Result));
end;

function TStream.ReadLongWord: LongWord;
begin
  Read(Result, SizeOf(Result));
end;

function TStream.ReadShortInt: ShortInt;
begin
  Read(Result, SizeOf(Result));
end;

function TStream.ReadSmallInt: SmallInt;
begin
  Read(Result, SizeOf(Result));
end;

function TStream.ReadLongInt: LongInt;
begin
  Read(Result, SizeOf(Result));
end;

function TStream.ReadSingle: Single;
begin
  Read(Result, SizeOf(Result));
end;

function TStream.ReadBoolean: Boolean;
begin
  Read(Result, SizeOf(Result));
end;

function TStream.ReadAnsiStr: AnsiString;
var
  tmpLen: LongWord;
begin
  Result := '';
  Read(tmpLen, SizeOf(tmpLen));
  if (tmpLen > 0) then begin
    SetLength(Result, tmpLen);
    Read(Result[1], tmpLen * SizeOf(Result[1]));
  end;
end;

function TStream.ReadWideStr: WideString;
var
  tmpLen: LongWord;
begin
  Result := '';
  Read(tmpLen, SizeOf(tmpLen));
  if (tmpLen > 0) then begin
    SetLength(Result, tmpLen);
    Read(Result[1], tmpLen * SizeOf(Result[1]));
  end;
end;

function TStream.ReadStr: string;
//var
//  tmpstr: AnsiString;
//begin
//  if (Size > 0) then begin
//    SetLength(tmpstr, Self.Size);
//    Read(tmpstr[1], Self.Size);
//    Result := tmpstr;
//  end
//  else
//   Result := '';
begin
  Result := ReadWideStr;
end;

{$IFDEF ICORE}
function TStream.ReadV2f: TVec2f;
begin
  Read(Result, SizeOf(Result));
end;

function TStream.ReadV3f: TVec3f;
begin
  Read(Result, SizeOf(Result));
end;

function TStream.ReadV4f: TVec4f;
begin
  Read(Result, SizeOf(Result));
end;
{$ENDIF}

function TStream.Write(const But; Count: LongInt): LongWord;
begin
  Result := 0;
end;

function TStream.WriteByte(const Value: Byte): LongWord;
begin
  Result := Write(Value, SizeOf(Value));
end;

function TStream.WriteWord(const Value: Word): LongWord;
begin
  Result := Write(Value, SizeOf(Value));
end;

function TStream.WriteLongWord(const Value: Cardinal): LongWord;
begin
  Result := Write(Value, SizeOf(Value));
end;

function TStream.WriteShortInt(const Value: ShortInt): LongWord;
begin
  Result := Write(Value, SizeOf(Value));
end;

function TStream.WriteSmallInt(const Value: SmallInt): LongWord;
begin
  Result := Write(Value, SizeOf(Value));
end;

function TStream.WriteLongInt(const Value: Integer): LongWord;
begin
  Result := Write(Value, SizeOf(Value));
end;

function TStream.WriteSingle(const Value: Single): LongWord;
begin
  Result := Write(Value, SizeOf(Value));
end;

function TStream.WriteBoolean(const Value: Boolean): LongWord;
begin
  Result := Write(Value, SizeOf(Value));
end;

function TStream.WriteAnsiStr(const Value: AnsiString): LongWord;
var
  tmpLen: LongWord;
begin
  Result := 0;
  tmpLen := Length(Value);
  Write(tmpLen, SizeOf(tmpLen));
  if (tmpLen > 0) then
   Result := Write(Value[1], tmpLen * SizeOf(Value[1]));
end;

function TStream.WriteWideStr(const Value: WideString): LongWord;
var
  tmpLen: LongWord;
begin
  Result := 0;
  tmpLen := Length(Value);
  Write(tmpLen, SizeOf(tmpLen));
  if (tmpLen > 0) then
   Result := Write(Value[1], tmpLen * SizeOf(Value[1]));
end;

function TStream.WriteTextWide(const Value: string): LongWord;
begin
  Result := Write(Value[1], Length(Value));
end;

function TStream.WriteTextAnsi(const Value: AnsiString): LongWord;
begin
  Result := Write(Value[1], Length(Value));
end;

function TStream.WriteStr(const Value: string): LongWord;
begin
  Result := WriteWideStr(Value);
end;

{$IFDEF ICORE}
function TStream.WriteV2f(const Value: TVec2f): LongWord;
begin
  Result := Write(Value, SizeOf(Value));
end;

function TStream.WriteV3F(const Value: TVec3f): LongWord;
begin
  Result := Write(Value, SizeOf(Value));
end;

function TStream.WriteV4F(const Value: TVec4f): LongWord;
begin
  Result := Write(Value, SizeOf(Value));
end;
{$ENDIF}

function TStream.Seek(const Value: LongInt): LongWord;
begin
  FPos := FPos + Value;
  Result := FPos;
end;
{$ENDREGION}

{$REGION 'TStreamFile'}
constructor TStreamFile.Create;
begin
  inherited;
end;

destructor TStreamFile.Destroy;
begin
  Close;
  inherited;
end;

procedure TStreamFile.SetPosition(const Value: LongInt);
begin
  inherited SetPosition(Value);
  System.Seek(F, FPos);
end;

procedure TStreamFile.Open(const FileName: string; const Rewrite: Boolean = False);
begin
  Close;
//  if FileExists(FileName) then
//  F := FileOpen(TPChar(FileName), Rewrite);
  FileMode := 2;
  RW := Rewrite;
{$IFDEF UNICODE}
  AssignFile(F, FileName);
{$ELSE}
  AssignFile(F, string(FileName));
{$ENDIF}
{$I-}
  if Rewrite then begin
    FileMode := 1; //write-only
    System.Rewrite(F, 1);
  end else begin
    FileMode := 0;//read-only
    Reset(F, 1);
  end;
{$I+}
  if IOResult = 0 then begin
    FSize := System.FileSize(F);
    FValid := True;
  end;
end;

procedure TStreamFile.Close;
begin
  if Valid then
   System.CloseFile(F);
end;

function TStreamFile.Seek(const Value: LongInt): LongWord;
begin
//  FileSeek(F, Pos);
  Result := inherited Seek(Value);
  System.Seek(F, LongInt(inherited Seek(Position)));
end;

function TStreamFile.Read(out Data; Count: LongInt): LongWord;
begin
//  try
//    Result := FileRead(F, Data, Count);
//  except
//    Result := 0;
//  end;
  Result := 0;
  if not RW then
   BlockRead(F, Data, Count, Result);
  Inc(FPos, Result);
end;

function TStreamFile.Write(const Data; Count: LongInt): LongWord;
begin
//  try
//    Result := FileWrite(F, Data, Count);
//  except
//    Result := 0;
//  end;
  Result := 0;
  if RW then
   BlockWrite(F, Data, Count, Result);
  Inc(FSize, Result);
  {$IFDEF ICORE}
  Inc(FPos, TMath.Max(0, FPos - FSize));
  {$ELSE}
  Inc(FPos, Max(0, FPos - FSize));
  {$ENDIF}
end;

procedure TStreamFile.SetBlock(const Start: Cardinal; const Size: Cardinal);
begin
  FStart := Start;
  FPos := Size;
  Seek(0);
end;

{$IFDEF ICORE_CRC32}
function TStreamFile.GetCRC32: LongWord;
var
  Buf: Pointer;
begin
  Result := $FFFFFFFF;
  if Valid then begin
    Buf := GetMemory(Size);
    Self.Read(Buf^, Size);
    Result := TCRC32.GetCRC32Table($FFFFFFFF, Buf, Size);
    FreeMemory(Buf);
  end;
end;
{$ENDIF}

{$ENDREGION}

end.
