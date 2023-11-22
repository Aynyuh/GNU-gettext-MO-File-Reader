unit GNUMOFileReader;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults;

type
  { char *gettext (const char *msgid) }
  TGetTextProc = function(const MsgId: string): string of object;

  { char *ngettext (const char *msgid1, const char *msgid2, unsigned long int n) }
  TNGetTextProc = function(const MsgId1, MsgId2: string; N: Integer): string of object;

  /// <summary>
  /// Handler for plural form detection. See PluralRU in this module for example.
  ///
  /// https://www.gnu.org/software/gettext/manual/html_node/Plural-forms.html
  /// </summary>
  TPluralHandler = reference to function(N: Integer): Integer;

  EGNUGettext = class(Exception);
  EGGIOError = class(EGNUGettext);

  TGNUMOFileReader = class sealed
  private type
    { Metadata headers dictionary }
    TMetadataDict = TDictionary<string, string>;

    TPluralPair = TPair<string, Integer>;
    /// <summary>
    /// Catalog with msgid and its translation. The key is TPluralPair to support plural forms.
    /// By default non plural msgid is TPluralPair<msgid, -1>
    /// Plural forms stored as TPluralPair<msgid, plural>
    /// </summary>
    TStringsPair = TDictionary<TPluralPair, string>;

    { Header for binary .mo file format }
    TMOFileHeader = record
      { The magic number }
      Magic: Cardinal;
      { The revision number of the file format }
      Revision: Cardinal;
      { The number of strings pairs }
      NStrings: Cardinal;
      { Offset of table with start offsets of original strings }
      OrigTabOffset: Cardinal;
      { Offset of table with start offsets of translated strings }
      TransTabOffset: Cardinal;

      { Size of hash table }
      // HashTabSize: Cardinal;
      { Offset of first hash table entry }
      // HashTabOffset: Cardinal;
    end;

    // to be able locate TPluralPair key in TDictionary
    TPluralPairComparer = class(TEqualityComparer<TPluralPair>)
    public
      function Equals(const Left: TPluralPair; const Right: TPluralPair): Boolean; override;
      function GetHashCode(const Value: TPluralPair): Integer; override;
    end;

  private
  var
    FHeader: TMOFileHeader;
    FFileName: string;
    FBuffer: TArray<Byte>;
    FBufLen: NativeInt;
    FNeedSwap: Boolean;
    FInfo: TMetadataDict;
    FCatalog: TStringsPair;
    // encoding used for reading metadata before charset is retrieved from headers
    FDefaultEnc: TEncoding;
    // encoding determined from Context-Type field of metadata
    FStringsEncoding: TEncoding;
    FEncodingIsSet: Boolean;
    FGetText: TGetTextProc;
    FNGetText: TNGetTextProc;
    FComparer: IEqualityComparer<TPluralPair>;
    FPluralHandler: TPluralHandler;

    procedure EnsureSignature;
    procedure EnsureRevision;
    procedure ReadBinaryFile;
    procedure ReadHeader;
    procedure ReadStrings;
    procedure ReadMetadata(const ABuf: TBytes);
  public
    constructor Create(const AMOFileName: string);
    destructor Destroy; override;

    procedure Parse;

    function GetKeys: TStringsPair.TKeyCollection;
    function GetValues: TStringsPair.TValueCollection;

    /// <summary>
    /// This encoding is used for parsing metadata.
    /// The strings encoding will be determined later based on metadata Content-Type field.
    /// </summary>
    property DefaultEncoding: TEncoding read FDefaultEnc write FDefaultEnc;
    property Info: TMetadataDict read FInfo;

    function GetText(const MsgId: string): string;
    function NGetText(const MsgId1, MsgId2: string; N: Integer): string;

    property Items[const MsgId: string]: string read GetText; default;
    property Plurals[const MsgId1, MsgId2: string; N: Integer]: string read NGetText;

    /// <summary>
    /// Pointer to GetText method usually used for translations.
    /// e.g. var _: TGetTextProc; _('Text to translate');
    /// </summary>
    property Get_gettext: TGetTextProc read FGetText;
    property Get_ngettext: TNGetTextProc read FNGetText;

    /// <summary>
    /// Handler used to evaluate plural form
    /// </summary>
    property PluralHandler: TPluralHandler read FPluralHandler write FPluralHandler;
  end;

function PluralEN(N: Integer): Integer;
function PluralRU(N: Integer): Integer;

implementation

type

  Consts = record
  type
    Offset = record
    public const
      SIGNATURE = 0;
      VERSION = 4;
      MSG_COUNT = 8;
      TEXTS = 12;
      TRANSLATION = 16;
    end;

    Str = record
    public const
      S_VER_NOT_SUPPORTED = 'The version %d is not supported.';
      S_NOT_VALID_FILE = 'File "%s" is not in GNU .mo format.';
    end;

  public const
    MinVersion = [0, 1];
    LE_MAGIC = $950412DE; // little-endian magic number
    BE_MAGIC = $DE120495; // big-endian magic number
  end;

  /// <summary>
  /// Plural-Forms: nplurals=2; plural=n != 1;
  ///
  /// See: https://www.gnu.org/software/gettext/manual/html_node/Plural-forms.html
  /// </summary>
function PluralEN(N: Integer): Integer;
begin
  Result := Integer(Abs(N) <> 1);
end;

/// <summary>
/// Plural-Forms: nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;
///
/// See: https://www.gnu.org/software/gettext/manual/html_node/Plural-forms.html
/// </summary>
function PluralRU(N: Integer): Integer;
var
  n1, n2: Integer;
begin
  N := Abs(N);

  n1 := N mod 10;
  n2 := N mod 100;

  if (n1 = 1) and (n2 <> 11) then
    Result := 0
  else if (n1 >= 2) and (n1 <= 4) and ((n2 < 10) or (n2 >= 20)) then
    Result := 1
  else
    Result := 2;
end;

function GetDW(ABuffer: TBytes; Offset: Cardinal): Cardinal; inline;
begin
  Result := PCardinal(PByte(@ABuffer[0]) + Offset)^;
end;

function GetBuff(ABuffer: TBytes; AOffset: Cardinal; ALength: Integer): TBytes;
var
  PByteDst: PByte;
  PByteSrc: PByte;
begin
  if ALength < 1 then
    Exit;

  SetLength(Result, ALength);

  PByteSrc := PByte(@ABuffer[0]) + AOffset;
  PByteDst := @Result[0];

  // CopyMemory(@Result[0], PByte(@ABuffer[0]) + AOffset, ALength);

  Move(PByteSrc^, PByteDst^, ALength);
end;

{ TGNUMOFileReader }

constructor TGNUMOFileReader.Create(const AMOFileName: string);
begin
  FFileName := AMOFileName;

  // key case insensetive dictionary
  FInfo := TMetadataDict.Create(TDelegatedEqualityComparer<string>.Create(
    function(const Left, Right: string): Boolean
    begin
      Result := CompareText(Left, Right) = 0;
    end,
    function(const Value: string): Integer
    begin
      Result := Value.ToLower.GetHashCode;
    end));

  FComparer := TPluralPairComparer.Create;
  FCatalog := TStringsPair.Create(FComparer);

  FDefaultEnc := TEncoding.Default;
  FStringsEncoding := TEncoding.Default;

  FGetText := GetText;
  FNGetText := NGetText;

  // default handler for English language
  // FPluralHandler := PluralEN;
  FPluralHandler := function(N: Integer): Integer
    begin
      Result := Integer(Abs(N) <> 1);
    end;
end;

destructor TGNUMOFileReader.Destroy;
begin
  FInfo.Free;
  FCatalog.Free;
  FDefaultEnc := nil;

  if (FEncodingIsSet) then
    FStringsEncoding.Free;

  inherited;
end;

procedure TGNUMOFileReader.EnsureRevision;
begin
  if not(FHeader.Revision shr 16 in Consts.MinVersion) then
    raise EGNUGettext.CreateFmt(Consts.Str.S_VER_NOT_SUPPORTED, [FHeader.Revision shr 16]);
end;

procedure TGNUMOFileReader.EnsureSignature;
begin
  if (FHeader.Magic <> Consts.LE_MAGIC) and (FHeader.Magic <> Consts.BE_MAGIC) then
    raise EGGIOError.CreateFmt(Consts.Str.S_NOT_VALID_FILE, [FFileName]);
end;

function TGNUMOFileReader.GetKeys: TStringsPair.TKeyCollection;
begin
  Result := FCatalog.Keys;
end;

function TGNUMOFileReader.GetText(const MsgId: string): string;
var
  p: TPluralPair;
begin
  Result := MsgId;
  p := TPluralPair.Create(MsgId, -1);
  if FCatalog.ContainsKey(p) then
    Result := FCatalog[p];
end;

function TGNUMOFileReader.GetValues: TStringsPair.TValueCollection;
begin
  Result := FCatalog.Values;
end;

function TGNUMOFileReader.NGetText(const MsgId1, MsgId2: string; N: Integer): string;
var
  p: TPluralPair;
begin
  p := TPluralPair.Create(MsgId1, FPluralHandler(N));

  if FCatalog.ContainsKey(p) then
    Result := FCatalog[p]
  else if N = 1 then
    Result := MsgId1
  else
    Result := MsgId2;
end;

procedure TGNUMOFileReader.Parse;
begin
  ReadBinaryFile();
  ReadHeader();
  EnsureSignature();
  Assert(not(FHeader.Magic = Consts.BE_MAGIC), 'Big-endian is not supported.');
  EnsureRevision();
  ReadStrings();

  // we don't need buffer anymore
  SetLength(FBuffer, 0);
end;

procedure TGNUMOFileReader.ReadHeader;
begin
  FHeader.Magic := GetDW(FBuffer, Consts.Offset.SIGNATURE);
  FHeader.Revision := GetDW(FBuffer, Consts.Offset.VERSION);
  FHeader.NStrings := GetDW(FBuffer, Consts.Offset.MSG_COUNT);
  FHeader.OrigTabOffset := GetDW(FBuffer, Consts.Offset.TEXTS);
  FHeader.TransTabOffset := GetDW(FBuffer, Consts.Offset.TRANSLATION);
end;

procedure TGNUMOFileReader.ReadBinaryFile;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(FFileName, fmOpenRead, fmShareDenyWrite);
  try
    FBufLen := stream.Size;
    SetLength(FBuffer, FBufLen);
    stream.ReadBuffer(FBuffer, FBufLen);
  finally
    stream.Free;
  end;
end;

procedure TGNUMOFileReader.ReadMetadata(const ABuf: TBytes);
var
  msgs, line, lastk, k, v: string;
begin
  msgs := FDefaultEnc.GetString(ABuf);

  for var l in msgs.Split([AnsiChar(#10)]) do
  begin
    line := l.Trim();

    if line = '' then
      Continue;

    // skipping comments
    if line.StartsWith('#-#-#-#-#') and line.EndsWith('#-#-#-#-#') then
      Continue;

    if line.IndexOf(':') > -1 then
    begin
      var _ := line.Split([':'], 2);
      k := LowerCase(_[0]).Trim();
      v := _[1].Trim();

      FInfo.Add(k, v);
      lastk := k;

    end
    else if lastk <> '' then
      FInfo[lastk] := #10 + FInfo[lastk] + line;

    if k = 'content-type' then
    begin
      var _ := v.Split(['charset='], 2);
      if High(_) = 1 then
      begin
        FStringsEncoding := TEncoding.GetEncoding(_[1]);
        FEncodingIsSet := True;
      end;
    end
    else if (k = 'plural-forms') then
    begin
      // raise EGNUGettext.Create('Plural forms format is not supported yet.');
    end;
  end;
end;

procedure TGNUMOFileReader.ReadStrings;
var
  mlen, moff, tlen, toff, msgCount, textsOffset, transOffset: Cardinal;
  MsgId, msg: TBytes;
  sMsgId: string;
  AMsgs: TArray<string>;
begin
  msgCount := FHeader.NStrings;
  textsOffset := FHeader.OrigTabOffset;
  transOffset := FHeader.TransTabOffset;

  for var i := 0 to msgCount - 1 do
  begin
    mlen := GetDW(FBuffer, textsOffset); // O - length of first string
    moff := GetDW(FBuffer, textsOffset + 4); // offset of first string
    tlen := GetDW(FBuffer, transOffset); // T - length of first translation
    toff := GetDW(FBuffer, transOffset + 4); // offset of first translation

    if (moff + mlen < FBufLen) and (toff + tlen < FBufLen) then
    begin
      MsgId := GetBuff(FBuffer, moff, mlen);
      msg := GetBuff(FBuffer, toff, tlen);

      if mlen = 0 then
        ReadMetadata(msg);

      sMsgId := FStringsEncoding.GetString(MsgId);

      // if contains plural forms
      if sMsgId.IndexOf(#0) > -1 then
      begin
        sMsgId := sMsgId.Split([#0], 2)[0];
        AMsgs := FStringsEncoding.GetString(msg).Split([#0], 2);

        for var j := 0 to High(AMsgs) do
          FCatalog.Add(TPluralPair.Create(sMsgId, j), AMsgs[j]);
      end
      else
        FCatalog.Add(TPluralPair.Create(sMsgId, -1), FStringsEncoding.GetString(msg));

      Inc(textsOffset, 8);
      Inc(transOffset, 8);
    end;
  end;
end;

{ TGNUMOFileReader.TPluralPairComparer }

function TGNUMOFileReader.TPluralPairComparer.Equals(const Left, Right: TPluralPair): Boolean;
begin
  Result := (Left.Key = Right.Key) and (Left.Value = Right.Value);
end;

function TGNUMOFileReader.TPluralPairComparer.GetHashCode(const Value: TPluralPair): Integer;
begin
  Result := Value.Key.GetHashCode;
end;

end.
