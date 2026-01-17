unit Syntax.Code.CSS;

interface

uses
  System.SysUtils, System.StrUtils, Syntax.Code, FMX.TextLayout, FMX.Graphics,
  System.UITypes;

type
  TCodeSyntaxCSS = class(TCodeSyntax)
  private
    FAtRuleKey: TKeyWord;
    FPropertyKey: TKeyWord;
    FStringKey: TKeyWord;
    FCommentKey: TKeyWord;
    FNumberKey: TKeyWord;
    FHexColorKey: TKeyWord;
    FFunctionKey: TKeyWord;
    FImportantKey: TKeyWord;
    FVarKey: TKeyWord;
    FAttrKey: TKeyWord;
    FPseudoKey: TKeyWord;
    FUrlKey: TKeyWord;
    FSelectorKey: TKeyWord;
    FClassKey: TKeyWord;
    FIdKey: TKeyWord;
    FCombinatorKey: TKeyWord;
    FBraceKey: TKeyWord;
    FCommaKey: TKeyWord;
    FMediaFeatureKey: TKeyWord;
    FAtRuleNameKey: TKeyWord;
  public
    constructor Create(DefaultFont: TFont; DefaultColor: TAlphaColor); override;
    destructor Destroy; override;
    function GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>; override;
  end;

implementation

type
  TSimpleRange = record
    Start: Integer;
    Len: Integer;
    function Contains(const Index: Integer): Boolean;
    function EndIndex: Integer;
  end;

function TSimpleRange.Contains(const Index: Integer): Boolean;
begin
  Result := (Index >= Start) and (Index < Start + Len);
end;

function TSimpleRange.EndIndex: Integer;
begin
  Result := Start + Len;
end;

function IsWS(const C: Char): Boolean;
begin
  Result := CharInSet(C, [#9, #10, #13, ' ']);
end;

function IsIdentStart(const C: Char): Boolean;
begin
  Result := CharInSet(C, ['a'..'z', 'A'..'Z', '_', '-']);
end;

function IsIdentChar(const C: Char): Boolean;
begin
  Result := CharInSet(C, ['a'..'z', 'A'..'Z', '0'..'9', '_', '-']);
end;

function IsHexDigit(const C: Char): Boolean;
begin
  Result := CharInSet(C, ['0'..'9', 'a'..'f', 'A'..'F']);
end;

{ TCodeSyntaxCSS }

constructor TCodeSyntaxCSS.Create(DefaultFont: TFont; DefaultColor: TAlphaColor);
begin
  inherited;

  FAtRuleKey := TKeyWord.Create;
  FAtRuleKey.Color := $FF7CC3FF;
  FAtRuleKey.Font.Assign(FDefaultFont);
  FAtRuleKey.Font.Style := [TFontStyle.fsBold];

  FPropertyKey := TKeyWord.Create;
  FPropertyKey.Color := $FFFFD8A8;
  FPropertyKey.Font.Assign(FDefaultFont);

  FStringKey := TKeyWord.Create;
  FStringKey.Color := $FF8CE8FF;
  FStringKey.Font.Assign(FDefaultFont);

  FCommentKey := TKeyWord.Create;
  FCommentKey.Color := $FF88E775;
  FCommentKey.Font.Assign(FDefaultFont);

  FNumberKey := TKeyWord.Create;
  FNumberKey.Color := $FFAC80FF;
  FNumberKey.Font.Assign(FDefaultFont);

  FHexColorKey := TKeyWord.Create;
  FHexColorKey.Color := $FFF1C974;
  FHexColorKey.Font.Assign(FDefaultFont);

  FFunctionKey := TKeyWord.Create;
  FFunctionKey.Color := $FFDCBDFB;
  FFunctionKey.Font.Assign(FDefaultFont);

  FImportantKey := TKeyWord.Create;
  FImportantKey.Color := $FFFF5A3C;
  FImportantKey.Font.Assign(FDefaultFont);
  FImportantKey.Font.Style := [TFontStyle.fsBold];

  FVarKey := TKeyWord.Create;
  FVarKey.Color := $FF9EEB90;
  FVarKey.Font.Assign(FDefaultFont);

  FAttrKey := TKeyWord.Create;
  FAttrKey.Color := $FFD8E8FF;
  FAttrKey.Font.Assign(FDefaultFont);

  FPseudoKey := TKeyWord.Create;
  FPseudoKey.Color := $FFF2C6FF; 
  FPseudoKey.Font.Assign(FDefaultFont);

  FUrlKey := TKeyWord.Create;
  FUrlKey.Color := $FF9FDFFF; 
  FUrlKey.Font.Assign(FDefaultFont);

  FSelectorKey := TKeyWord.Create;
  FSelectorKey.Color := $FFEEF6FF;
  FSelectorKey.Font.Assign(FDefaultFont);

  FClassKey := TKeyWord.Create;
  FClassKey.Color := $FFFFD8A8; 
  FClassKey.Font.Assign(FDefaultFont);

  FIdKey := TKeyWord.Create;
  FIdKey.Color := $FFBFD8FF; 
  FIdKey.Font.Assign(FDefaultFont);

  FCombinatorKey := TKeyWord.Create;
  FCombinatorKey.Color := $FF9EA7B8; 
  FCombinatorKey.Font.Assign(FDefaultFont);

  FBraceKey := TKeyWord.Create;
  FBraceKey.Color := $FF8F9AB0;
  FBraceKey.Font.Assign(FDefaultFont);

  FCommaKey := TKeyWord.Create;
  FCommaKey.Color := $FF8F9AB0;
  FCommaKey.Font.Assign(FDefaultFont);

  FMediaFeatureKey := TKeyWord.Create;
  FMediaFeatureKey.Color := $FFBEE3FF; 
  FMediaFeatureKey.Font.Assign(FDefaultFont);

  FAtRuleNameKey := TKeyWord.Create;
  FAtRuleNameKey.Color := $FF7CC3FF; 
  FAtRuleNameKey.Font.Assign(FDefaultFont);
end;

destructor TCodeSyntaxCSS.Destroy;
begin
  FImportantKey.Free;
  FFunctionKey.Free;
  FHexColorKey.Free;
  FNumberKey.Free;
  FCommentKey.Free;
  FStringKey.Free;
  FPropertyKey.Free;
  FAtRuleKey.Free;
  FVarKey.Free;
  FAttrKey.Free;
  FPseudoKey.Free;
  FUrlKey.Free;
  FSelectorKey.Free;
  FClassKey.Free;
  FIdKey.Free;
  FCombinatorKey.Free;
  FBraceKey.Free;
  FCommaKey.Free;
  FMediaFeatureKey.Free;
  FAtRuleNameKey.Free;
  inherited;
end;

function TCodeSyntaxCSS.GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>;
  procedure AddRange(const AStart, ALen: Integer; const Key: TKeyWord);
  begin
    if (ALen <= 0) or (AStart < 0) then
      Exit;
    Result := Result + [
      TTextAttributedRangeData.Create(
        TTextRange.Create(AStart, ALen),
        TTextAttribute.Create(Key.Font, Key.Color)
      )
    ];
  end;

  function IsInsideExcluded(const Excluded: TArray<TSimpleRange>; const Pos0: Integer): Boolean;
  begin
    for var R in Excluded do
      if R.Contains(Pos0) then
        Exit(True);
    Result := False;
  end;

  function SkipExcluded(const Excluded: TArray<TSimpleRange>; const Pos0: Integer): Integer;
  begin
    for var R in Excluded do
      if R.Contains(Pos0) then
        Exit(R.EndIndex);
    Result := Pos0;
  end;

  procedure AddExcludedRange(var Excluded: TArray<TSimpleRange>; const AStart, ALen: Integer);
  begin
    if (ALen <= 0) or (AStart < 0) then
      Exit;
    var R: TSimpleRange;
    R.Start := AStart;
    R.Len := ALen;
    Excluded := Excluded + [R];
  end;

begin
  if FCached.TryGetValue(Index, Result) then
    Exit;

  try
    var Excluded: TArray<TSimpleRange> := [];

    // 1) Single-line strings and comments (/* ... */). Multi-line comments are partially supported
    //    (start-to-end-of-line if no closing */ found on the same line).
    var I := 0;
    var InString := False;
    var Quote: Char := #0;
    var TokenStart := -1;
    var Escaped := False;
    var InComment := False;

    while I < Line.Length do
    begin
      var Ch := Line.Chars[I];

      if InComment then
      begin
        if (Ch = '*') and (I + 1 < Line.Length) and (Line.Chars[I + 1] = '/') then
        begin
          AddRange(TokenStart, (I + 2) - TokenStart, FCommentKey);
          AddExcludedRange(Excluded, TokenStart, (I + 2) - TokenStart);
          InComment := False;
          TokenStart := -1;
          Inc(I, 2);
          Continue;
        end;
        Inc(I);
        Continue;
      end;

      if InString then
      begin
        if Escaped then
        begin
          Escaped := False;
          Inc(I);
          Continue;
        end;

        if Ch = '\\' then
        begin
          Escaped := True;
          Inc(I);
          Continue;
        end;

        if Ch = Quote then
        begin
          AddRange(TokenStart, (I + 1) - TokenStart, FStringKey);
          AddExcludedRange(Excluded, TokenStart, (I + 1) - TokenStart);
          InString := False;
          Quote := #0;
          TokenStart := -1;
          Inc(I);
          Continue;
        end;

        Inc(I);
        Continue;
      end;

      // Not in string/comment
      if (Ch = '/') and (I + 1 < Line.Length) and (Line.Chars[I + 1] = '*') then
      begin
        InComment := True;
        TokenStart := I;
        Inc(I, 2);
        Continue;
      end;

      if CharInSet(Ch, [#39, #34]) then
      begin
        InString := True;
        Quote := Ch;
        TokenStart := I;
        Escaped := False;
        Inc(I);
        Continue;
      end;

      Inc(I);
    end;

    if InComment and (TokenStart >= 0) then
    begin
      AddRange(TokenStart, Line.Length - TokenStart, FCommentKey);
      AddExcludedRange(Excluded, TokenStart, Line.Length - TokenStart);
    end;

    if InString and (TokenStart >= 0) then
    begin
      AddRange(TokenStart, Line.Length - TokenStart, FStringKey);
      AddExcludedRange(Excluded, TokenStart, Line.Length - TokenStart);
    end;

    // 2) At-rules: @media, @import, @supports, etc.
    I := 0;
    while I < Line.Length do
    begin
      if IsInsideExcluded(Excluded, I) then
      begin
        I := SkipExcluded(Excluded, I);
        Continue;
      end;

      if Line.Chars[I] = '@' then
      begin
        var J := I + 1;
        if (J < Line.Length) and IsIdentStart(Line.Chars[J]) then
        begin
          Inc(J);
          while (J < Line.Length) and IsIdentChar(Line.Chars[J]) do
            Inc(J);
          AddRange(I, J - I, FAtRuleKey);
          I := J;
          Continue;
        end;
      end;

      Inc(I);
    end;

    // 3) !important
    var Lower := Line.ToLower;
    var SearchFrom := 1;
    while True do
    begin
      var P := PosEx('!important', Lower, SearchFrom);
      if P = 0 then
        Break;
      var Start0 := P - 1;
      if not IsInsideExcluded(Excluded, Start0) then
        AddRange(Start0, 10, FImportantKey);
      SearchFrom := P + 10;
      if SearchFrom > Line.Length then
        Break;
    end;

    // 4) Hex colors: #RGB/#RGBA/#RRGGBB/#RRGGBBAA
    I := 0;
    while I < Line.Length do
    begin
      if IsInsideExcluded(Excluded, I) then
      begin
        I := SkipExcluded(Excluded, I);
        Continue;
      end;

      if Line.Chars[I] = '#' then
      begin
        var J := I + 1;
        while (J < Line.Length) and IsHexDigit(Line.Chars[J]) and (J - (I + 1) < 8) do
          Inc(J);
        var DigitCount := J - (I + 1);
        if (DigitCount = 3) or (DigitCount = 4) or (DigitCount = 6) or (DigitCount = 8) then
          AddRange(I, 1 + DigitCount, FHexColorKey);
        I := J;
        Continue;
      end;

      Inc(I);
    end;

    // 5) Property names: <name> : <value>
    I := 0;
    while I < Line.Length do
    begin
      if IsInsideExcluded(Excluded, I) then
      begin
        I := SkipExcluded(Excluded, I);
        Continue;
      end;

      if Line.Chars[I] = ':' then
      begin
        var K := I - 1;
        while (K >= 0) and IsWS(Line.Chars[K]) do
          Dec(K);
        var EndName := K;
        while (K >= 0) and IsIdentChar(Line.Chars[K]) do
          Dec(K);
        var StartName := K + 1;
        if (StartName <= EndName) and (StartName >= 0) then
          AddRange(StartName, EndName - StartName + 1, FPropertyKey);
      end;

      Inc(I);
    end;

    // 6) Function names: ident(
    I := 0;
    while I < Line.Length do
    begin
      if IsInsideExcluded(Excluded, I) then
      begin
        I := SkipExcluded(Excluded, I);
        Continue;
      end;

      if Line.Chars[I] = '(' then
      begin
        var K := I - 1;
        while (K >= 0) and IsWS(Line.Chars[K]) do
          Dec(K);
        var EndName := K;
        while (K >= 0) and IsIdentChar(Line.Chars[K]) do
          Dec(K);
        var StartName := K + 1;
        if (StartName <= EndName) and (StartName >= 0) then
          AddRange(StartName, EndName - StartName + 1, FFunctionKey);
      end;

      Inc(I);
    end;

    // 7) Numbers (with optional unit)
    I := 0;
    while I < Line.Length do
    begin
      if IsInsideExcluded(Excluded, I) then
      begin
        I := SkipExcluded(Excluded, I);
        Continue;
      end;

      var Ch := Line.Chars[I];
      var IsStart := CharInSet(Ch, ['0'..'9']) or ((Ch = '.') and (I + 1 < Line.Length) and CharInSet(Line.Chars[I + 1], ['0'..'9']))
        or (CharInSet(Ch, ['+', '-']) and (I + 1 < Line.Length) and (CharInSet(Line.Chars[I + 1], ['0'..'9']) or ((Line.Chars[I + 1] = '.') and (I + 2 < Line.Length) and CharInSet(Line.Chars[I + 2], ['0'..'9']))));

      if not IsStart then
      begin
        Inc(I);
        Continue;
      end;

      var J := I;
      if CharInSet(Line.Chars[J], ['+', '-']) then
        Inc(J);

      while (J < Line.Length) and CharInSet(Line.Chars[J], ['0'..'9']) do
        Inc(J);

      if (J < Line.Length) and (Line.Chars[J] = '.') then
      begin
        Inc(J);
        while (J < Line.Length) and CharInSet(Line.Chars[J], ['0'..'9']) do
          Inc(J);
      end;

      // unit: letters or %
      while (J < Line.Length) and (CharInSet(Line.Chars[J], ['a'..'z', 'A'..'Z', '%'])) do
        Inc(J);

      AddRange(I, J - I, FNumberKey);
      I := J;
    end;

    // 8) CSS variables usages: --var-name
    I := 0;
    while I < Line.Length do
    begin
      if IsInsideExcluded(Excluded, I) then
      begin
        I := SkipExcluded(Excluded, I);
        Continue;
      end;

      if (Line.Chars[I] = '-') and (I + 1 < Line.Length) and (Line.Chars[I + 1] = '-') then
      begin
        var J := I + 2;
        while (J < Line.Length) and IsIdentChar(Line.Chars[J]) do
          Inc(J);
        AddRange(I, J - I, FVarKey);
        AddExcludedRange(Excluded, I, J - I);
        I := J;
        Continue;
      end;

      Inc(I);
    end;

    // 9) Attribute selectors: [attr=value]
    I := 0;
    while I < Line.Length do
    begin
      if IsInsideExcluded(Excluded, I) then
      begin
        I := SkipExcluded(Excluded, I);
        Continue;
      end;

      if Line.Chars[I] = '[' then
      begin
        var J := I + 1;
        while (J < Line.Length) and (Line.Chars[J] <> ']') do
          Inc(J);
        if J < Line.Length then
        begin
          AddRange(I, (J - I) + 1, FAttrKey);
          AddExcludedRange(Excluded, I, (J - I) + 1);
          I := J + 1;
          Continue;
        end
        else
        begin
          AddRange(I, Line.Length - I, FAttrKey);
          AddExcludedRange(Excluded, I, Line.Length - I);
          Break;
        end;
      end;

      Inc(I);
    end;

    // 10) Pseudo-classes/elements: :hover, ::after
    I := 0;
    while I < Line.Length do
    begin
      if IsInsideExcluded(Excluded, I) then
      begin
        I := SkipExcluded(Excluded, I);
        Continue;
      end;

      if Line.Chars[I] = ':' then
      begin
        var J := I;
        if (J + 1 < Line.Length) and (Line.Chars[J + 1] = ':') then
          Inc(J);
        Inc(J);
        while (J < Line.Length) and IsIdentChar(Line.Chars[J]) do
          Inc(J);
        AddRange(I, J - I, FPseudoKey);
        AddExcludedRange(Excluded, I, J - I);
        I := J;
        Continue;
      end;

      Inc(I);
    end;

    // 11) url(...) contents
    var LowerLine := Line.ToLower;
    var UrlSearchFrom := 1;
    while True do
    begin
      var P := PosEx('url(', LowerLine, UrlSearchFrom);
      if P = 0 then
        Break;
      var Start0 := P - 1; // 0-based
      var ContentStart := Start0 + Length('url(');
      var K := ContentStart;
      while (K < Line.Length) and (Line.Chars[K] <> ')') do
        Inc(K);
      if K < Line.Length then
      begin
        if ContentStart < K then
        begin
          AddRange(ContentStart, K - ContentStart, FUrlKey);
          AddExcludedRange(Excluded, ContentStart, K - ContentStart);
        end;
        UrlSearchFrom := P + Length('url(');
      end
      else
      begin
        // highlight till line end
        if ContentStart < Line.Length then
        begin
          AddRange(ContentStart, Line.Length - ContentStart, FUrlKey);
          AddExcludedRange(Excluded, ContentStart, Line.Length - ContentStart);
        end;
        Break;
      end;
    end;

    // 12) Class selectors (.class) and ID selectors (#id)
    I := 0;
    while I < Line.Length do
    begin
      if IsInsideExcluded(Excluded, I) then
      begin
        I := SkipExcluded(Excluded, I);
        Continue;
      end;

      var Ch2 := Line.Chars[I];
      if (Ch2 = '.') or (Ch2 = '#') then
      begin
        var J := I + 1;
        // allow escaped or hyphen in class/id names via IsIdentChar
        while (J < Line.Length) and IsIdentChar(Line.Chars[J]) do
          Inc(J);
        if J > I + 1 then
        begin
          if Ch2 = '.' then
            AddRange(I, J - I, FClassKey)
          else
            AddRange(I, J - I, FIdKey);
          AddExcludedRange(Excluded, I, J - I);
          I := J;
          Continue;
        end;
      end;

      Inc(I);
    end;

    // 13) Combinators (> + ~)
    I := 0;
    while I < Line.Length do
    begin
      if IsInsideExcluded(Excluded, I) then
      begin
        I := SkipExcluded(Excluded, I);
        Continue;
      end;
      if CharInSet(Line.Chars[I], ['>', '+', '~']) then
      begin
        AddRange(I, 1, FCombinatorKey);
        AddExcludedRange(Excluded, I, 1);
      end;
      Inc(I);
    end;

    // 14) Braces, parentheses, commas, semicolons
    I := 0;
    while I < Line.Length do
    begin
      if IsInsideExcluded(Excluded, I) then
      begin
        I := SkipExcluded(Excluded, I);
        Continue;
      end;
      var Cpun := Line.Chars[I];
      if CharInSet(Cpun, ['{', '}', '(', ')']) then
      begin
        AddRange(I, 1, FBraceKey);
        AddExcludedRange(Excluded, I, 1);
      end
      else if Cpun = ',' then
      begin
        AddRange(I, 1, FCommaKey);
        AddExcludedRange(Excluded, I, 1);
      end
      else if Cpun = ';' then
      begin
        AddRange(I, 1, FCommaKey);
        AddExcludedRange(Excluded, I, 1);
      end;
      Inc(I);
    end;

    // 15) Heuristic: Type selectors (only on lines without declarations ':')
    if Pos(':', Line) = 0 then
    begin
      I := 0;
      while I < Line.Length do
      begin
        if IsInsideExcluded(Excluded, I) then
        begin
          I := SkipExcluded(Excluded, I);
          Continue;
        end;

        if IsIdentStart(Line.Chars[I]) then
        begin
          // ensure not preceded by . # : [ or alphanumeric (part of other token)
          var PN := I - 1;
          while (PN >= 0) and IsWS(Line.Chars[PN]) do
            Dec(PN);
          if (PN >= 0) and CharInSet(Line.Chars[PN], ['.', '#', ':', '[']) then
          begin
            Inc(I);
            Continue;
          end;

          var J := I;
          while (J < Line.Length) and IsIdentChar(Line.Chars[J]) do
            Inc(J);
          if J > I then
          begin
            AddRange(I, J - I, FSelectorKey);
            AddExcludedRange(Excluded, I, J - I);
            I := J;
            Continue;
          end;
        end;

        Inc(I);
      end;
    end;
  finally
    FCached.AddOrSetValue(Index, Result);
  end;
end;

initialization
  TCodeSyntax.RegisterSyntax(['css', 'text/css'], TCodeSyntaxCSS);

end.
