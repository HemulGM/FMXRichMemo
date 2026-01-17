unit Syntax.Code.HTML;

interface

uses
  System.SysUtils, System.StrUtils, Syntax.Code, FMX.TextLayout, FMX.Graphics,
  System.UITypes;

type
  TCodeSyntaxHTML = class(TCodeSyntax)
  private
    FTagKey: TKeyWord;
    FAttrKey: TKeyWord;
    FStringKey: TKeyWord;
    FCommentKey: TKeyWord;
    FEntityKey: TKeyWord;
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

function IsNameChar(const C: Char): Boolean;
begin
  Result := CharInSet(C, ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', ':', '.']);
end;

{ TCodeSyntaxHTML }

constructor TCodeSyntaxHTML.Create(DefaultFont: TFont; DefaultColor: TAlphaColor);
begin
  inherited;

  FTagKey := TKeyWord.Create;
  FTagKey.Color := $FF638FCF;
  FTagKey.Font.Assign(FDefaultFont);
  FTagKey.Font.Style := [TFontStyle.fsBold];

  FAttrKey := TKeyWord.Create;
  FAttrKey.Color := $FFFFE0BC;
  FAttrKey.Font.Assign(FDefaultFont);

  FStringKey := TKeyWord.Create;
  FStringKey.Color := $FF87D0FF;
  FStringKey.Font.Assign(FDefaultFont);

  FCommentKey := TKeyWord.Create;
  FCommentKey.Color := $FF88E775;
  FCommentKey.Font.Assign(FDefaultFont);

  FEntityKey := TKeyWord.Create;
  FEntityKey.Color := $FFAC80FF;
  FEntityKey.Font.Assign(FDefaultFont);
end;

destructor TCodeSyntaxHTML.Destroy;
begin
  FEntityKey.Free;
  FCommentKey.Free;
  FStringKey.Free;
  FAttrKey.Free;
  FTagKey.Free;
  inherited;
end;

function TCodeSyntaxHTML.GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>;
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

  function FindCharOutsideExcluded(const Excluded: TArray<TSimpleRange>; const Ch: Char; const FromPos0: Integer): Integer;
  begin
    Result := -1;
    for var P := FromPos0 to Line.Length - 1 do
    begin
      if IsInsideExcluded(Excluded, P) then
        Continue;
      if Line.Chars[P] = Ch then
        Exit(P);
    end;
  end;

begin
  if FCached.TryGetValue(Index, Result) then
    Exit;

  try
    var Excluded: TArray<TSimpleRange> := [];

    // Highlight single-line HTML comments: <!-- ... -->
    var SearchFrom := 1;
    while True do
    begin
      var PStart1 := PosEx('<!--', Line, SearchFrom);
      if PStart1 = 0 then
        Break;

      var PEnd1 := PosEx('-->', Line, PStart1 + 4);
      var Start0 := PStart1 - 1;
      var End0: Integer;
      if PEnd1 = 0 then
        End0 := Line.Length
      else
        End0 := (PEnd1 - 1) + 3;

      var R: TSimpleRange;
      R.Start := Start0;
      R.Len := End0 - Start0;
      Excluded := Excluded + [R];
      AddRange(R.Start, R.Len, FCommentKey);

      SearchFrom := End0 + 1;
      if SearchFrom > Line.Length then
        Break;
    end;

    // Highlight entities: &nbsp; &amp; etc.
    var I := 0;
    while I < Line.Length do
    begin
      if IsInsideExcluded(Excluded, I) then
      begin
        I := SkipExcluded(Excluded, I);
        Continue;
      end;

      if Line.Chars[I] = '&' then
      begin
        var Semi := FindCharOutsideExcluded(Excluded, ';', I + 1);
        if (Semi <> -1) and (Semi > I) then
        begin
          AddRange(I, Semi - I + 1, FEntityKey);
          I := Semi + 1;
          Continue;
        end;
      end;

      Inc(I);
    end;

    // Highlight tags, attribute names, and attribute values.
    I := 0;
    while I < Line.Length do
    begin
      if IsInsideExcluded(Excluded, I) then
      begin
        I := SkipExcluded(Excluded, I);
        Continue;
      end;

      if Line.Chars[I] <> '<' then
      begin
        Inc(I);
        Continue;
      end;

      // Skip comments already handled
      if (I + 3 < Line.Length) and (Line.Substring(I, 4) = '<!--') then
      begin
        Inc(I);
        Continue;
      end;

      var ClosePos := FindCharOutsideExcluded(Excluded, '>', I + 1);
      if ClosePos = -1 then
        Break;

      // Colorize the whole tag region (including < and >)
      AddRange(I, ClosePos - I + 1, FTagKey);

      var J := I + 1;
      while (J < ClosePos) and IsWS(Line.Chars[J]) do
        Inc(J);

      if (J < ClosePos) and (Line.Chars[J] = '/') then
      begin
        Inc(J);
        while (J < ClosePos) and IsWS(Line.Chars[J]) do
          Inc(J);
      end;

      if (J < ClosePos) and CharInSet(Line.Chars[J], ['!', '?']) then
      begin
        Inc(J);
        while (J < ClosePos) and IsWS(Line.Chars[J]) do
          Inc(J);
      end;

      var TagNameStart := J;
      while (J < ClosePos) and IsNameChar(Line.Chars[J]) do
        Inc(J);
      AddRange(TagNameStart, J - TagNameStart, FTagKey);

      // Attributes
      while J < ClosePos do
      begin
        while (J < ClosePos) and (IsWS(Line.Chars[J]) or (Line.Chars[J] = '/')) do
          Inc(J);
        if J >= ClosePos then
          Break;

        var AttrStart := J;
        while (J < ClosePos) and IsNameChar(Line.Chars[J]) do
          Inc(J);
        AddRange(AttrStart, J - AttrStart, FAttrKey);

        while (J < ClosePos) and IsWS(Line.Chars[J]) do
          Inc(J);

        if (J < ClosePos) and (Line.Chars[J] = '=') then
        begin
          Inc(J);
          while (J < ClosePos) and IsWS(Line.Chars[J]) do
            Inc(J);

          if J >= ClosePos then
            Break;

          if CharInSet(Line.Chars[J], ['"', '''']) then
          begin
            var Quote := Line.Chars[J];
            var ValStart := J;
            Inc(J);
            while (J < ClosePos) and (Line.Chars[J] <> Quote) do
              Inc(J);
            if (J < ClosePos) and (Line.Chars[J] = Quote) then
              Inc(J);
            AddRange(ValStart, J - ValStart, FStringKey);
          end
          else
          begin
            var ValStart := J;
            while (J < ClosePos) and (not IsWS(Line.Chars[J])) and (Line.Chars[J] <> '>') do
              Inc(J);
            AddRange(ValStart, J - ValStart, FStringKey);
          end;
        end;
      end;

      I := ClosePos + 1;
    end;

  finally
    FCached.AddOrSetValue(Index, Result);
  end;
end;

initialization
  TCodeSyntax.RegisterSyntax(['html', 'htm', 'xhtml'], TCodeSyntaxHTML);

end.
