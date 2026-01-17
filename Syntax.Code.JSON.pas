unit Syntax.Code.JSON;

interface

uses
  System.SysUtils, Syntax.Code, System.Generics.Collections, FMX.TextLayout,
  FMX.Graphics, System.UITypes;

type
  TCodeSyntaxJson = class(TCodeSyntax)
  private
    FKeyWords: TKeyWords;
    FStringKey, FNumKey, FFieldKey: TKeyWord;
  public
    constructor Create(DefaultFont: TFont; DefaultColor: TAlphaColor); override;
    destructor Destroy; override;
    function GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>; override;
  end;

implementation

{ TCodeSyntaxJson }

constructor TCodeSyntaxJson.Create(DefaultFont: TFont; DefaultColor: TAlphaColor);
begin
  inherited;

  FStringKey := TKeyWord.Create;
  FStringKey.Color := $FFE7DB74;
  FStringKey.Font.Assign(FDefaultFont);

  FFieldKey := TKeyWord.Create;
  FFieldKey.Color := $FFFD7C22;
  FFieldKey.Font.Assign(FDefaultFont);

  FNumKey := TKeyWord.Create;
  FNumKey.Color := $FFAC80FF;
  FNumKey.Font.Assign(FDefaultFont);

  FKeyWords := TKeyWords.Create;

  var KeyWord := TKeyWord.Create;
  KeyWord.Word := ['true', 'false'];
  KeyWord.Color := $FFAC80FF;
  KeyWord.Font.Assign(FDefaultFont);
  FKeyWords.Add(KeyWord);
end;

destructor TCodeSyntaxJson.Destroy;
begin
  FStringKey.Free;
  FNumKey.Free;
  FFieldKey.Free;
  FKeyWords.Free;
  inherited;
end;

function TCodeSyntaxJson.GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>;
const
  Seps =[' ', ';', ')', '(', '[', ']', ':', '<', '>', ',', '+', '-', '=', '*', '/', '\'];
begin
  if FCached.TryGetValue(Index, Result) then
    Exit;
  try
    var Buf: string := '';
    var IsString: Boolean := False;
    var WasColon := False;
    for var C := 0 to Line.Length do
    begin
      if Line.IsEmpty then
        Continue;
      if Line.Chars[C] = ':' then
        WasColon := True;
      if IsString then
      begin
        if Line.Chars[C] = '"' then
        begin
          IsString := False;
          //if not Buf.IsEmpty then
          begin
            if WasColon then
              Result := Result + [
                TTextAttributedRangeData.Create(
                TTextRange.Create(C - Buf.Length - 1, Buf.Length + 2),
                TTextAttribute.Create(FStringKey.Font, FStringKey.Color)
                )]
            else
              Result := Result + [
                TTextAttributedRangeData.Create(
                TTextRange.Create(C - Buf.Length - 1, Buf.Length + 2),
                TTextAttribute.Create(FFieldKey.Font, FFieldKey.Color)
                )];
            WasColon := False;
            Buf := '';
          end;
          Continue;
        end;
        Buf := Buf + Line.Chars[C];
        Continue;
      end;
      if Line.Chars[C] = '"' then
      begin
        IsString := True;
        Buf := '';
        Continue;
      end;

      if (C = Line.Length) or CharInSet(Line.Chars[C], Seps) then
      begin
        if not CharInSet(Line.Chars[C], [' ', ':']) then
          WasColon := False;
        if not Buf.IsEmpty then
        begin
          var KeyWord: TKeyWord;
          var FL: Extended;
          if TryStrToFloat(Buf.Replace('.', FormatSettings.DecimalSeparator), FL) then
          begin
            Result := Result + [TTextAttributedRangeData.Create(
              TTextRange.Create(C - Buf.Length, Buf.Length),
              TTextAttribute.Create(FNumKey.Font, FNumKey.Color)
              )];
          end
          else if Buf.StartsWith('\') then
          begin
            Result := Result + [TTextAttributedRangeData.Create(
              TTextRange.Create(C - Buf.Length, Buf.Length),
              TTextAttribute.Create(FStringKey.Font, FStringKey.Color)
              )];
          end
          else if FKeyWords.FindWord(Buf, KeyWord) then
          begin
            Result := Result + [TTextAttributedRangeData.Create(
              TTextRange.Create(C - Buf.Length, Buf.Length),
              TTextAttribute.Create(KeyWord.Font, KeyWord.Color)
              )];
          end;

          Buf := '';
        end;
      end
      else
        Buf := Buf + Line.Chars[C];
    end;
  finally
    FCached.AddOrSetValue(Index, Result);
  end;
end;

initialization
  TCodeSyntax.RegisterSyntax(['json'], TCodeSyntaxJson);

end.

