unit SpellChecker;

interface

uses
  System.SysUtils, FMX.SpellChecker;

type
  TWinSpellCheckerService = class(TInterfacedObject, IFMXSpellCheckerService)
    ///<summary>
    ///  Check for word spelling.
    ///</summary>
    function CheckSpelling(Word: string): TArray<string>;
    ///<summary>
    ///  Check for word completion.
    ///</summary>
    function GuessCompletions(Word: string): TArray<string>;
  end;

implementation

uses
  FMX.Platform;

{ TWinSpellCheckerService }

function TWinSpellCheckerService.CheckSpelling(Word: string): TArray<string>;
begin
  Result := [];
  if Word.ToLower = 'begin' then
  begin
    Result := ['beging'];
  end;
end;

function TWinSpellCheckerService.GuessCompletions(Word: string): TArray<string>;
begin
  Result := [];
  if Word.ToLower = 'begin' then
  begin
    Result := ['beging'];
  end;
end;

initialization
begin
  {$IF CompilerVersion >= 37.0}
    // Delphi 13 and newer already register IFMXSpellCheckerService
    // Remove existing before adding your own
    if TPlatformServices.Current.SupportsPlatformService(IFMXSpellCheckerService) then
      TPlatformServices.Current.RemovePlatformService(IFMXSpellCheckerService);
  {$ENDIF}

  TPlatformServices.Current.AddPlatformService(IFMXSpellCheckerService, TWinSpellCheckerService.Create);
end;

end.

