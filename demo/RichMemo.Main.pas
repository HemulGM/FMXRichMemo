unit RichMemo.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.RichEdit.Style,
  FMX.TabControl, FMX.Objects, FMX.Filter.Effects, FMX.StdCtrls;

type
  TFormMain = class(TForm)
    MemoPascal: TMemo;
    TabControlMain: TTabControl;
    TabItemPascal: TTabItem;
    TabItemJSON: TTabItem;
    TabItemSQL: TTabItem;
    StyleBookWinUI3: TStyleBook;
    MemoJSON: TMemo;
    TabItemMD: TTabItem;
    MemoSQL: TMemo;
    MemoMD: TMemo;
    TabItemPython: TTabItem;
    MemoPython: TMemo;
    TabItemHTML: TTabItem;
    MemoHTML: TMemo;
    TabItemCSS: TTabItem;
    MemoCSS: TMemo;
    procedure MemoPascalPresentationNameChoosing(Sender: TObject; var PresenterName: string);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  FMX.BehaviorManager;

{$R *.fmx}

procedure TFormMain.MemoPascalPresentationNameChoosing(Sender: TObject; var PresenterName: string);
begin
  // The choice of the presentation class by the control
  PresenterName := 'RichEditStyled';
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  MemoPascal.ScrollAnimation := TBehaviorBoolean.True;
  MemoJSON.ScrollAnimation := TBehaviorBoolean.True;
  MemoSQL.ScrollAnimation := TBehaviorBoolean.True;
  MemoMD.ScrollAnimation := TBehaviorBoolean.True;
  MemoPython.ScrollAnimation := TBehaviorBoolean.True;
  MemoHTML.ScrollAnimation := TBehaviorBoolean.True;
  MemoCSS.ScrollAnimation := TBehaviorBoolean.True;

  // Setting the default syntax and fonts
  if MemoPascal.Presentation is TRichEditStyled then
    TRichEditStyled(MemoPascal.Presentation).SetCodeSyntaxName('pascal', MemoPascal.Font, MemoPascal.FontColor);

  if MemoJSON.Presentation is TRichEditStyled then
    TRichEditStyled(MemoJSON.Presentation).SetCodeSyntaxName('json', MemoJSON.Font, MemoJSON.FontColor);

  if MemoSQL.Presentation is TRichEditStyled then
    TRichEditStyled(MemoSQL.Presentation).SetCodeSyntaxName('sql', MemoSQL.Font, MemoSQL.FontColor);

  if MemoMD.Presentation is TRichEditStyled then
    TRichEditStyled(MemoMD.Presentation).SetCodeSyntaxName('md', MemoMD.Font, MemoMD.FontColor);

  if MemoPython.Presentation is TRichEditStyled then
    TRichEditStyled(MemoPython.Presentation).SetCodeSyntaxName('python', MemoPython.Font, MemoPython.FontColor);

  if MemoHTML.Presentation is TRichEditStyled then
    TRichEditStyled(MemoHTML.Presentation).SetCodeSyntaxName('html', MemoHTML.Font, MemoHTML.FontColor);

  if MemoCSS.Presentation is TRichEditStyled then
    TRichEditStyled(MemoCSS.Presentation).SetCodeSyntaxName('css', MemoCSS.Font, MemoCSS.FontColor);
end;

end.

