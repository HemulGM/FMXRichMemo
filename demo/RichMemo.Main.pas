unit RichMemo.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.RichEdit.Style,
  FMX.TabControl, FMX.Objects, FMX.Filter.Effects, FMX.StdCtrls, FMX.Layouts,
  FMX.Edit, FMX.EditBox, FMX.SpinBox;

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
    Layout1: TLayout;
    CheckBoxGutter: TCheckBox;
    Label1: TLabel;
    CheckBoxCurrentLine: TCheckBox;
    CheckBoxErrorLine: TCheckBox;
    SpinBoxLineSpacing: TSpinBox;
    Label2: TLabel;
    procedure CheckBoxCurrentLineChange(Sender: TObject);
    procedure CheckBoxErrorLineChange(Sender: TObject);
    procedure CheckBoxGutterChange(Sender: TObject);
    procedure MemoPascalPresentationNameChoosing(Sender: TObject; var PresenterName: string);
    procedure FormCreate(Sender: TObject);
    procedure SpinBoxLineSpacingChange(Sender: TObject);
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

procedure TFormMain.CheckBoxCurrentLineChange(Sender: TObject);
begin
  TRichEditStyled(MemoPascal.Presentation).ShowCurrentLine := CheckBoxCurrentLine.IsChecked;
  TRichEditStyled(MemoJSON.Presentation).ShowCurrentLine := CheckBoxCurrentLine.IsChecked;
  TRichEditStyled(MemoSQL.Presentation).ShowCurrentLine := CheckBoxCurrentLine.IsChecked;
  TRichEditStyled(MemoMD.Presentation).ShowCurrentLine := CheckBoxCurrentLine.IsChecked;
  TRichEditStyled(MemoPython.Presentation).ShowCurrentLine := CheckBoxCurrentLine.IsChecked;
  TRichEditStyled(MemoHTML.Presentation).ShowCurrentLine := CheckBoxCurrentLine.IsChecked;
  TRichEditStyled(MemoCSS.Presentation).ShowCurrentLine := CheckBoxCurrentLine.IsChecked;
end;

procedure TFormMain.CheckBoxErrorLineChange(Sender: TObject);
begin
  TRichEditStyled(MemoPascal.Presentation).ShowError := CheckBoxErrorLine.IsChecked;
  TRichEditStyled(MemoJSON.Presentation).ShowError := CheckBoxErrorLine.IsChecked;
  TRichEditStyled(MemoSQL.Presentation).ShowError := CheckBoxErrorLine.IsChecked;
  TRichEditStyled(MemoMD.Presentation).ShowError := CheckBoxErrorLine.IsChecked;
  TRichEditStyled(MemoPython.Presentation).ShowError := CheckBoxErrorLine.IsChecked;
  TRichEditStyled(MemoHTML.Presentation).ShowError := CheckBoxErrorLine.IsChecked;
  TRichEditStyled(MemoCSS.Presentation).ShowError := CheckBoxErrorLine.IsChecked;
end;

procedure TFormMain.CheckBoxGutterChange(Sender: TObject);
begin
  TRichEditStyled(MemoPascal.Presentation).ShowGutter := CheckBoxGutter.IsChecked;
  TRichEditStyled(MemoJSON.Presentation).ShowGutter := CheckBoxGutter.IsChecked;
  TRichEditStyled(MemoSQL.Presentation).ShowGutter := CheckBoxGutter.IsChecked;
  TRichEditStyled(MemoMD.Presentation).ShowGutter := CheckBoxGutter.IsChecked;
  TRichEditStyled(MemoPython.Presentation).ShowGutter := CheckBoxGutter.IsChecked;
  TRichEditStyled(MemoHTML.Presentation).ShowGutter := CheckBoxGutter.IsChecked;
  TRichEditStyled(MemoCSS.Presentation).ShowGutter := CheckBoxGutter.IsChecked;
end;

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
  begin
    TRichEditStyled(MemoPascal.Presentation).SetCodeSyntaxName('pascal', MemoPascal.Font, MemoPascal.FontColor);
    TRichEditStyled(MemoPascal.Presentation).ErrorLine := 10;
  end;

  if MemoJSON.Presentation is TRichEditStyled then
  begin
    TRichEditStyled(MemoJSON.Presentation).SetCodeSyntaxName('json', MemoJSON.Font, MemoJSON.FontColor);
    TRichEditStyled(MemoJSON.Presentation).ErrorLine := 10;
  end;

  if MemoSQL.Presentation is TRichEditStyled then
  begin
    TRichEditStyled(MemoSQL.Presentation).SetCodeSyntaxName('sql', MemoSQL.Font, MemoSQL.FontColor);
    TRichEditStyled(MemoSQL.Presentation).ErrorLine := 10;
  end;

  if MemoMD.Presentation is TRichEditStyled then
  begin
    TRichEditStyled(MemoMD.Presentation).SetCodeSyntaxName('md', MemoMD.Font, MemoMD.FontColor);
    TRichEditStyled(MemoMD.Presentation).ErrorLine := 10;
  end;

  if MemoPython.Presentation is TRichEditStyled then
  begin
    TRichEditStyled(MemoPython.Presentation).SetCodeSyntaxName('python', MemoPython.Font, MemoPython.FontColor);
    TRichEditStyled(MemoPython.Presentation).ErrorLine := 10;
  end;

  if MemoHTML.Presentation is TRichEditStyled then
  begin
    TRichEditStyled(MemoHTML.Presentation).SetCodeSyntaxName('html', MemoHTML.Font, MemoHTML.FontColor);
    TRichEditStyled(MemoHTML.Presentation).ErrorLine := 10;
  end;

  if MemoCSS.Presentation is TRichEditStyled then
  begin
    TRichEditStyled(MemoCSS.Presentation).SetCodeSyntaxName('css', MemoCSS.Font, MemoCSS.FontColor);
    TRichEditStyled(MemoCSS.Presentation).ErrorLine := 10;
  end;
end;

procedure TFormMain.SpinBoxLineSpacingChange(Sender: TObject);
begin
  TRichEditStyled(MemoPascal.Presentation).LineSpacing := SpinBoxLineSpacing.Value;
  TRichEditStyled(MemoJSON.Presentation).LineSpacing := SpinBoxLineSpacing.Value;
  TRichEditStyled(MemoSQL.Presentation).LineSpacing := SpinBoxLineSpacing.Value;
  TRichEditStyled(MemoMD.Presentation).LineSpacing := SpinBoxLineSpacing.Value;
  TRichEditStyled(MemoPython.Presentation).LineSpacing := SpinBoxLineSpacing.Value;
  TRichEditStyled(MemoHTML.Presentation).LineSpacing := SpinBoxLineSpacing.Value;
  TRichEditStyled(MemoCSS.Presentation).LineSpacing := SpinBoxLineSpacing.Value;

end;

end.

