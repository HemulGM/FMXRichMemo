unit RichMemo.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.RichEdit.Style,
  FMX.TabControl, FMX.Objects, FMX.Filter.Effects, FMX.StdCtrls, FMX.Layouts,
  FMX.Edit, FMX.EditBox, FMX.SpinBox, FMX.Colors;

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
    StyleBook1: TStyleBook;
    CheckBoxRoundedSelection: TCheckBox;
    CheckBoxSelectedTextColor: TCheckBox;
    ComboColorBoxSelectedText: TComboColorBox;
    CheckBoxWordHighlight: TCheckBox;
    CheckBoxDrawBefore: TCheckBox;
    CheckBoxDrawAfter: TCheckBox;
    CheckBoxLineBG: TCheckBox;
    CheckBoxGutterNumberAllLines: TCheckBox;
    procedure CheckBoxCurrentLineChange(Sender: TObject);
    procedure CheckBoxErrorLineChange(Sender: TObject);
    procedure CheckBoxGutterChange(Sender: TObject);
    procedure MemoPascalPresentationNameChoosing(Sender: TObject; var PresenterName: string);
    procedure FormCreate(Sender: TObject);
    procedure SpinBoxLineSpacingChange(Sender: TObject);
    procedure CheckBoxRoundedSelectionChange(Sender: TObject);
    procedure ComboColorBoxSelectedTextChange(Sender: TObject);
    procedure CheckBoxSelectedTextColorChange(Sender: TObject);
    procedure CheckBoxWordHighlightChange(Sender: TObject);
    procedure CheckBoxDrawBeforeChange(Sender: TObject);
    procedure CheckBoxLineBGChange(Sender: TObject);
    procedure CheckBoxGutterNumberAllLinesChange(Sender: TObject);
  private
    procedure FOnMemoDrawAfter(Sender: TObject; ACanvas: TCanvas);
    procedure FOnMemoDrawBefore(Sender: TObject; ACanvas: TCanvas);
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

procedure TFormMain.CheckBoxDrawBeforeChange(Sender: TObject);
begin
  MemoPascal.Repaint;
  MemoJSON.Repaint;
  MemoSQL.Repaint;
  MemoMD.Repaint;
  MemoPython.Repaint;
  MemoHTML.Repaint;
  MemoCSS.Repaint;
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

procedure TFormMain.CheckBoxGutterNumberAllLinesChange(Sender: TObject);
begin
  TRichEditStyled(MemoPascal.Presentation).GutterNumberAllLines := CheckBoxGutterNumberAllLines.IsChecked;
  TRichEditStyled(MemoJSON.Presentation).GutterNumberAllLines := CheckBoxGutterNumberAllLines.IsChecked;
  TRichEditStyled(MemoSQL.Presentation).GutterNumberAllLines := CheckBoxGutterNumberAllLines.IsChecked;
  TRichEditStyled(MemoMD.Presentation).GutterNumberAllLines := CheckBoxGutterNumberAllLines.IsChecked;
  TRichEditStyled(MemoPython.Presentation).GutterNumberAllLines := CheckBoxGutterNumberAllLines.IsChecked;
  TRichEditStyled(MemoHTML.Presentation).GutterNumberAllLines := CheckBoxGutterNumberAllLines.IsChecked;
  TRichEditStyled(MemoCSS.Presentation).GutterNumberAllLines := CheckBoxGutterNumberAllLines.IsChecked;
end;

procedure TFormMain.CheckBoxLineBGChange(Sender: TObject);
begin
  TRichEditStyled(MemoPascal.Presentation).ShowLinesBackgroundColor := CheckBoxLineBG.IsChecked;
  TRichEditStyled(MemoJSON.Presentation).ShowLinesBackgroundColor := CheckBoxLineBG.IsChecked;
  TRichEditStyled(MemoSQL.Presentation).ShowLinesBackgroundColor := CheckBoxLineBG.IsChecked;
  TRichEditStyled(MemoMD.Presentation).ShowLinesBackgroundColor := CheckBoxLineBG.IsChecked;
  TRichEditStyled(MemoPython.Presentation).ShowLinesBackgroundColor := CheckBoxLineBG.IsChecked;
  TRichEditStyled(MemoHTML.Presentation).ShowLinesBackgroundColor := CheckBoxLineBG.IsChecked;
  TRichEditStyled(MemoCSS.Presentation).ShowLinesBackgroundColor := CheckBoxLineBG.IsChecked;
end;

procedure TFormMain.CheckBoxRoundedSelectionChange(Sender: TObject);
begin
  TRichEditStyled(MemoPascal.Presentation).RoundedSelection := CheckBoxRoundedSelection.IsChecked;
  TRichEditStyled(MemoJSON.Presentation).RoundedSelection := CheckBoxRoundedSelection.IsChecked;
  TRichEditStyled(MemoSQL.Presentation).RoundedSelection := CheckBoxRoundedSelection.IsChecked;
  TRichEditStyled(MemoMD.Presentation).RoundedSelection := CheckBoxRoundedSelection.IsChecked;
  TRichEditStyled(MemoPython.Presentation).RoundedSelection := CheckBoxRoundedSelection.IsChecked;
  TRichEditStyled(MemoHTML.Presentation).RoundedSelection := CheckBoxRoundedSelection.IsChecked;
  TRichEditStyled(MemoCSS.Presentation).RoundedSelection := CheckBoxRoundedSelection.IsChecked;
end;

procedure TFormMain.CheckBoxSelectedTextColorChange(Sender: TObject);
begin
  TRichEditStyled(MemoPascal.Presentation).UseSelectedTextColor := CheckBoxSelectedTextColor.IsChecked;
  TRichEditStyled(MemoJSON.Presentation).UseSelectedTextColor := CheckBoxSelectedTextColor.IsChecked;
  TRichEditStyled(MemoSQL.Presentation).UseSelectedTextColor := CheckBoxSelectedTextColor.IsChecked;
  TRichEditStyled(MemoMD.Presentation).UseSelectedTextColor := CheckBoxSelectedTextColor.IsChecked;
  TRichEditStyled(MemoPython.Presentation).UseSelectedTextColor := CheckBoxSelectedTextColor.IsChecked;
  TRichEditStyled(MemoHTML.Presentation).UseSelectedTextColor := CheckBoxSelectedTextColor.IsChecked;
  TRichEditStyled(MemoCSS.Presentation).UseSelectedTextColor := CheckBoxSelectedTextColor.IsChecked;
end;

procedure TFormMain.CheckBoxWordHighlightChange(Sender: TObject);
begin
  TRichEditStyled(MemoPascal.Presentation).ShowWordHighLight := CheckBoxWordHighlight.IsChecked;
  TRichEditStyled(MemoJSON.Presentation).ShowWordHighLight := CheckBoxWordHighlight.IsChecked;
  TRichEditStyled(MemoSQL.Presentation).ShowWordHighLight := CheckBoxWordHighlight.IsChecked;
  TRichEditStyled(MemoMD.Presentation).ShowWordHighLight := CheckBoxWordHighlight.IsChecked;
  TRichEditStyled(MemoPython.Presentation).ShowWordHighLight := CheckBoxWordHighlight.IsChecked;
  TRichEditStyled(MemoHTML.Presentation).ShowWordHighLight := CheckBoxWordHighlight.IsChecked;
  TRichEditStyled(MemoCSS.Presentation).ShowWordHighLight := CheckBoxWordHighlight.IsChecked;
end;

procedure TFormMain.ComboColorBoxSelectedTextChange(Sender: TObject);
begin
  TRichEditStyled(MemoPascal.Presentation).SelectedTextColor := ComboColorBoxSelectedText.Color;
  TRichEditStyled(MemoJSON.Presentation).SelectedTextColor := ComboColorBoxSelectedText.Color;
  TRichEditStyled(MemoSQL.Presentation).SelectedTextColor := ComboColorBoxSelectedText.Color;
  TRichEditStyled(MemoMD.Presentation).SelectedTextColor := ComboColorBoxSelectedText.Color;
  TRichEditStyled(MemoPython.Presentation).SelectedTextColor := ComboColorBoxSelectedText.Color;
  TRichEditStyled(MemoHTML.Presentation).SelectedTextColor := ComboColorBoxSelectedText.Color;
  TRichEditStyled(MemoCSS.Presentation).SelectedTextColor := ComboColorBoxSelectedText.Color;
end;

procedure TFormMain.MemoPascalPresentationNameChoosing(Sender: TObject; var PresenterName: string);
begin
  // The choice of the presentation class by the control
  PresenterName := 'RichEditStyled';
end;

procedure TFormMain.FOnMemoDrawBefore(Sender: TObject; ACanvas: TCanvas);
begin
  if not CheckBoxDrawBefore.IsChecked then
    Exit;
  ACanvas.Fill.Kind := TBrushKind.Solid;
  ACanvas.Fill.Color := TAlphaColors.Green;
  ACanvas.FillEllipse(TRectF.Create(140, 140, 240, 240), 1);
end;

procedure TFormMain.FOnMemoDrawAfter(Sender: TObject; ACanvas: TCanvas);
begin
  if not CheckBoxDrawAfter.IsChecked then
    Exit;
  ACanvas.Fill.Kind := TBrushKind.Solid;
  ACanvas.Fill.Color := TAlphaColors.Dimgrey;
  ACanvas.FillEllipse(TRectF.Create(40, 40, 140, 140), 1);
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

    var Stroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Red);
    Stroke.Thickness := 2;
    Stroke.Dash := TStrokeDash.Dot;
    TRichEditStyled(MemoPascal.Presentation).WordHighlight.Add(
        TTextRange.Create(1, 40),
        Stroke
      );
    TRichEditStyled(MemoPascal.Presentation).OnDrawBefore := FOnMemoDrawBefore;
    TRichEditStyled(MemoPascal.Presentation).OnDrawAfter := FOnMemoDrawAfter;

    var Brush := TBrush.Create(TBrushKind.Gradient, TAlphaColors.Red);
    Brush.Gradient.Color := $99FF0000;
    Brush.Gradient.Color1 := $00FF0000;
    Brush.Gradient.StartPosition.Point := TPointF.Create(0.5, 0.5);
    Brush.Gradient.StopPosition.Point := TPointF.Create(1, 0.5);
    TRichEditStyled(MemoPascal.Presentation).LinesBackgroundColor.Add(44, Brush);
  end;

  if MemoJSON.Presentation is TRichEditStyled then
  begin
    TRichEditStyled(MemoJSON.Presentation).SetCodeSyntaxName('json', MemoJSON.Font, MemoJSON.FontColor);
    TRichEditStyled(MemoJSON.Presentation).ErrorLine := 10;

    var Stroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Red);
    Stroke.Thickness := 2;
    Stroke.Dash := TStrokeDash.Dot;
    TRichEditStyled(MemoJSON.Presentation).WordHighlight.Add(
        TTextRange.Create(1, 40),
        Stroke
      );
  end;

  if MemoSQL.Presentation is TRichEditStyled then
  begin
    TRichEditStyled(MemoSQL.Presentation).SetCodeSyntaxName('sql', MemoSQL.Font, MemoSQL.FontColor);
    TRichEditStyled(MemoSQL.Presentation).ErrorLine := 10;

    var Stroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Red);
    Stroke.Thickness := 2;
    Stroke.Dash := TStrokeDash.Dot;
    TRichEditStyled(MemoSQL.Presentation).WordHighlight.Add(
        TTextRange.Create(1, 40),
        Stroke
      );
  end;

  if MemoMD.Presentation is TRichEditStyled then
  begin
    TRichEditStyled(MemoMD.Presentation).SetCodeSyntaxName('md', MemoMD.Font, MemoMD.FontColor);
    TRichEditStyled(MemoMD.Presentation).ErrorLine := 10;

    var Stroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Red);
    Stroke.Thickness := 2;
    Stroke.Dash := TStrokeDash.Dot;
    TRichEditStyled(MemoMD.Presentation).WordHighlight.Add(
        TTextRange.Create(1, 40),
        Stroke
      );
  end;

  if MemoPython.Presentation is TRichEditStyled then
  begin
    TRichEditStyled(MemoPython.Presentation).SetCodeSyntaxName('python', MemoPython.Font, MemoPython.FontColor);
    TRichEditStyled(MemoPython.Presentation).ErrorLine := 10;

    var Stroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Red);
    Stroke.Thickness := 2;
    Stroke.Dash := TStrokeDash.Dot;
    TRichEditStyled(MemoPython.Presentation).WordHighlight.Add(
        TTextRange.Create(1, 40),
        Stroke
      );
  end;

  if MemoHTML.Presentation is TRichEditStyled then
  begin
    TRichEditStyled(MemoHTML.Presentation).SetCodeSyntaxName('html', MemoHTML.Font, MemoHTML.FontColor);
    TRichEditStyled(MemoHTML.Presentation).ErrorLine := 10;

    var Stroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Red);
    Stroke.Thickness := 2;
    Stroke.Dash := TStrokeDash.Dot;
    TRichEditStyled(MemoHTML.Presentation).WordHighlight.Add(
        TTextRange.Create(1, 40),
        Stroke
      );
  end;

  if MemoCSS.Presentation is TRichEditStyled then
  begin
    TRichEditStyled(MemoCSS.Presentation).SetCodeSyntaxName('css', MemoCSS.Font, MemoCSS.FontColor);
    TRichEditStyled(MemoCSS.Presentation).ErrorLine := 10;

    var Stroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Red);
    Stroke.Thickness := 2;
    Stroke.Dash := TStrokeDash.Dot;
    TRichEditStyled(MemoCSS.Presentation).WordHighlight.Add(
        TTextRange.Create(1, 40),
        Stroke
      );
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

