unit FMX.RichEdit.Style;

interface

uses
  FMX.Text.TextEditor, FMX.Text.LinesLayout, FMX.TextLayout, FMX.Memo.Style.New,
  FMX.Controls.Presentation, FMX.Text, FMX.ScrollBox.Style, FMX.Controls,
  FMX.Graphics, System.UITypes, Syntax.Code, FMX.Presentation.Messages,
  System.Classes, System.Types, FMX.Types, FMX.Memo;

type
  TRichEditLinesLayout = class(TLinesLayout)
  private
    FCodeSyntax: TCodeSyntax;
  protected
    procedure UpdateLayoutParams(const ALineIndex: Integer; const ALayout: TTextLayout); override;
    procedure InvalidateLinesLayouts;
  public
    constructor Create(const ALineSource: ITextLinesSource; const AScrollableContent: IScrollableContent);
    destructor Destroy; override;

    procedure ReplaceLine(const AIndex: Integer; const ALine: string); override;
  public
    procedure SetCodeSyntaxName(const Lang: string; const DefFont: TFont; DefColor: TAlphaColor);
    procedure ClearCache;
  end;

  TRichEditTextEditor = class(TTextEditor)
  private
    FOnChangeCaretPos: TCaretPositionChanged;
    procedure SetOnChangeCaretPos(const Value: TCaretPositionChanged);
  protected
    procedure DoCaretPositionChanged; override;
    function CreateLinesLayout: TLinesLayout; override;
  public
    property OnChangeCaretPos: TCaretPositionChanged read FOnChangeCaretPos write SetOnChangeCaretPos;
  end;

  TRichEditStyled = class(TStyledMemo)
  private
    FWasInitialized: Boolean;
    FDragButton: TMouseButton;
    FDragShift: TShiftState;
    FDragX, FDragY: Single;
    FCancelDrag, FNeedDefClick, FDragging: Boolean;
    // FLinesBackgroundColor := TDictionary<Integer, TAlphaColor>.Create;
    FOnChangeCaretPos: TCaretPositionChanged;
    procedure DoChangeCaretPos(Sender: TObject; const ACaretPosition: TCaretPosition);
    procedure SetOnChangeCaretPos(const Value: TCaretPositionChanged);
  protected
    function CreateEditor: TTextEditor; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure DragEnd; override;
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure PMSetStyleLookup(var AMessage: TDispatchMessageWithValue<string>); message PM_SET_STYLE_LOOKUP;
    procedure MMLinesChanged(var Message: TDispatchMessage); message MM_MEMO_LINES_CHANGED;
    procedure PMInit(var Message: TDispatchMessage); message PM_INIT;
  public
    function GetSelectionRectTest: TRectF;
    procedure UpdateVisibleLayoutParams;
    procedure SetCodeSyntaxName(const Lang: string; const DefFont: TFont; DefColor: TAlphaColor);
    property OnChangeCaretPos: TCaretPositionChanged read FOnChangeCaretPos write SetOnChangeCaretPos;
  end;

implementation

uses
  System.SysUtils, FMX.Presentation.Style, FMX.Presentation.Factory,
  FMX.Platform, FMX.Forms;

{ TRichEditTextEditor }

function TRichEditTextEditor.CreateLinesLayout: TLinesLayout;
begin
  Result := TRichEditLinesLayout.Create(Lines, ScrollableContent);
end;

procedure TRichEditTextEditor.DoCaretPositionChanged;
begin
  inherited;
  if Assigned(FOnChangeCaretPos) then
    FOnChangeCaretPos(Self, CaretPosition);
end;

procedure TRichEditTextEditor.SetOnChangeCaretPos(const Value: TCaretPositionChanged);
begin
  FOnChangeCaretPos := Value;
end;

{ TRichEditLinesLayout }

procedure TRichEditLinesLayout.ClearCache;
begin
  if Assigned(FCodeSyntax) then
    FCodeSyntax.DropCache;
end;

constructor TRichEditLinesLayout.Create(const ALineSource: ITextLinesSource; const AScrollableContent: IScrollableContent);
begin
  inherited;
end;

destructor TRichEditLinesLayout.Destroy;
begin
  FreeAndNil(FCodeSyntax);
  inherited;
end;

procedure TRichEditLinesLayout.InvalidateLinesLayouts;
begin
  for var i := 0 to Count - 1 do
  begin
    Items[i].Invalidate;
    Items[i].RenderingMode := RenderingMode;
  end;
end;

procedure TRichEditLinesLayout.ReplaceLine(const AIndex: Integer; const ALine: string);
begin
  inherited;
  // We have to reapply style attributes after line modification
  if (AIndex >= 0) and (AIndex < Count) then // Guard against invalid indices (e.g. when lines are cleared)
    Items[AIndex].InvalidateLayout;
end;

procedure TRichEditLinesLayout.SetCodeSyntaxName(const Lang: string; const DefFont: TFont; DefColor: TAlphaColor);
begin
  if Assigned(FCodeSyntax) then
  begin
    FCodeSyntax.Free;
    FCodeSyntax := nil;
  end;
  FCodeSyntax := TCodeSyntax.FindSyntax(Lang, DefFont, DefColor);
  if not Assigned(FCodeSyntax) then
    FCodeSyntax := TCodeSyntax.FindSyntax('md', DefFont, DefColor);
  InvalidateLinesLayouts;
  Realign;
end;

procedure TRichEditLinesLayout.UpdateLayoutParams(const ALineIndex: Integer; const ALayout: TTextLayout);
begin
  if not Assigned(FCodeSyntax) then
    Exit;

  ALayout.BeginUpdate;
  try
    inherited;
    ALayout.ClearAttributes;
    for var Attr in FCodeSyntax.GetAttributesForLine(LinesSource[ALineIndex], ALineIndex) do
    begin
      Attr.Attribute.Font.Family := TextSettings.Font.Family;
      ALayout.AddAttribute(Attr.Range, Attr.Attribute);
    end;
  finally
    ALayout.EndUpdate;
  end;
end;

{ TRichEditStyled }

function TRichEditStyled.CreateEditor: TTextEditor;
begin
  Result := TRichEditTextEditor.Create(Self, Memo.Content, Model, Self);
  TRichEditTextEditor(Result).OnChangeCaretPos := DoChangeCaretPos;
end;

procedure TRichEditStyled.DoChangeCaretPos(Sender: TObject; const ACaretPosition: TCaretPosition);
begin
  if Assigned(FOnChangeCaretPos) then
    FOnChangeCaretPos(Self, ACaretPosition);
end;

procedure TRichEditStyled.DragEnd;
begin
  if FNeedDefClick then
  begin
    FDragging := False;
    FNeedDefClick := False;
    FCancelDrag := True;
    MouseDown(FDragButton, FDragShift, FDragX, FDragY);
    MouseUp(FDragButton, FDragShift, FDragX, FDragY);
    FCancelDrag := False;
  end;
  inherited;
end;

procedure TRichEditStyled.DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if FDragging and (Point <> TPointF.Create(FDragX, FDragY)) then
    FNeedDefClick := False;
  inherited;
end;

function TRichEditStyled.GetSelectionRectTest: TRectF;
begin
  Result := GetSelectionRect;
end;

procedure TRichEditStyled.MMLinesChanged(var Message: TDispatchMessage);
begin
  inherited;
  UpdateVisibleLayoutParams;
end;

procedure TRichEditStyled.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  // sorry dragdrop in progress
  {
  var R := GetSelectionRect;
  R.NormalizeRect;
  if (not FCancelDrag) and (not Self.GetSelection.IsEmpty) and R.Contains(TPointF.Create(X, Y)) then
  begin
    FNeedDefClick := True;
    FDragging := True;
    FDragButton := Button;
    FDragShift := Shift;
    FDragX := X;
    FDragY := Y;
    var D: TDragObject;
    var DDService: IFMXDragDropService;
    begin
      if TPlatformServices.Current.SupportsPlatformService(IFMXDragDropService, DDService) then
      begin
        Root.SetCaptured(Self.Memo);
        Fillchar(D, SizeOf(D), 0);
        D.Source := Self;
        D.Data := Self.GetSelection;
        var Bitmap := TBitmap.Create(30, 30);
        try
          DDService.BeginDragDrop(Application.MainForm, D, Bitmap);
        finally
          Bitmap.Free;
        end;
      end;
    end;
  end
  else     }
    inherited;
end;

procedure TRichEditStyled.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
end;

procedure TRichEditStyled.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
end;

procedure TRichEditStyled.PMInit(var Message: TDispatchMessage);
begin
  inherited;
  FWasInitialized := True;
  StyleLookup := Memo.StyleLookup;
end;

procedure TRichEditStyled.PMSetStyleLookup(var AMessage: TDispatchMessageWithValue<string>);
begin
  if FWasInitialized then
    inherited;
end;

procedure TRichEditStyled.SetCodeSyntaxName(const Lang: string; const DefFont: TFont; DefColor: TAlphaColor);
begin
  TRichEditLinesLayout(Editor.LinesLayout).SetCodeSyntaxName(Lang, DefFont, DefColor);
end;

procedure TRichEditStyled.SetOnChangeCaretPos(const Value: TCaretPositionChanged);
begin

end;

procedure TRichEditStyled.UpdateVisibleLayoutParams;
begin
  TRichEditLinesLayout(Editor.LinesLayout).ClearCache;
  var First := Editor.LinesLayout.FirstVisibleLineIndex;
  var Last := Editor.LinesLayout.LastVisibleLineIndex;
  if (First < 0) or (Last < 0) or (First > Last) then
    Exit;
  if Last >= Editor.LinesLayout.Count then
    Last := Editor.LinesLayout.Count - 1;

  for var i := First to Last do
  begin
    var Line := Editor.LinesLayout.Items[i];
    if Line.Layout <> nil then
      TRichEditLinesLayout(Editor.LinesLayout).UpdateLayoutParams(i, Line.Layout);
  end;
end;

initialization
  TPresentationProxyFactory.Current.Register('RichEditStyled', TStyledPresentationProxy<TRichEditStyled>);

finalization
  TPresentationProxyFactory.Current.Unregister('RichEditStyled', TStyledPresentationProxy<TRichEditStyled>);

end.

