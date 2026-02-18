unit FMX.RichEdit.Style;

interface

uses
  FMX.Text.TextEditor, FMX.Text.LinesLayout, FMX.TextLayout, FMX.Memo.Style.New,
  FMX.Controls.Presentation, FMX.Text, FMX.ScrollBox.Style, FMX.Controls,
  FMX.Layouts, FMX.Graphics, System.UITypes, Syntax.Code,
  FMX.Presentation.Messages, FMX.Menus, FMX.Objects, System.Classes,
  System.Types, FMX.Types, FMX.Memo, System.Generics.Collections;

type
  TMemoGutter = class(TPaintBox)
  public
    destructor Destroy; override;
  end;

  TTextRange = FMX.TextLayout.TTextRange;

  TRichEditLinesLayout = class(TLinesLayout)
  private
    FSelectedRange: TDictionary<Integer, TTextRange>;
    FSelectedColor: TAlphaColor;
    FNoHighlight: Boolean;
    FUseSelectedTextColor: Boolean;
    FCodeSyntax: TCodeSyntax;
    FLineSpacing: Single;
    procedure SetLineSpacing(const Value: Single);
    procedure UpdateLayout;
  protected
    procedure UpdateLayoutParams(const ALineIndex: Integer; const ALayout: TTextLayout); override;
    procedure InvalidateLinesLayouts;
    procedure SetNoHighlight(const Value: Boolean);
  public
    constructor Create(const ALineSource: ITextLinesSource; const AScrollableContent: IScrollableContent);
    destructor Destroy; override;

    procedure ReplaceLine(const AIndex: Integer; const ALine: string); override;
  public
    procedure SetCodeSyntaxName(const Lang: string; const DefFont: TFont; DefColor: TAlphaColor);
    procedure ClearCache;
    property LineSpacing: Single read FLineSpacing write SetLineSpacing;
    procedure SetSelectedTextColor(const Color: TAlphaColor);
    procedure SetUseSelectedTextColor(const Value: Boolean);
  end;

  TRichEditTextEditor = class(TTextEditor)
  private
    FOnChangeCaretPos: TCaretPositionChanged;
    FRoundedSelection: Boolean;
    procedure SetOnChangeCaretPos(const Value: TCaretPositionChanged);
    procedure SetRoundedSelection(const Value: Boolean);
    procedure UpdateSelRanges;
  protected
    procedure DoSelectionChanged(const ASelStart, ALength: Integer); override;
    procedure DoCaretPositionChanged; override;
    function CreateLinesLayout: TLinesLayout; override;
    procedure DrawSelection(const ACanvas: TCanvas); override;
    procedure DrawSelectionRegionCorner(const ACanvas: TCanvas; const Corners: TCorners; const ARegion: TRectF);
  public
    property OnChangeCaretPos: TCaretPositionChanged read FOnChangeCaretPos write SetOnChangeCaretPos;
    property RoundedSelection: Boolean read FRoundedSelection write SetRoundedSelection;
  end;

  TRichEditStyled = class(TStyledMemo)
  private
    FWasInitialized: Boolean;
    FDragButton: TMouseButton;
    FDragShift: TShiftState;
    FDragX, FDragY: Single;
    FCancelDrag, FNeedDefClick, FDragging: Boolean;
    FLinesBackgroundColor: TObjectDictionary<Integer, TBrush>;
    FOnChangeCaretPos: TCaretPositionChanged;
    FGutterWidth: Single;
    FShowError: Boolean;
    FErrorLine: Int64;
    FShowGutter: Boolean;
    FShowCurrentLine: Boolean;
    FLineSpacing: Single;
    FGutterControl: TPaintBox;
    FRoundedSelection: Boolean;
    FSelectedTextColor: TAlphaColor;
    FUseSelectedTextColor: Boolean;
    FWordHighlight: TObjectDictionary<TTextRange, TStrokeBrush>;
    FOnDrawAfter: TPaintEvent;
    FOnDrawBefore: TPaintEvent;
    FShowWordHighLight: Boolean;
    FShowLinesBackgroundColor: Boolean;
    FGutterNumberAllLines: Boolean;
    procedure DoChangeCaretPos(Sender: TObject; const ACaretPosition: TCaretPosition);
    procedure SetOnChangeCaretPos(const Value: TCaretPositionChanged);
    function GetCanCopy: Boolean;
    function GetCanCut: Boolean;
    function GetCanDelete: Boolean;
    function GetCanPaste: Boolean;
    function GetCanSelectAll: Boolean;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
    procedure InternalContentPaint(Sender: TObject; ACanvas: TCanvas; const ARect: TRectF);
    procedure SetShowGutter(const Value: Boolean);
    procedure SetErrorLine(const Value: Int64);
    procedure SetShowError(const Value: Boolean);
    procedure SetShowCurrentLine(const Value: Boolean);
    procedure SetLineSpacing(const Value: Single);
    procedure SetRoundedSelection(const Value: Boolean);
    procedure DrawHighlightLines(ACanvas: TCanvas);
    procedure RecalcGutter;
    procedure SetSelectedTextColor(const Value: TAlphaColor);
    procedure SetUseSelectedTextColor(const Value: Boolean);
    procedure SetOnDrawAfter(const Value: TPaintEvent);
    procedure SetOnDrawBefore(const Value: TPaintEvent);
    procedure DoDrawBefore(ACanvas: TCanvas; const ARect: TRectF);
    procedure DoDrawAfter(ACanvas: TCanvas; const ARect: TRectF);
    procedure SetShowWordHighLight(const Value: Boolean);
    procedure SetShowLinesBackgroundColor(const Value: Boolean);
    procedure SetGutterNumberAllLines(const Value: Boolean);
  protected
    function CreateEditor: TTextEditor; override;
    procedure FGutterPaint(Sender: TObject; Canvas: TCanvas);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure DragEnd; override;
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure PMSetStyleLookup(var AMessage: TDispatchMessageWithValue<string>); message PM_SET_STYLE_LOOKUP;
    procedure MMLinesChanged(var Message: TDispatchMessage); message MM_MEMO_LINES_CHANGED;
    procedure PMInit(var Message: TDispatchMessage); message PM_INIT;
    procedure PaintChildren; override;
    procedure DoViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean); override;
    procedure FillPopupMenu(const AMenu: TPopupMenu); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure UpdateVisibleLayoutParams;
  public
    procedure Paint; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    function GetTextRegion(const Range: TTextRange): TRegion;
    procedure SetCodeSyntaxName(const Lang: string; const DefFont: TFont; DefColor: TAlphaColor);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LinesBackgroundColor: TObjectDictionary<Integer, TBrush> read FLinesBackgroundColor;
    property ShowLinesBackgroundColor: Boolean read FShowLinesBackgroundColor write SetShowLinesBackgroundColor;
    property WordHighlight: TObjectDictionary<TTextRange, TStrokeBrush> read FWordHighlight;
    property OnChangeCaretPos: TCaretPositionChanged read FOnChangeCaretPos write SetOnChangeCaretPos;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
    property CanCut: Boolean read GetCanCut;
    property CanCopy: Boolean read GetCanCopy;
    property CanPaste: Boolean read GetCanPaste;
    property CanDelete: Boolean read GetCanDelete;
    property CanSelectAll: Boolean read GetCanSelectAll;
    property ShowGutter: Boolean read FShowGutter write SetShowGutter;
    property ShowError: Boolean read FShowError write SetShowError;
    property ShowCurrentLine: Boolean read FShowCurrentLine write SetShowCurrentLine;
    property ShowWordHighLight: Boolean read FShowWordHighLight write SetShowWordHighLight;
    property ErrorLine: Int64 read FErrorLine write SetErrorLine;
    property LineSpacing: Single read FLineSpacing write SetLineSpacing;
    property RoundedSelection: Boolean read FRoundedSelection write SetRoundedSelection;
    property SelectedTextColor: TAlphaColor read FSelectedTextColor write SetSelectedTextColor;
    property UseSelectedTextColor: Boolean read FUseSelectedTextColor write SetUseSelectedTextColor;
    property GutterNumberAllLines: Boolean read FGutterNumberAllLines write SetGutterNumberAllLines;
    property OnDrawBefore: TPaintEvent read FOnDrawBefore write SetOnDrawBefore;
    property OnDrawAfter: TPaintEvent read FOnDrawAfter write SetOnDrawAfter;
  end;

implementation

uses
  System.SysUtils, FMX.Presentation.Style, FMX.Presentation.Factory,
  FMX.Platform, FMX.Forms, FMX.Clipboard, System.Math, FMX.StyledContextMenu;

{ TRichEditTextEditor }

function TRichEditTextEditor.CreateLinesLayout: TLinesLayout;
begin
  Result := TRichEditLinesLayout.Create(Lines, ScrollableContent);
end;

procedure TRichEditTextEditor.DoCaretPositionChanged;
begin
  inherited;
  UpdateSelRanges;
  if Assigned(FOnChangeCaretPos) then
    FOnChangeCaretPos(Self, CaretPosition);
end;

procedure TRichEditTextEditor.DoSelectionChanged(const ASelStart, ALength: Integer);
begin
  inherited;
  UpdateSelRanges;
end;

procedure TRichEditTextEditor.UpdateSelRanges;
begin
  var SelRange := TRichEditLinesLayout(LinesLayout).FSelectedRange;
  if not Assigned(SelRange) then
    Exit;
  SelRange.Clear;
  var SelBegin := SelectionController.SelBegin;
  var SelEnd := SelectionController.SelEnd;
  if SelBegin.Line = SelEnd.Line then
    SelRange.Add(SelBegin.Line, TTextRange.Create(SelBegin.Pos, SelectionController.SelLength))
  else
  begin
    SelRange.Add(SelBegin.Line, TTextRange.Create(SelBegin.Pos, Lines.Lines[SelBegin.Line].Length));
    for var i := SelBegin.Line + 1 to SelEnd.Line - 1 do
      SelRange.Add(i, TTextRange.Create(0, Lines.Lines[i].Length));
    SelRange.Add(SelEnd.Line, TTextRange.Create(0, SelEnd.Pos));
  end;
  TRichEditLinesLayout(LinesLayout).UpdateLayout;
end;

procedure TRichEditTextEditor.DrawSelection(const ACanvas: TCanvas);

  procedure DrawLeftAndRightSelectionSide(const ARegion: TRegion);
  var
    SelectionRect: TRectF;
    HalfCaretWidth: Single;
    SideRect: TRectF;
  begin
    if Length(ARegion) > 0 then
    begin
      HalfCaretWidth := Caret.Flasher.Size.Width / 2;
      ACanvas.Fill.Color := Caret.Flasher.Color;
      ACanvas.Fill.Kind := TBrushKind.Solid;
      // Draw Left selection side
      SelectionRect := ARegion[0];
      SideRect := TRectF.Create(SelectionRect.Left - HalfCaretWidth, SelectionRect.Top,
        SelectionRect.Left + HalfCaretWidth, SelectionRect.Bottom);
      ACanvas.FillRect(SideRect, 3, 3, AllCorners, 1{FOwner.AbsoluteOpacity});
      // Draw Right selection side
      SelectionRect := ARegion[High(ARegion)];
      SideRect := TRectF.Create(SelectionRect.Right - HalfCaretWidth, SelectionRect.Top,
        SelectionRect.Right + HalfCaretWidth, SelectionRect.Bottom);
      ACanvas.FillRect(SideRect, 3, 3, AllCorners, 1{FOwner.AbsoluteOpacity});
    end;
  end;

  function TryGetRegion(const ARegion: TRegion; const Index: Integer): TRectF;
  begin
    if Index < Low(ARegion) then
      Exit(TRectF.Create(-1, -1, -1, -1));
    if Index > High(ARegion) then
      Exit(TRectF.Create(-1, -1, -1, -1));
    Result := ARegion[Index];
  end;

begin
  var Region := GetVisibleSelectionRegion;
  if FRoundedSelection then
  begin
    for var i := Low(Region) to High(Region) do
    begin
      if Region[i].Width = 0 then
        Region[i].Width := DefaultEmptySelectionWidth;
      var RectLine := Region[i];
      RectLine.Inflate(0, 2);
      RectLine.Offset(0, -2);
      var Corners: TCorners := AllCorners;
      var Prev := TryGetRegion(Region, i - 1);
      var Next := TryGetRegion(Region, i + 1);
      if (Prev.Right > Region[i].Right) and (Prev.Left < Region[i].Right) then
        Exclude(Corners, TCorner.TopRight);
      if Next.Right > Region[i].Right then
        Exclude(Corners, TCorner.BottomRight);
      if ((Next.Left < Region[i].Left) and (Next.Width > 0)) and (Next.Right >= Region[i].Left) then
        Exclude(Corners, TCorner.BottomLeft);
      RectLine.Offset(0, 2);

      DrawSelectionRegionCorner(ACanvas, Corners, RectLine);
    end;
  end
  else
  begin
    var Path := TPathData.Create;
    try
      for var i := Low(Region) to High(Region) do
      begin
        if Region[i].Width = 0 then
          Region[i].Width := DefaultEmptySelectionWidth;
        Path.AddRectangle(Region[i], 0, 0, AllCorners);
      end;
      ACanvas.FillPath(Path, 1, SelectionFill);
    finally
      Path.Free;
    end;
  end;

  if ShouldDrawLeftAndRightSelectionSides then
    DrawLeftAndRightSelectionSide(Region);
end;

procedure TRichEditTextEditor.DrawSelectionRegionCorner(const ACanvas: TCanvas; const Corners: TCorners; const ARegion: TRectF);
begin
  ACanvas.FillRect(ARegion, 3, 3, Corners, 1, SelectionFill);
  if TCorner.BottomLeft not in Corners then
  begin
    var R := ARegion;
    R.Offset(-4, ARegion.Height - 7);
    R.Width := 6;
    ACanvas.FillRect(R, 3, 3, [TCorner.TopLeft], 1, SelectionFill, TCornerType.InnerLine);
  end;
  if TCorner.BottomRight not in Corners then
  begin
    var R := ARegion;
    R.Offset(ARegion.Width - 3, ARegion.Height - 7);
    R.Width := 6;
    ACanvas.FillRect(R, 3, 3, [TCorner.TopRight], 1, SelectionFill, TCornerType.InnerLine);
  end;
  if TCorner.TopRight not in Corners then
  begin
    var R := ARegion;
    R.Offset(ARegion.Width - 3, -ARegion.Height + 7);
    R.Width := 6;
    ACanvas.FillRect(R, 3, 3, [TCorner.BottomRight], 1, SelectionFill, TCornerType.InnerLine);
  end;
end;

procedure TRichEditTextEditor.SetOnChangeCaretPos(const Value: TCaretPositionChanged);
begin
  FOnChangeCaretPos := Value;
end;

procedure TRichEditTextEditor.SetRoundedSelection(const Value: Boolean);
begin
  FRoundedSelection := Value;
end;

{ TRichEditLinesLayout }

procedure TRichEditLinesLayout.ClearCache;
begin
  if Assigned(FCodeSyntax) then
    FCodeSyntax.DropCache;
end;

constructor TRichEditLinesLayout.Create(const ALineSource: ITextLinesSource; const AScrollableContent: IScrollableContent);
begin
  FLineSpacing := 1;
  FSelectedRange := TDictionary<Integer, TTextRange>.Create;
  FSelectedColor := TAlphaColorRec.White;
  inherited;
end;

destructor TRichEditLinesLayout.Destroy;
begin
  FreeAndNil(FSelectedRange);
  FreeAndNil(FCodeSyntax);
  inherited;
end;

procedure TRichEditLinesLayout.InvalidateLinesLayouts;
begin
  for var I := 0 to Count - 1 do
  begin
    Items[I].Invalidate;
    Items[I].RenderingMode := RenderingMode;
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

procedure TRichEditLinesLayout.SetLineSpacing(const Value: Single);
begin
  FLineSpacing := Value;
end;

procedure TRichEditLinesLayout.SetNoHighlight(const Value: Boolean);
begin
  FNoHighlight := Value;
  UpdateLayout;
end;

procedure TRichEditLinesLayout.SetSelectedTextColor(const Color: TAlphaColor);
begin
  FSelectedColor := Color;
  UpdateLayout;
end;

procedure TRichEditLinesLayout.UpdateLayout;
begin
  if (FirstVisibleLineIndex = -1) or (LastVisibleLineIndex = -1) or IsUpdating then
    Exit;
  for var I := Max(0, FirstVisibleLineIndex) to Min(LastVisibleLineIndex, Count - 1) do
    if Assigned(Items[i]) then
      Items[i].InvalidateLayout;
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
    var P := (FLineSpacing - 1) * GetLineHeight(ALineIndex) / 2;
    ALayout.Padding.Top := P;
    ALayout.Padding.Bottom := P;

    if not FUseSelectedTextColor or FNoHighlight then
      Exit;
    if FSelectedRange.ContainsKey(ALineIndex) then
    begin
      var Attr: TTextAttribute;
      Attr.Font := TextSettings.Font;
      Attr.Color := FSelectedColor;

      ALayout.AddAttribute(FSelectedRange[ALineIndex], Attr);
    end;
  finally
    ALayout.EndUpdate;
  end;
end;

procedure TRichEditLinesLayout.SetUseSelectedTextColor(const Value: Boolean);
begin
  FUseSelectedTextColor := Value;
  UpdateLayout;
end;

{ TRichEditStyled }

procedure TRichEditStyled.DrawHighlightLines(ACanvas: TCanvas);
begin
  ACanvas.BeginScene;
  try
    // Line number
    var BRect := Content.BoundsRect;
    for var i := Max(0, Editor.LinesLayout.FirstVisibleLineIndex) to Min(Editor.LinesLayout.LastVisibleLineIndex, Editor.LinesLayout.Count - 1) do
      if not Editor.LinesLayout[i].IsInvalidPosition then
      begin
        // Calc and normalize line rect
        var Rect := Editor.LinesLayout[i].Rect;
        Rect.Height := Editor.LinesLayout[i].Size.Height;
        Rect.Left := 0;
        Rect.Width := Max(Width, Content.Width);
        Rect.NormalizeRect;
        if (Rect.Top < 0) and (Rect.Bottom < 0) then
          Continue;
        if (Rect.Top > BRect.Height) and (Rect.Bottom > BRect.Height) then
          Continue;

        // Draw error line
        if FShowError and (i = FErrorLine - 1) then
        begin
          ACanvas.Fill.Color := TAlphaColorRec.Red;
          ACanvas.FillRect(Rect, 0.1);
        end // Draw current line
        else if FShowCurrentLine and (i = CaretPosition.Line) then
        begin
          ACanvas.Fill.Color := TAlphaColorRec.White;
          ACanvas.FillRect(Rect, 0.1);
        end; // Draw custom line background
        if FShowLinesBackgroundColor and FLinesBackgroundColor.ContainsKey(i) then
        begin
          var Brush := FLinesBackgroundColor.Items[i];
          ACanvas.Fill.Assign(Brush);
          ACanvas.FillRect(Rect, 1);
        end;
      end;
    // Draw range highlight
    if FShowWordHighLight then
      for var Item in FWordHighlight do
      begin
        var Region := GetTextRegion(Item.Key);
        for var Rect in Region do
          if Rect.Width > 3 then
          begin
            var R := Rect;
            if R.Right < 0 then
              Continue;
            R.Left := Max(0, R.Left);
            ACanvas.Stroke.Assign(Item.Value);
            ACanvas.DrawLine(
              TPointF.Create(R.Left, R.Bottom),
              TPointF.Create(R.Right, R.Bottom),
              1);
          end;
      end;
  finally
    ACanvas.EndScene;
  end;
end;

procedure TRichEditStyled.FGutterPaint(Sender: TObject; Canvas: TCanvas);
begin
  if FGutterControl.Width <= 0 then
    Exit;
  Canvas.BeginScene;
  try
    Canvas.Font.Assign(Memo.TextSettings.Font);
    // Line number
    var BRect := Content.BoundsRect;
    for var I := Max(0, Editor.LinesLayout.FirstVisibleLineIndex) to Min(Editor.LinesLayout.LastVisibleLineIndex, Editor.LinesLayout.Count - 1) do
      if not Editor.LinesLayout[I].IsInvalidPosition then
      begin
        var Rect := Editor.LinesLayout[I].Rect;
        Rect.Height := Editor.LinesLayout[I].Size.Height;
        Rect.Offset(0, -ViewportPosition.Y);
        Rect.Left := 0;
        Rect.Width := FGutterWidth;
        Rect.NormalizeRect;
        if (Rect.Top < 0) and (Rect.Bottom < 0) then
          Continue;
        if (Rect.Top > BRect.Height) and (Rect.Bottom > BRect.Height) then
          Continue;

        // Draw error line
        if FShowError and (i = FErrorLine - 1) then
        begin
          Canvas.Fill.Color := TAlphaColorRec.Red;
          Canvas.FillRect(Rect, 0.1);
        end // Draw current line
        else if FShowCurrentLine and (i = CaretPosition.Line) then
        begin
          Canvas.Fill.Color := TAlphaColorRec.White;
          Canvas.FillRect(Rect, 0.1);
        end; // Draw custom line background

          Rect.Width := FGutterWidth - 10;
          Canvas.Fill.Color := TAlphaColorRec.White;

          var NumOpacity := 0.3;
          if i = CaretPosition.Line then
            NumOpacity := 0.6;

        if FGutterNumberAllLines or (i = 0) or ((i + 1) mod 10 = 0) or (i = CaretPosition.Line) then
        begin
          Canvas.FillText(Rect, (i + 1).ToString, False, NumOpacity, [], TTextAlign.Trailing, TTextAlign.Center);
        end
        else
          Canvas.FillText(Rect, '·', False, NumOpacity, [], TTextAlign.Trailing, TTextAlign.Center);
      end;

    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.Stroke.Color := TAlphaColorRec.White;
    Canvas.Stroke.Thickness := 1;
    Canvas.DrawLine(
      Canvas.AlignToPixel(TPointF.Create(FGutterWidth - 1, BRect.Top - 5)),
      Canvas.AlignToPixel(TPointF.Create(FGutterWidth - 1, BRect.Bottom + 5)), 0.3);
  finally
    Canvas.EndScene;
  end;
end;

procedure TRichEditStyled.FillPopupMenu(const AMenu: TPopupMenu);
begin
  inherited;
  LocalizeDefPopupMenu(AMenu);
end;

procedure TRichEditStyled.FreeStyle;
begin
  inherited;
  FGutterControl := nil;
end;

procedure TRichEditStyled.Paint;
begin
  inherited;
end;

procedure TRichEditStyled.PaintChildren;
begin
  inherited;
end;

procedure TRichEditStyled.ApplyStyle;
begin
  inherited;
  var Content: TLayout;
  if FindStyleResource<TLayout>('content', Content) then
  begin
    // Gutter
    FGutterControl := TMemoGutter.Create(Content.Parent);
    Content.Parent.AddObject(FGutterControl);
    FGutterControl.Align := TAlignLayout.MostLeft;
    FGutterControl.Width := 0;
    FGutterControl.OnPaint := FGutterPaint;
    FGutterControl.Margins.Rect := Content.Margins.Rect;
    FGutterControl.Margins.Right := -12;
    Content.Margins.Left := 0;
    RecalcGutter;
  end;
end;

procedure TRichEditStyled.InternalContentPaint(Sender: TObject; ACanvas: TCanvas; const ARect: TRectF);
begin
  DrawHighlightLines(ACanvas);
  // Draw content
  DoDrawBefore(ACanvas, ARect);
  ContentPaint(Sender, ACanvas, ARect);
  DoDrawAfter(ACanvas, ARect);
end;

constructor TRichEditStyled.Create(AOwner: TComponent);
begin
  inherited;
  DisableDisappear := True;
  FLineSpacing := 1;
  FGutterNumberAllLines := True;
  FLinesBackgroundColor := TObjectDictionary<Integer, TBrush>.Create([doOwnsValues]);
  FWordHighlight := TObjectDictionary<TTextRange, TStrokeBrush>.Create([doOwnsValues]);
end;

function TRichEditStyled.CreateEditor: TTextEditor;
begin
  Result := TRichEditTextEditor.Create(Self, Memo.Content, Model, Self);
  TRichEditTextEditor(Result).OnChangeCaretPos := DoChangeCaretPos;
end;

destructor TRichEditStyled.Destroy;
begin
  FLinesBackgroundColor.Free;
  FWordHighlight.Free;
  inherited;
end;

procedure TRichEditStyled.DoChangeCaretPos(Sender: TObject; const ACaretPosition: TCaretPosition);
begin
  if Assigned(FOnChangeCaretPos) then
    FOnChangeCaretPos(Self, ACaretPosition);
  Repaint;
end;

procedure TRichEditStyled.DoDrawAfter(ACanvas: TCanvas; const ARect: TRectF);
begin
  if Assigned(FOnDrawAfter) then
    FOnDrawAfter(Self, ACanvas);
end;

procedure TRichEditStyled.DoDrawBefore(ACanvas: TCanvas; const ARect: TRectF);
begin
  if Assigned(FOnDrawBefore) then
    FOnDrawBefore(Self, ACanvas);
end;

procedure TRichEditStyled.DoEnter;
begin
  inherited;
  if not Model.HideSelectionOnExit then
    Exit;
  TRichEditLinesLayout(Editor.LinesLayout).SetNoHighlight(False);
end;

procedure TRichEditStyled.DoExit;
begin
  inherited;
  if not Model.HideSelectionOnExit then
    Exit;
  TRichEditLinesLayout(Editor.LinesLayout).SetNoHighlight(True);
end;

procedure TRichEditStyled.RecalcGutter;
begin
  if not Assigned(Memo.Canvas) then
    Exit;
  if FShowGutter then
  begin
    var W := Ceil(Memo.Canvas.TextWidth(Memo.Lines.Count.ToString) + 20);
    if FGutterWidth <> W then
      FGutterWidth := W;
  end
  else
  begin
    if FGutterWidth <> 0 then
      FGutterWidth := 0;
  end;
  if Assigned(FGutterControl) then
    FGutterControl.Width := FGutterWidth;
end;

procedure TRichEditStyled.DoViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  inherited;
  RecalcGutter;
  Repaint;
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

function TRichEditStyled.GetCanCopy: Boolean;
begin
  Result := Editor.SelectionController.IsSelected;
end;

function TRichEditStyled.GetCanCut: Boolean;
begin
  Result := Editor.SelectionController.IsSelected and not Model.ReadOnly;
end;

function TRichEditStyled.GetCanDelete: Boolean;
begin
  Result := Editor.SelectionController.IsSelected and not Model.ReadOnly;
end;

function TRichEditStyled.GetCanPaste: Boolean;
var
  ClipService: IFMXExtendedClipboardService;
begin
  Result := TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, ClipService) and
    ClipService.HasText and not Model.ReadOnly;
end;

function TRichEditStyled.GetCanRedo: Boolean;
begin
  Result := not Model.ReadOnly and UndoManager.CanRedo;
end;

function TRichEditStyled.GetCanSelectAll: Boolean;
begin
  Result := Model.SelLength <> Model.Lines.Text.Length;
end;

function TRichEditStyled.GetCanUndo: Boolean;
begin
  Result := not Model.ReadOnly and UndoManager.CanUndo;
end;

function TRichEditStyled.GetTextRegion(const Range: TTextRange): TRegion;
begin
  var LCaret := Editor.Lines.TextPosToPos(Range.Pos);
  Result := Editor.LinesLayout.GetRegionForRange(LCaret, Range.Length);
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
  Content.OnPaint := InternalContentPaint;
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

procedure TRichEditStyled.SetErrorLine(const Value: Int64);
begin
  FErrorLine := Value;
  Repaint;
end;

procedure TRichEditStyled.SetGutterNumberAllLines(const Value: Boolean);
begin
  FGutterNumberAllLines := Value;
  Repaint;
end;

procedure TRichEditStyled.SetLineSpacing(const Value: Single);
begin
  FLineSpacing := Value;
  TRichEditLinesLayout(Editor.LinesLayout).LineSpacing := FLineSpacing;
  TRichEditLinesLayout(Editor.LinesLayout).InvalidateLinesLayouts;
  TRichEditLinesLayout(Editor.LinesLayout).Realign;
end;

procedure TRichEditStyled.SetOnChangeCaretPos(const Value: TCaretPositionChanged);
begin
  FOnChangeCaretPos := Value;
end;

procedure TRichEditStyled.SetOnDrawAfter(const Value: TPaintEvent);
begin
  FOnDrawAfter := Value;
end;

procedure TRichEditStyled.SetOnDrawBefore(const Value: TPaintEvent);
begin
  FOnDrawBefore := Value;
end;

procedure TRichEditStyled.SetRoundedSelection(const Value: Boolean);
begin
  FRoundedSelection := Value;
  TRichEditTextEditor(Editor).RoundedSelection := Value;
  Repaint;
end;

procedure TRichEditStyled.SetSelectedTextColor(const Value: TAlphaColor);
begin
  FSelectedTextColor := Value;
  TRichEditLinesLayout(Editor.LinesLayout).SetSelectedTextColor(FSelectedTextColor);
  UpdateVisibleLayoutParams;
  Repaint;
end;

procedure TRichEditStyled.SetShowCurrentLine(const Value: Boolean);
begin
  FShowCurrentLine := Value;
  Repaint;
end;

procedure TRichEditStyled.SetShowError(const Value: Boolean);
begin
  FShowError := Value;
  Repaint;
end;

procedure TRichEditStyled.SetShowGutter(const Value: Boolean);
begin
  FShowGutter := Value;
  RecalcGutter;
  RealignContent;
end;

procedure TRichEditStyled.SetShowLinesBackgroundColor(const Value: Boolean);
begin
  FShowLinesBackgroundColor := Value;
  Repaint;
end;

procedure TRichEditStyled.SetShowWordHighLight(const Value: Boolean);
begin
  FShowWordHighLight := Value;
  Repaint;
end;

procedure TRichEditStyled.SetUseSelectedTextColor(const Value: Boolean);
begin
  FUseSelectedTextColor := Value;
  TRichEditLinesLayout(Editor.LinesLayout).SetUseSelectedTextColor(FUseSelectedTextColor);
  UpdateVisibleLayoutParams;
  Repaint;
end;

procedure TRichEditStyled.UpdateVisibleLayoutParams;
begin
  TRichEditLinesLayout(Editor.LinesLayout).ClearCache;

  for var I := Max(0, Editor.LinesLayout.FirstVisibleLineIndex) to Min(Editor.LinesLayout.LastVisibleLineIndex, Editor.LinesLayout.Count - 1) do
  begin
    var Line := Editor.LinesLayout.Items[I];
    if Line.Layout <> nil then
      TRichEditLinesLayout(Editor.LinesLayout).UpdateLayoutParams(I, Line.Layout);
  end;
end;

{ TMemoGutter }

destructor TMemoGutter.Destroy;
begin
  inherited;
end;

initialization
  TPresentationProxyFactory.Current.Register('RichEditStyled', TStyledPresentationProxy<TRichEditStyled>);

finalization
  TPresentationProxyFactory.Current.Unregister('RichEditStyled', TStyledPresentationProxy<TRichEditStyled>);

end.

