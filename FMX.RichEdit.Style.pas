unit FMX.RichEdit.Style;

interface

uses
  FMX.Text.TextEditor, FMX.Text.LinesLayout, FMX.TextLayout, FMX.Memo.Style.New,
  FMX.Controls.Presentation, FMX.Text, FMX.ScrollBox.Style, FMX.Controls,
  FMX.Graphics, System.UITypes, Syntax.Code, FMX.Presentation.Messages, FMX.Menus,
  System.Classes, System.Types, FMX.Types, FMX.Memo, System.Generics.Collections;

type
  TRichEditLinesLayout = class(TLinesLayout)
  private
    FCodeSyntax: TCodeSyntax;
    FLineSpacing: Single;
    procedure SetLineSpacing(const Value: Single);
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
    property LineSpacing: Single read FLineSpacing write SetLineSpacing;
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
    FLinesBackgroundColor: TDictionary<Integer, TAlphaColor>;
    FOnChangeCaretPos: TCaretPositionChanged;
    FGutterWidth: Single;
    FShowError: Boolean;
    FErrorLine: Int64;
    FShowGutter: Boolean;
    FShowCurrentLine: Boolean;
    FLineSpacing: Single;
    procedure DoChangeCaretPos(Sender: TObject; const ACaretPosition: TCaretPosition);
    procedure SetOnChangeCaretPos(const Value: TCaretPositionChanged);
    function GetCanCopy: Boolean;
    function GetCanCut: Boolean;
    function GetCanDelete: Boolean;
    function GetCanPaste: Boolean;
    function GetCanSelectAll: Boolean;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
    procedure DrawGutter(ACanvas: TCanvas);
    procedure InternalContentPaint(Sender: TObject; ACanvas: TCanvas; const ARect: TRectF);
    procedure SetShowGutter(const Value: Boolean);
    procedure SetErrorLine(const Value: Int64);
    procedure SetShowError(const Value: Boolean);
    procedure SetShowCurrentLine(const Value: Boolean);
    procedure SetLineSpacing(const Value: Single);
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
    procedure PaintChildren; override;
    procedure DoViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean); override;
    procedure FillPopupMenu(const AMenu: TPopupMenu); override;
  public
    procedure Paint; override;
    procedure ApplyStyle; override;
    function GetSelectionRectTest: TRectF;
    procedure UpdateVisibleLayoutParams;
    procedure SetCodeSyntaxName(const Lang: string; const DefFont: TFont; DefColor: TAlphaColor);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LinesBackgroundColor: TDictionary<Integer, TAlphaColor> read FLinesBackgroundColor;
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
    property ErrorLine: Int64 read FErrorLine write SetErrorLine;
    property ShowCurrentLine: Boolean read FShowCurrentLine write SetShowCurrentLine;
    property LineSpacing: Single read FLineSpacing write SetLineSpacing;
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
  FLineSpacing := 1;
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

procedure TRichEditLinesLayout.SetLineSpacing(const Value: Single);
begin
  FLineSpacing := Value;
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
  finally
    ALayout.EndUpdate;
  end;
end;

{ TRichEditStyled }

procedure TRichEditStyled.DrawGutter;
begin
  LinesBackgroundColor.Clear;
  ACanvas.Font.Assign(Memo.TextSettings.Font);
  if FShowGutter then
  begin
    var W := Ceil(ACanvas.TextWidth(Memo.Lines.Count.ToString) + 20);
    if FGutterWidth <> W then
    begin
      FGutterWidth := W;
      StylesData['Padding.Left'] := W;
      RealignContent;
      //Exit;
    end;
  end
  else
  begin
    if FGutterWidth <> 0 then
    begin
      FGutterWidth := 0;
      StylesData['Padding.Left'] := 0;
      RealignContent;
    end;
  end;
  // Line number
  var BRect := BoundsRect;
  for var i := Max(0, Editor.LinesLayout.FirstVisibleLineIndex) to Min(Editor.LinesLayout.LastVisibleLineIndex, Editor.LinesLayout.Count - 1) do
    if not Editor.LinesLayout[i].IsInvalidPosition then
    begin
      var Rect := Editor.LinesLayout[i].Rect;
      Rect.Height := Editor.LinesLayout[i].Size.Height;
      Rect.Offset(0, -ViewportPosition.Y + 6);
      Rect.Left := 0;
      Rect.Width := FGutterWidth - 10;
      Rect.NormalizeRect;
      if (Rect.Top < 0) and (Rect.Bottom < 0) then
        Continue;
      if (Rect.Top > BRect.Height) and (Rect.Bottom > BRect.Height) then
        Continue;

      ACanvas.Fill.Color := TAlphaColorRec.White;
      var DeltaOpacity: Single := (100 / Editor.LinesLayout.Items[i].Rect.Height * Rect.Height) / 100;

      if FShowError and (i = FErrorLine - 1) then
      begin
        ACanvas.Fill.Color := TAlphaColorRec.Red;
        var R := Rect;
        R.Width := Memo.Width;
        R.Offset(-1, 0);
        ACanvas.FillRect(R, 0.1);
        DeltaOpacity := 300;
      end
      else if FShowCurrentLine and (i = Memo.CaretPosition.Line) then
      begin
        var R := Rect;
        R.Width := Memo.Width;
        R.Offset(-1, 0);
        ACanvas.FillRect(R, 0.1);
        DeltaOpacity := 300;
      end;

      if FShowGutter then
        ACanvas.FillText(Rect, (i + 1).ToString, False, 0.3 * DeltaOpacity, [], TTextAlign.Trailing, TTextAlign.Center);
    end;
  if FShowGutter then
  begin
    ACanvas.Stroke.Kind := TBrushKind.Solid;
    ACanvas.Stroke.Color := TAlphaColorRec.White;
    ACanvas.Stroke.Thickness := 1;
    ACanvas.DrawLine(ACanvas.AlignToPixel(TPointF.Create(FGutterWidth, BRect.Top)), ACanvas.AlignToPixel(TPointF.Create(FGutterWidth, BRect.Bottom)), 0.3);
  end;
end;

procedure TRichEditStyled.FillPopupMenu(const AMenu: TPopupMenu);
begin
  inherited;
  LocalizeDefPopupMenu(AMenu);
end;

procedure TRichEditStyled.Paint;
begin
  inherited;
  DrawGutter(Canvas);
end;

procedure TRichEditStyled.PaintChildren;
begin
  inherited;
end;

procedure TRichEditStyled.ApplyStyle;
begin
  inherited;
end;

procedure TRichEditStyled.InternalContentPaint(Sender: TObject; ACanvas: TCanvas; const ARect: TRectF);
begin
  ContentPaint(Sender, ACanvas, ARect);
end;

constructor TRichEditStyled.Create(AOwner: TComponent);
begin
  inherited;
  FLineSpacing := 1;
  FLinesBackgroundColor := TDictionary<Integer, TAlphaColor>.Create;
end;

function TRichEditStyled.CreateEditor: TTextEditor;
begin
  Result := TRichEditTextEditor.Create(Self, Memo.Content, Model, Self);
  TRichEditTextEditor(Result).OnChangeCaretPos := DoChangeCaretPos;

end;

destructor TRichEditStyled.Destroy;
begin
  FLinesBackgroundColor.Free;
  inherited;
end;

procedure TRichEditStyled.DoChangeCaretPos(Sender: TObject; const ACaretPosition: TCaretPosition);
begin
  if Assigned(FOnChangeCaretPos) then
    FOnChangeCaretPos(Self, ACaretPosition);
  Repaint;
end;

procedure TRichEditStyled.DoViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  inherited;
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
  RecalcSize;
end;

procedure TRichEditStyled.UpdateVisibleLayoutParams;
begin
  TRichEditLinesLayout(Editor.LinesLayout).ClearCache;

  for var i := Max(0, Editor.LinesLayout.FirstVisibleLineIndex) to Min(Editor.LinesLayout.LastVisibleLineIndex, Editor.LinesLayout.Count - 1) do
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

