unit UVistView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, PairSplitter, Grids, StdCtrls,
  UGlobal, UDeviceData, UNav, UTypes;

type

  { TVistViewFrame }

  TVistViewFrame = class(TFrame)
    CommentLbl: TLabel;
    CommentMemo: TMemo;
    DataDrawGrid: TDrawGrid;
    NavFrame: TNavigatorFrame;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
  private
    { private declarations }
    FCurrItem: TVistItem;
    FDataEdited: TNotifyEvent;
    procedure UpdateButtonsState(Row: Integer);
    procedure UpdateButtonsState;
    procedure UpdateComment(Row: Integer);
    procedure UpdateComment;
    procedure DeleteBtnClick(Sender: TObject);
    procedure FirstBtnClick(Sender: TObject);
    procedure LastBtnClick(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure PriorBtnClick(Sender: TObject);
    procedure CommentMemoChange(Sender: TObject);
    procedure DataDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
     aRect: TRect; {%H-}aState: TGridDrawState);
    procedure DataDrawGridBeforeSelection(Sender: TObject; {%H-}aCol,
     {%H-}aRow: Integer);
    procedure DataDrawGridSelection(Sender: TObject; {%H-}aCol, aRow: Integer);
  protected
    procedure Loaded; override;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure LanguageChanged;
    procedure BeginAddData;
    procedure EndAddData;
    procedure DataChanged;
    procedure GotoFirstRecord;
    procedure Clear;
    procedure CommitData;
    property DataEdited: TNotifyEvent read FDataEdited write FDataEdited;
  end;

implementation

{$R *.lfm}

constructor TVistViewFrame.Create(AOwner: TComponent);
const
  ColWidthsEng: array[0..10] of Integer = (
    40, 68, 52, 48, 52,
    52, 48, 96, 64, 64,
    64
  );
  ColWidthsRus: array[0..10] of Integer = (
    40, 68, 52, 48, 52,
    52, 48, 96, 78, 64,
    64
  );
var
  I: Integer;
begin
  inherited Create(AOwner);

  FDataEdited := nil;

  case Lang of
    lRussian:
      for I := Low(ColWidthsRus) To High(ColWidthsRus) do
        DataDrawGrid.ColWidths[I - Low(ColWidthsRus)] := ColWidthsRus[I];
    else
      for I := Low(ColWidthsEng) To High(ColWidthsEng) do
        DataDrawGrid.ColWidths[I - Low(ColWidthsEng)] := ColWidthsEng[I];
  end;

  FCurrItem := nil;

  with NavFrame do
  begin
    FirstBtn.OnClick := @FirstBtnClick;
    PriorBtn.OnClick := @PriorBtnClick;
    NextBtn.OnClick := @NextBtnClick;
    LastBtn.OnClick := @LastBtnClick;
    DeleteBtn.OnClick := @DeleteBtnClick;
  end;

  DataDrawGrid.OnDrawCell := @DataDrawGridDrawCell;
  DataDrawGrid.OnBeforeSelection := @DataDrawGridBeforeSelection;
  DataDrawGrid.OnSelection := @DataDrawGridSelection;
  CommentMemo.OnChange := @CommentMemoChange;
end;

procedure TVistViewFrame.Loaded;
begin
  inherited;

  PairSplitter1.Cursor := crVSplit;
end;

procedure TVistViewFrame.LanguageChanged;
begin
  CommentLbl.Caption := Gti(153);
  DataDrawGrid.Invalidate;
end;

procedure TVistViewFrame.DeleteBtnClick(Sender: TObject);
var
  Index: Integer;
  Item: TVistItem;
begin
  if not Assigned(DevQuery) then
    Exit;
  Index := DataDrawGrid.Row - 1;
  if (Index < 0) or (Index >= DevQuery.VistData.Count) then
    Exit;
  Item := DevQuery.VistData.Items[Index];
  DevQuery.VistData.Delete(Index);
  Index := DevData.VistData.IndexOf(Item);
  if Index >= 0 then
    DevData.VistData.Delete(Index);
  DataDrawGrid.RowCount := DataDrawGrid.RowCount - 1;
  DataDrawGrid.Invalidate;
end;

procedure TVistViewFrame.FirstBtnClick(Sender: TObject);
begin
  if DataDrawGrid.RowCount >= 2 then
      DataDrawGrid.Row := 1;
  UpdateButtonsState;
end;

procedure TVistViewFrame.LastBtnClick(Sender: TObject);
begin
  with DataDrawGrid do
    if (RowCount > 1) and (Row + 1 < RowCount) then
      Row := RowCount - 1;
  UpdateButtonsState;
end;

procedure TVistViewFrame.NextBtnClick(Sender: TObject);
begin
  with DataDrawGrid do
    if Row < RowCount - 1 then
      Row := Row + 1;
  UpdateButtonsState;
end;

procedure TVistViewFrame.PriorBtnClick(Sender: TObject);
begin
  with DataDrawGrid do
    if Row > 1 then
      Row := Row - 1;
  UpdateButtonsState;
end;

procedure TVistViewFrame.BeginAddData;
var
  Index: Integer;
begin
  FCurrItem := nil;
  if not Assigned(DevQuery) then
    Exit;

  Index := DataDrawGrid.Row - 1;
  if (Index < 0) or (Index >= DevQuery.VistData.Count)
    then
    else FCurrItem := DevQuery.VistData.Items[Index];
end;

procedure TVistViewFrame.EndAddData;
var
  Index: Integer;
begin
  with DevQuery.VistData do
  begin
    Index := IndexOf(FCurrItem);
    if DataDrawGrid.RowCount <> (1 + Count) then
    begin
      DataDrawGrid.RowCount := 1 + Count;
    end;
    if Index >= 0 then
      DataDrawGrid.Row := Index + 1;
  end;

  UpdateButtonsState;
end;

procedure TVistViewFrame.UpdateButtonsState;
begin
  UpdateButtonsState(DataDrawGrid.Row);
end;

procedure TVistViewFrame.UpdateButtonsState(Row: Integer);
var
  I,N: Integer;
begin
  if DataDrawGrid.RowCount = 1 then
  begin
    NavFrame.EnableAll(False);
    Exit;
  end;

  I := Row - 1;
  N := DataDrawGrid.RowCount - 2;

  NavFrame.EnableFirst(I > 0);
  NavFrame.EnablePrior(I > 0);
  NavFrame.EnableNext(I < N);
  NavFrame.EnableLast(I < N);
  NavFrame.EnableDelete(True);
end;

procedure TVistViewFrame.UpdateComment(Row: Integer);
var
  Index: Integer;
  Comment: TWString;
begin
  with DevQuery.VistData do
  begin
    Index := Row - 1;
    if (Index >= 0) and (Index < Count) then
    begin
      Comment := Items[Index].Comments;
      if Assigned(Comment) then
      begin
        CommentMemo.Text := Comment.Value;
        CommentMemo.SelStart := 0;
        CommentMemo.SelLength := 0;
        Exit;
      end;
    end;
  end;

  CommentMemo.Clear;
end;

procedure TVistViewFrame.UpdateComment;
begin
  UpdateComment(DataDrawGrid.Row);
end;

procedure TVistViewFrame.DataChanged;
var
  I: Integer;
begin
  I := 1;
  if Assigned(DevQuery) then
    I := DevQuery.VistData.Count + 1;
  DataDrawGrid.RowCount := I;
  DataDrawGrid.Invalidate;
end;

procedure TVistViewFrame.GotoFirstRecord;
begin
  if DataDrawGrid.RowCount > 1 then
    DataDrawGrid.Row := 1;
end;

procedure TVistViewFrame.CommentMemoChange(Sender: TObject);
begin
  if ProjOpened and not ProjEdited then
  begin
    ProjEdited := True;
    if Assigned(FDataEdited) then
      FDataEdited(Self);
  end;
end;

procedure TVistViewFrame.Clear;
begin
  CommentMemo.Clear;
  DataDrawGrid.RowCount := 1;
end;

procedure TVistViewFrame.CommitData;
var
  Index: Integer;
begin
  Index := DataDrawGrid.Row - 1;
  with DevQuery.VistData do
  begin
    if (Index >= 0) and (Index < Count) then
    begin
      with Items[Index] do
      begin
        if not Assigned(Comments) then
          Comments := TWString.Create;
        Comments.Value := CommentMemo.Text;
      end;
    end;
  end;
end;

procedure TVistViewFrame.DataDrawGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Txt: String;
  Y: Integer;
  Id: Integer;
  Item: TVistItem;
begin
  with (Sender as TDrawGrid).Canvas do
  begin
    Y := (ARect.Top + ARect.Bottom - TextHeight('Wg')) div 2;

    if ARow = 0 then
    begin
      Id := -1;
      case ACol of
        0..VistColumnCount: Id := VistColumnIds[ACol];
      end;
      if Id >= 0 then
        Txt := Gti(Id);
      TextRect(ARect, aRect.Left + 4, Y, Txt);
      Exit;
    end;

    if not Assigned(DevQuery) then Exit;

    if DevQuery.VistData.Count < ARow then Exit;

    if ACol = 0 then
    begin
      Txt := IntToStr(ARow);
      TextRect(ARect, aRect.Right - 8 - TextWidth(Txt), Y, Txt);
      Exit;
    end;

    Item := DevQuery.VistData.Items[ARow - 1];
    case ACol of
      1: Txt := DateToStr(Item);
      2: Txt := TimeToStr(Item);
      3: Txt := NumberToStr(Item);
      4: Txt := MeasureTtoStr(Item);
      5: Txt := MeasureFtoStr(Item);
      6: Txt := NoiseToStr(Item);
      7: Txt := ObjToStr(Item);
      8: Txt := MeasureSppToStr(Item);
      9: Txt := MeasureVrmsToStr(Item);
      10: Txt := MeasureAampToStr(Item);
      else Txt := '';
    end;

    case ACol of
      3..6,8..13:
        TextRect(ARect, ARect.Right - 4 - TextWidth(Txt), Y, Txt);
      -1:
        TextRect(ARect, ARect.Left + 4, Y, Txt);
      else
        TextRect(ARect, (ARect.Left + ARect.Right - TextWidth(Txt)) div 2, Y, Txt);
    end;
  end;
end;

procedure TVistViewFrame.DataDrawGridBeforeSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  CommitData;
end;

procedure TVistViewFrame.DataDrawGridSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  UpdateButtonsState(ARow);
  UpdateComment(ARow);
end;

end.

