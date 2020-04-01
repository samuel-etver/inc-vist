unit UIncView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Grids, ExtCtrls, PairSplitter,
  StdCtrls, UGlobal, UNav, UDeviceData, UTypes;

type

  { TIncViewFrame }

  TIncViewFrame = class(TFrame)
    DataDrawGrid: TDrawGrid;
    CommentLbl: TLabel;
    CommentMemo: TMemo;
    NavFrame: TNavigatorFrame;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
  private
    { private declarations }
    FCurrItem: TIncItem;
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

{ TIncViewFrame }
constructor TIncViewFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataEdited := nil;

  with DataDrawGrid do
  begin
    ColWidths[0] := 40;
    ColWidths[1] := 68;
    ColWidths[2] := 52;
    ColWidths[3] := 48;
    ColWidths[4] := 78;
    ColWidths[5] := 64;
    ColWidths[6] := 52;
    ColWidths[7] := 52;
    ColWidths[8] := 48;
    ColWidths[9] := 32;
    ColWidths[10] := 48;
    ColWidths[11] := 64;
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

procedure TIncViewFrame.Loaded;
begin
  inherited;

  PairSplitter1.Cursor := crVSplit;
end;

procedure TIncViewFrame.LanguageChanged;
begin
  CommentLbl.Caption := Gti(153);
  DataDrawGrid.Invalidate;
end;

procedure TIncViewFrame.DeleteBtnClick(Sender: TObject);
var
  Index: Integer;
  Item: TIncItem;
begin
  if not Assigned(DevQuery) then
    Exit;
  Index := DataDrawGrid.Row - 1;
  if (Index < 0) or (Index >= DevQuery.IncData.Count) then
    Exit;
  Item := DevQuery.IncData.Items[Index];
  DevQuery.IncData.Delete(Index);
  Index := DevData.IncData.IndexOf(Item);
  if Index >= 0 then
    DevData.IncData.Delete(Index);
  DataDrawGrid.RowCount := DataDrawGrid.RowCount - 1;
  DataDrawGrid.Invalidate;
end;

procedure TIncViewFrame.FirstBtnClick(Sender: TObject);
begin
  if DataDrawGrid.RowCount >= 2 then
      DataDrawGrid.Row := 1;
  UpdateButtonsState;
end;

procedure TIncViewFrame.LastBtnClick(Sender: TObject);
begin
  with DataDrawGrid do
    if (RowCount > 1) and (Row + 1 < RowCount) then
      Row := RowCount - 1;
  UpdateButtonsState;
end;

procedure TIncViewFrame.NextBtnClick(Sender: TObject);
begin
  with DataDrawGrid do
    if Row < RowCount - 1 then
      Row := Row + 1;
  UpdateButtonsState;
end;

procedure TIncViewFrame.PriorBtnClick(Sender: TObject);
begin
with DataDrawGrid do
    if Row > 1 then
      Row := Row - 1;
  UpdateButtonsState;
end;

procedure TIncViewFrame.BeginAddData;
var
  Index: Integer;
begin
  FCurrItem := nil;
  if not Assigned(DevQuery) then
    Exit;

  Index := DataDrawGrid.Row - 1;
  if (Index < 0) or (Index >= DevQuery.IncData.Count)
    then
    else FCurrItem := DevQuery.IncData.Items[Index];
end;

procedure TIncViewFrame.EndAddData;
var
  Index: Integer;
begin
  with DevQuery.IncData do
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

procedure TIncViewFrame.UpdateButtonsState;
begin
  UpdateButtonsState(DataDrawGrid.Row);
end;

procedure TIncViewFrame.UpdateButtonsState(Row: Integer);
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

procedure TIncViewFrame.UpdateComment(Row: Integer);
var
  Index: Integer;
  Comment: TWString;
begin
  with DevQuery.IncData do
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

procedure TIncViewFrame.UpdateComment;
begin
  UpdateComment(DataDrawGrid.Row);
end;

procedure TIncViewFrame.DataChanged;
var
  I: Integer;
begin
  I := 1;
  if Assigned(DevQuery) then
    I := DevQuery.IncData.Count + 1;
  DataDrawGrid.RowCount := I;
  DataDrawGrid.Invalidate;
end;

procedure TIncViewFrame.GotoFirstRecord;
begin
  if DataDrawGrid.RowCount > 1 then
    DataDrawGrid.Row := 1;
end;

procedure TIncViewFrame.CommentMemoChange(Sender: TObject);
begin
  if ProjOpened and not ProjEdited then
  begin
    ProjEdited := True;
    if Assigned(FDataEdited) then
      FDataEdited(Self);
  end;
end;

procedure TIncViewFrame.Clear;
begin
  CommentMemo.Clear;
  DataDrawGrid.RowCount := 1;
end;

procedure TIncViewFrame.CommitData;
var
  Index: Integer;
begin
  Index := DataDrawGrid.Row - 1;
  with DevQuery.IncData do
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

procedure TIncViewFrame.DataDrawGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Txt: String;
  Y: Integer;
  Id: Integer;
  Item: TIncItem;
begin
  with (Sender as TDrawGrid).Canvas do
  begin
    Y := (ARect.Top + ARect.Bottom - TextHeight('Wg')) div 2;

    if ARow = 0 then
    begin
      Id := -1;
      case ACol of
        0..10: Id := IncColumnIds[ACol];
        11:    Txt := Gti(IncColumnIds[ACol]) + SigmaUnitsToStr(SigmaUnits);
      end;
      if Id >= 0 then
        Txt := Gti(Id);
      TextRect(ARect, aRect.Left + 4, Y, Txt);
      Exit;
    end;

    if not Assigned(DevQuery) then Exit;

    if DevQuery.IncData.Count < ARow then Exit;

    if ACol = 0 then
    begin
      Txt := IntToStr(ARow);
      TextRect(ARect, aRect.Right - 8 - TextWidth(Txt), Y, Txt);
      Exit;
    end;

    Item := DevQuery.IncData.Items[ARow - 1];
    case ACol of
      1: Txt := DateToStr(Item);
      2: Txt := TimeToStr(Item);
      3: Txt := NumberToStr(Item);
      4: Txt := DiameterToStr(Item);
      5: Txt := LengthToStr(Item);
      6: Txt := MeasureTtoStr(Item);
      7: Txt := MeasureFtoStr(Item);
      8: Txt := NoiseToStr(Item);
      9: Txt := EpsilonToStr(Item);
      10: Txt := DeltaLtoStr(Item);
      11: Txt := SigmaToStr(Item, SigmaUnits);
      else Txt := '';
    end;

    case ACol of
      1..11:
        TextRect(ARect, ARect.Right - 4 - TextWidth(Txt), Y, Txt);
      -1:
        TextRect(ARect, ARect.Left + 4, Y, Txt);
      else
        TextRect(ARect, (ARect.Left + ARect.Right - TextWidth(Txt)) div 2, Y, Txt);
    end;
  end;
end;

procedure TIncViewFrame.DataDrawGridBeforeSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  CommitData;
end;

procedure TIncViewFrame.DataDrawGridSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  UpdateButtonsState(ARow);
  UpdateComment(ARow);
end;

end.

