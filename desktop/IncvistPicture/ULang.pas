unit ULang;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, UGlobal, LazUTF8, Types;

type

  { TLangForm }

  TLangForm = class(TForm)
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    LangListBox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure LangListBoxDrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure OkBtnClick(Sender: TObject);
  private
    { private declarations }
    procedure LanguageChanged;
    function GetListItemText(Lang: TLanguage): String;
  public
    { public declarations }
    class function Execute: Boolean;
  end;

var
  LangForm: TLangForm;

implementation

{$R *.lfm}

{ TLangForm }

procedure TLangForm.FormCreate(Sender: TObject);
var
  LangI: TLanguage;
begin
  for LangI in TLanguage do
  begin
    LangListBox.AddItem('', TObject(LangI));
    if LangI = Lang then
      LangListBox.ItemIndex := LangListBox.Count - 1;
  end;
  LanguageChanged;
end;

procedure TLangForm.LangListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  LangI: TLanguage;
  Txt: String;
  Pic: TPicture;
begin
  with LangListBox.Canvas do
  begin
    FillRect(ARect);
    LangI := TLanguage(LangListBox.Items.Objects[Index]);
    Pic := GetPicRes('flag-' + LangToStr(LangI) + '.png');
    if Assigned(Pic) then
    begin
      Draw(ARect.Left + 8, (ARect.Top + ARect.Bottom - Pic.Height) div 2,
       Pic.Graphic);
    end;
    Txt := GetListItemText(LangI);
    TextRect(ARect, ARect.Left + 48,
     (ARect.Top + ARect.Bottom - TextHeight('Wg')) div 2, Txt);
  end;
end;

procedure TLangForm.LanguageChanged;
var
  I: Integer;
  J: Integer;
  N: Integer;
  LangI: TLanguage;
  LangJ: TLanguage;
  SelectedLang: TLanguage;
begin
  Caption := Gti(1600);
  OkBtn.Caption := Gti(1000);
  CancelBtn.Caption := Gti(1001);

  with LangListBox do
  begin
    SelectedLang := TLanguage(Items.Objects[ItemIndex]);
    N := LangListBox.Count - 1;
    for I := 0 to N - 1 do
    begin
      for J := I + 1 to N do begin
        LangI := TLanguage(Items.Objects[I]);
        LangJ := TLanguage(Items.Objects[J]);
        if UTF8CompareStr(GetListItemText(LangI), GetListItemText(LangJ)) > 0 then
        begin
          Items.Objects[I] := TObject(LangJ);
          Items.Objects[J] := TObject(LangI);
        end;
      end;
    end;
    for I := 0 to N do
    begin
      if TLanguage(Items.Objects[I]) = SelectedLang then
      begin
        ItemIndex := I;
        Break;
      end;
    end;
  end;
end;

procedure TLangForm.OkBtnClick(Sender: TObject);
begin
  with LangListBox do
    Lang := TLanguage(Items.Objects[ItemIndex]);
  Save;
end;

function TLangForm.GetListItemText(Lang: TLanguage): String;
begin
  Result := Gts(UpperCase(LangToStr(Lang)));
end;

class function TLangForm.Execute: Boolean;
begin
  LangForm := TLangForm.Create(Application);
  Result := LangForm.ShowModal = mrOk;
  LangForm.Free;
end;

end.

