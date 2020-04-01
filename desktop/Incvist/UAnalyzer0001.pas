unit UAnalyzer0001;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UAnalyzer, UDeviceData;

type
  TAnalyzer0001 = class(TAnalyzer)
  private
  protected
    function GetMemSize: Integer; override;
    function GetBulkSize: Integer; override;
    function AnalyzeImpl: Boolean; override;
  public
  end;

implementation

uses
  UTypes, UGlobal;

const
  //MEM_NULL                                 = $FFFF;

  MEM_SYS_ADDR                             = 1000;
  //MEM_SYS_SIZE                             = 6;
  MEM_SYS_USED_CELLS                       = 0;
  MEM_SYS_FIRST_CELL                       = 2;
  //MEM_SYS_LAST_CELL                        = 4;

  //MEM_CELLS_SYS_ADDR                       = 1010;
  //MEM_CELLS_SYS_SIZE                       = 250;
  MEM_CELLS_ADDR                           = 1500;
  //MEM_CELLS_SIZE                           = 2000;

  // Cells
  MEM_CELL_DATE                            = 0;
  MEM_CELL_TIME                            = 3;
  //MEM_CELL_PREV                            = 6;
  MEM_CELL_NEXT                            = 8;
  MEM_CELL_NUMBER                          = 10;
  //MEM_CELL_BASE_SIZE                       = 12;

  MEM_CELL_T                               = 12;
  MEM_CELL_NOISE                           = 16;
  MEM_CELL_MEASUREMODE                     = 20;

  // Inc
  MEM_CELL_INC_DIAMETER                    = 21;
  MEM_CELL_INC_LENGTH                      = 22;
  //MEM_CELL_INC_TENSION                     = 24;
  MEM_CELL_INC_EPSILON                     = 26;
  MEM_CELL_INC_DELTAL                      = 30;
  MEM_CELL_INC_SIGMA                       = 34;
  // Vist
  MEM_CELL_VIST_OBJECT                     = 38;
  MEM_CELL_VIST_TYPE                       = 39;
  MEM_CELL_VIST_S                          = 40;
  MEM_CELL_VIST_V                          = MEM_CELL_VIST_S;
  MEM_CELL_VIST_A                          = MEM_CELL_VIST_S;

  MEM_CELL_SIZE                            = 44;

  USB_ENDPOINT1_BUFFER_SIZE                = 4096;


function TAnalyzer0001.GetMemSize: Integer;
begin
  Result := $20000;
end;

function TAnalyzer0001.GetBulkSize: Integer;
begin
  Result := USB_ENDPOINT1_BUFFER_SIZE;
end;

function TAnalyzer0001.AnalyzeImpl: Boolean;
var
  UsedCells: Integer;
  I: Integer;
  CurrCell: Integer;
  NextCell: Integer;
  Dt: TDate;
  Tm: TTime;
  IncData: TIncData;
  VistData: TVistData;
  IncItem: TIncItem;
  VistItem: TVistItem;
  MeasureMode: Byte;
  CellAddr: Integer;
begin
  Result := False;

  IncData := DevData.IncData;
  VistData := DevData.VistData;

  UsedCells := GetU16(MEM_SYS_ADDR + MEM_SYS_USED_CELLS);
  CurrCell := GetU16(MEM_SYS_ADDR + MEM_SYS_FIRST_CELL);

  for I := 0 to UsedCells - 1 do
  begin
    CellAddr := MEM_CELLS_ADDR + MEM_CELL_SIZE*CurrCell;
    NextCell := GetU16(CellAddr + MEM_CELL_NEXT);

    Dt := GetDate(CellAddr + MEM_CELL_DATE);
    Tm := GetTime(CellAddr + MEM_CELL_TIME);
    MeasureMode := GetU8(CellAddr + MEM_CELL_MEASUREMODE);

    if MeasureMode = 0 then
    begin
      Inc(IncDataRead);

      if IncData.Find(Dt, Tm) >= 0 then
      begin
        Inc(IncDataMatched);
      end
      else
      begin
        IncItem := TIncItem.Create;
        try
          IncItem.Date := TWDate.Create(Dt);
          IncItem.Time := TWTime.Create(Tm);
          IncItem.Number := TWInt.Create(GetU16(CellAddr + MEM_CELL_NUMBER));
          IncItem.MeasureT := TWFloat32.Create(GetF32(CellAddr + MEM_CELL_T));
          IncItem.Noise := TWFloat32.Create(GetF32(CellAddr + MEM_CELL_NOISE));
          IncItem.Diameter :=
           TWInt.Create(GetU8(CellAddr + MEM_CELL_INC_DIAMETER));
          IncItem.Len := TWFloat32.Create(0.01*GetU16(CellAddr + MEM_CELL_INC_LENGTH));
          IncItem.Epsilon :=
           TWFloat32.Create(GetF32(CellAddr + MEM_CELL_INC_EPSILON));
          IncItem.DeltaL :=
           TWFloat32.Create(GetF32(CellAddr + MEM_CELL_INC_DELTAL));
          IncItem.Sigma :=
           TWFloat32.Create(GetF32(CellAddr + MEM_CELL_INC_SIGMA));
          IncData.Add(IncItem);
          Inc(IncDataAdded);
        except
          IncItem.Free;
          raise;
        end;
      end;
    end
    else
    begin
      Inc(VistDataRead);

      if VistData.Find(Dt, Tm) >= 0 then
      begin
        Inc(VistDataMatched);
      end
      else
      begin
        VistItem := TVistItem.Create;
        try
          VistItem.Date := TWDate.Create(Dt);
          VistItem.Time := TWTime.Create(Tm);
          VistItem.Number := TWInt.Create(GetU16(CellAddr + MEM_CELL_NUMBER));
          VistItem.MeasureT := TWFloat32.Create(GetF32(CellAddr + MEM_CELL_T));
          VistItem.Noise := TWFloat32.Create(GetF32(CellAddr + MEM_CELL_NOISE));

          case GetU8(CellAddr + MEM_CELL_VIST_TYPE) of
            0:
            begin
              VistItem.MeasureSpp :=
               TWFloat32.Create(GetF32(CellAddr + MEM_CELL_VIST_S));
              VistItem.Typ := TWInt.Create(VistTypSId);
            end;
            1:
            begin
              VistItem.MeasureVrms :=
               TWFloat32.Create(GetF32(CellAddr + MEM_CELL_VIST_V));
              VistItem.Typ := TWInt.Create(VistTypVId);
            end;
            2:
            begin
              VistItem.MeasureAamp :=
               TWFloat32.Create(GetF32(CellAddr + MEM_CELL_VIST_A));
              VistItem.Typ := TWInt.Create(VistTypAId);
            end;
          end;

          case GetU8(CellAddr + MEM_CELL_VIST_OBJECT) of
            0: VistItem.Obj := TWInt.Create(VistObjGeneralId);
            1: VistItem.Obj := TWInt.Create(VistObjShakerTableId);
          end;

          VistData.Add(VistItem);
          Inc(VistDataAdded);
        except
          VistItem.Free;
          raise;
        end;
      end;
    end;

    CurrCell := NextCell;
  end;

  Result := True;
end;

end.

