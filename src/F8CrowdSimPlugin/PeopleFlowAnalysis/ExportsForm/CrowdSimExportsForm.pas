unit CrowdSimExportsForm;

interface

uses
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.ComCtrls,
    PedestrianMapUser,
    F_CrossSectionGraph,
    F_HeatMapSettings,
    F_ContourSettings;

type
    /// <summary>
    ///    断面交通流解析結果のグラフ、ヒートマップ、コンターの描画フレームをまとめるフォームを定義するクラス
    /// </summary>
    TFormCrowdSimExports = class(TForm)
        PageControlExports: TPageControl;
        TabSheetCrossSectionGraph: TTabSheet;
        TabSheetHeatMap: TTabSheet;
        TabSheetContour: TTabSheet;
        private
            CSGraphFrame : TFrameCrossSectionGraph;
            HeatMapFrame : TFrameHeatMapSettings;
            ContourFrame : TFrameContourSettings;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure SetUpExportsData(const PedMapUser: TPedestrianMapUser);
        end;

implementation

{$R *.dfm}

{ TFormCrowdSimExports }
//==============================================================================
procedure TFormCrowdSimExports.AfterConstruction;
    begin
    CSGraphFrame        := TFrameCrossSectionGraph.Create(Self);
    CSGraphFrame.Parent := TabSheetCrossSectionGraph;
    CSGraphFrame.Name   := 'FrameCrossSectionGraph';
    CSGraphFrame.Align  := alClient;

    HeatMapFrame        := TFrameHeatMapSettings.Create(Self);
    HeatMapFrame.Parent := TabSheetHeatMap;
    HeatMapFrame.Name   := 'FrameHeatMap';
    HeatMapFrame.Align  := alClient;

    ContourFrame        := TFrameContourSettings.Create(Self);
    ContourFrame.Parent := TabSheetContour;
    ContourFrame.Name   := 'FrameContour';
    ContourFrame.Align  := alClient;
    end;

//==============================================================================
procedure TFormCrowdSimExports.BeforeDestruction;
    begin
    FreeAndNil(CSGraphFrame);
    FreeAndNil(HeatMapFrame);
    FreeAndNil(ContourFrame);
    end;

//==============================================================================
procedure TFormCrowdSimExports.SetUpExportsData(const PedMapUser: TPedestrianMapUser);
    begin
    CSGraphFrame.PedestrianMapUser := PedMapUser;
    CSGraphFrame.OnSimStop;

    HeatMapFrame.PedestrianMapUser := PedMapUser;
    HeatMapFrame.CellsInOutList    := PedMapUser.CrossSectionFlowLog.CellsInOutList;

    ContourFrame.PedestrianMapUser := PedMapUser;
    ContourFrame.CellsInOutList    := PedMapUser.CrossSectionFlowLog.CellsInOutList;
    end;
end.
