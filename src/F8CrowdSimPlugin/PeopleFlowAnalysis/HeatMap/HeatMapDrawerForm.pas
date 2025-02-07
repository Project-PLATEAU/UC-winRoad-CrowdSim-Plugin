unit HeatMapDrawerForm;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.GraphUtil;

type
    /// <summary>
    ///    ヒートマップを画像として保存するためのクラス
    /// </summary>
    TClipPanel = class(TCustomControl)
        property Canvas;
    end;

    /// <summary>
    ///    ヒートマップを描画するフォームとその機能を定義するクラス
    /// </summary>
    TFormDrawHeatMap = class(TForm)
        PanelLegend: TPanel;
        ImageLegendBtoG: TImage;
        ImageLegendYtoR: TImage;
        LabelLegend: TLabel;
        LabelMaxNum: TLabel;
        LabelMiddleNum: TLabel;
        LabelMinNum: TLabel;
        PanelHeatMap: TPanel;
        ImageHeatMap: TImage;
        PanelFotter: TPanel;
        ButtonExportImage: TButton;
        ImageLegendGtoY: TImage;
        SaveDialogHeatMapImage: TSaveDialog;
        PanelMain: TPanel;
        procedure FormResize(Sender: TObject);
        procedure ButtonExportImageClick(Sender: TObject);
        private
            p_DrawData      : TBitmap;
            p_AspectRaitoHM : double;

            procedure SetLegendGrad;
            procedure ChangeLegendLabels(const max, middle, min: integer);
            procedure Draw;
            procedure SetDrawsSize;
        public
            procedure AfterConstruction; override;

            procedure DrawHeatMap(const HMData: TBitmap; MaxNum, MiddleNum, MinNum: integer);
        end;

implementation

{$R *.dfm}

{ TFormDrawHeatMap }
//==============================================================================
procedure TFormDrawHeatMap.AfterConstruction;
    begin
    inherited;
    SetLegendGrad;
    end;

//==============================================================================
procedure TFormDrawHeatMap.SetLegendGrad;
    begin
    GradientFillCanvas(ImageLegendBtoG.Canvas, clLime, clBlue, ImageLegendBtoG.Canvas.ClipRect, gdVertical);
    ImageLegendBtoG.Invalidate;
    GradientFillCanvas(ImageLegendGtoY.Canvas, clYellow, clLime, ImageLegendGtoY.Canvas.ClipRect, gdVertical);
    ImageLegendGtoY.Invalidate;
    GradientFillCanvas(ImageLegendYtoR.Canvas, clRed, clYellow, ImageLegendYtoR.Canvas.ClipRect, gdVertical);
    ImageLegendYtoR.Invalidate;
    end;

//==============================================================================
procedure TFormDrawHeatMap.ButtonExportImageClick(Sender: TObject);
    var
        HMClip   : TBitmap;
        ClipRect : TRect;
    begin
    HMClip := TBitmap.Create;
    HMClip.PixelFormat := pf24bit;
    HMClip.Width  := PanelMain.ClientWidth;
    HMClip.Height := PanelMain.ClientHeight;
    ClipRect := Rect(0, 0, HMClip.Width, HMClip.Height);
    HMClip.Canvas.CopyRect(ClipRect, TClipPanel(PanelMain).Canvas, ClipRect);
    if SaveDialogHeatMapImage.Execute then
        HMClip.SaveToFile(SaveDialogHeatMapImage.FileName);

    FreeAndNil(HMClip);
    end;

//==============================================================================
procedure TFormDrawHeatMap.ChangeLegendLabels(const max, middle, min: integer);
    begin
    LabelMaxNum.Caption    := max.ToString;
    LabelMiddleNum.Caption := middle.ToString;
    LabelMinNum.Caption    := min.ToString+'(人)';
    end;

//==============================================================================
procedure TFormDrawHeatMap.DrawHeatMap(const HMData: TBitmap; MaxNum, MiddleNum, MinNum: integer);
    begin
    p_DrawData := TBitmap.Create;
    p_DrawData := HMData;
    p_AspectRaitoHM := p_DrawData.Width / p_DrawData.Height;
    ChangeLegendLabels(MaxNum, MiddleNum, MinNum);
    Draw;
    end;

//==============================================================================
procedure TFormDrawHeatMap.Draw;
    var
        DrawRect : TRect;
    begin
    SetDrawsSize;
    DrawRect := Rect(0, 0, ImageHeatMap.Width, ImageHeatMap.Height);
    ImageHeatMap.Canvas.StretchDraw(DrawRect, p_DrawData);
    end;

//==============================================================================
procedure TFormDrawHeatMap.SetDrawsSize;
    var
        tmpbmp : TBitmap;
    begin
    ImageHeatMap.Width  := Round(ImageHeatMap.Height * p_AspectRaitoHM);
    tmpbmp := TBitmap.Create;
    tmpbmp.Width  := ImageHeatMap.Width;
    tmpbmp.Height := ImageHeatMap.Height;
    ImageHeatMap.Picture.Bitmap := tmpbmp;
    FreeAndNil(tmpbmp);
    end;

//==============================================================================
procedure TFormDrawHeatMap.FormResize(Sender: TObject);
    begin
    if Assigned(p_DrawData) then
        Draw;
    end;
end.
