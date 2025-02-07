unit ContourDrawerForm;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.Math,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.ExtCtrls,
    Vcl.StdCtrls,
    Vcl.GraphUtil;

type
    /// <summary>
    ///    コンターを画像として保存するためのクラス
    /// </summary>
    TClipPanel = class(TCustomControl)
        property Canvas;
    end;

    /// <summary>
    ///    コンターを描画するフォームとその機能を定義するクラス
    /// </summary>
    TFormDrawContour = class(TForm)
        PanelMain: TPanel;
        PanelFotter: TPanel;
        SaveDialogContourImage: TSaveDialog;
        PanelContour: TPanel;
        PanelLegend: TPanel;
        ImageContour: TImage;
        ButtonExportImage: TButton;
        LabelLegend: TLabel;
        LabelMaxmax: TLabel;
        LabelMaxmiddle: TLabel;
        LabelMaxmin: TLabel;
        LabelMiddlemax: TLabel;
        LabelMinmin: TLabel;
        LabelMinmiddle: TLabel;
        LabelMiddlemiddle: TLabel;
        LabelMiddlemin: TLabel;
        LabelMinmax: TLabel;
        ImageLegend: TImage;
        procedure FormResize(Sender: TObject);
        procedure ButtonExportImageClick(Sender: TObject);
        private
            p_DrawData       : TBitmap;
            p_AspectRaitoCon : double;

            procedure SetLegendColor;
            procedure ChangeLegendLabels(const max, middle, min: integer);
            procedure Draw;
            procedure SetDrawsSize;
        public
            procedure AfterConstruction; override;

            procedure DrawContour(const ConData: TBitmap; MaxNum, MiddleNum, MinNum: integer);
        end;

implementation

{$R *.dfm}

{ TFormDrawContour }
//==============================================================================
procedure TFormDrawContour.AfterConstruction;
    begin
    inherited;
    SetLegendColor;
    end;

//==============================================================================
procedure TFormDrawContour.SetLegendColor;

    //--------------------------------------------------------------------------
    procedure DrawLegend(const r, g, b, RectWOri, RectHOri, RectWEnd, RectHEnd: integer);
        begin
        ImageLegend.Canvas.Brush.Color := RGB(r, g, b);
        ImageLegend.Canvas.FillRect(Rect(RectWOri, RectHOri, RectWEnd, RectHEnd));
        end;

    //--------------------------------------------------------------------------
    begin
    SetRoundMode(rmTruncate);
    DrawLegend(255, 0, 0,                             0, 0, ImageLegend.Width, Trunc(ImageLegend.Height / 8));
    DrawLegend(255, Trunc(255*(1/3)), 0,              0, Trunc(ImageLegend.Height / 8), ImageLegend.Width, Trunc((ImageLegend.Height * 2) / 8));
    DrawLegend(255, Trunc(255*(2/3)), 0,              0, Trunc((ImageLegend.Height * 2)/ 8), ImageLegend.Width, Trunc((ImageLegend.Height * 3) / 8));
    DrawLegend(255, 255, 0,                           0, Trunc((ImageLegend.Height * 3)/ 8), ImageLegend.Width, Trunc((ImageLegend.Height * 4) / 8));
    DrawLegend(0, 255, 0,                             0, Trunc((ImageLegend.Height * 4)/ 8), ImageLegend.Width, Trunc((ImageLegend.Height * 5) / 8));
    DrawLegend(0, Trunc(255*(2/3)), Trunc(255*(1/3)), 0, Trunc((ImageLegend.Height * 5)/ 8), ImageLegend.Width, Trunc((ImageLegend.Height * 6) / 8));
    DrawLegend(0, Trunc(255*(1/3)), Trunc(255*(2/3)), 0, Trunc((ImageLegend.Height * 6)/ 8), ImageLegend.Width, Trunc((ImageLegend.Height * 7) / 8));
    DrawLegend(0, 0, 255,                             0, Trunc((ImageLegend.Height * 7)/ 8), ImageLegend.Width, ImageLegend.Height);
    end;

//==============================================================================
procedure TFormDrawContour.ButtonExportImageClick(Sender: TObject);
    var
        ConClip   : TBitmap;
        ClipRect  : TRect;
    begin
    ConClip := TBitmap.Create;
    ConClip.PixelFormat := pf24bit;
    ConClip.Width  := PanelMain.ClientWidth;
    ConClip.Height := PanelMain.ClientHeight;
    ClipRect := Rect(0, 0, ConClip.Width, ConClip.Height);
    ConClip.Canvas.CopyRect(ClipRect, TClipPanel(PanelMain).Canvas, ClipRect);
    if SaveDialogContourImage.Execute then
        ConClip.SaveToFile(SaveDialogContourImage.FileName);

    FreeAndNil(ConClip);
    end;

//==============================================================================
procedure TFormDrawContour.ChangeLegendLabels(const max, middle, min: integer);
    begin
    SetRoundMode(rmTruncate);
    LabelMaxmax.Caption       := max.ToString;
    LabelMaxmiddle.Caption    := Round((max + Round((max + middle) / 2)) / 2).ToString;
    LabelMaxmin.Caption       := Round((max + middle) / 2).ToString;
    LabelMiddlemax.Caption    := Round((Round((max + middle) / 2) + middle) / 2).ToString;
    LabelMiddlemiddle.Caption := middle.ToString;
    LabelMiddlemin.Caption    := Round((middle + Round((middle + min) / 2)) / 2).ToString;
    LabelMinmax.Caption       := Round((middle + min) / 2).ToString;
    LabelMinmiddle.Caption    := Round((Round((middle + min) / 2) + min) / 2).ToString;
    LabelMinmin.Caption       := min.ToString+'(人)';
    end;

//==============================================================================
procedure TFormDrawContour.DrawContour(const ConData: TBitmap; MaxNum, MiddleNum, MinNum: integer);
    begin
    p_DrawData := TBitmap.Create;
    p_DrawData := ConData;
    p_AspectRaitoCon := p_DrawData.Width / p_DrawData.Height;
    ChangeLegendLabels(MaxNum, MiddleNum, MinNum);
    Draw;
    end;

//==============================================================================
procedure TFormDrawContour.Draw;
    var
        DrawRect : TRect;
    begin
    SetDrawsSize;
    DrawRect := Rect(0, 0, ImageContour.Width, ImageContour.Height);
    ImageContour.Canvas.StretchDraw(DrawRect, p_DrawData);
    end;

//==============================================================================
procedure TFormDrawContour.SetDrawsSize;
    var
        tmpbmp : TBitmap;
    begin
    ImageContour.Width  := Round(ImageContour.Height * p_AspectRaitoCon);
    tmpbmp := TBitmap.Create;
    tmpbmp.Width  := ImageContour.Width;
    tmpbmp.Height := ImageContour.Height;
    ImageContour.Picture.Bitmap := tmpbmp;
    FreeAndNil(tmpbmp);
    end;

//==============================================================================
procedure TFormDrawContour.FormResize(Sender: TObject);
    begin
    if Assigned(p_DrawData) then
        Draw;
    end;
end.
