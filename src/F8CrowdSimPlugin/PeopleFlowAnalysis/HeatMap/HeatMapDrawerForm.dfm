object FormDrawHeatMap: TFormDrawHeatMap
  Left = 0
  Top = 0
  Caption = #12498#12540#12488#12510#12483#12503#29983#25104#32080#26524
  ClientHeight = 433
  ClientWidth = 669
  Color = clBtnFace
  Constraints.MinHeight = 472
  Constraints.MinWidth = 685
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object PanelFotter: TPanel
    Left = 0
    Top = 392
    Width = 669
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object ButtonExportImage: TButton
      AlignWithMargins = True
      Left = 50
      Top = 7
      Width = 569
      Height = 27
      Margins.Left = 50
      Margins.Top = 7
      Margins.Right = 50
      Margins.Bottom = 7
      Align = alClient
      Caption = #30011#20687#12392#12375#12390#20445#23384#12377#12427
      TabOrder = 0
      OnClick = ButtonExportImageClick
    end
  end
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 669
    Height = 392
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object PanelHeatMap: TPanel
      Left = 0
      Top = 0
      Width = 528
      Height = 392
      Align = alClient
      BevelOuter = bvNone
      Constraints.MaxHeight = 99999
      Constraints.MaxWidth = 99999
      TabOrder = 0
      object ImageHeatMap: TImage
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 422
        Height = 386
        Align = alLeft
        Constraints.MaxHeight = 99999
        Constraints.MaxWidth = 99999
        ExplicitHeight = 326
      end
    end
    object PanelLegend: TPanel
      Left = 528
      Top = 0
      Width = 141
      Height = 392
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object ImageLegendBtoG: TImage
        AlignWithMargins = True
        Left = 20
        Top = 293
        Width = 35
        Height = 78
        Margins.Left = 20
        Margins.Top = 30
        Margins.Right = 40
        Margins.Bottom = 30
      end
      object ImageLegendYtoR: TImage
        AlignWithMargins = True
        Left = 20
        Top = 62
        Width = 35
        Height = 155
        Margins.Left = 20
        Margins.Top = 30
        Margins.Right = 40
        Margins.Bottom = 30
      end
      object LabelLegend: TLabel
        Left = 18
        Top = 24
        Width = 40
        Height = 24
        Caption = #20961#20363
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object LabelMaxNum: TLabel
        Left = 58
        Top = 54
        Width = 27
        Height = 19
        Caption = '100'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object LabelMiddleNum: TLabel
        Left = 58
        Top = 206
        Width = 18
        Height = 19
        Caption = '50'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object LabelMinNum: TLabel
        Left = 58
        Top = 358
        Width = 37
        Height = 19
        Caption = '0('#20154')'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object ImageLegendGtoY: TImage
        AlignWithMargins = True
        Left = 20
        Top = 216
        Width = 35
        Height = 78
        Margins.Left = 20
        Margins.Top = 30
        Margins.Right = 40
        Margins.Bottom = 30
      end
    end
  end
  object SaveDialogHeatMapImage: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 
      'png'#12501#12449#12452#12523'(*.png)|*.png|jpeg'#12501#12449#12452#12523'(*.jpeg)|*.jpeg|bmp'#12501#12449#12452#12523'(*.bmp)|*.bm' +
      'p'
    Left = 8
    Top = 400
  end
end
