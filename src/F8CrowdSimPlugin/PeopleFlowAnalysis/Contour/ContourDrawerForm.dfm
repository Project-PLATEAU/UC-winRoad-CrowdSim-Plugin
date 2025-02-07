object FormDrawContour: TFormDrawContour
  Left = 0
  Top = 0
  Caption = #12467#12531#12479#12540#29983#25104#32080#26524
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
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 669
    Height = 392
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object PanelContour: TPanel
      Left = 0
      Top = 0
      Width = 528
      Height = 392
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object ImageContour: TImage
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 422
        Height = 386
        Align = alLeft
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
      object LabelMaxmax: TLabel
        Left = 61
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
      object LabelMaxmiddle: TLabel
        Left = 61
        Top = 92
        Width = 18
        Height = 19
        Caption = '88'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object LabelMaxmin: TLabel
        Left = 61
        Top = 129
        Width = 18
        Height = 19
        Caption = '75'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object LabelMiddlemax: TLabel
        Left = 61
        Top = 167
        Width = 18
        Height = 19
        Caption = '62'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object LabelMinmin: TLabel
        Left = 61
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
      object LabelMinmiddle: TLabel
        Left = 61
        Top = 320
        Width = 18
        Height = 19
        Caption = '12'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object LabelMiddlemiddle: TLabel
        Left = 61
        Top = 205
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
      object LabelMiddlemin: TLabel
        Left = 61
        Top = 243
        Width = 18
        Height = 19
        Caption = '38'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object LabelMinmax: TLabel
        Left = 61
        Top = 282
        Width = 18
        Height = 19
        Caption = '25'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object ImageLegend: TImage
        Left = 20
        Top = 64
        Width = 35
        Height = 305
      end
    end
  end
  object PanelFotter: TPanel
    Left = 0
    Top = 392
    Width = 669
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
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
  object SaveDialogContourImage: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 
      'png'#12501#12449#12452#12523'(*.png)|*.png|jpeg'#12501#12449#12452#12523'(*.jpeg)|*.jpeg|bmp'#12501#12449#12452#12523'(*.bmp)|*.bm' +
      'p'
    Left = 8
    Top = 400
  end
end
