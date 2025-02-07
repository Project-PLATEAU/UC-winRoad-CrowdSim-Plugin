object FormProgress: TFormProgress
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'FormProgress'
  ClientHeight = 146
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressBar1: TProgressBar
    AlignWithMargins = True
    Left = 3
    Top = 90
    Width = 293
    Height = 17
    Align = alBottom
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 299
    Height = 87
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 110
    Width = 299
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Button1: TButton
      Left = 104
      Top = 8
      Width = 75
      Height = 25
      Caption = #20013#26029
      ModalResult = 8
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object TimerProgress: TTimer
    Interval = 100
    OnTimer = TimerProgressTimer
    Left = 216
    Top = 16
  end
end
