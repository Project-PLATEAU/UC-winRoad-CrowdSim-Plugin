object FormCrowdSimMain: TFormCrowdSimMain
  Left = 0
  Top = 0
  Caption = 'MENGE '#36890#20449#12471#12511#12517#12524#12540#12479
  ClientHeight = 417
  ClientWidth = 660
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    660
    417)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelPortNumber: TLabel
    Left = 8
    Top = 11
    Width = 95
    Height = 13
    Caption = #24453#12385#21463#12369#12509#12540#12488#30058#21495
  end
  object SpinEditPort: TSpinEdit
    Left = 109
    Top = 8
    Width = 69
    Height = 22
    MaxValue = 65535
    MinValue = 0
    TabOrder = 0
    Value = 0
  end
  object ButtonStart: TButton
    Left = 530
    Top = 16
    Width = 75
    Height = 25
    Action = ActionStart
    TabOrder = 1
  end
  object ButtonStop: TButton
    Left = 530
    Top = 47
    Width = 75
    Height = 25
    Action = ActionFinish
    TabOrder = 2
  end
  object MemoStatus: TMemo
    Left = 8
    Top = 94
    Width = 644
    Height = 315
    Anchors = [akLeft, akRight, akBottom]
    Lines.Strings = (
      'MemoStatus')
    TabOrder = 3
  end
  object IdUDPServer: TIdUDPServer
    Bindings = <>
    DefaultPort = 0
    OnUDPRead = IdUDPServerUDPRead
    Left = 248
    Top = 16
  end
  object ActionList: TActionList
    Left = 320
    Top = 16
    object ActionStart: TAction
      Category = 'Connection'
      Caption = #38283#22987
      OnExecute = ActionStartExecute
      OnUpdate = ActionStartUpdate
    end
    object ActionFinish: TAction
      Category = 'Connection'
      Caption = #32066#20102
      OnExecute = ActionFinishExecute
      OnUpdate = ActionFinishUpdate
    end
  end
  object TimerStepDisplay: TTimer
    Enabled = False
    OnTimer = TimerStepDisplayTimer
    Left = 16
    Top = 40
  end
end
