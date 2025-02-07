object FormAgentDetailSetting: TFormAgentDetailSetting
  Left = 0
  Top = 0
  Margins.Top = 0
  BorderStyle = bsDialog
  Caption = #35443#32048#35373#23450
  ClientHeight = 292
  ClientWidth = 455
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PanelFutter: TPanel
    Left = 0
    Top = 256
    Width = 455
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object BevelFutter: TBevel
      Left = 0
      Top = 0
      Width = 455
      Height = 3
      Align = alTop
      ExplicitLeft = 296
      ExplicitTop = -8
      ExplicitWidth = 50
    end
    object ButtonCancel: TButton
      AlignWithMargins = True
      Left = 377
      Top = 6
      Width = 75
      Height = 27
      Align = alRight
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object ButtonOk: TButton
      AlignWithMargins = True
      Left = 296
      Top = 6
      Width = 75
      Height = 27
      Align = alRight
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 1
    end
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 449
    Height = 243
    Align = alTop
    Caption = #12471#12511#12517#12524#12540#12471#12519#12531#35443#32048#35373#23450
    TabOrder = 1
    object GridPanel1: TGridPanel
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 439
      Height = 31
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 120.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 110.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = Label1
          Row = 0
        end
        item
          Column = 1
          Control = EditMaxWalkSpeed
          Row = 0
        end>
      RowCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 31.000000000000000000
        end>
      TabOrder = 0
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 72
        Height = 13
        Align = alLeft
        Caption = #26368#22823#27497#34892#36895#24230
        Layout = tlCenter
      end
      object EditMaxWalkSpeed: TF8RealSpinEdit
        AlignWithMargins = True
        Left = 123
        Top = 3
        Width = 104
        Height = 25
        Align = alClient
        TabOrder = 0
        Value = 4.000000000000000000
        MaxNumericCharacters = 9
        MaxValue = 6.000000000000000000
        Increment = 1.000000000000000000
        DecimalPlaces = 2
        Tail = 'km/h'
        ExplicitHeight = 21
      end
    end
    object GridPanel2: TGridPanel
      AlignWithMargins = True
      Left = 5
      Top = 49
      Width = 439
      Height = 26
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 120.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 110.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 60.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 110.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = Label3
          Row = 0
        end
        item
          Column = 1
          Control = EditPedestrainRadiusSunny
          Row = 0
        end
        item
          Column = 2
          Control = Label6
          Row = 0
        end
        item
          Column = 3
          Control = EditPedestrainRadiusRainny
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      object Label3: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 108
        Height = 13
        Align = alLeft
        Anchors = []
        Caption = #27497#34892#32773#21322#24452#12288#26228#22825#26178':'
        Layout = tlCenter
      end
      object EditPedestrainRadiusSunny: TF8RealSpinEdit
        AlignWithMargins = True
        Left = 123
        Top = 3
        Width = 104
        Height = 20
        Align = alClient
        Anchors = []
        TabOrder = 0
        Value = 1.000000000000000000
        MaxNumericCharacters = 9
        MaxValue = 9.000000000000000000
        MinValue = 0.010000000000000000
        Increment = 0.100000000000000000
        DecimalPlaces = 2
        Tail = 'm'
        ExplicitHeight = 21
      end
      object Label6: TLabel
        AlignWithMargins = True
        Left = 247
        Top = 3
        Width = 40
        Height = 13
        Align = alRight
        Caption = #38632#22825#26178':'
        Layout = tlCenter
      end
      object EditPedestrainRadiusRainny: TF8RealSpinEdit
        AlignWithMargins = True
        Left = 293
        Top = 3
        Width = 104
        Height = 20
        Align = alClient
        Anchors = []
        TabOrder = 1
        Value = 2.000000000000000000
        MaxNumericCharacters = 9
        MaxValue = 9.000000000000000000
        MinValue = 0.010000000000000000
        Increment = 0.100000000000000000
        DecimalPlaces = 2
        Tail = 'm'
        ExplicitHeight = 21
      end
    end
    object GridPanel4: TGridPanel
      AlignWithMargins = True
      Left = 5
      Top = 75
      Width = 439
      Height = 66
      Margins.Top = 0
      Align = alTop
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 120.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 110.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = Label2
          Row = 0
        end
        item
          Column = 1
          Control = EditSimPerPedestrainData
          Row = 0
        end
        item
          Column = 0
          Control = Label4
          Row = 1
        end
        item
          Column = 1
          Control = F8RealSpinEdit1
          Row = 1
        end>
      RowCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 26.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 26.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 2
      object Label2: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 88
        Height = 13
        Align = alLeft
        Anchors = []
        Caption = #20154#27969#12487#12540#12479#21033#29992#29575
        Layout = tlCenter
      end
      object EditSimPerPedestrainData: TF8RealSpinEdit
        AlignWithMargins = True
        Left = 123
        Top = 3
        Width = 104
        Height = 20
        Align = alClient
        Anchors = []
        TabOrder = 0
        Value = 100.000000000000000000
        MaxNumericCharacters = 9
        MaxValue = 100.000000000000000000
        Increment = 1.000000000000000000
        Tail = ' %'
        ExplicitHeight = 21
      end
      object Label4: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 29
        Width = 72
        Height = 13
        Align = alLeft
        Anchors = []
        Caption = #26908#20986#24231#27161#35492#24046
        Layout = tlCenter
      end
      object F8RealSpinEdit1: TF8RealSpinEdit
        AlignWithMargins = True
        Left = 123
        Top = 29
        Width = 104
        Height = 20
        Align = alClient
        Anchors = []
        TabOrder = 1
        Value = 8.000000000000000000
        MaxNumericCharacters = 9
        MaxValue = 100.000000000000000000
        Increment = 1.000000000000000000
        Tail = ' m'
        ExplicitHeight = 21
      end
    end
    object GridPanel3: TGridPanel
      Left = 2
      Top = 144
      Width = 445
      Height = 81
      Align = alTop
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 22.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 100.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 110.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 1
          Control = Label5
          Row = 0
        end
        item
          Column = 0
          Control = CheckBoxMagnificationFactor
          Row = 0
        end
        item
          Column = 3
          Control = GridPanel5
          Row = 0
        end
        item
          Column = 2
          Control = RadioButtonMgUU
          Row = 0
        end
        item
          Column = 2
          Control = RadioButtonMgValue
          Row = 1
        end
        item
          Column = 3
          Control = EditRealMg
          Row = 1
        end
        item
          Column = 2
          Control = RadioButtonMagCrossSection
          Row = 2
        end>
      RowCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 26.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 26.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 26.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 3
      DesignSize = (
        445
        81)
      object Label5: TLabel
        AlignWithMargins = True
        Left = 25
        Top = 3
        Width = 48
        Height = 20
        Align = alLeft
        Caption = #25313#22823#20418#25968
        Layout = tlCenter
        ExplicitHeight = 13
      end
      object CheckBoxMagnificationFactor: TCheckBox
        Left = 1
        Top = 4
        Width = 20
        Height = 17
        Anchors = []
        TabOrder = 0
        OnClick = CheckBoxMagnificationFactorClick
      end
      object GridPanel5: TGridPanel
        Left = 232
        Top = 0
        Width = 213
        Height = 26
        Align = alTop
        BevelOuter = bvNone
        ColumnCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 80.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 10.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 80.000000000000000000
          end
          item
            Value = 100.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 1
            Control = Label7
            Row = 0
          end
          item
            Column = 0
            Control = EditPopulation
            Row = 0
          end
          item
            Column = 2
            Control = EditUU
            Row = 0
          end>
        RowCollection = <
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 1
        object Label7: TLabel
          Left = 80
          Top = 0
          Width = 4
          Height = 13
          Align = alLeft
          Caption = '/'
          Layout = tlCenter
        end
        object EditPopulation: TF8RealSpinEdit
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 74
          Height = 20
          Align = alClient
          TabOrder = 0
          Value = 1.000000000000000000
          MaxNumericCharacters = 9
          MaxValue = 100.000000000000000000
          Increment = 1.000000000000000000
          Tail = ' '#19975#20154
          ExplicitHeight = 21
        end
        object EditUU: TF8RealSpinEdit
          AlignWithMargins = True
          Left = 93
          Top = 3
          Width = 74
          Height = 20
          Align = alClient
          TabOrder = 1
          Value = 1000.000000000000000000
          MaxNumericCharacters = 9
          MaxValue = 1000000.000000000000000000
          MinValue = 0.010000000000000000
          Increment = 1.000000000000000000
          Tail = ' '#19975#20154
          ExplicitHeight = 21
        end
      end
      object RadioButtonMgUU: TRadioButton
        Left = 122
        Top = 0
        Width = 100
        Height = 26
        Align = alLeft
        Caption = #20154#21475' / UU'#25968
        TabOrder = 2
        OnClick = RadioMagnificationClick
      end
      object RadioButtonMgValue: TRadioButton
        Left = 122
        Top = 26
        Width = 94
        Height = 26
        Align = alLeft
        Caption = #23455#25968#20516
        TabOrder = 3
        OnClick = RadioMagnificationClick
      end
      object EditRealMg: TF8RealSpinEdit
        AlignWithMargins = True
        Left = 235
        Top = 29
        Width = 74
        Height = 20
        Align = alLeft
        TabOrder = 4
        MaxNumericCharacters = 9
        MaxValue = 100.000000000000000000
        Increment = 1.000000000000000000
        Tail = 'x'
        ExplicitHeight = 21
      end
      object RadioButtonMagCrossSection: TRadioButton
        Left = 122
        Top = 52
        Width = 107
        Height = 26
        Align = alLeft
        Caption = #26029#38754#20132#36890#27969#12487#12540#12479
        TabOrder = 5
        OnClick = RadioMagnificationClick
      end
    end
  end
end
