object FormMFJsonLoaderSetting: TFormMFJsonLoaderSetting
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'MF-JSON '#12452#12531#12509#12540#12488#35373#23450
  ClientHeight = 558
  ClientWidth = 891
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBottom: TPanel
    Left = 0
    Top = 522
    Width = 891
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 891
      Height = 3
      Align = alTop
      ExplicitTop = -3
    end
    object buttonImportMFJson: TBitBtn
      AlignWithMargins = True
      Left = 801
      Top = 6
      Width = 87
      Height = 27
      Align = alRight
      Caption = #12452#12531#12509#12540#12488
      TabOrder = 0
      OnClick = buttonImportMFJsonClick
    end
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 885
    Height = 516
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object PanelModels: TPanel
      Left = 0
      Top = 229
      Width = 885
      Height = 287
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object Panel2: TPanel
        AlignWithMargins = True
        Left = 104
        Top = 3
        Width = 778
        Height = 281
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Panel3: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 772
          Height = 26
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object SpeedButtonOpen: TSpeedButton
            Left = 0
            Top = 0
            Width = 26
            Height = 26
            Align = alLeft
            ImageIndex = 0
            Images = ImageList1
            OnClick = SpeedButtonOpenClick
            ExplicitHeight = 772
          end
        end
        object GroupBoxFemale: TGroupBox
          AlignWithMargins = True
          Left = 394
          Top = 36
          Width = 376
          Height = 239
          Margins.Left = 8
          Margins.Top = 4
          Margins.Right = 8
          Margins.Bottom = 6
          Align = alRight
          Caption = #22899#24615#12514#12487#12523
          TabOrder = 1
          object GridPanelFemale: TGridPanel
            Left = 2
            Top = 15
            Width = 372
            Height = 222
            Align = alClient
            BevelOuter = bvNone
            ColumnCollection = <
              item
                SizeStyle = ssAbsolute
                Value = 60.000000000000000000
              end
              item
                SizeStyle = ssAbsolute
                Value = 100.000000000000000000
              end
              item
                SizeStyle = ssAbsolute
                Value = 100.000000000000000000
              end
              item
                SizeStyle = ssAbsolute
                Value = 100.000000000000000000
              end
              item
                Value = 100.000000000000000000
              end>
            ControlCollection = <
              item
                Column = 0
                Control = Label9
                Row = 2
              end
              item
                Column = 1
                Control = ComboBoxFemaleUnder20
                Row = 1
              end
              item
                Column = 2
                Control = ComboBoxFemaleUnder60
                Row = 1
              end
              item
                Column = 3
                Control = ComboBoxFemaleOver60
                Row = 1
              end
              item
                Column = 1
                Control = ComboBoxFemaleunder20Rain
                Row = 2
              end
              item
                Column = 2
                Control = ComboBoxFemaleUnder60Rain
                Row = 2
              end
              item
                Column = 3
                Control = ComboBoxFemaleover60Rain
                Row = 2
              end
              item
                Column = 0
                Control = Label8
                Row = 1
              end
              item
                Column = 1
                Control = LabelFemaleUnder20
                Row = 0
              end
              item
                Column = 3
                Control = LabelFemaleOver20
                Row = 0
              end
              item
                Column = 2
                Control = LabelFemaleUnder60
                Row = 0
              end>
            RowCollection = <
              item
                SizeStyle = ssAbsolute
                Value = 26.000000000000000000
              end
              item
                SizeStyle = ssAbsolute
                Value = 100.000000000000000000
              end
              item
                SizeStyle = ssAbsolute
                Value = 100.000000000000000000
              end
              item
                Value = 100.000000000000000000
              end>
            TabOrder = 0
            DesignSize = (
              372
              222)
            object Label9: TLabel
              Left = 12
              Top = 169
              Width = 36
              Height = 13
              Anchors = []
              Caption = #38632#22825#26178
              ExplicitLeft = 28
            end
            object ComboBoxFemaleUnder20: TComboBox
              Tag = 3
              AlignWithMargins = True
              Left = 66
              Top = 33
              Width = 87
              Height = 80
              Margins.Left = 6
              Margins.Top = 0
              Margins.Right = 6
              Margins.Bottom = 6
              Style = csOwnerDrawFixed
              Anchors = []
              ItemHeight = 74
              MaxLength = 6
              TabOrder = 0
              OnChange = ComboBoxChange
              OnDrawItem = ComboBoxDrawItem
            end
            object ComboBoxFemaleUnder60: TComboBox
              Tag = 4
              AlignWithMargins = True
              Left = 166
              Top = 33
              Width = 87
              Height = 80
              Margins.Left = 6
              Margins.Top = 0
              Margins.Right = 6
              Margins.Bottom = 6
              Style = csOwnerDrawFixed
              Anchors = []
              ItemHeight = 74
              MaxLength = 6
              TabOrder = 1
              OnChange = ComboBoxChange
              OnDrawItem = ComboBoxDrawItem
            end
            object ComboBoxFemaleOver60: TComboBox
              Tag = 5
              AlignWithMargins = True
              Left = 266
              Top = 33
              Width = 87
              Height = 80
              Margins.Left = 6
              Margins.Top = 0
              Margins.Right = 6
              Margins.Bottom = 6
              Style = csOwnerDrawFixed
              Anchors = []
              ItemHeight = 74
              MaxLength = 6
              TabOrder = 2
              OnChange = ComboBoxChange
              OnDrawItem = ComboBoxDrawItem
            end
            object ComboBoxFemaleunder20Rain: TComboBox
              Tag = 9
              AlignWithMargins = True
              Left = 66
              Top = 133
              Width = 87
              Height = 80
              Margins.Left = 6
              Margins.Top = 0
              Margins.Right = 6
              Margins.Bottom = 6
              Style = csOwnerDrawFixed
              Anchors = []
              ItemHeight = 74
              MaxLength = 6
              TabOrder = 3
              OnChange = ComboBoxChange
              OnDrawItem = ComboBoxDrawItem
            end
            object ComboBoxFemaleUnder60Rain: TComboBox
              Tag = 10
              AlignWithMargins = True
              Left = 166
              Top = 133
              Width = 87
              Height = 80
              Margins.Left = 6
              Margins.Top = 0
              Margins.Right = 6
              Margins.Bottom = 6
              Style = csOwnerDrawFixed
              Anchors = []
              ItemHeight = 74
              MaxLength = 6
              TabOrder = 4
              OnChange = ComboBoxChange
              OnDrawItem = ComboBoxDrawItem
            end
            object ComboBoxFemaleover60Rain: TComboBox
              Tag = 5
              AlignWithMargins = True
              Left = 266
              Top = 133
              Width = 87
              Height = 80
              Margins.Left = 6
              Margins.Top = 0
              Margins.Right = 6
              Margins.Bottom = 6
              Style = csOwnerDrawFixed
              Anchors = []
              ItemHeight = 74
              MaxLength = 6
              TabOrder = 5
              OnChange = ComboBoxChange
              OnDrawItem = ComboBoxDrawItem
            end
            object Label8: TLabel
              Left = 12
              Top = 69
              Width = 36
              Height = 13
              Anchors = []
              Caption = #26228#22825#26178
              ExplicitLeft = 28
            end
            object LabelFemaleUnder20: TLabel
              Left = 92
              Top = 6
              Width = 36
              Height = 13
              Anchors = []
              Caption = #65374'19'#27507
              ExplicitLeft = 98
            end
            object LabelFemaleOver20: TLabel
              Left = 292
              Top = 6
              Width = 36
              Height = 13
              Anchors = []
              Caption = '60'#27507#65374
              ExplicitLeft = 198
            end
            object LabelFemaleUnder60: TLabel
              Left = 186
              Top = 6
              Width = 48
              Height = 13
              Anchors = []
              Caption = '20'#65374'59'#27507
              ExplicitLeft = 182
              ExplicitTop = 17
            end
          end
        end
        object GroupBoxMale: TGroupBox
          AlignWithMargins = True
          Left = 9
          Top = 36
          Width = 369
          Height = 239
          Margins.Left = 8
          Margins.Top = 4
          Margins.Right = 8
          Margins.Bottom = 6
          Align = alRight
          Caption = #30007#24615#12514#12487#12523
          TabOrder = 2
          object GridPanelMale: TGridPanel
            Left = 2
            Top = 15
            Width = 365
            Height = 222
            Align = alClient
            BevelOuter = bvNone
            ColumnCollection = <
              item
                SizeStyle = ssAbsolute
                Value = 60.000000000000000000
              end
              item
                SizeStyle = ssAbsolute
                Value = 100.000000000000000000
              end
              item
                SizeStyle = ssAbsolute
                Value = 100.000000000000000000
              end
              item
                SizeStyle = ssAbsolute
                Value = 100.000000000000000000
              end
              item
                Value = 100.000000000000000000
              end>
            ControlCollection = <
              item
                Column = 1
                Control = LabelMaleUnder20
                Row = 0
              end
              item
                Column = 2
                Control = LabelMaleUnder60
                Row = 0
              end
              item
                Column = 3
                Control = LabelMaleOver60
                Row = 0
              end
              item
                Column = 0
                Control = Label3
                Row = 1
              end
              item
                Column = 1
                Control = ComboBoxMaleUnder20
                Row = 1
              end
              item
                Column = 2
                Control = ComboBoxMaleUnder60
                Row = 1
              end
              item
                Column = 3
                Control = ComboBoxMaleOver60
                Row = 1
              end
              item
                Column = 0
                Control = Label5
                Row = 2
              end
              item
                Column = 1
                Control = ComboBoxMaleUnder20Rain
                Row = 2
              end
              item
                Column = 2
                Control = ComboBoxMaleUnder60Rain
                Row = 2
              end
              item
                Column = 3
                Control = ComboBoxMaleOver60Rain
                Row = 2
              end>
            RowCollection = <
              item
                SizeStyle = ssAbsolute
                Value = 26.000000000000000000
              end
              item
                SizeStyle = ssAbsolute
                Value = 100.000000000000000000
              end
              item
                SizeStyle = ssAbsolute
                Value = 100.000000000000000000
              end
              item
                Value = 100.000000000000000000
              end>
            TabOrder = 0
            DesignSize = (
              365
              222)
            object LabelMaleUnder20: TLabel
              Left = 92
              Top = 6
              Width = 36
              Height = 13
              Anchors = []
              Caption = #65374'19'#27507
              ExplicitLeft = 98
            end
            object LabelMaleUnder60: TLabel
              Left = 186
              Top = 6
              Width = 48
              Height = 13
              Anchors = []
              Caption = '20'#65374'59'#27507
              ExplicitLeft = 192
            end
            object LabelMaleOver60: TLabel
              Left = 292
              Top = 6
              Width = 36
              Height = 13
              Anchors = []
              Caption = '60'#27507#65374
              ExplicitLeft = 298
            end
            object Label3: TLabel
              Left = 12
              Top = 69
              Width = 36
              Height = 13
              Anchors = []
              Caption = #26228#22825#26178
              ExplicitLeft = 7
            end
            object ComboBoxMaleUnder20: TComboBox
              AlignWithMargins = True
              Left = 66
              Top = 33
              Width = 87
              Height = 80
              Margins.Left = 6
              Margins.Top = 0
              Margins.Right = 6
              Margins.Bottom = 6
              Style = csOwnerDrawFixed
              Anchors = []
              ItemHeight = 74
              MaxLength = 6
              TabOrder = 0
              OnChange = ComboBoxChange
              OnDrawItem = ComboBoxDrawItem
            end
            object ComboBoxMaleUnder60: TComboBox
              Tag = 1
              AlignWithMargins = True
              Left = 166
              Top = 33
              Width = 87
              Height = 80
              Margins.Left = 6
              Margins.Top = 0
              Margins.Right = 6
              Margins.Bottom = 6
              Style = csOwnerDrawFixed
              Anchors = []
              ItemHeight = 74
              MaxLength = 6
              TabOrder = 1
              OnChange = ComboBoxChange
              OnDrawItem = ComboBoxDrawItem
            end
            object ComboBoxMaleOver60: TComboBox
              Tag = 2
              AlignWithMargins = True
              Left = 266
              Top = 33
              Width = 87
              Height = 80
              Margins.Left = 6
              Margins.Top = 0
              Margins.Right = 6
              Margins.Bottom = 6
              Style = csOwnerDrawFixed
              Anchors = []
              ItemHeight = 74
              MaxLength = 6
              TabOrder = 2
              OnChange = ComboBoxChange
              OnDrawItem = ComboBoxDrawItem
            end
            object Label5: TLabel
              Left = 12
              Top = 169
              Width = 36
              Height = 13
              Anchors = []
              Caption = #38632#22825#26178
              ExplicitLeft = 28
            end
            object ComboBoxMaleUnder20Rain: TComboBox
              Tag = 6
              AlignWithMargins = True
              Left = 66
              Top = 133
              Width = 87
              Height = 80
              Margins.Left = 6
              Margins.Top = 0
              Margins.Right = 6
              Margins.Bottom = 6
              Style = csOwnerDrawFixed
              Anchors = []
              ItemHeight = 74
              MaxLength = 6
              TabOrder = 3
              OnChange = ComboBoxChange
              OnDrawItem = ComboBoxDrawItem
            end
            object ComboBoxMaleUnder60Rain: TComboBox
              Tag = 7
              AlignWithMargins = True
              Left = 166
              Top = 133
              Width = 87
              Height = 80
              Margins.Left = 6
              Margins.Top = 0
              Margins.Right = 6
              Margins.Bottom = 6
              Style = csOwnerDrawFixed
              Anchors = []
              ItemHeight = 74
              MaxLength = 6
              TabOrder = 4
              OnChange = ComboBoxChange
              OnDrawItem = ComboBoxDrawItem
            end
            object ComboBoxMaleOver60Rain: TComboBox
              Tag = 8
              AlignWithMargins = True
              Left = 266
              Top = 133
              Width = 87
              Height = 80
              Margins.Left = 6
              Margins.Top = 0
              Margins.Right = 6
              Margins.Bottom = 6
              Style = csOwnerDrawFixed
              Anchors = []
              ItemHeight = 74
              MaxLength = 6
              TabOrder = 5
              OnChange = ComboBoxChange
              OnDrawItem = ComboBoxDrawItem
            end
          end
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 101
        Height = 287
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 101
          Height = 129
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            101
            129)
          object Label1: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 95
            Height = 13
            Align = alTop
            Caption = #12487#12501#12457#12523#12488#12514#12487#12523
            ExplicitWidth = 73
          end
          object ComboBoxUnknown: TComboBox
            Tag = 12
            AlignWithMargins = True
            Left = 8
            Top = 27
            Width = 87
            Height = 80
            Margins.Left = 6
            Margins.Top = 0
            Margins.Right = 6
            Margins.Bottom = 6
            Style = csOwnerDrawFixed
            Anchors = []
            ItemHeight = 74
            MaxLength = 6
            TabOrder = 0
            OnChange = ComboBoxChange
            OnDrawItem = ComboBoxDrawItem
          end
        end
      end
    end
    inline FrameMfMultiLoader1: TFrameMfMultiLoader
      Left = 0
      Top = 0
      Width = 885
      Height = 229
      Align = alClient
      TabOrder = 1
      ExplicitWidth = 885
      ExplicitHeight = 229
      inherited GroupBoxImportFiles: TGroupBox
        Width = 885
        Height = 229
        ExplicitWidth = 885
        ExplicitHeight = 229
        inherited Panel3: TPanel
          Left = 855
          Height = 212
          ExplicitLeft = 855
          ExplicitHeight = 212
        end
        inherited Panel1: TPanel
          Width = 853
          Height = 212
          ExplicitWidth = 853
          ExplicitHeight = 212
          inherited ListBoxReadFiles: TListBox
            Width = 847
            Height = 174
            ExplicitWidth = 847
            ExplicitHeight = 174
          end
          inherited Panel2: TPanel
            Top = 183
            Width = 847
            ExplicitTop = 183
            ExplicitWidth = 847
            inherited Label1: TLabel
              Height = 26
            end
            inherited ButtonDelete: TButton
              Left = 772
              ExplicitLeft = 772
            end
            inherited ButtonAdd: TButton
              Left = 697
              ExplicitLeft = 697
            end
          end
        end
      end
    end
  end
  object OpenDialog: TOpenDialog
    Left = 240
    Top = 126
  end
  object ImageList1: TImageList
    AllocBy = 1
    Left = 440
    Top = 248
    Bitmap = {
      494C010102000800010010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF004E855C000251170002511700025117000251170051876000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00025117000E94300006BB340006BB34000E94300002511700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00025117000D962F0005C3340005C434000D952F0002511700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00025117000D98310003CF350003D135000D98300002511700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00025117000D99300002D6370002D636000D9A300002511700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000846C500054340900533209004F30
      07004F2F06004B2C04004A2B0500482902004829020048290200482902004829
      02004829020048290200492A03007D674C005187600002511700025117000251
      170002511700025117000E9C300001DA360001DD38000C9C3000025117000251
      1700025117000251170002511700538861000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005C3B0D00DB943C00D8933B00D791
      3A00D6913B00D5903900D48F3A00D38E3800D18E3900D18E3700D18E3700D18E
      3700D18E3700D28E3800D48F39004B2C05000251170036A4530031A34E0027A2
      4500169B37000D9A30000C9F300002D5360002D736000BA130000E9A30000D98
      2F000C962F000E942F000D923000105D25000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000069441300C8873300C7873100C786
      3100C7863100C7863100C7863100C7863100C7863100C7863100C7863100C786
      3100C7863100C7873100C98832004C2D06000251170055BA70004EBC69003EBE
      5E0036C25A000EBF3A0004C1340004C7340004C9350004C5340005C1340006B8
      320007B1320009A530000AA03000095B20000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000714A1600A7742E00A4702800A470
      2800A4702800A4702800A4702800A4702800A4702800A4702800A4702800A470
      2800A4702800A6702800A9752F00543309000251170062BB780058BD710049BF
      660041C1610034C5580017C1410005C1340005C1340005BE330006BC330007B4
      320008AD31000AA430000B9E2F00095B20000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007C521B00AA814900AA804800A97F
      4800AA7F4800A97E4800A77D4700A57C4600A47B4600A37A4500A27945009F78
      44009F794400997442009874440065401000025117005AB6720054B26C004AB0
      640044AF600039AC550040B25D0033C257001DBB45000B9C2E000C9930000C99
      31000C962F000D932F000D9230000F5D24000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000987E59007C5219007C5219007C52
      19007C5219007B5119007A501900774E1700764D1700734B1600724A15006F48
      14006E4713006C4512006A4411008D7555005187600002511700025117000251
      170002511700025117003CAD57003EBF5D0036BE590022A14200025117000251
      17000251170002511700025117004D845C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00025117004BB1650052BD6D004BBA67002FA54D0002511700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000251170053B36B005DBC750056B96F0037A6530002511700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000251170061BA770072BD85006BB97F0044AB5D0002511700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00025117006BBC7F007CBF8E0075BB88004CAC650002511700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF004C845B000251170002511700025117000251170051875F00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
end
