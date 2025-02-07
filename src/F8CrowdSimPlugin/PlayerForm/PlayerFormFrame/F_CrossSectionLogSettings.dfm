object FrameCrossSectionLogSettings: TFrameCrossSectionLogSettings
  Left = 0
  Top = 0
  Width = 615
  Height = 559
  TabOrder = 0
  object PageControlCSLogSettings: TPageControl
    Left = 0
    Top = 0
    Width = 615
    Height = 559
    ActivePage = TabSheetCSLogSettings
    Align = alClient
    TabOrder = 0
    OnChange = PageControlCSLogSettingsChange
    object TabSheetCSLogSettings: TTabSheet
      Caption = #12525#12464#20986#21147#35373#23450
      TabVisible = False
      object PanelDataInterval: TPanel
        Left = 0
        Top = 0
        Width = 607
        Height = 72
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object GroupBoxDataInterval: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 601
          Height = 66
          Align = alClient
          Caption = #12498#12540#12488#12510#12483#12503#12539#12467#12531#12479#12540#35373#23450
          TabOrder = 0
          object LabelCellDataAddInterval: TLabel
            Left = 17
            Top = 29
            Width = 100
            Height = 13
            Caption = #36890#36942#12487#12540#12479#38598#35336#38291#38548
          end
          object SpinEditCellDataAddInterval: TF8RealSpinEdit
            Left = 123
            Top = 26
            Width = 73
            Height = 21
            TabOrder = 0
            OnChange = SpinEditIntervalsChange
            Value = 5.000000000000000000
            MaxNumericCharacters = 9
            MaxValue = 9999.000000000000000000
            MinValue = 5.000000000000000000
            Increment = 5.000000000000000000
            Tail = #20998
          end
        end
      end
      object PanelCSLog: TPanel
        Left = 0
        Top = 72
        Width = 607
        Height = 477
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object GroupBoxSensorArea: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 601
          Height = 471
          Align = alClient
          Caption = #26029#38754#20132#36890#27969#35373#23450
          TabOrder = 0
          object PanelSensorAreaInfo: TPanel
            Left = 329
            Top = 15
            Width = 270
            Height = 454
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object MemoSensorAreaInfos: TMemo
              AlignWithMargins = True
              Left = 20
              Top = 15
              Width = 230
              Height = 419
              Margins.Left = 20
              Margins.Top = 15
              Margins.Right = 20
              Margins.Bottom = 20
              Align = alClient
              ReadOnly = True
              ScrollBars = ssVertical
              TabOrder = 0
            end
          end
          object PanelSensorAreaSettings: TPanel
            Left = 2
            Top = 15
            Width = 327
            Height = 454
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 1
            object LabelLogExportInterval: TLabel
              Left = 16
              Top = 16
              Width = 66
              Height = 13
              Caption = #12525#12464#20986#21147#38291#38548
            end
            object GroupBoxSensorAreaSettings: TGroupBox
              AlignWithMargins = True
              Left = 10
              Top = 46
              Width = 307
              Height = 187
              Margins.Left = 10
              Margins.Top = 10
              Margins.Right = 10
              Margins.Bottom = 10
              Caption = #35336#28204#31684#22258#25351#23450
              TabOrder = 0
              object LabelAreaName: TLabel
                Left = 12
                Top = 104
                Width = 60
                Height = 13
                Caption = #35336#28204#31684#22258#21517
              end
              object EditSensorAreaName: TEdit
                Left = 10
                Top = 123
                Width = 284
                Height = 21
                TabOrder = 0
              end
              object ButtonSetLeftTop: TButton
                Left = 20
                Top = 27
                Width = 126
                Height = 25
                Caption = #35336#28204#31684#22258#24038#19978#12434#36984#25246
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clTeal
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
                TabOrder = 1
                OnClick = ButtonSetLeftTopClick
              end
              object ButtonSetRightBottom: TButton
                Left = 20
                Top = 65
                Width = 126
                Height = 25
                Caption = #35336#28204#31684#22258#21491#19979#12434#36984#25246
                TabOrder = 2
                OnClick = ButtonSetRightBottomClick
              end
              object ButtonAddSensorAreas: TButton
                Left = 10
                Top = 150
                Width = 284
                Height = 25
                Caption = #35336#28204#31684#22258#12434#36861#21152
                TabOrder = 3
                OnClick = ButtonAddSensorAreasClick
              end
            end
            object SpinEditLogExportInterval: TF8RealSpinEdit
              Left = 88
              Top = 12
              Width = 73
              Height = 21
              TabOrder = 1
              OnChange = SpinEditIntervalsChange
              Value = 5.000000000000000000
              MaxNumericCharacters = 9
              MaxValue = 9999.000000000000000000
              MinValue = 5.000000000000000000
              Increment = 5.000000000000000000
              Tail = #20998
            end
            object GroupBoxSelectSensorArea: TGroupBox
              Left = 10
              Top = 246
              Width = 307
              Height = 107
              Caption = #35336#28204#31684#22258#30906#35469
              TabOrder = 2
              object LabelAreaDrawSelect: TLabel
                Left = 12
                Top = 24
                Width = 48
                Height = 13
                Caption = #35336#28204#31684#22258
              end
              object ComboBoxDrawAreaName: TComboBox
                Left = 10
                Top = 43
                Width = 284
                Height = 21
                Style = csDropDownList
                TabOrder = 0
                OnChange = ComboBoxDrawAreaNameChange
              end
              object ButtonDeleteSensorArea: TButton
                Left = 10
                Top = 70
                Width = 284
                Height = 25
                Caption = #36984#25246#20013#12398#35336#28204#31684#22258#12434#21066#38500
                TabOrder = 1
                OnClick = ButtonDeleteSensorAreaClick
              end
            end
          end
        end
      end
    end
  end
end
