object FormMengeCommunicateSetting: TFormMengeCommunicateSetting
  Left = 0
  Top = 0
  Caption = 'menge'#36890#20449#35373#23450
  ClientHeight = 198
  ClientWidth = 438
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
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 432
    Height = 150
    Align = alTop
    Caption = 'Menge'#25509#32154#35373#23450
    TabOrder = 0
    object Panel1: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 422
      Height = 21
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object PanelHostIpAddress: TPanel
        Left = 0
        Top = 0
        Width = 170
        Height = 21
        Margins.Left = 8
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alLeft
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 0
        object LabelIpAddress: TLabel
          Left = 0
          Top = 0
          Width = 59
          Height = 21
          Align = alLeft
          Caption = 'Ip Address :'
          Layout = tlCenter
          ExplicitHeight = 13
        end
        object editHostIp: TEdit
          AlignWithMargins = True
          Left = 75
          Top = 0
          Width = 95
          Height = 21
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alRight
          Alignment = taRightJustify
          TabOrder = 0
          Text = '127.0.0.1'
        end
      end
      object PanelHostPort: TPanel
        Left = 170
        Top = 0
        Width = 252
        Height = 21
        Margins.Left = 8
        Margins.Top = 6
        Margins.Right = 8
        Margins.Bottom = 6
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 1
        object LabelUDPPort: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 50
          Height = 15
          Align = alLeft
          Caption = 'UDP Port :'
          Layout = tlCenter
          ExplicitHeight = 13
        end
        object editHostPort: TEdit
          AlignWithMargins = True
          Left = 56
          Top = 0
          Width = 180
          Height = 21
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alClient
          Alignment = taRightJustify
          TabOrder = 0
          Text = '43500'
        end
      end
    end
    object GridPanel6: TGridPanel
      Left = 2
      Top = 42
      Width = 428
      Height = 106
      Align = alClient
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = Panel2
          Row = 0
        end
        item
          Column = 0
          Control = Panel3
          Row = 1
        end
        item
          Column = 0
          Control = Panel4
          Row = 2
        end>
      RowCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 31.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 31.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 31.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 428
        Height = 31
        Align = alClient
        Anchors = []
        BevelOuter = bvNone
        TabOrder = 0
        object Label8: TLabel
          Left = 0
          Top = 0
          Width = 73
          Height = 31
          Align = alLeft
          Caption = #20316#26989#12487#12451#12524#12463#12488#12522
          Layout = tlCenter
          ExplicitHeight = 13
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 31
        Width = 428
        Height = 31
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object ActionSelectMFJsonExportDir: TSpeedButton
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 23
          Height = 25
          Align = alLeft
          ImageIndex = 4
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FFFFF400FFFF
            F800FFFEFF00F9FCFF00FDFCFF00FFFCFF00FFFDFF00FFFBFC00FAFFFE00F1FD
            FD00F3FFFF00EFFEFF00EAFFFF00EDFFFF00F4FCFC00FDFFFE00FFFFF800FFFF
            FB00F8FBFF00F8FDFF00F6FCFF00F5F9FF00F9FBFF00F6FFFF00E4F8FD00BAD6
            DD008AA4B4005573840045667600425C6A00C9D2DB00FBFEFF00FFFFFB00FCFE
            FE00F2FBFF00ECFCFF00E9F9FF00C0D4E5008DA1B2005775860039667B003971
            8A004A85A50072ACCF006BA1C2007DA5C20069778E00F8F9FF00FFFEFF00F4FC
            FF00BCD4E0005A7B8E003C607800466E87005D859E007BAAC60078B6D40068AF
            D1006BB3DB0064ACD4004085AC0098CCF000465B7A00DBE4FF00F8FBFF00E2F4
            FF00376275007BB6D0007DBDDF007EBDE30081BCE3007DB8DF0074B8DD006FB5
            DA006EB3DA0071B8DE00317DA10094D4F300587D9900BCD3ED00F4FDFF00AFC9
            D7005590A4007FCEE90079CBEE007CCDF3007DC8EF0081C8EE0078C1E70079BE
            E50080C1E8005599BE004B9BBA0088CFEA006692A900A5C6DA00EEF9FF00809F
            AE0086CBDC0089E2F7007ED7F2007AD1F1007ED3F30077C8E9007DCBEF007CC3
            E80086C1E9004785A9006CBAD70086D1E7007CACBE008FB4C200CDE1F2006289
            98009FE7F90088E1F50084DEF60080DAF3007FD6F10081D5F1007BCCED0086CE
            F0008DC8EF00417EA00078C3DD008FD6EA0093C1D20084A7B40091B7C9008ABC
            CE00A1E9FB0090E3F80090E3F9008EE0F9008BDDF60086D8F10083D6F20083CF
            EC0063A2C40063A0C0008ED1EA009CD8EE00A4CCDE007A99A8005B839500B8E6
            F7009DD7EA007EC0D3003E8397003C8299004C92A9007AC5DB008CDBF00091DB
            F300498BA40099D7EF009CDBF000A3DBEE00B2DDEE006F91A100839DAB004964
            7200648091006D90A4006392A700A8DFF400A7DFF2006FAABD005398A700428A
            960079BDCA00ABECFA00AEE9F800B6EDFC00C4F7FF0052839300F1FEFF00F3FD
            FF00EFF7FF00EDFDFF0061839000CFF9FF00CAF6FF00C0F2FE00BCF7FF00BEFD
            FF00BBFAFF00C1FAFF00D3FFFF00C1EBF800AFDBE80046727F00F9FDFE00FDFF
            FF00FFFEFF00F2F7F80069818100DCFFFF00D9FEFF00D1FBFF00C6F7FF00CAFF
            FF006D9EA800678F9B006E8896007D909D008B9EAB00A8BCC700FFFFFF00FFFE
            FD00FFFFFE00FFFFFE006C807B00E6FFFF00E4FFFF00D9FCFF00C9EFF400ADD2
            DA0068879000E4F8FF00F1FBFF00F6FDFF00F8FEFF00F4FCFF00FFFDFF00FFFE
            FF00FBFAFC00FAFFFF00C7D6D9005E727700566C7200677E86006D848C008396
            9D00D8E3E700FAFFFF00FBFFFF00FAFFFE00FDFFFF00FBFFFF00FFFEFF00FFFE
            FF00FCFDFF00F8FEFF00F3FFFF00EEFEFF00EDFDFF00ECFDFF00EDFEFF00F3FF
            FF00FFFEFF00FEFAF900FFFFFE00FFFFFC00FFFFFC00FFFFFC00}
          OnClick = ActionSelectMFJsonExportDirClick
          ExplicitLeft = 11
          ExplicitTop = 9
          ExplicitHeight = 22
        end
        object EditSelectMENGEdir: TEdit
          AlignWithMargins = True
          Left = 32
          Top = 3
          Width = 393
          Height = 25
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 21
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 62
        Width = 428
        Height = 31
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object LabelMENGEProjectName: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 115
          Height = 25
          Align = alLeft
          Caption = 'MENGE'#12503#12525#12472#12455#12463#12488#21517#31216#65306
          Layout = tlCenter
          ExplicitHeight = 13
        end
        object EditMENGEProjectName: TEdit
          AlignWithMargins = True
          Left = 124
          Top = 3
          Width = 124
          Height = 25
          Align = alLeft
          TabOrder = 0
          ExplicitHeight = 21
        end
      end
    end
  end
  object PanelFutter: TPanel
    Left = 0
    Top = 162
    Width = 438
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BevelFutter: TBevel
      Left = 0
      Top = 0
      Width = 438
      Height = 3
      Align = alTop
      ExplicitLeft = 296
      ExplicitTop = -8
      ExplicitWidth = 50
    end
    object ButtonCancel: TButton
      AlignWithMargins = True
      Left = 360
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
      Left = 279
      Top = 6
      Width = 75
      Height = 27
      Align = alRight
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 1
    end
  end
end
