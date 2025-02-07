unit MFJsonLoaderSettingForm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
    Vcl.Buttons,
    PluginCore,
    MFJsonLoaderOptions,AgentSettings, F_MFJsonMultiLoader, System.ImageList,
    Vcl.ImgList;

type
    /// <summary>
    ///    MF-Jsonの読み込みおよびAgentのキャラクターモデル設定を行うフォームとその機能を定義するクラス
    /// </summary>
    TFormMFJsonLoaderSetting = class(TForm)
        PanelModels: TPanel;
        PanelBottom: TPanel;
        GroupBoxMale: TGroupBox;
        ComboBoxMaleUnder20: TComboBox;
        LabelMaleUnder20: TLabel;
        LabelMaleUnder60: TLabel;
        ComboBoxMaleUnder60: TComboBox;
        ComboBoxMaleOver60: TComboBox;
        LabelMaleOver60: TLabel;
        GroupBoxFemale: TGroupBox;
        LabelFemaleUnder20: TLabel;
        LabelFemaleUnder60: TLabel;
        LabelFemaleOver20: TLabel;
        ComboBoxFemaleUnder20: TComboBox;
        ComboBoxFemaleUnder60: TComboBox;
        ComboBoxFemaleOver60: TComboBox;
        buttonImportMFJson: TBitBtn;
        OpenDialog: TOpenDialog;
        ComboBoxMaleUnder20Rain: TComboBox;
        ComboBoxMaleUnder60Rain: TComboBox;
        ComboBoxMaleOver60Rain: TComboBox;
        ComboBoxFemaleunder20Rain: TComboBox;
        ComboBoxFemaleUnder60Rain: TComboBox;
        ComboBoxFemaleover60Rain: TComboBox;
        GridPanelMale: TGridPanel;
        Label3: TLabel;
        Label5: TLabel;
        GridPanelFemale: TGridPanel;
        Label9: TLabel;
        Label8: TLabel;
        Panel1: TPanel;
        Panel2: TPanel;
        SpeedButtonOpen: TSpeedButton;
        Panel3: TPanel;
        Bevel1: TBevel;
        Panel4: TPanel;
        ComboBoxUnknown: TComboBox;
        Panel5: TPanel;
        Label1: TLabel;
        ImageList1: TImageList;
        FrameMfMultiLoader1: TFrameMfMultiLoader;
        procedure buttonImportMFJsonClick(Sender: TObject);

        procedure ComboBoxChange(Sender: TObject);
        procedure ComboBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
        procedure FormShow(Sender: TObject);
        procedure SpeedButtonOpenClick(Sender: TObject);

        private
            p_project   : IF8ProjectForRoad;
            p_options   : MFJsonLoaderOptionsClass;
            p_models    : TModelSettingType;
            function    GetProject: IF8ProjectForRoad;
            procedure   SetProject(const Value: IF8ProjectForRoad);
            procedure   DrawBitmapInRect(thumbNail: TBitmap; const name: String; canvas: TCanvas;
                            rect: TRect; const useEllipsisIfTooLong: Boolean = false);

            procedure   InitialiseComboBox(const cb: TComboBox);

        public
            app   : IF8ApplicationServices;
            procedure   AfterConstruction; override;
            procedure   BeforeDestruction; override;
            property    project : IF8ProjectForRoad read GetProject write SetProject;
        end;

implementation

{$R *.dfm}

uses
    MFJsonLoader,
    MovingFeature;

procedure TFormMFJsonLoaderSetting.AfterConstruction;
    //==========================================================================
    function GetCharaFromGUID(id  :TGUID) : IF8QuakeIII;
        var
            i : Integer;
        begin
        if IsEqualGUID(id,TGUID.Empty) and (theApplicationServices.project.GetNumberOfCharacters>0)  then
            Result :=theApplicationServices.project.character[1];

        for I := 1 to theApplicationServices.project.GetNumberOfCharacters do
            begin
            if IsEqualGUID(id,theApplicationServices.project.character[i].GUID) then
                begin
                Result := theApplicationServices.project.character[i];
                Exit;
                end;
            end;
        end;
    //==========================================================================
    var
        ar : AgeRangeType;
    begin
    inherited;
    p_options := MFJsonLoaderOptionsClass.Create;
    p_models := LoadModelSettings;
    for ar := Low(AgeRangeType) to High(AgeRangeType) do
        begin
        p_options.unknown := GetCharaFromGUID(p_models.UnknownModel);
        p_options.male[ar] := GetCharaFromGUID(p_models.models[Male][integer(ar)][SUNNY]);
        p_options.female[ar] := GetCharaFromGUID(p_models.models[Female][integer(ar)][SUNNY]);
        p_options.maleinRain[ar] := GetCharaFromGUID(p_models.models[Male][integer(ar)][RAINNY]);
        p_options.femaleinRain[ar] := GetCharaFromGUID(p_models.models[FeMale][integer(ar)][RAINNY]);
        end;

    GroupBoxFemale.Visible := False;
    GroupBoxMale.Visible   := False;
    SpeedButtonOpen.ImageIndex := 1;
    PanelModels.Height :=120;
    self.Height := self.Height -167;
    end;

procedure TFormMFJsonLoaderSetting.BeforeDestruction;
    begin
    inherited;
    SaveModelSettings(p_models);
    FreeAndNil(p_options);
    end;

procedure TFormMFJsonLoaderSetting.buttonImportMFJsonClick(Sender: TObject);
    begin
    MovingFeatureListClass.ClearMovingFeatures;
    if FrameMfMultiLoader1.DoImportAllMFJson(p_options) then
        ModalResult := mrClose;
    end;

procedure TFormMFJsonLoaderSetting.ComboBoxChange(Sender: TObject);
    var
        cb  : TComboBox;
        ch  : IF8QuakeIII;
    begin
    cb := Sender as TComboBox;
    if Supports(cb.items.Objects[cb.ItemIndex], IF8QuakeIII, ch) then
        begin
        case cb.Tag of
            0:
                begin
                p_options.male[under20] := ch;
                p_models.Models[MALE][YOUNG][SUNNY] := ch.GUID;
                end;
            1:
                begin
                p_options.male[under60] := ch;
                p_models.Models[MALE][ADULT][SUNNY] := ch.GUID;
                end;
            2:
                begin
                p_options.male[over60]  := ch;
                p_models.Models[MALE][OLD][SUNNY] := ch.GUID;
                end;
            3:
                begin
                p_options.female[under20] := ch;
                p_models.Models[FEMALE][YOUNG][SUNNY] := ch.GUID;
                end;
            4:
                begin
                p_options.female[under60] := ch;
                p_models.Models[FEMALE][ADULT][SUNNY] := ch.GUID;
                end;
            5:
                begin
                p_options.female[over60]  := ch;
                p_models.Models[FEMALE][OLD][SUNNY] := ch.GUID;
                end;
            6:
                begin
                p_options.maleinRain[under20] := ch;
                p_models.Models[MALE][YOUNG][RAINNY] := ch.GUID;
                end;
            7:
                begin
                p_options.maleinRain[under60] := ch;
                p_models.Models[MALE][ADULT][RAINNY] := ch.GUID;
                end;

            8:
                begin
                p_options.maleinRain[over60]  := ch;
                p_models.Models[MALE][OLD][RAINNY] := ch.GUID;
                end;
            9:
                begin
                p_options.femaleinRain[under20] := ch;
                p_models.Models[FEMALE][YOUNG][RAINNY] := ch.GUID;
                end;
            10:
                begin
                p_options.femaleinRain[under60] := ch;
                p_models.Models[FEMALE][ADULT][RAINNY] := ch.GUID;
                end;
            11:
                begin
                p_options.femaleinRain[over60]  := ch;
                p_models.Models[FEMALE][OLD][RAINNY] := ch.GUID;
                end;
            12:
                begin
                p_options.Unknown  := ch;
                p_models.UnknownModel := ch.GUID;
                end;
            end;
        end;

    end;

procedure TFormMFJsonLoaderSetting.ComboBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    var
        cb  : TComboBox;
        r : TRect;
        bitmap : TBitmap;
        cc  : IF8QuakeIII;
    begin
    cb := Control as TComboBox;
    bitmap := TBitmap.Create;
    try
        bitmap.Width := 60;
        bitmap.Height := 60;
        r.Top := 1;
        r.Left := 1;
        r.Bottom := 58;
        r.Right := 58;
        if Supports(cb.Items.Objects[Index], IF8QuakeIII, cc) then
            bitmap.Canvas.StretchDraw(r, cc.GetThumbnail);
        DrawBitmapInRect(bitmap,
                         cb.Items.Strings[Index],
                         cb.Canvas,
                         Rect);
    finally
        bitmap.Free;
        end;
    end;

procedure TFormMFJsonLoaderSetting.DrawBitmapInRect(thumbNail: TBitmap; const name: String; canvas: TCanvas; rect: TRect; const useEllipsisIfTooLong: Boolean);
    const
        OFFSET = 2;
    var
        displayedName: String;
        lastNameCharacterIndex: Integer;
        maxTextWidth: Integer;
        textExtent: TSize;
        textWidth: Integer;
    begin
    canvas.FillRect(rect);

    if Assigned(thumbnail) then
        BitBlt(canvas.Handle, rect.Left + 1, rect.Top + 1, rect.Right - rect.Left - 1,
                            rect.Bottom - rect.Top - 1, thumbnail.Canvas.Handle, 1, 1, SRCCOPY);

    displayedName := name;
    textExtent := Canvas.TextExtent(displayedName);

    if useEllipsisIfTooLong then     // Equivalent to Windows' DT_END_ELLIPSIS
        begin
        maxTextWidth := rect.Right - (rect.Left + OFFSET) - 1;
        textWidth := textExtent.cx;
        if textWidth > maxTextWidth then
            begin
            lastNameCharacterIndex := Length(displayedName);
            displayedName := displayedName + '...';
            repeat
                Delete(displayedName, lastNameCharacterIndex, 1);
                Dec(lastNameCharacterIndex);
                textWidth := Canvas.TextWidth(displayedName);
            until (textWidth <= maxTextWidth) or (lastNameCharacterIndex = 1);
            end;
        end;
    Canvas.TextOut(rect.Left + OFFSET, rect.Bottom - textExtent.cy - 2, displayedName);
    end;

procedure TFormMFJsonLoaderSetting.FormShow(Sender: TObject);
    begin
    InitialiseComboBox(ComboBoxMaleUnder20);
    InitialiseComboBox(ComboBoxMaleUnder60);
    InitialiseComboBox(ComboBoxMaleOver60);
    InitialiseComboBox(ComboBoxFemaleUnder20);
    InitialiseComboBox(ComboBoxFemaleUnder60);
    InitialiseComboBox(ComboBoxFemaleOver60);
    InitialiseComboBox(ComboBoxMaleUnder20Rain);
    InitialiseComboBox(ComboBoxMaleUnder60Rain);
    InitialiseComboBox(ComboBoxMaleOver60Rain);
    InitialiseComboBox(ComboBoxFemaleUnder20Rain);
    InitialiseComboBox(ComboBoxFemaleUnder60Rain);
    InitialiseComboBox(ComboBoxFemaleOver60Rain);
    InitialiseComboBox(ComboBoxUnknown);
    end;

function TFormMFJsonLoaderSetting.GetProject: IF8ProjectForRoad;
    begin
    Result := p_project;
    end;

procedure TFormMFJsonLoaderSetting.InitialiseComboBox(const cb: TComboBox);
    var
        i   : Integer;
        ch  : IF8QuakeIII;
    begin
    case cb.tag of
        0:  ch :=p_options.male[under20];
        1:  ch :=p_options.male[under60];
        2:  ch :=p_options.male[over60];
        3:  ch :=p_options.female[under20];
        4:  ch :=p_options.female[under60];
        5:  ch :=p_options.female[over60];
        6:  ch :=p_options.maleinRain[under20];
        7:  ch :=p_options.maleinRain[under60];
        8:  ch :=p_options.maleinRain[over60];
        9:  ch :=p_options.femaleinRain[under20];
        10: ch :=p_options.femaleinRain[under60];
        11: ch :=p_options.femaleinRain[over60];
        12: ch :=p_options.unknown;
    end;

    cb.Clear;
    for i := 1 to p_project.numberOfCharacters do
        begin
        cb.AddItem(p_project.character[i].name, TObject(p_project.character[i]));
        if p_project.character[i] = ch then
            cb.ItemIndex := i - 1;
        end;
    end;

procedure TFormMFJsonLoaderSetting.SetProject(const Value: IF8ProjectForRoad);
    begin
    p_project := Value;
    p_options.Initilize(p_project);
    end;

procedure TFormMFJsonLoaderSetting.SpeedButtonOpenClick(Sender: TObject);
    begin
    GroupBoxFemale.Visible := not GroupBoxMale.Visible;
    GroupBoxMale.Visible   := not GroupBoxMale.Visible;

    if GroupBoxMale.Visible then
        begin
        SpeedButtonOpen.ImageIndex := 0;
        self.Height := self.Height +167;
        PanelModels.Height :=287;
        end
    else
        begin
        SpeedButtonOpen.ImageIndex := 1;
        PanelModels.Height :=120;
        self.Height := self.Height -167;
        end;
    end;

end.
