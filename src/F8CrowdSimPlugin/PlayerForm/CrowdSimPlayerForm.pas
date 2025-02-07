unit CrowdSimPlayerForm;

interface

uses
    Winapi.Windows, Winapi.Messages, Vcl.FileCtrl, System.SysUtils, System.Variants,System.Dateutils,
    System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons,
    Vcl.ToolWin, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, System.Math,
    PluginCore,
    F8Utils,
    F8GLUtils,
    F8CrowdSimController,
    System.Actions,
    Vcl.ActnList,
    VirtualTrees,
    VirtualButtonTree,
    VTEditors,
    F8OpenGL,
    F8VTEditors,
    F8RealSpinEdit,
    F8FloatSpinEdit,
    ComboDrawingHelper, Vcl.Menus, Vcl.Samples.Spin,
    MengeXML, System.ImageList, Vcl.ImgList,
    PedestrianMapList,
    PedestrianMapUser,
    PedestrianUtil,
    CrowdSimUDPClient,
    F_SimulationAreaSettings,
    F_CrossSectionLogSettings,
    CrowdSimLogSensorAreaRenderer,
    F_TimeController,
    MengeCommunicator,
    AgentSettings,
    CrowdSimExportsForm;

type
    BeforeExportXMLEvent = function: Boolean of Object;

    /// <summary>
    ///    人流シミュレーションシステムの各種操作を行うフォームとその機能を定義するクラス
    /// </summary>
    TFormCrowdSimPlayer = class(TForm)
        ToolBar3: TToolBar;
        ActionList: TActionList;
        ActionPlay: TAction;
        ActionPause: TAction;
        ActionStop: TAction;
        PanelGridOwner: TPanel;
        PopupMenu1: TPopupMenu;
        TabControlSettings: TTabControl;
        ActionSelectDir: TAction;
        PanelGridTopControl: TPanel;
        SpinEditPageNumber: TSpinEdit;
        LabelMaxPageCount: TLabel;
        SpeedButtonCheckEverything: TSpeedButton;
        SpeedButtonUncheckEverything: TSpeedButton;
        ActionCheckAll: TAction;
        ActionUncheckAll: TAction;
        ActionCheckEverything: TAction;
        ActionUncheckEverything: TAction;
        ImageList: TImageList;
        SpeedButtonCheckAll: TSpeedButton;
        SpeedButtonUncheckAll: TSpeedButton;
        ButtonCalcMENGE: TButton;
        PageControlClient: TPageControl;
        TabSheetArea: TTabSheet;
        TabSheetMENGE: TTabSheet;
        TabSheetAGENT: TTabSheet;
        TabSheetCSLog: TTabSheet;
        GroupBox1: TGroupBox;
        GridPanel2: TGridPanel;
        Label2: TLabel;
        DateTimePickerStart: TDateTimePicker;
        DateTimePickerEnd: TDateTimePicker;
        ButtonAgentDetailSettings: TButton;
        ActionFast: TAction;
        ButtonMFJsonExport : TButton;
        PanelExportMFJson: TPanel;
        GroupBoxExportMFJson: TGroupBox;
        SEMFJsonInterval: TF8RealSpinEdit;
    	LabelMFJsonInterval: TLabel;
        ButtonSelectExportMFJsonDirectory: TSpeedButton;
    	LabelMFJsonDirectory: TLabel;
        EditExportMFJsonDirectory: TEdit;
    	LabelMFJsonName: TLabel;
    	EditMFJsonName: TEdit;
        FrameControlTime1: TFrameControlTime;
        Label1: TLabel;
        Label3: TLabel;
        GridPanel1: TGridPanel;
        Label4: TLabel;
        RadioButtonSunny: TRadioButton;
        RadioButtonRain: TRadioButton;
        PanelCalcSim: TPanel;
        ButtonMengeSetting: TButton;
       
        procedure FormShow(Sender: TObject);
        procedure TabControlSettingsChange(Sender: TObject);
        procedure SpinEditPageNumberChange(Sender: TObject);

        procedure ActionCheckAllExecute(Sender: TObject);
        procedure ActionCheckAllUpdate(Sender: TObject);
        procedure ActionUncheckAllExecute(Sender: TObject);
        procedure ActionUncheckAllUpdate(Sender: TObject);
        procedure ActionCheckEverythingExecute(Sender: TObject);
        procedure ActionUncheckEverythingExecute(Sender: TObject);
        procedure ActionCheckEverythingUpdate(Sender: TObject);
        procedure ActionUncheckEverythingUpdate(Sender: TObject);
        procedure ButtonCalcMENGEClick(Sender: TObject);
        procedure ButtonAgentDetailSettingsClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure ActionSelectMFJsonExportDir(Sender: TObject);
        procedure ButtonMFJsonExportClick(Sender: TObject);
        procedure ButtonMengeSettingClick(Sender: TObject);

        private
            var
                p_comboDrawingHelperIntf: IComboCustomDraw;
                p_grid          : TVirtualButtonTree;
                p_characterList : TStringList;

                p_currentEditLink: TCustomEditLink;
                p_currentEditLinkIntf : IVTEditLink;

                p_pageIndex : Integer;
                p_shiftNumber   : Cardinal;
                p_initializing_grid : Boolean;
                p_MengeXML : MengeXmlClass;

                p_CalcSimTime  : TDateTime;

                SimulationAreaFrame: TFrameSimulationAreaSettings;
                CSLogFrame         : TFrameCrossSectionLogSettings;

                p_OnBeforeExportXMLEvent: BeforeExportXMLEvent;

                p_ExportsForm : TFormCrowdSimExports;
                p_Communicator : TMengeCommunicationManagerClass;

                p_PedestrianMapList: TPedestrianMapList;
                p_PedestrianMapUser: TPedestrianMapUser;

			function    GetController: F8CrowdSimControllerClass;
            function    GetApplication: IF8ApplicationServices;
            procedure   SetApplication(const Value: IF8ApplicationServices);
            function    GetPedestrianMapList: TPedestrianMapList;
            procedure   SetPedestrianMapList(const aValue: TPedestrianMapList);
            function    GetPedestrianMapUser: TPedestrianMapUser;
            procedure   SetPedestrianMapUser(const aValue: TPedestrianMapUser);

            procedure   Initialize;
            procedure   MakeGridChilds;

            procedure   GridBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
            procedure   GridAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
            procedure   GridCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
            procedure   GridEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
            procedure   GridFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
            procedure   GridExit(Sender: TObject);
            procedure   GridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
            procedure   GridInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
            procedure   GridNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: String);
            procedure   GridDblClick(Sender: TObject);
            procedure   GridChecked(Sender : TBaseVirtualTree; Node : PVirtualNode);

            function    IsCellEditable( Node: PVirtualNode; Column: TColumnIndex ) : Boolean;
            function    IndexOfCharacter(const ch: IF8QuakeIII): Integer;
            procedure   DrawBitmapInRect(thumbNail: TBitmap; const name: String; canvas: TCanvas; rect: TRect; const useEllipsisIfTooLong: Boolean = false);

            function    GetMengeXml: MengeXmlClass;
            procedure   SetMengeXml(const Value: MengeXmlClass);

            procedure   CheckEveryGrid(const check: Boolean);

            procedure   SetSimBeginningTime(sender : Tobject);
            procedure   SetUpDiagramsData;
            procedure   SetController(const Value: F8CrowdSimControllerClass);
            procedure   ResetDiagramsData(sender :Tobject);
            function    AssignedLogSettingsAndMap: boolean;
            procedure   OnStop(sender :Tobject);
        public
            procedure   AfterConstruction; override;
            procedure   BeforeDestruction; override;

            procedure   Collect;
            procedure   UpdateSensorAreaRendererData(const aRenderer: TCrowdSimLogSensorAreaRenderer);
            procedure   OnChangedUserMap;

            property    application : IF8ApplicationServices read GetApplication write SetApplication;
            property    mengeXML : MengeXmlClass read GetMengeXml write SetMengeXml;
            property    PedestrianMapList: TPedestrianMapList read GetPedestrianMapList write SetPedestrianMapList;
            property    PedestrianMapUser: TPedestrianMapUser read GetPedestrianMapUser write SetPedestrianMapUser;
            property    OnBeforeExportXMLEvent: BeforeExportXMLEvent read p_OnBeforeExportXMLEvent write p_OnBeforeExportXMLEvent;
            property    Controller : F8CrowdSimControllerClass read GetController write SetController;
        end;

implementation

uses
    System.IOUtils,System.Types,
    Winapi.ShellAPI,
    MovingFeature,
    AgentPacketData,
    LatLonHelper,
    AgentDetailSettingForm,
    mengeSimSetting,
    MenngeCommunicateSettingForm;
{$R *.dfm}

type
    CrowdSimDataType = record
        walker: MovingFeatureClass;
        spLat : Double;
        spLong: Double;
        epLat : Double;
        epLong: Double;
        maxSpeed: Double;
        radius: Double;
        neighbor_dis: Double;
        end;
    PCrowdSimDataType = ^CrowdSimDataType;

    TCrowdSimGraphicComboEditLink = class(TF8GraphicComboEditLink)
        private
            p_selectedObject : TObject;
        protected
            procedure ComboChange(Sender: TObject);
            procedure PrepareEditControl; override;
        public
            property selectedObject : TObject read p_selectedObject write p_selectedObject;
        end;

const
    MODEL_COL = 0;
    CONNECT_COL = 1;
    SP_LAT_COL = 2;
    SP_LONG_COL = 3;
    EP_LAT_COL = 4;
    EP_LONG_COL = 5;
    TAB_INDEX_AREA    = 0;
    TAB_INDEX_AGENT   = 1;
    TAB_INDEX_CSLOG   = 2;
    TAB_INDEX_MENGE   = 3;

const
    READONLY_COLOUR = clMoneyGreen;

{ TFormCrowdSimPlayer }


procedure TFormCrowdSimPlayer.ActionCheckAllExecute(Sender: TObject);
    var
        i   : Integer;
    begin
    for i := 0 to MovingFeatureListClass.numberOfMovingFeatures - 1 do
        MovingFeatureListClass.movingFeature[i].communicateMenge := True;
    CheckEveryGrid(True);
    end;

procedure TFormCrowdSimPlayer.ActionCheckAllUpdate(Sender: TObject);
    begin
    ActionCheckAll.Enabled := True;
    end;

procedure TFormCrowdSimPlayer.ActionCheckEverythingExecute(Sender: TObject);
    var
        Node: PVirtualNode;
        pData       : PCrowdSimDataType;
        checkState  : TCheckState;
    begin
    p_grid.OnChecked := nil;
    try
        Node := p_grid.GetFirstChild(nil);
        checkState := csCheckedNormal;

        while Assigned(Node) do
            begin
            p_grid.CheckState[Node] := checkState;
            pData := p_grid.GetNodeData(Node);
            if Assigned(pData) then
                pData^.walker.communicateMenge := True;

            Node := Node.NextSibling;
            end;

    finally
        p_grid.OnChecked := GridChecked;
        end;
    end;

procedure TFormCrowdSimPlayer.ActionCheckEverythingUpdate(Sender: TObject);
    begin
    ActionUncheckEverything.Enabled := True;
    end;

procedure TFormCrowdSimPlayer.ActionSelectMFJsonExportDir(Sender: TObject);
    var
        SelectFolder: TArray<String>;
    begin
    if SelectDirectory(application.UserDirectory,
                       SelectFolder,
                       [],
                       '出力先の指定',
                       '出力先フォルダ',
                       '確定') then
        begin
        EditExportMFJsonDirectory.Text := SelectFolder[0];
        end;
    end;

procedure TFormCrowdSimPlayer.ActionUncheckAllExecute(Sender: TObject);
    var
        i   : Integer;
    begin
    for i := 0 to MovingFeatureListClass.numberOfMovingFeatures - 1 do
        MovingFeatureListClass.movingFeature[i].communicateMenge := False;
    CheckEveryGrid(False);
    end;

procedure TFormCrowdSimPlayer.ActionUncheckAllUpdate(Sender: TObject);
    begin
    ActionUncheckAll.Enabled := True;
    end;

procedure TFormCrowdSimPlayer.ActionUncheckEverythingExecute(Sender: TObject);
    var
        Node: PVirtualNode;
        pData       : PCrowdSimDataType;
        checkState  : TCheckState;
    begin
    p_grid.OnChecked := nil;
    try
        Node := p_grid.GetFirstChild(nil);
        checkState := csUncheckedNormal;

        while Assigned(Node) do
            begin
            p_grid.CheckState[Node] := checkState;
            pData := p_grid.GetNodeData(Node);
            if Assigned(pData) then
                pData^.walker.communicateMenge := False;

            Node := Node.NextSibling;
            end;

    finally
        p_grid.OnChecked := GridChecked;
        end;
    end;

procedure TFormCrowdSimPlayer.ActionUncheckEverythingUpdate(Sender: TObject);
    begin
    ActionUncheckEverything.Enabled := True;
    end;

function TFormCrowdSimPlayer.AssignedLogSettingsAndMap: boolean;
    begin
    Result := Assigned(PedestrianMapUser) and PedestrianMapUser.Enabled;
    end;

procedure TFormCrowdSimPlayer.AfterConstruction;
    const
        GRID_COLUMN_WIDTH = 80;

    procedure SetUpGrid;
        var
            gridColumn: TVirtualTreeColumn;
            i: Integer;
        begin
        p_grid := TVirtualButtonTree.Create(PanelGridOwner);
        p_grid.Parent := PanelGridOwner;


        // Set up the 'design-time' properties
        with p_grid do
            begin
            Align := alClient;
            DefaultNodeHeight := 74;
            EditDelay := 0;
            Header.Height := 20;
            Margin := 0;
            ScrollBarOptions.ScrollBars := ssVertical;
            TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toCheckSupport, toGridExtensions];
            TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowHorzGridLines, toShowVertGridLines];
            TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowRoot, toShowTreeLines];
            TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toExtendedFocus];
            WantTabs := true;
            HintMode := hmTooltip;
            ShowHint := true;
            end;

        p_grid.NodeDataSize := SizeOf(CrowdSimDataType);

        p_grid.Header.Options := p_grid.Header.Options + [hoColumnResize];
        gridColumn := p_grid.Header.Columns.Add();
        gridColumn.Position := MODEL_COL;
        gridColumn.Text := 'モデル';
        gridColumn.Width  := 80;

        gridColumn := p_grid.Header.Columns.Add();
        gridColumn.Position := CONNECT_COL;
        gridColumn.Text := '接続';
        gridColumn.Width  := 60;

        gridColumn := p_grid.Header.Columns.Add();
        gridColumn.Position := SP_LAT_COL;
        gridColumn.Text := '始点　緯度';
        gridColumn.Width := GRID_COLUMN_WIDTH;

        gridColumn := p_grid.Header.Columns.Add();
        gridColumn.Position := SP_LONG_COL;
        gridColumn.Text := '始点　経度';
        gridColumn.Width := GRID_COLUMN_WIDTH;

        gridColumn := p_grid.Header.Columns.Add();
        gridColumn.Position := EP_LAT_COL;
        gridColumn.Text := '終点　緯度';
        gridColumn.Width := GRID_COLUMN_WIDTH;

        gridColumn := p_grid.Header.Columns.Add();
        gridColumn.Position := EP_LONG_COL;
        gridColumn.Text := '終点　経度';
        gridColumn.Width := GRID_COLUMN_WIDTH;

        for i := 0 to (p_grid.Header.Columns.Count - 1) do
            begin
            gridColumn := p_grid.Header.Columns[i];
            gridColumn.Options := gridColumn.Options + [coFixed];
            gridColumn.Margin := 0;
            end;

        p_grid.Margin := 0;
        p_grid.TreeOptions.PaintOptions := p_grid.TreeOptions.PaintOptions - [toFullVertGridLines];
        p_grid.Header.Options := p_grid.Header.Options + [hoVisible];
        p_grid.Header.Options := p_grid.Header.Options - [hoDrag];
        p_grid.Header.MainColumn := CONNECT_COL;
        p_grid.OnBeforeCellPaint := GridBeforeCellPaint;
        p_grid.OnAfterCellPaint := GridAfterCellPaint;
        p_grid.OnCreateEditor := GridCreateEditor;
        p_grid.OnEditing := GridEditing;
        p_grid.OnFocusChanged := GridFocusChanged;
        p_grid.OnExit := GridExit;
        p_grid.OnGetText := GridGetText;
        p_grid.OnInitNode := GridInitNode;
        p_grid.OnNewText := GridNewText;
        p_grid.OnDblClick := GridDblClick;
        p_grid.OnChecked := GridChecked;
        end;
    var
        time : TDateTime;
    begin
    inherited;
    p_OnBeforeExportXMLEvent := nil;
    p_initializing_grid := true;

    SetUpGrid;

    p_comboDrawingHelperIntf := TComboDrawingHelperClass.Create() as IComboCustomDraw;

    SimulationAreaFrame := TFrameSimulationAreaSettings.Create(Self);
    SimulationAreaFrame.Parent := TabSheetArea;
    SimulationAreaFrame.Name := 'FrameSimulationArea';
    SimulationAreaFrame.Align := alClient;

    CSLogFrame        := TFrameCrossSectionLogSettings.Create(Self);
    CSLogFrame.Parent := TabSheetCSLog;
    CSLogFrame.Name   := 'FrameCSLog';
    CSLogFrame.Align  := alClient;

    time := RecodeSecond(MovingFeatureListClass.beginningTime,0);
    DateTimePickerStart.DateTime := time;
    DateTimePickerEnd.DateTime :=  RecodeHour(time,hourOf(time)+1);

    FrameControlTime1.OnNotifyReset := ResetDiagramsData;
    FrameControlTime1.OnNotifyPlay  := SetSimBeginningTime;
    FrameControlTime1.OnNotifyStop  := OnStop;

    p_ExportsForm := TFormCrowdSimExports.Create(nil);
    p_Communicator := TMengeCommunicationManagerClass.Create;
    p_PedestrianMapList := nil;
    p_PedestrianMapUser := nil;
    end;

procedure TFormCrowdSimPlayer.BeforeDestruction;
    begin
    inherited;
    p_currentEditLink := nil;
    p_currentEditLinkIntf := nil;
    p_PedestrianMapList := nil;
    p_PedestrianMapUser := nil;


    p_comboDrawingHelperIntf := nil;  // Reduce the reference count to free the object
    if Assigned(p_characterList) then
        FreeAndNil(p_characterList);

    FreeAndNil(p_Communicator);
    FreeAndNil(p_ExportsForm);
    FreeAndNil(CSLogFrame);
    FreeAndNil(SimulationAreaFrame);
    p_MengeXML.ResetParams;
    p_grid := nil;
    end;

procedure TFormCrowdSimPlayer.ButtonAgentDetailSettingsClick(Sender: TObject);
    var
        Form : TFormAgentDetailSetting;
    begin
    Form := TFormAgentDetailSetting.Create(self);
    try
        if (Form.ShowModal = mrOK) then
            begin
            Form.SaveData;
            end;
    finally
        FreeAndNil(Form);
        end;
    end;

procedure TFormCrowdSimPlayer.ButtonCalcMENGEClick(Sender: TObject);
    var
        agentSetting : TAgentSettingsType;
        ltTmp, rbTmp: TPoint3D;
        lt, rb: GLPointType;
        signalInterval : integer;
    begin
    if DateTimePickerStart.DateTime >= DateTimePickerEnd.DateTime then
        begin
        Showmessage('シミュレーション時刻を修正してください。');
        Exit;
        end;

    p_CalcSimTime  := DateTimePickerStart.DateTime;

    agentSetting := LoadAgentSettings;
    agentSetting.IsRain := RadioButtonRain.Checked;
    SaveAgentSettings(agentSetting);

    PedestrianMapUser.Map.ExportMapAreaWithMargin(ltTmp, rbTmp);
    lt := asGLPointType(ltTmp.X, ltTmp.Y, ltTmp.Z);
    rb := asGLPointType(rbTmp.X, rbTmp.Y, rbTmp.Z);
    FrameControlTime1.Controller.IsRain := RadioButtonRain.Checked;
    FrameControlTime1.Controller.AssignSimlationTime(DateTimePickerStart.DateTime,DateTimePickerEnd.DateTime);

    if Assigned(p_OnBeforeExportXMLEvent) then
        begin
        if not p_OnBeforeExportXMLEvent then
            begin
            Showmessage('連携用データ作成で問題が発生しましたので計算を中断します');
            Exit;
            end;
        end;

    signalInterval := p_PedestrianMapUser.Map.CrossWalkIntervalSecond;
    if p_Communicator.ExportMENGEFiles(lt,rb,signalInterval) then
        begin
        p_Communicator.StartCalcMENGE(DateTimePickerStart.DateTime);
        ButtonMFJsonExport.Enabled := true;
        end;
    end;

procedure TFormCrowdSimPlayer.ButtonMengeSettingClick(Sender: TObject);
    var
        form : TFormMengeCommunicateSetting;
    begin
    form := TFormMengeCommunicateSetting.Create(Self);
    try
        if form.ShowModal = mrOk then
            begin
            form.SaveData;
            end;
    finally
        FreeAndNil(form);
        end;
    end;

procedure TFormCrowdSimPlayer.ButtonMFJsonExportClick(Sender: TObject);
    var
        filename : string;
    begin
    if DirectoryExists(EditExportMFJsonDirectory.Text) then
        begin
        if EditMFJsonName.Text <> '' then
            begin
            ButtonMFJsonExport.Enabled := false;
            filename := TPath.Combine(EditExportMFJsonDirectory.Text, EditMFJsonName.Text+'.json');
            MovingFeatureListClass.ExportMFJson(filename, SEMFJsonInterval.Value, application);
            showmessage('MF-Jsonを出力しました');
            ButtonMFJsonExport.Enabled := true;
            end
        else
            showmessage('ファイル名を指定してください');
        end
    else
        showmessage('指定されているディレクトリが見つかりません');
    end;

procedure TFormCrowdSimPlayer.CheckEveryGrid(const check: Boolean);
    var
        Node: PVirtualNode;
        checkState  : TCheckState;
    begin
    p_grid.OnChecked := nil;
    try
        Node := p_grid.GetFirstChild(nil);
        if check then
            checkState := csCheckedNormal
        else
            checkState := csUncheckedNormal;

        while Assigned(Node) do
            begin
            p_grid.CheckState[Node] := checkState;

            Node := Node.NextSibling;
            end;

    finally
        p_grid.OnChecked := GridChecked;
        end;
    end;

procedure TFormCrowdSimPlayer.Collect;
    begin
    if Assigned(controller) and (controller.currentState in [csPlay,csFast,csBack]) then
        FrameControlTime1.UpdateTime;
    PedestrianMapUser.CollectCSFlowData;
    end;

procedure TFormCrowdSimPlayer.UpdateSensorAreaRendererData(const aRenderer: TCrowdSimLogSensorAreaRenderer);
    begin
    CSLogFrame.UpdateSensorAreaRendererData(aRenderer);
    end;

procedure TFormCrowdSimPlayer.OnChangedUserMap;
    begin
    if Assigned(CSLogFrame) then
        CSLogFrame.OnChangeUserMap;
    end;

procedure TFormCrowdSimPlayer.DrawBitmapInRect(thumbNail: TBitmap; const name: String; canvas: TCanvas; rect: TRect; const useEllipsisIfTooLong: Boolean);
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


procedure TFormCrowdSimPlayer.FormShow(Sender: TObject);
    begin
    if MovingFeatureListClass.numberOfMovingFeatures > 0 then
        Initialize;

    TabControlSettings.TabIndex  := TAB_INDEX_AREA;
    PageControlClient.ActivePage := TabSheetArea;
    SimulationAreaFrame.OnFormShow;
    CSLogFrame.OnChangeActivate(False);
    end;

procedure TFormCrowdSimPlayer.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
    SimulationAreaFrame.OnFormClose;
    CSLogFrame.OnChangeActivate(False);
    if p_ExportsForm.Showing then
        p_ExportsForm.Close;
    end;

function TFormCrowdSimPlayer.GetApplication: IF8ApplicationServices;
    begin
    if Assigned(FrameControlTime1.Controller) then
        Result := FrameControlTime1.Controller.application
    else
        Result := nil;
    end;

function TFormCrowdSimPlayer.GetController: F8CrowdSimControllerClass;
    begin
    Result := FrameControlTime1.Controller;
    end;

procedure TFormCrowdSimPlayer.GridBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    begin
    case Column of
        SP_LAT_COL,
        SP_LONG_COL,
        EP_LAT_COL,
        EP_LONG_COL:
            begin
            if not IsCellEditable( Node, Column ) then
                begin
                TargetCanvas.Brush.Color := READONLY_COLOUR;
                TargetCanvas.FillRect( CellRect );
                end;
            end;
        end;
    end;

procedure TFormCrowdSimPlayer.GridAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    var
        itemIndex   : Integer;
        pData       : PCrowdSimDataType;
        saveColour  : TColor;
        text        : WideString;
        thumbNail   : TBitmap;
        ch          : IF8QuakeIII;
    begin
    pData := Sender.GetNodeData(Node);
    case Column of
        MODEL_COL:
            if pData^.walker <> nil then
                begin
                saveColour := TargetCanvas.Font.Color;
                TargetCanvas.Font.Color := clWindowText;
                try
                    itemIndex := IndexOfCharacter(pData^.walker.myCharacter);
                    if itemIndex <= 0 then
                        begin
                        text := '';
                        thumbnail := nil;
                        end
                    else
                        begin
                        ch := application.project.character[itemIndex];
                        text := ch.name;
                        thumbnail := ch.thumbnail;
                        end;

                    if Assigned(thumbnail) then
                        DrawBitmapInRect(thumbnail, text, TargetCanvas, CellRect);
                finally
                    TargetCanvas.Font.Color := saveColour;
                    end;
                end;
        end;
    end;

procedure TFormCrowdSimPlayer.GridChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    var
        pData       : PCrowdSimDataType;
    begin
    if Sender = p_grid then
        begin
        if p_initializing_grid then
            exit;
        pData := Sender.GetNodeData(Node);
        pData^.walker.communicateMenge := (Sender.CheckState[Node] = csCheckedNormal);
        end;
    end;

procedure TFormCrowdSimPlayer.GridCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    var
        comboEditLink : TCrowdSimGraphicComboEditLink;
    begin
    case Column of
        MODEL_COL:
            begin
            comboEditLink := TCrowdSimGraphicComboEditLink.Create(p_comboDrawingHelperIntf);
            comboEditLink.PickList := p_characterList;
            EditLink := comboEditLink;
            p_currentEditLink := comboEditLink;
            p_currentEditLinkIntf := comboEditLink;
            end;
        end;
    end;

procedure TFormCrowdSimPlayer.GridEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    begin
    if (Column = CONNECT_COL)then
        Allowed := False
    else
        Allowed := True;
    end;

procedure TFormCrowdSimPlayer.GridFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    begin
    if Assigned(Node) then
        begin
        if IsCellEditable( Node, Column ) then
            Sender.EditNode( Node, Column );
        end;
    end;

procedure TFormCrowdSimPlayer.GridExit(Sender: TObject);
    begin
    p_grid.Invalidate;
    end;

procedure TFormCrowdSimPlayer.GridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    var
        itemIndex: Integer;
        pData: PCrowdSimDataType;
    begin
    pData := p_grid.GetNodeData(Node);
    case Column of
        MODEL_COL:
            if pData^.walker = nil then
                CellText := ''
            else
                begin
                itemIndex := p_characterList.IndexOfObject(TObject(pData^.walker.myCharacter));
                if itemIndex = -1 then
                    CellText := ''
                else
                    begin
                    CellText := p_characterList[itemIndex];
                    if CellText = '' then
                        begin
                        if Assigned(pData^.walker) then
                            CellText := pData^.walker.myCharacter.name
                        else
                            CellText := '';
                        end;
                    end;
                end;
        SP_LAT_COL:
            CellText := Format('%.1f', [pData^.walker.startPoint[_x]]);
        SP_LONG_COL:
            CellText := Format('%.1f', [pData^.walker.startPoint[_z]]);
        EP_LAT_COL:
            CellText := Format('%.1f', [pData^.walker.goalPoint[_x]]);
        EP_LONG_COL:
            CellText := Format('%.1f', [pData^.walker.goalPoint[_z]]);
    else
        CellText := '';
        end;
    end;

procedure TFormCrowdSimPlayer.GridInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    var
        pIndex  : Integer;
        pData   : PCrowdSimDataType;
    begin
    pData := Sender.GetNodeData(Node);

    pIndex := Node^.Index + p_shiftNumber;     // Node^.Index is 0-based

    if (pIndex < MovingFeatureListClass.numberOfMovingFeatures) then
        begin
        pData^.walker := MovingFeatureListClass.movingFeature[pIndex];
        pData^.spLat  := pData^.walker.startPoint[_z];
        pData^.spLong := pData^.walker.startPoint[_x];
        pData^.epLat  := pData^.walker.goalPoint[_z];
        pData^.epLong := pData^.walker.goalPoint[_x];
        pData^.maxSpeed:= 0.0;
        pData^.radius:= 1.0;
        pData^.neighbor_dis:= 1.0;
        end
    else
        begin
        pData^.walker := nil;
        pData^.spLat  := 0.0;
        pData^.spLong := 0.0;
        pData^.epLat  := 0.0;
        pData^.epLong := 0.0;
        pData^.maxSpeed:= 0.0;
        pData^.radius:= 0.0;
        pData^.neighbor_dis:= 0.0;
        end;
    end;

procedure TFormCrowdSimPlayer.GridNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: String);
    var
        pData   : PCrowdSimDataType;
        newModel: IF8QuakeIII;
    begin
    pData := p_grid.GetNodeData(Node);
    case Column of
        MODEL_COL:
            begin
            if Supports(TCrowdSimGraphicComboEditLink(p_currentEditLink).selectedObject, IF8QuakeIII, newModel) then
                begin
                if (not Assigned(pData^.walker.myCharacter)) or (pData^.walker.myCharacter <> newModel) then
                    begin
                    pData^.walker.myCharacter := newModel;
                    end;
                end;
            end;
        end;
    end;

procedure TFormCrowdSimPlayer.GridDblClick(Sender: TObject);
    var
        pData: PCrowdSimDataType;
        pNode: PVirtualNode;
        vp  : GLPointType;
        yaw : Single;
    begin
    if (Sender = p_grid) and Assigned(p_grid.FocusedNode) then
        begin
        pNode := p_grid.FocusedNode;
        pData := p_grid.GetNodeData(pNode);

        if Assigned(pData.walker) and Assigned(pData.walker.myInstance) then
            begin
            pData.walker.myInstance.SetVisible(True);
            vp := pData.walker.myInstance.instancePosition;// savedPosition;
            yaw := pData.walker.myInstance.savedYawAngle;
            application.mainForm.openGL.Camera.Eye.CopyFrom(F8Add(vp, AsGLPointType(-3*Sin(yaw + Pi / 2), 1.0, -3*Cos(yaw + Pi / 2))));
            application.mainForm.openGL.Camera.ViewPoint.CopyFrom(vp);
            end;
        end;
    end;

function TFormCrowdSimPlayer.IndexOfCharacter(const ch: IF8QuakeIII): Integer;
    var
        i   : Integer;
    begin
    if not Assigned(ch) then
        Result := -1
    else
        begin
        Result := -1;
        for i := 0 to application.project.numberOfCharacters-1 do
            if application.project.character[i+1] = ch then
                begin
                Result := i+1;
                break;
                end;
        end;
    end;

procedure TFormCrowdSimPlayer.Initialize;
    var
        i   : Integer;
        ch  : IF8QuakeIII;
    begin

    if (application <> nil) then
        begin
        p_characterList := TStringList.Create();
        for i := 1 to application.project.numberOfCharacters do
            begin
            ch := application.project.character[i];
            p_characterList.AddObject(ch.name, TObject(ch));
            end;
        end;

    p_pageIndex := 1;
    p_shiftNumber := 0;
    SpinEditPageNumber.MinValue := p_pageIndex;
    SpinEditPageNumber.MaxValue := MovingFeatureListClass.numberOfMovingFeatures div 100 + 1;
    LabelMaxPageCount.Caption := ' / ' + IntToStr(SpinEditPageNumber.MaxValue);

    SpinEditPageNumber.Increment := 1;
    SpinEditPageNumber.Value := p_pageIndex;
    end;

function TFormCrowdSimPlayer.IsCellEditable(Node: PVirtualNode; Column: TColumnIndex): Boolean;
    var
        pData: PCrowdSimDataType;
    begin
    result := True;
    pData := p_grid.GetNodeData(Node);
    case Column of
        SP_LAT_COL,
        SP_LONG_COL,
        EP_LAT_COL,
        EP_LONG_COL:
            begin
            if not Assigned( pData ) or not Assigned( pData^.walker ) then
                Result := False;
            end;
        end;
    end;

procedure TFormCrowdSimPlayer.MakeGridChilds;
    var
        i: Integer;
        pNode: PVirtualNode;
        first,
        last    : Integer;
    begin
    p_grid.Clear;
    p_initializing_grid := true;
    try
        first := (p_pageIndex - 1) * 100;
        last  := Min(p_pageIndex * 100, MovingFeatureListClass.numberOfMovingFeatures) - 1;
        for i := first to last do
            begin
            pNode := p_grid.AddChild(nil);
            p_grid.CheckType[pNode] := ctCheckBox;
            if MovingFeatureListClass.movingFeature[i].communicateMenge then
                p_grid.CheckState[pNode] := csCheckedNormal
            else
                p_grid.CheckState[pNode] := csUncheckedNormal;
            end;
    finally
        p_initializing_grid := false;

        p_grid.InvalidateChildren(nil, false);  // Force the nodes to get initialised NOW!
        pNode := p_grid.GetFirst();
        p_grid.FocusedNode := pNode;
        end;
    end;

procedure TFormCrowdSimPlayer.OnStop(sender :Tobject);
    begin
    if controller.currentState in [csPlay, csFast, csBack] then
        begin
        PedestrianMapUser.ExportCrossSectionLog;
        SetUpDiagramsData;
        p_ExportsForm.Show;
        end;

    if Assigned(CSLogFrame) then
        CSLogFrame.ChangeEnabledIntervals(true);
    end;


procedure TFormCrowdSimPlayer.ResetDiagramsData(sender :Tobject);
    begin
    PedestrianMapUser.ResetCrossSectionFlowLog;
    if p_ExportsForm.Showing then
        p_ExportsForm.Close;

    if Assigned(CSLogFrame) then
        CSLogFrame.ChangeEnabledIntervals(false);
    end;

procedure TFormCrowdSimPlayer.SetApplication(const Value: IF8ApplicationServices);
    begin
    SimulationAreaFrame.Application := Value;
    end;

procedure TFormCrowdSimPlayer.SetController(const Value: F8CrowdSimControllerClass);
    begin
    p_Communicator.Controller := Value;
    FrameControlTime1.Controller := Value;
    end;

function TFormCrowdSimPlayer.GetPedestrianMapList: TPedestrianMapList;
    begin
    Result := p_PedestrianMapList;
    end;

procedure TFormCrowdSimPlayer.SetPedestrianMapList(const aValue: TPedestrianMapList);
    begin
    p_PedestrianMapList := aValue;
    SimulationAreaFrame.PedestrianMapList := p_PedestrianMapList;
    end;

function TFormCrowdSimPlayer.GetPedestrianMapUser: TPedestrianMapUser;
    begin
    Result := p_PedestrianMapUser;
    end;

procedure TFormCrowdSimPlayer.SetPedestrianMapUser(const aValue: TPedestrianMapUser);
    begin
    p_PedestrianMapUser := aValue;
    SimulationAreaFrame.PedestrianMapUser := p_PedestrianMapUser;
    CSLogFrame.PedestrianMapUser := p_PedestrianMapUser;
    end;

procedure TFormCrowdSimPlayer.SetMengeXml(const Value: MengeXmlClass);
    begin
    p_MengeXml := value;
    p_Communicator.XML := p_MengeXml;
    end;

function TFormCrowdSimPlayer.GetMengeXml: MengeXmlClass;
    begin
    Result := p_MengeXml;
    end;

procedure TFormCrowdSimPlayer.SpinEditPageNumberChange(Sender: TObject);
    begin
    p_pageIndex := SpinEditPageNumber.Value;
    p_shiftNumber := (p_pageIndex - 1) * 100;
    MakeGridChilds;
    end;

procedure TFormCrowdSimPlayer.SetSimBeginningTime(sender : Tobject);
    begin
    PedestrianMapUser.PlayStart;
    end;

procedure TFormCrowdSimPlayer.SetUpDiagramsData;
    begin
    if AssignedLogSettingsAndMap then
        p_ExportsForm.SetUpExportsData(PedestrianMapUser);
    end;

procedure TFormCrowdSimPlayer.TabControlSettingsChange(Sender: TObject);
    procedure WarnAndSetTabSheetArea;
        begin
        TabControlSettings.TabIndex  := TAB_INDEX_AREA;
        PageControlClient.TabIndex   := TAB_INDEX_AREA;
        PageControlClient.ActivePage := TabSheetArea;
        ShowMessage('メッシュが生成されていません');
        end;

    procedure OpenLogSettingsPage;
        begin
        if CSLogFrame.AbleToOpen then
            begin
            CSLogFrame.CSLogSettingsShow;
             TabControlSettings.TabIndex := TAB_INDEX_CSLOG;
            PageControlClient.ActivePage := TabSheetCSLog;
            end
        else
            WarnAndSetTabSheetArea;
        end;
    begin
    case TabControlSettings.TabIndex of
        TAB_INDEX_AREA   : PageControlClient.ActivePage := TabSheetArea;
        TAB_INDEX_MENGE  :
            begin
            if Assigned(PedestrianMapUser) and PedestrianMapUser.Enabled then
                PageControlClient.ActivePage := TabSheetMENGE
            else
                WarnAndSetTabSheetArea;
            end;
        TAB_INDEX_AGENT  : PageControlClient.ActivePage := TabSheetAGENT;
        TAB_INDEX_CSLOG  : OpenLogSettingsPage;
        else
            PageControlClient.ActivePage := TabSheetArea;
        end;

    PedestrianMapUser.Visible := SimulationAreaFrame.MeshVisible or ((TabControlSettings.TabIndex = TAB_INDEX_AREA)
                                                                  or (TabControlSettings.TabIndex = TAB_INDEX_CSLOG));

    SimulationAreaFrame.OnChangeTab(TabControlSettings.TabIndex = TAB_INDEX_AREA);
    CSLogFrame.OnChangeActivate(TabControlSettings.TabIndex = TAB_INDEX_CSLOG);
    end;

{ TCrowdSimGraphicComboEditLink }

procedure TCrowdSimGraphicComboEditLink.ComboChange(Sender: TObject);
    var
        comboBox : TComboBox;
        indexCombo : Integer;
    begin
    comboBox := TComboBox(EditControl);
    indexCombo := comboBox.ItemIndex;
    if indexCombo >= 0 then
        p_selectedObject := PickList.Objects[indexCombo]
    else
        p_selectedObject := nil;

    inherited ComboChange(Sender); // It will be set a text and GridNewText will be called.
    end;

procedure TCrowdSimGraphicComboEditLink.PrepareEditControl;
    var
        comboBox : TComboBox;
    begin
    inherited;
    comboBox := TComboBox(EditControl);
    comboBox.OnChange := ComboChange;
    comboBox.ItemIndex := PickList.IndexOfObject(p_selectedObject);
    end;

end.
