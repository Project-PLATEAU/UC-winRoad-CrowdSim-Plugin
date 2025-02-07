unit F_CrossSectionLogSettings;

interface

uses
    Winapi.Messages,
    System.SysUtils,
    System.DateUtils,
    System.Math,
    System.Classes,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.ComCtrls,
    F8RealSpinEdit,
    F8GLUtils,
    F8Utils,
    PluginCore,
    PedestrianMapUser,
    PedestrianCell,
    PedestrianUtil,
    CrowdSimLogSensorAreaRenderer,
    CrowdSimLogUtils,
    F8CrowdSimController;

type
    /// <summary>
    ///    �l���V�~�����[�V�����̃��O�W�v�ݒ肨��ђf�ʌ�ʗ��v���͈͂̐ݒ���s���t���[���Ƃ��̋@�\���`����N���X
    /// </summary>
    TFrameCrossSectionLogSettings = class(TFrame)
        PanelSensorAreaSettings: TPanel;
        PageControlCSLogSettings: TPageControl;
        TabSheetCSLogSettings: TTabSheet;
        LabelLogExportInterval: TLabel;
        LabelCellDataAddInterval: TLabel;
        LabelAreaName: TLabel;
        LabelAreaDrawSelect: TLabel;
        GroupBoxSensorAreaSettings: TGroupBox;
        SpinEditLogExportInterval: TF8RealSpinEdit;
        SpinEditCellDataAddInterval: TF8RealSpinEdit;
        EditSensorAreaName: TEdit;
        MemoSensorAreaInfos: TMemo;
        ComboBoxDrawAreaName: TComboBox;
        ButtonDeleteSensorArea: TButton;
        PanelSensorAreaInfo: TPanel;
        ButtonSetLeftTop: TButton;
        ButtonSetRightBottom: TButton;
        ButtonAddSensorAreas: TButton;
        PanelDataInterval: TPanel;
        GroupBoxDataInterval: TGroupBox;
        PanelCSLog: TPanel;
        GroupBoxSensorArea: TGroupBox;
        GroupBoxSelectSensorArea: TGroupBox;
        procedure ButtonAddSensorAreasClick(Sender: TObject);
        procedure SpinEditIntervalsChange(Sender: TObject);
        procedure ComboBoxDrawAreaNameChange(Sender: TObject);
        procedure ButtonDeleteSensorAreaClick(Sender: TObject);
        procedure PageControlCSLogSettingsChange(Sender: TObject);
        procedure ButtonSetLeftTopClick(Sender: TObject);
        procedure ButtonSetRightBottomClick(Sender: TObject);
        private
            p_PedestrianMapUser : TPedestrianMapUser;

            p_RemoveSensorDataQueue: TQueue<TSensorAreaData>;
            p_AddSensorDataQueue   : TQueue<TSensorAreaData>;
            p_FocusedSensorName    : String;

            p_IsActiveFrame    : Boolean;
            p_IsSetLeftTop     : boolean;
            p_IsSetRightBottom : boolean;
            p_AddedNewArea     : boolean;
            p_DeletedArea      : boolean;
            p_SALTCell         : TPedestrianCell;
            p_SARBCell         : TPedestrianCell;
            p_Converter        : IF8WrHorizontalCoordinateConvertor;

            p_SetX : double;
            p_SetZ : double;

            function  GetPedestrianMapUser: TPedestrianMapUser;
            function  GetNewSensorArea(out aNewArea: TSensorAreaData): Boolean;
            function  ConvertGLToLocal(const aPositionGL: TPoint3D): TPoint3D;
            procedure SetPedestrianMapUser(const aValue: TPedestrianMapUser);
            procedure SetSAInfos(const saList: TList<TSensorAreaData>);
            procedure AddSAInfo(const saData: TSensorAreaData);
            procedure UpdateMemoSAInfos(const saList: TList<TSensorAreaData>);
            procedure SetSADataForRenderer(const saList: TList<TSensorAreaData>);
            procedure SetButtonStatus;
            procedure OnFormMainTerrainClick(const ClickPoint : GLPointType; const ClickTri : F8TriangleType; var ClickCommand : ModelSelectCommandType);

            property  IsActiveFrame: Boolean read p_IsActiveFrame;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure UpdateSensorAreaRendererData(const aRenderer: TCrowdSimLogSensorAreaRenderer);
            procedure OnChangeUserMap;
            function  AbleToOpen: Boolean;
            procedure OnChangeActivate(const aActivate: Boolean);
            procedure CSLogSettingsShow;
            procedure ChangeEnabledIntervals(const status: boolean);

            property  PedestrianMapUser: TPedestrianMapUser read GetPedestrianMapUser write SetPedestrianMapUser;
        end;

implementation

const
    EDITING_AREA = '';

{$R *.dfm}

{ TFrameCrossSectionLogSettings }
//==============================================================================
procedure TFrameCrossSectionLogSettings.AfterConstruction;
    var
        method : Tmethod;
    begin
    inherited;
    theApplicationServices.mainForm.openGL.MakeCurrent(True);
    p_RemoveSensorDataQueue := TQueue<TSensorAreaData>.Create;
    p_AddSensorDataQueue    := TQueue<TSensorAreaData>.Create;
    p_FocusedSensorName     := '';
    p_IsActiveFrame         := False;
    p_IsSetLeftTop          := false;
    p_IsSetRightBottom      := false;
    p_AddedNewArea          := false;
    p_DeletedArea           := false;
    p_Converter             := theApplicationServices.GetWRCoordinateConvertor.HoirizontalCSConvertor;
    FormMainTerrainClickProc(method) := OnFormMainTerrainClick;
    theApplicationServices.RegisterEventHandler(_plgFormMainTerrainClick, method);
    p_SALTCell := nil;
    p_SARBCell := nil;
    p_SetX     := NaN;
    p_SetZ     := NaN;
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.BeforeDestruction;
    var
        method : Tmethod;
    begin
    inherited;
    p_Converter := nil;
    FreeAndNil(p_RemoveSensorDataQueue);
    FreeAndNil(p_AddSensorDataQueue);
    FormMainTerrainClickProc(method) := OnFormMainTerrainClick;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainTerrainClick, method);
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.ButtonAddSensorAreasClick(Sender: TObject);
    var
        sensorArea: TSensorAreaData;
    begin
    ButtonAddSensorAreas.Enabled := False;
    if EditSensorAreaName.Text = '' then
        begin
        ShowMessage('�v���͈͖���ݒ肵�Ă�������');
        ButtonAddSensorAreas.Enabled := True;
        Exit;
        end;

    if p_PedestrianMapUser.CrossSectionFlowLog.IsExistSameName(EditSensorAreaName.Text) then
        begin
        ShowMessage('���łɐݒ肵�Ă���v���͈͖��ł�');
        ButtonAddSensorAreas.Enabled := True;
        Exit;
        end;

    if not GetNewSensorArea(sensorArea) then
        begin
        ShowMessage('���b�V���͈͓��Ɏ��܂�悤�Ɏw�肵�Ă�������');
        ButtonAddSensorAreas.Enabled := True;
        Exit;
        end;

    p_PedestrianMapUser.CrossSectionFlowLog.AddSensorAreaData(sensorArea);
    AddSAInfo(sensorArea);
    p_AddSensorDataQueue.Enqueue(sensorArea);
    EditSensorAreaName.Text := '';
    p_SALTCell := nil;
    p_SARBCell := nil;
    p_SetX     := NaN;
    p_SetZ     := NaN;
    p_AddedNewArea := true;
    ShowMessage('�v���͈͂�ǉ����܂���');
    ButtonAddSensorAreas.Enabled := True;
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.ButtonDeleteSensorAreaClick(Sender: TObject);
    var
        idx: integer;
    begin
    if ComboBoxDrawAreaName.ItemIndex >= 0 then
        begin
        idx := ComboBoxDrawAreaName.ItemIndex;
        p_RemoveSensorDataQueue.Enqueue(p_PedestrianMapUser.CrossSectionFlowLog.SensorAreas[idx]);
        p_FocusedSensorName := '';
        p_PedestrianMapUser.CrossSectionFlowLog.DeleteSensorAreaData(idx);
        ComboBoxDrawAreaName.Items.Clear;
        UpdateMemoSAInfos(p_PedestrianMapUser.CrossSectionFlowLog.SensorAreas);
        ComboBoxDrawAreaName.Text      := '';
        ComboBoxDrawAreaName.ItemIndex := -1;
        p_DeletedArea := true;
        ShowMessage('�v���͈͂��폜���܂���');
        end;
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.ButtonSetLeftTopClick(Sender: TObject);
    begin
    p_IsSetRightBottom := false;
    if p_IsSetLeftTop = false then
        begin
        p_IsSetLeftTop := true;
        p_SetX         := NaN;
        p_SetZ         := NaN;
        end
    else
        p_IsSetLeftTop := false;

    SetButtonStatus;
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.ButtonSetRightBottomClick(Sender: TObject);
    begin
    p_IsSetLeftTop := false;
    if p_IsSetRightBottom = false then
        begin
        p_IsSetRightBottom := true;
        p_SetX             := NaN;
        p_SetZ             := NaN;
        end
    else
        p_IsSetRightBottom := false;

    SetButtonStatus;
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.AddSAInfo(const saData: TSensorAreaData);
    begin
    ComboBoxDrawAreaName.Items.Add(saData.SensorAreaName);
    MemoSensorAreaInfos.Lines.Add('�v���͈͖��F'+saData.SensorAreaName);
    MemoSensorAreaInfos.Lines.Add('�v���͈͍�����W�F�k��'+saData.LatLonOrigin.X.ToString+', ���o'+saData.LatLonOrigin.Y.ToString);
    MemoSensorAreaInfos.Lines.Add('�v���͈͉E�����W�F�k��'+saData.LatLonEnd.X.ToString+', ���o'+saData.LatLonEnd.Y.ToString);
    MemoSensorAreaInfos.Lines.Add('');
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.ChangeEnabledIntervals(const status: boolean);
    begin
    SpinEditLogExportInterval.Enabled   := status;
    SpinEditCellDataAddInterval.Enabled := status;
    end;

//==============================================================================
function TFrameCrossSectionLogSettings.GetPedestrianMapUser: TPedestrianMapUser;
    begin
    Result := p_PedestrianMapUser;
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.PageControlCSLogSettingsChange(Sender: TObject);
    var
        sensorData: TSensorAreaData;
    begin
    case PageControlCSLogSettings.ActivePageIndex of
        0:
            begin
            if GetNewSensorArea(sensorData) then
                begin
                p_AddSensorDataQueue.Enqueue(sensorData);
                sensorData.SensorAreaName := EDITING_AREA;
                p_RemoveSensorDataQueue.Enqueue(sensorData);
                if ComboBoxDrawAreaName.Text = '' then
                    p_FocusedSensorName := EDITING_AREA
                else
                    p_FocusedSensorName := ComboBoxDrawAreaName.Text;
                end;
            end;
        end;
    end;

//==============================================================================
function TFrameCrossSectionLogSettings.GetNewSensorArea(out aNewArea: TSensorAreaData): Boolean;
    var
        originCell: TPedestrianCell;
        endCell   : TPedestrianCell;
    begin
    Result := False;
    originCell := p_SALTCell;
    endCell    := p_SARBCell;
    if not Assigned(originCell) then
        Exit;

    if not Assigned(endCell) then
        Exit;

    aNewArea.SensorAreaName := EditSensorAreaName.Text;
    aNewArea.AreaOrigin     := originCell.AreaGL[VertexPositionType._LeftTop];
    aNewArea.AreaEnd        := endCell.AreaGL[VertexPositionType._RightBottom];
    aNewArea.LatLonOrigin   := originCell.Area[VertexPositionType._LeftTop];
    aNewArea.LatLonEnd      := endCell.Area[VertexPositionType._RightBottom];
    aNewArea.PassedNum      := 0;
    Result := True;
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.SetPedestrianMapUser(const aValue: TPedestrianMapUser);
    begin
    p_PedestrianMapUser := aValue;
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.SetButtonStatus;
    begin
    if p_IsSetLeftTop then
        ButtonSetLeftTop.Font.Style := [fsBold]
    else
        ButtonSetLeftTop.Font.Style := [];

    if p_IsSetRightBottom then
        ButtonSetRightBottom.Font.Style := [fsBold]
    else
        ButtonSetRightBottom.Font.Style := [];
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.CSLogSettingsShow;
    begin
    if Assigned(p_PedestrianMapUser.CrossSectionFlowLog.SensorAreas) then
        begin
        SetSAInfos(p_PedestrianMapUser.CrossSectionFlowLog.SensorAreas);
        SetSADataForRenderer(p_PedestrianMapUser.CrossSectionFlowLog.SensorAreas);
        end;

    p_PedestrianMapUser.CrossSectionFlowLog.SetIntervals(Trunc(SpinEditLogExportInterval.Value), Trunc(SpinEditCellDataAddInterval.Value));
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.UpdateMemoSAInfos(const saList: TList<TSensorAreaData>);
    begin
    if MemoSensorAreaInfos.Lines.Count > 0 then
        MemoSensorAreaInfos.Lines.Clear;

    for var i := 0 to saList.Count - 1 do
        AddSAInfo(saList[i]);
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.SetSAInfos(const saList: TList<TSensorAreaData>);
    begin
    if ComboBoxDrawAreaName.Items.Count > 0 then
        ComboBoxDrawAreaName.Items.Clear;

    if MemoSensorAreaInfos.Lines.Count > 0 then
        MemoSensorAreaInfos.Lines.Clear;

    for var i := 0 to saList.Count - 1 do
        AddSAInfo(saList[i]);
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.SpinEditIntervalsChange(Sender: TObject);
    begin
    p_PedestrianMapUser.CrossSectionFlowLog.SetIntervals(Trunc(SpinEditLogExportInterval.Value), Trunc(SpinEditCellDataAddInterval.Value));
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.ComboBoxDrawAreaNameChange(Sender: TObject);
    var
        saData : TSensorAreaData;
    begin
    if ComboBoxDrawAreaName.ItemIndex >= 0 then
        begin
        saData := p_PedestrianMapUser.CrossSectionFlowLog.SensorAreas[ComboBoxDrawAreaName.ItemIndex];
        p_FocusedSensorName := saData.SensorAreaName;
        end;
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.UpdateSensorAreaRendererData(const aRenderer: TCrowdSimLogSensorAreaRenderer);

    //--------------------------------------------------------------------------
    procedure UpdateEditingData(const UpdatePos: TPoint3D; SAData: TSensorAreaData);
        begin
        if p_AddedNewArea then
            begin
            aRenderer.SelectedPointLT := UpdatePos;
            aRenderer.SelectedPointRB := UpdatePos;
            p_AddedNewArea := false;
            ButtonSetLeftTop.Enabled     := true;
            ButtonSetRightBottom.Enabled := true;
            end
        else if p_IsSetLeftTop then
            begin
            aRenderer.RemoveSensorData(SAData);
            aRenderer.SelectedPointLT := UpdatePos;
            if (IsNaN(aRenderer.SelectedPointLT.X) = false) and (IsNaN(aRenderer.SelectedPointLT.Z) = false) then
                begin
                p_IsSetLeftTop := false;
                ButtonSetLeftTop.Enabled     := true;
                ButtonSetRightBottom.Enabled := true;
                end;
            end
        else if p_IsSetRightBottom then
            begin
            aRenderer.RemoveSensorData(SAData);
            aRenderer.SelectedPointRB := UpdatePos;
            if (IsNaN(aRenderer.SelectedPointRB.X) = false) and (IsNaN(aRenderer.SelectedPointRB.Z) = false) then
                begin
                p_IsSetRightBottom := false;
                ButtonSetLeftTop.Enabled     := true;
                ButtonSetRightBottom.Enabled := true;
                end;
            end;
        end;

    //--------------------------------------------------------------------------
    var
        tmpTP3D   : TPoint3D;
        EditingSA : TSensorAreaData;
        newData   : TSensorAreaData;
    begin
    aRenderer.ClearSensorData;
    if not IsActiveFrame then
        begin
        ButtonSetLeftTop.Enabled     := true;
        ButtonSetRightBottom.Enabled := true;
        Exit;
        end;

    SetSADataForRenderer(p_PedestrianMapUser.CrossSectionFlowLog.SensorAreas);
    if p_DeletedArea then
        p_DeletedArea := false;

    while p_RemoveSensorDataQueue.Count > 0 do
        aRenderer.RemoveSensorData(p_RemoveSensorDataQueue.Dequeue);

    while p_AddSensorDataQueue.Count > 0 do
        aRenderer.AddOrSetSensorData(p_AddSensorDataQueue.Dequeue);

    tmpTP3D.X := p_SetX;
    tmpTP3D.Z := p_SetZ;
    EditingSA.SensorAreaName := EditSensorAreaName.Text;
    UpdateEditingData(tmpTP3D, EditingSA);
    if (GetNewSensorArea(newData)) and (p_IsSetLeftTop = false) and (p_IsSetRightBottom = false) then
        begin
        newData.SensorAreaName := EDITING_AREA;
        p_RemoveSensorDataQueue.Enqueue(newData);
        p_AddSensorDataQueue.Enqueue(newData);
        end;

    aRenderer.FocusedName := p_FocusedSensorName;
    aRenderer.SetLTMode   := p_IsSetLeftTop;
    aRenderer.SetRBMode   := p_IsSetRightBottom;
    SetButtonStatus;
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.OnChangeUserMap;
    begin
    if Assigned(p_RemoveSensorDataQueue) then
        p_RemoveSensorDataQueue.Clear;

    if Assigned(MemoSensorAreaInfos) then
        begin
        if MemoSensorAreaInfos.Lines.Count > 0 then
            MemoSensorAreaInfos.Lines.Clear;
        end;

    if Assigned(p_AddSensorDataQueue) then
        p_AddSensorDataQueue.Clear;
    end;

//==============================================================================
/// <summary>�N���b�N���ꂽUC-win/Road��Ԃ̒n�_�Ɋ�Â��Ēf�ʌ�ʗ��v���͈͂�ݒ肷��֐�</summary>
procedure TFrameCrossSectionLogSettings.OnFormMainTerrainClick(const ClickPoint : GLPointType; const ClickTri : F8TriangleType; var ClickCommand : ModelSelectCommandType);

    //--------------------------------------------------------------------------
    procedure InitializeSettingData;
        begin
        if p_IsSetLeftTop then
            p_SALTCell := nil
        else if p_IsSetRightBottom then
            p_SARBCell := nil;

        p_SetX     := NaN;
        p_SetZ     := NaN;
        ButtonSetLeftTop.Enabled     := true;
        ButtonSetRightBottom.Enabled := true;
        end;

    //--------------------------------------------------------------------------
    function SetClickPoint(const MapOri, MapEnd: TPedestrianCell): boolean;
        var
            LocalCP : TPoint3D;
        begin
        Result := false;
        LocalCP := ConvertGLToLocal(Point3D(ClickPoint[_x], ClickPoint[_y], ClickPoint[_z]));
        if (ClickPoint[_x] < MapOri.AreaGL[VertexPositionType._LeftTop].X) or (ClickPoint[_x] > MapEnd.AreaGL[VertexPositionType._RightBottom].X) then
            begin
            InitializeSettingData;
            ShowMessage('���b�V���͈͓����N���b�N���Ă�������');
            Exit;
            end
        else
            p_SetX := LocalCP.X;

        if (ClickPoint[_z] < MapOri.AreaGL[VertexPositionType._LeftTop].Z) or (ClickPoint[_z] > MapEnd.AreaGL[VertexPositionType._RightBottom].Z) then
            begin
            InitializeSettingData;
            ShowMessage('���b�V���͈͓����N���b�N���Ă�������');
            Exit;
            end
        else
            p_SetZ := LocalCP.Z;

        Result := true;
        end;

    //--------------------------------------------------------------------------
    var
        differ  : TPoint3D;
        MapOri  : TPedestrianCell;
        MapEnd  : TPedestrianCell;
    begin
    ClickCommand := _msc_none;
    if not IsActiveFrame then
        Exit;

    if (p_IsSetLeftTop = false) and (p_IsSetRightBottom = false) then
        Exit;

    ButtonSetLeftTop.Enabled     := false;
    ButtonSetRightBottom.Enabled := false;
    p_PedestrianMapUser.Map.RequireCellInfo(0, MapOri);
    p_PedestrianMapUser.Map.RequireCellInfo(p_PedestrianMapUser.Map.Config.AllCellCount - 1, MapEnd);
    if not SetClickPoint(MapOri, MapEnd) then
        Exit;

    if p_IsSetLeftTop then
        begin
        p_PedestrianMapUser.Map.RequireCellInfo(Point3D(p_SetX, 0, p_SetZ), p_SALTCell, differ);
        p_SetX := p_SALTCell.AreaGL[VertexPositionType._LeftTop].X;
        p_SetZ := p_SALTCell.AreaGL[VertexPositionType._LeftTop].Z;
        if Assigned(p_SARBCell) then
            begin
            if (p_SetX > p_SARBCell.AreaGL[VertexPositionType._RightBottom].X)
                or (p_SetZ > p_SARBCell.AreaGL[VertexPositionType._RightBottom].Z) then
                begin
                InitializeSettingData;
                ShowMessage('�v���͈͂̍���ƉE���̈ʒu�֌W���t�]���Ă��܂�');
                Exit;
                end;
            end;
        end
    else if p_IsSetRightBottom then
        begin
        p_PedestrianMapUser.Map.RequireCellInfo(Point3D(p_SetX, 0, p_SetZ), p_SARBCell, differ);
        p_SetX := p_SARBCell.AreaGL[VertexPositionType._RightBottom].X;
        p_SetZ := p_SARBCell.AreaGL[VertexPositionType._RightBottom].Z;
        if Assigned(p_SALTCell) then
            begin
            if (p_SetX < p_SALTCell.AreaGL[VertexPositionType._LeftTop].X)
                or (p_SetZ < p_SALTCell.AreaGL[VertexPositionType._LeftTop].Z) then
                begin
                InitializeSettingData;
                ShowMessage('�v���͈͂̍���ƉE���̈ʒu�֌W���t�]���Ă��܂�');
                Exit;
                end;
            end;
        end;
    end;

//==============================================================================
function TFrameCrossSectionLogSettings.AbleToOpen: Boolean;
    begin
    Result := False;
    if not Assigned(p_PedestrianMapUser) then
        Exit;

    Result := p_PedestrianMapUser.Enabled;
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.OnChangeActivate(const aActivate: Boolean);
    begin
    p_IsActiveFrame := aActivate;
    end;

//==============================================================================
procedure TFrameCrossSectionLogSettings.SetSADataForRenderer(const saList: TList<TSensorAreaData>);
    var
        i : integer;
    begin
    if saList.Count < 1 then
        Exit;

    for i := 0 to saList.Count - 1 do
        begin
        p_RemoveSensorDataQueue.Enqueue(saList[i]);
        p_AddSensorDataQueue.Enqueue(saList[i]);
        end;
    end;

//==============================================================================
function TFrameCrossSectionLogSettings.ConvertGLToLocal(const aPositionGL: TPoint3D): TPoint3D;
    var
        srcPoint, dstPoint: F8PointType;
    begin
    srcPoint[_x] := aPositionGL.X;
    srcPoint[_y] := aPositionGL.Z;

    p_Converter.Convert(_hctOpenGL_XZ, 0, _hctLocal_XY, 0, srcPoint, dstPoint);

    Result.X := dstPoint[_x];
    Result.Y := aPositionGL.Y;
    Result.Z := dstPoint[_y];
    end;
end.
