﻿unit CrowdSimLogSensorAreaRenderer;

interface

uses
    System.Generics.Collections,
    F8OpenGL,
    GL,
    F8GLUtils,
    F8Utils,
    PluginCore,
    CrowdSimLogUtils;

type
    /// <summary>
    ///    断面交通流計測範囲を描画するクラス
    /// </summary>
    TCrowdSimLogSensorAreaRenderer = class
        private
            const
                FOCUSED_COLOR:GLPointType = (0.3, 0.8, 0.8, 1.0);
                NOT_FOCUSED_COLOR: GLPointType = (0.3, 0.5, 0.5, 1.0);
                DEFAULT_HEIGHT_OFFSET = 52.0;
            var
                p_RenderDataList  : TList<TSensorAreaData>;
                p_FocusedName     : String;
                p_SetLTMode       : boolean;
                p_SetRBMode       : boolean;
                p_SelectedPointLT : TPoint3D;
                p_SelectedPointRB : TPoint3D;

            procedure SetFocusedName(const aValue: String);

            procedure DoRender(const aOpenGL: TF8OpenGL; const aSensorArea: TSensorAreaData; const aHeightOffset: Double; const aIsFocused: Boolean);
            procedure SettingRender(const aOpenGL: TF8OpenGL; const aHeightOffset: Double);

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure Render(const aOpenGL: TF8OpenGL);

            procedure AddOrSetSensorData(const aSensorArea: TSensorAreaData);
            procedure RemoveSensorData(const aSensorArea: TSensorAreaData);
            procedure ClearSensorData;

            property  FocusedName : String  read p_FocusedName write SetFocusedName;
            property  SetLTMode   : boolean read p_SetLTMode   write p_SetLTMode;
            property  SetRBMode   : boolean read p_SetRBMode   write p_SetRBMode;
            property  SelectedPointLT : TPoint3D read p_SelectedPointLT write p_SelectedPointLT;
            property  SelectedPointRB : TPoint3D read p_SelectedPointRB write p_SelectedPointRB;
        end;

implementation

uses
    System.SysUtils,
    System.Math;

{ TCrowdSimLogSensorAreaRenderer }
procedure TCrowdSimLogSensorAreaRenderer.AfterConstruction;
    begin
    inherited;

    p_RenderDataList := TList<TSensorAreaData>.Create;
    p_SelectedPointLT.X := NaN;
    p_SelectedPointLT.Z := NaN;
    p_SelectedPointRB.X := NaN;
    p_SelectedPointRB.Z := NaN;
    end;

procedure TCrowdSimLogSensorAreaRenderer.BeforeDestruction;
    begin
    inherited;

    p_RenderDataList.Clear;
    FreeAndNil(p_RenderDataList);
    end;

procedure TCrowdSimLogSensorAreaRenderer.Render(const aOpenGL: TF8OpenGL);
    var
        data: TSensorAreaData;
    begin
    SettingRender(aOpenGL, DEFAULT_HEIGHT_OFFSET);
    for data in p_RenderDataList do
        DoRender(aOpenGL, data, DEFAULT_HEIGHT_OFFSET, (data.SensorAreaName = p_FocusedName));
    end;

procedure TCrowdSimLogSensorAreaRenderer.AddOrSetSensorData(const aSensorArea: TSensorAreaData);
    var
        i: Integer;
        find: Boolean;
    begin
    find := False;
    for i := 0 to p_RenderDataList.Count - 1 do
        begin
        if p_RenderDataList[i].SensorAreaName = aSensorArea.SensorAreaName then
            begin
            p_RenderDataList[i] := aSensorArea;
            find := True;
            Break;
            end;
        end;

    if not find then
        p_RenderDataList.Add(aSensorArea);
    end;

procedure TCrowdSimLogSensorAreaRenderer.RemoveSensorData(const aSensorArea: TSensorAreaData);
    var
        i: Integer;
    begin
    for i := p_RenderDataList.Count - 1 downto 0 do
        begin
        if p_RenderDataList[i].SensorAreaName = aSensorArea.SensorAreaName then
            begin
            p_RenderDataList.Delete(i);
            Break;
            end;
        end;
    end;

procedure TCrowdSimLogSensorAreaRenderer.ClearSensorData;
    begin
    p_RenderDataList.Clear;
    end;

procedure TCrowdSimLogSensorAreaRenderer.SetFocusedName(const aValue: String);
    begin
    p_FocusedName := aValue;
    end;

procedure TCrowdSimLogSensorAreaRenderer.SettingRender(const aOpenGL: TF8OpenGL; const aHeightOffset: Double);

    //--------------------------------------------------------------------------
    procedure GetSelectedHeight(const CurPos: GLPointType; SelectedPoint: TPoint3D; var terrainHeight: single);
        var
            HeightsData    : HeightResultArrayType;
            SelectedHeight : single;
            i              : integer;
        begin
        SelectedHeight := 0.0;
        HeightsData := theApplicationServices.project.GetHeightsAt(SelectedPoint.X, SelectedPoint.Z, [_hTerrain, _hRoad, _hIntersection]);
        if Length(HeightsData) > 0 then
            begin
            for i := 0 to Length(HeightsData) - 1 do
                SelectedHeight := Max(CurPos[_y], HeightsData[i].hHeight);
            end
        else
            Assert(False);

        terrainHeight := SelectedHeight + 0.6;
        end;

    //--------------------------------------------------------------------------
    procedure RenderArea(const CurPos: GLPointType; SelectedPoint: TPoint3D);
        var
            terrainHeight  : single;
        begin
        GetSelectedHeight(CurPos, SelectedPoint, terrainHeight);
        glMatrixMode(GL_MODELVIEW);
        glPushAttrib(GL_ALL_ATTRIB_BITS);
        glDisable(GL_LIGHTING);
        glDepthMask(GL_FALSE);
        try
            glLineWidth(5.0);
            glColor4f(FOCUSED_COLOR[_x], FOCUSED_COLOR[_y], FOCUSED_COLOR[_z], FOCUSED_COLOR[_w]);
            glPushMatrix;
            glBegin(GL_LINE_LOOP);
                glVertex3f(SelectedPoint.X, terrainHeight, SelectedPoint.Z);
                glVertex3f(CurPos[_x], terrainHeight, SelectedPoint.Z);
                glVertex3f(CurPos[_x], terrainHeight, CurPos[_z]);
                glVertex3f(SelectedPoint.X, terrainHeight, CurPos[_z]);
            glEnd;
            glPopMatrix;
        finally
            glLineWidth(1.0);
            glColor4f(1.0, 1.0, 1.0, 1.0);
            glPopAttrib;
            end;
        end;

    //--------------------------------------------------------------------------
    var
        CursorPos : GLPointType;
    begin
    if (SetLTMode = false) and (SetRBMode = false) then
        Exit;

    if (IsNaN(p_SelectedPointRB.X) = false) and (IsNaN(p_SelectedPointRB.Z) = false)
        and (IsNaN(p_SelectedPointLT.X) = false) and (IsNaN(p_SelectedPointLT.Z) = false) then
        Exit;

    CursorPos := aOpenGL.Find3DCoordinatesUnderMouse;
    if (IsNaN(p_SelectedPointRB.X) = false) and (IsNaN(p_SelectedPointRB.Z) = false) then
        RenderArea(CursorPos, p_SelectedPointRB)
    else if (IsNaN(p_SelectedPointLT.X) = false) and (IsNaN(p_SelectedPointLT.Z) = false) then
        RenderArea(CursorPos, p_SelectedPointLT);
    end;

procedure TCrowdSimLogSensorAreaRenderer.DoRender(const aOpenGL: TF8OpenGL; const aSensorArea: TSensorAreaData; const aHeightOffset: Double; const aIsFocused: Boolean);

    //--------------------------------------------------------------------------
    procedure GetHeight(var terrainHeight: single);
        var
            originHeights, endHeights: HeightResultArrayType;
            originHeight, endHeight: Single;
            i: Integer;
        begin
        terrainHeight := aHeightOffset;
        originHeight := 0.0;
        endHeight    := 0.0;
        originHeights := theApplicationServices.project.GetHeightsAt(aSensorArea.AreaOrigin.X, aSensorArea.AreaOrigin.Z, [_hTerrain, _hRoad, _hIntersection]);
        if Length(originHeights) > 0 then
            begin
            for i := 0 to Length(originHeights) - 1 do
                originHeight := Max(originHeight, originHeights[i].hHeight);
            end
        else
            Assert(False);

        endHeights := theApplicationServices.project.GetHeightsAt(aSensorArea.AreaEnd.X, aSensorArea.AreaEnd.Z, [_hTerrain, _hRoad, _hIntersection]);
        if Length(endHeights) > 0 then
            begin
            for i := 0 to Length(endHeights) - 1 do
                endHeight := Max(endHeight, endHeights[i].hHeight);
            end
        else
            Assert(False);

        terrainHeight := Max(originHeight, endHeight) + 0.6;
        end;

    //--------------------------------------------------------------------------
    var
        terrainHeight : single;
    begin
    GetHeight(terrainHeight);
    glMatrixMode(GL_MODELVIEW);
    glPushAttrib(GL_ALL_ATTRIB_BITS);
    glDisable(GL_LIGHTING);
    glDepthMask(GL_FALSE);
    try
        glLineWidth(5.0);
        if aIsFocused then
            glColor4f(FOCUSED_COLOR[_x], FOCUSED_COLOR[_y], FOCUSED_COLOR[_z], FOCUSED_COLOR[_w])
        else
            glColor4f(NOT_FOCUSED_COLOR[_x], NOT_FOCUSED_COLOR[_y], NOT_FOCUSED_COLOR[_z], NOT_FOCUSED_COLOR[_w]);
        glPushMatrix;
        glBegin(GL_LINE_LOOP);
            glVertex3f(aSensorArea.AreaOrigin.X, terrainHeight, aSensorArea.AreaOrigin.Z);
            glVertex3f(aSensorArea.AreaEnd.X, terrainHeight, aSensorArea.AreaOrigin.Z);
            glVertex3f(aSensorArea.AreaEnd.X, terrainHeight, aSensorArea.AreaEnd.Z);
            glVertex3f(aSensorArea.AreaOrigin.X, terrainHeight, aSensorArea.AreaEnd.Z);
        glEnd;
        glPopMatrix;
    finally
        glLineWidth(1.0);
        glColor4f(1.0, 1.0, 1.0, 1.0);
        glPopAttrib;
        end;
    end;
end.
