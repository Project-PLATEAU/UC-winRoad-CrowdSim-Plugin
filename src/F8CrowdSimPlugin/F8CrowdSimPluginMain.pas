//Main unit of the plug-in, it declares and implements the plug-in object
//that will be loaded by UC-win/Road.
unit F8CrowdSimPluginMain;

interface

uses
    Winapi.Windows,
    System.SysUtils,
    System.Classes,
    System.Math,
    Vcl.StdCtrls,
    Vcl.Dialogs,
    PluginCore,
    CrowdSimPlayerForm,
    MengeXML,
    F8OpenGL,
    F8GLUtils,
    TranRoadList,
    CityGMLAfterImportEventEntry,
    CrowdSimLogSensorAreaRenderer,
    PedestrianMapList,
    PedestrianMapUser,
    F8CrowdSimController,
    SensorDataLoaderSettingForm,
    TrafficSensor,
    TrafficSensorDat;

type
    /// <summary>
    ///    人流シミュレーションシステムのメインクラス
    /// </summary>
    TF8CrowdSimPlugin = class(TF8PLuginClass, IF8OpenGLPlugin)
        private
            p_winRoadApplication : IF8ApplicationServices;
            p_ribbon : IF8Ribbon;
            p_ribbonTab : IF8RibbonTab;
            p_ribbonGroup : IF8RibbonGroup;

            ButtonCrowdSimPlayer    : TButton;
            p_formCrowdSimPlayer    : TFormCrowdSimPlayer;
            p_MengeXML : MengeXmlClass;
            p_Controller : F8CrowdSimControllerClass;

            {CityGML}
            p_CityGMLRoadList: TTranRoadList;
            p_CityGMLEvent: TCityGMLPluginAfterImportEventEntry;
            p_PedestrianMapList: TPedestrianMapList;
            p_PedestrianMapUser: TPedestrianMapUser;

            p_AreaSensorRenderer: TCrowdSimLogSensorAreaRenderer;

            p_SensorDataLoaderSettingForm : TFormSensorDataLoaderSetting;
            p_SensorData                  : TrafficSensorListClass;
            p_SensorDatData               : TrafficSensorDatClass;

            procedure   CreateRibbonGUI;
            function    CreateButton(const menuName, menuCaption : String; const onClick : TNotifyEvent) : TButton;
            procedure   DestroyRibbonGUI;
            procedure   DestroyViews;
            procedure   CrowdSimPlayerMenuClick(sender : Tobject);

            // IF8OpenGL implements
            procedure   SetupCamera(const opengl : TF8OpenGL; const view: ViewType);
            procedure   PaintScene(const opengl : TF8OpenGL; const view: ViewType);
            procedure   PaintHUD(const opengl : TF8OpenGL; const view: ViewType);

            procedure   ImportMFJsonClick(Sender: TObject);
            procedure   ImportTrafficSensorClick(Sender: TObject);
            procedure   AbleMenus(enable: Boolean);
            procedure   FixupPluginData;
            procedure   BeforeSaveProject(const name : string);
            procedure   ProjectDestroy;

            procedure   TransientWorldAfterMove(dTimeInSeconds : Double);

            procedure   BeforeTrafficStarted;
            procedure   TrafficStarted;
            procedure   TrafficStopped;

            procedure   OnImportSensorsData;
            procedure   OnImportSensorDetails;
            procedure   OnImportInOutDat;
            procedure   OnImportGenderOldDat;

            function    OnBeforeExportXMLEvent: Boolean;
        public
            procedure   AfterConstruction; override;
            procedure   BeforeDestruction; override;

            property    winRoadApplication : IF8ApplicationServices read p_winRoadApplication;
            property    PedestrianMapList  : TPedestrianMapList     read p_PedestrianMapList;
            property    SensorData         : TrafficSensorListClass read p_SensorData;
            property    SensorDatData      : TrafficSensorDatClass  read p_SensorDatData;
        end;

    //Call back functions
    procedure RegisterUserPlugin(out optionName, apiVersion, Copyright : String);
    procedure LoadPlugin;
    procedure UnloadPlugin;

var
    F8CrowdSimPlugin : TF8CrowdSimPlugin;

implementation

uses
    Vcl.Controls,
    MFJsonLoaderSettingForm,
    MovingFeature,
    MFJsonLoader,
    TrafficSensorLoader,
    GL,
    F8Utils,
    PedestrianUtil,
    LatLonHelper;

const
    PLUGIN_NAME = 'MF-Json Loader Plugin';
    COPY_RIGHT = 'FORUM8 Co., Ltd.';

    MENU_ITEM_MFJSON_IMPORT = 'Import MF-Json';
    MENU_ITEM_TRAFFICSENSORDATA_IMPORT = 'Import TrafficSernsorData';

//==============================================================================
//   Register / Unregister the plugin
//==============================================================================
procedure RegisterUserPlugin(out optionName, apiVersion, Copyright : String);
    begin
    optionName := PLUGIN_NAME;
    apiVersion := PLUGIN_VERSION;
    Copyright := COPY_RIGHT;
    end;

procedure LoadPlugin;
    begin
    if not Assigned(F8CrowdSimPlugin) then
        F8CrowdSimPlugin := TF8CrowdSimPlugin.Create;
    if Assigned(F8CrowdSimPlugin) and Assigned(F8CrowdSimPlugin.winRoadApplication) then
        F8CrowdSimPlugin.winRoadApplication.RegisterPluginObject(F8CrowdSimPlugin);
    end;

procedure UnloadPlugin;
    begin
    if Assigned(F8CrowdSimPlugin) and Assigned(F8CrowdSimPlugin.winRoadApplication) then
        F8CrowdSimPlugin.winRoadApplication.UnRegisterPluginObject(F8CrowdSimPlugin);
    FreeAndNil(F8CrowdSimPlugin);
    end;

{ TAPlugin }

//==============================================================================
//  On plugin creation :
//     Gets the interface of the application.
//==============================================================================
procedure TF8CrowdSimPlugin.AfterConstruction;
    procedure RegisterAppEvents;
        var
            method : TMethod;
        begin
        PluginAbleMenusProc(method) := AbleMenus;
        winRoadApplication.RegisterEventHandler(_plgPluginAbleMenus, method);

        PluginNotifyProc(method) := FixupPluginData;
        winRoadApplication.RegisterEventHandler(_plgFixUpPluginData, method);

        NameProc(method) := BeforeSaveProject;
        winRoadApplication.RegisterEventHandler(_plgBeforeSaveProject, method);

        PluginNotifyProc(method) := ProjectDestroy;
        winRoadApplication.RegisterEventHandler(_plgProjectDestroy, method);

        PluginNotifyProc(method) := BeforeTrafficStarted;
        winRoadApplication.RegisterEventHandler(_plgBeforeTrafficStarted, method);

        PluginNotifyProc(method) := TrafficStarted;
        winRoadApplication.RegisterEventHandler(_plgTrafficStarted, method);

        PluginNotifyProc(method) := TrafficStopped;
        winRoadApplication.RegisterEventHandler(_plgTrafficStopped, method);

        TransientWorldAfterMoveProc(method) := TransientWorldAfterMove;
        winRoadApplication.RegisterEventHandler(_plgTransientWorldAfterMove, method);
        end;
    begin
    Supports(ApplicationServices, IF8ApplicationServices, p_winRoadApplication);

    //Add the sub-menu for simplified model to File - Import
    winRoadApplication.mainForm.AddImportExportMenuItem(MENU_ITEM_MFJSON_IMPORT,
                                                        'Import MF-Json...',
                                                        True,
                                                        ImportMFJsonClick);

    winRoadApplication.mainForm.AddImportExportMenuItem(MENU_ITEM_TRAFFICSENSORDATA_IMPORT,
                                                        'Import TrafficSernsorData...',
                                                        True,
                                                        ImportTrafficSensorClick);

    RegisterAppEvents;

    p_MengeXML := MengeXmlClass.Create();
    CreateRibbonGUI;

    p_CityGMLRoadList := TTranRoadList.Create;
    p_controller := F8CrowdSimControllerClass.Create(p_winRoadApplication);
    p_CityGMLEvent := TCityGMLPluginAfterImportEventEntry.Create(p_winRoadApplication, p_CityGMLRoadList);
    p_PedestrianMapList := TPedestrianMapList.Create(p_winRoadApplication, p_CityGMLRoadList);
    p_PedestrianMapUser := TPedestrianMapUser.Create(p_winRoadApplication, p_controller);

    p_MengeXML.Scene.RegisterExportNodeEvent(p_PedestrianMapUser.ExportObstacleXML);
    p_MengeXML.OnExportNavMesh := p_PedestrianMapUser.ExportRoadMap;
    p_MengeXML.OnExportVpsJson := p_PedestrianMapUser.ExportVpsJson;
    p_AreaSensorRenderer := TCrowdSimLogSensorAreaRenderer.Create;
    p_MengeXML.AssignMap(p_PedestrianMapUser);

    p_PedestrianMapList.RegisterOnChangeActiveMapIndexEvent(p_PedestrianMapUser.OnChangeActiveMapIndex);

    p_SensorData    := TrafficSensorListClass.Create;
    p_SensorDatData := TrafficSensorDatClass.Create;
    end;

//==============================================================================
//  On plugin destruction :
//     Releases the interface of the application.
//==============================================================================
procedure TF8CrowdSimPlugin.BeforeDestruction;
    procedure UnRegisterAppEvents;
        var
            method : TMethod;
        begin
        PluginAbleMenusProc(method) := AbleMenus;
        winRoadApplication.UnRegisterEventHandler(_plgPluginAbleMenus, method);
        PluginNotifyProc(method) := FixupPluginData;
        winRoadApplication.UnRegisterEventHandler(_plgFixUpPluginData, method);
        NameProc(method) := BeforeSaveProject;
        winRoadApplication.UnRegisterEventHandler(_plgBeforeSaveProject, method);
        PluginNotifyProc(method) := ProjectDestroy;
        winRoadApplication.UnRegisterEventHandler(_plgProjectDestroy, method);
        PluginNotifyProc(method) := BeforeTrafficStarted;
        winRoadApplication.UnRegisterEventHandler(_plgBeforeTrafficStarted, method);
        PluginNotifyProc(method) := TrafficStarted;
        winRoadApplication.UnRegisterEventHandler(_plgTrafficStarted, method);
        PluginNotifyProc(method) := TrafficStopped;
        winRoadApplication.UnRegisterEventHandler(_plgTrafficStopped, method);
        TransientWorldAfterMoveProc(method) := TransientWorldAfterMove;
        winRoadApplication.UnRegisterEventHandler(_plgTransientWorldAfterMove, method);
        end;
    begin
    DestroyRibbonGUI;
    DestroyViews;
    if Assigned(p_SensorDataLoaderSettingForm) then
        FreeAndNil(p_SensorDataLoaderSettingForm);

    p_SensorData    := nil;
    p_SensorDatData := nil;

    p_MengeXML.Scene.UnRegisterExportNodeEvent(p_PedestrianMapUser.ExportObstacleXML);
    p_PedestrianMapList.UnRegisterOnChangeActiveMapIndexEvent(p_PedestrianMapUser.OnChangeActiveMapIndex);

    p_MengeXML.OnExportNavMesh := nil;
    p_MengeXML.OnExportVpsJson := nil;

    UnRegisterAppEvents;

    winRoadApplication.mainForm.RemoveImportExportMenuItem(MENU_ITEM_MFJSON_IMPORT, True);
    winRoadApplication.mainForm.RemoveImportExportMenuItem(MENU_ITEM_TRAFFICSENSORDATA_IMPORT, True);

    FreeAndNil(p_MengeXML);
    FreeAndNil(p_CityGMLEvent);
    FreeAndNil(p_PedestrianMapUser);
    FreeAndNil(p_PedestrianMapList);
    FreeAndNil(p_CityGMLRoadList);
    FreeAndNil(p_AreaSensorRenderer);
    FreeAndNil(p_Controller);

    MovingFeatureListClass.BeforeDestructions;
    p_winRoadApplication := nil;
    end;

function TF8CrowdSimPlugin.CreateButton(const menuName, menuCaption: String; const onClick: TNotifyEvent): TButton;
    begin
    p_ribbonGroup.Width := p_ribbonGroup.Width + 107;

    result := TButton.Create(p_ribbonGroup.GetSelf);
    result.Name := menuName;
    result.Align := alLeft;
    result.Margins.Left := 1;
    result.Margins.Top := 1;
    result.Margins.Bottom := 4;
    result.Margins.Right := 1;
    result.AlignWithMargins := true;
    result.Width := 105;
    result.Caption := menuCaption;
    result.WordWrap := True;
    result.OnClick := onClick;
    end;

procedure TF8CrowdSimPlugin.CreateRibbonGUI;
    const
        PLUGIN_TABCONTROLNAME = 'CrowdSimTabControl';
        PLUGIN_TABCAPTION = 'Crowd Sim';
        PLUGIN_RIBBONGROUP_CONTROLNAME = 'CrowdSimPlayer';
        PLUGIN_RIBBONGROUP_NAME = 'Crowd Sim Player';
    begin
    if Assigned(p_winRoadApplication) and not Assigned(p_ribbon) then
        begin
        p_ribbon := p_winRoadApplication.mainForm.GetMainRibbonMenu;
        p_ribbonTab := p_winRoadApplication.mainForm.GetMainRibbonMenuTabByName(PLUGIN_TABCONTROLNAME);
        if Assigned(p_ribbon) and not Assigned(p_ribbonTab)then
            begin
            p_ribbonTab := p_Ribbon.CreateRibbonTab(PLUGIN_TABCONTROLNAME, 11000);
            p_ribbonTab.Caption := PLUGIN_TABCAPTION;
            end;
        end;

    p_ribbonGroup := p_ribbonTab.CreateRibbonGroup(PLUGIN_RIBBONGROUP_CONTROLNAME, 20);
    p_ribbonGroup.Caption := PLUGIN_RIBBONGROUP_NAME;
    p_ribbonGroup.AutoPositionControl := false;
    p_ribbonGroup.Width := 2;

    ButtonCrowdSimPlayer := CreateButton('ClowdSimPlayerMenu', 'Crowd Sim Player...', CrowdSimPlayerMenuClick);
    p_ribbonGroup.AddGroupControl(ButtonCrowdSimPlayer);
    end;

procedure TF8CrowdSimPlugin.CrowdSimPlayerMenuClick(sender: Tobject);
    begin
    if not Assigned(p_formCrowdSimPlayer) then
        begin
        p_formCrowdSimPlayer := TFormCrowdSimPlayer.Create(nil);
        p_formCrowdSimPlayer.OnBeforeExportXMLEvent := OnBeforeExportXMLEvent;

        p_formCrowdSimPlayer.application := p_winRoadApplication;
        p_formCrowdSimPlayer.mengeXML := p_MengeXML;
        p_formCrowdSimPlayer.PedestrianMapList := p_PedestrianMapList;
        p_formCrowdSimPlayer.PedestrianMapUser := p_PedestrianMapUser;
        p_formCrowdSimPlayer.Controller := p_Controller;
        p_PedestrianMapUser.RegisterOnChangeUserPedestrianMapEvent(p_formCrowdSimPlayer.OnChangedUserMap);
        end;
    p_MengeXML.Sensors := p_SensorData;
    p_formCrowdSimPlayer.Show;
    end;

procedure TF8CrowdSimPlugin.DestroyRibbonGUI;
    begin
    //Remove Control from the RibbonGroup
    if Assigned(p_RibbonGroup) then
        begin
        p_RibbonGroup.RemoveGroupControl(ButtonCrowdSimPlayer);
        ButtonCrowdSimPlayer := nil;
        end;

    //Remove RibbonGroup from the RibbonTab
    if Assigned(p_RibbonTab) then
        begin
        p_RibbonTab.DeleteGroup(p_RibbonGroup);
        p_RibbonGroup := nil;
        end;

    p_RibbonTab := nil;
    p_Ribbon := nil;
    end;

procedure TF8CrowdSimPlugin.DestroyViews;
    begin
    if Assigned(p_formCrowdSimPlayer) then
        begin
        p_formCrowdSimPlayer.Close;
        p_PedestrianMapUser.UnRegisterOnChangeUserPedestrianMapEvent(p_formCrowdSimPlayer.OnChangedUserMap);
        p_formCrowdSimPlayer.Free;
        p_formCrowdSimPlayer := nil;
        end;
    end;

procedure TF8CrowdSimPlugin.ImportMFJsonClick(Sender: TObject);
    var
        aform : TFormMFJsonLoaderSetting;
    begin
    if not Assigned(winRoadApplication.project) then
        begin
        ShowMessage('先にプロジェクトを作成してください。');
        Exit();
        end;

    if Assigned(p_formCrowdSimPlayer) then
        begin
        p_formCrowdSimPlayer.Close;
        FreeAndNil(p_formCrowdSimPlayer);
        end;

    aform := TFormMFJsonLoaderSetting.Create(nil);
    try
        aform.app := winRoadApplication;
        aform.project := winRoadApplication.project;
        aform.ShowModal;
    finally
        if MovingFeatureListClass.numberOfMovingFeatures > 0 then
            ButtonCrowdSimPlayer.Enabled := True;
        aform.Free;
        end;
    end;

procedure TF8CrowdSimPlugin.ImportTrafficSensorClick(Sender: TObject);
    begin
    if not Assigned(winRoadApplication.project) then
        begin
        ShowMessage('先にプロジェクトを作成してください。');
        Exit();
        end;

    if not Assigned(p_SensorDataLoaderSettingForm) then
        p_SensorDataLoaderSettingForm := TFormSensorDataLoaderSetting.Create(nil);

    p_SensorDataLoaderSettingForm.OnImportSensorsData   := OnImportSensorsData;
    p_SensorDataLoaderSettingForm.OnImportSensorDetails := OnImportSensorDetails;
    p_SensorDataLoaderSettingForm.OnImportInOutDat      := OnImportInOutDat;
    p_SensorDataLoaderSettingForm.OnImportGenderOldDat  := OnImportGenderOldDat;
    try
        p_SensorDataLoaderSettingForm.app := winRoadApplication;
        p_SensorDataLoaderSettingForm.project := winRoadApplication.project;
        p_SensorDataLoaderSettingForm.ShowModal;
    finally
        p_SensorDataLoaderSettingForm.OnImportGenderOldDat  := nil;
        p_SensorDataLoaderSettingForm.OnImportInOutDat      := nil;
        p_SensorDataLoaderSettingForm.OnImportSensorDetails := nil;
        p_SensorDataLoaderSettingForm.OnImportSensorsData   := nil;
        end;
    end;

procedure TF8CrowdSimPlugin.OnImportSensorsData;
    var
        i    : integer;
        name : string;
    begin
    if not Assigned(p_SensorDataLoaderSettingForm) then
        Exit;

    if not Assigned(p_SensorData) then
        p_SensorData := TrafficSensorListClass.Create;

    if p_SensorDataLoaderSettingForm.CheckBoxAddedData.Checked = false then
        p_SensorData.ClearSensorData;

    for i := 0 to p_SensorDataLoaderSettingForm.OpenDialogJson.Files.Count - 1 do
        begin
        name := p_SensorDataLoaderSettingForm.OpenDialogJson.Files[i];
        try
            ImportTrafficSensorJson(winRoadApplication.project, name, p_SensorData);
            p_SensorDataLoaderSettingForm.ImportedSensorData := true;
        finally
            p_SensorDataLoaderSettingForm.SensorData := p_SensorData;
            end;
        end;
    end;

procedure TF8CrowdSimPlugin.OnImportSensorDetails;
    var
        i    : integer;
        name : string;
    begin
    if not Assigned(p_SensorDataLoaderSettingForm) then
        Exit;

    if not Assigned(p_SensorData) then
        p_SensorData := TrafficSensorListClass.Create;

    if p_SensorDataLoaderSettingForm.CheckBoxAddedData.Checked = false then
        p_SensorData.ClearDetail;

    for i := 0 to p_SensorDataLoaderSettingForm.OpenDialogdetailJson.Files.Count - 1 do
        begin
        name := p_SensorDataLoaderSettingForm.OpenDialogdetailJson.Files[i];
        try
            ImportTrafficSensorDetailJson(winRoadApplication.project, name, p_SensorData);
            p_SensorDataLoaderSettingForm.ImportedDetailData := true;
        finally
            p_SensorDataLoaderSettingForm.SensorData := p_SensorData;
            end;
        end;
    end;

procedure TF8CrowdSimPlugin.OnImportInOutDat;
    var
        name : string;
    begin
    if not Assigned(p_SensorDataLoaderSettingForm) then
        Exit;

    if not Assigned(p_SensorDatData) then
        p_SensorDatData := TrafficSensorDatClass.Create;

    p_SensorDatData.ClearInOutData;
    name := p_SensorDataLoaderSettingForm.OpenDialogDat.FileName;
    try
        ImportTrafficSensorDat(winRoadApplication.project, name, p_SensorDatData);
        p_SensorDataLoaderSettingForm.ImportedDatData := true;
    finally
        p_SensorDataLoaderSettingForm.DatData := p_SensorDatData;
        end;
    end;

procedure TF8CrowdSimPlugin.OnImportGenderOldDat;
    var
        name : string;
    begin
    if not Assigned(p_SensorDataLoaderSettingForm) then
        Exit;

    if not Assigned(p_SensorDatData) then
        p_SensorDatData := TrafficSensorDatClass.Create;

    p_SensorDatData.ClearGenderOldData;
    name := p_SensorDataLoaderSettingForm.OpenDialogGenderOldDat.FileName;
    try
        ImportTrafficSensorGenderOldDat(winRoadApplication.project, name, p_SensorDatData);
        p_SensorDataLoaderSettingForm.ImportedDatData := true;
    finally
        p_SensorDataLoaderSettingForm.DatData := p_SensorDatData;
        end;
    end;

procedure TF8CrowdSimPlugin.PaintHUD(const opengl: TF8OpenGL; const view: ViewType);
    begin
    end;

procedure TF8CrowdSimPlugin.PaintScene(const opengl: TF8OpenGL; const view: ViewType);
    procedure RenderScene;
        begin
        if Assigned(p_formCrowdSimPlayer) then
            begin
            p_PedestrianMapList.Render(openGL);
            p_PedestrianMapUser.Render(OpenGL);
            end
        else
            p_PedestrianMapUser.Render(OpenGL);
        end;
    begin
    if view <> _ViewMain then
        Exit;

    RenderScene;

    if Assigned(p_formCrowdSimPlayer) then
        p_formCrowdSimPlayer.UpdateSensorAreaRendererData(p_AreaSensorRenderer);

    p_AreaSensorRenderer.Render(opengl);
{$IFDEF DEBUG}
    p_CityGMLRoadList.DoRender(opengl);
{$ENDIF}
    end;

procedure TF8CrowdSimPlugin.ProjectDestroy;
    begin
    if Assigned(p_formCrowdSimPlayer) then
        begin
        p_PedestrianMapUser.UnRegisterOnChangeUserPedestrianMapEvent(p_formCrowdSimPlayer.OnChangedUserMap);
        p_formCrowdSimPlayer.Close;
        FreeAndNil(p_formCrowdSimPlayer);
        end;
    MovingFeatureListClass.ClearMovingFeatures;

    if Assigned(p_SensorDataLoaderSettingForm) then
        FreeAndNil(p_SensorDataLoaderSettingForm);

    p_MengeXML.Sensors := nil;
    p_SensorData    := nil;
    p_SensorDatData := nil;
    end;

procedure TF8CrowdSimPlugin.SetupCamera(const opengl: TF8OpenGL; const view: ViewType);
    var
        glPosition: GLPointType;
    begin
    glPosition := opengl.Find3DCoordinatesUnderMouse;
    if F8IsAGLPoint(glPosition) and p_PedestrianMapUser.Enabled then
        p_PedestrianMapUser.UpdateUnderMouseGuide(Point3D(glPosition[_x], glPosition[_y], glPosition[_z]));

    if Assigned(p_formCrowdSimPlayer) then
        p_formCrowdSimPlayer.Collect;
    end;

procedure TF8CrowdSimPlugin.AbleMenus(enable: Boolean);
    begin
    enable := Assigned(p_winRoadApplication.project) and enable;
    winRoadApplication.mainForm.SetEnabledImportExportMenuItem(MENU_ITEM_MFJSON_IMPORT,
                                                               True,
                                                               enable);
    ButtonCrowdSimPlayer.Enabled := enable and (MovingFeatureListClass.numberOfMovingFeatures > 0);
    end;

procedure TF8CrowdSimPlugin.FixupPluginData;
    begin
    p_PedestrianMapList.ImportFromPluginData;
    p_CityGMLRoadList.ImportFromPluginData(winRoadApplication.project);
    end;

procedure TF8CrowdSimPlugin.BeforeSaveProject(const name : string);
    begin
    p_PedestrianMapList.ExportToPluginData;
    p_CityGMLRoadList.ExportToPluginData(winRoadApplication.project);
    end;

procedure TF8CrowdSimPlugin.BeforeTrafficStarted;
    begin
    MovingFeatureListClass.BeforeStartTraffic(winRoadApplication.project,p_Controller.IsRain);
    end;

procedure TF8CrowdSimPlugin.TrafficStarted;
    begin
    p_PedestrianMapList.TrafficStarted;
    end;

procedure TF8CrowdSimPlugin.TrafficStopped;
    begin
    MovingFeatureListClass.StopTraffic(winRoadApplication.project);
    p_PedestrianMapList.TrafficStopped;
    end;

procedure TF8CrowdSimPlugin.TransientWorldAfterMove(dTimeInSeconds: Double);
    begin
    if not Assigned(p_Controller) then
        Exit;
    if MovingFeatureListClass.numberOfMovingFeatures > 0 then
        p_Controller.UpdateTimer(dTimeInSeconds);
    end;

function TF8CrowdSimPlugin.OnBeforeExportXMLEvent: Boolean;
    begin
    Result := p_PedestrianMapUser.Map.OnBeforeExportXML;
    end;
end.
