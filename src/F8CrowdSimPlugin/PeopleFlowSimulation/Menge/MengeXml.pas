unit MengeXml;

interface

uses
    System.Generics.Collections,
    System.Classes,
    PluginCore,
    MengeViewXml,
    MengeBehaviorXML,
    MengeSceneXML,
    MengeAgent,
    PedestrianMapUser,
    TrafficSensor;

type
    NavMeshExportProc = procedure(const aStrList: TStringList) of Object;
    VpsJsonExportProc = procedure(const aDir: String) of Object;
    SimSettingType = record
        startTime : TDateTime;
        endTime : TDateTime;
        maxWalkSpeed : double;
        useMagnification : Boolean;
        UU : Integer;
        population : Integer;
    end;

    /// <summary>
    ///    MengexmlファイルおよびMenge内の歩行エリアデータ出力処理を行うクラス
    /// </summary>
    MengeXmlClass = class
        private
            p_SimInc : Integer;
            p_Application : IF8ApplicationServices;

            p_Scene    : MengeSceneXMLClass;
            p_Bahavior : MengeBehaviorXmlClass;
            p_View     : MengeViewXmlClass;
            p_AgentList: MengeAgentList;
            p_NavMesh  : TStringList;
            p_Maps     : TPedestrianMapUser;
            p_Sensors  : TrafficSensorListClass;

            F_OnExportNavMeshProc: NavMeshExportProc;
            F_OnExportVpsJsonProc: VpsJsonExportProc;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ResetParams;
            function  SummarizeMovingFeature(startTime,EndTime : TDatetime) : Boolean;
            function  SummerizeXmls(path,projectName : string; startTime,EndTime: TDatetime; signalTime : Integer) : Boolean;
            function  ExportXmls(path,projectName : string; SignalTime,Elapsed : Integer) : Boolean;
            procedure Assignmap(map : TPedestrianMapUser);

            property Scene    : MengeSceneXMLClass    read p_Scene    write p_Scene;
            property Bahavior : MengeBehaviorXmlClass read p_Bahavior write p_Bahavior;
            property View     : MengeViewXmlClass     read p_View     write p_View;
            property Sensors  : TrafficSensorListClass read p_Sensors write p_Sensors;

            property OnExportNavMesh: NavMeshExportProc read F_OnExportNavMeshProc write F_OnExportNavMeshProc;
            property OnExportVpsJson: VpsJsonExportProc read F_OnExportVpsJsonProc write F_OnExportVpsJsonProc;
    end;

implementation

uses
    Winapi.Windows,
    Vcl.Dialogs,
    System.SysUtils,
    System.DateUtils,
    System.Math,
    F8Utils,
    F8GLUtils,
    MovingFeature,
    AgentSettings,
    MengeAgentDefinitions,MengeConditionDefinitions,MengeStateDefinitions,MengeGeneratorDefinitions,
    PedestrianMap;

{ MengeXmlClass }
//==============================================================================
procedure MengeXmlClass.AfterConstruction;
    begin
    inherited;
    Supports(ApplicationServices, IF8ApplicationServices, p_Application);
    p_AgentList:= MengeAgentList.Create;
    p_Scene   := MengeSceneXMLClass.Create(p_AgentList);
    p_Bahavior := MengeBehaviorXmlClass.Create(p_AgentList);

    p_View   := MengeViewXmlClass.Create;
    p_NavMesh := TStringList.Create;
    F_OnExportNavMeshProc := nil;
    F_OnExportVpsJsonProc := nil;
    end;

//==============================================================================
procedure MengeXmlClass.BeforeDestruction;
    begin
    FreeAndNil(p_Scene);
    FreeAndNil(p_Bahavior);
    FreeAndNil(p_View);
    FreeAndNil(p_AgentList);
    FreeAndNil(p_NavMesh);
    p_Sensors := nil;

    F_OnExportNavMeshProc := nil;
    F_OnExportVpsJsonProc := nil;
    inherited;
    end;

//==============================================================================
procedure MengeXmlClass.Assignmap(map: TPedestrianMapUser);
    begin
    p_Maps := map;
    end;

//==============================================================================
function MengeXmlClass.SummerizeXmls(path, projectName: string; startTime, EndTime: TDatetime; signalTime : Integer) : Boolean;
    var
        SimTime,dTime : Integer;//Sec
        xml : TStringList;
        Elapsed : Integer;
    begin
    Result := True;
    ResetParams;

    dTime := SecondsBetween(startTime,EndTime);
    SimTime := Min(signalTime,dTime);
    p_SimInc := 0;


    xml := TStringList.Create;
    try
        xml.Add('<Project ' +
        'scene="'+projectName+'/'+projectName+'S.xml" ' +
        'behavior="'+projectName+'/'+projectName+'B.xml" ' +
        'view="'+projectName+'/'+projectName+'V.xml" ' +
        'model="orca" dumpPath="images/test" ' +
        'duration="'+IntToStr(SimTime)+'"'+
        '/>'
        );
        xml.SaveToFile(path + '\' + projectName + '.xml');
    finally
        xml.Clear;
        end;

    Result := Result and SummarizeMovingFeature(startTime,EndTime);

    p_View.ExportXml(path+'\'+projectName, projectName);
    Elapsed := 0;
    while ((Elapsed < dTime) and (not p_AgentList.Update(Elapsed,SignalTime))) do
        begin
        Inc(Elapsed,SignalTime);
        end;

    if Result then
        begin
        p_Scene.ExportXML(path+'\'+projectName, projectName,Elapsed);
        p_Bahavior.ExportXml(path+'\'+projectName, projectName,Elapsed);
        end;

    if Assigned(OnExportNavMesh) then
        begin
        p_NavMesh.Clear;
        OnExportNavMesh(p_NavMesh);
        p_NavMesh.SaveToFile(path + '\' + projectName + '\' + 'graph.txt');
        end;

    if Assigned(OnExportVpsJson) then
        OnExportVpsJson(path + '\' + projectName);
    end;


//==============================================================================
function MengeXmlClass.ExportXmls(path,projectName : string; SignalTime,Elapsed : Integer) : Boolean;
    {$ifdef Debug}
    var
        xml : TStringList;
    {$endif}
    begin
    if Elapsed <>0 then
        p_AgentList.UpdateStatesOnMenge;
    Result := p_AgentList.Update(Elapsed,SignalTime);

    {$ifdef Debug}
    Inc(p_SimInc);
    xml := TStringList.Create;
    try
        xml.Add('<Project ' +
        'scene="'+projectName+'/'+projectName+IntToStr(p_SimInc)+'S.xml" ' +
        'behavior="'+projectName+'/'+projectName+IntToStr(p_SimInc)+'B.xml" ' +
        'view="'+projectName+'/'+projectName+'V.xml" ' +
        'model="orca" dumpPath="images/test" ' +
        'duration="'+IntToStr(signalTime)+'"'+
        '/>'
        );
        xml.SaveToFile(path + '\' + projectName + '.xml');
    finally
        xml.Clear;
        end;
    {$endif}

    if Result then
        begin
        {$ifdef Debug}
        p_Scene.ExportXML(path+'\'+projectName, projectName+IntToStr(p_SimInc),Elapsed);
        p_Bahavior.ExportXml(path+'\'+projectName, projectName+IntToStr(p_SimInc),Elapsed);
        {$endif}

        {$ifdef Release}
        p_Scene.ExportXML(path+'\'+projectName, projectName,Elapsed);
        p_Bahavior.ExportXml(path+'\'+projectName, projectName,Elapsed);
        {$endif}
        end;
    end;

//==============================================================================
procedure MengeXmlClass.ResetParams;
    begin
    p_AgentList.Clear;
    p_Scene.Clear;
    p_Bahavior.Clear;
    end;

//==============================================================================
function MengeXmlClass.SummarizeMovingFeature(startTime,EndTime : TDatetime) : Boolean;
    var
        i,j,f,t : integer;
        mag : integer;
        setting : TAgentSettingsType;
        map : TPedestrianMap;
        values : TLoadedBlesensorValues;
        lt, rb : TPoint3D;
        spos :GLPointType;
        flow : integer;
    begin
    setting := LoadAgentSettings;
    map := p_Maps.Map;
    p_AgentList.CreateAgents(startTime,EndTime,100,setting);

    mag := 1;
    if Setting.IsUseMagnificationFactor then
        begin
        case MangnificationType(Setting.UseMagnificationType) of
            Population:
                begin
                mag := Trunc(Setting.PopulationValue/Setting.UUValue)-1;
                end;
            RealUU:
                begin
                mag := Setting.RealMagnification-1;
                end;
            CrossTraffic:
                begin
                flow := 0;
                if Assigned(sensors) and (sensors.numberOfTrafficSensorData>0) then
                    begin
                    map.ExportMapAreaWithMargin(lt, rb);
                    for i := 0 to sensors.numberOfTrafficSensorData-1 do
                        begin
                        values := sensors.TrafficSensorData[i].BleSensorData;
                        theApplicationservices.project.ConvertLatLongToPosition(values.latitude,values.longitude,spos);
                        if InRange(spos[_x],lt.x,rb.x) and InRange(spos[_z],lt.z,rb.z) then
                            begin
                            for f := 0 to values.dateObservedFrom.Count-1 do
                                begin
                                if startTime <values.dateObservedFrom[f].attrValue then
                                    begin
                                    break;
                                    end;
                                end;
                            for t := 0 to values.dateObservedTo.Count do
                                begin
                                if EndTime<values.dateObservedTo[t].attrValue then
                                    begin
                                    break;
                                    end;
                                end;
                            if (f<t) and InRange(f,0, values.dateObservedFrom.Count-1) and InRange(t,0,values.dateObservedTo.Count-1) then
                                begin
                                for j := f to t-1 do
                                    begin
                                    flow := flow + values.peopleCountFar[j].attrValue + values.peopleCountNear[j].attrValue;
                                    end;
                                end;
                            end;
                        end;
                    mag := Trunc(flow/p_AgentList.NumberOfAgent);
                    end;
                end;
            end;
        end;
    p_AgentList.CreateMagnificateFeature(mag);

    for i := 0 to p_AgentList.NumberOfAgent-1 do
        begin
        Assert(Assigned(p_AgentList.Agent[i]),'NoAgent');
        p_AgentList.Agent[i].SummarizeBehavior(startTime,EndTime,setting,map);
        end;
    Result := (p_AgentList.NumberOfAgent>0);
    end;
end.



