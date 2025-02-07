unit MengeBehaviorXML;

interface

uses
    System.Generics.Collections,
    XML.XMLDom,
    XML.XMlIntf,
    XML.XMLDoc,
    PluginCore,
    F8GLUtils,
    XmlExporter,
    MengeAgent,
    AgentSettings,
    MovingFeature,
    MengeAgentDefinitions,
    MengeGoalDefinitions,
    MengeEventDefinitions,
    MengeStateDefinitions,
    PedestrianMap;

type
    /// <summary>
    ///    Mengexmlファイルのうち、B.xmlファイルの出力処理を行うクラス
    /// </summary>
    MengeBehaviorXmlClass = class(xmlExporterClass)
        private
            p_XmlDocment : IXMLDocument;
            p_NodeBfsm : IXMLNode;
            p_AgentList : MengeAgentList;
            p_Application : IF8ApplicationServices;
        public
            constructor Create( Value: MengeAgentList);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure Clear;
            procedure ExportXML(path, projectName: string; Elapsed: Integer);// overload;
    end;

implementation

uses
    Winapi.Windows,
    System.SysUtils,
    System.DateUtils,
    MengeConditionDefinitions,
    F8Utils,
    MengeVelComponentDefinitions,
    MengeActionDefinitions,
    PedestrianUtil,
    CellID,
    PedestrianCell;

{ TMengeBehaviorXxmlClass }
//==============================================================================
constructor MengeBehaviorXmlClass.Create(Value: MengeAgentList);
    begin
    p_AgentList := Value;
    end;

//==============================================================================
procedure MengeBehaviorXmlClass.AfterConstruction;
    begin
    inherited;
    Supports(ApplicationServices, IF8ApplicationServices, p_Application);

    p_xmlDocment := NewXMLDocument();
    p_NodeBfsm := p_xmlDocment.AddChild('BFSM');

    end;

//==============================================================================
procedure MengeBehaviorXmlClass.BeforeDestruction;
    begin
    inherited;
    p_Application := nil;
    end;

//==============================================================================
procedure MengeBehaviorXmlClass.Clear;
    begin

    end;

//==============================================================================
procedure MengeBehaviorXmlClass.ExportXML(path, projectName: string; Elapsed: Integer);
    var
        I: Integer;
        agent : MengeAgentClass;
    begin
    p_NodeBfsm.ChildNodes.Clear;
    DoExportNodes(p_NodeBfsm,Elapsed);

    for I := 0 to p_AgentList.NumberOfAgent-1 do
        begin
        agent := p_AgentList.agent[i];
        if agent.IsExport then
            begin
            agent.ExportGoals(p_NodeBfsm);
            end;
        end;

    for I := 0 to p_AgentList.NumberOfAgent-1 do
        begin
        agent := p_AgentList.agent[i];
        if agent.IsExport then
            begin
            agent.ExportStates(p_NodeBfsm);
            end;
        end;

    for I := 0 to p_AgentList.NumberOfAgent-1 do
        begin
        agent := p_AgentList.agent[i];
        if agent.IsExport then
            begin
            agent.ExportTransitions(p_NodeBfsm);
            end;
        end;


    p_xmlDocment.SaveToFile(path +'\'+projectName+'B.xml');
    end;



end.
