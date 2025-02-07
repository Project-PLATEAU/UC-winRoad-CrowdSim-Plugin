unit MengeSceneXML;

interface

uses
    System.Generics.Collections,
    XML.XMLDom,
    XML.XMlIntf,
    XML.XMLDoc,
    PluginCore,
    F8GLUtils,
    AgentSettings,mengeAgent,
    PedestrianMap,
    XmlExporter,
    MengeBehaviorXML,
    MengeAgentDefinitions,
    MengeStateDefinitions,
    MengeExperimentDefinitions;


type
    /// <summary>
    ///    Mengexmlファイルのうち、S.xmlファイルの出力処理を行うクラス
    /// </summary>
    MengeSceneXMLClass = class(xmlExporterClass)
        private
            p_XmlDocment : IXMLDocument;
            p_NodeExperiment : IXMLNode;//Root

            p_SpatialQuery : TSpatialQuery;
            p_OpenSteer : TExperimentOpenSteer;
            p_NodeCommon : TExperimentCoommon;
            p_Orca : TExperimentOrca;

            p_AgentList : MengeAgentList;

            p_Application : IF8ApplicationServices;

        public
            constructor Create(Value: MengeAgentList);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure Clear;

            procedure ExportXML(path,projectName : string; Elapsed : Integer);
        end;




implementation

uses
    Winapi.Windows,
    System.SysUtils,
    F8Utils,
    PedestrianUtil,
    CellID,
    PedestrianCell,
    MengeGeneratorDefinitions,
    MovingFeature;

{ MengeSceneXMLClass }
//==============================================================================
constructor MengeSceneXMLClass.Create(Value: MengeAgentList);
    begin
    p_AgentList := value;
    end;

//==============================================================================
procedure MengeSceneXMLClass.AfterConstruction;
    begin
    inherited;
    Supports(ApplicationServices, IF8ApplicationServices, p_Application);

    p_xmlDocment := NewXMLDocument();
    p_NodeExperiment := p_xmlDocment.AddChild('Experiment');
    p_NodeExperiment.SetAttribute('version','2.0');


    p_SpatialQuery := TSpatialQuery.Create;
    p_OpenSteer := TExperimentOpenSteer.Create;
    p_NodeCommon := TExperimentCoommon.Create;
    p_Orca := TExperimentOrca.Create;
    end;

//==============================================================================
procedure MengeSceneXMLClass.BeforeDestruction;
    begin
    inherited;
    end;

//==============================================================================
procedure MengeSceneXMLClass.Clear;
    begin

    end;

//==============================================================================
procedure MengeSceneXMLClass.ExportXML(path, projectName: string; Elapsed: Integer);
    var
        i : Integer;
    begin
    p_NodeExperiment.ChildNodes.Clear;

    p_SpatialQuery.ExportNode(p_NodeExperiment);
    p_OpenSteer.ExportNode(p_NodeExperiment);
    p_NodeCommon.ExportNode(p_NodeExperiment);
    p_Orca.ExportNode(p_NodeExperiment);

    for I := 0 to p_AgentList.NumberOfAgent-1 do
        begin
        p_AgentList.Agent[I].Exportprofile(p_NodeExperiment);
        end;

    for I := 0 to p_AgentList.NumberOfAgent-1 do
        begin
        p_AgentList.Agent[I].ExportGroup(p_NodeExperiment);
        end;

    DoExportNodes(p_NodeExperiment,Elapsed);
    p_xmlDocment.SaveToFile(path +'\'+ projectName +'S.xml');
    end;

end.
