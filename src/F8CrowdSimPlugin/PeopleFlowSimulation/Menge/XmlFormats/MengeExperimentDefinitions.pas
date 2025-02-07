unit MengeExperimentDefinitions;

interface

uses
    System.Generics.Collections,
    System.Classes,
    XML.XMLDom,
    XML.XMlIntf,
    XML.XMLDoc,
    F8GLUtils,
    xmlExportHelper,
    MengeGeneratorDefinitions,
    MengeStateDefinitions;

type
    TSpatialQuery = class(xmlNodeClass)
            at_type : string;
            at_testVisibility : Boolean;
        protected
            procedure DoInitialize;  override;
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;

    TExperimentOpenSteer = class(xmlNodeClass)
            at_maxForce : integer;
            at_leakThrough : double;
            at_reactionTime : double;
        protected
            procedure DoInitialize;  override;
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;

    end;

     TExperimentCoommon = class(xmlNodeClass)
            at_timeStep : double;
        protected
            procedure DoInitialize;  override;
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;

     TExperimentOrca = class(xmlNodeClass)
            aT_AgentScale : integer;//="25"
            at_ObstacleScale : integer;//="35"
            at_reactionTime : double;//="0.5"
            at_forceDistance : double;//="0.15"
            at_strideTime : double;//="0.5" />
        protected
            procedure DoInitialize;  override;
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
     end;



implementation

{ TSpatialQuery }
//==============================================================================
procedure TSpatialQuery.DoInitialize;
    begin
    inherited;
    at_testVisibility := False;
    at_type := 'kd-tree';
    end;

//==============================================================================
procedure TSpatialQuery.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type', at_type);
    selfNode.SetAttribute('test_visibility',  at_testVisibility);
    end;

//==============================================================================
function TSpatialQuery.GetNodeTag: string;
    begin
    Result := 'SpatialQuery';
    end;

{ TExperimentOpenSteer }
//==============================================================================
procedure TExperimentOpenSteer.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('max_force', at_maxForce);
    selfNode.SetAttribute('leak_through', at_leakThrough);
    selfNode.SetAttribute('reaction_time', at_reactionTime);
    end;

//==============================================================================
procedure TExperimentOpenSteer.DoInitialize;
    begin
    inherited;
    at_maxForce := 8;
    at_leakThrough := 0.1;
    at_reactionTime := 0.1;
    end;

//==============================================================================
function TExperimentOpenSteer.GetNodeTag: string;
    begin
    Result := 'OpenSteer';
    end;


{ TExperimentCoommon }
//==============================================================================
procedure TExperimentCoommon.DoInitialize;
    begin
    at_timeStep := 0.1;
    end;

//==============================================================================
procedure TExperimentCoommon.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('time_step', at_timeStep);
    end;

//==============================================================================
function TExperimentCoommon.GetNodeTag: string;
    begin
    Result := 'Common';
    end;


{ TExperimentOrca }
//==============================================================================
procedure TExperimentOrca.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('agent_scale',    at_AgentScale);
    selfNode.SetAttribute('obstacle_scale', at_ObstacleScale);
    selfNode.SetAttribute('reaction_time',  at_reactionTime);
    selfNode.SetAttribute('force_distance', at_forceDistance);
    selfNode.SetAttribute('stride_time',    at_strideTime);
    end;

//==============================================================================
procedure TExperimentOrca.DoInitialize;
    begin
    inherited;
    aT_AgentScale := 25;
    at_ObstacleScale := 35;
    at_reactionTime :=0.5;
    at_forceDistance := 0.15;
    at_strideTime := 0.5;
    end;

//==============================================================================
function TExperimentOrca.GetNodeTag: string;
    begin
    Result := 'ORCA';
    end;

end.

