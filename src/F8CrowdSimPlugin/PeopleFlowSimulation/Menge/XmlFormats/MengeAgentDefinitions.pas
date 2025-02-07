unit MengeAgentDefinitions;

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

//Scene=========================================================================
    TAgentProfileCommon = class(xmlNodeClass)
            at_r : double;
            at_maxNeighbors : integer;//一度に衝突判定を計算する人数、0だと衝突しない
            at_neighborDist : double;
            at_class : integer;
            at_maxSpeed : double;
            at_maxAccel : double;
            at_prefSpeed : double;
            at_maxAngleVel : double;
            at_ObstacleSet : integer; //有効障害物
        protected
            procedure DoInitialize;  override;
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
            procedure DoExportInherited(selfNode : IXMLNode);
        public
            function Clone : TAgentProfileCommon;
        end;

    TAgentProfileOrca = class(xmlNodeClass)
            at_tau : double;//"3.0"
            at_tauObst : integer;//="15"
            at_personalSpace : integer;
            at_anticipation : integer;
        protected
            procedure DoInitialize; override;
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        public
            function Clone : TAgentProfileOrca;
        end;

    TAgentProfile = class(xmlNodeClass)
            at_name : string;
            at_inherits : string;
            Common : TAgentProfileCommon;
            Orca : TAgentProfileOrca;
        protected
            procedure DoInitialize; override;
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        public
            function Clone : TAgentProfile;
        end;

    TProfileSelector = class(xmlNodeClass)
            at_type : string;
            at_name : string;
        protected
            procedure DoInitialize; override;
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        public
            function  Clone : TProfileSelector;
        end;

    TAgentGroup = class(xmlNodeClass)
            ProfileSelector : TProfileSelector;
            StateSelector : TStateSelector;
            Generator   : TGenerator;

        protected
            procedure DoInitialize; override;
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        public
            function  Clone : TAgentGroup;
        end;


implementation


{ TAgentProfileOrca }

//==============================================================================
function TAgentProfileOrca.Clone: TAgentProfileOrca;
    begin
    Result := TAgentProfileOrca.Create;
    Result.at_tau := self.at_tau;
    Result.at_tauObst := self.at_tauObst;
    Result.at_personalSpace := self.at_personalSpace;
    Result.at_anticipation := self.at_anticipation;
    end;

//==============================================================================
procedure TAgentProfileOrca.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('tau',            at_tau);
    selfNode.SetAttribute('tauObst',        at_tauObst);
    selfNode.SetAttribute('personal_space', at_personalSpace);
    selfNode.SetAttribute('anticipation',   at_anticipation);
    end;

//==============================================================================
procedure TAgentProfileOrca.DoInitialize;
    begin
    inherited;
    at_tau := 3.0;
    at_tauObst := 15;
    at_personalSpace := 500;
    at_anticipation := 8;
    end;

//==============================================================================
function TAgentProfileOrca.GetNodeTag: string;
    begin
    Result := 'Orca';
    end;

{ TAgentProfileCommon }
//==============================================================================
function TAgentProfileCommon.GetNodeTag: string;
    begin
    Result := 'Common';
    end;

//==============================================================================
procedure TAgentProfileCommon.DoInitialize;
    begin
    at_r := 0.5;
    at_maxNeighbors := 10;
    at_neighborDist := 5;
    at_class := 1;
    at_maxSpeed := 1;
    at_maxAccel := 5;
    at_prefSpeed := 1.0;
    at_maxAngleVel := 360;
    at_ObstacleSet := 1;
    end;

//==============================================================================
function TAgentProfileCommon.Clone: TAgentProfileCommon;
    begin
    Result := TAgentProfileCommon.Create;
    Result.at_r := self.at_r;
    Result.at_maxNeighbors := self.at_maxNeighbors;
    Result.at_neighborDist := self.at_neighborDist;
    Result.at_class := self.at_class;
    Result.at_maxSpeed := self.at_maxSpeed;
    Result.at_maxAccel := self.at_maxAccel;
    Result.at_prefSpeed := self.at_prefSpeed;
    Result.at_maxAngleVel :=self.at_maxAngleVel;
    Result.at_ObstacleSet := self.at_ObstacleSet;
    end;


//==============================================================================
procedure TAgentProfileCommon.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('max_angle_vel', at_maxAngleVel);
    selfNode.SetAttribute('max_neighbors', at_maxNeighbors);
    selfNode.SetAttribute('obstacleSet',   at_ObstacleSet);
    selfNode.SetAttribute('neighbor_dist', at_neighborDist);
    selfNode.SetAttribute('r',             at_r);
    selfNode.SetAttribute('class',         at_class);
    selfNode.SetAttribute('pref_speed',    at_prefSpeed);
    selfNode.SetAttribute('max_speed',     at_maxSpeed);
    selfNode.SetAttribute('max_accel',     at_maxAccel);
    end;

//==============================================================================
procedure TAgentProfileCommon.DoExportInherited(selfNode: IXMLNode);
    begin
    selfNode.SetAttribute('class',         at_class);
    if at_maxSpeed <> 1 then
        selfNode.SetAttribute('max_speed',     at_maxSpeed);
    end;


{ TAgentProfile }
//==============================================================================
procedure TAgentProfile.DoInitialize;
    begin
    inherited;
    Common :=  TAgentProfileCommon.Create;
    Orca := TAgentProfileOrca.Create;
    at_inherits := '';
    at_name := 'profile';
    end;

//==============================================================================
function TAgentProfile.Clone: TAgentProfile;
    begin
    Result := TAgentProfile.Create;
    Result.at_name := self.at_name;
    Result.at_inherits := self.at_inherits;
    Result.Common := self.Common.Clone;
    Result.Orca := self.Orca.Clone;
    end;

//==============================================================================
procedure TAgentProfile.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('name', at_name);
    if Length(at_inherits)>0 then
        begin
        selfNode.SetAttribute('inherits', at_inherits);
        Common.DoExportInherited(selfNode.AddChild(Common.NodeTag));
        end
    else
        begin
        Common.ExportNode(selfNode);
        end;
    end;

//==============================================================================
function TAgentProfile.GetNodeTag: string;
    begin
    Result := 'AgentProfile';
    end;


{ TProfileSelecor }
//==============================================================================
procedure TProfileSelector.DoInitialize;
    begin
    inherited;
    at_type := 'const';
    at_name := 'group1';
    end;

//==============================================================================
function TProfileSelector.GetNodeTag: string;
    begin
    Result := 'ProfileSelector';
    end;

//==============================================================================
function TProfileSelector.Clone: TProfileSelector;
    begin
    Result := TProfileSelector.Create;
    Result.at_type := self.at_type;
    Result.at_name := self.at_name;
    end;

//==============================================================================
procedure TProfileSelector.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type', at_type);
    selfNode.SetAttribute('name', at_name);
    end;


{ TAgentGroup }
//==============================================================================
function TAgentGroup.GetNodeTag: string;
    begin
    Result := 'AgentGroup';
    end;

//==============================================================================
function TAgentGroup.Clone: TAgentGroup;
    begin
    Result := TAgentGroup.Create;
    ProfileSelector := self.ProfileSelector.Clone;
    StateSelector := self.StateSelector;
    Generator := self.Generator;
    end;

//==============================================================================
procedure TAgentGroup.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    if Assigned(ProfileSelector) then
        begin
        ProfileSelector.ExportNode(selfnode);
        end;

    if Assigned(StateSelector) then
        begin
        StateSelector.ExportNode(selfnode);
        end;

    if Assigned(Generator) then
        begin
        Generator.ExportNode(selfNode);
        end;
    end;

//==============================================================================
procedure TAgentGroup.DoInitialize;
    begin
    inherited;
    
    end;

end.
