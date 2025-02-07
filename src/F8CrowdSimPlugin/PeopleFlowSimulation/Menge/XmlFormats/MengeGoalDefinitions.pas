unit MengeGoalDefinitions;

interface
uses
    System.Generics.Collections,
    System.Classes,
    XML.XMLDom,
    XML.XMlIntf,
    XML.XMLDoc,
    F8GLUtils,
    xmlExportHelper;

type
    {goal---}
    TGoal = class(xmlNodeClass)
            at_type : string;//"AABB";
            at_id : integer;//"0";
        protected
            function  GetNodeTag: string; override;
    end;


    TAABBGoal = class(TGoal)
            at_min_x : integer;//"-5";
            at_max_x : integer;//"-4";
            at_min_y : integer;//"-5.5";
            at_max_y : integer;//"-5";
            at_capacity : integer;//"1"

        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;

    TpointGoal = class(TGoal)
            at_pos : GLpointType2;
            at_capacity : integer;
            at_weight : double;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;
    {---end goal}

    TGoalSet = class(xmlNodeClass)
            at_id : integer;
            goals : TList<TGoal>;
        protected
            function  GetNodeTag: string; override;
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        end;

    {GoalSelector---}
    TGoalSelector = class(xmlNodeClass)
            at_type  : string;
        protected
            function  GetNodeTag: string; override;
        public
            function Clone : TGoalSelector; virtual; abstract;
        end;

    TMirrorGoalSelector = class(TGoalSelector)
            at_mirror_x : integer;
            at_mirror_y : integer;
            at_per_agent : integer;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        public
            function Clone : TGoalSelector; override;
        end;

    TExplicitGoalSelector = class(TGoalSelector)
            at_goalSet : integer;
            at_goal : integer;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        public
            function Clone : TGoalSelector; override;
    end;

    TIdentityGoalSelector = class(TGoalSelector)
        //No Attribute
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        public
            function Clone : TGoalSelector; override;
        end;

    TNearestGoalSelector = class(TGoalSelector)
            at_goalSet : integer;
            at_perAgent : integer;
            at_persistent : integer;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        public
            function Clone : TGoalSelector; override;
        end;
    {---end GoalSelector}

implementation


{ TGoal }
//==============================================================================
function TGoal.GetNodeTag: string;
    begin
    Result := 'Goal';
    end;

{ TAABBGoal }
//==============================================================================
procedure TAABBGoal.DoInitialize;
    begin
    inherited;
    at_type := 'AABB';
    end;

//==============================================================================
procedure TAABBGoal.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',at_type);
    end;

{ TpointGoal }
//==============================================================================
procedure TpointGoal.DoInitialize;
    begin
    inherited;
    at_type := 'point';
    at_capacity := 10;
    at_weight := 1.0;
    end;

//==============================================================================
procedure TpointGoal.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('capacity',at_capacity);
    selfNode.SetAttribute('id',      at_id);
    selfNode.SetAttribute('type',    at_type);
    selfNode.SetAttribute('weight',  at_weight);
    selfNode.SetAttribute('x',       at_Pos[_x]);
    selfNode.SetAttribute('y',       at_Pos[_y]);
    end;

{ TGoalSet }
//==============================================================================
procedure TGoalSet.DoInitialize;
    begin
    inherited;
    at_id := 0;
    goals := TList<TGoal>.Create;
    end;

//==============================================================================
function TGoalSet.GetNodeTag: string;
    begin
    Result := 'GoalSet';
    end;

//==============================================================================
procedure TGoalSet.DoExportAttributes(selfNode: IXMLNode);
    var
        goal :  TGoal;
    begin
    inherited;
    selfNode.SetAttribute('id', at_id);
    for goal in goals do
        begin
        goal.ExportNode(selfNode);
        end;
    end;

{ TGoalSelector }
//==============================================================================
function TGoalSelector.GetNodeTag: string;
    begin
    Result := 'GoalSelector';
    end;

{ TMirrorGoalSelector }
//==============================================================================
procedure TMirrorGoalSelector.DoInitialize;
    begin
    inherited;
    at_type := 'mirror';
    end;

//==============================================================================
function TMirrorGoalSelector.Clone: TGoalSelector;
    begin
    Result := TMirrorGoalSelector.Create;
    TMirrorGoalSelector(Result).at_mirror_x := self.at_mirror_x;
    TMirrorGoalSelector(Result).at_mirror_y := self.at_mirror_y;
    TMirrorGoalSelector(Result).at_per_agent := self.at_per_agent;
    end;

//==============================================================================
procedure TMirrorGoalSelector.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type','mirror');
    end;

{ TExplicitGoalSelector }
//==============================================================================
procedure TExplicitGoalSelector.DoInitialize;
    begin
    inherited;
    at_type := 'explicit';
    at_goalSet := 0;
    at_goal := 0;
    end;

//==============================================================================
function TExplicitGoalSelector.Clone: TGoalSelector;
    begin
    Result := TExplicitGoalSelector.Create;
    TExplicitGoalSelector(Result).at_goalSet := self.at_goalSet;
    TExplicitGoalSelector(Result).at_goal := self.at_goal;
    end;

//==============================================================================
procedure TExplicitGoalSelector.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',    at_type);
    selfNode.SetAttribute('goal_set',at_goalSet);
    selfNode.SetAttribute('goal',    at_goal);
    end;

{ TIdentityGoalSelector }
//==============================================================================
procedure TIdentityGoalSelector.DoInitialize;
    begin
    inherited;
    at_type := 'identity';
    end;

//==============================================================================
function TIdentityGoalSelector.Clone: TGoalSelector;
    begin
    Result := TIdentityGoalSelector.Create;
    end;

//==============================================================================
procedure TIdentityGoalSelector.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',at_type);
    end;

{ TNearestGoalSelector }
//==============================================================================
procedure TNearestGoalSelector.DoInitialize;
    begin
    inherited;
    at_type := 'nearest';
    end;

//==============================================================================
function TNearestGoalSelector.Clone: TGoalSelector;
    begin
    Result := TNearestGoalSelector.Create;
    TNearestGoalSelector(Result).at_goalSet := Self.at_goalSet;
    TNearestGoalSelector(Result).at_perAgent := Self.at_perAgent;
    TNearestGoalSelector(Result).at_persistent := Self.at_persistent;
    end;

//==============================================================================
procedure TNearestGoalSelector.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',     at_type);
    selfNode.SetAttribute('goal_set', at_goalSet);
    selfNode.SetAttribute('per_agent',at_perAgent);
    end;

end.

