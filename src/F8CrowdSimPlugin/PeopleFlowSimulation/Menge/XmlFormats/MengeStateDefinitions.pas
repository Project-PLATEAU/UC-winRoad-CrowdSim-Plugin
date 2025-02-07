unit MengeStateDefinitions;

interface

uses
    System.Generics.Collections,
    System.Classes,
    XML.XMLDom,
    XML.XMlIntf,
    XML.XMLDoc,
    F8GLUtils,
    xmlExportHelper,
    MengeActionDefinitions,
    MengeConditionDefinitions,
    MengeGoalDefinitions,
    MengeVelComponentDefinitions;

const
    STATE_NAME_NONE     = 'none';
    STATE_NAME_WAIT     = 'wait_';
    STATE_NAME_WALK     = 'walk_';
    STATE_NAME_GOAL     = 'goal_';
    STATE_NAME_TELEPORT = 'telepo_';

type
    TState = class(xmlNodeClass)
            at_name : string;
            at_speedPolicy : string;//NotNeed
            at_final : integer;
            Actions : TList<TAction>;
            GoalSelector : TGoalSelector;
            VelComponent : TVelComponent;
        protected
            procedure DoInitialize; override;
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        public
            function  Clone : TState;
    end;


    TTransition = class(xmlNodeClass)
            at_from : array of TState;
            at_to : TState;
            Condition : TCondition;
        protected
            procedure DoInitialize; override;
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        end;

    TStateSelector = class(xmlNodeClass)
            at_type : string;
            at_name : string;
        protected
            procedure DoInitialize; override;
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        end;



implementation


{ TState }
//==============================================================================
procedure TState.DoInitialize;
    begin
    at_name := 'State';
    at_speedPolicy := '';
    at_final := 0;
    Actions := TList<TAction>.Create;
    end;

//==============================================================================
function TState.Clone: TState;
    begin
    Result := TState.Create;
    Result.at_name := self.at_name;
    Result.at_speedPolicy := self.at_speedPolicy;
    Result.at_final := self.at_final;
    Result.Actions := self.Actions;
    Result.GoalSelector := self.GoalSelector.Clone;
    Result.VelComponent := self.VelComponent.Clone;
    end;

//==============================================================================
procedure TState.DoExportAttributes(selfNode: IXMLNode);
    var
        action : TAction;
    begin
    inherited;
    selfNode.SetAttribute('name', at_name);
    selfNode.SetAttribute('final', at_final);

    if length(at_speedPolicy) >0 then
        begin
        selfNode.SetAttribute('speed_polocy', at_speedPolicy);
        end;

    if Assigned(VelComponent) then
        begin
        VelComponent.ExportNode(selfNode);
        end;

    if Assigned(GoalSelector) then
        begin
        GoalSelector.ExportNode(selfNode);
        end;

    for action in Actions do
        begin
        action.ExportNode(selfNode);
        end;
    end;

//==============================================================================
function TState.GetNodeTag: string;
    begin
    Result := 'State';
    end;

{ TTransition }
//==============================================================================
procedure TTransition.DoInitialize;
    begin
    //Do Nothing here
    end;

//==============================================================================
procedure TTransition.DoExportAttributes(selfNode: IXMLNode);
    var
        i : integer;
        from : string;
    begin
    inherited;
    from := '';
    for i:= 0 to length(at_from)-1 do
        begin
        if i=0 then
            from := at_from[i].at_name
        else
            from := from +',' + at_from[i].at_name;
        end;
    selfNode.SetAttribute('from', from);
    selfNode.SetAttribute('to', at_to.at_name);

    if Assigned(Condition) then
        begin
        Condition.ExportNode(selfNode);
        end;
    end;

//==============================================================================
function TTransition.GetNodeTag: string;
    begin
    Result := 'Transition';
    end;


{ TStateSelecor }
//==============================================================================
procedure TStateSelector.DoInitialize;
    begin
    inherited;
    at_type := 'const';
    at_name := 'State1';
    end;

//==============================================================================
function TStateSelector.GetNodeTag: string;
    begin
    Result := 'StateSelector';
    end;

//==============================================================================
procedure TStateSelector.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type', at_type);
    selfNode.SetAttribute('name', at_name);
    end;

end.
