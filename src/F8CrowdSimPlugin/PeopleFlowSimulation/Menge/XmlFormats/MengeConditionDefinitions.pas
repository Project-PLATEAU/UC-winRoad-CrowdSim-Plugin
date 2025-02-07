unit MengeConditionDefinitions;

interface

uses
    System.Generics.Collections,
    System.Classes,
    XML.XMLDom,
    XML.XMlIntf,
    XML.XMLDoc,
    F8GLUtils,
    xmlExportHelper;

const
    TAG_CONDITION = 'Condition';
    TYPE_CONDITION_AUTO  = 'auto';
    TYPE_CONDITION_TIMER = 'timer';
    TYPE_CONDITION_GOAL  = 'goal_reached';
    TYPE_CONDITION_AABB  = 'AABB';
    TYPE_CONDITION_AND   = 'and';

type
    {Conditions----}
    TCondition = class(xmlNodeClass)
            at_type : string;//"timer","goal_reached","AA_BB","auto","and"
        protected
            function  GetNodeTag: string; override;
    end;

    TAutoCondition = class(TCondition)
        //no Attribute
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;

    TGoalReachedCondition = class(TCondition)
            at_distance : double;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;

    TAABBCondition = class(TCondition)
            at_inside : integer;
            at_min : GLPointType2;
            at_max : GLPointType2;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;

    TTimerCondition = class(TCondition)
            at_dist : string;//u=minxmax? c=notReached valueTime?
            at_min : integer;//cの場合不要
            at_max : integer;//cの場合不要
            at_value : integer;//uの場合不要
            at_perAgent : integer;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;

    TAndCondition = class(TCondition)
            Conditions : array of TCondition;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;
    TNotCondition = class(TCondition)
            Conditon : TCondition;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;
    {----end Conditions}

implementation

{ TCondition }
//==============================================================================
function TCondition.GetNodeTag: string;
    begin
    Result := TAG_CONDITION;
    end;

{ TAutoCondition }
//==============================================================================
procedure TAutoCondition.DoInitialize;
    begin
    inherited;
    at_type := TYPE_CONDITION_AUTO;
    end;

//==============================================================================
procedure TAutoCondition.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;

    end;

{ TGoalReachedCondition }
//==============================================================================
procedure TGoalReachedCondition.DoInitialize;
    begin
    inherited;
    at_type := TYPE_CONDITION_GOAL;
    at_distance := 0.05;
    end;

//==============================================================================
procedure TGoalReachedCondition.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',    at_type);
    selfNode.SetAttribute('distance',at_distance);
    end;

{ TAABBCondition }
//==============================================================================
procedure TAABBCondition.DoInitialize;
    begin
    inherited;
    at_type := TYPE_CONDITION_AABB;
    end;

//==============================================================================
procedure TAABBCondition.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',   at_type);
    selfNode.SetAttribute('inside', at_inside);
    selfNode.SetAttribute('min_x',  at_min[_x]);
    selfNode.SetAttribute('max_x',  at_max[_x]);
    selfNode.SetAttribute('min_y',  at_min[_y]);
    selfNode.SetAttribute('max_y',  at_max[_y]);
    end;

{ TTimerCondition }
//==============================================================================
procedure TTimerCondition.DoInitialize;
    begin
    inherited;
    at_type := TYPE_CONDITION_TIMER;
    at_perAgent := 0;
    end;

//==============================================================================
procedure TTimerCondition.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',    at_type);
    if at_dist ='u' then
        begin
        selfNode.SetAttribute('dist',     at_dist);
        selfNode.SetAttribute('min',      at_min);
        selfNode.SetAttribute('max',      at_max);
        end
    else if at_dist='c' then
        begin
        selfNode.SetAttribute('dist',     at_dist);
        selfNode.SetAttribute('value',    at_value);
        end;
    selfNode.SetAttribute('per_agent',at_perAgent);
    end;

{ TAndCondition }
//==============================================================================
procedure TAndCondition.DoInitialize;
    begin
    inherited;
    at_type := 'and';
    end;

//==============================================================================
procedure TAndCondition.DoExportAttributes(selfNode: IXMLNode);
    var
        child : TCondition;
    begin
    inherited;
    selfNode.SetAttribute('type',    at_type);
    for child in Conditions do
        begin
        child.ExportNode(selfNode);
        end;
    end;

{ TNotCondition }
//==============================================================================
procedure TNotCondition.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',    at_type);
    Conditon.ExportNode(selfNode);
    end;

//==============================================================================
procedure TNotCondition.DoInitialize;
    begin
    inherited;
    at_type := 'not';
    end;

end.
