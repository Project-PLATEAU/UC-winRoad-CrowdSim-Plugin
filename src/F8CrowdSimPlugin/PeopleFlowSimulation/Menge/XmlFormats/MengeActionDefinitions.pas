unit MengeActionDefinitions;

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
{Actions----}
    TAction = class(xmlNodeClass)
            at_type : string;//"teleport","scale_property","set_property","set_obstacle","offset_property",
            dist : string;//"c";
            at_x : double;
            at_y : double;
        protected
            function  GetNodeTag: string; override;
    end;

    TTeleportAction = class(TAction)
            at_dist : string;//"u"
            at_min  : GLPointType2;
            at_max  : GLPointType2;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;

    TSetObstacleAction = class(TAction)
            at_operand : integer;//2 ObstacleSet の　Classに対応？？？ 1と2のみ？
            at_ExitReset : Integer;//0 State変更時にリセット(y=1,n=0)
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;

    TScalePropertyAction = class(TAction)
            at_property : string;//="r"
            at_dist : string;//="c"
            at_value : integer;//="5"
            at_exit_reset : integer;//="1"/
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;

    TOffsetPropertyAction = class(TAction)
            at_property : string;//"priority"
            at_dist : string;//"c"
            at_value : double;
            at_exit_reset : integer;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;

    TSetPropertyAction = class(TAction)
            at_property : string;//"priority","pref_speed"
            at_dist : string;//"c"
            at_value : double;
            at_exit_reset : integer;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;

    {----end Actions}

implementation

{ TAction }
//==============================================================================
function TAction.GetNodeTag: string;
    begin
    Result := 'Action';
    end;

{ TTeleportAction }
//==============================================================================
procedure TTeleportAction.DoInitialize;
    begin
    inherited;
    at_type := 'teleport';
    at_min := AsGLPointType2(0,0);
    at_max := AsGLPointType2(0,0);
    end;


//==============================================================================
procedure TTeleportAction.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type','teleport');
    selfNode.SetAttribute('dist',at_dist);
    selfNode.SetAttribute('min_x',at_min[_x]);
    selfNode.SetAttribute('max_x',at_max[_x]);
    selfNode.SetAttribute('min_y',at_min[_y]);
    selfNode.SetAttribute('max_y',at_max[_y]);
    end;

{ TSetObstacleAction }
//==============================================================================
procedure TSetObstacleAction.DoInitialize;
    begin
    inherited;
    at_type := 'set_obstacle';
    end;

//==============================================================================
procedure TSetObstacleAction.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',      'set_obstacle');
    selfNode.SetAttribute('operand',    at_operand);
    selfNode.SetAttribute('exit_reset', at_exitReset);
    end;

{ TScalePropertyAction }
//==============================================================================
procedure TScalePropertyAction.DoInitialize;
    begin
    inherited;
    at_type := 'scale_property';
    end;

//==============================================================================
procedure TScalePropertyAction.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',      'scale_property');

    end;

{ TOffsetProperty }
//==============================================================================
procedure TOffsetPropertyAction.DoInitialize;
    begin
    inherited;
    at_type := 'priority';
    end;

//==============================================================================
procedure TOffsetPropertyAction.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',     'priority');
    end;

{ TSetProperty }
//==============================================================================
procedure TSetPropertyAction.DoInitialize;
    begin
    inherited;
    at_type := 'set_property';
    end;

//==============================================================================
procedure TSetPropertyAction.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',     'set_property');
    selfNode.SetAttribute('property',at_property);
    selfNode.SetAttribute('dist', at_dist);
    selfNode.SetAttribute('value',  at_value);
    selfNode.SetAttribute('exit_reset',at_exit_reset);
    end;
end.
