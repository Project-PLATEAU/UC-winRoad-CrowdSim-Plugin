unit MengeGeneratorDefinitions;

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
    TYPE_GENERATOR_EXPLICIT = 'explicit';

type
    /// <remarks>
    ///    Mengeで設定可能なパラメータはat_type以外にもあるが現在は使用していない
    ///    パラメータには次のようなものがある
    ///    - at_displaceStddev : double;
    ///    - at_alignment : integer;
    ///    - at_rowDirection : integer;//="y"
    ///    - at_population : integer;
    ///    - at_rotation : double;
    /// </remarks>
    TGenerator = class(xmlNodeClass)
            at_type : string;//"explicit" 、"rect_grid"、"hex_lattice"
        protected
            function  GetNodeTag: string; override;
        public
    end;


    TRectGridGenerator =class(TGenerator)//"rect_grid"
            at_anchor : glPointType2;
            at_offset : glPointType2;
            at_count  : glPointType2;
            at_displaceDist : string;//="u"
            at_displaceMin : double;
            at_displaceMax : double;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        public
    end;

    TExplicitGenerator =class(TGenerator)//"rect_grid"
            at_AgentArray : Array of GLPointType2;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        public
    end;

    TNavMeshExplicitGenerator = class(TGenerator)//"nav_mesh_explicit"
            at_FileName : string;
            at_GroupName : string;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        public
    end;

implementation

{ TGenerator }
//==============================================================================
function TGenerator.GetNodetag: string;
    begin
    Result := 'Generator';
    end;

{ TRectGridGenerator }
//==============================================================================
procedure TRectGridGenerator.DoInitialize;
    begin
    at_type := 'rect_grid';
    end;

//==============================================================================
procedure TRectGridGenerator.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',at_type);
    selfNode.SetAttribute('anchor_x',at_anchor[_x]);
    selfNode.SetAttribute('anchor_y',at_anchor[_y]);
    selfNode.SetAttribute('offset_x',at_offset[_x]);
    selfNode.SetAttribute('offset_y',at_offset[_y]);
    selfNode.SetAttribute('count_x',at_count[_x]);
    selfNode.SetAttribute('count_y',at_count[_y]);
    selfNode.SetAttribute('displace_dist',at_displaceDist);
    selfNode.SetAttribute('displace_min',at_displaceMin);
    selfNode.SetAttribute('displace_max',at_displaceMax);
    end;

{ TExplicitGenerator }
//==============================================================================
procedure TExplicitGenerator.DoInitialize;
    begin
    at_type := 'explicit';

    end;

//==============================================================================
procedure TExplicitGenerator.DoExportAttributes(selfNode: IXMLNode);
    var
        agent : GLPointType2;
        agentNode : IXMLNode;
    begin
    inherited;
    selfNode.SetAttribute('type',at_type);
    for agent in at_AgentArray  do
        begin
        agentNode := selfNode.AddChild('Agent');
        agentNode.SetAttribute('p_x',agent[_x]);
        agentNode.SetAttribute('p_y',agent[_y]);
        end;
    end;

{ TNavMeshExplicitGenerator }
//==============================================================================
procedure TNavMeshExplicitGenerator.DoInitialize;
    begin
    {no action}
    end;

//==============================================================================
procedure TNavMeshExplicitGenerator.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',at_type);
    end;
end.
