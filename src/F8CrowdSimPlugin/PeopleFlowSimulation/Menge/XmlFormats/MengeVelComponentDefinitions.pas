unit MengeVelComponentDefinitions;

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
     {VelComponent---}
    TVelComponent =  class(xmlNodeClass)
//            at_type : string;//zero
        protected
            function  GetNodeTag: string; override;
            function  GetAtType: String; virtual; abstract;
        public

            function  Clone : TVelComponent; virtual; abstract;
        end;

    TZeroVelComponent = class(TVelComponent)
        //NpAttribute
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
            function  GetAtType: String; override;
        public
            function  Clone : TVelComponent; override;
        end;

    TGoalVelComponent = class(TVelComponent)
            at_weight : double;
        protected
            procedure DoInitialize;  override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
            function  GetAtType: String; override;
        public
            function  Clone : TVelComponent; override;
        end;

    TRoadMapVelComponent = class(TVelComponent)
            at_filename : string;
        protected
            procedure DoInitialize;  override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
            function  GetAtType: String; override;
        public
            function  Clone : TVelComponent; override;
        end;

    TNavMeshVelComponent = class(TVelComponent)
            at_filename : string;
            at_headingThreshold : integer;
        protected
            procedure DoInitialize;  override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
            function  GetAtType: String; override;
        public
            function  Clone : TVelComponent; override;
        end;

    TVelFieldVelComponent = class(TVelComponent)
            at_filename : string;
            at_useNearest : integer;
        protected
            procedure DoInitialize; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
            function  GetAtType: String; override;
        public
            function  Clone : TVelComponent; override;
        end;
    {---end velComponent}


implementation

{ TVelComponent }
//==============================================================================
function TVelComponent.GetNodeTag: string;
    begin
    Result := 'VelComponent';
    end;

{ TZeroVelComponent }
//==============================================================================
procedure TZeroVelComponent.DoInitialize;
    begin

    end;

//==============================================================================
procedure TZeroVelComponent.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type', GetAtType);
    end;

function TZeroVelComponent.GetAtType: String;
    begin
    Result := 'zero';
    end;

//==============================================================================
function TZeroVelComponent.Clone: TVelComponent;
    begin
    Result := TZeroVelComponent.Create;
    end;

{ TGoalVelComponent }
//==============================================================================
procedure TGoalVelComponent.DoInitialize;
    begin
    at_weight := 1.0;
    end;

//==============================================================================
procedure TGoalVelComponent.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',GetAtType);
    selfNode.SetAttribute('weight',at_weight);
    end;

function TGoalVelComponent.GetAtType: String;
    begin
    Result := 'goal';
    end;

//==============================================================================
function TGoalVelComponent.Clone: TVelComponent;
    begin
    Result := TGoalVelComponent.Create;
    TGoalVelComponent(Result).at_weight := self.at_weight;
    end;

{ TRoadMapVelComponent }
//==============================================================================
procedure TRoadMapVelComponent.DoInitialize;
    begin
    at_filename := 'graph.txt';
    end;

//==============================================================================
procedure TRoadMapVelComponent.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',GetAtType);
    selfNode.SetAttribute('file_name','graph.txt');
    end;

function TRoadMapVelComponent.GetAtType: String;
    begin
    Result := 'road_map';
    end;

//==============================================================================
function TRoadMapVelComponent.Clone: TVelComponent;
    begin
    Result := TRoadMapVelComponent.Create;
    TRoadMapVelComponent(Result).at_filename := self.at_filename;
    end;

{ TNavMeshVelComponent }
//==============================================================================
procedure TNavMeshVelComponent.DoInitialize;
    begin
    at_headingThreshold := 15;
    at_filename := 'mesh.nav'
    end;

//==============================================================================
procedure TNavMeshVelComponent.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',GetAtType);
    selfNode.SetAttribute('heading_threshold',at_headingThreshold);
    selfNode.SetAttribute('file_name', at_filename);
    end;

function TNavMeshVelComponent.GetAtType: String;
    begin
    Result := 'nav_mesh';
    end;

//==============================================================================
function TNavMeshVelComponent.Clone: TVelComponent;
    begin
    Result := TNavMeshVelComponent.Create;
    TNavMeshVelComponent(Result).at_headingThreshold := self.at_headingThreshold;
    TNavMeshVelComponent(Result).at_filename := self.at_filename;
    end;

{ TVelFieldVelComponent }
//==============================================================================
procedure TVelFieldVelComponent.DoInitialize;
    begin
    at_filename := 'fieldM.txt';
    at_useNearest := 1;
    end;

//==============================================================================
procedure TVelFieldVelComponent.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('type',       GetAtType);
    selfNode.SetAttribute('use_nearest',at_useNearest);
    selfNode.SetAttribute('file_name',  at_filename);
    end;

function TVelFieldVelComponent.GetAtType: String;
    begin
    Result := 'vel_field';
    end;

//==============================================================================
function TVelFieldVelComponent.Clone: TVelComponent;
    begin
    Result := TVelFieldVelComponent.Create;
    TVelFieldVelComponent(Result).at_filename := self.at_filename;
    TVelFieldVelComponent(Result).at_useNearest := self.at_useNearest;
    end;
end.
