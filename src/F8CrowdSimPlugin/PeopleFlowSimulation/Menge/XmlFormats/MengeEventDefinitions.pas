unit MengeEventDefinitions;

interface

uses
    System.Generics.Collections,
    System.Classes,
    XML.XMLDom,
    XML.XMlIntf,
    XML.XMLDoc,
    F8GLUtils,
    xmlExportHelper,
    MengeStateDefinitions;

type
     TTrigger = class(xmlNodeClass)
            at_name : string;
            at_type : string;
        protected
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        end;

    TResponse = class(xmlNodeClass)
            at_effect : string;
            at_target : string;
        protected
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        end;

    TEvent =  class(xmlNodeClass)
            at_name : string;
            Trigger : TTrigger;
            Response : TResponse;
         protected
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        end;

    TTarget =  class(xmlNodeClass)
            at_name : string;
            at_type : string;//"agent_id"
            at_id : integer;
         protected
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        end;

    TEffect = class(xmlNodeClass)
            at_name : string;
            at_type : string;//"set_agent_state"
        protected
            function  GetNodeTag: string; override;
        end;

    TSetAgentStateEffect = class(TEffect)
            StateSelector : TStateSelector;
        protected
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        end;

    TExternalEffect = class(TEffect)
            StateSelector : TStateSelector;
        protected
            procedure DoExportAttributes(selfNode : IXMLNode); override;
        end;

    TEventSystem = class(xmlNodeClass)
            at_conservative : integer;
            Target : TTarget;
            Effect : array of TEffect;
            Event : array of TEvent;
        protected
            function  GetNodeTag: string; override;
            procedure DoExportAttributes(selfNode : IXMLNode); override;
    end;

implementation

{ TTrigger }

//==============================================================================
function TTrigger.GetNodeTag: string;
    begin
    Result :='Trigger';
    end;

//==============================================================================
procedure TTrigger.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('name', at_name);
    selfNode.SetAttribute('type', at_type);
    end;

{ TResponse }
//==============================================================================
function TResponse.GetNodeTag: string;
    begin
    Result := 'Response';
    end;

//==============================================================================
procedure TResponse.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('effect', at_effect);
    selfNode.SetAttribute('target', at_target);
    end;

{ TEvent }
//==============================================================================
function TEvent.GetNodeTag: string;
    begin
    Result := 'Event';
    end;

//==============================================================================
procedure TEvent.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('name', at_name);
    end;


{ TTarget }
//==============================================================================
function TTarget.GetNodeTag: string;
    begin
    Result := 'Target';
    end;

//==============================================================================
procedure TTarget.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('name', at_name);
    selfNode.SetAttribute('type', at_type);
    selfNode.SetAttribute('id',   at_id);
    end;


{ TEffect }
//==============================================================================
function TEffect.GetNodeTag: string;
    begin
    Result := 'Effect';
    end;

{ TSetAgentStateEffect }
//==============================================================================
procedure TSetAgentStateEffect.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('name', at_name);
    selfNode.SetAttribute('type', at_type);
    end;

{ TExternalEffect }
//==============================================================================
procedure TExternalEffect.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;
    selfNode.SetAttribute('name', at_name);
    selfNode.SetAttribute('type', at_type);
    end;

{ TEventSystem }
//==============================================================================
function TEventSystem.GetNodeTag: string;
    begin
    Result := 'EventSystem';
    end;

//==============================================================================
procedure TEventSystem.DoExportAttributes(selfNode: IXMLNode);
    begin
    inherited;

    end;



end.
