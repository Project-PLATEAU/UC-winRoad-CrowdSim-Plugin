unit XmlExporter;

interface

uses
    System.Generics.Collections,
    System.SyncObjs,
    XML.XMLDom,
    XML.XMlIntf,
    XML.XMLDoc;

type
    OnExportNodeEvent = procedure(var rootNode : IXMLNode; Elapsed : Integer) of Object;

    /// <summary>
    ///    シミュレーション計算時のMengexmlファイル出力処理を管理するクラス
    ///    xmlファイル出力処理は独自スレッドで実行される
    /// </summary>
    xmlExporterClass = class
        private
            p_eventCS : TCriticalSection;

            p_NodeExportEventList : Tlist<OnExportNodeEvent>;

        protected
            procedure DoExportNodes(rootNode: IXMLNode; Elapsed : Integer);

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure RegisterExportNodeEvent(event : OnExportNodeEvent);
            procedure UnRegisterExportNodeEvent(event : OnExportNodeEvent);
    end;

implementation

uses
    System.SysUtils;

{ xmlExporterClass }
//==============================================================================
procedure xmlExporterClass.AfterConstruction;
    begin
    inherited;
    p_eventCS := TCriticalSection.Create;
    p_NodeExportEventList := Tlist<OnExportNodeEvent>.Create;
    end;

//==============================================================================
procedure xmlExporterClass.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_eventCS);
    FreeAndNil(p_NodeExportEventList);
    end;

//==============================================================================
procedure xmlExporterClass.DoExportNodes(rootNode: IXMLNode; Elapsed : Integer);
    var
        event : OnExportNodeEvent;
    begin
    p_eventCS.Enter;
    try
        for event in p_NodeExportEventList do
            begin
            event(rootNode,Elapsed);
            end;
    finally
        p_eventCS.Leave;
        end;
    end;


//==============================================================================
procedure xmlExporterClass.RegisterExportNodeEvent(event: OnExportNodeEvent);
    begin
    p_eventCS.Enter;
    try
        if not p_NodeExportEventList.Contains(event) then
            begin
            p_NodeExportEventList.Add(event);
            end;
    finally
        p_eventCS.Leave;
        end;
    end;

//==============================================================================
procedure xmlExporterClass.UnRegisterExportNodeEvent(event: OnExportNodeEvent);
    begin
    p_eventCS.Enter;
    try
        if not p_NodeExportEventList.Contains(event) then
            begin
            p_NodeExportEventList.Remove(event);
            end;
    finally
        p_eventCS.Leave;
        end;
    end;

end.
