unit xmlExportHelper;

interface

uses
    System.Generics.Collections,
    XML.XMLDom,
    XML.XMlIntf,
    XML.XMLDoc;

type
    /// <summary>
    ///    Mengexmlファイルのノード・属性出力処理を管理するクラス
    /// </summary>
    xmlNodeClass = class
        private

        protected
            procedure DoInitialize; virtual; abstract;
            procedure DoExportAttributes(selfNode : IXMLNode); virtual; abstract;

            function  GetNodeTag: string; virtual; abstract;

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure Initialize;

            procedure ExportNode(parentNode : IXMLNode);
            procedure ExportAttributes(selfNode : IXMLNode);

            property NodeTag : string read GetNodeTag;
    end;

implementation

{ xmlNodeClass }
//==============================================================================
procedure xmlNodeClass.AfterConstruction;
    begin
    inherited;
    DoInitialize;
    end;

//==============================================================================
procedure xmlNodeClass.BeforeDestruction;
    begin
    inherited;
    end;

//==============================================================================
procedure xmlNodeClass.Initialize;
    begin
    DoInitialize;
    end;

//==============================================================================
procedure xmlNodeClass.ExportNode(parentNode : IXMLNode);
    var
        selfNode : IXMLNode;
    begin
    selfNode := parentNode.AddChild(NodeTag);
    DoExportAttributes(selfNode);
    end;

//==============================================================================
procedure xmlNodeClass.ExportAttributes(selfNode: IXMLNode);
    begin
    DoExportAttributes(selfNode);
    end;

end.
