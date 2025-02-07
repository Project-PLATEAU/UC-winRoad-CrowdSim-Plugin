unit SurfaceMember;

interface

uses
    Xml.XMLIntf,
    Xml.XMLDoc,
    Xml.xmldom,
    F8Utils,
    F8OpenGL,
    PedestrianUtil;

type
    /// <summary>
    ///    PLATEAU道路モデルのポリゴンを表すクラス
    ///    TTrafficAreaの要素として生成・使用する
    ///    Createで頂点情報が確定し、以降不変
    ///    本システムではTSurfaceMemberのポリゴン形状は凸型を想定している
    ///    PLATEAU道路モデルのLOD1道路は凸型形状ではない場合があるため、三角形に分割している
    /// </summary>
    /// <remarks>
    ///    PLATEAU道路モデルのSurfaceMemberの構成についてメモ
    ///    読込み処理はTTrafficArea.ImportPointListで実装
    ///    - CityGMLのgml:posListを読み込む想定
    ///    - 頂点情報が半角スペース区切りで、Lat1 Lon1 Height1 Lat2 Lon2 Height2 Lat3 Lon3 Height3の順に並んでいる
    ///    - 頂点数は3以上(3つとはかぎらない)
    ///    - 連続する頂点間に辺がある(上の例では1と2, 2と3)
    ///    - 始点と終点を結ぶ辺がある(上の例では1と3)。これにより閉じたポリゴンが生成される
    /// </remarks>
    TSurfaceMember = record
        private
            const
                NODE_NAME_SURFACE_MEMBER = 'SurfaceMember';
                NODE_NAME_POINT_LIST     = 'PointList';
                NODE_NAME_EDGE_LIST      = 'EdgeList';
                NODE_NAME_BOUNDING_AREA  = 'BoundingArea';
            var
                p_PointList: TPoint3DListType;
{$IFDEF DEBUG}
                p_PointListGL: TPoint3DListType;
{$ENDIF}
                p_EdgeList : TParametricLineList;
                p_BoundingArea: VertexPositions;

            function  GetPointCount: Integer;
            function  GetEdgeCount: Integer;

            function  GetPoint(const aIdx: Integer): TPoint3D;
            function  GetEdge(const aIdx: Integer): TParametricLine2D;
            function  GetBoundingArea: VertexPositions;

            procedure MakeEdge;
            procedure MakeBoundingArea;
        public
            constructor Create(const aPosList: TPoint3DListType);
            class operator Finalize(var aDest: TSurfaceMember);

            function  IsInSurface(const aPointLatLon: TPoint3D): Boolean;
            function  IsOverlaySurface(const aVertexPositions: VertexPositions): Boolean;
            function  Clone: TSurfaceMember;

            procedure DoRender(const aOpenGL);

            procedure ExportToPluginData(const aParentNode: IXMLNode);
            class function  ImportFromPluginData(const aDataNode: IXMLNode; out aRes: TSurfaceMember): Boolean; static;

            property  PointCount                : Integer           read GetPointCount;
            property  EdgeCount                 : Integer           read GetEdgeCount;
            property  Point[const aIdx: Integer]: TPoint3D          read GetPoint;
            property  Edge[const aIdx: Integer] : TParametricLine2D read GetEdge;
            property  BoundingArea              : VertexPositions   read GetBoundingArea;
        end;

    TSurfacemembers = array of TSurfaceMember;

implementation

uses
    WinAPI.Windows,
    System.Math,
    PluginCore,
    F8GLUtils,
    GL,
    XMLPointType,
    LatLonHelper;

constructor TSurfaceMember.Create(const aPosList: TPoint3DListType);
{$IFDEF DEBUG}
    var
        i: Integer;
        api: IF8ApplicationServices;
        horiConv: IF8WrHorizontalCoordinateConvertor;
        src, dst: F8PointType;
{$ENDIF}
    begin
    SetLength(p_PointList, Length(aPosList));
    CopyMemory(p_PointList, aPosList, SizeOf(TPoint3D) * Length(aPosList));
    MakeEdge;
    MakeBoundingArea;
{$IFDEF DEBUG}
    api := theApplicationServices;

    if not Assigned(api) then
        Exit;

    SetLength(p_PointListGL, Length(p_PointList));

    horiConv := api.GetWRCoordinateConvertor.HoirizontalCSConvertor;
    for i := 0 to Length(p_PointList) - 1 do
        begin
        src[_x] := p_PointList[i].Lon;
        src[_y] := p_PointList[i].Lat;

        horiConv.Convert(_hctSpecifiedCS, 6668, _hctOpenGL_XZ, 0, src, dst);

        p_PointListGL[i].X := dst[_x];
        p_PointListGL[i].Z := dst[_y];
        end;
{$ENDIF}
    end;

class operator TSurfaceMember.Finalize(var aDest: TSurfaceMember);
    begin
    SetLength(aDest.p_PointList, 0);
    SetLength(aDest.p_EdgeList, 0);
    end;


function TSurfaceMember.IsInSurface(const aPointLatLon: TPoint3D): Boolean;
    var
        i: Integer;
        edg: TParametricLine2D;
        locations: array of Boolean;
        nt: Integer;
        abvec, apvec: TDoublePoint;
    begin
    SetLength(locations, EdgeCount);
    nt := 0;
    for i := 0 to EdgeCount - 1 do
        begin
        edg := Edge[i];

        abvec := edg.a.LatLon - edg.b.LatLon;
        apvec := edg.a.LatLon - aPointLatLon.LatLon;
        locations[i] := (abvec.Cross(apvec) > 0);
        if locations[i] then
            Inc(nt);
        end;

    Result := (nt = 0) or (nt = Length(locations));
    end;

function TSurfaceMember.IsOverlaySurface(const aVertexPositions: VertexPositions): Boolean;
    begin
    Result := IsOverlay(aVertexPositions, p_PointList);
    end;

function TSurfaceMember.Clone: TSurfaceMember;
    begin
    SetLength(Result.p_PointList, PointCount);
    CopyMemory(Result.p_PointList, p_PointList, SizeOf(TPoint3D) * PointCount);
    Result.MakeEdge;
    Result.MakeBoundingArea;
    end;

procedure TSurfaceMember.DoRender(const aOpenGL);
{$IFDEF DEBUG}
    var
        i: Integer;
{$ENDIF}
    begin
{$IFDEF DEBUG}
    glMatrixMode(GL_MODELVIEW);
        glPushAttrib(GL_ALL_ATTRIB_BITS);
        try
            glDisable(GL_LIGHTING);
            glDepthMask(GL_FALSE);
            glLineWidth(3.0);
            glColor4f(1.0, 0.5, 0.8, 1.0);

            glBegin(GL_LINE_STRIP);
            for i := 0 to Length(p_PointListGL) - 1 do
                glVertex3f(p_PointListGL[i].X, p_PointListGL[i].Y, p_PointListGL[i].Z);

            glEnd;
        finally
            glColor4f(1.0, 1.0, 1.0, 1.0);
            glLineWidth(1.0);
            glPopAttrib;
        end;
{$ENDIF}
    end;

procedure TSurfaceMember.ExportToPluginData(const aParentNode: IXMLNode);
    var
        headNode: IXMLNode;
    begin
    headNode := aParentNode.AddChild(NODE_NAME_SURFACE_MEMBER);
    TPoint3DListXMLType.ExportToXML(p_PointList, NODE_NAME_POINT_LIST, headNode);
    TParametricLineListXMLType.ExportToXML(p_EdgeList, NODE_NAME_EDGE_LIST, headNode);
    TVertexPositionsXMLType.ExportToXML(p_BoundingArea, NODE_NAME_BOUNDING_AREA, headNode);
    end;

class function TSurfaceMember.ImportFromPluginData(const aDataNode: IXMLNode; out aRes: TSurfaceMember): Boolean;
    var
        headNode, pointListNode, edgeListNode, boundingNode: IXMLNode;
        tmpPointList: TPoint3DListType;
        tmpEdgeList: TParametricLineList;
        tmpBoundingArea: VertexPositions;
    begin
    Result := False;

    headNode := aDataNode.ChildNodes.FindNode(NODE_NAME_SURFACE_MEMBER);
    if not Assigned(headNode) then
        Exit;

    pointListNode := headNode.ChildNodes.FindNode(NODE_NAME_POINT_LIST);
    if not (Assigned(pointListNode) and TPoint3DListXMLType.ImportFromXML(pointListNode, tmpPointList)) then
        Exit;

    aRes.p_PointList := tmpPointList;

    edgeListNode := headNode.ChildNodes.FindNode(NODE_NAME_EDGE_LIST);
    if not (Assigned(edgeListNode) and TParametricLineListXMLType.ImportFromXML(edgeListNode, tmpEdgeList)) then
        Exit;

    aRes.p_EdgeList := tmpEdgeList;

    boundingNode := headNode.ChildNodes.FindNode(NODE_NAME_BOUNDING_AREA);
    if not (Assigned(boundingNode) and TVertexPositionsXMLType.ImportFromXML(boundingNode, tmpBoundingArea)) then
        Exit;

    aRes.p_BoundingArea := tmpBoundingArea;

    Result := True;
    end;

function TSurfaceMember.GetPointCount: Integer;
    begin
    Result := Length(p_PointList);
    end;

function TSurfaceMember.GetEdgeCount: Integer;
    begin
    Result := Length(p_EdgeList);
    end;

function TSurfaceMember.GetPoint(const aIdx: Integer): TPoint3D;
    begin
    Result := p_PointList[aIdx];
    end;

function TSurfaceMember.GetEdge(const aIdx: Integer): TParametricLine2D;
    begin
    Result := p_EdgeList[aIdx];
    end;

function TSurfaceMember.GetBoundingArea: VertexPositions;
    begin
    Result := p_BoundingArea;
    end;

procedure TSurfaceMember.MakeEdge;
    var
        len: Integer;
        i: Integer;
        edg: TParametricLine2D;
    begin
    len := PointCount;
    SetLength(p_EdgeList, len);
    for i := 0 to len - 1 do
        begin
        if i = (len - 1) then
            begin
            // last point
            edg.a := Point[i];
            edg.b := Point[0];
            end
        else
            begin
            edg.a := Point[i];
            edg.b := Point[i + 1];
            end;
        p_EdgeList[i] := edg;
        end;
    end;

procedure TSurfaceMember.MakeBoundingArea;
    var
        minLatLon, maxLatLon: TPoint3D;
        i: Integer;
    begin
    if PointCount <= 0 then
        Exit;

    minLatLon := Point[0];
    maxLatLon := Point[0];

    for i := 0 to PointCount - 1 do
        begin
        minLatLon.Lat := Min(minLatLon.Lat, Point[i].Lat);
        minLatLon.Lon := Min(minLatLon.Lon, Point[i].Lon);
        maxLatLon.Lat := Max(maxLatLon.Lat, Point[i].Lat);
        maxLatLon.Lon := Max(maxLatLon.Lon, Point[i].Lon);
        end;

    p_BoundingArea[VertexPositionType._LeftTop].Lat := maxLatLon.Lat;
    p_BoundingArea[VertexPositionType._LeftTop].Lon := minLatLon.Lon;

    p_BoundingArea[VertexPositionType._RightTop].Lat := maxLatLon.Lat;
    p_BoundingArea[VertexPositionType._RightTop].Lon := maxLatLon.Lon;

    p_BoundingArea[VertexPositionType._RightBottom].Lat := minLatLon.Lat;
    p_BoundingArea[VertexPositionType._RightBottom].Lon := maxLatLon.Lon;

    p_BoundingArea[VertexPositionType._LeftBottom].Lat := minLatLon.Lat;
    p_BoundingArea[VertexPositionType._LeftBottom].Lon := minLatLon.Lon;
    end;
end.

