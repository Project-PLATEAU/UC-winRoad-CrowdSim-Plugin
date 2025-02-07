unit TrafficArea;

interface

uses
    System.Generics.Collections,
    System.Classes,
    Xml.XMLIntf,
    Xml.XMLDoc,
    Xml.xmldom,
    F8Utils,
    F8OpenGL,
    SurfaceMember,
    PedestrianUtil;

type
    /// <summary>
    ///    PLATEAU道路モデルの道路エリアを表すクラス
    ///    TranRoadの要素として生成・使用する
    /// </summary>
    TTrafficArea = class
        private
            p_FunctionID: Integer;
            p_SurfaceMembers: TSurfaceMembers;
            p_EdgeList: TParametricLineList;
            p_BoundingArea: VertexPositions;

            procedure Reset;
            function  ImportPointList(const aPosList: String; const aIsCheck: Boolean = False): TPoint3DListType;
            function  GetFunctionID: Integer;
            procedure MakeBoundingArea;
        public
            constructor Create(const aFunctionID: Integer);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ImportSingleSurface(const aPosList: String);
            procedure ImportMultiSurface(const aPosLists: TStringList);

            function  IsOverlayArea(const aVertexPositions: VertexPositions): Boolean;
            function  IsOverlayBoundingArea(const aVertexPositions: VertexPositions): Boolean;

            function  Clone: TTrafficArea;

            procedure DoRender(const aOpenGL: TF8OpenGL);

            property  FunctionID  : Integer         read GetFunctionID;
            property  BoundingArea: VertexPositions read p_BoundingArea;
        end;

    /// <summary>
    ///    TTrafficAreaをUC-win/Roadのプロジェクトファイルに保存するためのクラス
    ///    このクラス自身はデータを持たない
    /// </summary>
    TTraffiCAreaPluginData = class
        strict private
            const
                NODE_NAME_TRAFFIC_AREA        = 'TrafficArea';
                NODE_NAME_SURFACE_MEMBER_LIST = 'SurfaceMemberList';
                NODE_NAME_SURFACE_MEMBER_ITEM = 'SurfaceMemberItem_';
                NODE_NAME_EDGE_LIST           = 'EdgeList';
                NODE_NAME_BOUNDING_AREA       = 'BoundingArea';
                TAG_FUNCTION_ID               = 'FunctionID';
                TAG_SURFACE_MEMBER_COUNT      = 'SurfaceMemberCount';
        public
            class procedure ExportToPluginData(const aTrafficArea: TTrafficArea; const aParentNode:IXMLNode);
            class function  ImportFromPluginData(const aDataNode: IXMLNode; out aRes: TTrafficArea): Boolean;
        end;

implementation

uses
    Winapi.Windows,
    System.SysUtils,
    System.Math,
    ParametricLine2DHelper,
    XMLPointType,
    LatLonHelper;

{TTrafficArea}
constructor TTrafficArea.Create(const aFunctionID: Integer);
    begin
    p_FunctionID := aFunctionID;
    end;

procedure TTrafficArea.AfterConstruction;
    begin
    inherited;

    Reset;
    end;

procedure TTrafficArea.BeforeDestruction;
    begin
    inherited;

    Reset;
    end;

/// <summary>
///    PLATEAU道路モデルのLOD1道路(SurfaceMemberが1つ)を読み込む
///    本システムではTSurfaceMemberのポリゴン形状は凸型を想定している
///    PLATEAU道路モデルのLOD1道路は凸型形状ではない場合があるため、三角形に分割する処理を行っている
///    分割の考え方は「ある地点から最も遠い頂点とその前後の頂点から成る三角形を順に切り出していく」手法
/// </summary>
procedure TTrafficArea.ImportSingleSurface(const aPosList: String);
    var
        posList: TPoint3DListType;
        positionCheckTable: TDictionary<Integer, Boolean>;
        newPosList: TPoint3DListType;
        currentIdx, previousIdx, nextIdx: Integer;
        isIncludingIndex: Boolean;
        preDirection: TPoint3D;

    function FindNextIndex(const aStartIdx: Integer): Integer;
        var
            count: Integer;
        begin
        Result := aStartIdx;
        count := Length(posList);
        while True do
            begin
            Result := (Result + 1) mod count;
            if not positionCheckTable[Result] then
                Break;
            end;
        end;

    function FindPreviousIndex(const aStartIdx: Integer): Integer;
        var
            count: Integer;
        begin
        Result := aStartIdx;
        count := Length(posList);
        while True do
            begin
            Result := IfThen((Result - 1) >= 0, Result - 1, count - 1);
            if not positionCheckTable[Result] then
                Break;
            end;
        end;

    procedure FindFarPoint;
        var
            key: Integer;
            farIndex: Integer;
            maxDist, dist: Double;
        begin
        farIndex := -1;
        maxDist := MinDouble;

        for key in positionCheckTable.Keys do
            begin
            if positionCheckTable[key] then
                Continue;

            dist := posList[key].DistanceTo(ZERO_POINT3D);
            if (dist > maxDist) then
                begin
                maxDist := dist;
                farIndex := key;
                end;
            end;

        currentIdx  := farIndex;
        nextIdx     := FindNextIndex(farIndex);
        previousIdx := FindPreviousIndex(farIndex);
        end;

    function  CheckInPoint(const aPos: TPoint3D): Boolean;
        var
            i: Integer;
            tp: TPoint3DListType;
            edg1, edg2: Tpoint3D;
            prevNormal, normal: TPoint3D;
        begin
        Result := True;
        SetLength(tp, 3);
        tp[0] := posList[currentIdx];
        tp[1] := posList[nextIdx];
        tp[2] := posList[previousIdx];

        prevNormal := ZERO_POINT3D;
        for i := 0 to 2 do
            begin
            edg1 := aPos - tp[i];
            edg2 := aPos - tp[(i + 1) mod 3];
            normal := CrossProduct(edg1, edg2).Normalise;
            if (prevNormal = ZERO_POINT3D) then
                begin
                prevNormal := normal;
                Continue;
                end;
            if DotProduct(prevNormal, normal) <= 0.99 then
                begin
                Result := False;
                Break;
                end;
            end;
        end;

    function IsIncludePoint: Boolean;
        var
            key: Integer;
        begin
        Result := False;
        for key in positionCheckTable.Keys do
            begin
            if positionCheckTable[key] then
                Continue;

            if (key = currentIdx) or (key = NextIdx) or (key = PreviousIdx) then
                Continue;

            if CheckInPoint(posList[key]) then
                begin
                Result := True;
                Break;
                end;
            end;
        end;

    function GetCurrentDirection: TPoint3D;
        var
            edg1, edg2: TPoint3D;
        begin
        edg1 := (posList[nextIdx] - posList[currentIdx]).Normalise;
        edg2 := (posList[previousIdx] - posList[currentIdx]).Normalise;
        Result := CrossProduct(edg2, edg1);
        end;

    procedure MoveToNext;
        begin
        currentIdx := FindNextIndex(currentIdx);
        nextIdx := FindNextIndex(currentIdx);
        previousIdx := FindPreviousIndex(currentIdx);
        end;

    function DetectTriangle: Boolean;
        var
            a, b, c: TPoint3D;
            newDirection: TPoint3D;
        begin
        Result := False;
        if not isIncludingIndex then
            FindFarPoint;

        a := posList[currentIdx];
        b := posList[nextIdx];
        c := posList[previousIdx];

        if IsIncludePoint then
            begin
            preDirection := GetCurrentDirection;
            isIncludingIndex := True;
            MoveToNext;
            Exit;
            end;

        if isIncludingIndex then
            begin
            newDirection := GetCurrentDirection;
            if DotProduct(newDirection, preDirection) <= 0.0 then
                begin
                MoveToNext;
                Exit;
                end;
            end;

        isIncludingIndex := False;

        newPosList[0] := a;
        newPosList[1] := b;
        newPosList[2] := c;

        positionCheckTable[currentIdx] := True;
        Result := True;
        end;

    procedure SetLastTriangle;
        var
            i, key: Integer;
        begin
        for i := 0 to 2 do
            begin
            for key in positionCheckTable.Keys do
                begin
                if not positionCheckTable[key] then
                    begin
                    newPosList[i] := posList[key];
                    positionCheckTable[key] := True;
                    Break;
                    end;
                end;
            end;
        end;

    var
        len: Integer;
        edg: TParametricLine2D;
        i: Integer;
        value: Boolean;
        unChecked: Integer;
    begin
    Reset;
    posList := ImportPointList(aPosList, true);

    SetLength(newPosList, 3);
    isIncludingIndex := False;
    preDirection := ZERO_POINT3D;

    positionCheckTable := TDictionary<Integer, Boolean>.Create;
    try
        for i := 0 to Length(posList) - 1 do
            positionCheckTable.Add(i, False);

        while True do
            begin
            unChecked := 0;
            for value in positionCheckTable.Values do
                begin
                if not value then
                    Inc(unChecked);
                end;
            if unChecked = 3 then
                begin
                SetLastTriangle;

                len := Length(p_SurfaceMembers);
                SetLength(p_SurfaceMembers, len + 1);
                p_SurfaceMembers[len].Create(newPosList);

                Break;
                end;

            if unChecked <= 2 then
                Break;

            if DetectTriangle then
                begin
                len := Length(p_SurfaceMembers);
                SetLength(p_SurfaceMembers, len + 1);
                p_SurfaceMembers[len].Create(newPosList);
                end;
            end;
    finally
        FreeAndNil(positionCheckTable);
        end;

    SetLength(p_EdgeList, Length(posList));
    len := Length(p_EdgeList);
    for i := 0 to len - 1 do
        begin
        if i = (len - 1) then
            begin
            // last point
            edg.a := posList[i];
            edg.b := posList[0];
            end
        else
            begin
            edg.a := posList[i];
            edg.b := posList[i + 1];
            end;
        p_EdgeList[i] := edg;
        end;

    MakeBoundingArea;
    end;

/// <summary>
///    PLATEAU道路モデルのLOD3道路(SurfaceMemberが複数)を読み込む
///    SurfaceMemberが複数あるので、「TrafficArea全体」で囲われている領域が分からない
///    そこで、SurfaceMemberの外形線を抽出して「TrafficArea全体」の領域計算も同時に行っている
///    (LOD1道路ではSurfaceMember=TrafficArea全体なのでこの処理は不要。逆にTrafficArea全体を分割する処理が要る)
/// </summary>
/// <remarks>
///    ImportSingleSurfaceと同様に凸多角形になるよう分割したほうが良いと思われるが
///    ユースケースで使用した都市データに含まれるLOD3道路のSurfaceMemberは全て三角形で構成されていたので
///    今回はそれらのデータを前提として分割処理を省略している
/// </remarks>
procedure TTrafficArea.ImportMultiSurface(const aPosLists: TStringList);
    procedure MakeEdge;
        var
            len: Integer;
            suf: TSurfaceMember;
            edg: TParametricLine2D;
            i, idx, duplicateIdx, resIdx: Integer;
            tmpItemList: TParametricLineList;
            tmpDuplicateItemList: TParametricLineList;
        begin
        SetLength(p_EdgeList, 0);

        // 辺の数を計算
        len := 0;
        for suf in p_SurfaceMembers do
            len := len + suf.EdgeCount;

        SetLength(tmpItemList, len);
        SetLength(tmpDuplicateItemList, (len div 2) + 1);

        idx := 0;
        duplicateIdx := 0;
        for suf in p_SurfaceMembers do
            begin
            for i := 0 to suf.EdgeCount - 1 do
                begin
                edg := suf.Edge[i];
                if edg.CheckContains(tmpItemList, idx) then
                    begin
                    tmpDuplicateItemList[duplicateIdx] := edg;
                    Inc(duplicateIdx);
                    Continue;
                    end;
                tmpItemList[idx] := edg;
                Inc(idx);
                end;
            end;

        SetLength(tmpDuplicateItemList, duplicateIdx);
        SetLength(tmpItemList, idx);
        SetLength(p_EdgeList, idx);

        resIdx := 0;
        for i := 0 to idx - 1 do
            begin
            edg := tmpItemList[i];
            if edg.CheckContains(tmpDuplicateItemList, duplicateIdx) then
                Continue;

            p_EdgeList[resIdx] := edg;
            Inc(resIdx);
            end;

        SetLength(p_EdgeList, resIdx);
        end;
    var
        i: Integer;
    begin
    Reset;
    SetLength(p_SurfaceMembers, aPosLists.Count);
    for i := 0 to aPosLists.Count - 1 do
        p_SurfaceMembers[i].Create(ImportPointList(aPosLists[i]));

    MakeEdge;
    MakeBoundingArea;
    end;

function TTrafficArea.IsOverlayArea(const aVertexPositions: VertexPositions): Boolean;
    var
        suf: TSurfaceMember;
    begin
    Result := False;

    if not IsOverlayBoundingArea(aVertexPositions) then
        Exit;

    for suf in p_SurfaceMembers do
        begin
        if suf.IsOverlaySurface(aVertexPositions) then
            begin
            Result := True;
            Break;
            end;
        end;
    end;

function TTrafficArea.IsOverlayBoundingArea(const aVertexPositions: VertexPositions): Boolean;
    begin
    Result := IsOverlay(p_BoundingArea, aVertexPositions);
    end;

function TTrafficArea.Clone: TTrafficArea;
    var
        i: Integer;
    begin
    Result := TTrafficArea.Create(FunctionID);

    SetLength(Result.p_SurfaceMembers, Length(p_SurfaceMembers));
    for i := 0 to Length(p_SurfaceMembers) - 1 do
        Result.p_SurfaceMembers[i] := p_SurfaceMembers[i].Clone;

    SetLength(Result.p_EdgeList, Length(p_EdgeList));
    CopyMemory(Result.p_EdgeList, p_EdgeList, SizeOf(TParametricLine2D) * Length(p_EdgeList));
    Result.MakeBoundingArea;
    end;

procedure TTrafficArea.DoRender(const aOpenGL: TF8OpenGL);
{$IFDEF DEBUG}
    var
        i: Integer;
        len: Integer;
{$ENDIF}
    begin
{$IFDEF DEBUG}
    len := Length(p_SurfaceMembers);
    for i := 0 to len - 1 do
        p_SurfaceMembers[i].DoRender(aOpenGL);
{$ENDIF}
    end;

procedure TTrafficArea.Reset;
    begin
    SetLength(p_SurfaceMembers, 0);
    SetLength(p_EdgeList, 0);
    end;

function TTrafficArea.ImportPointList(const aPosList: String; const aIsCheck: Boolean): TPoint3DListType;
    var
        strList: TStringList;
        l: TList<TPoint3D>;
        i: Integer;
        po: TPoint3D;
    begin
    strList := TStringList.Create;
    l := TList<TPoint3D>.Create;
    try
        strList.CommaText := aPosList;
        Assert((strList.Count mod 3) = 0);
        for i := 0 to strList.Count - 1 do
            begin
            case i mod 3 of
                0: po.Lat := strToFloatDef(strList[i], 0.0);
                1: po.Lon := strToFloatDef(strList[i], 0.0);
                2:
                    begin
                    po.Height := strToFloatDef(strList[i], 0.0);
                    if aIsCheck then
                        begin
                        if not l.Contains(po) then
                            l.Add(po);
                        end
                    else
                        l.Add(po);
                    end;
                end;
            end;

        SetLength(Result, l.Count);
        for i := 0 to l.Count - 1 do
            Result[i] := l[i];
    finally
        strList.Free;
        FreeAndNil(l);
        end;
    end;

function TTrafficArea.GetFunctionID: Integer;
    begin
    Result := p_FunctionID;
    end;

procedure TTrafficArea.MakeBoundingArea;
    var
        minLatLon, maxLatLon: TPoint3D;
        i: Integer;
        len: Integer;
    begin
    len := Length(p_EdgeList);
    if len <= 0 then
        Exit;

    minLatLon := p_EdgeList[0].a;
    maxLatLon := p_EdgeList[0].a;

    for i := 0 to len - 1 do
        begin
        minLatLon.Lat := Min(minLatLon.Lat, Min(p_EdgeList[i].a.Lat, p_EdgeList[i].b.Lat));
        minLatLon.Lon := Min(minLatLon.Lon, Min(p_EdgeList[i].a.Lon, p_EdgeList[i].b.Lon));
        maxLatLon.Lat := Max(maxLatLon.Lat, Max(p_EdgeList[i].a.Lat, p_EdgeList[i].b.Lat));
        maxLatLon.Lon := Max(maxLatLon.Lon, Max(p_EdgeList[i].a.Lon, p_EdgeList[i].b.Lon));
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

{ TTraffiCAreaPluginData }
class procedure TTraffiCAreaPluginData.ExportToPluginData(const aTrafficArea: TTrafficArea; const aParentNode: IXMLNode);
    var
        i: Integer;
        headNode: IXMLNode;
        listNode, itemNode: IXMLNode;
    begin
    headNode := aParentNode.AddChild(NODE_NAME_TRAFFIC_AREA);
    headNode.Attributes[TAG_FUNCTION_ID] := aTrafficArea.p_FunctionID.ToString;
    headNode.Attributes[TAG_SURFACE_MEMBER_COUNT] := Length(aTrafficArea.p_SurfaceMembers).ToString;
    listNode := headNode.AddChild(NODE_NAME_SURFACE_MEMBER_LIST);
    for i := 0 to Length(aTrafficArea.p_SurfaceMembers) - 1 do
        begin
        itemNode := listNode.AddChild(Format('%s%d', [NODE_NAME_SURFACE_MEMBER_ITEM, i]));
        aTrafficArea.p_SurfaceMembers[i].ExportToPluginData(itemNode);
        end;

    TParametricLineListXMLType.ExportToXML(aTrafficArea.p_EdgeList, NODE_NAME_EDGE_LIST, headNode);
    TVertexPositionsXMLType.ExportToXML(aTrafficArea.p_BoundingArea, NODE_NAME_BOUNDING_AREA, headNode);
    end;

class function TTraffiCAreaPluginData.ImportFromPluginData(const aDataNode: IXMLNode; out aRes: TTrafficArea): Boolean;
    var
        headNode, listNode, itemNode, EdgeNode, BoundingNode: IXMLNode;
        i: Integer;
        tmpFunctionID: Integer;
        tmpSurfaceCount: Integer;
        tmpEdgeList: TParametricLIneList;
        tmpBoundingArea: VertexPositions;
        tmpSurface: TSurfaceMember;
    begin
    Result := False;
    headNode := aDataNode.ChildNodes.FindNode(NODE_NAME_TRAFFIC_AREA);
    if not Assigned(headNode) then
        Exit;

    if not headNode.HasAttribute(TAG_FUNCTION_ID) then
        Exit;

    tmpFunctionID := StrToIntDef(headNode.Attributes[TAG_FUNCTION_ID], 0);

    edgeNode := headNode.ChildNodes.FindNode(NODE_NAME_EDGE_LIST);
    if not Assigned(edgeNode) then
        Exit;

    if not TParametricLineListXMLType.ImportFromXML(edgeNode, tmpEdgeList) then
        Exit;

    boundingNode := headNOde.ChildNodes.FindNode(NODE_NAME_BOUNDING_AREA);
    if not Assigned(boundingNode) then
        Exit;

    if not TVertexPositionsXMLType.ImportFromXML(boundingNode, tmpBoundingArea) then
        Exit;

    if not headNode.HasAttribute(TAG_SURFACE_MEMBER_COUNT) then
        Exit;

    tmpSurfaceCount := StrToIntDef(headNode.Attributes[TAG_SURFACE_MEMBER_COUNT], 0);

    listNode := headNode.ChildNodes.FindNode(NODE_NAME_SURFACE_MEMBER_LIST);
    if (not Assigned(listNode)) and (listNode.ChildNodes.Count <> tmpSurfaceCount) then
        Exit;

    aRes := TTrafficArea.Create(tmpFunctionID);
    SetLength(aRes.p_SurfaceMembers, tmpSurfaceCount);
    for i := 0 to listNode.ChildNodes.Count - 1 do
        begin
        itemNode := listNode.ChildNodes.FindNode(Format('%s%d', [NODE_NAME_SURFACE_MEMBER_ITEM, i]));
        if not Assigned(itemNode) then
            Break;

        if TSurfaceMember.ImportFromPLuginData(itemNode, tmpSurface) then
            aRes.p_SurfaceMembers[i] := tmpSurface;
        end;

    aRes.p_EdgeList := tmpEdgeList;
    aRes.p_BoundingArea := tmpBoundingArea;

    Result := True;
    end;
end.
