unit PedestrianMapEditingGuide;

interface

uses
    PluginCore,
    F8OpenGL,
    F8Utils,
    PedestrianMap,
    PedestrianUtil;

type
    /// <summary>
    ///    マウスカーソルの位置にあるTPedestrianCellに枠線を表示する
    ///    シミュレーションエリア編集時に編集予定地点を可視化するクラス
    ///    単一のセルだけでなく、カーソルの位置からaRangeで指定した範囲のTPedestrianCellを囲うような枠線を表示することも可能
    /// </summary>
    TPedestrianMapEditingGuide = class
        private
            p_MouseAreaGuide: VertexPositions;
        public
            procedure UpdateMouseAreaGuide(const aAPI: IF8ApplicationServices; const aMap: TPedestrianMap; const aLocalXZ: TPoint3D; const aRange: Cardinal);
            procedure DoRender(const aOpenGL: TF8OpenGL);
        end;

implementation

uses
    System.SysUtils,
    System.Math,
    System.Generics.Collections,
    GL,
    PedestrianCell;

{ TPedestrianMapEditingGuide }
procedure TPedestrianMapEditingGuide.UpdateMouseAreaGuide(const aAPI: IF8ApplicationServices; const aMap: TPedestrianMap; const aLocalXZ: TPoint3D;
  const aRange: Cardinal);
    function MakePoint(const aPosX, aPosZ: Double): TPoint3D;
        var
            hr: HeightResultArrayType;
            i: Integer;
            minH: Single;
        begin
        Result.X := aPosX;
        Result.Z := aPosZ;
        hr := aAPI.Project.GetHeightsAt(Result.X, Result.Z, [_hTerrain, _hRoad, _hInterSection]);
        if Length(hr) > 0 then
            begin
            minH := hr[0].hHeight;

            if Length(hr) > 1 then
                begin
                for i := 1 to Length(hr) - 1 do
                    minH := Min(minH, hr[i].hHeight);
                end;

            Result.Y := minH;
            end
        else
            Result.Y := 50.0;
        end;
    var
        cells: TObjectList<TPedestrianCell>;
        lt, rb: TPoint3D;
    begin
    if aMap.RequireCellsInfo(aLocalXZ, aRange, cells) then
        begin
        lt := cells.First.AreaGL[VertexPositionType._RightBottom];
        lt := MakePoint(lt.X, lt.Z);
        rb := cells.Last.AreaGL[VertexPositionType._LeftTop];
        rb := MakePoint(rb.X, rb.Z);

        p_MouseAreaGuide[VertexPositionType._LeftTop]     := lt;
        p_MouseAreaGuide[VertexPositionType._RightTop]    := MakePoint(rb.X, lt.Z);
        p_MouseAreaGuide[VertexPositionType._RightBottom] := rb;
        p_MouseAreaGuide[VertexPositionType._LeftBottom]  := MakePoint(lt.X, rb.Z);

        FreeAndNil(cells);
        end;
    end;

procedure TPedestrianMapEditingGuide.DoRender(const aOpenGL: TF8OpenGL);
    procedure glVertexTPoint3D(const aPoint: TPoint3D);
        begin
        glVertex3f(aPoint.X, aPoint.Y + 0.5, aPoint.Z);
        end;
    begin
    // 引数はOpenGL座標値
    glMatrixMode(GL_MODELVIEW);
        glPushAttrib(GL_ALL_ATTRIB_BITS);
        try
            glDisable(GL_LIGHTING);
            glDepthMask(GL_TRUE);
            glLineWidth(3.0);
            glColor4f(0.8, 0.8, 0.4, 0.7);

            glBegin(GL_LINE_LOOP);
            glVertexTPoint3D(p_MouseAreaGuide[VertexPositionType._LeftTop]);
            glVertexTPoint3D(p_MouseAreaGuide[VertexPositionType._RightTop]);
            glVertexTPoint3D(p_MouseAreaGuide[VertexPositionType._RightBottom]);
            glVertexTPoint3D(p_MouseAreaGuide[VertexPositionType._LeftBottom]);
            glEnd;
        finally
            glColor4f(1.0, 1.0, 1.0, 1.0);
            glLineWidth(1.0);
            glPopAttrib;
        end;
    end;
end.
