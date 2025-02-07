unit Agent;

interface

uses
    Vector2,
    Vector3;

type

    /// <summary>
    ///    CrowdSim.exe上でAgentのパラメータを管理するクラス
    /// </summary>
    AgentClass = class
        private
            p_pos   : TVector3;
            p_vel   : TVector3;
            p_orient: TVector2;
            p_class : Integer;
            p_radius: Single;
            p_state : NativeInt;
        public
            procedure   AfterConstruction; override;

            property    Position    : TVector3  read p_pos;
            property    Velocity    : TVector3  read p_vel;
            property    Orientation : TVector2  read p_orient;
            property    ClassNo     : Integer   read p_class write p_class;
            property    Radius      : Single    read p_radius write p_radius;
            property    State       : NativeInt read p_state write p_state; // もし、-1なら失敗
        end;

implementation

{ AgentClass }

procedure AgentClass.AfterConstruction;
    begin
    inherited;
    p_pos.Initialize;
    p_vel.Initialize;
    p_orient.Initialize;
    p_class := 0;
    p_radius := 0;
    p_state := -1;
    end;

end.
