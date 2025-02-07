unit PacketRecord;

interface

uses
    Vector2,
    Vector3;

type
    PSimulationFileType = ^SimulationFileType;
    SimulationFileType = packed record
        projectXml: array[0..255] of AnsiChar;
        end;

    PAgentDataType = ^AgentDataType;
    AgentDataType = packed record
        pos     : TVector3;
        vel     : TVector3;
        orient  : TVector2;
        classI  : Integer;
        radius  : Single;
        state   : NativeInt;

        procedure Initialise;
        end;

implementation

{ AgentDataType }

procedure AgentDataType.Initialise;
    begin
    pos.Initialize();
    vel.Initialize();
    orient.Initialize();
    classI := 0;
    radius := 0.0;
    state := -1;
    end;

end.
