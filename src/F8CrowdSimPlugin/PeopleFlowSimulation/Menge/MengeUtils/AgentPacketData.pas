unit AgentPacketData;

interface

uses
    System.SysUtils;

type
    TVector2 = packed record
        x: Single;
        y: Single;
        procedure Initialise;
        end;

    TVector3 = packed record
        x: Single;
        y: Single;
        z: Single;
        procedure Initialise;
        end;


    PAgentPacketDataType = ^AgentPacketDataType;
    AgentPacketDataType = packed record
        pos     : TVector3;
        vel     : TVector3;
        orient  : TVector2;
        classI  : Integer;
        radius  : Single;
        state   : NativeInt;

        procedure Initialise;
        procedure CopyFrom(const ABytes: TBytes; var index: Integer);
        end;
    TArrayAgentPacketDataType = array of AgentPacketDataType;


implementation

{ AgentPacketDataType }

procedure AgentPacketDataType.CopyFrom(const ABytes: TBytes; var index: Integer);
    begin
    Move(ABytes[index], Self, SizeOf(AgentPacketDataType));
    Inc(index, SizeOf(AgentPacketDataType));
    end;

procedure AgentPacketDataType.Initialise;
    begin
    pos.Initialise;
    vel.Initialise;
    orient.Initialise;
    classI := -1;
    radius := 0.0;
    state := -1;
    end;

{ TVector2 }

procedure TVector2.Initialise;
    begin
    x := 0.0;
    y := 0.0;
    end;

{ TVector3 }

procedure TVector3.Initialise;
    begin
    x := 0.0;
    y := 0.0;
    z := 0.0;
    end;

end.
