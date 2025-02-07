unit CrowdSimUDPClient;

interface

uses
    Winapi.Windows,
    System.Classes,
    System.SysUtils,
    IdUDPClient,
    IdComponent,
    IdGlobal,
    F8GLUtils,
    F8Utils,
    PluginCore,
    CrowdSimUtils;

type
    TCrowdSimUDPReceiveEvent = procedure(const ABytes : TBytes; const ALen : Integer) of object;

    /// <summary>
    ///    シミュレーション計算時のUDP通信を管理するクラス
    /// </summary>
    TCrowdSimUDPClient = class(TIdUDPClient)
        strict private
            type
                TUDPReceiveThread = class(TThread)
                    private
                        p_UDPClient     : TCrowdSimUDPClient; // Reference to Owner UDP Client
                        p_ExitThread    : Boolean;
                    protected
                        procedure Execute; override;
                    public
                        constructor Create(const AParent: TCrowdSimUDPClient);
                end;
            var
                p_UDPReceiveThread : TUDPReceiveThread;

            p_onUDPDisconnected : TNotifyEvent;
            p_onReceive : TCrowdSimUDPReceiveEvent;

            procedure SendCmd(const cmd : Byte); overload;
            procedure SendCmd(const cmd : Byte; const val: Single); overload;
            procedure SendCmd(const cmd : Byte; const val: String); overload;
        public
            constructor Create(Host : String; Port : Integer; onReceive : TCrowdSimUDPReceiveEvent; onUDPDisconnected : TNotifyEvent);
            destructor  Destroy; override;

            procedure   LoadFile(const afile: String);
            procedure   Start;
            procedure   DoConnect;
            procedure   SetSimulationStep(const step: Single);
            procedure   SetExternalTrigger(const trigger: String);

            property    onReceive : TCrowdSimUDPReceiveEvent read p_onReceive write p_onReceive;
            property    onUDPDisconnected : TNotifyEvent read p_onUDPDisconnected write p_onUDPDisconnected;
        end;

const
    UDP_DEFAULT_HOST_ADDRESS    = '127.0.0.1';
    UDP_DEFAULT_PORT            = 43500;

    LOAD_INIT_FILE      = $01;
    SET_SIMULATION_STEP = $02;
    START_SIMULATION    = $03;
    SET_EXTERNAL_TRIGGER= $04;



implementation

uses
    AgentPacketData;

{ TCrowdSimUDPClient }

procedure   TCrowdSimUDPClient.DoConnect;
    begin
    Connect;
    p_UDPReceiveThread := TUDPReceiveThread.Create(Self);
    end;

constructor TCrowdSimUDPClient.Create(Host: String; Port: Integer; onReceive: TCrowdSimUDPReceiveEvent; onUDPDisconnected: TNotifyEvent);
    begin
    inherited Create(nil);
    Self.Host := Host;
    Self.Port := Port;
    Self.onReceive := onReceive;
    Self.onUDPDisconnected := onUDPDisconnected;
    end;

destructor TCrowdSimUDPClient.Destroy;
    begin
    if Assigned(p_UDPReceiveThread) then
        with p_UDPReceiveThread do
            begin
            FreeOnTerminate := True;

            if not Terminated then
                begin
                onReceive := nil;
                p_ExitThread := True;

                Terminate;
                end;
            end;

    if Connected then
        Disconnect;
    p_UDPReceiveThread := nil;
    inherited;
    end;

procedure TCrowdSimUDPClient.LoadFile(const afile: String);
    begin
    SendCmd(LOAD_INIT_FILE, afile);
    end;

procedure TCrowdSimUDPClient.SendCmd(const cmd: Byte);
    var
        buffer : TIdBytes;
    begin
    SetLength(buffer, 1);
    buffer[0] := cmd;
    SendBuffer(buffer);
    end;

procedure TCrowdSimUDPClient.SendCmd(const cmd: Byte; const val: String);
    var
        buffer : TIdBytes;
    begin
    Assert(Length(val) > 0, 'invalid trigger');

    SetLength(buffer, Length(val) * SizeOf(Char) + 1);
    buffer[0] := cmd;
    CopyMemory(@buffer[1], PChar(val), Length(val) * SizeOf(Char));

    SendBuffer(buffer);
    end;

procedure TCrowdSimUDPClient.SetExternalTrigger(const trigger: String);
    begin
    SendCmd(SET_EXTERNAL_TRIGGER, trigger);
    end;

procedure TCrowdSimUDPClient.SetSimulationStep(const step: Single);
    begin
    SendCmd(SET_SIMULATION_STEP, step);
    end;

procedure TCrowdSimUDPClient.SendCmd(const cmd: Byte; const val: Single);
    var
        buffer : TIdBytes;
    begin
    SetLength(buffer, SizeOf(Single) + 1);
    buffer[0] := cmd;
    CopyMemory(@buffer[1], @val, SizeOf(Single));

    SendBuffer(buffer);
    end;

procedure TCrowdSimUDPClient.Start;
    begin
    SendCmd(START_SIMULATION);
    end;

{ TCrowdSimUDPClient.TUDPReceiveThread }

constructor TCrowdSimUDPClient .TUDPReceiveThread.Create(const AParent: TCrowdSimUDPClient);
    begin
    p_UDPClient := AParent;
    p_UDPClient.BufferSize := SizeOf(AgentPacketDataType) * 1000 + 2000;
    inherited Create(False);
    p_ExitThread := False;
    FreeOnTerminate := False;
    end;

procedure TCrowdSimUDPClient.TUDPReceiveThread.Execute;
    var
        receivedBuffer  : TBytes;
        nBytesReceived  : Integer;
    begin
    inherited;
    Assert(Assigned(p_UDPClient));
    SetLength(receivedBuffer, p_UDPClient.BufferSize);

    while (not Terminated) and (not p_ExitThread) do
        with p_UDPClient do
            begin
            try
                nBytesReceived := ReceiveBuffer(TIdBytes(receivedBuffer), 1000);
                if (nBytesReceived > 0) and Assigned(p_onReceive) then
                    p_onReceive(receivedBuffer, nBytesReceived);
            except
                on e: Exception do
                    begin
                    if Assigned(onUDPDisconnected) then
                        onUDPDisconnected(Self);
                    break;
                    end;
                end;
            end;

    if Assigned(p_UDPClient) and (p_UDPClient.Connected) then
        p_UDPClient.Disconnect;
    if Assigned(p_UDPClient.onUDPDisconnected) then
        p_UDPClient.onUDPDisconnected(Self);
    end;

end.
