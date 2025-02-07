unit Simulator;

interface

uses
    Classes,
    SysUtils,
    IdSocketHandle,
    IdGlobal,
    IdUDPBase,
    IdUDPServer,
    Agent,
    ExternalTrigger,
    Generics.Collections,
    MengeWrapper;

type
    TOnCalcErrorMsg = procedure(const msg: String) of object;

    /// <summary>
    ///    CrowdSim.exeでのシミュレーション計算を実行するクラス
    /// </summary>
    SimulatorClass = class
        private
            p_timeStep  : Single;
            p_agents    : TObjectList<AgentClass>;
            p_triggers  : TObjectList<ExternalTriggerClass>;
            p_initSim   : Boolean;
            p_duration  : Integer;
            p_counter   : Integer;
            p_totalSteps: Integer;

            p_mengeCore : TMengeWrapper;

            FOnStartCalc: TNotifyEvent;
            FOnStopCalc : TNotifyEvent;
            FOnCalcErrorMsg : TOnCalcErrorMsg;

            function    Initialize(behaveXml: AnsiString; sceneXml: AnsiString; model: AnsiString): Boolean;

            procedure   FindTriggers;
            function    GetAgent(const i: Integer): AgentClass;
            function    GetAgentCount: Integer;
            function    GetTimeStep: Single;
            procedure   SetTimeStep(const Value: Single);
            function    GetCounter: Integer;

            procedure   DoOnStartCalc;
            procedure   DoOnStopCalc;
            procedure   DoOnCalcErrorMsg(const msg: String);
            function    GetTotalSteps: Integer;

            procedure   OutputStatusList(const targetDir: String);

        public
            procedure   AfterConstruction; override;
            procedure   BeforeDestruction; override;

            function    LoadProject(const projectName:  String): Boolean;
            procedure   Start(const server: TIdUDPServer; const clientIP: String; const clientPort: Word);
            procedure   FireExternalTrigger(const triggerName: AnsiString);

            property    agentCount: Integer read GetAgentCount;
            property    agent[const i: Integer]: AgentClass read GetAgent;
            property    timeStep: Single read GetTimeStep write SetTimeStep;
            property    totalSteps  : Integer read GetTotalSteps;

            property    initSim : Boolean read p_initSim;
            property    counter : Integer read GetCounter;

            property    OnStartCalc: TNotifyEvent read FOnStartCalc write FOnStartCalc;
            property    OnStopCalc : TNotifyEvent read FOnStopCalc  write FOnStopCalc;
            property    OnCalcErrorMsg : TOnCalcErrorMsg read FOnCalcErrorMsg write FOnCalcErrorMsg;

        end;

implementation

uses
    Math,
    Xml.XMLIntf,
    Xml.XMLDoc,
    PacketRecord;

{ SimulatorClass }

procedure SimulatorClass.AfterConstruction;
    begin
    inherited;
    p_initSim := False;
    p_timeStep := 0.1;
    p_agents := TObjectList<AGentClass>.Create;
    p_triggers := nil;
    p_totalSteps := 0;

    p_mengeCore := TMengeWrapper.Create;
    end;

procedure SimulatorClass.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_mengeCore);
    FreeAndNil(p_agents);
    if Assigned(p_triggers) then
        FreeAndNil(p_triggers);
    end;

procedure SimulatorClass.DoOnCalcErrorMsg(const msg: String);
    begin
    if Assigned(OnCalcErrorMsg) then
        OnCalcErrorMsg(msg);
    end;

procedure SimulatorClass.DoOnStartCalc;
    begin
    if Assigned(OnStartCalc) then
        OnStartCalc(Self);
    end;

procedure SimulatorClass.DoOnStopCalc;
    begin
    if Assigned(OnStopCalc) then
        OnStopCalc(Self);
    end;

procedure SimulatorClass.FindTriggers;
    var
        triggerCount : Integer;
        i   : Integer;
        s   : AnsiString;
    begin
    if Assigned(p_triggers) then
        FreeAndNil(p_triggers);
    p_triggers := TObjectList<ExternalTriggerClass>.Create;
    triggerCount := p_mengeCore.ExternalTriggerCount;
    for i := 0 to triggerCount - 1 do
        begin
        s := p_mengeCore.ExternalTriggerName(i);
        p_triggers.Add(ExternalTriggerClass.Create(s));
        end;
    end;

procedure SimulatorClass.FireExternalTrigger(const triggerName: AnsiString);
    begin
    if Length(triggerName) > 0 then
        p_mengeCore.FireExternalTrigger(PAnsiChar(triggerName[1]));
    end;

function SimulatorClass.GetAgent(const i: Integer): AgentClass;
    begin
    Result := p_agents[i];
    end;

function SimulatorClass.GetAgentCount: Integer;
    begin
    Result := p_agents.Count;
    end;

function SimulatorClass.GetCounter: Integer;
    begin
    Result := p_counter;
    end;

function SimulatorClass.GetTimeStep: Single;
    begin
    Result := p_timeStep;
    end;

function SimulatorClass.GetTotalSteps: Integer;
    begin
    Result := p_totalSteps;
    end;

function SimulatorClass.Initialize(behaveXml, sceneXml, model: AnsiString): Boolean;
    var
        count   : NativeUInt;
        i       : NativeUInt;
        agt     : AgentClass;
        x, y, z : Single;
        state   : NativeUInt;
    begin
    p_mengeCore.BeforeCalculation;
    Result := p_mengeCore.InitSimulator(PAnsiChar(@behaveXml[1]), PAnsiChar(@sceneXml[1]), PAnsiChar(@model[1]), nil);
    if Result then
        begin
        FindTriggers;
        count := p_mengeCore.AgentCount;
        for i := 0 to count - 1 do
            begin
            agt := AgentClass.Create;
            x := 0.0;
            y := 0.0;
            z := 0.0;
            p_mengeCore.GetAgentPosition(i, x, y, z);
            agt.Position.Initialize(x, y, z);
            p_mengeCore.GetAgentOrient(i, x, y);
            agt.Orientation.Initialize(x, y);
            agt.ClassNo := p_mengeCore.GetAgentClass(i);
            agt.Radius := p_mengeCore.GetAgentRadius(i);
            state := 0;
            if p_mengeCore.GetAgentState(i, state) then
                agt.State := state
            else
                agt.State := -1;
            p_agents.Add(agt);
            end;
        p_initSim := True;
        end;
    end;

function SimulatorClass.LoadProject(const projectName: String): Boolean;
    var
        XMLDoc  : IXMLDocument;
        node    : IXMLNode;
        model   : String;
        scene   : String;
        behavior: String;
    begin
    Result := False;
    p_initSim := False;
    if FileExists(projectName) then
        begin
        XMLDoc := TXMLDocument.Create(nil);
        try
            XMLDoc.LoadFromFile(projectName);
            XMLDoc.Active := True;
            node := XMLDoc.DocumentElement;
            if node.HasAttribute('scene') then
                scene := node.Attributes['scene'];
            if node.HasAttribute('behavior') then
                behavior := node.Attributes['behavior'];
            if node.HasAttribute('model') then
                model := node.Attributes['model'];
            if node.HasAttribute('duration') then
                p_duration := node.Attributes['duration'];


            scene := ExtractFileDir(projectName) + '\' + scene;
            behavior := ExtractFileDir(projectName) + '\' + behavior;

            Result := Initialize(AnsiString(behavior), AnsiString(scene), AnsiString(model));

            if Result then
                OutputStatusList( ExtractFileDir(projectName) + '\' + ChangeFileExt(ExtractFileName(projectName), '') );

        except


            end;

        end;

    end;

procedure SimulatorClass.OutputStatusList(const targetDir: String);
    var
        csvStatusList  : TStrings;
        count : NativeUInt;
        i     : NativeUInt;
        name  : AnsiString;
        pch   : PAnsiChar;
        fileName    : String;
    begin
    csvStatusList := TStringList.Create;
    try
        if DirectoryExists(targetDir) then
            begin
            count := p_mengeCore.StateCount;
            for i := 0 to count - 1 do
                begin
                pch := p_mengeCore.GetStateName(i);
                if Assigned(pch) then
                    begin
                    name := AnsiString(pch);
                    csvStatusList.Add(format('%d,%s', [i, String(name)]));
                    end;
                end;

            fileName := targetDir + '\' + 'StatusList.csv';
            csvStatusList.SaveToFile(fileName, TEncoding.UTF8);

            end;
    finally
        FreeAndNil(csvStatusList);
        end;
    end;

procedure SimulatorClass.SetTimeStep(const Value: Single);
    begin
    p_timeStep := Value;
    p_mengeCore.SetTimeStep(p_timeStep);
    end;

procedure SimulatorClass.Start(const server: TIdUDPServer; const clientIP: String; const clientPort: Word);
    const
        MAX_AGENT_PER_PACKET = 1000;
    var
        i   : Integer;
        packetCount : Integer;
        ad  : array of AgentDataType;
        ac  : NativeUInt;
        aci : TIdBytes;
        sd  : TIdBytes;
        x, y, z : Single;
        CalcCount   : Integer;
        agentCounter: NativeUInt;
        state   : NativeUInt;
    begin
    if not p_initSim then
        exit;

    if p_timeStep < 1E-2 then
        begin

        exit;
        end;

    CalcCount := Trunc(p_duration / p_timeStep);
    p_counter := 0;
    p_totalSteps := CalcCount;

    TThread.CreateAnonymousThread(
        procedure
            begin
            TThread.Synchronize(nil,
                procedure
                    begin
                    DoOnStartCalc;
                    end
                );
            try
                p_mengeCore.SetTimeStep( p_timeStep );

                SetLength(ad, MAX_AGENT_PER_PACKET);
                SetLength(aci, SizeOf(NativeUInt));
                while True do
                    begin
                    try
                        p_mengeCore.DoStep;
                    except
                        on EZeroDivide do
                            TThread.Synchronize(nil,
                                procedure
                                    begin
                                    DoOnCalcErrorMsg('浮動小数点エラー：ステップ' + IntToStr(p_counter))
                                    end
                                );
                        else
                            begin
                            TThread.Synchronize(nil,
                                procedure
                                    begin
                                    DoOnCalcErrorMsg('予期しないエラー：ステップ' + IntToStr(p_counter));
                                    end
                                );
                            break;
                            end;
                        end;

                    ac := p_mengeCore.AgentCount;
                    Move(ac, aci[0], SizeOf(NativeUInt));
                    server.SendBuffer(clientIP, clientPort, aci);

                    packetCount := 0;
                    i := 0;
                    while i < min(ac, MAX_AGENT_PER_PACKET) do
                        begin
                        agentCounter := i + packetCount * MAX_AGENT_PER_PACKET;
                        ad[i].Initialise;
                        if p_mengeCore.GetAgentPosition(agentCounter, x, y, z) then
                            ad[i].pos.CopyFrom(x, y, z);
                        if p_mengeCore.GetAgentVelocity(agentCounter, x, y, z) then
                            ad[i].vel.CopyFrom(x, y, z);
                        if p_mengeCore.GetAgentOrient(agentCounter, x, y) then
                            ad[i].orient.CopyFrom(x, y);
                        ad[i].classI := p_mengeCore.GetAgentClass(agentCounter);
                        ad[i].radius := p_mengeCore.GetAgentRadius(agentCounter);
                        state := 0;
                        if p_mengeCore.GetAgentState(agentCounter, state) then
                            ad[i].state := state
                        else
                            ad[i].state := -1;


                        if i = (MAX_AGENT_PER_PACKET - 1) then
                            begin
                            Inc(packetCount);
                            i := 0;
                            Dec(ac, MAX_AGENT_PER_PACKET);
                            SetLength(sd, MAX_AGENT_PER_PACKET * SizeOf(AgentDataType));
                            Move(ad[0], sd[0], Length(sd));
                            server.SendBuffer(clientIP, clientPort, sd);
                            end
                        else
                            Inc(i);
                        end;
                    if i > 0 then
                        begin
                        SetLength(sd, i * SizeOf(AgentDataType));
                        Move(ad[0], sd[0], Length(sd));
                        server.SendBuffer(clientIP, clientPort, sd);
                        end;

                    Inc(p_counter);
                    if p_counter >= CalcCount then
                        break;
                    Sleep(10);
                    end;

                SetLength(sd, 1);
                sd[0] := 0;
                server.SendBuffer(clientIP, clientPort, sd);
            finally
            TThread.Synchronize(nil,
                procedure
                    begin
                    DoOnStopCalc;
                    end
                );
                end;
            end
        ).Start;
    end;

end.
