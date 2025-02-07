unit MengeCommunicator;

interface

uses
    System.SysUtils,System.Generics.Collections,System.Classes,System.SyncObjs,
    AgentPacketData,
    F8GLUtils,PluginCore,F8Utils,
    F8CrowdSimController,CrowdSimUDPClient,MengeXml,
    MFProgressForm;

type
    /// <summary>
    ///    シミュレーション計算時の独自スレッドを管理するクラス
    /// </summary>
    Tcommunicatethread = class(TThread)
        private
            FOnComwithSim : TNotifyEvent;

        public
            procedure Execute; override;
            property OnComwithSim : TNotifyEvent read FOnComwithSim write FOnComwithSim;
    end;

    /// <summary>
    ///    シミュレーション計算と計算時の通信を管理するクラス
    ///    シミュレーション計算は独自スレッドで実行される
    /// </summary>
    TMengeCommunicationManagerClass = Class
        private
            const
                PROCESSNAME = 'CrowdSim';
            var
                p_SigTime    : integer;
                p_Controller : F8CrowdSimControllerClass;
                p_CrowdSim : TCrowdSimUDPClient;
                p_MengeXML : MengeXmlClass;

                p_communicatethread : Tcommunicatethread;
                p_progressForm : TFormProgress;

                p_wait : Integer;
              
                p_Directory : String;
                p_ProjectName : String;
                p_CalcSimTime : TDateTime;
                p_ProgressPer : integer;
                p_ProgressPerCS : TCriticalSection;

            procedure OnSimUDPRcv(const ABytes: TBytes; const ALen: Integer);
            procedure OnSimUDPDisconnected(Sender: TObject);
            procedure ComWithSim(sender : TObject);
            function  IsProcessRunning(const Process : string): Boolean;
            procedure EndSim;
            procedure SuspendSim;
            function  GetProgressPer: Integer;
            procedure SetProgressPer(const Value: Integer);
            procedure DoOnCloseProgress(Sender: TObject);

            property  ProgressPer : Integer read GetProgressPer write SetProgressPer;

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            function  ExportMENGEFiles(lt, rb: GLPointType; SigTime: integer): Boolean;
            procedure StartCalcMENGE(cstime : TDateTime);

            property Controller : F8CrowdSimControllerClass read p_Controller write p_Controller;
            property XML        : MengeXmlClass             read p_MengeXML   write p_MengeXML;
        end;

implementation

uses
    Vcl.Dialogs,
    Winapi.Windows,
    System.DateUtils,System.Types,System.IOUtils,
    Winapi.ShellAPI,
    Winapi.psapi,
    mengeSimSetting,
    MovingFeature;

{ communicatethread }

//==============================================================================
procedure Tcommunicatethread.Execute;
    begin
    inherited;
    if Assigned(FOnComwithSim) then
        FOnComwithSim(self);

    end;

{ MengeCommunicationManager }
//==============================================================================
procedure TMengeCommunicationManagerClass.AfterConstruction;
    begin
    inherited;
    p_ProgressPer := 0;
    p_ProgressPerCS := TCriticalSection.Create;
    end;

//==============================================================================
procedure TMengeCommunicationManagerClass.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_ProgressPerCS);
    end;

//==============================================================================
procedure TMengeCommunicationManagerClass.ComWithSim(sender : TObject);
    begin
    if Assigned(p_CrowdSim) then
        begin
        while not p_CrowdSim.Connected do
            begin

            if not IsProcessRunning(PROCESSNAME) then
                begin
                sleep(1000);
                Inc(p_wait,1000);
                end
            else
                begin
                sleep(1000);
                p_crowdSim.DoConnect;
                p_crowdSim.LoadFile(p_Directory + '\' + p_Projectname + '.xml');
                p_crowdSim.Start;
                end;
            if p_wait>5000 then
                begin
                EndSim;
                Exit;
                end;
            end;
        end;
    end;

//==============================================================================
function TMengeCommunicationManagerClass.IsProcessRunning(const Process: string): Boolean;
    var
        ProcessList: array of DWORD;
        ProcessCount, i: Cardinal;
        hProcess: THandle;
        ExeFileName: array[0..MAX_PATH] of Char;
    begin
    Result := False;
    SetLength(ProcessList, 1024);  // プロセスリストのサイズ
    if EnumProcesses(@ProcessList[0], Length(ProcessList) * SizeOf(DWORD), ProcessCount) then
        begin
        // プロセス数を取得
        ProcessCount := ProcessCount div SizeOf(DWORD);
        for i := 0 to ProcessCount - 1 do
            begin
            hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessList[i]);
            if hProcess <> 0 then
                begin
                // 実行中のプロセスのファイルパスを取得
                if GetModuleFileNameEx(hProcess, 0, ExeFileName, MAX_PATH) > 0 then
                    begin
                    if Pos(UpperCase(Process), UpperCase(ExeFileName)) > 0 then
                        begin
                        Result := True;
                        Break;
                        end;
                    end;
                CloseHandle(hProcess);
                end;
            end;
        end;
    end;

//==============================================================================
procedure TMengeCommunicationManagerClass.StartCalcMENGE(cstime : TDateTime);
    var
        fn      : String;
        m_setting : mengeSimSettingType;
        path : string;
    begin
    p_CalcSimTime  := cstime;
    m_setting :=  LoadMengeSimSettingFromRegistry;

	// Simulation
    p_CrowdSim := TCrowdSimUDPClient.Create(m_setting.Ip, m_setting.Port, OnSimUDPRcv, OnSimUDPDisconnected);

    if DirectoryExists(p_Directory) then
        fn := p_Directory + '\' + p_Projectname + '.xml';
    if FileExists(fn) then
        begin
        if not IsProcessRunning(PROCESSNAME) then
            begin
            path := theApplicationServices.HomeDirectory + '\' + PROCESSNAME + '.exe';
            if fileExists(path) then
                begin
                ShellExecute(0, 'open', PChar(path), nil, nil, SW_SHOWNORMAL);
                end;
            end;

        ProgressPer := 0;
        p_progressForm := TFormProgress.Create(nil);
        p_progressForm.OnGetProgressValue := GetProgressPer;
        p_progressForm.mainMessage := '計算中';
        p_progressForm.ButtonLabel := '中断';
        p_progressForm.OnClose := DoOnCloseProgress;

        p_progressForm.Show;

        p_wait := 0;
        p_communicatethread := Tcommunicatethread.Create;
        p_communicatethread.OnComwithSim := ComWithSim;
        p_communicatethread.FreeOnTerminate := True;

        MovingFeatureListClass.ClearFocusedHistories;
        end;
    end;

//==============================================================================
procedure TMengeCommunicationManagerClass.DoOnCloseProgress(Sender: TObject);
    begin
    SuspendSim;
    end;

//==============================================================================
procedure TMengeCommunicationManagerClass.SuspendSim;
    begin
    if Assigned(p_communicatethread) and (not p_communicatethread.Terminated) then
        p_communicatethread.Terminate;

    if Assigned(p_crowdSim) then
        FreeAndNil(p_crowdSim);

    MovingFeatureListClass.EndSimulationFromMenge;
    if Assigned(p_progressForm) then
        begin
        p_progressForm.mainMessage := '中断しました。';
        p_progressForm.ButtonLabel := '終了';
        p_progressForm.OnClose := nil;
        end;

    end;

//==============================================================================
procedure TMengeCommunicationManagerClass.EndSim;
    begin
    ProgressPer := 1;
    if Assigned(p_communicatethread) and (not p_communicatethread.Terminated) then
        p_communicatethread.Terminate;

    if Assigned(p_crowdSim) then
        FreeAndNil(p_crowdSim);

    Progressper := 100;
    if Assigned(p_progressForm) then
        begin
        p_progressForm.mainMessage := '計算が終了しました。';
        p_progressForm.ButtonLabel := '終了';
        p_progressForm.OnClose := nil;
        end;

    MovingFeatureListClass.EndSimulationFromMenge;
    end;

//==============================================================================
function TMengeCommunicationManagerClass.ExportMENGEFiles(lt, rb: GLPointType; SigTime: integer): Boolean;
    var
        pd  : String;
        i: Integer;
        {$ifdef Debug}
        s : TDateTime;
        {$endif}
        m_setting : mengeSimSettingType;
    begin
    m_setting :=  LoadMengeSimSettingFromRegistry;
    p_Directory    := m_setting.Directory;
    p_Projectname  := m_setting.ProjectName;
    p_SigTime      := SigTime;

    Result := False;
    MovingFeatureListClass.ClearMagnificateFeature;
    if DirectoryExists(p_Directory) then
        begin
        {$ifdef Debug}
        s := Now;
        {$endif}
        for i := 0 to MovingFeatureListClass.numberOfMovingFeatures - 1 do
            MovingFeatureListClass.movingFeature[i].CalcTargetArea(lt, rb);

        // ここにMENGE用ファイル作成
        pd := p_Directory + '\' + p_ProjectName;
        if not DirectoryExists(p_ProjectName) then
            TDirectory.CreateDirectory(pd);

        Result := p_MengeXML.SummerizeXmls(p_Directory, p_ProjectName,p_Controller.StartSimTime,
                                                            p_Controller.EndSimTime, p_SigTime);

        {$ifdef Debug}
        outputDebugString(PWideChar(Format('出力時間%f(sec)',[MilliSecondsBetween(s,Now)/1000])))
        {$endif}
        end
    else
        ShowMessage('指定した作業フォルダはありませんでした。');
    end;

//==============================================================================
procedure TMengeCommunicationManagerClass.OnSimUDPRcv(const ABytes: TBytes; const ALen: Integer);
    var
        adata   : TArrayAgentPacketDataType;
        i       : Integer;
        index   : Integer;
        pd     : String;
        fn : String;
    begin
    if ALen = 0 then
        Exit;

    if (ALen = 1) and (ABytes[0] = 0) then
        begin
        if MovingFeatureListClass.numberOfMovingFeatures > 0 then
            for i := 0 to MovingFeatureListClass.numberOfMovingFeatures-1 do
                MovingFeatureListClass.movingFeature[i].AfterCalculateByMENGE;

        pd := p_Directory + '\' + p_ProjectName;
        if not MovingFeatureListClass.MakeCurrentStateList(pd) then
            ShowMessage('ステート情報が読み込めませんでした。\n' + p_Directory + 'が存在するかご確認ください。');

        ProgressPer := Trunc(SecondSpan(p_CalcSimTime,p_Controller.StartSimTime)/SecondSpan(p_Controller.StartSimTime,p_Controller.EndSimTime)*100);
        p_CalcSimTime := IncSecond(p_CalcSimTime,p_SigTime);
        if CompareDateTime(p_CalcSimTime,p_Controller.EndSimTime) <> LessThanValue then//End Sim
            begin
            EndSim;
            end
        else//Between Sim and Sim
            begin
            fn := p_Directory + '\' + p_ProjectName + '.xml';
            if FileExists(fn) then
                begin
                if p_MengeXML.ExportXmls(p_Directory, p_ProjectName, p_SigTime, SecondsBetween(p_CalcSimTime,p_Controller.StartSimTime)) then
                    begin
                    p_crowdSim.LoadFile(fn);
                    p_crowdSim.Start;
                    end
                else
                    begin
                    while(CompareDateTime(p_CalcSimTime,p_Controller.EndSimTime) = LessThanValue) do
                        begin
                        p_CalcSimTime := IncSecond(p_CalcSimTime,p_SigTime);
                        if p_MengeXML.ExportXmls(p_Directory, p_ProjectName, p_SigTime, SecondsBetween(p_CalcSimTime,p_Controller.StartSimTime)) then
                            begin
                            p_crowdSim.LoadFile(fn);
                            p_crowdSim.Start;
                            Exit;
                            end;
                        end;
                    EndSim;
                    end;
                end
            else
                begin
                ShowMessage('指定した作業フォルダはありませんでした。');
                end;
            end;
        end
    else if ALen = 4 then
        begin
        //
        end
    else//During Sim
        begin
        SetLength(adata, ALen div SizeOf(AgentPacketDataType));

        if MovingFeatureListClass.numberOfMovingFeatures = 0 then
            begin
            for i := 0 to Length(adata)-1 do
                MovingFeatureListClass.AppendNewMovingFeature;
            end;

        index := 0;
        for i := Low(adata) to High(adata) do
            adata[i].CopyFrom(ABytes, index);
        MovingFeatureListClass.SetThePositionFromMENGE(p_CalcSimTime, adata);
        end;
    end;

//==============================================================================
procedure TMengeCommunicationManagerClass.OnSimUDPDisconnected(Sender: TObject);
    begin
    FreeAndNil(p_crowdSim);
    end;

//==============================================================================
procedure TMengeCommunicationManagerClass.SetProgressPer(const Value: Integer);
    begin
    if not Assigned(p_ProgressPerCS) then
        Exit;

    p_ProgressperCS.Enter;
    try
        p_Progressper := Value;
    finally
        p_ProgressperCS.Leave;
        end;
    end;

//==============================================================================
function TMengeCommunicationManagerClass.GetProgressPer: Integer;
    begin
    Result := 1;
    if not Assigned(p_ProgressPerCS) then
        Exit;

    p_ProgressperCS.Enter;
    try
        Result := p_Progressper;
    finally
        p_ProgressperCS.Leave;
        end;
    end;

end.
