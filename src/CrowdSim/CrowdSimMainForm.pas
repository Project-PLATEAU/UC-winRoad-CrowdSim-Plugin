unit CrowdSimMainForm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
    IdBaseComponent, IdComponent, IdUDPBase, IdUDPServer, System.Actions,
    Vcl.ActnList,
    Simulator, IdGlobal, IdSocketHandle, Vcl.ExtCtrls;

type
    /// <summary>
    ///    CrowdSim.exeのメインフォームとその機能を定義するクラス
    /// </summary>
    TFormCrowdSimMain = class(TForm)
        SpinEditPort: TSpinEdit;
        ButtonStart: TButton;
        ButtonStop: TButton;
        IdUDPServer: TIdUDPServer;
        LabelPortNumber: TLabel;
        MemoStatus: TMemo;
        ActionList: TActionList;
        ActionStart: TAction;
        ActionFinish: TAction;
        TimerStepDisplay: TTimer;
        procedure ActionStartExecute(Sender: TObject);
        procedure ActionFinishExecute(Sender: TObject);
        procedure ActionStartUpdate(Sender: TObject);
        procedure ActionFinishUpdate(Sender: TObject);
        procedure IdUDPServerUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
        procedure TimerStepDisplayTimer(Sender: TObject);
        private
            p_sim   : SimulatorClass;
            p_stepLineIndex : Integer;

            procedure   DoStartCalc(Sender: TObject);
            procedure   DoStopCalc(Sender: TObject);
            procedure   DoCalcErrorMsg(const msg: String);

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
        end;

var
    FormCrowdSimMain: TFormCrowdSimMain;

const
    LOAD_INIT_FILE      = $01;
    SET_SIMULATION_STEP = $02;
    START_SIMULATION    = $03;
    SET_EXTERNAL_TRIGGER= $04;

    UDP_DEFAULT_PORT    = 43500;

implementation

{$R *.dfm}

uses
    MengeWrapper;

procedure TFormCrowdSimMain.ActionFinishExecute(Sender: TObject);
    begin
    IdUDPServer.Active := False;
    end;

procedure TFormCrowdSimMain.ActionFinishUpdate(Sender: TObject);
    begin
    ActionFinish.Enabled := IdUDPServer.Active and (not TimerStepDisplay.Enabled);
    end;

procedure TFormCrowdSimMain.ActionStartExecute(Sender: TObject);
    begin
    try
        IdUDPServer.DefaultPort := SpinEditPort.Value;
        IdUDPServer.Active := True;
        MemoStatus.Lines.Clear;
        MemoStatus.Lines.Add('通信開始しました。');
    except
        ShowMessage('接続開始時にエラーが発生しました。ネットワーク環境を見直してください。');
        end;
    end;

procedure TFormCrowdSimMain.ActionStartUpdate(Sender: TObject);
    begin
    ActionStart.Enabled := not IdUDPServer.Active;
    end;

procedure TFormCrowdSimMain.AfterConstruction;
    begin
    inherited;
    TimerStepDisplay.Enabled := False;
    SpinEditPort.Value := UDP_DEFAULT_PORT;
    p_sim := SimulatorClass.Create;
    p_sim.OnStartCalc := DoStartCalc;
    p_sim.OnStopCalc  := DoStopCalc;
    p_sim.OnCalcErrorMsg := DoCalcErrorMsg;

    ActionStartExecute(nil);
    end;

procedure TFormCrowdSimMain.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_sim);
    end;

procedure TFormCrowdSimMain.DoCalcErrorMsg(const msg: String);
    begin
    MemoStatus.Lines.Add(msg);
    p_stepLineIndex := MemoStatus.Lines.Count;
    end;

procedure TFormCrowdSimMain.DoStartCalc(Sender: TObject);
    begin
    if Assigned(p_sim) then
        MemoStatus.Lines.Add('計算開始しました。');
    p_stepLineIndex := MemoStatus.Lines.Count;
    TimerStepDisplay.Enabled := True;
    end;

procedure TFormCrowdSimMain.DoStopCalc(Sender: TObject);
    begin
    TimerStepDisplayTimer(Sender);
    TimerStepDisplay.Enabled := False;
    if Assigned(p_sim) then
        MemoStatus.Lines.Add('計算が終了しました。：ステップ' + IntToStr(p_sim.counter));
    p_stepLineIndex := MemoStatus.Lines.Count;
    end;

procedure TFormCrowdSimMain.IdUDPServerUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
    var
        c   : Byte;
        p   : TIdBytes;
        r   : String;
        t   : Single;
        counter : Integer;
    begin
    if Length(AData) > 0 then
        begin
        c := AData[0];

        if c = LOAD_INIT_FILE then
            begin
            SetLength(p, Length(AData) - 1);
            CopyMemory(@p[0], @AData[1], Length(AData) - 1);
            SetLength(r, Length(p) div SizeOf(Char));
            CopyMemory(PChar(r), @p[0], Length(p));

            MemoStatus.Lines.Add('初期化指示受け取りました。ファイル名は、' + r + 'です。');
            if p_sim.LoadProject(r) then
                MemoStatus.Lines.Add('初期化成功しました。')
            else
                MemoStatus.Lines.Add('初期化失敗しました。');
            end
        else if c = SET_SIMULATION_STEP then
            begin
            CopyMemory(@t, @AData[1], SizeOf(Single));

            p_sim.timeStep := t;
            MemoStatus.Lines.Add('時間間隔指示受け取りました。時間間隔は、' + FloatToStr(t) + 'です。');
            end
        else if c = START_SIMULATION then
            begin
            counter := 0;
            while not p_sim.initSim do
                begin
                Sleep(1000);
                Inc(counter);
                if counter > 10 then
                    break;
                end;

            if not p_sim.initSim then
                begin
                MemoStatus.Lines.Add('計算開始指示受け取りましたが、初期化できていないので終了します。通信先は、' + ABinding.PeerIP + 'です。');
                exit;
                end;

            MemoStatus.Lines.Add('計算開始指示受け取りました。通信先は、' + ABinding.PeerIP + 'です。');
            p_sim.Start(IdUDPServer, ABinding.PeerIP, ABinding.PeerPort);
            end
        else if c = SET_EXTERNAL_TRIGGER then
            begin
            SetLength(p, Length(AData) - 1);
            CopyMemory(@p[0], @AData[1], Length(AData) - 1);
            r := BytesToString(p, IndyTextEncoding_UTF8);

            p_sim.FireExternalTrigger( AnsiString(r) );
            MemoStatus.Lines.Add('トリガー設定指示受け取りました。トリガー名は、' + r + 'です。');
            end;

        end;
    end;


procedure TFormCrowdSimMain.TimerStepDisplayTimer(Sender: TObject);
    var
        msg : String;
    begin
    if Assigned(p_sim) then
        msg := format('計算中：%d/%d', [p_sim.counter, p_sim.totalSteps])
    else
        begin
        TimerStepDisplay.Enabled := False;
        ShowMessage('計算中ではないので、タイマーが動いていますよ。');
        exit;
        end;
    if p_stepLineIndex = MemoStatus.Lines.Count then
        MemoStatus.Lines.Add(msg)
    else if p_stepLineIndex + 1 = MemoStatus.Lines.Count then
        MemoStatus.Lines[p_stepLineIndex] := msg
    else
        MemoStatus.Lines.Add(msg);

    end;

end.
