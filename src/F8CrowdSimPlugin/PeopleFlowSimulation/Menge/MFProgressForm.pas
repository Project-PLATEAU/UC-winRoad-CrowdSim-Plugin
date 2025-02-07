unit MFProgressForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  System.SyncObjs,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
    OnGetProgressValueEvent = function : Integer of Object;

    /// <summary>
    ///    シミュレーション計算の進行度を示すフォームとその機能を定義するクラス
    /// </summary>
    TFormProgress = class(TForm)
            ProgressBar1: TProgressBar;
            Panel1: TPanel;
            Panel2: TPanel;
            Button1: TButton;
            TimerProgress: TTimer;
            procedure TimerProgressTimer(Sender: TObject);
            procedure Button1Click(Sender: TObject);

        private
            p_ProgressPer : Integer;
            p_ProgressCS : TCriticalSection;
            FOnGetProgressValue : OnGetProgressValueEvent;
            FOnClose : TNotifyEvent;

            function  GetProgressPas: Integer;
            procedure SetProgressPar(const Value: Integer);
            function GetbuttonString: String;
            function GetmainMessage: String;
            procedure SetButtonString(const Value: String);
            procedure SetMainMessage(const Value: String);

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure AssignMessage(str : String);

            property OnGetProgressValue : OnGetProgressValueEvent read FOnGetProgressValue write FOnGetProgressValue;
            property OnClose : TNotifyEvent read FOnClose write FOnClose;
            property ProgressPer : Integer read GetProgressPas write SetProgressPar;
            property ButtonLabel : String read GetbuttonString write SetButtonString;
            property mainMessage : String read GetmainMessage  write SetMainMessage;
        end;

implementation

{$R *.dfm}

{ TFormProgress }
//==============================================================================
procedure TFormProgress.AfterConstruction;
    begin
    inherited;
    p_ProgressPer := 0;
    p_ProgressCS := TCriticalSection.Create;
    ProgressBar1.Min :=0;
    ProgressBar1.Max :=100;
    end;

//==============================================================================
procedure TFormProgress.AssignMessage(str: String);
    begin
    Panel1.Caption :=  str;
    end;

//==============================================================================
procedure TFormProgress.BeforeDestruction;
    begin
    inherited;

    end;

//==============================================================================
procedure TFormProgress.Button1Click(Sender: TObject);
    begin
    if Assigned(FOnClose) then
        FOnClose(Sender)
    else
        self.Close;
    end;

//==============================================================================
function TFormProgress.GetProgressPas: Integer;
    begin
    p_ProgressCS.Enter;
    try
        Result := p_ProgressPer;
    finally
        p_ProgressCS.Leave;
        end;
    end;

//==============================================================================
procedure TFormProgress.SetProgressPar(const Value: Integer);
    begin
    p_ProgressCS.Enter;
    try
        p_ProgressPer := Value;
    finally
        p_ProgressCS.Leave;
        end;
    end;

//==============================================================================
procedure TFormProgress.TimerProgressTimer(Sender: TObject);
    begin
    if not self.Showing then
        Exit;

    if Assigned(FOnGetProgressValue) then
        ProgressPer := FOnGetProgressValue();

    ProgressBar1.Position := ProgressPer;
    end;

//==============================================================================
procedure TFormProgress.SetButtonString(const Value: String);
    begin
    Button1.Caption := Value;
    end;

//==============================================================================
procedure TFormProgress.SetMainMessage(const Value: String);
    begin
    Panel1.Caption := Value;
    end;

//==============================================================================
function TFormProgress.GetbuttonString: String;
    begin
    Result := Button1.Caption;
    end;

//==============================================================================
function TFormProgress.GetmainMessage: String;
    begin
    Result := Panel1.Caption;
    end;

end.
