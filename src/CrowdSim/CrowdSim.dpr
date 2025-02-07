program CrowdSim;

uses
  Vcl.Forms,
  CrowdSimMainForm in 'CrowdSimMainForm.pas' {FormCrowdSimMain},
  MengeWrapper in 'MengeWrapper.pas',
  Vector2 in 'util\Vector2.pas',
  Vector3 in 'util\Vector3.pas',
  Agent in 'Agent.pas',
  Simulator in 'Simulator.pas',
  PacketRecord in 'data\PacketRecord.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormCrowdSimMain, FormCrowdSimMain);
  Application.Run;
end.
