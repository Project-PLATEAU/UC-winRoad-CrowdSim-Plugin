unit AgentDetailSettingForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  PluginCore,Environment,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Samples.Spin, F8RealSpinEdit, Vcl.Buttons;

type
    /// <summary>
    ///    Agentの詳細設定を行うフォームとその機能を定義するクラス
    /// </summary>
    TFormAgentDetailSetting = class(TForm)
        PanelFutter: TPanel;
        BevelFutter: TBevel;
        ButtonCancel: TButton;
        ButtonOk: TButton;
        GridPanel1: TGridPanel;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        EditPedestrainRadiusSunny: TF8RealSpinEdit;
        EditMaxWalkSpeed: TF8RealSpinEdit;
        EditSimPerPedestrainData: TF8RealSpinEdit;
        Label4: TLabel;
        F8RealSpinEdit1: TF8RealSpinEdit;
        GroupBox1: TGroupBox;
        GridPanel3: TGridPanel;
        Label5: TLabel;
        CheckBoxMagnificationFactor: TCheckBox;
        GridPanel5: TGridPanel;
        Label7: TLabel;
        RadioButtonMgUU: TRadioButton;
        RadioButtonMgValue: TRadioButton;
        EditPopulation: TF8RealSpinEdit;
        EditUU: TF8RealSpinEdit;
        EditRealMg: TF8RealSpinEdit;
        GridPanel2: TGridPanel;
        GridPanel4: TGridPanel;
        Label6: TLabel;
        EditPedestrainRadiusRainny: TF8RealSpinEdit;
        RadioButtonMagCrossSection: TRadioButton;

            procedure CheckBoxMagnificationFactorClick(Sender: TObject);
            procedure RadioMagnificationClick(Sender: TObject);
        private
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure SaveData;

    end;

implementation

uses
    Vcl.FileCtrl,
    AgentSettings,mengeSimSetting;

{$R *.dfm}

{ TFormAgentDetailSetting }
//==============================================================================
procedure TFormAgentDetailSetting.AfterConstruction;
    var
        a_settings : TAgentSettingsType;
    begin
    inherited;
    a_settings := LoadAgentSettings;

    EditSimPerPedestrainData.Value := a_settings.simPerPedestrainData;
    EditPedestrainRadiusSunny.Value := a_settings.pedestrainRadiusInSunny;
    EditPedestrainRadiusRainny.Value := a_settings.pedestrainRadiusInRainny;
    EditMaxWalkSpeed.Value := a_settings.maxWalkSpeed;

    case MangnificationType(a_settings.UseMagnificationType) of
        Population:
            begin
            RadioButtonMgUU.Checked := True;
            EditPopulation.Enabled := true and a_settings.IsUseMagnificationFactor;
            EditUU.Enabled := true and a_settings.IsUseMagnificationFactor;
            EditRealMg.Enabled := false;
            end;
        RealUU:
            begin
            RadioButtonMgValue.Checked := True;
            EditPopulation.Enabled := false;
            EditUU.Enabled := false;
            EditRealMg.Enabled := true and a_settings.IsUseMagnificationFactor;
            end;
        CrossTraffic:
            begin
            RadioButtonMagCrossSection.Checked := True;
            end;
    end;

    EditPopulation.Value := a_settings.PopulationValue;
    EditUU.Value         := a_settings.UUValue;
    EditRealMg.Value     := a_settings.RealMagnification;

    CheckBoxMagnificationFactor.Checked := a_settings.IsUseMagnificationFactor;
    RadioButtonMgUU.Enabled := a_settings.IsUseMagnificationFactor;
    RadioButtonMgValue.Enabled := a_settings.IsUseMagnificationFactor;
    RadioButtonMagCrossSection.Enabled := a_settings.IsUseMagnificationFactor;
    end;

//==============================================================================
procedure TFormAgentDetailSetting.BeforeDestruction;
    begin
    inherited;

    end;

//==============================================================================
procedure TFormAgentDetailSetting.SaveData;
    var
        a_settings : TAgentSettingsType;
    begin
    a_settings.Initialize;

    a_settings.simPerPedestrainData     := Trunc(EditSimPerPedestrainData.Value);
    a_settings.pedestrainRadiusInSunny  := EditPedestrainRadiusSunny.Value;
    a_settings.pedestrainRadiusInRainny := EditPedestrainRadiusRainny.Value;
    a_settings.maxWalkSpeed             := EditMaxWalkSpeed.Value;

    a_settings.IsUseMagnificationFactor := CheckBoxMagnificationFactor.Checked;

    if RadioButtonMgUU.Checked then
        a_settings.UseMagnificationType := integer(Population)
    else if RadioButtonMgValue.Checked then
        a_settings.UseMagnificationType := integer(RealUU)
    else
        a_settings.UseMagnificationType := integer(CrossTraffic);

    a_settings.PopulationValue := Trunc(EditPopulation.Value);
    a_settings.UUValue := Trunc(EditUU.Value);
    a_settings.RealMagnification := Trunc(EditRealMg.Value);

    SaveAgentSettings(a_settings);
    end;

//==============================================================================
procedure TFormAgentDetailSetting.CheckBoxMagnificationFactorClick(Sender: TObject);
    var
        use : Boolean;
    begin
    use := CheckBoxMagnificationFactor.checked;

    RadioButtonMgUU.Enabled := use;
    RadioButtonMgValue.Enabled := use;
    if RadioButtonMgUU.Checked then
        begin
        EditPopulation.Enabled := true;
        EditUU.Enabled := true;
        EditRealMg.Enabled := false;
        end
    else if RadioButtonMgValue.Checked then
        begin
        EditPopulation.Enabled := false;
        EditUU.Enabled := false;
        EditRealMg.Enabled := true;
        end;
    end;

//==============================================================================
procedure TFormAgentDetailSetting.RadioMagnificationClick(Sender: TObject);
    begin
     if RadioButtonMgUU.Checked then
        begin
        EditPopulation.Enabled := true;
        EditUU.Enabled := true;
        EditRealMg.Enabled := false;
        end
    else if RadioButtonMgValue.Checked then
        begin
        EditPopulation.Enabled := false;
        EditUU.Enabled := false;
        EditRealMg.Enabled := true;
        end;
    end;

end.
