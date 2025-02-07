unit MenngeCommunicateSettingForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.ExtCtrls, Vcl.StdCtrls;

type
    /// <summary>
    ///    �V�~�����[�V�����v�Z���̒ʐM�Ɋւ���ڍאݒ���s���t�H�[���Ƃ��̋@�\���`����N���X
    /// </summary>
    TFormMengeCommunicateSetting = class(TForm)
        GroupBox2: TGroupBox;
        Panel1: TPanel;
        PanelHostIpAddress: TPanel;
        LabelIpAddress: TLabel;
        editHostIp: TEdit;
        PanelHostPort: TPanel;
        LabelUDPPort: TLabel;
        editHostPort: TEdit;
        GridPanel6: TGridPanel;
        Panel2: TPanel;
        Label8: TLabel;
        Panel3: TPanel;
        ActionSelectMFJsonExportDir: TSpeedButton;
        EditSelectMENGEdir: TEdit;
        Panel4: TPanel;
        LabelMENGEProjectName: TLabel;
        EditMENGEProjectName: TEdit;
        PanelFutter: TPanel;
        BevelFutter: TBevel;
        ButtonCancel: TButton;
        ButtonOk: TButton;
        procedure ActionSelectMFJsonExportDirClick(Sender: TObject);
        private
        public
            procedure AfterConstruction; override;
            procedure SaveData;
    end;

implementation

uses
    PluginCore,
    Vcl.FileCtrl,
    mengeSimSetting;

{$R *.dfm}

{ TFormMengeCommunicateSetting }
//==============================================================================
procedure TFormMengeCommunicateSetting.AfterConstruction;
    var
        m_settings : mengeSimSettingType;
    begin
    inherited;
    m_settings :=LoadMengeSimSettingFromRegistry;
    editHostIp.Text := m_settings.Ip;
    editHostPort.Text := IntToStr(m_settings.Port);
    EditSelectMENGEdir.Text := m_settings.Directory;
    EditMENGEProjectName.Text := m_settings.ProjectName;
    end;

//==============================================================================
procedure TFormMengeCommunicateSetting.ActionSelectMFJsonExportDirClick(Sender: TObject);
    var
        SelectFolder: TArray<String>;
    begin
    if SelectDirectory(theApplicationServices.UserDirectory,
                       SelectFolder,
                       [],
                       '�o�͐�̎w��',
                       '�o�͐�t�H���_',
                       '�m��') then
        begin
        EditSelectMENGEdir.Text := SelectFolder[0];
        end;

    end;

//==============================================================================
procedure TFormMengeCommunicateSetting.SaveData;
    var
        m_settings : mengeSimSettingType;
    begin
    m_settings.Ip := editHostIp.Text;
    m_settings.Port := StrToIntDef(editHostPort.Text,43500);
    m_settings.Directory := EditSelectMENGEdir.Text;
    m_settings.ProjectName := EditMENGEProjectName.Text;
    SaveMengeSimSettingToRegistry(m_settings);
    end;

end.
