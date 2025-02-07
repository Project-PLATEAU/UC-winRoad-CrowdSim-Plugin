unit mengeSimSetting;

interface

uses
    PluginCore,
    Environment;

type
    mengeSimSettingType = record
        IP : String;
        Port : Integer;
        Directory : String;
        ProjectName : String;
    end;
const
    CROWDSIM_REGISTRY_PATH = 'F8CrowdSimPlugin';
    MENGE_IP_KEY           = 'MengeSimIp';
    MENGE_PORT_KEY         = 'MengeSimPort';
    MENGE_PROJECT_KEY      = 'MengeSimProjectName';
    MENGE_DIRECTORY_KEY    = 'MengeSimDirectory';

procedure SaveMengeSimSettingToRegistry(setting : mengeSimSettingType);
function  LoadMengeSimSettingFromRegistry : mengeSimSettingType;

implementation

uses
    System.SysUtils;

//==============================================================================
procedure SaveMengeSimSettingToRegistry(setting : mengeSimSettingType);
    begin
    globalEnvironment.WriteStringTo(CROWDSIM_REGISTRY_PATH,'',MENGE_IP_KEY,setting.IP);
    globalEnvironment.WriteStringTo(CROWDSIM_REGISTRY_PATH,'',MENGE_PORT_KEY,IntToSTr(setting.Port));
    globalEnvironment.WriteStringTo(CROWDSIM_REGISTRY_PATH,'',MENGE_PROJECT_KEY,setting.ProjectName);
    globalEnvironment.WriteStringTo(CROWDSIM_REGISTRY_PATH,'',MENGE_DIRECTORY_KEY,setting.Directory);
    end;

//==============================================================================
function  LoadMengeSimSettingFromRegistry : mengeSimSettingType;
    var
        defDir : String;
    begin
    defDir := theApplicationServices.GetUserDirectory+'\Data';

    Result.Ip := globalEnvironment.ReadStringFrom(CROWDSIM_REGISTRY_PATH,'',MENGE_IP_KEY,'127.0.0.1');
    Result.Port := StrToIntDef(globalEnvironment.ReadStringFrom(CROWDSIM_REGISTRY_PATH,'',MENGE_PORT_KEY,'43500'),43500);
    Result.ProjectName := globalEnvironment.ReadStringFrom(CROWDSIM_REGISTRY_PATH,'',MENGE_PROJECT_KEY,'mengeData');
    Result.Directory := globalEnvironment.ReadStringFrom(CROWDSIM_REGISTRY_PATH,'',MENGE_DIRECTORY_KEY,defDir);
    end;
end.
