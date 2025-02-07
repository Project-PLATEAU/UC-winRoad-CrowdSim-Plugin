unit MFJsonExportHelper;

interface

uses
    System.SysUtils,
    System.DateUtils,
    System.Types,
    System.Classes,
    System.JSON,
    System.JSON.Types,
    System.JSON.Readers,
    System.JSON.Builders,
    System.JSON.Serializers,
    System.Generics.Collections,
    System.Math,
    TemporalProperty,
    PluginCore,
    F8Utils,
    MFJsonLoaderOptions,
    MovingFeature,
    MFJsonExportUtils;

type
    /// <summary>
    ///    シミュレーション計算結果に基づいてMF-Jsonを出力するクラス
    /// </summary>
    TMFJsonExportHelper = class
        private
            p_HoriConv : IF8WrHorizontalCoordinateConvertor;
            function  SetExportData(const MFData: TObjectList<MovingFeatureClass>; InDataMF: integer; expInterval: double;
                var ExpData: TArray<TMFJsonExportData>; ExpIdx: integer; Serializer: TJsonSerializer): string;
            function  SetExportDataNoAgeAndGender(const MFData: TObjectList<MovingFeatureClass>; InDataMF: integer; expInterval: double;
                var ExpData: TArray<TMFJsonExportDataNoAgeAndGender>; ExpIdx: integer; Serializer: TJsonSerializer): string;
            procedure SetProperties(const MFProp: TProperties; var ExportProp: TExportProperties);
            procedure SetPropertiesNoAgeAndGender(const MFProp: TProperties; var ExportProp: TExportPropertiesNoAgeAndGender);
            procedure SettemporalGeometry(const MF: MovingFeatureClass; expInterval: double; var ExporttmpG: TExporttemporalGeometry);
        public
            function SetMFJsonData(const api: IF8ApplicationServices; MFData: TObjectList<MovingFeatureClass>; expInterval: double; var Serializer: TJsonSerializer): string;
        end;

implementation

uses
    AgentSettings;

{ TMFJsonExportHelper }
//==============================================================================
function TMFJsonExportHelper.SetMFJsonData(const api: IF8ApplicationServices; MFData: TObjectList<MovingFeatureClass>; expInterval: double; var Serializer: TJsonSerializer): string;
    var
        i          : integer;
        InDataMF   : integer;
        ExportIdx  : integer;
        ExportData               : TArray<TMFJsonExportData>;
        ExportDataNoAgeAndGender : TArray<TMFJsonExportDataNoAgeAndGender>;
    begin
    InDataMF  := 0;
    ExportIdx := 0;
    p_HoriConv := api.GetWRCoordinateConvertor.HoirizontalCSConvertor;
    Serializer.Formatting := TJsonFormatting.Indented;
    if MFData.Count < 1 then
        Exit;

    for i := 0 to MFData.Count - 1 do
        begin
        if MFData[i].numberOfFocusedHistories > 0 then
            InDataMF := InDataMF + 1;
        end;

    if (MFData[0].properties.age = '') and (MFData[0].properties.gender = '') then
        Result := SetExportDataNoAgeAndGender(MFData, InDataMF, expInterval, ExportDataNoAgeAndGender, ExportIdx, Serializer)
    else
        Result := SetExportData(MFData, InDataMF, expInterval, ExportData, ExportIdx, Serializer);
    end;

//==============================================================================
function TMFJsonExportHelper.SetExportData(const MFData: TObjectList<MovingFeatureClass>; InDataMF: integer; expInterval: double;
    var ExpData: TArray<TMFJsonExportData>; ExpIdx: integer; Serializer: TJsonSerializer): string;
    var
        i : integer;
    begin
    SetLength(ExpData, InDataMF);
    for i := 0 to MFData.Count - 1 do
        begin
        if MFData[i].numberOfFocusedHistories > 0 then
            begin
            SetProperties(MFData[i].properties, ExpData[ExpIdx].properties);
            ExpData[ExpIdx]._type := 'MovingFeatures';
            SettemporalGeometry(MFData[i], expInterval, ExpData[ExpIdx].temporalGeometry);
            ExpIdx := ExpIdx + 1;
            end;

        if ExpIdx = InDataMF then
            Break;
        end;

    Result := Serializer.Serialize<TArray<TMFJsonExportData>>(ExpData);
    end;

//==============================================================================
function TMFJsonExportHelper.SetExportDataNoAgeAndGender(const MFData: TObjectList<MovingFeatureClass>; InDataMF: integer; expInterval: double;
    var ExpData: TArray<TMFJsonExportDataNoAgeAndGender>; ExpIdx: integer; Serializer: TJsonSerializer): string;
    var
        i : integer;
    begin
    SetLength(ExpData, InDataMF);
    for i := 0 to MFData.Count - 1 do
        begin
        if MFData[i].numberOfFocusedHistories > 0 then
            begin
            SetPropertiesNoAgeAndGender(MFData[i].properties, ExpData[ExpIdx].properties);
            ExpData[ExpIdx]._type := 'MovingFeatures';
            SettemporalGeometry(MFData[i], expInterval, ExpData[ExpIdx].temporalGeometry);
            ExpIdx := ExpIdx + 1;
            end;

        if ExpIdx = InDataMF then
            Break;
        end;

    Result := Serializer.Serialize<TArray<TMFJsonExportDataNoAgeAndGender>>(ExpData);
    end;

//==============================================================================
procedure TMFJsonExportHelper.SetProperties(const MFProp: TProperties; var ExportProp: TExportProperties);
    var
        AgentSetting : TAgentSettingsType;
    begin
    ExportProp.name := MFProp.name;
    AgentSetting := LoadAgentSettings;
    if AgentSetting.IsRain then
        ExportProp.weather := 'rainny'
    else
        ExportProp.weather := 'sunny';

    if MFProp.gender <> '' then
        ExportProp.gender := MFProp.gender;

    if MFProp.age <> '' then
        ExportProp.age := MFProp.age;
    end;

//==============================================================================
procedure TMFJsonExportHelper.SetPropertiesNoAgeAndGender(const MFProp: TProperties; var ExportProp: TExportPropertiesNoAgeAndGender);
    var
        AgentSetting : TAgentSettingsType;
    begin
    ExportProp.name := MFProp.name;
    AgentSetting := LoadAgentSettings;
    if AgentSetting.IsRain then
        ExportProp.weather := 'rainny'
    else
        ExportProp.weather := 'sunny';
    end;

//==============================================================================
procedure TMFJsonExportHelper.SettemporalGeometry(const MF: MovingFeatureClass; expInterval: double; var ExporttmpG: TExporttemporalGeometry);
    const
        CALC_INTERVAL = 10;

    //--------------------------------------------------------------------------
    function IsOnInterval(const idx, interval: integer): boolean;
        begin
        Result := (((idx + 1) mod (interval * CALC_INTERVAL)) = 0);
        end;

    //--------------------------------------------------------------------------
    procedure ConvertAndSet(const aMF: MovingFeatureClass; ExpIdx, MFIdx: integer; var exptmpG: TExporttemporalGeometry);
        var
            GLCoord, LatLon　: F8PointType;
        begin
        GLCoord[_x] := aMF.focusedHistory[MFIdx].position[_x];
        GLCoord[_y] := theApplicationServices.DistanceNorth-aMF.focusedHistory[MFIdx].position[_z];
        p_HoriConv.Convert(_hctOpenGL_XZ, 0, _hctWGS84_LonLat, 6668, GLCoord, LatLon);
        exptmpG.coordinates[ExpIdx][0] := LatLon[_y];
        exptmpG.coordinates[ExpIdx][1] := LatLon[_x];
        exptmpG.datetimes[ExpIdx]      := DateToISO8601(aMF.focusedHistory[MFIdx].time);
        end;

    //--------------------------------------------------------------------------
    procedure SetPositionAndTimeByInterval(const aMF: MovingFeatureClass; interval: double; var exptmpG: TExporttemporalGeometry);
        var
            i, ExportIdx : integer;
        begin
        SetLength(exptmpG.coordinates, Trunc(aMF.numberOfFocusedHistories / (interval * CALC_INTERVAL)) + 1);
        SetLength(exptmpG.datetimes, Trunc(aMF.numberOfFocusedHistories / (interval * CALC_INTERVAL)) + 1);
        for i := 0 to MF.numberOfFocusedHistories - 2 do
            begin
            if (i = 0) or IsOnInterval(i, Trunc(interval)) then
                begin
                ExportIdx   := Trunc((i + 1) / (interval * CALC_INTERVAL));
                ConvertAndSet(aMF, ExportIdx, i, exptmpG);
                end;
            end;

        ConvertAndSet(aMF, Length(exptmpG.coordinates) - 1, MF.numberOfFocusedHistories - 1, exptmpG);
        end;

    //--------------------------------------------------------------------------
    procedure SetAllPositionAndTime(const aMF: MovingFeatureClass; var exptmpG: TExporttemporalGeometry);
        var
            i : integer;
        begin
        SetLength(exptmpG.coordinates, aMF.numberOfFocusedHistories);
        SetLength(exptmpG.datetimes, aMF.numberOfFocusedHistories);
        for i := 0 to MF.numberOfFocusedHistories - 1 do
            ConvertAndSet(aMF, i, i, exptmpG);
        end;

    //--------------------------------------------------------------------------
    begin
    ExporttmpG.interpolations := MF.temporalGeometry.interpolations;
    ExporttmpG._type          := MF.temporalGeometry.atype;
    if Trunc(expInterval) <> 0 then
        SetPositionAndTimeByInterval(MF, expInterval, ExporttmpG)
    else
        SetAllPositionAndTime(MF, ExporttmpG);
    end;
end.
