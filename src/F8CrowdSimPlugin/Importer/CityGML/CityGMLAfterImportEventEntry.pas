unit CityGMLAfterImportEventEntry;

interface

uses
    PluginCore,
    cityGMLBase,
    transportation,
    TranRoadList;

type
    /// <summary>
    ///    CityGMLプラグインがCityGMLファイルから都市モデルを読み込んだ後にイベントを実行するクラス
    ///    イベントでは道路情報リストの更新を行う
    /// </summary>
    TCityGMLPluginAfterImportEventEntry = class
        private
            p_API: IF8ApplicationServices;
            p_TranRoadList: TTranRoadList;

            procedure OnAfterImportRoad(const aObj: IXMLAbstractCityObjectType);
        public
            constructor Create(const aAPI: IF8ApplicationServices; const aList: TTranRoadList);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
        end;

implementation

{ TCityGMLPluginAfterImportEventEntry }

constructor TCityGMLPluginAfterImportEventEntry.Create(const aAPI: IF8ApplicationServices; const aList: TTranRoadList);
    begin
    Assert(Assigned(aAPI) and Assigned(aList));

    p_API := aAPI;
    p_TranRoadList := aList;
    end;

procedure TCityGMLPluginAfterImportEventEntry.AfterConstruction;
    var
        cityGMLPlugin: IF8CityGMLPlugin;
    begin
    inherited;

    if p_API.GetPlugInThatSupports(IF8CityGMLPlugin, cityGMLPlugin) then
        cityGMLPlugin.RegisterOnAfterImportRoadEvent(OnAfterImportRoad);
    end;

procedure TCityGMLPluginAfterImportEventEntry.BeforeDestruction;
    var
        cityGMLPlugin: IF8CityGMLPlugin;
    begin
    inherited;

    if p_API.GetPlugInThatSupports(IF8CityGMLPlugin, cityGMLPlugin) then
        cityGMLPlugin.UnRegisterOnAfterImportRoadEvent(OnAfterImportRoad);

    p_TranRoadList := nil;
    end;

procedure TCityGMLPluginAfterImportEventEntry.OnAfterImportRoad(const aObj: IXMLAbstractCityObjectType);
    begin
    p_TranRoadList.OnAfterImportRoad(aObj);
    end;
end.
