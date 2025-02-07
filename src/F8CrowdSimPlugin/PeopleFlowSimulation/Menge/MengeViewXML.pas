unit MengeViewXML;

interface

uses
    System.Classes,
    XmlExporter;

type
    /// <summary>
    ///    Mengexmlファイルのうち、V.xmlファイルの出力処理を行うクラス
    /// </summary>
    MengeViewXmlClass = class(xmlExporterClass)
        private

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ExportXML(path,projectName : string);

    end;


implementation

uses
    System.SysUtils;

{ MengeBehaviorXmlClass }
//==============================================================================
procedure MengeViewXmlClass.AfterConstruction;
    begin
    inherited;

    end;

//==============================================================================
procedure MengeViewXmlClass.BeforeDestruction;
    begin
    inherited;

    end;

//==============================================================================
procedure MengeViewXmlClass.ExportXML(path, projectName: string);
    var
        xml : TStringList;
    begin
    xml := TStringList.Create;
    try
        xml.Add('<View width="640" height="480" z_up="1">');
        xml.Add('<Camera xpos="0.0" ypos="200.0" zpos="0.0" xtgt="0" ytgt="0" ztgt="0" far="600" near="0.01" fov="45"/>');
        xml.Add('<Light x="1" y="0" z="-1" type="directional" diffR="1.0" diffG="0.8" diffB="0.8" space="camera"/>');
        xml.Add('<Light x="0" y="1" z="0" type="directional" diffR="0.8" diffG="0.8" diffB="0.8" space="world"/>');
        //            xml.Add('<Watermark file_name="../../mengeLogo.png" alignment="bottom_right" scale="0.5" opacity="0.25"/>');
        xml.Add('</View>');
        xml.SaveToFile(path + '\' + projectName + 'V.xml');
    finally
        FreeAndNil(xml);
        end;
    end;

end.
