# 環境構築手順書

## 1 本書について

本書は、人流シミュレーションプラグイン(UC-win/Roadプラグイン)の利用環境構築手順について記載しています。

> [!TIP]
> 本システムの構成や仕様の詳細については[技術検証レポート][TechnicalReport]も参考にしてください。

## 2 動作環境

本システムの動作環境は以下のとおりです。

|項目|最小動作環境|推奨動作環境|
| - | - | - |
|OS|Microsoft Windows10 / 11(64bit)|同左|
|CPU|IntelCorei5以上|同左|
|GPU|NVIDIA製GPU|NVIDIA Geforce GTX1650以上(4GB以上のメモリ)|
|メモリ|4GB以上|32GB以上|
|ストレージ|最低30GB以上の空き容量|SSDドライブ<br>最低60GB以上の空き容量|
|ディスプレイ解像度|1920×1080以上|同左|
|ネットワーク|必須|同左|

## 3 インストール手順

> [!Important]
> 本システムを実行するにはUC-win/Roadのライセンスが必要です。詳細は株式会社フォーラムエイトにお問い合わせ下さい。<br>
> [フォーラムエイト(HP)][Forum8HP]<br>
> [フォーラムエイト(サポートページ)][Forum8Support]

### 3-1 F8CrowdSimPlugin

1. [GitHubページ][CrowdSimGitHub]からUC-winRoad-CrowdSim-Plugin-v.0.1.0.zipをダウンロードします。
2. [GitHubページ][CrowdSimGitHub]からソースファイルをダウンロードします。
3. WindowsのエクスプローラでUC-win/Roadのデータディレクトリを開きます。
    > [!TIP]
    > データディレクトリの場所は、UC-win/Roadで確認できます。<br>
    > UC-win/Roadを起動して、メイン画面上部の「ファイル」タブ>>「アプリケーションオプション」>>「デフォルト設定」を押下します。<br>
    > 「アプリケーションデフォルト」画面が開くので、画面左側のタブから「フォルダ、ファイル関連」を選択します。「データディレクトリ」項目に記載されている場所を確認します。<br>
    > ![アプリケーションデフォルト画面][ApplicationDefaultForm]
4. データディレクトリ直下の「Plugins」フォルダを開きます(もし「Plugins」フォルダが無ければ作成してください)。![データディレクトリ_Plugins][DataDirectory_Plugins]
5. ダウンロードしたzipファイルを解凍し、「Plugins」フォルダ直下にF8CrowdSimPlugin.bplを設置します。
6. WindowsのエクスプローラでUC-win/Roadの実行ファイル(UCwinRoad.exe)があるフォルダを開きます。
    > [!TIP]
    > UC-win/Roadをインストールすると、デフォルト設定ではデスクトップにショートカットが作成されます。<br>
    > ショートカットを右クリックし、「ファイルの場所を開く」を選択するとUC-win/Roadの実行ファイル(UCwinRoad.exe)があるフォルダを開けます。<br>
    > ![UCwinRoad_ショートカット][UCwinRoad_Shortcut]
7. Shadersフォルダを開きます。
8. Pluginsフォルダを開きます。
9. ダウンロードしたソースファイルのsrc\F8CrowdSimPlugin\Shadersフォルダ内のCrowdSimMeshフォルダを前項のPluginsフォルダにコピーします。

### 3-2 CrowdSim

1. [GitHubページ][CrowdSimGitHub]からUC-winRoad-CrowdSim-Plugin-v.0.1.0.zipをダウンロードします。
2. WindowsのエクスプローラでUC-win/Roadの実行ファイル(UCwinRoad.exe)があるフォルダを開きます。
3. ダウンロードしたファイルを解凍し、前項で開いたフォルダ内にCrowdSim.exeを設置します。

UC-win/Roadをデフォルト設定でインストールし、[3-1](#3-インストール手順)と[3-2](#3-2-crowdsim)を手順どおりに進めた場合、最終的にフォルダ構成は次のようになります。

```bash
C:
├─Program Files
│  └─FORUM8
│      └─UCwinRoad 17.2 <------ UCwin/Roadの実行ファイルがあるフォルダ
│          ├─CrowdSim.exe <------ ダウンロードしたexeを設置(3-2)
│          └─shaders
│              └─Plugins
│                  └─CrowdSimMesh <------ ダウンロードしたフォルダを設置(3-1)
└─UCwinRoad Data 17.2 <------ データディレクトリ
│  └─Plugins
│      └─F8CrowdSimPlugin.bpl <------ ダウンロードしたbplを設置(3-1)
```

## 4 ビルド手順

> [!Important]
> F8CrowdSimPluginをビルドするにはUC-win/Road SDKが必要です。詳細は株式会社フォーラムエイトにお問い合わせ下さい。<br>
> [フォーラムエイト(HP)][Forum8HP]<br>
> [フォーラムエイト(サポートページ)][Forum8Support]
> [!Important]
> F8CrowdSimPlugin、CrowdSimをビルドするにはDelphi 10.4.2が必要です。IDE Patchesは全て適用済みの状態にして下さい。詳しくは[こちら][embarcadero]をご参照ください。<br>
ソースファイルからF8CrowdSimPlugin、CrowdSimを生成することができます。<br>
ソースファイルは[こちら][CrowdSimGitHub]からダウンロード可能です。<br>
srcフォルダ下にF8CrowdSimPlugin、CrowdSimフォルダがあります。

|フォルダ名|詳細|
| - | - |
| F8CrowdSimPlugin |F8CrowdSimPlugin.bplのソースコード|
| CrowdSim| CrowdSim.exeのソースコード |

### 4-1 F8CrowdSimPlugin

ソースファイルからF8CrowdSimPluginを生成することができます。<br>
ソースコードは[GitHub][CrowdSimGitHub]からダウンロードしたフォルダに含まれるsrcフォルダ内のF8CrowdSimPluginフォルダにあります。<br>
フォルダ構成は次のようになっています。

```bash
├─Importer
│  ├─CityGML
│  ├─MFJSON
│  └─TrafficSensor
├─PeopleFlowAnalysis
│  ├─Contour
│  ├─ExportsForm
│  ├─FlowLog
│  └─HeatMap
├─PeopleFlowSimulation
│  ├─Menge
│  │  ├─MengeUtils
│  │  └─XmlFormats
│  └─PedestrianMap
│      └─PedestrianMapUtil
├─PlayerForm
│  ├─imgs
│  └─PlayerFormFrame
└─Shaders
    └─CrowdSimMesh
        └─Sources
```

ビルド方法は次のとおりです。

1. UC-win/Road SDKのヘルプファイルに従って開発環境の初期設定を行います。
2. 本システムのプロジェクトファイル（F8CrowdSimPlugin.dproj）をDelphi 10.4.2で開きます。
3. プラグインの出力先を確認します。
   1. プロジェクト画面で「F8CrowdSimPlugin.bpl」を右クリックし、「オプション」を選択します。
   2. 「ビルド」-「Delphiコンパイラ」を開きます。
   3. 「ターゲット」を「すべての構成 - Windows64ビット プラットフォーム」に変更します。
   4. 「パッケージの出力ディレクトリ」が「(UC-wi/Roadのデータディレクトリ)\Plugins」になってない場合は変更して下さい。![Delphiプロジェクトオプション][delphiProjectOptionForm]
4. ビルド構成を「Release」、ターゲットプラットフォームを「Windows 64ビット」にしてビルドします。![F8CrowdSimPluginビルド時の構成][buildSetting_F8CrowdSimPlugin]
5. ビルドに成功すると、前項で確認した「パッケージの出力ディレクトリ」の場所に「F8CrowdSim.bpl」が出力されます。

#### 4-1-1 シェーダファイルの更新

Shadersフォルダ内のファイルを変更した場合は、インストール手順を参考にCrowdSimMeshフォルダを更新して下さい。

### 4-2 CrowdSim

ソースファイルからCrowdSimを生成することができます。<br>
ソースコードは[GitHub][CrowdSimGitHub]からダウンロードしたフォルダに含まれるsrcフォルダ内のCrowdSimフォルダにあります。<br>
フォルダ構成は次のようになっています。

```bash
├─data
├─util
```

ビルド方法は次のとおりです。

1. 本システムのプロジェクトファイル（CrowdSim. dproj）をDelphi 10.4.2で開きます。
2. ビルド構成を「Release」、ターゲットプラットフォームを「Windows64ビット」にしてビルドします。![CrowdSimビルド時の構成][buildSetting_CrowdSim]

次に、ビルドしたCrowdSimを使用できるようセットアップします。<br>セットアップ方法は次の通りです。

1. ビルドに成功すると、CrowdSimフォルダにWin64\Releaseフォルダが自動生成されるので、フォルダ内に移動します。
2. フォルダ内にある「CrowdSim.exe」をUC-win/Roadの実行ファイル(UCwinRoad.exe)があるフォルダにコピーします。
3. [MengeのGitHubページ][MENGEGitHub]からMengeのソースコードを取得し、同ページの記述に従ってビルドします。<br>ビルドすると、「MengeCore.dll」が生成されます。
4. 「MengeCore.dll」をUC-win/Roadの実行ファイル(UCwinRoad.exe)があるフォルダにコピーします。

## 5 準備物一覧

アプリケーションを利用するために以下のデータを入手します。<br>
データの入力方法については[操作マニュアル][UserMan]をご参照下さい。

|# | データ種別 | 機能| 用途| 入力方法 |
| - | - | - | - | - |
| 1| 3D都市モデル(CityGML)<br>[G空間情報センター][geospatial]から取得します。| 全般| 全般| 格納フォルダパス指定|
| 2 | 人流データ(MF-Json)<br>[人流データ変換ツール][MFJsonConverter]を使って作成します。| 3次元形状の物体の移動データ|人流シミュレーションの初期値|格納フォルダパス指定|

本システムでは、3D都市モデルの交通（道路）モデルLOD1～LOD3.3を活用します。
また、歩行エリア編集時の参考情報として建築物モデルの形状も利用しています。

| 地物| 地物型| 属性区分| 属性名| 内容|
| - | - | - | - | - |
| 建築物LOD1| bldg:Building| 空間属性| bldg:lod1Solid|建築物のLOD1の立体|
| 建築物LOD2| bldg:Building| 空間属性| bldg:lod2Solid|建築物のLOD2の立体|
| 道路LOD1| tran:Road| 空間属性| tran:lod1MultiSurface|道路のLOD1の形状|
| 道路LOD3. 3| tran:Road| 空間属性| tran:lod3MultiSurface|道路のLOD3の形状|

<!---GitHubページなどは確定次第修正します-->
<!--URL-->
[TechnicalReport]: https://www.mlit.go.jp/plateau/file/libraries/doc/plateau_tech_doc_0102_ver01.pdf
[Forum8HP]: https://www.forum8.co.jp/index.html
[Forum8Support]: https://www.forum8.co.jp/tech/tech.htm
[CrowdSimGitHub]: https://github.com/Project-PLATEAU/UC-winRoad-CrowdSim-Plugin
[geospatial]: https://front.geospatial.jp/
[MFJsonConverter]: https://github.com/Project-PLATEAU/MF-JSON-Converter
[MENGEGitHub]: https://github.com/MengeCrowdSim/Menge
[embarcadero]:https://blogs.embarcadero.com/ja/rad-studio-10-4-2patch-general-patchdelphi-compiler-patch-ja/
<!--画像-->
[ApplicationDefaultForm]: ../resources/devMan/applicationDefaultForm.png
[delphiProjectOptionForm]: ../resources/devMan/delphiProjectOption.png
[buildSetting_F8CrowdSimPlugin]: ../resources/devMan/BuildSettings_Plugin.png
[buildSetting_CrowdSim]: ../resources/devMan/BuildSettings_FrowdSim.png
[DataDirectory_Plugins]: ../resources/devMan/RoadData_Plugins.png
[UCwinRoad_Shortcut]: ../resources/devMan/UCwinRoad_shortcut.png
<!--MD-->
[UserMan]: userMan.md