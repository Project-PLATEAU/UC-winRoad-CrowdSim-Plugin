unit F_MFJsonMultiLoader;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,Vcl.Buttons,
  MFJsonLoaderOptions;

type
    /// <summary>
    ///    MF-Jsonの読み込みを行うフレームとその機能を定義するクラス
    /// </summary>
    TFrameMfMultiLoader = class(TFrame)
        GroupBoxImportFiles: TGroupBox;
        ListBoxReadFiles: TListBox;
        Panel2: TPanel;
        ButtonDelete: TButton;
        ButtonAdd: TButton;
        Panel3: TPanel;
        Panel1: TPanel;
        OpenDialog: TOpenDialog;
        SpeedButtonDown: TSpeedButton;
        SpeedButtonUp: TSpeedButton;
    Label1: TLabel;

            procedure SpeedButtonUpClick(Sender: TObject);
            procedure SpeedButtonDownClick(Sender: TObject);
            procedure ButtonAddClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
        private

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function DoImportAllMFJson(option : MFJsonLoaderOptionsClass) : Boolean;
    end;

implementation

uses
    PluginCore,
    MFJsonLoader;

{$R *.dfm}

{ TFrame1 }
//==============================================================================
procedure TFrameMfMultiLoader.AfterConstruction;
    begin
    inherited;
    ListBoxReadFiles.Items.Clear;
    end;

//==============================================================================
procedure TFrameMfMultiLoader.BeforeDestruction;
    begin
    inherited;

    end;

//==============================================================================
procedure TFrameMfMultiLoader.ButtonAddClick(Sender: TObject);
    begin
    OpenDialog.DefaultExt := 'json';
    OpenDialog.Filter := 'MF-Json file (json)|*.json';
    if OpenDialog.Execute then
        begin
        ListBoxReadFiles.AddItem(OpenDialog.FileName,nil);
        end;
    end;

//==============================================================================
procedure TFrameMfMultiLoader.ButtonDeleteClick(Sender: TObject);
    begin
    if ListBoxReadFiles.Items.Count=0 then
        Exit;
    if ListBoxReadFiles.ItemIndex = -1 then
        Exit;
    ListBoxReadFiles.Items.Delete(ListBoxReadFiles.ItemIndex);
    end;

//==============================================================================
function TFrameMfMultiLoader.DoImportAllMFJson(option : MFJsonLoaderOptionsClass) : Boolean;
    var
        saveCursor  : TCursor;
        fn : String;
    begin
    Result := False;
    saveCursor := Screen.Cursor;
    try
        Screen.Cursor := crHourGlass;
        for fn in ListBoxReadFiles.Items  do
            begin
            ImportMFJson(theApplicationServices.project, fn, option);
            end;
    finally
        Screen.Cursor := saveCursor;
        end;
    if ListBoxReadFiles.Items.Count>0  then
        begin
        showmessage('読み込みを完了しました');
        Result := True;
        end;
    end;

//==============================================================================
procedure TFrameMfMultiLoader.SpeedButtonDownClick(Sender: TObject);
    begin
    if ListBoxReadFiles.Items.Count<=1 then
        Exit;
    if ListBoxReadFiles.ItemIndex = ListBoxReadFiles.Items.Count-1 then
        Exit;
    if ListBoxReadFiles.ItemIndex = -1 then
        Exit;
    ListBoxReadFiles.Items.Exchange(ListBoxReadFiles.ItemIndex,ListBoxReadFiles.ItemIndex+1)
    end;

//==============================================================================
procedure TFrameMfMultiLoader.SpeedButtonUpClick(Sender: TObject);
    begin
    if ListBoxReadFiles.Items.Count<=1 then
        Exit;
    if ListBoxReadFiles.ItemIndex = 0 then
        Exit;
    if ListBoxReadFiles.ItemIndex = -1 then
        Exit;
    ListBoxReadFiles.Items.Exchange(ListBoxReadFiles.ItemIndex,ListBoxReadFiles.ItemIndex-1)
    end;

end.
