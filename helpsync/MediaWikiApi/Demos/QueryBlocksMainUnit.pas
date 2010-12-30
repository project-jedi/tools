unit QueryBlocksMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  MediaWikiUtils,
  MediaWikiApi;

type
  TMainForm = class(TForm)
    MemoResult: TMemo;
    ButtonQueryAsync: TButton;
    ButtonQuerySync: TButton;
    EditMaxBlocks: TEdit;
    LabelMaxBlocks: TLabel;
    EditStartBlock: TEdit;
    LabelStartBlock: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuerySyncClick(Sender: TObject);
    procedure ButtonQueryAsyncClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMediaWikiApi: TMediaWikiApi;
    FContinueInfo: TMediaWikiContinueInfo;
    procedure MediaWikiBlockDone(Sender: TMediaWikiApi; const BlockInfos: TMediaWikiBlockInfos;
      const ContinueInfo: TMediaWikiContinueInfo);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonQueryAsyncClick(Sender: TObject);
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryBlockInfoDone := MediaWikiBlockDone;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryBlockInfoAsync(FContinueInfo, 0.0, 0.0, '', '', '', StrToInt(EditMaxBlocks.Text), []);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  BlockInfos: TMediaWikiBlockInfos;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryBlockInfoDone := nil;
  FMediaWikiApi.QueryBlockInfo(BlockInfos, FContinueInfo, 0.0, 0.0, '', '', '', StrToInt(EditMaxBlocks.Text), []);
  for Index := Low(BlockInfos) to High(BlockInfos) do
    MemoResult.Lines.Add(BlockInfos[Index].BlockUser);
  EditStartBlock.Text := FContinueInfo.ParameterValue;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  // logout is not required
  //FMediaWikiApi.Logout;
  FMediaWikiApi.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMediaWikiApi := TMediaWikiApi.Create;
  FMediaWikiApi.URL := 'http://wiki.delphi-jedi.org/api.php';
  FMediaWikiApi.UserAgent := 'MediaWiki JEDI bot';
  FMediaWikiApi.FollowRelocation := False;
  // login is not mandatory
  //FMediaWikiApi.Login()
end;

procedure TMainForm.MediaWikiBlockDone(Sender: TMediaWikiApi;
  const BlockInfos: TMediaWikiBlockInfos; const ContinueInfo: TMediaWikiContinueInfo);
var
  Index: Integer;
begin
  for Index := Low(BlockInfos) to High(BlockInfos) do
    MemoResult.Lines.Add(BlockInfos[Index].BlockUser);
  FContinueInfo := ContinueInfo;
  EditStartBlock.Text := FContinueInfo.ParameterValue;
end;

end.

