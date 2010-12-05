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
    procedure FormDestroy(Sender: TObject);
  private
    FMediaWikiApi: TMediaWikiApi;
    procedure MediaWikiBlockDone(Sender: TMediaWikiApi; const BlockInfos: TMediaWikiBlockInfos);
    procedure MediaWikiBlockContinue(Sender: TMediaWikiApi; const Start: string);
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
  FMediaWikiApi.OnQueryBlockInfoContinue := MediaWikiBlockContinue;
  FMediaWikiApi.QueryInit;
  FMediaWikiApi.QueryBlockInfoAsync(0.0, 0.0, '', '', '', StrToInt(EditMaxBlocks.Text), EditStartBlock.Text, []);
  FMediaWikiApi.QueryExecuteAsync;
end;

procedure TMainForm.ButtonQuerySyncClick(Sender: TObject);
var
  BlockInfos: TMediaWikiBlockInfos;
  Index: Integer;
begin
  MemoResult.Lines.Clear;
  FMediaWikiApi.OnQueryBlockInfoDone := nil;
  FMediaWikiApi.OnQueryBlockInfoContinue := nil;
  FMediaWikiApi.QueryBlockInfo(BlockInfos, 0.0, 0.0, '', '', '', StrToInt(EditMaxBlocks.Text), EditStartBlock.Text, []);
  for Index := Low(BlockInfos) to High(BlockInfos) do
    MemoResult.Lines.Add(BlockInfos[Index].BlockUser);
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

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // logout is not required
  //FMediaWikiApi.Logout;
  FMediaWikiApi.Free;
end;

procedure TMainForm.MediaWikiBlockContinue(Sender: TMediaWikiApi;
  const Start: string);
begin
  EditStartBlock.Text := Start;
end;

procedure TMainForm.MediaWikiBlockDone(Sender: TMediaWikiApi;
  const BlockInfos: TMediaWikiBlockInfos);
var
  Index: Integer;
begin
  for Index := Low(BlockInfos) to High(BlockInfos) do
    MemoResult.Lines.Add(BlockInfos[Index].BlockUser);
end;

end.

