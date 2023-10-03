(*
 * IPWorks 2022 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworks
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit nntpreadf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ipwNNTP, ipwcore, ipwtypes, ComCtrls, StdCtrls, nntppostf, ExtCtrls;

type
  TFormNntpread = class(TForm)
    ipwNNTP1: TipwNNTP;
    EditGroup: TEdit;
    Label4: TLabel;
    Label2: TLabel;
    EditUser: TEdit;
    Label3: TLabel;
    EditPassword: TEdit;
    EditServer: TEdit;
    Label1: TLabel;
    Label5: TLabel;
    ListBoxMessage: TListBox;
    ListViewArticles: TListView;
    ButtonConnect: TButton;
    ButtonGo: TButton;
    ButtonCompose: TButton;
    ButtonReply: TButton;
    procedure ButtonGoClick(Sender: TObject);
    procedure ListViewArticlesClick(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonComposeClick(Sender: TObject);
    procedure ButtonReplyClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ipwNNTP1Header(Sender: TObject; const Field, Value: String);
    procedure ipwNNTP1Transfer(Sender: TObject; Direction: Integer;
      BytesTransferred: Int64; PercentDone: Integer; Text: string; TextB: TArray<System.Byte>;
      EOL: Boolean);
    procedure ipwNNTP1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
    procedure ipwNNTP1GroupOverview(Sender: TObject; ArticleNumber: Int64;
      const Subject, From, ArticleDate, MessageId, References: string;
      ArticleSize, ArticleLines: Int64; const OtherHeaders: string);
  private
    { Private declarations }
    ArticleID: String;
  public
    { Public declarations }
  end;

var
  FormNntpread: TFormNntpread;

implementation

{$R *.DFM}



procedure TFormNntpread.ButtonGoClick(Sender: TObject);
begin
// Here are some free news servers:
// news.uni-stuttgart.de
// liveupdate.com

   Screen.Cursor := crHourGlass;
   try
      ListViewArticles.Items.Clear();
      ipwNNTP1.CurrentGroup := EditGroup.Text;
      //we downlaod at most 100 articles
      If ipwNNTP1.LastArticle > ipwNNTP1.FirstArticle + 100 Then begin
        ipwNNTP1.OverviewRange := IntToStr(ipwNNTP1.LastArticle - 100) + '-' + IntToStr(ipwNNTP1.LastArticle);
      end;
      ipwNNTP1.GroupOverview();
   except on E: EipwNNTP do
      ShowMessage(E.Message);
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormNntpread.ListViewArticlesClick(Sender: TObject);
begin
// A message header was clicked.  Display the message text.
   Screen.Cursor := crHourGlass;
   try
      ListBoxMessage.Items.Clear();
      ipwNNTP1.CurrentArticle := ListViewArticles.Selected.Caption;
      ipwNNTP1.FetchArticle();
   except on E: EipwNNTP do
      ShowMessage(E.Message);
   end;
   Screen.Cursor := crDefault;

end;

procedure TFormNntpread.ButtonConnectClick(Sender: TObject);
begin
// Connect
   Screen.Cursor := crHourGlass;
   if ButtonConnect.Caption = 'Connect' then
   begin
      ipwNNTP1.NewsServer := EditServer.Text;
      ipwNNTP1.User := EditUser.Text;
      ipwNNTP1.Password := EditPassword.Text;
      ipwNNTP1.NewsPort := 119;
      ipwNNTP1.SSLStartMode := TipwnntpSSLStartModes(3);
      try
         ipwNNTP1.Connect();
      except on E: EipwNNTP do
         ShowMessage(E.Message);
      end;
      ButtonConnect.Caption := 'Disconnect';
      ButtonGo.Enabled := True;
      ButtonCompose.Enabled := True;
      ButtonReply.Enabled := True;
   end
   else
   begin
      try
         ipwNNTP1.Disconnect();
      except on E: EipwNNTP do
         ShowMessage(E.Message);
      end;
      ButtonConnect.Caption := 'Connect';
      ListViewArticles.Items.Clear();
      ListBoxMessage.Items.Clear();
      ButtonGo.Enabled := False;
      ButtonCompose.Enabled := False;
      ButtonReply.Enabled := False;
   end;
   Screen.Cursor := crDefault;
end;

procedure TFormNntpread.ButtonComposeClick(Sender: TObject);
begin
   Screen.Cursor := crHourGlass;
   FormNntppost.EditSubject.Text := '';
   FormNntppost.MemoMessage.Text := '';
   if FormNntppost.ShowModal() = mrOk then
   begin
      ipwNNTP1.ArticleFrom := FormNntppost.EditFrom.Text;
      ipwNNTP1.ArticleSubject := FormNntppost.EditSubject.Text;
      ipwNNTP1.ArticleText := FormNntppost.MemoMessage.Text;
      try
         ipwNNTP1.PostArticle();
      except on E: EipwNNTP do
         ShowMessage(E.Message);
      end;
   end;
   Screen.Cursor := crDefault;
end;

procedure TFormNntpread.ButtonReplyClick(Sender: TObject);
var
   i: integer;
begin
   Screen.Cursor := crHourGlass;
   FormNntppost.EditSubject.Text := 'Re: ' + ListViewArticles.Selected.SubItems.Strings[0];
   for i := 0 to (ListBoxMessage.Items.Count - 1) do
      FormNntppost.MemoMessage.Lines.Add( '> ' + ListBoxMessage.Items.Strings[i] );
   if FormNntppost.ShowModal() = mrOk then
   begin
      ipwNNTP1.ArticleFrom := FormNntppost.EditFrom.Text;
      ipwNNTP1.ArticleSubject := FormNntppost.EditSubject.Text;
      ipwNNTP1.ArticleReferences := '';
      ipwNNTP1.ArticleReferences := ArticleID;
      ipwNNTP1.ArticleText := FormNntppost.MemoMessage.Text;
      try
         ipwNNTP1.PostArticle();
      except on E: EipwNNTP do
         ShowMessage(E.Message);
      end;
   end;
   Screen.Cursor := crDefault;
end;

procedure TFormNntpread.FormResize(Sender: TObject);
begin
   ListViewArticles.Columns.Items[1].Width := ( ListViewArticles.Width - 50 ) div 3;
   ListViewArticles.Columns.Items[2].Width := ( ListViewArticles.Width - 50 ) div 3;
   ListViewArticles.Columns.Items[3].Width := ( ListViewArticles.Width - 50 ) div 3;
end;

procedure TFormNntpread.ipwNNTP1GroupOverview(Sender: TObject;
  ArticleNumber: Int64; const Subject, From, ArticleDate, MessageId,
  References: string; ArticleSize, ArticleLines: Int64;
  const OtherHeaders: string);
begin
   ListViewArticles.Items.Add();
   ListViewArticles.Items.Item[ListViewArticles.Items.Count - 1].Caption := IntToStr(ArticleNumber);
   ListViewArticles.Items.Item[ListViewArticles.Items.Count - 1].SubItems.Add(Subject);
   ListViewArticles.Items.Item[ListViewArticles.Items.Count - 1].SubItems.Add(From);
   ListViewArticles.Items.Item[ListViewArticles.Items.Count - 1].SubItems.Add(ArticleDate);
end;

procedure TFormNntpread.ipwNNTP1Header(Sender: TObject; const Field,
  Value: String);
begin

     if Field = 'Message-ID' then
     begin
          ArticleID := Value;
     end

end;

procedure TFormNntpread.ipwNNTP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:= true;
end;

procedure TFormNntpread.ipwNNTP1Transfer(Sender: TObject; Direction: Integer;
  BytesTransferred: Int64; PercentDone: Integer; Text: string; TextB: TArray<System.Byte>; EOL: Boolean);
begin
   ListBoxMessage.Items.Add(Text);
end;

end.
//---------------------------------------------------------------------------

