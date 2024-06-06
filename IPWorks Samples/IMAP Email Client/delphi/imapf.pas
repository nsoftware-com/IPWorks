(*
 * IPWorks 2024 Delphi Edition - Sample Project
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
unit imapf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ipwMIME, ipwSMTP, ipwcore, ipwtypes, ipwIMAP, StdCtrls, ExtCtrls, ComCtrls;

type
  GetMode = ( HEADERS_, TEXT_ );
  TFormImap = class(TForm)
    ipwIMAP1: TipwIMAP;
    ipwSMTP1: TipwSMTP;
    ipwMIME1: TipwMIME;
    ListViewMessages: TListView;
    ListBoxMessage: TListBox;
    TreeViewMailboxes: TTreeView;
    ButtonCompose: TButton;
    ButtonLogin: TButton;
    Label5: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    procedure ButtonLoginClick(Sender: TObject);
    procedure ipwIMAP1MailboxList(Sender: TObject; const Mailbox,
      Separator, Flags: String);
    procedure TreeViewMailboxesClick(Sender: TObject);
    procedure ListViewMessagesClick(Sender: TObject);

    procedure ButtonComposeClick(Sender: TObject);
    procedure ipwIMAP1MessageInfo(Sender: TObject; const MessageId,
      Subject, MessageDate, From, Flags: String; Size: Int64);
    procedure ipwIMAP1Transfer(Sender: TObject; Direction: Integer;
      BytesTransferred: Int64; PercentDone: Integer; const Text: string);
    procedure ipwSMTP1SSLServerAuthentication(Sender: TObject;
      const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
    procedure ipwIMAP1SSLServerAuthentication(Sender: TObject;
      const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
    procedure ipwIMAP1MessagePart(Sender: TObject; const PartId: string;
      Size: Int64; const ContentType, FileName, ContentEncoding, Parameters,
      MultipartMode, ContentId, ContentDisposition: string);
  private
    { Private declarations }
    head_: TTreeNode;
    sep: String;
    textpart: String;
    state: GetMode;
  public
    { Public declarations }
    constructor Create( Owner: TComponent );  override;
    function treesearch( Node: TTreeNode; text: String): TTreeNode;
    procedure buildtree( Node: TTreeNode; lstr: String; rstr: String);
    function mailboxname( Node: TTreeNode ): String;
  end;

var
  FormImap: TFormImap;

implementation

uses loginf, composef;

{$R *.DFM}

constructor TFormImap.Create( Owner: TComponent );
begin
   inherited Create(Owner);
   head_ := TreeViewMailboxes.Items.Add( nil, 'Folders');
   textpart := '0';
end;


procedure TFormImap.ButtonLoginClick(Sender: TObject);
begin
// Login / Logout
   Screen.Cursor := crHourGlass;
   if ButtonLogin.Caption = 'Login' then
   begin
      if FormLogin.ShowModal() = mrOk then
      begin
         ipwIMAP1.MailServer := FormLogin.EditIMAPServer.Text;
         ipwSMTP1.MailServer := FormLogin.EditSMTPServer.Text;
         ipwIMAP1.User := FormLogin.EditUser.Text;
         ipwIMAP1.Password := FormLogin.EditPassword.Text;
         try
            ipwIMAP1.Connect;
            ipwIMAP1.Mailbox := '*';
            ipwIMAP1.ListMailboxes;
         except on E: EIPWorks do
            ShowMessage(E.Message);
         end;
         ButtonLogin.Caption := 'Logout';
         ButtonCompose.Enabled := true;
      end;
   end
   else
   begin
      try
         ipwIMAP1.disconnect();
      except on E: EIPWorks do
         ShowMessage(E.Message);
      end;
      TreeViewMailboxes.Items.Clear();
      ListViewMessages.Items.Clear();
      ListBoxMessage.Items.Clear();
      ButtonLogin.Caption := 'Login';
      head_ := TreeViewMailboxes.Items.Add( nil, 'Folders' );
      ButtonCompose.Enabled := false;
   end;
   Screen.Cursor := crDefault;
end;


function TFormImap.treesearch( Node: TTreeNode; text: String): TTreeNode;
var
   key: TTreeNode;
   i: integer;
begin
   key := nil;
   if Node.Text = text then
      Result := Node
   else if Node.Count = 0 then
      Result := nil
   else
   begin
      for i := 0 to Node.Count - 1 do
         if key = nil then
            key := treesearch(Node.Item[i], text);
      Result := key;
   end;
end;


procedure TFormImap.buildtree( Node: TTreeNode; lstr: String; rstr: String);
var
   key: TTreeNode;
begin
   key := treesearch(node, lstr);
   if key = nil then
      key := TreeViewMailboxes.Items.AddChild(node, lstr);

   lstr := rstr;
   if Pos(sep, lstr) = 0 then
      rstr := ''
   else
   begin
      rstr := lstr;
      Delete( lstr, Pos(sep, lstr), Length(lstr) );
      Delete( rstr, 1, Length(lstr) + Length(sep) );
   end;

   if Length(lstr) > 0 then
      buildtree(key, lstr, rstr);

end;


function TFormImap.mailboxname( Node: TTreeNode ): String;
begin
  if node.Parent.Level <> 0 then
     Result := mailboxname(node.Parent) + sep + node.Text
  else
     Result := node.Text;
end;


procedure TFormImap.ipwIMAP1MailboxList(Sender: TObject; const Mailbox,
  Separator, Flags: String);
var
   ltmp: String;
   rtmp: String;

begin

   sep := Separator;
   ltmp := Mailbox;

   if Pos(sep, ltmp) = 0 then
      rtmp := ''
   else
   begin
      rtmp := ltmp;
      Delete( ltmp, Pos(sep, ltmp), Length(ltmp) );
      Delete( rtmp, 1, Length(ltmp) + Length(sep) );
   end;

   buildtree(head_, ltmp, rtmp);

end;



procedure TFormImap.TreeViewMailboxesClick(Sender: TObject);
begin
// Click the tree
   Screen.Cursor := crHourGlass;
   try
      if TreeViewMailboxes.Selected.Level > 0 then
      begin
         ipwIMAP1.Mailbox := '"' + mailboxname(TreeViewMailboxes.Selected) + '"';
         ipwIMAP1.SelectMailbox();
         ListViewMessages.Items.Clear();
         if ipwIMAP1.MessageCount > 0 then
         begin
            ipwIMAP1.MessageSet := '1:' + IntToStr(ipwIMAP1.MessageCount);
            state := HEADERS_;
            ipwIMAP1.RetrieveMessageInfo();
         end;
      end;
   except on E: EIPWorks do
      ShowMessage(E.Message);
   end;
   Screen.Cursor := crDefault;
end;



procedure TFormImap.ListViewMessagesClick(Sender: TObject);
begin
// Select a message
   Screen.Cursor := crHourGlass;
   try
      ipwIMAP1.MessageSet := ListViewMessages.Selected.Caption;
      ListBoxMessage.Items.Clear();
      state := TEXT_;
      ipwIMAP1.RetrieveMessageInfo();
      if textpart = '0' then
         ipwIMAP1.RetrieveMessageText
      else
      begin
         ipwIMAP1.RetrieveMessagePart(textpart);
         ListBoxMessage.Items.Text := ipwIMAP1.MessageText;
         textpart := '0';
      end;
      Label3.Caption := ListViewMessages.Selected.SubItems.Strings[0];
      Label4.Caption := ListViewMessages.Selected.SubItems.Strings[1];
   except on E: EIPWorks do
      ShowMessage(E.Message);
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormImap.ipwIMAP1SSLServerAuthentication(Sender: TObject;
  const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
  CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormImap.ipwIMAP1Transfer(Sender: TObject; Direction: Integer;
  BytesTransferred: Int64; PercentDone: Integer; const Text: string);
begin
  ListBoxMessage.Items.Add(Text);
end;

procedure TFormImap.ipwSMTP1SSLServerAuthentication(Sender: TObject;
  const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
  CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept:= true;
end;

procedure TFormImap.ButtonComposeClick(Sender: TObject);
var
   i: integer;
begin
// Compose
   Screen.Cursor := crHourGlass;
   if FormCompose.ShowModal() = mrOk then
   begin
      ipwSMTP1.ResetHeaders();
      ipwSMTP1.From := FormCompose.EditFrom.Text;
      ipwSMTP1.SendTo:= FormCompose.EditTo.Text;
      ipwSMTP1.Cc := FormCompose.EditCc.Text;
      ipwSMTP1.Subject := FormCompose.EditSubject.Text;

      if FormCompose.ComboBoxAttachments.Items.Count <> 0 then
      begin
         //reset data
         ipwMIME1.ResetData;

         //set PartCount.  Part 0 is the plain text of the message
         ipwMIME1.PartCount := FormCompose.ComboBoxAttachments.Items.Count + 1;
         ipwMIME1.PartDecodedString[0] := FormCompose.MemoMessage.Text;

         //add attachments
         for i := 1 to FormCompose.ComboBoxAttachments.Items.Count do
            ipwMIME1.PartDecodedFile[i] := FormCompose.ComboBoxAttachments.Items.Strings[i-1];

         //encode
         try
            ipwMIME1.EncodeToString;
         except on E: EIPWorks do
            ShowMessage(E.Message);
         end;

         //assign the data. The Headers are indispensable!
         ipwSMTP1.MessageText := ipwMIME1.Message;
         ipwSMTP1.OtherHeaders := ipwMIME1.MessageHeadersString;

      end
      else
      begin
         // No MIME.  Just send the message.
         ipwSMTP1.MessageText := FormCompose.MemoMessage.Text;
      end;

      try
         ipwSMTP1.Connect();
         ipwSMTP1.Send();
         ipwSMTP1.Disconnect();
      except on E: EIPWorks do
         ShowMessage(E.Message);
      end;
      FormCompose.EditCc.Text := '';
      FormCompose.EditTo.Text := '';
      FormCompose.EditSubject.Text := '';
      FormCompose.ComboBoxAttachments.Items.Clear();
      FormCompose.MemoMessage.Lines.Clear();
   end;
   Screen.Cursor := crDefault;
end;

procedure TFormImap.ipwIMAP1MessageInfo(Sender: TObject;
  const MessageId, Subject, MessageDate, From, Flags: String;
  Size: Int64);
begin
   if state = HEADERS_ then
   begin
      ListViewMessages.Items.Add();
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].Caption := MessageId;
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].SubItems.Add(From);
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].SubItems.Add(Subject);
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].SubItems.Add(MessageDate);
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].SubItems.Add(IntToStr(Size));
   end;
end;

procedure TFormImap.ipwIMAP1MessagePart(Sender: TObject; const PartId: string;
  Size: Int64; const ContentType, FileName, ContentEncoding, Parameters,
  MultipartMode, ContentId, ContentDisposition: string);
begin
//   if (ContentType = 'text/plain') and (Filename = '' ) then
//      textpart := PartId;
end;

end.


