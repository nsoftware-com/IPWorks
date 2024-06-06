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
unit popclientf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ipwPOP, ipwSMTP, ipwMIME, ipwcore, ipwtypes, ComCtrls, StdCtrls, ExtCtrls;

type
  Encoding = ( MIME, NOTMIME );

  TFormPopclient = class(TForm)
    ipwMIME1: TipwMIME;
    ipwPOP1: TipwPOP;
    ipwSMTP1: TipwSMTP;
    ListBoxMessage: TListBox;
    ListViewMailbox: TListView;
    SaveDialog1: TSaveDialog;
    ButtonLoginLogout: TButton;
    ButtonRetrieve: TButton;
    ButtonCompose: TButton;
    ButtonReply: TButton;
    ButtonDelete: TButton;
    Label2: TLabel;
    ButtonSave: TButton;
    ComboBoxAttachments: TComboBox;
    Label1: TLabel;
    ComboBoxFileNames: TComboBox;
    procedure ipwPOP1Header(Sender: TObject; const Field, Value: String);
    procedure ButtonLoginLogoutClick(Sender: TObject);
    procedure ButtonComposeClick(Sender: TObject);
    procedure ButtonReplyClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonRetrieveClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ListViewMailboxClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Decode(EntityHeaders: String; EntityText: String);
    procedure ipwSMTP1PITrail(Sender: TObject; Direction: Integer;
      const Message: String);
    procedure ipwSMTP1EndTransfer(Sender: TObject; Direction: Integer);
    procedure ipwSMTP1StartTransfer(Sender: TObject; Direction: Integer);
    procedure ipwPOP1SSLServerAuthentication(Sender: TObject;
      const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
    procedure ipwSMTP1SSLServerAuthentication(Sender: TObject;
      const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);

  private
    { Private declarations }
    IsMime: Encoding;
    msg_from: string;
    msg_date: string;
    msg_subject: string;
    lines: integer;

  public
    { Public declarations }
    constructor Create( Owner: TComponent ); override;

  end;

var
  FormPopclient: TFormPopclient;

implementation

uses progressf, loginf, composef;

{$R *.DFM}


constructor TFormPopclient.Create( Owner: TComponent );
begin
   inherited Create(Owner);
   lines := 0;
   msg_from := '';
   msg_date := '';
   msg_subject := '';
end;




procedure TFormPopclient.ipwPOP1Header(Sender: TObject; const Field,
  Value: String);
begin
   if Field = 'From' then
   begin
   	msg_from := Value;
   end

   else if Field = 'Date' then
   begin
   	msg_date := Value;
   end

   else if Field = 'Subject' then
   begin
      msg_subject := Value;
   end
end;

procedure TFormPopclient.ipwPOP1SSLServerAuthentication(Sender: TObject;
  const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
  CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormPopclient.ButtonLoginLogoutClick(Sender: TObject);
var
   k: integer;
begin
   // Login
   Screen.Cursor := crHourGlass;
   if ButtonLoginLogout.Caption = 'Login' then
   begin
      if FormLogin.ShowModal() = mrOk then
      begin
         ListViewMailbox.Items.Clear();
         ipwPOP1.Disconnect();

         ipwPOP1.MailServer := FormLogin.EditPOPServer.Text;
         ipwPOP1.User := FormLogin.EditUser.Text;
         ipwPOP1.Password := FormLogin.EditPassword.Text;
         ipwSMTP1.MailServer := FormLogin.EditSMTPServer.Text;
         try
            ipwPOP1.Connect;
         except on E: EIPWorks do
            ShowMessage(E.Message);
         end;

         ButtonLoginLogout.Caption := 'Logout';
         ButtonRetrieve.Enabled := true;
         ButtonCompose.Enabled := true;

         // Get message headers.
         ipwPOP1.MaxLines := 1;
         for k := 1 to ipwPOP1.MessageCount do
         begin
            msg_from := '';
            msg_date := '';
            msg_subject := '';
            ipwPOP1.MessageNumber := k;
            try
               ipwPOP1.Retrieve;
            except on E: EIPWorks do
               ShowMessage(E.Message);
            end;
            ListViewMailbox.Items.Add();
            ListViewMailbox.Items.Item[k - 1].Caption := IntToStr(ipwPOP1.MessageNumber);
            ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_from);
            ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_subject);
            ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_date);
         end;
         ipwPOP1.MaxLines := 0;
      end;
   end
   else
   begin
      try
         ipwPOP1.Disconnect();
      except on E: EIPWorks do
         ShowMessage(E.Message);
      end;
      ButtonLoginLogout.Caption := 'Login';
      ButtonCompose.Enabled := false;
      ButtonRetrieve.Enabled := false;
      ButtonReply.Enabled := false;
      ButtonDelete.Enabled := false;
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormPopclient.ButtonComposeClick(Sender: TObject);
var
   i: integer;
begin
// Compose
   Screen.Cursor := crHourGlass;
   if FormCompose.ShowModal() = mrOk then
   begin
      ipwSMTP1.ResetHeaders();
      ipwSMTP1.From := FormCompose.EditFrom.Text;
      ipwSMTP1.SendTo := FormCompose.EditTo.Text;
      ipwSMTP1.Cc := FormCompose.EditCc.Text;
      ipwSMTP1.Subject := FormCompose.EditSubject.Text;

      if FormCompose.ComboBoxAttachments.Items.Count > 0 then
      begin
         //reset data
         ipwMIME1.ResetData;

         //set PartCount.  Part 0 is the plain text of the message
         ipwMIME1.PartCount := FormCompose.ComboBoxAttachments.Items.Count + 1;
         ipwMIME1.PartDecodedString[0] := FormCompose.MemoMessage.Text;

         //add attachments
         for i := 1 to FormCompose.ComboBoxAttachments.Items.Count do
            ipwMIME1.PartDecodedFile[i] := FormCompose.ComboBoxAttachments.Items[i-1];

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
         ipwSMTP1.MessageText := FormCompose.MemoMessage.Text ;
      end;

      try
         ipwSMTP1.Connect();
         ipwSMTP1.Send();
         ipwSMTP1.Disconnect();
      except on E: EIPWorks do
         ShowMessage(E.Message);
      end;
      FormCompose.EditTo.Text := '';
      FormCompose.EditCc.Text := '';
      FormCompose.EditSubject.Text := '';
      FormCompose.ComboBoxAttachments.Items.Clear();
      FormCompose.ComboBoxAttachments.Text := '';
      FormCompose.MemoMessage.Lines.Clear();
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormPopclient.ButtonReplyClick(Sender: TObject);
var
   j: integer;
begin
// Reply
   Screen.Cursor := crHourGlass;
   // Only reply if a message is selected.
   if ListViewMailbox.SelCount > 0 then
   begin
      FormCompose.EditTo.Text := msg_from;
      FormCompose.EditSubject.Text := 'Re: ' + msg_subject;
      for j := 0 to ListBoxMessage.Items.Count - 1 do
         FormCompose.MemoMessage.Lines.Add( '> ' + ListBoxMessage.Items.Strings[j] );

      if FormCompose.ShowModal() = mrOk then
      begin
         ipwSMTP1.ResetHeaders();
         ipwSMTP1.From := FormCompose.EditFrom.Text;
         ipwSMTP1.SendTo := FormCompose.EditTo.Text;
         ipwSMTP1.Cc := FormCompose.EditCc.Text ;
         ipwSMTP1.Subject := FormCompose.EditSubject.Text;

         if FormCompose.ComboBoxAttachments.Items.Count <> 0 then
         begin
            //reset data
            ipwMIME1.ResetData;

            //set PartCount.  Part 0 is the plain text of the message
            ipwMIME1.PartCount := FormCompose.ComboBoxAttachments.Items.Count + 1;
            ipwMIME1.PartDecodedString[0] := FormCompose.MemoMessage.Text;

            //add attachments
            for j := 1 to FormCompose.ComboBoxAttachments.Items.Count do
               ipwMIME1.PartDecodedFile[j] := FormCompose.ComboBoxAttachments.Items.Strings[j-1];

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
            ipwSMTP1.MessageText := FormCompose.MemoMessage.Text ;


         try
            ipwSMTP1.Connect;
            ipwSMTP1.Send;
            ipwSMTP1.Disconnect;
         except on E: EIPWorks do
            ShowMessage(E.Message);
         end;
         FormCompose.EditTo.Text := '';
         FormCompose.EditCc.Text := '';
         FormCompose.EditSubject.Text := '';
         FormCompose.ComboBoxAttachments.Items.Clear();
         FormCompose.ComboBoxAttachments.Text := '';
         FormCompose.MemoMessage.Lines.Clear();
      end;
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormPopclient.ButtonDeleteClick(Sender: TObject);
var
   k: integer;
begin
// Delete a message.
   Screen.Cursor := crHourGlass;
   // Only delete if a message is selected.
   if ListViewMailbox.SelCount > 0 then
   begin
      ipwPOP1.MessageNumber := StrToInt(ListViewMailbox.Selected.Caption);
      try
         ipwPOP1.Delete();
      except on E: EIPWorks do
         ShowMessage(E.Message);
      end;
      // Now for convenience, refresh the header list for remaining messages.
      if ButtonLoginLogout.Caption = 'Logout' then
      begin
         ListViewMailbox.Items.Clear();
         try
            ipwPOP1.Disconnect();
            ipwPOP1.Connect();
         except on E: EIPWorks do
            ShowMessage(E.Message);
         end;
         ipwPOP1.MaxLines := 1;
         for k := 1 to ipwPOP1.MessageCount do
         begin
            msg_from := '';
            msg_date := '';
            msg_subject := '';
            ipwPOP1.MessageNumber := k;
            try
               ipwPOP1.Retrieve();
            except on E: EIPWorks do
               ShowMessage(E.Message);
            end;
            ListViewMailbox.Items.Add();
            ListViewMailbox.Items.Item[k - 1].Caption := IntToStr( ipwPOP1.MessageNumber );
            ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_from);
            ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_subject);
            ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_date);
         end;
         ipwPOP1.MaxLines := 0;
      end;
   end
   else
   begin
      ShowMessage('No message selected.');
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormPopclient.ButtonRetrieveClick(Sender: TObject);
var
   k: integer;
begin
// Retrieve
   Screen.Cursor := crHourGlass;
   if ButtonLoginLogout.Caption = 'Logout' then
   begin
      ListViewMailbox.Items.Clear();
      try
         ipwPOP1.Disconnect();
         ipwPOP1.Connect();
      except on E: EIPWorks do
         ShowMessage(E.Message);
      end;
      ipwPOP1.MaxLines := 1;

      for k := 1 to ipwPOP1.MessageCount do
      begin
         msg_from := '';
         msg_date := '';
         msg_subject := '';
         ipwPOP1.MessageNumber := k;
         try
            ipwPOP1.Retrieve();
         except on E: EIPWorks do
            ShowMessage(E.Message);
         end;
         ListViewMailbox.Items.Add();
         ListViewMailbox.Items.Item[k - 1].Caption := IntToStr( ipwPOP1.MessageNumber );
         ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_from);
         ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_subject);
         ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_date);
      end;
      ipwPOP1.MaxLines := 0;
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormPopclient.ButtonSaveClick(Sender: TObject);
begin
// Save an attachment.
   Screen.Cursor := crHourGlass;
   begin
      SaveDialog1.Title := 'Save Attachment As';
      SaveDialog1.FileName := ComboBoxAttachments.Items.Strings[ComboBoxAttachments.ItemIndex];
      if SaveDialog1.Execute() then
      begin
         // Move the decoded temp file to the location you want to save it.
         RenameFile(ComboBoxFileNames.Items.Strings[ComboBoxAttachments.ItemIndex], SaveDialog1.FileName);
      end;
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormPopclient.ListViewMailboxClick(Sender: TObject);
var
headerIndex: Integer;
begin
// A message header was clicked.  Display the message text.
   Screen.Cursor := crHourGlass;
   ListBoxMessage.Items.Clear();
   if ListViewMailbox.SelCount > 0 then
   begin
      ipwPOP1.MessageNumber := StrToInt(ListViewMailbox.Selected.Caption);
      IsMime := NOTMIME;
      ComboBoxAttachments.Items.Clear();
      ComboBoxFileNames.Items.Clear();
      try
         ipwPOP1.Retrieve();
         //If this is a MIME message decode it, otherwise just display the text

        for headerIndex := 1 to ipwPOP1.MessageHeaderCount - 1 do
        begin
          if(AnsiLowerCase(ipwPOP1.MessageHeaderField[headerIndex]) = 'content-type') then
          begin
            if(Pos('multipart',AnsiLowerCase(ipwPOP1.MessageHeaderValue[headerIndex])) > 0 ) then
              IsMime := MIME;
          end;


        end;

        if(IsMime = MIME) then
        begin
           Decode(ipwPOP1.MessageHeadersString, ipwPOP1.MessageText);
        end
        else
        begin
           ListBoxMessage.Items.Text := ipwPOP1.MessageText;
        end;



      except on E: EIPWorks do
         ShowMessage(E.Message);
      end;

      ComboBoxAttachments.ItemIndex := 0;
      ButtonReply.Enabled := true;
      ButtonDelete.Enabled := true;
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormPopclient.FormResize(Sender: TObject);
begin
   ListViewMailbox.Columns.Items[1].Width := (ListViewMailbox.Width - 50) div 3;
   ListViewMailbox.Columns.Items[2].Width := ListViewMailbox.Columns.Items[1].Width;
   ListViewMailbox.Columns.Items[3].Width := ListViewMailbox.Columns.Items[1].Width;
end;

procedure TFormPopclient.ipwSMTP1EndTransfer(Sender: TObject; Direction: Integer);
begin
   FormProgress.Hide();
end;

procedure TFormPopclient.ipwSMTP1PITrail(Sender: TObject; Direction: Integer;
  const Message: String);
begin
   FormProgress.ListBoxPITrail.Items.Add(Message);
end;


procedure TFormPopclient.ipwSMTP1SSLServerAuthentication(Sender: TObject;
  const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
  CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormPopclient.ipwSMTP1StartTransfer(Sender: TObject;
  Direction: Integer);
begin
   FormProgress.Show();
end;

procedure TFormPopclient.Decode(EntityHeaders: String; EntityText: String);
var
   i: integer;
   ipwMIME1: TipwMIME;
begin
  ipwMIME1 := TipwMIME.Create(nil);
  ipwMIME1.ResetData;
  ipwMIME1.MessageHeadersString := EntityHeaders;
  ipwMIME1.Message := EntityText;
  try
    ipwMIME1.DecodeFromString;
  except on E: EIPWorks do
    //Part is not encoded
    ListBoxMessage.Items.Text := EntityText;
  end;
  for i := 0 to ipwMIME1.PartCount - 1 do
  begin
    //If the Conent-Type is "Multipart/????", then that part is an encoded entity
    if AnsiLowerCase(Copy(ipwMime1.PartContentType[i], 1, 9)) = 'multipart' then
      Decode(ipwMime1.PartHeaders[i], ipwMime1.PartDecodedString[i]);

    //The first "text/plain" part encountered will be used as the body
    if (ListBoxMessage.Items.Text = '') And
       (ipwMime1.PartFilename[i] = '') And
       (AnsiLowerCase(Copy(ipwMime1.PartContentType[i], 1, 10)) = 'text/plain') then
         ListBoxMessage.Items.Text := ipwMime1.PartDecodedString[i];

    //Any attachments will be added to the listbox and associated to their
    //decoded file name through an array.
    if AnsiLowerCase(Copy(ipwMime1.PartContentDisposition[i], 1, 10)) = 'attachment' then
    begin
      ComboBoxAttachments.Items.Add(ipwMime1.PartFilename[i]);
      ComboBoxFileNames.Items.Add(ipwMime1.DecodePartToFile(i));
    end;
  end;
end;



end.

