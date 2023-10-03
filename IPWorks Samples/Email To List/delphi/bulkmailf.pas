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
unit bulkmailf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ipwcore, ipwtypes, ipwhtmlmailer;

type
  TFormBulkmail = class(TForm)
    lblHeading: TLabel;
    txtMailServer: TEdit;
    txtFrom: TEdit;
    txtRecipients: TMemo;
    txtSubject: TEdit;
    txtBody: TMemo;
    btnSend: TButton;
    ipwHTMLMailer1: TipwHTMLMailer;
    procedure btnSendClick(Sender: TObject);
    procedure ipwHTMLMailer1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormBulkmail: TFormBulkmail;

implementation

{$R *.dfm}

procedure TFormBulkmail.btnSendClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  btnSend.Enabled := false;

  try
    ipwHTMLMailer1.Timeout := 60;
    ipwHTMLMailer1.MailServer := txtMailServer.Text;
    ipwHTMLMailer1.Subject := txtSubject.Text;
    ipwHTMLMailer1.From := txtFrom.Text;
    ipwHTMLMailer1.MessageHTML := txtBody.Text;
    ipwHTMLMailer1.SendTo := txtRecipients.Text;
    ipwHTMLMailer1.Send;
    ShowMessage('Message sent successfully');
  Except on E:Exception do
    MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
  end;

  ipwHTMLMailer1.Disconnect;
  Screen.Cursor := crDefault;
  btnSend.Enabled := true;
end;

procedure TFormBulkmail.ipwHTMLMailer1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept := true;
end;

end.

