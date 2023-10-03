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
unit htmlmailerf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ipwcore, ipwtypes, ipwhtmlmailer;

type
  TFormHtmlmailer = class(TForm)
    lblHeading: TLabel;
    txtMailServer: TEdit;
    lblMailServer: TLabel;
    txtSendTo: TEdit;
    lblSendTo: TLabel;
    txtFrom: TEdit;
    lblFrom: TLabel;
    txtSubject: TEdit;
    lblSubject: TLabel;
    lblHTML: TLabel;
    txtHTML: TMemo;
    lstAttachments: TListBox;
    lblAttachments: TLabel;
    btnAdd: TButton;
    btnRemove: TButton;
    btnSend: TButton;
    ipwHTMLMailer1: TipwHTMLMailer;
    OpenDialog1: TOpenDialog;
    procedure btnSendClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure ipwHTMLMailer1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormHtmlmailer: TFormHtmlmailer;

implementation

{$R *.dfm}

procedure TFormHtmlmailer.btnAddClick(Sender: TObject);
begin
  if OpenDialog1.Execute then lstAttachments.Items.Add(OpenDialog1.FileName);
end;

procedure TFormHtmlmailer.btnRemoveClick(Sender: TObject);
var I: Integer;
begin
    for I := lstAttachments.Items.Count - 1 downto 0 do
    if lstAttachments.Selected[I] then lstAttachments.Items.Delete(I) ;
end;

procedure TFormHtmlmailer.btnSendClick(Sender: TObject);
var I: Integer;
begin
  Screen.Cursor := crHourGlass;

  try
    ipwHTMLMailer1.MailServer := txtMailServer.Text;
    ipwHTMLMailer1.SendTo := txtSendTo.Text;
    ipwHTMLMailer1.Subject := txtSubject.Text;
    ipwHTMLMailer1.From := txtFrom.Text;
    //if you wanted basic user authentication:
    //ipwHTMLMailer1.User := "username";
    //ipwHTMLMailer1.Password := "password";

    ipwHTMLMailer1.MessageHTML := txtHtml.Text;
    ipwHTMLMailer1.AttachmentCount := 0;

    for I := 0 to lstAttachments.Items.Count - 1 do
    begin
      ipwHTMLMailer1.AddAttachment (lstAttachments.Items[I]);
    end;

    ipwHTMLMailer1.Connect;
    ipwHTMLMailer1.Send;
    ShowMessage('Message Sent');
  Except on E:Exception do
    MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
  end;
  ipwHTMLMailer1.Disconnect;
  Screen.Cursor := crDefault;
end;

procedure TFormHtmlmailer.ipwHTMLMailer1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

end.

