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
unit smppf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ipwcore, ipwtypes, ipwsmpp;

type
  TFormSmpp = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    txtServer: TEdit;
    Label3: TLabel;
    txtUser: TEdit;
    Label4: TLabel;
    txtPassword: TEdit;
    Label5: TLabel;
    txtRecipient: TEdit;
    txtMessage: TMemo;
    bSend: TButton;
    SMPP1: TipwSMPP;
    procedure bSendClick(sender: TObject);
    procedure SMPP1Disconnected(sender: TObject; StatusCode: Integer;
      const Description: String);
    procedure SMPP1SSLServerAuthentication(Sender: TObject; CertEncoded: string; CertEncodedB: TArray<System.Byte>;
      const CertSubject, CertIssuer, Status: string; var Accept: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSmpp: TFormSmpp;

implementation

{$R *.DFM}

procedure TFormSmpp.bSendClick(sender: TObject);
begin
        Try
            Screen.Cursor := crHourGlass;
            Smpp1.SMPPServer := txtServer.Text;
            Smpp1.ConnectTo(txtUser.Text, txtPassword.Text);
            Smpp1.AddRecipient(0,txtRecipient.Text);
            Smpp1.SendMessage(txtMessage.Text);
            Smpp1.Disconnect();
        except on ipwime:Exception do begin
            ShowMessage(ipwime.Message);
            Smpp1.Disconnect();
            end;
        end;
end;

procedure TFormSmpp.SMPP1Disconnected(sender: TObject; StatusCode: Integer;
  const Description: String);
begin
        If (StatusCode <> 0) Then begin
            ShowMessage('Disconnected (' + IntToStr(StatusCode) + '): ' + Description);
            end
        else begin
            ShowMessage('Message Sent.');
        end;
        Screen.Cursor := crDefault;
end;

procedure TFormSmpp.SMPP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

end.

