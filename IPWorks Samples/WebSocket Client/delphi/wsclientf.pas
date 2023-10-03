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
unit wsclientf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ipwcore, ipwtypes, ipwwsclient;

type
  TFormWSClient = class(TForm)
    txtServer: TEdit;
    Label1: TLabel;
    btnConnect: TButton;
    btnDisconnect: TButton;
    Label3: TLabel;
    txtData: TEdit;
    btnSend: TButton;
    txtReceived: TMemo;
    Label2: TLabel;
    Label4: TLabel;
    lblStatus: TLabel;
    Label5: TLabel;
    ipwWSClient1: TipwWSClient;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure WSClient1Connected(Sender: TObject; StatusCode: Integer;
      const Description: string);
    procedure WSClient1Disconnected(Sender: TObject; StatusCode: Integer;
      const Description: string);
    procedure WSClient1Error(Sender: TObject; ErrorCode: Integer;
      const Description: string);
    procedure WSClient1DataIn(Sender: TObject; DataFormat: Integer;
      Text: string; TextB: TArray<System.Byte>; EOM: Boolean; EOL: Boolean);
    procedure WSClient1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormWSClient: TFormWSClient;

implementation

{$R *.dfm}

procedure TFormWSClient.btnConnectClick(Sender: TObject);
begin
  // connect to the specified server at the specified port
  ipwWSClient1.ConnectTo(txtServer.Text);
  txtReceived.Lines.Add('Connected.');
end;

procedure TFormWSClient.btnDisconnectClick(Sender: TObject);
begin
  // close the connection
  ipwWSClient1.Disconnect();
end;

procedure TFormWSClient.btnSendClick(Sender: TObject);
begin
  ipwWSClient1.DataToSend := txtData.Text;
  txtData.Text := '';
end;

procedure TFormWSClient.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Disconnect if the form is closed
  ipwWSClient1.Connected := False;
end;


procedure TFormWSClient.WSClient1Connected(Sender: TObject; StatusCode: Integer;
  const Description: string);
begin
  if (StatusCode = 0) then
  begin
    lblStatus.Caption := 'Connected: ' + Description;
  end
  else
  begin
    lblStatus.Caption := 'Failed Connection: ' + Description;
  end;
end;

procedure TFormWSClient.WSClient1DataIn(Sender: TObject;
  DataFormat: Integer; Text: string; TextB: TArray<System.Byte>; EOM: Boolean; EOL: Boolean);
begin
  // display received data to user
  txtReceived.Lines.Add('Received: ' + Text);
end;

procedure TFormWSClient.WSClient1Disconnected(Sender: TObject;
  StatusCode: Integer; const Description: string);
begin
  // disconnected
  lblStatus.Caption := 'Not Connected';
end;

procedure TFormWSClient.WSClient1Error(Sender: TObject; ErrorCode: Integer;
  const Description: string);
begin
  lblStatus.Caption := 'WebSocketClient Error: [' + IntToStr(ErrorCode) + '] ' + Description;
end;

procedure TFormWSClient.WSClient1SSLServerAuthentication(
  Sender: TObject; CertEncoded: string; CertEncodedB: TArray<System.Byte>;
  const CertSubject, CertIssuer, Status: string; var Accept: Boolean);
begin
Accept := True;
end;

end.


