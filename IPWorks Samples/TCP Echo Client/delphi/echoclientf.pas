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
unit echoclientf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, ipwcore, ipwtypes,  ipwTCPClient,
  {$IF CompilerVersion >= 24 } //XE3 or higher
  Winapi.Windows;
  {$ELSE}
  Windows;
  {$IFEND}

type
  TFormEchoclient = class(TForm)
    lTrack: TListBox;
    TCPClient1: TipwTCPClient;
    Panel1: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    GroupBoxStatus: TGroupBox;
    bEcho: TButton;
    tEcho: TEdit;
    bConnect: TButton;
    Disconnect: TButton;
    EditPort: TEdit;
    Label2: TLabel;
    Label1: TLabel;
    tHost: TEdit;
    ListStatus: TListBox;
    procedure bEchoClick(Sender: TObject);
    procedure TCPClient1Error(Sender: TObject; ErrorCode: integer;
      const Description: String);
    procedure TCPClient1Connected(Sender: TObject; StatusCode: integer;
      const Description: String);
    procedure TCPClient1ReadyToSend(Sender: TObject);
    procedure bConnectClick(Sender: TObject);
    procedure tHostKeyPress(Sender: TObject; var Key: Char);
    procedure tEchoKeyPress(Sender: TObject; var Key: Char);
    procedure lTrackMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DisconnectClick(Sender: TObject);
    procedure TCPClient1Disconnected(Sender: TObject; StatusCode: Integer;
      const Description: String);
   
    procedure TCPClient1DataIn(Sender: TObject; const Text: string;
      const TextB: TBytes; EOL: Boolean);
    procedure TCPClient1SSLServerAuthentication(Sender: TObject;
      const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEchoclient: TFormEchoclient;

implementation

{$R *.DFM}

var
	gStartTime: Longint;
	bError: Boolean;

procedure TFormEchoclient.bEchoClick(Sender: TObject);
begin
    {notice the time}
    gStartTime := GetTickCount;

    {send anything and the server will send the time}
    TCPClient1.SendText(tEcho.Text + #10);
end;

procedure TFormEchoclient.TCPClient1Error(Sender: TObject; ErrorCode: integer;
  const Description: String);
begin
	ListStatus.Items.Add('!!Error: ' + IntToStr(ErrorCode) + Description);
    bError := TRUE;
end;

procedure TFormEchoclient.TCPClient1Connected(Sender: TObject; StatusCode: integer;
  const Description: String);
begin
    if 0 <> StatusCode then
    begin
    	ListStatus.Items.Add('Connection failed: ' + Description);
        bError := TRUE;
    end
    else ListStatus.Items.Add('Connected: ' + Description);
end;

procedure TFormEchoclient.TCPClient1ReadyToSend(Sender: TObject);
begin
    ListStatus.Items.Add('ready to send');
end;

procedure TFormEchoclient.TCPClient1SSLServerAuthentication(Sender: TObject;
  const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
  CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormEchoclient.bConnectClick(Sender: TObject);
var
    After10Seconds: DWORD;
begin
    lTrack.Clear;
	bEcho.Enabled := FALSE;
    try
        TCPClient1.EOL := #10;

        if TCPClient1.RemoteHost <> tHost.Text
        then begin
            TCPClient1.Disconnect(); {unconditionally break the old connection}
            TCPClient1.RemoteHost := tHost.Text;
        end;

        TCPClient1.RemotePort := strtoint(EditPort.Text); {TCP echo service}
        TCPClient1.Connect();

        ListStatus.Items.Add('Connecting to ' + TCPClient1.RemoteHost);

        bError := FALSE;
        After10Seconds := GetTickCount + 30 * 1000;
        while GetTickCount < After10Seconds do
        begin
            if TCPClient1.Connected or bError then break;
            Application.ProcessMessages;
        end;

        if TCPClient1.Connected then
        begin
        	bEcho.Enabled := TRUE;
                tEcho.SetFocus;
        end
        else begin
            TCPClient1.Disconnect();
            ListStatus.Items.Add('Timed out ');
        end;

    except on E: EIPWorks do
        ListStatus.Items.Add('Exception: ' + E.Message);
    end;
end;

procedure TFormEchoclient.tHostKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #13 then
    begin
        Key := #0;
        bConnect.Click;
    end;
end;

procedure TFormEchoclient.tEchoKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #13 then
    begin
        Key := #0;
        bEcho.Click;
    end;
end;

procedure TFormEchoclient.lTrackMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbRight
	then lTrack.Clear;
end;

procedure TFormEchoclient.FormDestroy(Sender: TObject);
begin
	TCPClient1.Disconnect();
end;

procedure TFormEchoclient.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	TCPClient1.Disconnect();
end;

procedure TFormEchoclient.DisconnectClick(Sender: TObject);
begin
    TCPClient1.Disconnect()
end;

procedure TFormEchoclient.TCPClient1DataIn(Sender: TObject; const Text: string;
  const TextB: TBytes; EOL: Boolean);
begin
  lTrack.Items.Add(Text + ': ' +
  IntToStr(GetTickCount - gStartTime) + ' milliseconds');
end;

procedure TFormEchoclient.TCPClient1Disconnected(Sender: TObject; StatusCode: Integer;
  const Description: String);
begin
    ListStatus.Items.Add('Disconnected');
end;


end.


