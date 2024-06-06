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
unit echoserverf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Spin, ExtCtrls, ipwcore, ipwtypes, Buttons,  ipwTCPServer;

type
  TFormEchoserver = class(TForm)
    lTrack: TListBox;
    TCPServer1: TipwTCPServer;
    Label1: TLabel;
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditLocalPort: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    procedure lTrackDblClick(Sender: TObject);
    procedure TCPServer1Error(Sender: TObject; ConnectionId, ErrorCode: integer;
      const Description: String);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure TCPServer1Connected(Sender: TObject; ConnectionId,
      StatusCode: Integer; const Description: String);
    procedure TCPServer1Disconnected(Sender: TObject; ConnectionId,
      StatusCode: Integer; const Description: String);
    procedure TCPServer1DataIn(Sender: TObject; ConnectionId: Integer;
      const Text: string; const TextB: TBytes; EOL: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEchoserver: TFormEchoserver;

implementation

{$R *.DFM}

procedure TFormEchoserver.lTrackDblClick(Sender: TObject);
begin
	lTrack.Clear;
end;

procedure TFormEchoserver.TCPServer1Error(Sender: TObject; ConnectionId, ErrorCode: integer;
  const Description: String);
begin
	lTrack.Items.Add('!!Error: ' + IntToStr(ErrorCode) + Description);
end;

procedure TFormEchoserver.ButtonStopClick(Sender: TObject);
var i: integer;
begin
        tcpserver1.Shutdown;
end;



procedure TFormEchoserver.ButtonStartClick(Sender: TObject);
begin

     TCPServer1.LocalPort := strtoint(EditLocalPort.Text);

    {If you get an "address already in use" error in the next line
    then your system --for instance Windows NT--
    has already implemented this service.}
    TCPServer1.StartListening();

    lTrack.Clear;
    lTrack.Items.Add('Host ' + TCPServer1.LocalHost + ' ' + inttostr(TCPServer1.LocalPort));

end;

procedure TFormEchoserver.TCPServer1Connected(Sender: TObject; ConnectionId,
  StatusCode: Integer; const Description: String);
begin
    lTrack.Items.Add('Connected from ' + TCPServer1.RemoteHost[ConnectionID] +
                    ' StatusCode: ' + IntToStr(StatusCode) +
                    ' ' + Description);
    TCPServer1.EOL[ConnectionID] := #10;
end;

procedure TFormEchoserver.TCPServer1DataIn(Sender: TObject;
  ConnectionId: Integer; const Text: string; const TextB: TBytes; EOL: Boolean);
begin
  lTrack.Items.Add('Heard: ' + Text);
  TCPServer1.SendText(ConnectionID, Text + #10);
end;

procedure TFormEchoserver.TCPServer1Disconnected(Sender: TObject; ConnectionId,
  StatusCode: Integer; const Description: String);
begin
    lTrack.Items.Add('disconnected');
end;

end.
