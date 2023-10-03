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
unit tftpserverf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShellAPI, ipwcore, ipwtypes, ipwtftpserver, StdCtrls, ComCtrls;

type
  TFormTFTPServer = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    EditLocalPort: TEdit;
    EditLocalDir: TEdit;
    GroupBox2: TGroupBox;
    EventLog: TListBox;
    Start: TButton;
    Stop: TButton;
    ipwTFTPServer1: TipwTFTPServer;
    procedure StartClick(Sender: TObject);
    procedure StopClick(Sender: TObject);
    procedure ipwTFTPServer1Connected(Sender: TObject; ConnectionId,
      StatusCode: Integer; const Description: string);
    procedure ipwTFTPServer1ConnectionRequest(Sender: TObject;
      const RemoteHost: string; RemotePort: Integer; const Filename,
      TransferMode: string; Operation: Integer; var Accept: Boolean);
    procedure ipwTFTPServer1Disconnected(Sender: TObject; ConnectionId,
      StatusCode: Integer; const Description: string);
    procedure ipwTFTPServer1EndTransfer(Sender: TObject; ConnectionId,
      Direction: Integer);
    procedure ipwTFTPServer1Error(Sender: TObject; ErrorCode: Integer;
      const Description: string);
    procedure ipwTFTPServer1StartTransfer(Sender: TObject; ConnectionId,
      Direction: Integer);
    procedure ipwTFTPServer1Transfer(Sender: TObject; ConnectionId,
      Direction: Integer; BytesTransferred: Int64; PercentDone: Integer;
      Text: string; TextB: TArray<System.Byte>);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTFTPServer: TFormTFTPServer;

implementation

{$R *.dfm}

procedure TFormTFTPServer.ipwTFTPServer1Connected(Sender: TObject; ConnectionId,
  StatusCode: Integer; const Description: string);
begin
  EventLog.Items.Add(inttostr(ConnectionId) + ': Connected.');
end;

procedure TFormTFTPServer.ipwTFTPServer1ConnectionRequest(Sender: TObject;
  const RemoteHost: string; RemotePort: Integer; const Filename,
  TransferMode: string; Operation: Integer; var Accept: Boolean);
begin
   EventLog.Items.Add(RemoteHost + ': Requesting connection.');
end;

procedure TFormTFTPServer.ipwTFTPServer1Disconnected(Sender: TObject; ConnectionId,
  StatusCode: Integer; const Description: string);
begin
  EventLog.Items.Add(inttostr(ConnectionId) + ': Disconnected.');
end;

procedure TFormTFTPServer.ipwTFTPServer1EndTransfer(Sender: TObject; ConnectionId,
  Direction: Integer);
begin
  EventLog.Items.Add(inttostr(ConnectionId) + ': Transfer complete.');
end;

procedure TFormTFTPServer.ipwTFTPServer1Error(Sender: TObject; ErrorCode: Integer;
  const Description: string);
begin
  EventLog.Items.Add('Error: ' + Description + ' [' + inttostr(ErrorCode) + ']');
end;

procedure TFormTFTPServer.ipwTFTPServer1StartTransfer(Sender: TObject; ConnectionId,
  Direction: Integer);
begin
  EventLog.Items.Add(inttostr(ConnectionId) + ': Transfer initiated');
end;

procedure TFormTFTPServer.ipwTFTPServer1Transfer(Sender: TObject; ConnectionId,
  Direction: Integer; BytesTransferred: Int64; PercentDone: Integer;
  Text: string; TextB: TArray<System.Byte>);
begin
  EventLog.Items.Add(inttostr(ConnectionId) + ': Transfer in progress.');
end;

procedure TFormTFTPServer.StartClick(Sender: TObject);
begin
  ipwTFTPServer1.LocalDir := EditLocalDir.Text;
  ipwTFTPServer1.LocalPort := strtoint(EditLocalPort.Text);
  ipwTFTPServer1.StartListening();
  EventLog.Items.Add('Listening on port: ' + inttostr(ipwTFTPServer1.LocalPort));
end;

procedure TFormTFTPServer.StopClick(Sender: TObject);
begin
  ipwTFTPServer1.StopListening();
  EventLog.Items.Add('Server shutting down...');
end;

end.



