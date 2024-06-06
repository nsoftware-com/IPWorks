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
unit udpserverf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Spin, ExtCtrls, ipwcore, ipwtypes, Buttons,
  ipwudp;

type
  TFormUdpserver = class(TForm)
    lTrack: TListBox;
    Label1: TLabel;
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditLocalPort: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    UDPServer1: TipwUDP;
    procedure lTrackDblClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure UDPServer1DataIn(Sender: TObject; const Datagram: string;
      const DatagramB: TBytes; const SourceAddress: string;
      SourcePort: Integer);
    procedure UDPServer1Error(Sender: TObject; ErrorCode: Integer;
      const Description: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormUdpserver: TFormUdpserver;

implementation

{$R *.DFM}

procedure TFormUdpserver.lTrackDblClick(Sender: TObject);
begin
	lTrack.Clear;
end;

procedure TFormUdpserver.ButtonStartClick(Sender: TObject);
begin

  UDPServer1.LocalPort := strtoint(EditLocalPort.Text);
  UDPServer1.Activate();

  lTrack.Clear;
  lTrack.Items.Add('Listening on port: ' + inttostr(UDPServer1.LocalPort));

end;

procedure TFormUdpserver.ButtonStopClick(Sender: TObject);
begin
  UDPServer1.Deactivate;
  lTrack.Items.Add('Server stopped.');
end;

procedure TFormUdpserver.UDPServer1DataIn(Sender: TObject; const Datagram: string;
  const DatagramB: TBytes; const SourceAddress: string; SourcePort: Integer);
begin
  lTrack.Items.Add('Echoing: "' + Datagram + '" back to client: '+ SourceAddress + ':' + inttostr(SourcePort));
  UDPServer1.RemoteHost:= SourceAddress;
  UDPServer1.RemotePort:= SourcePort;
  UDPServer1.SendText(Datagram);

end;

procedure TFormUdpserver.UDPServer1Error(Sender: TObject; ErrorCode: Integer;
  const Description: string);
begin
  lTrack.Items.Add('!!Error: ' + IntToStr(ErrorCode) + Description);
end;

end.

