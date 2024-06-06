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
unit udpclientf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Spin, ExtCtrls, ipwcore, ipwtypes,  ipwUDP,
  {$IF CompilerVersion >= 24 } //XE3 or higher
  Winapi.Windows;
  {$ELSE}
  Windows;
  {$IFEND}

type
  TFormUdpclient = class(TForm)
    lTrack: TListBox;
    UDPClient1: TipwUDP;
    Button1: TButton;
    Label5: TLabel;
    tHost: TEdit;
    tPort: TEdit;
    tEcho: TEdit;
    Intro: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure lTrackDblClick(Sender: TObject);
    procedure UDPClient1Error(Sender: TObject; ErrorCode: integer;
      const Description: String);
    procedure UDPClient1DataIn(Sender: TObject; const Datagram: string;
      const DatagramB: TBytes; const SourceAddress: string;
      SourcePort: Integer);

  
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormUdpclient: TFormUdpclient;

implementation

{$R *.DFM}

procedure TFormUdpclient.Button1Click(Sender: TObject);
begin

    UDPClient1.RemoteHost := tHost.Text;
    UDPClient1.RemotePort := strtoint(tPort.Text);
    
    if not UDPClient1.Active
    then UDPClient1.Activate();

    UDPClient1.SendText(tEcho.Text);
end;

procedure TFormUdpclient.lTrackDblClick(Sender: TObject);
begin
	lTrack.Clear;
end;


procedure TFormUdpclient.UDPClient1DataIn(Sender: TObject; const Datagram: string;
  const DatagramB: TBytes; const SourceAddress: string; SourcePort: Integer);
begin
    lTrack.Items.Add('Received: "' + Datagram + '" from: ' + SourceAddress + ':' + inttostr(SourcePort));

end;

procedure TFormUdpclient.UDPClient1Error(Sender: TObject; ErrorCode: integer;
  const Description: String);
begin
	lTrack.Items.Add('!!Error: ' + IntToStr(ErrorCode) + Description);
end;

end.

