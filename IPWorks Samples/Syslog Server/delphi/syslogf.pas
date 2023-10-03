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
unit syslogf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ipwcore, ipwtypes, ipwsyslog;

type
  TFormSyslog = class(TForm)
    Label1: TLabel;
    bListen: TButton;
    bSend: TButton;
    lvwPackets: TListView;
    SysLog1: TipwSysLog;
    procedure bListenClick(Sender: TObject);
    procedure bSendClick(Sender: TObject);
    procedure SysLog1PacketIn(Sender: TObject; FacilityCode: Integer;
      const Facility: String; SeverityCode: Integer; const Severity,
      Timestamp, Hostname, Message: String; Conforms: Boolean;
      Packet: String; const SourceAddress: String; SourcePort: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSyslog: TFormSyslog;

implementation

{$R *.DFM}

procedure TFormSyslog.bListenClick(Sender: TObject);
begin
    //bind to one address:
    Syslog1.LocalHost := Syslog1.LocalHost;
    SysLog1.Active := true;
end;

procedure TFormSyslog.bSendClick(Sender: TObject);
begin
    SysLog1.SendPacket(1, 5, 'Hello');
end;

procedure TFormSyslog.SysLog1PacketIn(Sender: TObject; FacilityCode: Integer;
  const Facility: String; SeverityCode: Integer; const Severity, Timestamp,
  Hostname, Message: String; Conforms: Boolean; Packet: String;
  const SourceAddress: String; SourcePort: Integer);
begin
    lvwPackets.Items.Add();
    lvwPackets.Items[lvwPackets.Items.Count-1].Caption := SourceAddress;
    lvwPackets.Items[lvwPackets.Items.Count-1].SubItems.Add(Hostname);
    lvwPackets.Items[lvwPackets.Items.Count-1].SubItems.Add(Message);
    lvwPackets.Items[lvwPackets.Items.Count-1].SubItems.Add(Facility);
    lvwPackets.Items[lvwPackets.Items.Count-1].SubItems.Add(Severity);
    lvwPackets.Items[lvwPackets.Items.Count-1].SubItems.Add(Timestamp);

end;

end.

