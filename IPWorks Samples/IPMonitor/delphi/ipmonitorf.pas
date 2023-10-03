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
unit ipmonitorf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ipwcore, ipwtypes, ipwipmonitor;

type
  TFormIpmonitor = class(TForm)
    Label1: TLabel;
    bStart: TButton;
    txtPacketData: TMemo;
    txtPayload: TMemo;
    lvwPackets: TListView;
    ipmonitor1: Tipwipmonitor;
    procedure bStartClick(Sender: TObject);
    procedure lvwPacketsClick(Sender: TObject);
    procedure IPMonitor1IPPacket(Sender: TObject; const SourceAddress: string;
      SourcePort: Integer; const DestinationAddress: string; DestinationPort,
      IPVersion, TOS, Id, Flags, Offset, TTL, Checksum, IPProtocol: Integer;
      Payload: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type Packet_INFO = record
    SourceAddress:String;
    SourcePort:longint;
    DestinationAddress:String;
    DestinationPort:longint;
    IPVersion:longint;
    TOS :longint;
    Id:longint;
    Flags:longint;
    Offset: longint;
    TTL:longint;
    Checksum:longint;
    IPProtocol:longint;
    Payload:String;
    end;


var
  FormIpmonitor: TFormIpmonitor;
  packets: Array of PACKET_INFO;
  packetcount:integer;


implementation

{$R *.DFM}

procedure TFormIpmonitor.bStartClick(Sender: TObject);
begin
    If ipmonitor1.Active = False Then begin
        lvwPackets.Items.Clear();
        ipmonitor1.Activate();
        ipmonitor1.ProcessData();
        bStart.Caption := 'S&top';
        end
    else begin
        ipmonitor1.Deactivate();
        bStart.Caption := '&Start';
        end;
end;

procedure TFormIpmonitor.lvwPacketsClick(Sender: TObject);
var i,j,ascvalue: integer;
        ch:char;
begin
    if lvwPackets.Selected <> nil then begin
            txtPacketData.Clear();
            txtPacketData.Text := txtPacketData.Text + 'SOURCE' + Chr(9) + Chr(9) + ': ' + Packets[lvwPackets.Selected.Index].SourceAddress + ', port ' + IntToStr(Packets[lvwPackets.Selected.Index].SourcePort) + #13#10;
            txtPacketData.Text := txtPacketData.Text + 'DESTINATION' + #9+ ': ' +  Packets[lvwPackets.Selected.Index].DestinationAddress+ ', port ' + IntToStr(Packets[lvwPackets.Selected.Index].DestinationPort) + #13#10;
            txtPacketData.Text := txtPacketData.Text + 'FLAGS' + #9#9 + ': ' + IntToStr(Packets[lvwPackets.Selected.Index].Flags) + #13#10;
            txtPacketData.Text := txtPacketData.Text + 'ID' + #9#9 + ': ' + IntToStr(Packets[lvwPackets.Selected.Index].Id) + #13#10;
            txtPacketData.Text := txtPacketData.Text + 'Time-To-Live' + #9 + ': ' + IntToStr(Packets[lvwPackets.Selected.Index].TTL) + #13#10;

            txtPayLoad.Clear();
            //txtPayLoad.Text := Packets[lvwPackets.Selected.Index].PayLoad;

            //txtPayLoad.Text : '';
            For j := 1 To Length(Packets[lvwPackets.Selected.Index].Payload) do begin
                ch := Packets[lvwPackets.Selected.Index].Payload[j];
                ascvalue := ORD(ch);
                If (ascvalue >= 32) And (ascvalue <= 126) Then begin
                    txtPayLoad.Text := txtPayLoad.Text + ch;
                end;
            end;
      end;
end;

procedure TFormIpmonitor.ipmonitor1IPPacket(Sender: TObject;
  const SourceAddress: String; SourcePort: Integer;
  const DestinationAddress: String; DestinationPort, IPVersion, TOS, Id,
  Flags, Offset, TTL, Checksum, IPProtocol: Integer; Payload: String);
var pi:Packet_INFO;
begin
    pi.Checksum := Checksum;
    pi.DestinationAddress := DestinationAddress;
    pi.DestinationPort := DestinationPort;
    pi.Flags := Flags;
    pi.Id := Id;
    pi.IPProtocol := IPProtocol;
    pi.IPVersion := IPVersion;
    pi.Offset := Offset;
    pi.Payload := Payload;
    pi.SourceAddress := SourceAddress;
    pi.SourcePort := SourcePort;
    SetLength(Packets, Length(Packets)+1);
    Packets[packetcount] := pi;
    packetcount := packetcount + 1;
    lvwPackets.Items.Add();
    lvwPackets.Items.Item[lvwPackets.Items.Count-1].Caption := IntToStr(pi.Id);
    lvwPackets.Items.Item[lvwPackets.Items.Count-1].SubItems.Add(IntToStr(pi.IPProtocol));
    lvwPackets.Items.Item[lvwPackets.Items.Count-1].SubItems.Add(pi.SourceAddress);
    lvwPackets.Items.Item[lvwPackets.Items.Count-1].SubItems.Add(IntToStr(pi.SourcePort));
    lvwPackets.Items.Item[lvwPackets.Items.Count-1].SubItems.Add(pi.DestinationAddress);
    lvwPackets.Items.Item[lvwPackets.Items.Count-1].SubItems.Add(IntToStr(pi.DestinationPort));
    lvwPackets.Items.Item[lvwPackets.Items.Count-1].SubItems.Add(IntToStr(pi.IPVersion));
end;

end.

