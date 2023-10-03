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
unit udpechof;

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
  TFormUdpecho = class(TForm)
    lTrack: TListBox;
    UDP1: TipwUDP;
    Label3: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure lTrackDblClick(Sender: TObject);
    procedure UDP1Error(Sender: TObject; ErrorCode: integer;
      const Description: String);
    procedure UDP1DataIn(Sender: TObject; Datagram: String;
      const SourceAddress: String; SourcePort: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormUdpecho: TFormUdpecho;

implementation

{$R *.DFM}

var
	gStartTime: Longint;

procedure TFormUdpecho.Button1Click(Sender: TObject);
begin
    UDP1.RemoteHost := '255.255.255.255';
    UDP1.RemotePort := 7;{echo service}
    UDP1.LocalHost := UDP1.LocalHost;
    
    lTrack.Clear;

    if not UDP1.Active
    then UDP1.Activate();

    gStartTime := GetTickCount;
    {send anything and the server will send the time}
    UDP1.DataToSend := 'hello?';
end;

procedure TFormUdpecho.lTrackDblClick(Sender: TObject);
begin
	lTrack.Clear;
end;

procedure TFormUdpecho.UDP1Error(Sender: TObject; ErrorCode: integer;
  const Description: String);
begin
	lTrack.Items.Add('!!Error: ' + IntToStr(ErrorCode) + Description);
end;

procedure TFormUdpecho.UDP1DataIn(Sender: TObject; Datagram: String;
  const SourceAddress: String; SourcePort: Integer);
begin
    lTrack.Items.Add(SourceAddress + ': ' +
        IntToStr(GetTickCount - gStartTime) + ' milliseconds');
end;

end.

