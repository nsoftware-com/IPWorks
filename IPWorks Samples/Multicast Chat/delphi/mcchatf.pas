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
unit mcchatf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, ipwcore, ipwtypes,  ipwMCast;

type
  TFormMcchat = class(TForm)
    MCast1: TipwMCast;
    mMessages: TMemo;
    Label2: TLabel;
    eGroup: TEdit;
    eName: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    btnCommand1: TButton;
    Label3: TLabel;
    eLine: TEdit;
    btnSend: TButton;
    procedure btnCommand1Click(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure MCast1DataIn(Sender: TObject; const Datagram: string;
      const DatagramB: TBytes; const SourceAddress: string;
      SourcePort: Integer);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMcchat: TFormMcchat;

implementation

{$R *.DFM}

procedure TFormMcchat.btnCommand1Click(Sender: TObject);
begin
    if (Sender as TButton).Caption = '&Join' then
    begin
        MCast1.Deactivate();
        MCast1.LocalPort := 333;
        MCast1.RemotePort := 333;
        MCast1.MulticastGroup := eGroup.Text;
        MCast1.RemoteHost := eGroup.Text;
        MCast1.Activate();
        MCast1.SendText(eName.Text + ': ' + 'joining discussion...');
        (Sender as TButton).Caption := '&Leave';
        eLine.SetFocus;
    end
    else begin
        MCast1.SendText(eName.Text + ': ' + ' leaving...');
        MCast1.Deactivate();
        (Sender as TButton).Caption := '&Join';
    end
end;

procedure TFormMcchat.btnSendClick(Sender: TObject);
begin
	MCast1.SendText(eName.Text + ': ' + eLine.Text);
end;

procedure TFormMcchat.MCast1DataIn(Sender: TObject; const Datagram: string;
  const DatagramB: TBytes; const SourceAddress: string; SourcePort: Integer);
begin
	mMessages.Lines.Add('[' + SourceAddress + '] ' + Datagram);
	mMessages.Lines.Add('');
end;

end.
