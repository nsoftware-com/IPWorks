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
unit telnetf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ipwcore, ipwtypes,
  ExtCtrls, ipwTelnet,
  {$IF CompilerVersion >= 24 } //XE3 or higher
  Winapi.Windows;
  {$ELSE}
  Windows;
  {$IFEND}
type
  TFormTelnet = class(TForm)
    Telnet1: TipwTelnet;
    Memo1: TMemo;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    tHost: TEdit;
    bLookUp: TButton;
    Label3: TLabel;
    Memo2: TMemo;
    procedure bLookUpClick(Sender: TObject);
    procedure tHostKeyPress(Sender: TObject; var Key: Char);
    procedure Telnet1Connected(Sender: TObject; StatusCode: Integer;
      const Description: String);
    procedure Telnet1Command(Sender: TObject; CommandCode: Integer);
    procedure Memo2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Telnet1Do(Sender: TObject; OptionCode: Integer);
    procedure Telnet1Disconnected(Sender: TObject; StatusCode: Integer;
      const Description: String);
    procedure Telnet1Dont(Sender: TObject; OptionCode: Integer);
    procedure Telnet1Will(Sender: TObject; OptionCode: Integer);
    procedure Telnet1Wont(Sender: TObject; OptionCode: Integer);
    procedure Memo1KeyPress(Sender: TObject; var Key: Char);
    procedure Telnet1DataIn(Sender: TObject; const Text: string;
      const TextB: TBytes);
    procedure Telnet1SSLServerAuthentication(Sender: TObject;
      const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
    procedure Telnet1SubOption(Sender: TObject; const SubOption: string;
      const SubOptionB: TBytes);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTelnet: TFormTelnet;

implementation
{$R *.DFM}

procedure TFormTelnet.bLookUpClick(Sender: TObject);
{finger user@hostname}
var
    After10Seconds: Longint;
begin
    try
        if tHost.Text = '' then
        begin
            MessageBeep($FFFF);
            exit;
        end;

        {UNCONDITIONALLY close old connections or attempts}
        Telnet1.Disconnect();

        {set the remote host}
	Telnet1.RemoteHost := tHost.Text;

        {attempt connection}
        Memo1.Lines.Add('Connecting to ' + Telnet1.RemoteHost + '...   ');
        Memo1.Lines.Add('Requested ' + tHost.Text);
		Telnet1.Connect();

        {wait until the connection is achieved
        (timeout in 10 seconds)}
        After10Seconds := GetTickCount + 10 * 1000;
        while GetTickCount < After10Seconds do
        begin
            if Telnet1.Connected then break;
            Application.ProcessMessages;
        end;

        if not Telnet1.Connected then
        begin
            MessageDlg( 'Connection timed out.', mtWarning, [mbOK], 0);
            Memo1.Clear;
        end
    except on E: EIPWorks do
        Memo1.Lines.Add('EXCEPTION: ' + E.Message);
    end;
end;

procedure TFormTelnet.Telnet1Connected(Sender: TObject; StatusCode: Integer;
  const Description: String);
begin
	if 0 <> StatusCode
	then Memo2.Lines.Add('Connection failed: ' + Description);
end;


procedure TFormTelnet.Telnet1Command(Sender: TObject; CommandCode: Integer);
begin
	Memo2.Lines.Add('SERVER: COMMAND ' + IntToStr(CommandCode));
end;

procedure TFormTelnet.tHostKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #13 then
    begin
        bLookUp.Click;
    	Key := #0;
    end;
end;

procedure TFormTelnet.Memo2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	if mbRight = Button
    then (Sender as TMemo).Clear;
end;

procedure TFormTelnet.Telnet1Do(Sender: TObject; OptionCode: Integer);
begin
	Memo2.Lines.Add('SERVER: DO OPTION ' + IntToStr(OptionCode));
{	if OptionCode = 24 then
        Telnet1.SendWillOption(OptionCode);
    else}
        Telnet1.SendWontOption(OptionCode);
	Memo2.Lines.Add('CLIENT: WONT OPTION ' + IntToStr(OptionCode));
end;

procedure TFormTelnet.Telnet1DataIn(Sender: TObject; const Text: string;
  const TextB: TBytes);
begin
  Memo1.SelStart := Length(Memo1.Text);
    Memo1.SelText := (Text);
    Memo1.SetFocus;
end;

procedure TFormTelnet.Telnet1Disconnected(Sender: TObject; StatusCode: Integer;
  const Description: String);
begin
	Memo2.Lines.Add('Disconnected: ' + Description);
end;

procedure TFormTelnet.Telnet1Dont(Sender: TObject; OptionCode: Integer);
begin
	Memo2.Lines.Add('SERVER: DON''T OPTION ' + IntToStr(OptionCode));
end;

procedure TFormTelnet.Telnet1SSLServerAuthentication(Sender: TObject;
  const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
  CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormTelnet.Telnet1SubOption(Sender: TObject; const SubOption: string;
  const SubOptionB: TBytes);
begin
  Memo2.Lines.Add('SERVER: DO SUBOPTION ' + SubOption);
  Telnet1.SendDoSubOption(TEncoding.ASCII.GetBytes(char(24) + char(0) + 'vt100'));
end;

procedure TFormTelnet.Telnet1Will(Sender: TObject; OptionCode: Integer);
begin
	Memo2.Lines.Add('SERVER: WILL OPTION ' + IntToStr(OptionCode));
end;

procedure TFormTelnet.Telnet1Wont(Sender: TObject; OptionCode: Integer);
begin
	Memo2.Lines.Add('SERVER: WON''T OPTION ' + IntToStr(OptionCode));
end;

procedure TFormTelnet.Memo1KeyPress(Sender: TObject; var Key: Char);
begin
    {send the character}
    Telnet1.SendText(Key);

    {add a line feed after carriage return}
    If Key = #13 Then Telnet1.SendText(#10);

    {do not allow local echo}
    Key := #0;

end;

end.
