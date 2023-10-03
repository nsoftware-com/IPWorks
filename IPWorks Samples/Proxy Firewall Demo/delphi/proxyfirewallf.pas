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
unit proxyfirewallf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ipwcore, ipwtypes, ipwhttp;

type
  TFormProxyfirewall = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    rbAuto: TRadioButton;
    gbManual: TGroupBox;
    rbManual: TRadioButton;
    Label2: TLabel;
    cbProxyType: TComboBox;
    Label3: TLabel;
    tHost: TEdit;
    Label4: TLabel;
    tPort: TEdit;
    gbAuthenticate: TGroupBox;
    cbAuthenticate: TCheckBox;
    Label5: TLabel;
    tUser: TEdit;
    Label6: TLabel;
    tPass: TEdit;
    Label7: TLabel;
    tURL: TEdit;
    bGet: TButton;
    tResults: TMemo;
    Panel1: TPanel;
    Shape1: TShape;
    HTTP1: TipwHTTP;
    procedure bGetClick(Sender: TObject);
    procedure HTTP1Transfer(Sender: TObject; Direction: Integer;
      BytesTransferred: Int64; PercentDone: Integer; Text: String);
    procedure cbProxyTypeSelect(Sender: TObject);
    procedure HTTP1SSLServerAuthentication(Sender: TObject; CertEncoded: string; CertEncodedB: TArray<System.Byte>;
      const CertSubject, CertIssuer, Status: string; var Accept: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormProxyfirewall: TFormProxyfirewall;

implementation

{$R *.dfm}

procedure TFormProxyfirewall.bGetClick(Sender: TObject);
begin
  if rbAuto.Checked then
    HTTP1.FirewallAutoDetect := true
  else
  begin
    HTTP1.FirewallType := TipwhttpFirewallTypes(cbProxyType.ItemIndex);
    HTTP1.FirewallHost := tHost.Text;
    HTTP1.FirewallPort := StrToInt(tPort.Text);
  end;
  if cbAuthenticate.Checked then
  begin
    HTTP1.FirewallUser := tUser.Text;
    HTTP1.FirewallPassword := tPass.Text;
  end;
  try
    Screen.Cursor := crHourGlass;
    HTTP1.FollowRedirects := frAlways;
    HTTP1.Get(tURL.Text);
  Except on E:EipwHTTP do
    MessageDlg('Exception: ' + E.Message, mtWarning, [mbOk], 0);
  end;
  Shape1.Width := 0;
  Screen.Cursor := crDefault;

end;

procedure TFormProxyfirewall.HTTP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormProxyfirewall.HTTP1Transfer(Sender: TObject; Direction: Integer;
  BytesTransferred: Int64; PercentDone: Integer; Text: String);
begin
  tResults.Text := tResults.Text + Text;
  Shape1.Width := (PercentDone * Panel1.Width) div 100;
end;

procedure TFormProxyfirewall.cbProxyTypeSelect(Sender: TObject);
begin
  Case cbProxyType.ItemIndex of
    0: tPort.Text := '';
    1: tPort.Text := '80';
    2, 3: tPort.Text := '1080';
  end;
end;

end.

