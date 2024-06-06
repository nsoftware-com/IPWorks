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
unit mxf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ipwcore, ipwtypes, ipwMX, StdCtrls, ExtCtrls;

type
  TFormMx = class(TForm)
    ipwMX1: TipwMX;
    ListServer: TMemo;
    Label4: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    EditDNSServer: TEdit;
    EditEmail: TEdit;
    ButtonQuery: TButton;
    Label1: TLabel;
    procedure ButtonQueryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ipwMX1Response(Sender: TObject; RequestId: Integer;
      const Domain, MailServer: String; Precedence, TimeToLive,
      StatusCode: Integer; const Description: String;
      Authoritative: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMx: TFormMx;

implementation

{$R *.DFM}

procedure TFormMx.ButtonQueryClick(Sender: TObject);
begin
    ListServer.Clear;
    ipwMX1.DNSServer := EditDNSServer.Text;
    ipwMX1.Resolve(EditEmail.Text);
end;

procedure TFormMx.FormCreate(Sender: TObject);
begin
      EditDNSServer.Text := ipwMX1.DNSServer;
end;

procedure TFormMx.ipwMX1Response(Sender: TObject; RequestId: Integer;
  const Domain, MailServer: String; Precedence, TimeToLive,
  StatusCode: Integer; const Description: String; Authoritative: Boolean);
begin
     ListServer.Lines.Add(MailServer);
end;

end.

