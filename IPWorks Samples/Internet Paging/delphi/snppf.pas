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
unit snppf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ipwcore, ipwtypes, ipwSNPP, StdCtrls, ExtCtrls;

type
  TFormSnpp = class(TForm)
    ipwSNPP1: TipwSNPP;
    MemoMessage: TMemo;
    Label7: TLabel;
    ListBoxPITrail: TListBox;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Label6: TLabel;
    ComboServer: TComboBox;
    EditFrom: TEdit;
    EditNumber: TEdit;
    ButtonPage: TButton;
    procedure ipwSNPP1Error(Sender: TObject; ErrorCode: Integer;
      const Description: String);
    procedure ButtonPageClick(Sender: TObject);
    procedure ipwSNPP1PITrail(Sender: TObject; Direction: Integer;
      const Message: String);
    procedure ipwSNPP1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSnpp: TFormSnpp;

implementation

{$R *.DFM}

procedure TFormSnpp.ipwSNPP1Error(Sender: TObject; ErrorCode: Integer;
  const Description: String);
begin
    ListBoxPITrail.Items.Add( IntToStr(ErrorCode) + Description );
    ListBoxPITrail.TopIndex := ListBoxPITrail.Items.Count - 3;
end;



procedure TFormSnpp.ButtonPageClick(Sender: TObject);
begin
// Send page
    ipwSNPP1.ServerName := ComboServer.Text;
    ipwSNPP1.CallerId := EditFrom.Text;
    ipwSNPP1.PagerId := EditNumber.Text;
    ipwSNPP1.Message := MemoMessage.Text + #10#13;
    ipwSNPP1.Send();
    ipwSNPP1.Disconnect();
    ShowMessage( 'Page sent successfully.' );
end;

procedure TFormSnpp.ipwSNPP1PITrail(Sender: TObject; Direction: Integer;
  const Message: String);
begin
    ListBoxPITrail.Items.Add( Message );
    ListBoxPITrail.TopIndex := ListBoxPITrail.Items.Count - 3;
end;

procedure TFormSnpp.ipwSNPP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

end.

//---------------------------------------------------------------------------



