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
unit whoisf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ipwcore, ipwtypes, ExtCtrls, ipwwhois;

type
  TFormWhois = class(TForm)
    mResponse: TMemo;
    Label1: TLabel;
    tDomain: TEdit;
    bLookUp: TButton;
    Label2: TLabel;
    ipwWhois1: TipwWhois;
    procedure bLookUpClick(Sender: TObject);
  end;

var
  FormWhois: TFormWhois;

implementation
{$R *.DFM}

procedure TFormWhois.bLookUpClick(Sender: TObject);

begin
   try
      if tDomain.Text = '' then exit;
      ipwWhois1.Server := 'whois.internic.net';
      ipwWhois1.Query(tDomain.Text);
      mResponse.Lines.Clear;
      mResponse.Lines.Add('Looking up host whois.internic.net...');
      mResponse.Lines.Add('Connecting to ' + ipwWhois1.server + ' ...');
      mResponse.Lines.Add(ipwWhois1.domaininfo);
	except on E: EipwWhois do
      mResponse.Lines.Add('Exception: ' + E.Message);
   end;
end;

end.

