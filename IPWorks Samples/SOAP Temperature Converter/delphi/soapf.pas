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
unit soapf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, ipwcore, ipwtypes, ipwsoap;

type
  TFormSoap = class(TForm)
    SOAP1: TipwSOAP;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    txtFahrenheit: TEdit;
    btnFahrenheit: TButton;
    btnCelsius: TButton;
    Label3: TLabel;
    txtCelsius: TEdit;
    procedure SOAP1SSLServerAuthentication(Sender: TObject; CertEncoded: string; CertEncodedB: TArray<System.Byte>;
      const CertSubject, CertIssuer, Status: string; var Accept: Boolean);
    procedure btnFahrenheitClick(Sender: TObject);
    procedure btnCelsiusClick(Sender: TObject);
    procedure InitialSetup();
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSoap: TFormSoap;

implementation

{$R *.DFM}

procedure TFormSoap.InitialSetup();
begin
  SOAP1.Reset();
  SOAP1.Config('TransferredRequest=on');
  SOAP1.Config('MethodNamespacePrefix=');
  SOAP1.URL := 'https://www.w3schools.com/xml/tempconvert.asmx';
  SOAP1.MethodURI := 'https://www.w3schools.com/xml/';
end;

procedure TFormSoap.btnCelsiusClick(Sender: TObject);
begin
  InitialSetup();
  SOAP1.Method := 'FahrenheitToCelsius';
  SOAP1.AddParam('Fahrenheit', txtFahrenheit.Text);

  SOAP1.ActionURI := SOAP1.MethodURI + SOAP1.Method;

  begin
    try
      Screen.Cursor := crHourGlass;
      SOAP1.SendRequest();
    Except
      on E: Exception do
        MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
    end;
    Screen.Cursor := crDefault;
  end;

  SOAP1.XPath :=
    '/Envelope/Body/FahrenheitToCelsiusResponse/FahrenheitToCelsiusResult';

  txtCelsius.Text := SOAP1.XText;

end;

procedure TFormSoap.btnFahrenheitClick(Sender: TObject);
begin
  InitialSetup();
  SOAP1.Method := 'CelsiusToFahrenheit';
  SOAP1.AddParam('Celsius', txtCelsius.Text);
  SOAP1.ActionURI := SOAP1.MethodURI + SOAP1.Method;

  begin
    try
      Screen.Cursor := crHourGlass;
      SOAP1.SendRequest();
    Except
      on E: Exception do
        MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
    end;
    Screen.Cursor := crDefault;
  end;

  SOAP1.XPath :=
    '/Envelope/Body/CelsiusToFahrenheitResponse/CelsiusToFahrenheitResult';
  txtFahrenheit.Text := SOAP1.XText;
end;

procedure TFormSoap.SOAP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept := true;
end;

end.

