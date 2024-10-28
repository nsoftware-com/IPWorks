unit wscertf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ipwcertmgr, ipwcore, ipwtypes;

type
  TFormWscert = class(TForm)
    txtInfo: TMemo;
    lCerts: TListBox;
    bSelectCert: TButton;
    cbStores: TListBox;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    ipwCertMgr1: TipwCertMgr;
    procedure FormCreate(Sender: TObject);
    procedure bSelectCertClick(Sender: TObject);
    procedure cbStoresClick(Sender: TObject);
    procedure lCertsClick(Sender: TObject);
    procedure ipwCertMgr1StoreList(Sender: TObject; const CertStore: string);
    procedure ipwCertMgr1CertList(Sender: TObject; CertEncoded: string;
      CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer,
      CertSerialNumber: string; HasPrivateKey: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormWscert: TFormWscert;
  CertHandle: integer;

implementation

{$R *.dfm}

procedure TFormWscert.FormCreate(Sender: TObject);
begin
  CertHandle := 0;
  ipwCertMgr1.ListCertificateStores;
  if cbStores.Items.Count > 0 then
    begin
      cbStores.ItemIndex := 0;
      cbStoresClick(sender);
    end;

end;

procedure TFormWscert.bSelectCertClick(Sender: TObject);
begin
    FormWscert.Close;
end;

procedure TFormWscert.cbStoresClick(Sender: TObject);
begin
  lCerts.Clear;
  txtInfo.Text := '';
  ipwCertMgr1.CertStore := cbStores.Items.Strings[cbStores.ItemIndex];
  ipwCertMgr1.ListStoreCertificates;
  if (lCerts.Items.Count > 0) then
    begin
      lCerts.ItemIndex := 0;
      lCertsClick(Sender);
      bSelectCert.Enabled := True;
    end;
end;

procedure TFormWscert.lCertsClick(Sender: TObject);
var x: string;
begin
    txtInfo.Text := '';
    x :=lCerts.Items.Strings[lCerts.Itemindex];
    ipwCertMgr1.CertSubject := x;
    ipwCertMgr1.CertSubject := lCerts.Items.Strings[lCerts.Itemindex];
    txtInfo.text := txtInfo.text + 'Issuer: ' + ipwCertMgr1.CertIssuer + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Subject: ' + ipwCertMgr1.CertSubject + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Version: ' + ipwCertMgr1.CertVersion + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Serial Number: ' + ipwCertMgr1.CertSerialNumber + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Signature Algorithm: ' + ipwCertMgr1.CertSignatureAlgorithm + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Effective Date: ' + ipwCertMgr1.CertEffectiveDate + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Expiration Date: ' + ipwCertMgr1.CertExpirationDate + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Public Key Algorithm: ' + ipwCertMgr1.CertPublicKeyAlgorithm + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Public Key Length: ' + inttostr(ipwCertMgr1.CertPublicKeyLength) + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Public Key: ' + ipwCertMgr1.CertPublicKey + Chr(13) + Chr(10);
end;

procedure TFormWscert.ipwCertMgr1CertList(Sender: TObject; CertEncoded: string;
  CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer,
  CertSerialNumber: string; HasPrivateKey: Boolean);
begin
  lCerts.Items.Add(CertSubject);
end;

procedure TFormWscert.ipwCertMgr1StoreList(Sender: TObject;
  const CertStore: string);
begin
    cbStores.Items.Add(CertStore);
end;

end.
