unit createcertf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ipwcore, ipwtypes, ipwcertmgr;

type
  TFormCreatecert = class(TForm)
    tSubject: TEdit;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    cbIssuer: TComboBox;
    Label3: TLabel;
    rbSelfSigned: TRadioButton;
    rbSigned: TRadioButton;
    Label2: TLabel;
    bOK: TButton;
    bCancel: TButton;
    tSerialNumber: TEdit;
    ipwCertMgr1: TipwCertMgr;
    procedure bCancelClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ipwCertMgr1CertList(Sender: TObject; CertEncoded: string; CertEncodedB: TArray<System.Byte>;
      const CertSubject, CertIssuer, CertSerialNumber: string;
      HasPrivateKey: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCreatecert: TFormCreatecert;

implementation

{$R *.dfm}

procedure TFormCreatecert.ipwCertMgr1CertList(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer,
  CertSerialNumber: String; HasPrivateKey: Boolean);
begin
  if HasPrivateKey then cbIssuer.Items.Add(CertSubject);
end;

procedure TFormCreatecert.bCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormCreatecert.bOKClick(Sender: TObject);
begin

  if Copy(tSubject.Text, 0, 3) <> 'CN=' then begin
    ShowMessage('Certificate subject must start with "CN="');
    exit;
  end;
  
  if rbSelfSigned.Checked then begin
    ipwCertMgr1.createcertificate(tSubject.Text, StrToInt(tSerialNumber.Text));
  end;
  
  if rbSigned.Checked then begin
    ipwCertMgr1.CertSubject := cbIssuer.Text;
    ipwCertMgr1.IssueCertificate(tSubject.Text, StrToInt(tSerialNumber.Text));
  end;

  ShowMessage('Certificate created and inserted in the store.');

  Close;

end;

procedure TFormCreatecert.FormActivate(Sender: TObject);
begin
  tSerialNumber.Text := IntToStr(StrToInt(tSerialNumber.Text) + 1);
  cbIssuer.items.Clear;
  ipwCertMgr1.ListStoreCertificates;
  if cbIssuer.Items.Count > 0 then cbIssuer.Text := cbIssuer.Items[1];
end;

end.
