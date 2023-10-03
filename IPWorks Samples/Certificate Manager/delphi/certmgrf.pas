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
unit certmgrf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls, createcertf, signcsrf,
  generatecsrf, importcsrf, ipwcore, ipwtypes, ipwcertmgr;

type
  TFormCertmgr = class(TForm)
    GroupBox1: TGroupBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Certificates1: TMenuItem;
    CAFunctions1: TMenuItem;
    DeleteCertificate1: TMenuItem;
    N1: TMenuItem;
    ImportCertificate1: TMenuItem;
    ExportCertificate1: TMenuItem;
    N2: TMenuItem;
    Refresh1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    CreateSignCertificate1: TMenuItem;
    N5: TMenuItem;
    GenerateCSR1: TMenuItem;
    ImportSignedCSR1: TMenuItem;
    SignCSR1: TMenuItem;
    GroupBox2: TGroupBox;
    lvCertInfo: TListView;
    pgCertsAndKeys: TPageControl;
    tabUserStores: TTabSheet;
    tabMachineStores: TTabSheet;
    tabPFXStores: TTabSheet;
    tabKeys: TTabSheet;
    tvUserStores: TTreeView;
    Label1: TLabel;
    Label2: TLabel;
    tvMachineStores: TTreeView;
    Label4: TLabel;
    Label3: TLabel;
    lvUserCerts: TListView;
    lvMachineCerts: TListView;
    spStatus: TStatusBar;
    lvKeys: TListView;
    lvPFXCerts: TListView;
    Label5: TLabel;
    tPFXFile: TEdit;
    btnBrowsePFX: TButton;
    btnLoadPFX: TButton;
    dlgOpen: TOpenDialog;
    A1: TMenuItem;
    CreateKey1: TMenuItem;
    lPFXPassword: TLabel;
    ipwCertMgr1: TipwCertMgr;
    procedure pgCertsAndKeysChange(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure tvUserStoresChange(Sender: TObject; Node: TTreeNode);
    procedure tvMachineStoresChange(Sender: TObject; Node: TTreeNode);
    procedure lvUserCertsClick(Sender: TObject);
    procedure lvMachineCertsClick(Sender: TObject);
    procedure btnBrowsePFXClick(Sender: TObject);
    procedure btnLoadPFXClick(Sender: TObject);
    procedure lvPFXCertsClick(Sender: TObject);
    procedure A1Click(Sender: TObject);
    procedure CreateSelfSignedCertificate1Click(Sender: TObject);
    procedure CreateSignCertificate1Click(Sender: TObject);
    procedure SignCSR1Click(Sender: TObject);
    procedure CreateKey1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure ExportCertificate1Click(Sender: TObject);
    procedure GenerateCSR1Click(Sender: TObject);
    procedure DeleteCertificate1Click(Sender: TObject);
    procedure ImportCertificate1Click(Sender: TObject);
    procedure ImportSignedCSR1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ipwCertMgr1StoreList(Sender: TObject; const CertStore: string);
    procedure ipwCertMgr1KeyList(Sender: TObject; const KeyContainer: string;
      KeyType: Integer; const AlgId: string; KeyLen: Integer);
    procedure ipwCertMgr1CertList(Sender: TObject; CertEncoded: string; CertEncodedB: TArray<System.Byte>;
      const CertSubject, CertIssuer, CertSerialNumber: string;
      HasPrivateKey: Boolean);
  private
    { Private declarations }
    procedure ShowCertInfo;
    function SelectCert: boolean;
  public
    { Public declarations }
  end;

var
  FormCertmgr: TFormCertmgr;
  FormCreateCert: TFormCreatecert;
  FormGenerateCSR: TFormGeneratecsr;
  FormImportCSR: TFormImportcsr;
  FormSignCSR: TFormSigncsr;

implementation

{$R *.dfm}

procedure TFormCertmgr.ipwCertMgr1StoreList(Sender: TObject;
  const CertStore: String);
begin
  if pgCertsAndKeys.ActivePage = tabUserStores then
    tvUserStores.Items.AddChild(tvUserStores.Items[0], CertStore);
  if pgCertsAndKeys.ActivePage = tabMachineStores then
    tvMachineStores.Items.AddChild(tvMachineStores.Items[0], CertStore);
end;

procedure TFormCertmgr.pgCertsAndKeysChange(Sender: TObject);
begin
  lvCertInfo.Items.Clear();
  lvCertInfo.Enabled := true;
  if pgCertsAndKeys.ActivePage = tabUserStores then begin
    tvUserStores.Items[0].DeleteChildren;
    lvUserCerts.Items.Clear();
    ipwCertMgr1.ListCertificateStores;
    tvUserStores.Items[0].Expand(true);
  end;

  if pgCertsAndKeys.ActivePage = tabMachineStores then begin
    tvMachineStores.Items[0].DeleteChildren;
    lvMachineCerts.Items.Clear();
    ipwCertMgr1.ListMachineStores;
    tvMachineStores.Items[0].Expand(true);
  end;

  if pgCertsAndKeys.ActivePage = tabKeys then begin
    lvCertInfo.Enabled := false;
    lvKeys.Items.Clear();
    ipwCertMgr1.CertStoreType := cstUser;
    ipwCertMgr1.ListKeys;
  end;
end;

procedure TFormCertmgr.Refresh1Click(Sender: TObject);
begin
  pgCertsAndKeysChange(Sender);
end;

procedure TFormCertmgr.FormActivate(Sender: TObject);
begin
  pgCertsAndKeysChange(Sender);
end;

procedure TFormCertmgr.ipwCertMgr1CertList(Sender: TObject; CertEncoded: string; CertEncodedB: TArray<System.Byte>;
  const CertSubject, CertIssuer, CertSerialNumber: String;
  HasPrivateKey: Boolean);
var
  listView: TListView;
  listItem: TListItem;
begin

  if pgCertsAndKeys.ActivePage = tabUserStores then listView :=  lvUserCerts;
  if pgCertsAndKeys.ActivePage = tabMachineStores then listView := lvMachineCerts;
  if pgCertsAndKeys.ActivePage = tabPFXStores then listView:= lvPFXCerts;
  
  listItem := listView.Items.Add;
  listItem.Caption := CertSubject;
  listItem.SubItems.Add(ipwCertMgr1.CertStore);
  listItem.SubItems.Add(CertEncoded);

end;

procedure TFormCertmgr.tvUserStoresChange(Sender: TObject; Node: TTreeNode);
begin
  lvUserCerts.Items.Clear();
  if tvUserStores.Selected.Parent = nil then exit;
  ipwCertMgr1.CertStoreType := cstUser;
  ipwCertMgr1.CertStore := tvUserStores.Selected.Text;
  ipwCertMgr1.ListStoreCertificates;
  if lvUserCerts.Items.Count> 0 then lvUserCerts.Items.Item[0].Selected := true;
end;

procedure TFormCertmgr.tvMachineStoresChange(Sender: TObject; Node: TTreeNode);
begin
  lvMachineCerts.Items.Clear();
  if tvMachineStores.Selected.Parent = nil then exit;
  ipwCertMgr1.CertStoreType := cstMachine;
  ipwCertMgr1.CertStore := tvMachineStores.Selected.Text;
  ipwCertMgr1.ListStoreCertificates;
  if lvMachineCerts.Items.Count> 0 then lvMachineCerts.Items.Item[0].Selected := true;
end;

procedure TFormCertmgr.lvUserCertsClick(Sender: TObject);
begin
  if lvUserCerts.SelCount > 0 then begin
          ipwCertMgr1.CertStoreType := cstUser;
          ipwCertMgr1.CertStore := lvUserCerts.Selected.SubItems[0];
          ipwCertMgr1.CertSubject := lvUserCerts.Selected.Caption;
          ShowCertInfo;
  end;
end;

procedure TFormCertmgr.lvMachineCertsClick(Sender: TObject);
begin
  if lvMachineCerts.SelCount > 0 then begin
        ipwCertMgr1.CertStoreType := cstMachine;
        ipwCertMgr1.CertStore := lvMachineCerts.Selected.SubItems[0];
        ipwCertMgr1.CertSubject := lvUserCerts.Selected.Caption;
        ShowCertInfo;
        end;
end;

procedure TFormCertmgr.ShowCertInfo;
var
  listItem: TListItem;
begin
  spStatus.SimpleText := '';
  lvCertInfo.Items.Clear();

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Subject';
	listItem.SubItems.Add(ipwCertMgr1.CertSubject);

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Issuer';
	listItem.SubItems.Add(ipwCertMgr1.CertIssuer);

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Version';
	listItem.SubItems.Add(ipwCertMgr1.CertVersion);

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Serial Number';
	listItem.SubItems.Add(ipwCertMgr1.CertSerialNumber);

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Signature Algorithm';
	listItem.SubItems.Add(ipwCertMgr1.CertSignatureAlgorithm);

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Effective Date';
	listItem.SubItems.Add(ipwCertMgr1.CertEffectiveDate);

	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Expiration Date';
	listItem.SubItems.Add(ipwCertMgr1.CertExpirationDate);
	
	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Public Key Algorithm';
	listItem.SubItems.Add(ipwCertMgr1.CertPublicKeyAlgorithm);
	
	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Public Key Length';
	listItem.SubItems.Add(IntToStr(ipwCertMgr1.CertPublicKeyLength));
	
	listItem := lvCertInfo.Items.Add;
	listItem.Caption := 'Public Key';
	listItem.SubItems.Add(ipwCertMgr1.CertPublicKey);

  if ipwCertMgr1.CertPrivateKeyAvailable then
    spStatus.SimpleText := '* You have a private key that is associated with this certificate.';

end;

procedure TFormCertmgr.ipwCertMgr1KeyList(Sender: TObject;
  const KeyContainer: String; KeyType: Integer; const AlgId: String;
  KeyLen: Integer);
var
  listItem: TListItem;
begin
  listItem := lvKeys.Items.Add;
  listItem.Caption := KeyContainer;
  case KeyType of
    1: listItem.SubItems.Add('Signature');
    2: listItem.SubItems.Add('Key Exchange');
    else listItem.SubItems.Add('Unknown: ' + IntToStr(KeyType));
  end;
  listItem.SubItems.Add(IntToStr(KeyLen));
  listItem.SubItems.Add(AlgId);
end;

procedure TFormCertmgr.btnBrowsePFXClick(Sender: TObject);
begin
  if dlgOpen.Execute then tPFXFile.Text := dlgOpen.FileName;
end;

procedure TFormCertmgr.btnLoadPFXClick(Sender: TObject);
begin
  if tPFXFile.Text = '' then btnBrowsePFXClick(Sender);
  lvPFXCerts.Items.Clear();
  ipwCertMgr1.CertStoreType := cstPFXFile;
  lPFXPassword.Caption := InputBox('PFX File Password', 'Please enter the PFX file password:', '');
  ipwCertMgr1.CertStorePassword := lPFXPassword.Caption;
  ipwCertMgr1.CertStore := tPFXFile.Text;
  ipwCertMgr1.ListStoreCertificates;
end;

procedure TFormCertmgr.lvPFXCertsClick(Sender: TObject);
begin
  if lvPFXCerts.SelCount > 0 then begin
        ipwCertMgr1.CertSubject := lvPFXCerts.Selected.Caption;
        ShowCertInfo;
        end;
end;

procedure TFormCertmgr.A1Click(Sender: TObject);
begin
  ShowMessage('IPWorks Certificate Manager');
end;

procedure TFormCertmgr.CreateSelfSignedCertificate1Click(Sender: TObject);
begin
  if FormCreateCert = nil then FormCreateCert := TFormCreateCert.Create(FormCertmgr);
  FormCreateCert.rbSelfSigned.Checked := true;
  FormCreateCert.Show;
  pgCertsAndKeys.ActivePage := tabUserStores
end;

procedure TFormCertmgr.CreateSignCertificate1Click(Sender: TObject);
begin
  if FormCreateCert = nil then FormCreateCert := TFormCreateCert.Create(FormCertmgr);
  FormCreateCert.rbSigned.Checked := true;
  FormCreateCert.Show;
  pgCertsAndKeys.ActivePage := tabUserStores
end;

procedure TFormCertmgr.SignCSR1Click(Sender: TObject);
begin
  if FormSignCSR = nil then FormSignCSR := TFormSignCSR.Create(FormCertmgr);
  FormSignCSR.Show;
end;

procedure TFormCertmgr.CreateKey1Click(Sender: TObject);
var
  keyName: string;
begin
  keyName := InputBox('Create Key', 'Please enter key name:', '');
  if keyName <> '' then ipwCertMgr1.CreateKey(keyName);
  pgCertsAndKeys.ActivePage := tabKeys;
  pgCertsAndKeysChange(Nil);
end;

procedure TFormCertmgr.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormCertmgr.ExportCertificate1Click(Sender: TObject);
var
  password: string;
begin

  if not SelectCert then exit;
  if not dlgOpen.Execute then exit;

  password := InputBox('Export Certificate', 'Please enter a password for the PFX file:', '');
  if password = '' then begin
    ShowMessage('Can''t export certificate without a password');
    exit;
  end;

  ipwCertMgr1.ExportCertificate(dlgOpen.FileName, password);
  ShowMessage('Certificate exported to: ' + dlgOpen.FileName);
end;

procedure TFormCertmgr.GenerateCSR1Click(Sender: TObject);
begin
  if FormGenerateCSR = nil then FormGenerateCSR := TFormGenerateCSR.Create(FormCertmgr);
  FormGenerateCSR.Show;
end;

function TFormCertmgr.SelectCert: boolean;
begin
  SelectCert := false;
  ipwCertMgr1.Reset;
  
  if pgCertsAndKeys.ActivePage = tabUserStores then begin
    ipwCertMgr1.CertStoreType := cstUser;
    if lvUserCerts.SelCount > 0 then begin
      ipwCertMgr1.CertStore := tvUserStores.Selected.Text;
      ipwCertMgr1.CertSubject := lvUserCerts.Selected.Caption;
    end;
  end;

  if pgCertsAndKeys.ActivePage = tabMachineStores then begin
    ipwCertMgr1.CertStoreType := cstMachine;
    if lvMachineCerts.SelCount > 0 then begin
      ipwCertMgr1.CertStore := tvMachineStores.Selected.Text;
      ipwCertMgr1.CertSubject := lvUserCerts.Selected.Caption;
    end;
  end;

  if ipwCertMgr1.CertSubject = '' then begin
    ShowMessage('Please select a certificate in a User or Machine store first.');
    exit;
  end;

  SelectCert := true;
end;

procedure TFormCertmgr.DeleteCertificate1Click(Sender: TObject);
begin
  if not SelectCert then exit;

  if MessageDlg('Are you sure you want to delete the certificate with subject: ' +
                 ipwCertMgr1.CertSubject + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    ipwCertMgr1.DeleteCertificate;
  end;

  if pgCertsAndKeys.ActivePage = tabUserStores then tvUserStoresChange(Sender, nil);
  if pgCertsAndKeys.ActivePage = tabMachineStores then tvMachineStoresChange(Sender, nil);
end;

procedure TFormCertmgr.ImportCertificate1Click(Sender: TObject);
begin

  if (pgCertsAndKeys.ActivePage <> tabPFXStores) or (lvPFXCerts.Items.Count = 0)  or (lvPFXCerts.Selected = nil) then begin
    ShowMessage('Please select a certificate in a PFX file first.');
    pgCertsAndKeys.ActivePage := tabPFXStores;
    exit;
  end;

  //this imports into the 'MY' store, but any other store would work as well
  ipwCertMgr1.CertStoreType := cstUser;
  ipwCertMgr1.CertStore := 'MY';
  ipwCertMgr1.ImportCertificate(tPFXFile.Text, lPFXPassword.Caption, lvPFXCerts.Selected.Caption);
  pgCertsAndKeys.ActivePage := tabUserStores;
  Refresh1Click(nil);
  
end;

procedure TFormCertmgr.ImportSignedCSR1Click(Sender: TObject);
begin
  if FormImportCSR = nil then FormImportCSR := TFormImportCSR.Create(FormCertmgr);
  FormImportCSR.Show;
  pgCertsAndKeys.ActivePage := tabUserStores;
  Refresh1Click(nil);
end;

procedure TFormCertmgr.FormCreate(Sender: TObject);
begin
  tvUserStores.Items.AddFirst(nil,'User Certificate Stores');
  tvMachineStores.Items.AddFirst(nil,'Machine Certificate Stores');
end;

end.


