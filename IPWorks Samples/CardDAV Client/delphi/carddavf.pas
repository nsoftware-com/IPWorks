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
unit carddavf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, StrUtils, DateUtils,
  carddavaddcontactf, ipwcore, ipwtypes, ipwoauth, ipwcarddav;

type
  TFormCarddav = class(TForm)
    demoInfoLabel: TLabel;
    setupInfoBox: TGroupBox;
    usernameLabel: TLabel;
    urlLabel: TLabel;
    txtUsername: TEdit;
    txtURL: TEdit;
    btnReport: TButton;
    btnAddContact: TButton;
    btnDeleteContact: TButton;
    btnExportVCF: TButton;
    contactDetailsBox: TGroupBox;
    lvwContactDetails: TListView;
    saveDialog: TSaveDialog;
    txtSecret: TEdit;
    txtID: TEdit;
    secretLabel: TLabel;
    idLabel: TLabel;
    ipwOAuth1: TipwOAuth;
    txtPassword: TEdit;
    ipwCardDAV1: TipwCardDAV;
    procedure btnReportClick(Sender: TObject);
    procedure btnAddContactClick(Sender: TObject);
    procedure btnDeleteContactClick(Sender: TObject);
    procedure btnExportVCFClick(Sender: TObject);
    procedure ipwCardDAV1ContactDetails(Sender: TObject;
      const ResourceURI, ResponseStatus, ETag, ContactData: string);
    procedure ipwCardDAV1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TBytes; const CertSubject, CertIssuer,
      Status: string; var Accept: Boolean);
  private
    procedure ListContacts();
    procedure Authenticate();
  public
  end;

var
  FormCarddav: TFormCarddav;

implementation

{$R *.dfm}

procedure TFormCarddav.btnAddContactClick(Sender: TObject);
var
  addContactForm: TFormCarddavAddContact;
  today: TDateTime;
  formattedDate: string;
  formattedTime: string;
begin
  addContactForm := TFormCarddavAddContact.Create(self);
  try
    if addContactForm.ShowModal() = mrOk then
    begin
      Screen.Cursor := crHourGlass;
      lvwContactDetails.Items.Clear();
      Authenticate();
      today := Now;
      DateTimeToString(formattedDate, 'YYYYMMDD', today);
      DateTimeToString(formattedTime, 'hhmmss', today);
      ipwCardDAV1.UID := formattedDate + 'T' + formattedTime + 'Z';
      ipwCardDAV1.FormattedName := addContactForm.txtFormattedName.Text;
      ipwCardDAV1.PhoneNumberCount := 1;
      ipwCardDAV1.PhoneNumberValue[0] := addContactForm.txtPhoneNumber.Text;
      ipwCardDAV1.EMailCount := 1;
      ipwCardDAV1.EMailValue[0] := addContactForm.txtEmail.Text;
      ipwCardDAV1.AddressCount := 1;
      ipwCardDAV1.AddressValue[0] := addContactForm.txtAddress.Text;
      ipwCardDAV1.CreateContact(txtURL.Text + '/' + ipwCardDAV1.UID + '.vcf');
      // List the contacts.
      ListContacts();
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message);
    end;
  end;
  Screen.Cursor := crDefault;
  addContactForm.Release();
end;

procedure TFormCarddav.btnDeleteContactClick(Sender: TObject);
begin
  if lvwContactDetails.SelCount <= 0 then
  begin
    ShowMessage('Please select a contact.');
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    Authenticate();
    ipwCardDAV1.DeleteContact(lvwContactDetails.Selected.SubItems[3]);
    ListContacts();
  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message);
    end;
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormCarddav.btnExportVCFClick(Sender: TObject);
var
  vcffile: TextFile;
begin
  if lvwContactDetails.SelCount <= 0 then
  begin
    ShowMessage('Please select a contact.');
    Exit;
  end;

  saveDialog.Title := 'Save VCF File.';
  saveDialog.Filter := '(*.vcf)|*.vcf';
  saveDialog.DefaultExt := 'vcf';
  if saveDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      Authenticate();
      ipwCardDAV1.GetContact(lvwContactDetails.Selected.SubItems[3]);
    except
      on E: Exception do
        ShowMessage('Exception: ' + E.Message);
    end;

    try
      AssignFile(vcffile, saveDialog.FileName);
      ReWrite(vcffile);
      Write(vcffile, ipwCardDAV1.ExportVCF());
    finally
      CloseFile(vcffile);
      Screen.Cursor := crDefault;
    end;

    ListContacts();
  end;
end;

procedure TFormCarddav.btnReportClick(Sender: TObject);
begin
  Authenticate();
  ListContacts();
end;

procedure TFormCarddav.ipwCardDAV1ContactDetails(Sender: TObject;
  const ResourceURI, ResponseStatus, ETag, ContactData: string);
var
  listItem: TListItem;
begin
  listItem := lvwContactDetails.Items.Add();
  listItem.Caption := ipwCardDAV1.FormattedName;
  if ipwCardDAV1.PhoneNumberCount > 0 then
  begin
    listItem.SubItems.Add(ipwCardDAV1.PhoneNumberValue[0]);
  end
  else
  begin
    listItem.SubItems.Add(' - ');
  end;
  if ipwCardDAV1.EMailCount > 0 then
  begin
    listItem.SubItems.Add(ipwCardDAV1.EMailValue[0]);
  end
  else
  begin
    listItem.SubItems.Add(' - ');
  end;
  if ipwCardDAV1.AddressCount > 0 then
  begin
    listItem.SubItems.Add(ipwCardDAV1.AddressValue[0]);
  end
  else
  begin
    listItem.SubItems.Add(' - ');
  end;
  listItem.SubItems.Add(ResourceURI);
end;


procedure TFormCarddav.ipwCardDAV1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TBytes; const CertSubject, CertIssuer,
  Status: string; var Accept: Boolean);
begin
  Accept := true;
end;

procedure TFormCarddav.ListContacts();
begin
  Screen.Cursor := crHourGlass;
  try
    lvwContactDetails.Items.Clear();
     ipwCardDAV1.GetAddressbookReport(txtURL.Text);
  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message + E.StackTrace);
    end;
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormCarddav.Authenticate();
begin
  Screen.Cursor := crHourGlass;
   ipwCardDAV1.User := txtUsername.Text;
  if (txtID.Text = '') and (txtSecret.Text = '') then
  begin
     ipwCardDAV1.Password := txtPassword.Text;
  end
  else
  begin
    ipwOAuth1.ClientId := txtID.Text;
    ipwOAuth1.ClientSecret := txtSecret.Text;
    ipwOAuth1.ServerAuthURL := 'https://accounts.google.com/o/oauth2/auth';
    ipwOAuth1.ServerTokenURL := 'https://accounts.google.com/o/oauth2/token';
    ipwOAuth1.AuthorizationScope := 'https://www.googleapis.com/auth/carddav';
    ipwCardDAV1.AuthScheme := authOAuth;
    ipwCardDAV1.Authorization := ipwOAuth1.GetAuthorization();
  end;
  Screen.Cursor := crDefault;
end;

end.

