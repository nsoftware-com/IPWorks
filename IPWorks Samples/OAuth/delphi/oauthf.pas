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
unit oauthf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Grids, ValEdit, StdCtrls,
  ExtCtrls, ComCtrls, jpeg, ipwcore, ipwtypes, ipwoauth, ipwjson, ipwhttp;

type
  TFormOauth = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    txtClientID: TEdit;
    txtClientSecret: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Panel2: TPanel;
    txtServerAuthURL: TEdit;
    txtServerTokenURL: TEdit;
    txtAuthScope: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    btnAuthorize: TButton;
    Panel3: TPanel;
    txtAuthString: TEdit;
    Label7: TLabel;
    btnGetUserInfo: TButton;
    Panel4: TPanel;
    Label8: TLabel;
    Label9: TLabel;
    lblEmail: TLabel;
    lblVerified: TLabel;
    Panel5: TPanel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    ipwOAuth1: TipwOAuth;
    ipwJSON1: TipwJSON;
    ipwHTTP1: TipwHTTP;
    procedure btnAuthorizeClick(Sender: TObject);
    procedure btnGetUserInfoClick(Sender: TObject);
    procedure ipwOAuth1ReturnURL(Sender: TObject; const URLPath,
      QueryString: string; var ResponseHeaders, ResponseBody: string);
    procedure ipwOAuth1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
    procedure ipwOAuth1LaunchBrowser(Sender: TObject; var URL, Command: string);
    procedure ipwHTTP1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);

  private

  public
    { Public declarations }
  end;

var
  FormOauth: TFormOauth;

implementation

{$R *.dfm}

procedure TFormOauth.btnAuthorizeClick(Sender: TObject);
begin
  lblEmail.Caption := '';
  lblVerified.Caption := '';

  ipwOAuth1.ClientId := txtClientID.Text;
  ipwOAuth1.ClientSecret := txtClientSecret.Text;
  ipwOAuth1.ServerAuthURL := txtServerAuthURL.Text;
  ipwOAuth1.ServerTokenURL := txtServerTokenURL.Text;
  ipwOAuth1.AuthorizationScope := txtAuthScope.Text;

  txtAuthString.Text := ipwOAuth1.GetAuthorization();
end;

procedure TFormOauth.btnGetUserInfoClick(Sender: TObject);
begin

  ipwHTTP1.Authorization := txtAuthString.Text;
  ipwHTTP1.Get('https://www.googleapis.com/oauth2/v1/userinfo');
  ipwJSON1.InputData :=  ipwHTTP1.TransferredData;
  ipwJSON1.Parse();
  ipwJSON1.XPath := '/json/email';
  lblEmail.Caption := StringReplace(ipwJSON1.XText, '"', '', [rfReplaceAll]);
  ipwJSON1.XPath := '/json/verified_email';
  lblVerified.Caption := ipwJSON1.XText;

end;


procedure TFormOauth.ipwHTTP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
    Accept:=true;
end;

procedure TFormOauth.ipwOAuth1LaunchBrowser(Sender: TObject; var URL,
  Command: string);
begin
    //This event is fired before the launching of a browser
  //(i.e. when GetAuthorization is called).

  //The Command parameter provides you with command that will be executed by the
  // component. The URL parameter provides you with the authorization URL which
  // the user will be directed to authenticate. Both the Command and URL
  // parameters can be overridden in this event.
end;

procedure TFormOauth.ipwOAuth1ReturnURL(Sender: TObject; const URLPath,
  QueryString: string; var ResponseHeaders, ResponseBody: string);
begin
  //This event is fired when the user is redirected to the embedded web server
  // (i.e. after successfully authenticating and allowing access).

  //You can set the ResponseHeaders and ResponseBody parameters to provide a
  // custom response that the user will see in their browser.
end;


procedure TFormOauth.ipwOAuth1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

end.

