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
unit ldapf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ipwcore, ipwtypes, ipwLDAP, ExtCtrls;

type
  TFormLdap = class(TForm)
    ipwLDAP1: TipwLDAP;
    Memo1: TMemo;
    tServer: TEdit;
    Label2: TLabel;
    tFilter: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
    tBaseDN: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ipwLDAP1SearchComplete(Sender: TObject; MessageId: Integer;
      const DN: String; ResultCode: Integer; const Description: String);
    procedure ipwLDAP1SearchResult(Sender: TObject; MessageId: Integer;
      const DN: String);
    procedure ipwLDAP1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
    procedure ipwLDAP1SSLStatus(Sender: TObject; const Message: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormLdap: TFormLdap;

implementation

{$R *.DFM}

procedure TFormLdap.Button1Click(Sender: TObject);
begin
    Memo1.Clear;
    ipwLDAP1.ServerName := tServer.Text;
    
    //Note that AD, unlike other LDAP servers, requires a bind before a search will be allowed, so the demo would 
    //have to include a few extra lines of code if you are working with AD:
    //ipwLDAP1.DN := binddn;
    //ipwLDAP1.Password := password;
    //ipwLDAP1.Bind();
    
    ipwLDAP1.SearchSizeLimit := 100; // at most 100 results
    ipwLDAP1.Timeout := 0; //set timeout > 0 to use in synch mode
    //set the required attributes
    ipwLDAP1.AttrCount := 1;
    ipwLDAP1.AttrType[0] := 'mail';  // put 'email' for addresses etc...
    ipwLDAP1.DN := tBaseDN.Text;

    Memo1.Lines.Add('sending search request...');

    ipwLDAP1.Search(tFilter.Text);

    Memo1.Lines.Add('waiting for server response...');
    Screen.Cursor := crHourGlass;
end;

procedure TFormLdap.Button2Click(Sender: TObject);
begin
    ipwLDAP1.Interrupt();
end;

procedure TFormLdap.ipwLDAP1SearchComplete(Sender: TObject;
  MessageId: Integer; const DN: String; ResultCode: Integer;
  const Description: String);
begin
    Memo1.Lines.Add('search complete: ' + IntToStr(ResultCode) + ' ' + Description);
    ipwLDAP1.Unbind();
    Screen.Cursor := crDefault;
end;

procedure TFormLdap.ipwLDAP1SearchResult(Sender: TObject; MessageId: Integer;
  const DN: String);
begin
    Memo1.Lines.Add(DN);
end;


procedure TFormLdap.ipwLDAP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormLdap.ipwLDAP1SSLStatus(Sender: TObject; const Message: string);
begin
     Memo1.Lines.Add(Message);
end;

end.


