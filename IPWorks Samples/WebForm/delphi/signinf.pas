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
unit signinf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ipwcore, ipwtypes, ipwHTTP, StdCtrls, ExtCtrls, ipwwebform;

type
  TFormSignin = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    EditName: TEdit;
    EditEmail: TEdit;
    MemoPublications: TMemo;
    MemoInternet: TMemo;
    ButtonSubmit: TButton;
    EditWhere: TEdit;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    ipwWebForm1: TipwWebForm;
    procedure ButtonSubmitClick(Sender: TObject);
    procedure ipwWebForm1Transfer(Sender: TObject; Direction: Integer;
      BytesTransferred: Int64; PercentDone: Integer; Text: string);
    procedure ipwWebForm1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
  private
    { Private declarations }
    response: String;
  public
    { Public declarations }
  end;

var
  FormSignin: TFormSignin;

implementation

{$R *.DFM}

procedure TFormSignin.ButtonSubmitClick(Sender: TObject);
begin
    if EditEmail.Text = '' then
       ShowMessage( 'Please specify a valid email address.' );

		ipwWebForm1.Reset;

    // set form data
    ipwWebForm1.AddFormVar('regname', EditName.Text);
    ipwWebForm1.AddFormVar('email', EditEmail.Text);
    ipwWebForm1.AddFormVar('about', EditWhere.Text);
    ipwWebForm1.AddFormVar('indpub1', MemoPublications.Text);
    ipwWebForm1.AddFormVar('indnet1', MemoInternet.Text);

    // post the data to the server
    ipwWebForm1.SubmitTo('http://www.nsoftware.com/demos/test/Default.aspx');
    ShowMessage('Thank you!');
    //ShowMessage( response );
end;

procedure TFormSignin.ipwWebForm1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormSignin.ipwWebForm1Transfer(Sender: TObject; Direction: Integer;
  BytesTransferred: Int64; PercentDone: Integer; Text: string);
begin
response := response + Text;
end;

end.


//---------------------------------------------------------------------------


