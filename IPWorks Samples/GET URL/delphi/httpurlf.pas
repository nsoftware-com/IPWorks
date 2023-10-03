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
unit httpurlf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ipwcore, ipwtypes, ipwHTTP,
  ExtCtrls;

type
  TFormHttpurl = class(TForm)
    lTrack: TListBox;
    HTTP1: TipwHTTP;
    Panel1: TPanel;
    Shape1: TShape;
    Label2: TLabel;
    tDiskfile: TEdit;
    Label1: TLabel;
    tURL: TEdit;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure HTTP1Connected(Sender: TObject; StatusCode: Integer;
      const Description: String);
    procedure HTTP1Disconnected(Sender: TObject; StatusCode: Integer;
      const Description: String);
    procedure HTTP1Header(Sender: TObject; const Field, Value: String);
    procedure Button2Click(Sender: TObject);
    procedure HTTP1EndTransfer(Sender: TObject; Direction: Integer);
    procedure HTTP1StartTransfer(Sender: TObject; Direction: Integer);
    procedure HTTP1SSLServerAuthentication(Sender: TObject; CertEncoded: string; CertEncodedB: TArray<System.Byte>;
      const CertSubject, CertIssuer, Status: string; var Accept: Boolean);
    procedure HTTP1Transfer(Sender: TObject; Direction: Integer;
      BytesTransferred: Int64; PercentDone: Integer; Text: string;
      TextB: TArray<System.Byte>);
  private
    { Private declarations }
  public
    { Public declarations }
    lTotal: longint;
  end;

var
  FormHttpurl: TFormHttpurl;

implementation

{$R *.DFM}

procedure TFormHttpurl.Button1Click(Sender: TObject);
begin
    lTotal := MaxLongint;
    HTTP1.FollowRedirects := frAlways;
    HTTP1.LocalFile := tDiskFile.Text;
    HTTP1.Get(tURL.Text);
end;

procedure TFormHttpurl.HTTP1Connected(Sender: TObject; StatusCode: Integer;
  const Description: String);
begin
    lTrack.Items.Add('Connected: ' + Description);
end;

procedure TFormHttpurl.HTTP1Disconnected(Sender: TObject; StatusCode: Integer;
  const Description: String);
begin
    lTrack.Items.Add('Disconnected: ' + Description);
end;

procedure TFormHttpurl.HTTP1Header(Sender: TObject; const Field, Value: String);
begin
    lTrack.Items.Add('Header: ' + Field + ':' + Value);
    if Uppercase(Field) = 'CONTENT-LENGTH' then lTotal := StrToInt(Value);
end;

procedure TFormHttpurl.Button2Click(Sender: TObject);
begin
    http1.Interrupt();
end;

procedure TFormHttpurl.HTTP1EndTransfer(Sender: TObject; Direction: Integer);
begin
    Shape1.Width := 0;
    Panel1.Caption := 'T R A N S F E R R E D';
end;

procedure TFormHttpurl.HTTP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormHttpurl.HTTP1StartTransfer(Sender: TObject; Direction: Integer);
begin
    Shape1.Width := 0;
end;

procedure TFormHttpurl.HTTP1Transfer(Sender: TObject; Direction: Integer;
  BytesTransferred: Int64; PercentDone: Integer; Text: string;
  TextB: TArray<System.Byte>);
begin
    lTrack.Items[lTrack.Items.Count-1] := IntToStr(BytesTransferred) + ' bytes written';
    Shape1.Width := (BytesTransferred * Panel1.Width) div lTotal;
    Panel1.Caption := IntToStr(PercentDone) + '%';
end;

end.


