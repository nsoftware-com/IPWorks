unit importcsrf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ipwcore, ipwtypes, ipwcertmgr;

type
  TFormImportcsr = class(TForm)
    tCSR: TMemo;
    bOK: TButton;
    bCancel: TButton;
    Label4: TLabel;
    Label2: TLabel;
    cbKey: TComboBox;
    ipwCertMgr1: TipwCertMgr;
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ipwCertMgr1KeyList(Sender: TObject; const KeyContainer: string;
      KeyType: Integer; const AlgId: string; KeyLen: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormImportcsr: TFormImportcsr;

implementation

{$R *.dfm}

procedure TFormImportcsr.ipwCertMgr1KeyList(Sender: TObject;
  const KeyContainer: String; KeyType: Integer; const AlgId: String;
  KeyLen: Integer);
begin
  cbKey.Items.Add(KeyContainer);
end;

procedure TFormImportcsr.bOKClick(Sender: TObject);
begin
  //this imports into the 'MY' store, but any other store would work as well
  ipwCertMgr1.CertStoreType := cstUser;
  ipwCertMgr1.CertStore := 'MY';
  ipwCertMgr1.ImportSignedCSR(TEncoding.Default.GetBytes(tCSR.Text), cbKey.Text);
  Close();
end;

procedure TFormImportcsr.bCancelClick(Sender: TObject);
begin
        Close();
end;

procedure TFormImportcsr.FormActivate(Sender: TObject);
begin
  cbKey.Items.Clear;
  ipwCertMgr1.ListKeys;
  cbKey.ItemIndex := 0;
end;

end.
