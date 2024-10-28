unit generatecsrf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ipwcore, ipwtypes, ipwcertmgr;

type
  TFormGeneratecsr = class(TForm)
    tCSR: TMemo;
    bSign: TButton;
    bOK: TButton;
    Label4: TLabel;
    tSubject: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    cbKey: TComboBox;
    ipwCertMgr1: TipwCertMgr;
    procedure bSignClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ipwCertMgr1KeyList(Sender: TObject; const KeyContainer: string;
      KeyType: Integer; const AlgId: string; KeyLen: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormGeneratecsrf: TFormGeneratecsr;

implementation

{$R *.dfm}

procedure TFormGeneratecsr.bSignClick(Sender: TObject);
begin

  if Copy(tSubject.Text, 0, 3) <> 'CN=' then begin
    ShowMessage('Certificate subject must start with "CN="');
    exit;
  end;

  tCSR.Text := ipwCertMgr1.generatecsr(tSubject.Text, cbKey.Text);

end;

procedure TFormGeneratecsr.ipwCertMgr1KeyList(Sender: TObject;
  const KeyContainer: String; KeyType: Integer; const AlgId: String;
  KeyLen: Integer);
begin
  cbKey.Items.Add(KeyContainer);
end;

procedure TFormGeneratecsr.bOKClick(Sender: TObject);
begin
  Close;
end;

procedure TFormGeneratecsr.FormActivate(Sender: TObject);
begin
  cbKey.Items.Clear;
  ipwCertMgr1.ListKeys;
  cbKey.ItemIndex := 0;
end;

end.
