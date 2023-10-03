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
unit dnsf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ipwcore, ipwtypes, ipwdns;

type
  TFormDns = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    tbDNS: TEdit;
    Label3: TLabel;
    txtDomain: TEdit;
    bQuery: TButton;
    lvwRecords: TListView;
    dns1: TipwDNS;
    procedure bQueryClick(Sender: TObject);
    procedure dns1_OnResponse(Sender: TObject; RequestId: Integer;
      const Domain: String; StatusCode: Integer; const Description: String;
      Authoritative: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDns: TFormDns;

implementation

{$R *.DFM}

procedure TFormDns.bQueryClick(Sender: TObject);
var i:integer;
begin
        dns1.DNSServer := tbDNS.Text;
        lvwRecords.Items.Clear();
        for i := 0 to 20 do begin
                dns1.QueryType := TipwdnsQueryTypes(i);
                dns1.Query(txtDomain.Text);
	end;
end;

procedure TFormDns.dns1_OnResponse(Sender: TObject; RequestId: Integer;
  const Domain: String; StatusCode: Integer; const Description: String;
  Authoritative: Boolean);
var i, f : integer;
begin
    //record, type, name, value
    If StatusCode <> 0 Then begin
        //No records were found, output a message and exit the sub
        lvwRecords.Items.Add();
        lvwRecords.Items.Item[lvwRecords.Items.Count-1].Caption := IntToStr(RequestId);
        lvwRecords.Items.Item[lvwRecords.Items.Count-1].SubItems.Add('-');
        lvwRecords.Items.Item[lvwRecords.Items.Count-1].SubItems.Add('N/A');
        lvwRecords.Items.Item[lvwRecords.Items.Count-1].SubItems.Add('N/A');
        end
    else begin
            For i := 0 To Dns1.RecordCount -1 do begin
                For f := 0 To Dns1.RecordFieldCount[i] -1 do begin
                    Dns1.RecordFieldIndex[i] := f;
                        If f = 0 Then begin
                            lvwRecords.Items.Add();
                            lvwRecords.Items.Item[lvwRecords.Items.Count-1].Caption := IntToStr(RequestId);
                            lvwRecords.Items.Item[lvwRecords.Items.Count-1].SubItems.Add(dns1.RecordTypeName[i]);
                            lvwRecords.Items.Item[lvwRecords.Items.Count-1].SubItems.Add(dns1.RecordFieldName[i]);
                            lvwRecords.Items.Item[lvwRecords.Items.Count-1].SubItems.Add(dns1.RecordFieldValue[i]);
                            end
                        else begin
                            lvwRecords.Items.Add();
                            lvwRecords.Items.Item[lvwRecords.Items.Count-1].Caption := '';
                            lvwRecords.Items.Item[lvwRecords.Items.Count-1].SubItems.Add('');
                            lvwRecords.Items.Item[lvwRecords.Items.Count-1].SubItems.Add(dns1.RecordFieldName[i]);
                            lvwRecords.Items.Item[lvwRecords.Items.Count-1].SubItems.Add(dns1.RecordFieldValue[i]);
                        end;
                end; //f
            end; //i
    end;

end;

procedure TFormDns.FormCreate(Sender: TObject);
begin
    tbDNS.Text := Dns1.DNSServer;
end;

end.

