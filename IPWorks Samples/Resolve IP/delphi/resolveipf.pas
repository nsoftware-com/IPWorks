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
unit resolveipf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ipwcore, ipwtypes, ExtCtrls,  ipwIPInfo;

type
  TFormResolveip = class(TForm)
    IPInfo1: TipwIPInfo;
    lPending: TListBox;
    lTrack: TListBox;
    Label3: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    Label4: TLabel;
    tHost: TEdit;
    Button1: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure IPInfo1RequestComplete(Sender: TObject; RequestID,
      StatusCode: integer; const Description: String);
    procedure tHostKeyPress(Sender: TObject; var Key: Char);
    procedure IPInfo1Error(Sender: TObject; ErrorCode: Integer;
      const Description: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormResolveip: TFormResolveip;

const
	HOST = 1;
    SRVC = 2;

implementation

{$R *.DFM}

var
	Reqs: TStringList;

procedure TFormResolveip.IPInfo1RequestComplete(Sender: TObject; RequestID,
  StatusCode: integer; const Description: String);
var
	i, idx: integer;
	query: string;
begin
	label3.caption := IntToStr(IPInfo1.PendingRequests) + ' pending requests';
    idx := Reqs.IndexOf(IntToStr(RequestID));
    if idx <> -1
    then begin
        query := lPending.Items[idx];
        Delete(query, 1, pos('.', query) + 1);
        lPending.Items.Delete(idx);
    end
    else exit;

    if StatusCode <> 0
    then lTrack.Items.Add(query + '->' + Description)
    else begin
     lTrack.Items.Add(IPInfo1.HostName + ' has address ' + IPInfo1.HostAddress);
     if IPInfo1.HostAliases <> '' then lTrack.Items.Add('alias ' + '. ' + IPInfo1.HostAliases);
     if IPInfo1.OtherAddresses <> '' then lTrack.Items.Add('other addresses ' +  '. ' + IPInfo1.OtherAddresses);
    end;
    Reqs.Delete(idx);
end;

procedure TFormResolveip.Button1Click(Sender: TObject);
var
	i: integer;
    bAddr: Boolean;
begin
    {is it a hostname or a hostaddress to query?}
    bAddr := TRUE;
    for i:=1 to Length(tHost.Text) do
        if not (tHost.Text[i] in ['0'..'9','.'])
        then bAddr := FALSE;

    if bAddr
    then IPInfo1.HostAddress := tHost.Text
    else IPInfo1.HostName := tHost.Text;

    {display request}
    label3.caption := IntToStr(IPInfo1.PendingRequests) + ' pending requests';
    lPending.Items.Add(IntToStr(IPInfo1.RequestID) + '. ' + tHost.Text);
    {register this request No}
    Reqs.AddObject(IntToStr(IPInfo1.RequestID), TObject(HOST));
end;

procedure TFormResolveip.FormCreate(Sender: TObject);
begin
	Reqs := TStringList.Create;
    Reqs.Sorted := FALSE;
    lPending.Sorted := FALSE;
    label3.caption := IntToStr(IPInfo1.PendingRequests) + ' pending requests';

	tHost.Text := 'www.ibm.com'; Button1Click(Sender);
	tHost.Text := 'www.nsoftware.com'; Button1Click(Sender);
	tHost.Text := 'www.novell.com'; Button1Click(Sender);
	tHost.Text := 'www.sun.com'; Button1Click(Sender);
	tHost.Text := 'www.apple.com'; Button1Click(Sender);

end;

procedure TFormResolveip.Button3Click(Sender: TObject);
begin
     IPInfo1.PendingRequests := 0;
    lPending.Clear;
    Reqs.Clear;
    label3.caption := IntToStr(IPInfo1.PendingRequests) + ' pending requests';
end;

procedure TFormResolveip.tHostKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #13 then
    begin
        Button1.Click;
        Key := #0;
    end;
end;

procedure TFormResolveip.IPInfo1Error(Sender: TObject; ErrorCode: Integer;
  const Description: String);
begin
    lTrack.Items.Add(Description);
end;

end.

