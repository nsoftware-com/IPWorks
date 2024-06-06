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
unit ipinfof;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ipwcore, ipwtypes, ipwipinfo, ComCtrls;

type
  TFormIpinfo = class(TForm)
    Label1: TLabel;
    lbNIC: TListBox;
    Label2: TLabel;
    bRelease: TButton;
    bRenew: TButton;
    Label3: TLabel;
    IPInfo1: TipwIPInfo;
    lvwInfo: TListView;
    procedure FormCreate(Sender: TObject);
    procedure bReleaseClick(Sender: TObject);
    procedure bRenewClick(Sender: TObject);
    procedure lbNICClick(Sender: TObject);
    procedure DisplayAdapterInfo();
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormIpinfo: TFormIpinfo;

implementation

{$R *.DFM}

procedure TFormIpinfo.FormCreate(Sender: TObject);
var i:integer;
begin
    for i := 0 to ipinfo1.AdapterCount-1 do begin
        ipinfo1.AdapterIndex := i;
        lbNIC.Items.Add(Ipinfo1.AdapterDescription);
        end;
    If lbNIC.Items.Count > 0 Then begin
        lbNIC.ItemIndex := 0;
        DisplayAdapterInfo();
        end;
end;

procedure TFormIpinfo.bReleaseClick(Sender: TObject);
begin
   Screen.Cursor := crHourGlass;
    Try
        Ipinfo1.DHCPRelease();
        DisplayAdapterInfo();
    except on ex1:Exception do
        begin
        ShowMessage(ex1.Message);
        end;
    end;
   Screen.Cursor := crDefault;
end;

procedure TFormIpinfo.bRenewClick(Sender: TObject);
begin
   Screen.Cursor := crHourGlass;
    Try
        Ipinfo1.DHCPRenew();
        DisplayAdapterInfo();
    except on ex1:Exception do begin
        ShowMessage(ex1.Message);
        end;
    end;
   Screen.Cursor := crDefault;
end;

procedure TFormIpinfo.lbNICClick(Sender: TObject);
begin
    DisplayAdapterInfo();
end;

procedure TFormIpinfo.DisplayAdapterInfo();
begin
   Ipinfo1.AdapterIndex := lbNIC.ItemIndex;
   lvwInfo.Items.Clear();
   lvwInfo.Items.Add();
   lvwInfo.Items[lvwInfo.Items.Count-1].Caption := 'DeviceIndex';
   lvwInfo.Items[lvwInfo.Items.Count-1].SubItems.Add(IntToStr(Ipinfo1.AdapterDeviceIndex));
   lvwInfo.Items.Add();
   lvwInfo.Items[lvwInfo.Items.Count-1].Caption := 'AdapterID';
   lvwInfo.Items[lvwInfo.Items.Count-1].SubItems.Add(Ipinfo1.AdapterName);
   lvwInfo.Items.Add();
   lvwInfo.Items[lvwInfo.Items.Count-1].Caption := 'Description';
   lvwInfo.Items[lvwInfo.Items.Count-1].SubItems.Add(Ipinfo1.AdapterDescription);
   lvwInfo.Items.Add();
   lvwInfo.Items[lvwInfo.Items.Count-1].Caption := 'PhysicalAddress';
   lvwInfo.Items[lvwInfo.Items.Count-1].SubItems.Add(Ipinfo1.AdapterPhysicalAddress);
   lvwInfo.Items.Add();
   lvwInfo.Items[lvwInfo.Items.Count-1].Caption := 'AdapterType';
   lvwInfo.Items[lvwInfo.Items.Count-1].SubItems.Add(Ipinfo1.AdapterType);
   lvwInfo.Items.Add();
   lvwInfo.Items[lvwInfo.Items.Count-1].Caption := 'DHCPEnabled';
   if IPinfo1.AdapterDHCPEnabled then
           lvwInfo.Items[lvwInfo.Items.Count-1].SubItems.Add('TRUE')
   else
           lvwInfo.Items[lvwInfo.Items.Count-1].SubItems.Add('FALSE');
   lvwInfo.Items.Add();
   lvwInfo.Items[lvwInfo.Items.Count-1].Caption := 'DHCPServer';
   lvwInfo.Items[lvwInfo.Items.Count-1].SubItems.Add(Ipinfo1.AdapterDHCPServer);
   lvwInfo.Items.Add();
   lvwInfo.Items[lvwInfo.Items.Count-1].Caption := 'Gateway';
   lvwInfo.Items[lvwInfo.Items.Count-1].SubItems.Add(Ipinfo1.AdapterGateway);
   lvwInfo.Items.Add();
   lvwInfo.Items[lvwInfo.Items.Count-1].Caption := 'SubnetMask';
   lvwInfo.Items[lvwInfo.Items.Count-1].SubItems.Add(Ipinfo1.AdapterSubnetMask);
   lvwInfo.Items.Add();
   lvwInfo.Items[lvwInfo.Items.Count-1].Caption := 'IPAddress';
   lvwInfo.Items[lvwInfo.Items.Count-1].SubItems.Add(Ipinfo1.AdapterIPAddress);
   lvwInfo.Items.Add();
   lvwInfo.Items[lvwInfo.Items.Count-1].Caption := 'OtherIPAddresses';
   lvwInfo.Items[lvwInfo.Items.Count-1].SubItems.Add(Ipinfo1.AdapterOtherIPAddresses);
   lvwInfo.Items.Add();
   lvwInfo.Items[lvwInfo.Items.Count-1].Caption := 'LeaseExpires';
   lvwInfo.Items[lvwInfo.Items.Count-1].SubItems.Add(Ipinfo1.AdapterLeaseExpires);
End;

end.

