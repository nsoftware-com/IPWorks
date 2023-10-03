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
unit odataf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Grids, ValEdit, StdCtrls,
  ExtCtrls, ComCtrls, jpeg, ipwcore, ipwtypes, ipwodata, ipwhttp;

type
  TFormOdata = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    Label2: TLabel;
    txtSearch: TEdit;
    lvwSearchResults: TListView;
    Panel2: TPanel;
    btnSearch: TButton;
    ipwOData1: TipwOData;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblUsername: TLabel;
    lblEmail: TLabel;
    lblGender: TLabel;
    lblAge: TLabel;
    lblAddress: TLabel;
    lblCity: TLabel;
    lblCountry: TLabel;
    procedure btnSearchClick(Sender: TObject);
    procedure lvwSearchResultsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ipwOData1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
  private
    queryType: string;
    idArray: Array of string;
  public
    { Public declarations }
  end;

var
  FormOdata: TFormOdata;

implementation

{$R *.dfm}

procedure TFormOdata.btnSearchClick(Sender: TObject);
var
  i: Integer;
  j: Integer;
  Item: TListItem;
begin
  try
    Screen.Cursor := crHourGlass;

    lblUsername.Caption := '';
    lblEmail.Caption := '';
    lblGender.Caption := '';
    lblAge.Caption := '';
    lblAddress.Caption := '';
    lblCity.Caption := '';
    lblCountry.Caption := '';

    lvwSearchResults.Items.Clear;
    ipwOData1.ODataVersion := TipwodataODataVersions.odV4; // OData V4
    ipwOData1.ServiceRootURI :=
      'http://services.odata.org/TripPinRESTierService/';
    ipwOData1.ResourcePath := 'People';
    ipwOData1.QueryFilter := 'contains(LastName, ''' + txtSearch.Text + ''')';
    // ipwOData1.QuerySelect := 'LastName,UserName'; //for returning only LastName, UserName
    ipwOData1.QueryService;

    for i := 0 to ipwOData1.EntryCount - 1 do
    begin
      ipwOData1.EntryIndex := i;
      Item := lvwSearchResults.Items.Add;
      Item.SubItems.Add('');
      for j := 0 to ipwOData1.EntryPropertiesCount - 1 do
      begin
        if ipwOData1.EntryPropertiesName[j] = 'FirstName' then
        begin
          Item.Caption := (ipwOData1.EntryPropertiesValue[j]);
        end;

        if ipwOData1.EntryPropertiesName[j] = 'LastName' then
        begin
          Item.SubItems[0] := (ipwOData1.EntryPropertiesValue[j]);
        end;
      end;
    end;

    ipwOData1.QuerySkipToken := '';
    ipwOData1.QueryFilter := '';
    ipwOData1.QuerySelect := '';
  finally
    Screen.Cursor := crDefault;
  end;
end;


procedure TFormOdata.ipwOData1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept := true;
end;

procedure TFormOdata.lvwSearchResultsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  i: Integer;
begin
  if Selected then
  begin
    ipwOData1.EntryIndex := Item.Index;
    for i := 0 to ipwOData1.EntryPropertiesCount - 1 do
    begin
      if ipwOData1.EntryPropertiesName[i] = 'UserName' then
      begin
        lblUsername.Caption := (ipwOData1.EntryPropertiesValue[i]);
        lblUsername.Visible := True;
      end;

      if ipwOData1.EntryPropertiesName[i] = 'Emails/[1]' then
      begin
        lblEmail.Caption := (ipwOData1.EntryPropertiesValue[i]);
        lblEmail.Visible := True;
      end;

      if ipwOData1.EntryPropertiesName[i] = 'Gender' then
      begin
        lblGender.Caption := (ipwOData1.EntryPropertiesValue[i]);
        lblGender.Visible := True;
      end;

      if ipwOData1.EntryPropertiesName[i] = 'Age' then
      begin
        lblAge.Caption := (ipwOData1.EntryPropertiesValue[i]);
        if ipwOData1.EntryPropertiesValue[i] = '' then
        begin
          lblAge.Caption := 'Not specified';
        end;
        lblAge.Visible := True;
      end;

      if ipwOData1.EntryPropertiesName[i] = 'AddressInfo/[1]/Address' then
      begin
        lblAddress.Caption := (ipwOData1.EntryPropertiesValue[i]);
        lblAddress.Visible := True;
      end;

      if ipwOData1.EntryPropertiesName[i] = 'AddressInfo/[1]/City/Name' then
      begin
        lblCity.Caption := (ipwOData1.EntryPropertiesValue[i]);
        lblCity.Visible := True;
      end;

      if ipwOData1.EntryPropertiesName[i] = 'AddressInfo/[1]/City/CountryRegion'
      then
      begin
        lblCountry.Caption := (ipwOData1.EntryPropertiesValue[i]);
        lblCountry.Visible := True;
      end;
    end;

  end;
end;

end.

