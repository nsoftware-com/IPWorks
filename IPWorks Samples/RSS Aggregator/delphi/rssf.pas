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
unit rssf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ipwcore, ipwtypes, ipwrss;

type
  TFormRss = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    txtURL: TEdit;
    lblChannelDescription: TLabel;
    lvwItems: TListView;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    lblFeed: TLabel;
    lblLink: TLabel;
    txtData: TMemo;
    bRefresh: TButton;
    RSS1: TipwRSS;
    procedure lvwItemsClick(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure lblLinkClick(Sender: TObject);
    procedure RSS1SSLServerAuthentication(Sender: TObject; CertEncoded: string; CertEncodedB: TArray<System.Byte>;
      const CertSubject, CertIssuer, Status: string; var Accept: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRss: TFormRss;

implementation

{$R *.DFM}

procedure TFormRss.lvwItemsClick(Sender: TObject);
var i:integer;
begin
    i := lvwItems.Selected.Index;
    lblFeed.Caption := Rss1.ChannelLink;
    lblLink.Caption := Rss1.ItemLink[i];
    txtData.Text := Rss1.ItemDescription[i];
end;

procedure TFormRss.RSS1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormRss.bRefreshClick(Sender: TObject);
var i:integer;
begin
    lvwItems.Items.Clear();
    Screen.Cursor := crHourGlass;
    RSS1.FollowRedirects := frAlways;
    Rss1.GetFeed(txtURL.Text);
    Screen.Cursor := crDefault;
    lblChannelDescription.Caption := Rss1.ChannelTitle;
    lblChannelDescription.Caption := Rss1.ChannelDescription;
    For i := 0 To Rss1.ItemCount-1 do begin
        lvwItems.Items.Add();
        lvwItems.Items[lvwItems.Items.Count-1].Caption := Rss1.ItemTitle[i];
        If Not (Rss1.ItemPubDate[i] = '') Then begin
            lvwItems.Items[lvwItems.Items.Count-1].SubItems.Add(Rss1.ItemPubDate[i]);
        end;
    end;
end;

procedure TFormRss.lblLinkClick(Sender: TObject);
begin
    RSS1.GetURL(RSS1.ItemLink[lvwItems.Selected.Index +1]);
    txtData.Text := RSS1.RSSData;
end;

end.

