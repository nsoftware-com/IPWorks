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
unit atomf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShellAPI, ipwcore, ipwtypes, ipwatom, StdCtrls, ComCtrls;

type
  TFormAtom = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    lblChannelDescription: TLabel;
    txtURL: TEdit;
    lvwItems: TListView;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    lblFeed: TLabel;
    lblTitle: TLabel;
    txtData: TMemo;
    bRefresh: TButton;
    ipwAtom1: TipwAtom;
    procedure bRefreshClick(Sender: TObject);
    procedure lvwItemsClick(Sender: TObject);
    procedure lblFeedClick(Sender: TObject);
    procedure lblTitleClick(Sender: TObject);
    procedure ipwAtom1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);


  private
    feedURL, titleURL: String;
  public
    { Public declarations }
  end;

var
  FormAtom: TFormAtom;

implementation

{$R *.dfm}

procedure TFormAtom.bRefreshClick(Sender: TObject);
var
  i: integer;
begin
  Screen.Cursor := crHourGlass;
  try
    ipwAtom1.FollowRedirects := frAlways;
    ipwAtom1.GetFeed(txtURL.Text);
  Except on E:Exception do
    MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
  end;
  Screen.Cursor := crDefault;
  lblChannelDescription.Caption := ipwAtom1.ChannelTitle + ' - ' +
    ipwAtom1.ChannelSubtitle;
  lvwItems.Items.Clear();
  For i := 0 To ipwAtom1.EntryCount - 1 do
  begin
    lvwItems.Items.Add();
    lvwItems.Items[lvwItems.Items.Count-1].Caption := ipwAtom1.EntryTitle[i];
    If Not (ipwAtom1.EntryPublished[i] = '') Then
    begin
      lvwItems.Items[lvwItems.Items.Count-1].SubItems.Add(ipwAtom1.EntryPublished[i]);
    end;
  end;
end;

procedure TFormAtom.lvwItemsClick(Sender: TObject);
var
  i: integer;
begin
  i := lvwItems.Selected.Index;
  lblFeed.Caption := ipwAtom1.ChannelTitle;
  feedURL := ipwAtom1.ChannelLinkHRef;
  lblTitle.Caption := ipwAtom1.EntryTitle[i];
  titleURL := ipwAtom1.EntryLinkHref[i];
  txtData.Text := ipwAtom1.EntryContent[i];
end;



procedure TFormAtom.ipwAtom1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
Accept := true;
end;

procedure TFormAtom.lblFeedClick(Sender: TObject);
begin
  if not(feedURL = '') then
    ShellExecute(0, 'open', PChar(feedURL), NIL, NIL, SW_SHOW);
end;

procedure TFormAtom.lblTitleClick(Sender: TObject);
begin
  if not(titleURL = '') then
    ShellExecute(0, 'open', PChar(titleURL), NIL, NIL, SW_SHOW);
end;

end.

