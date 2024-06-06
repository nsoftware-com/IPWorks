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
unit webdavf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ipwcore, ipwtypes, ipwwebdav;

type
  TFormWebdav = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    tbURL: TEdit;
    lvwLocal: TListView;
    lvwRemote: TListView;
    bDownload: TButton;
    bUpload: TButton;
    bGo: TButton;
    LabelUsername: TLabel;
    LabelPassword: TLabel;
    tbUsername: TEdit;
    tbPassword: TEdit;
    Webdav1: TipwWebDAV;
    procedure lvwLocalDblClick(Sender: TObject);
    procedure bUploadClick(Sender: TObject);
    procedure bDownloadClick(Sender: TObject);
    procedure bGoClick(Sender: TObject);
    procedure RemoteDirRefresh;
    procedure LocalDirRefresh;

    procedure Webdav1SSLServerAuthentication(Sender: TObject;
      const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
    procedure Webdav1DirList(Sender: TObject; const ResourceURI, DisplayName,
      ContentLanguage: string; ContentLength: Int64; const ContentType,
      LastModified: string);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormWebdav: TFormWebdav;

implementation

{$R *.DFM}

procedure TFormWebdav.RemoteDirRefresh;
begin
  lvwRemote.Items.Clear;
  Webdav1.Depth := TipwTDepths.dpImmediateChildren;
  Webdav1.User := tbUsername.Text;
  Webdav1.Password := tbPassword.Text;
  Webdav1.ListDirectory(tbURL.Text);
end;

procedure TFormWebdav.LocalDirRefresh;
var
  SearchRec: TSearchRec;
begin
  // Show updated file list for the local system
  // we know the value of CurrentLocDirectory  here

  Screen.Cursor := crAppStart;
  lvwLocal.Items.Clear;

  if FindFirst('*', faAnyFile, SearchRec) = 0 then
  repeat
    if (SearchRec.Attr and faDirectory) <> 0 then
    begin
      lvwLocal.Items.Add;
      lvwLocal.Items[lvwLocal.Items.Count - 1].Caption := '<DIR>  ' + SearchRec.Name;
    end
    else
    begin
      lvwLocal.Items.Add;
      lvwLocal.Items[lvwLocal.Items.Count - 1].Caption := SearchRec.Name;
      lvwLocal.items[lvwLocal.Items.Count - 1].SubItems.Add(IntToStr(SearchRec.Size div 1000) + ' KB');
    end;
  until FindNext(SearchRec) <> 0;

  FindClose(SearchRec);
  Screen.Cursor := crDefault;
end;

procedure TFormWebdav.lvwLocalDblClick(Sender: TObject);
begin
  ChDir('..');
  LocalDirRefresh;
end;

procedure TFormWebdav.bUploadClick(Sender: TObject);
var
  url:string;
begin
  url := tbURL.Text;
  //upload the file
  Webdav1.LocalFile := GetCurrentDir + '\' + lvwLocal.Selected.Caption;
  Webdav1.PutResource(url + '\' + lvwLocal.Selected.Caption);
  RemoteDirRefresh;
end;

procedure TFormWebdav.bDownloadClick(Sender: TObject);
var
  path:String;
begin
  path := GetCurrentDir;
  //download the file;
  Webdav1.LocalFile := path + '\' + lvwRemote.Selected.Caption;
  Webdav1.GetResource(tbURL.Text + lvwRemote.Selected.Caption);
  LocalDirRefresh;
end;

procedure TFormWebdav.bGoClick(Sender: TObject);
begin
  LocalDirRefresh;
  RemoteDirRefresh;
end;



procedure TFormWebdav.Webdav1DirList(Sender: TObject; const ResourceURI,
  DisplayName, ContentLanguage: string; ContentLength: Int64; const ContentType,
  LastModified: string);
begin
  lvwRemote.Items.Add;
  if (ResourceURI.EndsWith('/')) then
  begin
    lvwRemote.Items[lvwRemote.Items.Count - 1].Caption := '<DIR>  ' + DisplayName;
  end
  else
  begin
    lvwRemote.Items[lvwRemote.Items.Count - 1].Caption := DisplayName;
    lvwRemote.Items[lvwRemote.Items.Count - 1].SubItems.add(IntToStr(ContentLength));
  end;
  lvwRemote.Items[lvwRemote.Items.Count - 1].SubItems.add(LastModified);
end;

procedure TFormWebdav.Webdav1SSLServerAuthentication(Sender: TObject;
  const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
  CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept:=true;
end;

end.

