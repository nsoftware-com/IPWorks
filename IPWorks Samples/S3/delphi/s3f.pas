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
unit s3f;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ipwcore, ipwtypes, ipws3, Vcl.Menus,
  Vcl.ToolWin;

type
  TFormS3 = class(TForm)
    ipwS31: TipwS3;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    tAccessKey: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    tSecretKey: TEdit;
    cboProvider: TComboBox;
    bGo: TButton;
    GroupBox2: TGroupBox;
    lvwBuckets: TListView;
    bNewBucket: TButton;
    bDelete: TButton;
    gbObjects: TGroupBox;
    lvwObjects: TListView;
    bNewObject: TButton;
    bDeleteObject: TButton;
    bGetFile: TButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;



    procedure bGoClick(Sender: TObject);
    procedure bNewBucketClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure bDeleteObjectClick(Sender: TObject);
    procedure bGetFileClick(Sender: TObject);
    procedure bNewObjectClick(Sender: TObject);
    procedure lvwBucketsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ipwS31ObjectList(Sender: TObject; const BucketName,
      ObjectName, LastModified: string; Size: Int64; const ETag, OwnerId,
      OwnerName, UploadId, VersionId: string; LatestVersion, Deleted: Boolean);
    procedure ipwS31BucketList(Sender: TObject; const BucketName,
      CreationDate, OwnerId, OwnerName: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormS3: TFormS3;

implementation

{$R *.dfm}

procedure TFormS3.bGoClick(Sender: TObject);
var inputStr:string;
begin
  try
    Screen.Cursor := crHourGlass;
    ipwS31.AccessKey := tAccessKey.Text;
    ipwS31.SecretKey := tSecretKey.Text;

    if cboProvider.ItemIndex = -1 then
      cboProvider.ItemIndex := 0;

    if cboProvider.ItemIndex = 10 then
      ipwS31.ServiceProvider := TipwServiceProviders(255)
    Else
      ipwS31.ServiceProvider := TipwServiceProviders(cboProvider.ItemIndex);

    if ipwS31.ServiceProvider = TipwServiceProviders.spCustom then
      if InputQuery('Enter Custom URL', 'Custom URL?', inputStr) then
        ipwS31.Config('URL='+inputStr);

    if ipwS31.ServiceProvider = TipwServiceProviders.spOracle then
      if InputQuery('Enter Oracle Namespace', 'Oracle Namespace?', inputStr) then
        ipwS31.Config('OracleNamespace='+inputStr);

    lvwBuckets.Items.Clear();
    ipwS31.ListBuckets();
  except on ex: EIPWorks do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormS3.bNewBucketClick(Sender: TObject);
var newBucketName:string;
begin
  if InputQuery('Enter Bucket Name', 'Bucket Name?',
                newBucketName) then
  begin
    try
      Screen.Cursor := crHourGlass;
      ipwS31.Bucket := newBucketName;
      ipwS31.CreateBucket();

      lvwBuckets.Items.Clear();
      ipwS31.ListBuckets();

      gbObjects.Caption := 'Objects in bucket ' + ipwS31.Bucket;
    except on ex: EIPWorks do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormS3.bDeleteClick(Sender: TObject);
begin
  if lvwBuckets.SelCount = 0 then
  begin
    ShowMessage('Select a bucket first!');
    exit;
  end;

  if MessageDlg('Delete bucket ' + ipwS31.Bucket + '?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    try
      Screen.Cursor := crHourGlass;
      ipwS31.DeleteBucket();
      lvwBuckets.Items.Clear();
      ipwS31.ListBuckets();
    except on ex: EIPWorks do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormS3.bDeleteObjectClick(Sender: TObject);
begin
  if lvwObjects.SelCount = 0 then
  begin
    ShowMessage('Select an object first!');
    exit;
  end;

  if MessageDlg('Delete object ' + lvwObjects.Selected.Caption + '?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    try
      Screen.Cursor := crHourGlass;
      ipwS31.DeleteObject(lvwObjects.Selected.Caption);
      lvwObjects.Items.Clear();
      ipwS31.ListObjects();
    except on ex: EIPWorks do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormS3.bGetFileClick(Sender: TObject);
begin
  if lvwObjects.SelCount = 0 then
  begin
    ShowMessage('Select a file first!');
    exit;
  end;
  SaveDialog1.FileName := lvwObjects.Selected.Caption;
  if SaveDialog1.Execute = true then
  begin
    try
      Screen.Cursor := crHourGlass;
      ipwS31.LocalFile := SaveDialog1.FileName;
      ipwS31.GetObject(lvwObjects.Selected.Caption);
    except on ex: EIPWorks do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormS3.bNewObjectClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      Screen.Cursor := crHourGlass;
      ipwS31.LocalFile := OpenDialog1.FileName;
      ipwS31.CreateObject(ExtractFileName(OpenDialog1.FileName));

      lvwObjects.Items.Clear();
      ipwS31.ListObjects();
    except on ex: EIPWorks do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormS3.lvwBucketsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if lvwBuckets.SelCount = 0 then exit;

  try
    Screen.Cursor := crHourGlass;
    ipwS31.Bucket := lvwBuckets.Selected.Caption;

    lvwObjects.Items.Clear();
    ipwS31.ListObjects();
    gbObjects.Caption := 'Objects in bucket ' + lvwBuckets.Selected.Caption;
  except on ex: EIPWorks do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormS3.ipwS31BucketList(Sender: TObject;
  const BucketName, CreationDate, OwnerId, OwnerName: string);
begin
  lvwBuckets.Items.Add();
  lvwBuckets.Items.Item[lvwBuckets.Items.Count - 1].Caption := BucketName;
  lvwBuckets.Items.Item[lvwBuckets.Items.Count - 1].SubItems.Add(CreationDate);
end;

procedure TFormS3.ipwS31ObjectList(Sender: TObject;
  const BucketName, ObjectName, LastModified: string; Size: Int64; const ETag,
  OwnerId, OwnerName, UploadId, VersionId: string; LatestVersion,
  Deleted: Boolean);
begin
  lvwObjects.Items.Add();
  lvwObjects.Items.Item[lvwObjects.Items.Count - 1].Caption := ObjectName;
  lvwObjects.Items.Item[lvwObjects.Items.Count - 1].SubItems.Add(LastModified);
  lvwObjects.Items.Item[lvwObjects.Items.Count - 1].SubItems.Add(IntToStr(Size));
  lvwObjects.Items.Item[lvwObjects.Items.Count - 1].SubItems.Add(OwnerName);
end;

end.



