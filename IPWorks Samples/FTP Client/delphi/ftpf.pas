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
unit ftpf;

interface

uses
{$IFDEF LINUX}
  QForms, QExtCtrls, QStdCtrls, QControls, SysUtils, Classes, ipwcore, ipwtypes, ipwftp,
  ExtCtrls, StdCtrls, Controls;
{$ELSE}
  Windows, Messages, SysUtils, Graphics, Controls, Classes, Forms, Dialogs, StdCtrls, ExtCtrls,
  ipwcore, ipwtypes, ipwftp;
{$ENDIF}

const
  OffLineConst    = 1;
  OnLineConst     = 2;

type
  TFormFtp = class(TForm)
    GroupBox1: TGroupBox;
    ComboBoxLocHistory: TComboBox;
    ListBoxLocFiles: TListBox;
    ButtonLocChgDir: TButton;
    ButtonLocMkDir: TButton;
    ButtonLocRename: TButton;
    ButtonLocDelete: TButton;
    ButtonLocRefresh: TButton;
    ButtonDownload: TButton;
    ButtonUpload: TButton;
    ListBoxPiTrail: TListBox;
    ButtonConnectDisconnect: TButton;
    ButtonCancel: TButton;
    ButtonExit: TButton;
    GroupBox2: TGroupBox;
    ComboBoxRemHistory: TComboBox;
    ListBoxRemFiles: TListBox;
    ButtonRemChgDir: TButton;
    ButtonRemMkDir: TButton;
    ButtonRemRename: TButton;
    ButtonRemDelete: TButton;
    ButtonRemRefresh: TButton;
    RadioGroupTransferMode: TRadioGroup;
    ipwFTP1: TipwFTP;

    procedure UpdateLocal;
    procedure UpdateRemote;
    procedure UpdateNotes(Note: String);

    procedure ButtonConnectDisconnectClick( Sender: TObject);
    procedure ButtonExitClick( Sender: TObject );
    procedure ListBoxLocFilesDblClick( Sender: TObject );
    procedure ListBoxRemFilesDblClick( Sender: TObject );
    procedure ButtonCancelClick( Sender: TObject );
    procedure RadioGroupTransferModeClick( Sender: TObject );
    procedure ButtonRemChgDirClick( Sender: TObject );
    procedure ButtonRemMkDirClick( Sender: TObject );
    procedure ButtonRemRenameClick( Sender: TObject );
    procedure ButtonRemDeleteClick( Sender: TObject );
    procedure ButtonRemRefreshClick( Sender: TObject );
    procedure ButtonLocChgDirClick( Sender: TObject );
    procedure ButtonLocMkDirClick( Sender: TObject );
    procedure ButtonLocRenameClick( Sender: TObject );
    procedure ButtonLocDeleteClick( Sender: TObject );
    procedure ButtonLocRefreshClick( Sender: TObject );
    procedure ButtonDownloadClick( Sender: TObject );
    procedure ButtonUploadClick( Sender: TObject );
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBoxLocHistoryChange(Sender: TObject);
    procedure ComboBoxRemHistoryChange(Sender: TObject);
    procedure ipwFTP1PITrail(Sender: TObject; Direction: Integer;
      const Message: String);
    procedure ipwFTP1EndTransfer(Sender: TObject; Direction: Integer);
    procedure ipwFTP1StartTransfer(Sender: TObject; Direction: Integer);
    procedure ipwFTP1Transfer(Sender: TObject; Direction: Integer;
      BytesTransferred: Int64; PercentDone: Integer; Text: string);
    procedure ipwFTP1DirList(Sender: TObject; const DirEntry, FileName: string;
      IsDir: Boolean; FileSize: Int64; const FileTime: string);
    procedure ipwFTP1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
  private
    { Private declarations }
    CurrentFileSize: Integer;
    LocFileSizes, RemFileSizes: TStringList;
    State: Integer;
    procedure UpdateComboBox(ComboBox: TComboBox; ItemName: String);
    procedure UpdateButtons;
  public
    { Public declarations }
  end;

var
  FormFtp: TFormFtp;

implementation

uses
  ftploginf, ftpprogressf, ftpdialogf;

{$R *.dfm}

procedure TFormFtp.FormCreate(Sender: TObject);
begin
   State := OffLineConst;
   LocFileSizes := TStringList.Create;
   RemFileSizes := TStringList.Create;
   ComboBoxLocHistory.Items.Insert(0, GetCurrentDir);
   ComboBoxLocHistory.ItemIndex := 0;
   UpdateLocal;
   UpdateButtons;
end;

procedure TFormFtp.ButtonConnectDisconnectClick(Sender: TObject);
begin
   Screen.Cursor := crAppStart;
   if State = OffLineConst then
      with TFormFtplogin.Create(self) do
      begin
         if ShowModal = mrOK then
         begin
            ipwFTP1.RemotePath := '';
            ipwFTP1.RemoteFile := '';
            ipwFTP1.RemoteHost := EditHostName.Text;
            ipwFTP1.User := EditUser.Text;
            ipwFTP1.Password := EditPassword.Text;
            ComboBoxRemHistory.Clear;
            try
               ipwFTP1.Logon;
               State := OnLineConst;
               ComboBoxRemHistory.Items.Insert(0, ipwFTP1.RemotePath);
               ComboBoxRemHistory.ItemIndex := 0;
               UpdateRemote;
            except on E: EipwFTP do
               UpdateNotes(E.Message);
            end;
         end;
         Free;
      end
   else
   begin
      try
        ipwFTP1.Logoff
      except on E: EipwFTP do
         UpdateNotes(E.Message);
      end;
      State := OffLineConst;
   end;
   UpdateButtons;
   Screen.Cursor := crDefault;
end;

procedure TFormFtp.ButtonExitClick(Sender: TObject);
begin
   if State = OnLineConst then
   begin
      Screen.Cursor := crAppStart;
      try
         ipwFTP1.Logoff;
      except on E: EipwFTP do
         UpdateNotes(E.Message);
      end;
      Screen.Cursor := crDefault;
   end;
   Close;
end;

procedure TFormFtp.ListBoxLocFilesDblClick(Sender: TOBject);
var
   Name: String;
begin
   // Local System Box:  If double-clicked on a file, send it
   //                    If double-clicked on a directory, open it
   Name := ListBoxLocFiles.Items.Strings[ListBoxLocFiles.ItemIndex];
   if Name[1] = '<' then
   begin
      Delete(Name, 1, 7);  // Remove first seven characters, '<DIR>  '
      ChDir(Name);
      UpdateComboBox(ComboBoxLocHistory, GetCurrentDir);
      UpdateLocal;
   end else
      ButtonUploadClick(nil);
end;

procedure TFormFtp.ListBoxRemFilesDblClick( Sender: TOBject );
var
   Name: String;
begin
   // Remote Host Box:  If double-clicked on a file, get it
   //                   If double-clicked on a directory, open it
   Name := ListBoxRemFiles.Items.Strings[ListBoxRemFiles.ItemIndex];
   if Name[1] = '<' then
   begin
      Delete(Name, 1, 7); { Remove first seven characters, '<DIR>  '}
      ipwFTP1.RemotePath := ipwFTP1.RemotePath + '/' + Name;
      UpdateComboBox(ComboBoxRemHistory, ipwFTP1.RemotePath);
      UpdateRemote;
   end else
      ButtonDownloadClick(nil);
end;

procedure TFormFtp.RadioGroupTransferModeClick(Sender: TOBject);
begin
   // Set Transfer Mode
   Screen.Cursor := crAppStart;
   try
   if RadioGroupTransferMode.ItemIndex = 0 then
      ipwFTP1.TransferMode := tmASCII
   else if RadioGroupTransferMode.ItemIndex = 1 then
      ipwFTP1.TransferMode := tmBinary
   else if RadioGroupTransferMode.ItemIndex = 2 then
      ipwFTP1.TransferMode := tmDefault;
   except on E: EipwFTP do
      UpdateNotes(E.Message);
   end;
   Screen.Cursor := crDefault;
end;

procedure TFormFtp.ButtonLocChgDirClick( Sender: TObject );
begin
   // Local ChgDir Button
   with TFormFtpdialog.Create(self) do
   begin
      Caption := 'Change Directory';
      LabelWhat.Caption := 'New directory:';
      EditLine.Text := GetCurrentDir;
      if ShowModal = mrOk then
      begin
         ChDir(EditLine.Text);
         UpdateComboBox(ComboBoxLocHistory, GetCurrentDir);
         UpdateLocal;
      end;
      Free;
   end;
end;

procedure TFormFtp.ButtonLocMkDirClick(Sender: TOBject);
begin
   // Local MkDir Button
   with TFormFtpdialog.Create(self) do
   begin
      Caption := 'Make Directory';
      LabelWhat.Caption := 'New directory:';
      EditLine.Text := '';

      if ShowModal = mrOk then
      begin
         CreateDir(EditLine.Text);
         UpdateLocal;
      end;
    end;
end;

procedure TFormFtp.ButtonLocRenameClick( Sender: TObject);
var
   IsDir: Boolean;
   NameToRename: String;
begin
   // Local Rename Button
   if ListBoxLocFiles.ItemIndex = -1 then
   begin
       MessageBox(Handle, 'Please select the file or directory to rename', 'No file or directory selected', 0);
       Exit;
   end;

   NameToRename := ListBoxLocFiles.Items.Strings[ListBoxLocFiles.ItemIndex];
   IsDir := NameToRename[1] = '<';
   if IsDir then
   begin
      Delete(NameToRename, 1, 7); { // Remove first seven characters, '<DIR>  ' }
      if NameToRename[1] = '.' then Exit;
   end;

   with TFormFtpdialog.Create(self) do
   begin
      Caption := 'Rename File';
      LabelWhat.Caption := 'New filename:';
      EditLine.Text := NameToRename;

      if ShowModal = mrOk then
         if RenameFile(NameToRename, EditLine.Text) then
            UpdateLocal
         else
            UpdateNotes('Could not change the value.');
      Free;
   end;
end;

procedure TFormFtp.ButtonLocDeleteClick(Sender: TObject);
var
   IsDir: boolean;
   NameToDel: string;
begin
   // Local Delete Button
   if ListBoxLocFiles.ItemIndex = -1 then
   begin
       MessageBox(Handle, 'Please select the file or directory to delete', 'No file or directory selected', 0);
       Exit;
   end;

   NameToDel := ListBoxLocFiles.Items.Strings[ListBoxLocFiles.ItemIndex];
   IsDir := NameToDel[1] = '<';
   if IsDir then
   begin
      Delete(NameToDel, 1, 7); { // Remove first seven characters, '<DIR>  ' }
      if NameToDel[1] = '.' then Exit;
   end;

   with TFormFtpdialog.Create(self) do
   begin
      Caption := 'Remove File';
      LabelWhat.Caption := 'Remove file:';
      EditLine.Text := NameToDel;

      if ShowModal = mrOk then
         if IsDir then
            if RemoveDir(EditLine.Text) then
               UpdateLocal
            else
               UpdateNotes('Could not delete directory')
         else
            if DeleteFile(EditLine.Text) then
               UpdateLocal
            else
               UpdateNotes('Could not delete directory');
      Free;
   end;
end;

procedure TFormFtp.ButtonRemChgDirClick( Sender: TObject );
begin
   // Remote ChgDir Button
   with TFormFtpdialog.Create(self) do
   begin
      Caption := 'Change Directory';
      LabelWhat.Caption := 'New directory:';
      EditLine.Text := ipwFTP1.RemotePath;

      if ShowModal = mrOk then
      begin
         ipwFTP1.RemotePath := EditLine.Text;
         UpdateComboBox(ComboBoxRemHistory, EditLine.Text);
         UpdateRemote;
      end;
      Free;
   end;
end;

procedure TFormFtp.ButtonRemMkDirClick( Sender: TOBject );
begin
   with TFormFtpdialog.Create(self) do
   begin
      Caption := 'Make Directory';
      LabelWhat.Caption := 'New directory:';
      EditLine.Text := '';

      if ShowModal = mrOk then
      begin
         try
            ipwFTP1.MakeDirectory(EditLine.Text);
            UpdateRemote;
         except on E: EipwFTP do
            UpdateNotes(E.Message);
         end;
      end;
      Free;
   end;
end;

procedure TFormFtp.ButtonRemRenameClick( Sender: TObject );
var
   NameToRename: String;
begin
   // Remote Rename Button
   if ListBoxRemFiles.ItemIndex = -1 then
   begin
       MessageBox(Handle, 'Please select the file or directory to rename', 'No file or directory selected', 0);
       Exit;
   end;

   NameToRename := ListBoxRemFiles.Items.Strings[ListBoxRemFiles.ItemIndex];
   if NameToRename[1] = '<' then
   begin
      Delete(NameToRename, 1, 7); { // Remove first seven characters, '<DIR>  ' }
      if NameToRename[1] = '.' then Exit;
   end;

   with TFormFtpdialog.Create(self) do
   begin
      Caption := 'Rename File';
      LabelWhat.Caption := 'New filename:';
      EditLine.Text := NameToRename;

      if ShowModal = mrOk then
      begin
         if EditLine.Text = NameToRename then
            Exit;

         ipwFTP1.RemoteFile := NameToRename;
         try
            ipwFTP1.RenameFile(EditLine.Text);
         except on E: EipwFTP do
            UpdateNotes(E.Message);
         end;
         UpdateRemote;
      end;
      Free;
   end;
end;

procedure TFormFtp.ButtonRemDeleteClick( Sender: TObject );
var
   NameToDel: String;
   IsDir: Boolean;
begin
   // Remote Delete Button
   if ListBoxRemFiles.ItemIndex = -1 then
   begin
       MessageBox(Handle, 'Please select the file or directory to delete', 'No file or directory selected', 0);
       Exit;
   end;

   NameToDel := ListBoxRemFiles.Items.Strings[ListBoxRemFiles.ItemIndex];
   IsDir := NameToDel[1] = '<';
   if IsDir then
   begin
      Delete(NameToDel, 1, 7); { // Remove first seven characters, '<DIR>  ' }
      if NameToDel[1] = '.' then Exit;
   end;

   with TFormFtpdialog.Create(self) do
   begin
      Caption := 'Delete File or Directory';
      LabelWhat.Caption := 'Name:';
      EditLine.Text := NameToDel;

      if ShowModal = mrOk then
      begin
         try
            if IsDir then
               ipwFTP1.RemoveDirectory(NameToDel)
            else
               ipwFTP1.DeleteFile(NameToDel);
         except on E: EipwFTP do
            UpdateNotes(E.Message);
         end;
         UpdateRemote;
      end;
      Free;
   end;
end;

procedure TFormFtp.ButtonDownloadClick( Sender: TOBject );
var
   Name: String;
begin
   // Download currently selected file from Remote Host box
   if ListBoxRemFiles.ItemIndex = -1 then
      UpdateNotes('No file selected to download.')
   else if ListBoxRemFiles.Items.Strings[ListBoxRemFiles.ItemIndex][1] = '<' then
      UpdateNotes('Can not download directories.')
   else begin
      Name := ListBoxRemFiles.Items.Strings[ListBoxRemFiles.ItemIndex];
      ipwFTP1.RemoteFile := Name;
      ipwFTP1.LocalFile := Name;
      CurrentFileSize := StrToInt(RemFileSizes.Strings[ListBoxRemFiles.ItemIndex]);
      FormFtpprogress.Caption := 'Download Progress';
      FormFtpprogress.LabelUpDown.Caption := 'Download Progress';
      FormFtpprogress.Show;
      Screen.Cursor := crAppStart;
      try
         ipwFTP1.Download;
      except on E: EipwFTP do
      begin
         UpdateNotes(E.Message);
         UpdateRemote;  // this is to test the connection
      end;
      end;
      FormFtpprogress.Hide;
      UpdateLocal;
      Screen.Cursor := crDefault;
   end;
end;

procedure TFormFtp.ButtonUploadClick(Sender: TObject);
var
   Name: String;
begin
   // Upload currently selected file from Local System box }
   if ListBoxLocFiles.ItemIndex = -1 then
      UpdateNotes('No file selected to upload.')
   else if ListBoxLocFiles.Items.Strings[ListBoxLocFiles.ItemIndex][1] = '<' then
      UpdateNotes('Can not upload directories.')
   else begin
      Name := ListBoxLocFiles.Items.Strings[ListBoxLocFiles.ItemIndex];
      ipwFTP1.LocalFile := Name;
      ipwFTP1.RemoteFile := Name;
      CurrentFileSize := StrToInt(LocFileSizes.Strings[ListBoxLocFiles.ItemIndex]);
      FormFtpprogress.Caption := 'Upload Progress';
      FormFtpprogress.LabelUpDown.Caption := 'Upload Progress';
      FormFtpprogress.Show;
      Screen.Cursor := crAppStart;
      try
         ipwFTP1.Upload;
      except on E: EipwFTP do
         UpdateNotes(E.Message);
      end;

      FormFtpprogress.Hide;
      UpdateRemote;
      Screen.Cursor := crDefault;
   end;
end;

procedure TFormFtp.UpdateLocal;
var
   SearchRec: TSearchRec;
begin
   // Show updated file list for the local system
   // we know the value of CurrentLocDirectory  here

   Screen.Cursor := crAppStart;
   ListBoxLocFiles.Clear;
   LocFileSizes.Clear;

   if FindFirst('*', faAnyFile, SearchRec) = 0 then
   repeat
      if (SearchRec.Attr and faDirectory) <> 0 then
         ListBoxLocFiles.Items.Add('<DIR>  ' + SearchRec.Name)
      else
         ListBoxLocFiles.Items.Add(SearchRec.Name);

      LocFileSizes.Add(IntToStr(SearchRec.Size));
   until FindNext(SearchRec) <> 0;

   FindClose(SearchRec);
   Screen.Cursor := crDefault;
end;

procedure TFormFtp.UpdateButtons;
begin
   ButtonRemChgDir.Enabled := State = OnLineConst;
   ButtonRemMkDir.Enabled := State = OnLineConst;
   ButtonRemRename.Enabled := State = OnLineConst;
   ButtonRemDelete.Enabled := State = OnLineConst;
   ButtonRemRefresh.Enabled := State = OnLineConst;
   ButtonDownload.Enabled := State = OnLineConst;
   ButtonUpload.Enabled := State = OnLineConst;

   if State = OnLineConst then
      ButtonConnectDisconnect.Caption := 'Disconnect'
   else
   begin
      ComboBoxRemHistory.Clear;
      ButtonConnectDisconnect.Caption := 'Connect';
   end;

   //ListBoxRemFiles.Enabled := State = OnLineConst;
   ComboBoxRemHistory.Enabled := State = OnLineConst;
end;

procedure TFormFtp.UpdateRemote;
begin
   // Show updated file list for the remote host
   // Called only when connected

   Screen.Cursor := crAppStart;
   ListBoxRemFiles.Clear;
   RemFileSizes.Clear;
   ipwFTP1.RemoteFile := '';
   try
      ipwFTP1.ListDirectoryLong;
   except on E: EipwFTP do
   begin
      try
         ipwFTP1.Interrupt;
      except on E: EipwFTP do
      end;
      State := OffLineConst;
      UpdateButtons;
   end;
   end;
   Screen.Cursor := crDefault;
end;

procedure TFormFtp.UpdateNotes(note: string);
begin
   // Add a message to the PI Trail box and scroll down to make it visible
   ListBoxPiTrail.Items.Add(note);

   if ListBoxPiTrail.Items.Count > 255 then
      ListBoxPiTrail.Items.Delete(0);

   if ListBoxPiTrail.Items.Count > 3 then
      ListBoxPiTrail.TopIndex := ListBoxPiTrail.Items.Count - 3;
end;

procedure TFormFtp.UpdateComboBox(ComboBox: TComboBox; ItemName: String);
var
   Pos: Integer;
begin
   Pos := ComboBox.Items.IndexOf(ItemName);
   if Pos > -1 then
      ComboBox.Items.Delete(Pos);

   ComboBox.Items.Insert(0, ItemName);
   ComboBox.ItemIndex := 0;
end;

procedure TFormFtp.ButtonLocRefreshClick( Sender: TObject );
begin
   UpdateLocal;
end;

procedure TFormFtp.ButtonRemRefreshClick( Sender: TOBject );
begin
   UpdateRemote;
end;

procedure TFormFtp.ButtonCancelClick(Sender: TObject);
begin
   // Cancel Button:  Stop the current FTP process
   Screen.Cursor := crAppStart;
   try
      ipwFTP1.Interrupt;
   except on E: EipwFTP do
      UpdateNotes(E.Message);
   end;
   Screen.Cursor := crDefault;
end;

procedure TFormFtp.FormResize(Sender: TObject);
begin
    GroupBox1.Height := Height - 195 ;
    GroupBox1.Width := (Width div 2) - 27 ;
    ComboBoxLocHistory.Width := GroupBox1.Width - 16 ;
    ListBoxLocFiles.Height := GroupBox1.Height -  48;
    ListBoxLocFiles.Width := GroupBox1.Width - 65;
    ButtonLocChgDir.Left := ListBoxLocFiles.Left + ListBoxLocFiles.Width;
    ButtonLocMkDir.Left := ButtonLocChgDir.Left ;
    ButtonLocRename.Left := ButtonLocChgDir.Left ;
    ButtonLocDelete.Left := ButtonLocChgDir.Left ;
    ButtonLocRefresh.Left := ButtonLocChgDir.Left ;

    GroupBox2.Height := GroupBox1.Height ;
    GroupBox2.Width := GroupBox1.Width ;
    GroupBox2.Left := GroupBox1.Left + GroupBox1.Width + 39;
    ComboBoxRemHistory.Width := ComboBoxLocHistory.Width ;
    ListBoxRemFiles.Height := ListBoxLocFiles.Height ;
    ListBoxRemFiles.Width := ListBoxLocFiles.Width ;
    ButtonRemChgDir.Left := ListBoxRemFiles.Left + ListBoxRemFiles.Width;
    ButtonRemMkDir.Left := ButtonRemChgDir.Left ;
    ButtonRemRename.Left := ButtonRemChgDir.Left ;
    ButtonRemDelete.Left := ButtonRemChgDir.Left ;
    ButtonRemRefresh.Left := ButtonRemChgDir.Left ;

    ButtonDownload.Left := (Width div 2) - 16 ;
    ButtonDownload.Top := (Height div 2) - 124 ;
    ButtonUpload.Left := ButtonDownload.Left ;
    ButtonUpload.Top := ButtonDownload.Top + 32 ;

    RadioGroupTransferMode.Top := GroupBox1.Top + GroupBox1.Height + 3 ;
    RadioGroupTransferMode.Left := (Width div 2) - 124 ;
    ListBoxPiTrail.Top := RadioGroupTransferMode.Top + 44 ;
    ListBoxPiTrail.Width := Width - 16 ;
    ButtonConnectDisconnect.Top := RadioGroupTransferMode.Top + 128 ;
    ButtonCancel.Top := ButtonConnectDisconnect.Top ;
    ButtonExit.Top := ButtonConnectDisconnect.Top ;
end;

procedure TFormFtp.FormDestroy(Sender: TObject);
begin
   LocFileSizes.Free;
   RemFileSizes.Free;
end;

procedure TFormFtp.ComboBoxLocHistoryChange(Sender: TObject);
begin
   // Return to recent local directory
   ChDir(ComboBoxLocHistory.Text);
   UpdateComboBox(ComboBoxLocHistory, GetCurrentDir);
   UpdateLocal;
end;

procedure TFormFtp.ComboBoxRemHistoryChange(Sender: TObject);
begin
   // Return to recent remote directory
   ipwFTP1.RemotePath := ComboBoxRemHistory.Text;
   UpdateComboBox(ComboBoxRemHistory, ComboBoxRemHistory.Text);
   UpdateRemote;
end;

procedure TFormFtp.ipwFTP1PITrail(Sender: TObject; Direction: Integer;
  const Message: String);
begin
     UpdateNotes(Message);
end;

procedure TFormFtp.ipwFTP1DirList(Sender: TObject; const DirEntry,
  FileName: string; IsDir: Boolean; FileSize: Int64; const FileTime: string);
begin
   // DirList Event:  Keep directory items distinct from file items
   if IsDir then
      ListBoxRemFiles.Items.Add('<DIR>  ' + FileName)
   else
      ListBoxRemFiles.Items.Add(FileName);

   RemFileSizes.Add(IntToStr(FileSize));
end;

procedure TFormFtp.ipwFTP1EndTransfer(Sender: TObject;
  Direction: Integer);
begin
  // When finished transferring, hide the progress bar }
  FormFtpprogress.Hide;
  UpdateLocal;
end;

procedure TFormFtp.ipwFTP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormFtp.ipwFTP1StartTransfer(Sender: TObject;
  Direction: Integer);
begin
  FormFtpprogress.ProgressBar1.Position := 0;
end;

procedure TFormFtp.ipwFTP1Transfer(Sender: TObject; Direction: Integer;
  BytesTransferred: Int64; PercentDone: Integer; Text: string);
begin
  FormFtpprogress.ProgressBar1.Position := PercentDone;
end;

end.

