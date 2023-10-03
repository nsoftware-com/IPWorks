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
unit caldavf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, StrUtils, DateUtils, 
  caldavaddeventf, ipwcore, ipwtypes, ipwcaldav, ipwoauth;

type
  TFormCaldav = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label4: TLabel;
    txtUsername: TEdit;
    txtURL: TEdit;
    btnReport: TButton;
    btnAddEvent: TButton;
    btnDeleteEvent: TButton;
    btnExportICS: TButton;
    GroupBox2: TGroupBox;
    lvwEventDetails: TListView;
    SaveDialog1: TSaveDialog;
    ipwCalDAV1: TipwCalDAV;
    txtSecret: TEdit;
    txtID: TEdit;
    Label6: TLabel;
    Label5: TLabel;
    ipwOAuth1: TipwOAuth;
    procedure FormCreate(Sender: TObject);
    procedure txtUsernameChange(Sender: TObject);
    procedure btnReportClick(Sender: TObject);
    procedure btnAddEventClick(Sender: TObject);
    procedure btnDeleteEventClick(Sender: TObject);
    procedure btnExportICSClick(Sender: TObject);
    procedure ipwCalDAV1EventDetails(Sender: TObject; const ResourceURI,
      ResponseStatus, ETag, CalendarData: string);
    procedure ipwCalDAV1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
    procedure txtIDChange(Sender: TObject);
    procedure txtSecretChange(Sender: TObject);
  private
    procedure ListEvents();
  public
  end;

var
  FormCaldav: TFormCaldav;

implementation

{$R *.dfm}

procedure TFormCaldav.btnAddEventClick(Sender: TObject);
var
  addEventForm: TFormCaldavAddEvent;
  today: TDateTime;
  formattedDate: string;
  formattedTime: string;
begin
  addEventForm := TFormCaldavAddEvent.Create(self);
  try
    if addEventForm.ShowModal() = mrOk then
      begin
        Screen.Cursor := crHourGlass;
        lvwEventDetails.Items.Clear();
        ipwCalDAV1.User := txtUserName.Text;
        ipwCalDAV1.StartDate := addEventForm.txtStartDate.Text;
        ipwCalDAV1.EndDate := addEventForm.txtEndDate.Text;
        ipwCalDAV1.Summary := addEventForm.txtSummary.Text;
        ipwCalDAV1.Description := addEventForm.txtDescription.Text;
        ipwCalDAV1.Location := addEventForm.txtLocation.Text;
        today := Now;
        DateTimeToString(formattedDate, 'YYYYMMDD', today);
        DateTimeToString(formattedTime, 'hhmmss', today);
        ipwCalDAV1.UID := formattedDate + 'T' + formattedTime + 'Z';
        ipwCalDAV1.EventType := TipwcaldavEventTypes(vEvent);
        ipwCalDAV1.Authorization := ipwOAuth1.GetAuthorization();
        ipwCalDAV1.PutCalendarEvent(txtURL.Text +'/' + ipwCalDAV1.UID + '.ics');

        //List the events.
        ListEvents();
      end;
  except
    on E : Exception do
    begin
      ShowMessage('Exception: '+E.Message);
    end;
  end;
  Screen.Cursor := crDefault;
  addEventForm.Release();
end;

procedure TFormCaldav.FormCreate(Sender: TObject);
begin
  txtURL.Text := 'https://apidata.googleusercontent.com/caldav/v2/' +txtUsername.Text + '/events/';
  ipwOAuth1.ServerAuthURL := 'https://accounts.google.com/o/oauth2/auth';
	ipwOAuth1.ServerTokenURL := 'https://oauth2.googleapis.com/token';
	ipwCalDAV1.AuthScheme := TipwCalDAVAuthSchemes.authOAuth;
	ipwOAuth1.AuthorizationScope := 'https://www.googleapis.com/auth/calendar';
end;

procedure TFormCaldav.btnDeleteEventClick(Sender: TObject);
begin
  if lvwEventDetails.SelCount <= 0 then
    begin
      ShowMessage('Please select an Event.');
      Exit;
    end;

  Screen.Cursor := crHourGlass;
  try
    ipwCalDAV1.Authorization := ipwOAuth1.GetAuthorization();
    ipwCalDAV1.DeleteCalendarEvent('https://apidata.googleusercontent.com' + lvwEventDetails.Selected.SubItems[2]);
    ListEvents();
  except
    on E : Exception do
    begin
      ShowMessage('Exception: '+E.Message);
    end;
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormCaldav.btnExportICSClick(Sender: TObject);
var
  icsfile: TextFile;
begin
  if lvwEventDetails.SelCount <= 0 then
    begin
      ShowMessage('Please select an Event.');
      Exit;
    end;

  SaveDialog1.Title := 'Save ICS File.';
  SaveDialog1.Filter := '(*.ics)|*.ics';
  SaveDialog1.DefaultExt := 'ics';
  if SaveDialog1.Execute then
    begin
      Screen.Cursor := crHourGlass;
      try
        ipwCalDAV1.Authorization := ipwOAuth1.GetAuthorization();
        ipwCalDAV1.GetCalendarEvent('https://apidata.googleusercontent.com' + lvwEventDetails.Selected.SubItems[2])
      except
        on E : Exception do
        ShowMessage('Exception: '+E.Message);
      end;

      try
        AssignFile(icsfile, SaveDialog1.FileName);
        ReWrite(icsFile);
        Write(icsFile, ipwCalDAV1.ExportICS());
      finally
        CloseFile(icsfile);
        Screen.Cursor := crDefault;
      end;

      ListEvents();
    end;
end;

procedure TFormCaldav.btnReportClick(Sender: TObject);
begin

  ListEvents();
end;

procedure TFormCaldav.txtIDChange(Sender: TObject);
begin
  ipwOAuth1.ClientID := txtID.Text;
end;

procedure TFormCaldav.txtSecretChange(Sender: TObject);
begin
  ipwOAuth1.ClientSecret := txtSecret.Text;
end;

procedure TFormCaldav.ipwCalDAV1EventDetails(Sender: TObject; const ResourceURI,
      ResponseStatus, ETag, CalendarData: string);
var
  startDateTimeStr: string;
  year: Integer;
  month: Integer;
  day: Integer;
  hour: Integer;
  minute: Integer;
  second: Integer;
  formattedDateTime: string;
  startDateTime: TDateTime;
  listItem: TListItem;
begin
  listItem := lvwEventDetails.Items.Add();
  listItem.Caption := ipwCalDAV1.Summary;
  startDateTimeStr := ipwCalDAV1.StartDate;
  startDateTimeStr := StringReplace(startDateTimeStr, 'T', '', [rfReplaceAll]);
  startDateTimeStr := StringReplace(startDateTimeStr, 'Z', '', [rfReplaceAll]);
  if Length(startDateTimeStr) = 14 then
    begin
      year := StrToInt(AnsiMidStr(startDateTimeStr, 1, 4));
      month := StrToInt(AnsiMidStr(startDateTimeStr, 5, 2));
      day := StrToInt(AnsiMidStr(startDateTimeStr, 7, 2));
      hour := StrToInt(AnsiMidStr(startDateTimeStr, 9, 2));
      minute := StrToInt(AnsiMidStr(startDateTimeStr, 11, 2));
      second := StrToInt(AnsiMidStr(startDateTimeStr, 13, 2));
      startDateTime := EncodeDateTime(year, month, day, hour, minute, second, 0);
      DateTimeToString(formattedDateTime, 'MM/dd HH:mm:ss', startDateTime);
      listItem.SubItems.Add(formattedDateTime);
    end
  else
    begin
      listItem.SubItems.Add(ipwCalDAV1.StartDate);
    end;
  listItem.SubItems.Add(ipwCalDAV1.Location);
  listItem.SubItems.Add(ResourceURI);
end;


procedure TFormCaldav.ipwCalDAV1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;


procedure TFormCaldav.txtUsernameChange(Sender: TObject);
begin
  txtURL.Text := 'https://apidata.googleusercontent.com/caldav/v2/' + txtUsername.Text + '/events';


end;

procedure TFormCaldav.ListEvents();
var
  today : TDateTime;
  formattedDate : string;
  formattedTime : string;
begin
  Screen.Cursor := crHourGlass;
  try
    lvwEventDetails.Items.Clear();
    ipwCalDAV1.User := txtUsername.Text;
//    ipwOAuth1.ClientId := txtID.Text;
//    ipwOAuth1.ClientSecret := txtSecret.Text;
//    txtURL.Text := txtURL.Text;
//    ipwOAuth1.ServerAuthURL := 'https://accounts.google.com/o/oauth2/auth';
//    ipwOAuth1.ServerTokenURL := 'https://oauth2.googleapis.com/token';
//    ipwCalDAV1.AuthScheme := TipwCalDAVAuthSchemes.authOAuth;
//    ipwOAuth1.AuthorizationScope := 'https://www.googleapis.com/auth/calendar';
//    ipwCalDAV1.ReportFilterEventType := TipwcaldavReportFilterEventTypes.vtEvent;

    //Lists events for the next month
//    today := Now;
//    DateTimeToString(formattedDate, 'YYYYMMDD', today);
//    DateTimeToString(formattedTime, 'hhmmss', today);
//    ipwCalDAV1.ReportFilterStartDate := formattedDate + 'T' + formattedTime + 'Z';
//    today := IncMonth(today, 1);
//    DateTimeToString(formattedDate, 'YYYYMMDD', today);
//    DateTimeToString(formattedTime, 'hhmmss', today);
//    ipwCalDAV1.ReportFilterEndDate := formattedDate + 'T' + formattedTime + 'Z';

    ipwCalDAV1.Authorization := ipwOAuth1.GetAuthorization();
    ipwCalDAV1.GetCalendarReport(txtURL.Text);
  except
    on E : Exception do
    begin
      ShowMessage('Exception: '+E.Message + E.StackTrace);
    end;
  end;
  Screen.Cursor := crDefault;
end;

end.



