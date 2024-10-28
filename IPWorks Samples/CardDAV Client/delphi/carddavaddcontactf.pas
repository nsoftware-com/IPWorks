unit carddavaddcontactf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DateUtils;

type
  TFormCarddavAddContact = class(TForm)
    Label1: TLabel;
    fnLabel: TLabel;
    phoneNumberLabel: TLabel;
    emailLabel: TLabel;
    addressLabel: TLabel;
    txtFormattedName: TEdit;
    txtPhoneNumber: TEdit;
    txtEmail: TEdit;
    txtAddress: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCarddavAddContact: TFormCarddavAddContact;

implementation

{$R *.dfm}

procedure TFormCarddavAddContact.FormCreate(Sender: TObject);
var
  today: TDateTime;
  formattedDate: string;
  formattedTime: string;
begin
  today := Now;
  today := IncDay(today, 1);
  DateTimeToString(formattedDate, 'YYYYMMDD', today);
  DateTimeToString(formattedTime, 'hhmmss', today);
//  txtStartDate.Text := formattedDate + 'T' + formattedTime;
  today := IncHour(today, 1);
  DateTimeToString(formattedDate, 'YYYYMMDD', today);
  DateTimeToString(formattedTime, 'hhmmss', today);
//  txtEndDate.Text := formattedDate + 'T' + formattedTime;
end;

end.
