unit caldavaddeventf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DateUtils;

type
  TFormCaldavAddEvent = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    txtStartDate: TEdit;
    txtEndDate: TEdit;
    txtSummary: TEdit;
    txtDescription: TEdit;
    txtLocation: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCaldavAddEvent: TFormCaldavAddEvent;

implementation

{$R *.dfm}

procedure TFormCaldavAddEvent.FormCreate(Sender: TObject);
var
  today: TDateTime;
  formattedDate: string;
  formattedTime: string;
begin
  today := Now;
  today := IncDay(today, 1);
  DateTimeToString(formattedDate, 'YYYYMMDD', today);
  DateTimeToString(formattedTime, 'hhmmss', today);
  txtStartDate.Text := formattedDate + 'T' + formattedTime;
  today := IncHour(today, 1);
  DateTimeToString(formattedDate, 'YYYYMMDD', today);
  DateTimeToString(formattedTime, 'hhmmss', today);
  txtEndDate.Text := formattedDate + 'T' + formattedTime;
end;

end.
