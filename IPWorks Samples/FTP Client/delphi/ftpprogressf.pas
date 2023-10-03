unit ftpprogressf;

interface

uses
{$IFDEF LINUX}
  QForms, QStdCtrls, Classes, QControls, ipwcore, ipwtypes, ipwftp, Controls,
  StdCtrls, ComCtrls;
{$ELSE}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ipwcore, ipwtypes, ipwftp;
{$ENDIF}

type
  TFormFtpprogress = class(TForm)
    ButtonCancel: TButton;
    LabelUpDown: TLabel;
    ProgressBar1: TProgressBar;
    procedure ButtonCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
   FormFtpprogress: TFormFtpprogress;

implementation

uses ftpf;

{$R *.dfm}

procedure TFormFtpprogress.ButtonCancelClick(Sender: TObject);
begin
   try
      FormFtp.ipwFTP1.Abort();
   except on E: EipwFTP do
      FormFtp.UpdateNotes(E.Message);
   end;
end;

end.
