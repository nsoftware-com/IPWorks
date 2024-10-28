unit ftpdialogf;

interface

uses
{$IFDEF LINUX}
  QForms, QStdCtrls, Classes, QControls, StdCtrls, Controls;
{$ELSE}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ipwcore, ipwtypes, ipwftp;
{$ENDIF}

type
  TFormFtpdialog = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    LabelWhat: TLabel;
    EditLine: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormFtpdialog: TFormFtpdialog;

implementation

{$R *.dfm}

end.
