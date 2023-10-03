unit ftploginf;

interface

uses
{$IFDEF LINUX}
  QForms, QExtCtrls, QStdCtrls, QControls, Classes, QComCtrls, ipwcore, ipwtypes, ipwftp,
  StdCtrls, Controls;
{$ELSE}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ipwcore, ipwtypes, ipwftp;
{$ENDIF}

type
  TFormFtplogin = class(TForm)
    Label1: TLabel;
    EditHostName: TEdit;
    EditUser: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    EditPassword: TEdit;
    ButtonOk: TButton;
    ButtonCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormFtplogin: TFormFtplogin;

implementation

{$R *.dfm}

end.
