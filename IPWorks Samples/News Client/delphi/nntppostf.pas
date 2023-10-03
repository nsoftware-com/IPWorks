unit nntppostf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormNntppost = class(TForm)
    MemoMessage: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    EditFrom: TEdit;
    EditSubject: TEdit;
    ButtonPost: TButton;

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNntppost: TFormNntppost;

implementation

{$R *.DFM}



end.


