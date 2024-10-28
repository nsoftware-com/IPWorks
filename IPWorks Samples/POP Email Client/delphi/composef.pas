unit composef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type
  TFormCompose = class(TForm)
    OpenDialog1: TOpenDialog;
    MemoMessage: TMemo;
    Label2: TLabel;
    EditSubject: TEdit;
    Label3: TLabel;
    EditCc: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    EditTo: TEdit;
    EditFrom: TEdit;
    Label6: TLabel;
    Label5: TLabel;
    ComboBoxAttachments: TComboBox;
    ButtonRemove: TButton;
    ButtonAdd: TButton;
    ButtonSend: TButton;
    ButtonCancel: TButton;
    procedure ButtonRemoveClick( Sender: TObject );
    procedure ButtonAddClick( Sender: TObject );

  private
    { Private declarations }

  public
    { Public declarations }
    constructor Create( Owner: TComponent ); override;

  end;

var
  FormCompose: TFormCompose;

implementation

{$R *.DFM}

//---------------------------------------------------------------------------
constructor TFormCompose.Create( Owner: TComponent );
begin
   inherited Create(Owner);
end;


//---------------------------------------------------------------------------
procedure TFormCompose.ButtonRemoveClick( Sender: TObject );
begin
// Remove Attachment
   ComboBoxAttachments.Items.Delete(ComboBoxAttachments.ItemIndex);
   ComboBoxAttachments.ItemIndex := ComboBoxAttachments.Items.Count - 1;
   if ComboBoxAttachments.Items.Count > 0 then
      ComboBoxAttachments.ItemIndex := 0
   else
      ComboBoxAttachments.Text := '';
end;
//---------------------------------------------------------------------------

procedure TFormCompose.ButtonAddClick( Sender: TObject );
begin
// Add Attachment
   if OpenDialog1.Execute() then
   begin
      ComboBoxAttachments.Items.Add(OpenDialog1.FileName);
      ComboBoxAttachments.ItemIndex := ComboBoxAttachments.Items.Count - 1;
   end;
end;

end.

