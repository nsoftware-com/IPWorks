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
unit netcodef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ipwcore, ipwtypes, ipwNetCode, StdCtrls, ComCtrls, ExtCtrls, converterf;

type
  TFormNetcode = class(TForm)
    ipwNetCode1: TipwNetCode;
    OpenDialog1: TOpenDialog;
    RadioGroupAction: TRadioGroup;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    MemoDecoded: TMemo;
    MemoEncoded: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    EditSource: TEdit;
    EditTarget: TEdit;
    ButtonGetSource: TButton;
    ButtonGetTarget: TButton;
    ButtonConvert: TButton;
    Label5: TLabel;
    ComboBoxEncType: TComboBox;
    ProgressBarConvert: TProgressBar;
    Label6: TLabel;
    ButtonEncode: TButton;
    Label7: TLabel;
    ButtonDecode: TButton;
    procedure ipwNetCode1Progress(Sender: TObject; PercentDone: Integer);
    procedure ButtonGetSourceClick(Sender: TObject);
    procedure ButtonGetTargetClick(Sender: TObject);
    procedure ButtonConvertClick(Sender: TObject);
    procedure ButtonEncodeClick(Sender: TObject);
    procedure ButtonDecodeClick(Sender: TObject);
    procedure ComboBoxEncTypeChange(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  constructor Create( Owner: TComponent ); override;
  end;

var
  FormNetcode: TFormNetcode;

implementation

{$R *.DFM}

constructor TFormNetcode.Create( Owner: TComponent );
begin
   inherited Create( Owner );
   ComboBoxEncType.ItemIndex := 1;
   RadioGroupAction.ItemIndex := 0;
end;


procedure TFormNetcode.ipwNetCode1Progress(Sender: TObject;
  PercentDone: Integer);
begin
   ProgressBarConvert.Position := PercentDone;
   if ProgressBarConvert.Position = 100 then
      ProgressBarConvert.Position := 0;
end;

procedure TFormNetcode.ButtonGetSourceClick(Sender: TObject);
var
   saveas : String;
begin
   OpenDialog1.Execute();
   EditSource.Text := OpenDialog1.FileName;
   EditTarget.Text := OpenDialog1.FileName;

   if RadioGroupAction.ItemIndex = 0 then
   begin
      if ComboBoxEncType.ItemIndex = 0 then
         EditTarget.Text := EditSource.Text + '.uue'

      else if ComboBoxEncType.ItemIndex = 1 then
         EditTarget.Text := EditSource.Text + '.b64'

      else if ComboBoxEncType.ItemIndex = 2 then
         EditTarget.Text := EditSource.Text + '.q_p'

      else if ComboBoxEncType.ItemIndex = 3 then
         EditTarget.Text := EditSource.Text + '.url';
   end
   else if RadioGroupAction.ItemIndex = 1 then
   begin
      saveas := EditTarget.Text;
      Delete(saveas, Length(EditSource.Text) - 4, Length(EditSource.Text) );
      EditTarget.Text := saveas;
   end;
end;

procedure TFormNetcode.ButtonGetTargetClick(Sender: TObject);
var
   saveas : String;
begin
   OpenDialog1.Execute();
   EditTarget.Text := OpenDialog1.FileName;
   saveas := EditTarget.Text;
   Delete(saveas, Length(EditSource.Text) - 4, Length(EditSource.Text) );
   EditSource.Text := saveas;
end;

procedure TFormNetcode.ButtonConvertClick(Sender: TObject);
begin
   Screen.Cursor := crHourGlass;
// Encode
   try
      if RadioGroupAction.ItemIndex = 0 then
      begin
         ipwNetCode1.Format := TipwnetcodeFormats(ComboBoxEncType.ItemIndex);
         ipwNetCode1.EncodedFile := EditTarget.Text;
         ipwNetCode1.DecodedFile := EditSource.Text;
         ipwNetCode1.Encode;
      end
// Decode
      else if RadioGroupAction.ItemIndex = 1 then
      begin
         ipwNetCode1.Format := TipwnetcodeFormats(ComboBoxEncType.ItemIndex);
         ipwNetCode1.EncodedFile := EditSource.Text;
         ipwNetCode1.DecodedFile := EditTarget.Text;
         ipwNetcode1.Decode;
      end;
   except on E: EipwNetcode do
      ShowMessage(E.Message);
   end;
   Screen.Cursor := crDefault;
end;

procedure TFormNetcode.ButtonEncodeClick(Sender: TObject);
var myfrm: TFormConverter;
begin
// Encode
   Screen.Cursor := crHourGlass;
   if ComboBoxEncType.ItemIndex = 0 then
   begin
      myfrm := FormConverter.Create(myfrm);
      if myfrm.ShowModal() = mrOk then
         ipwNetCode1.FileName := FormConverter.EditFilename.Text;
   end;
   ipwNetCode1.Format := TipwnetcodeFormats(ComboBoxEncType.ItemIndex);
   ipwNetCode1.DecodedData := MemoDecoded.Text;
   try
      ipwNetcode1.Encode;
   except on E: EipwNetcode do
      ShowMessage(E.Message);
   end;
   ipwNetCode1.FileName := '';
   MemoEncoded.Lines.Clear();
   MemoEncoded.Text := ipwNetCode1.EncodedData;
   Screen.Cursor := crDefault;
end;

procedure TFormNetcode.ButtonDecodeClick(Sender: TObject);
begin
// Decode
   Screen.Cursor := crHourGlass;
   try
      ipwNetCode1.Format := TipwnetcodeFormats(ComboBoxEncType.ItemIndex);
      ipwNetCode1.EncodedData := MemoEncoded.Text;
      ipwNetcode1.Decode;
      MemoDecoded.Lines.Clear();
      MemoDecoded.Text := ipwNetCode1.DecodedData;
   except on E: EipwNetcode do
      ShowMessage(E.Message);
   end;
   Screen.Cursor := crDefault;
end;

procedure TFormNetcode.ComboBoxEncTypeChange(Sender: TObject);
begin
   EditSource.Text := '';
   EditTarget.Text := '';
end;

end.


//---------------------------------------------------------------------------


