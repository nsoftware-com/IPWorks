(*
 * IPWorks 2024 Delphi Edition - Sample Project
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
unit pingf;

// A simple demonstration of the Ping component.

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, ipwcore, ipwtypes, ipwping;

type
  TFormPing = class(TForm)
    lTrack: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PingButton: TButton;
    ClearButton: TButton;
    tHostName: TEdit;

    ipwPing1: TipwPing;

    procedure DisplayError(ErrorMessage: string);
    procedure DisplayInfo;
    procedure PingButtonClick(Sender: TObject);

    procedure tHostNameKeyPress(Sender: TObject; var Key: Char);
    procedure lTrackMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ClearButtonClick(Sender: TObject);
  end;

var
  FormPing: TFormPing;

implementation

{$R *.DFM}

procedure TFormPing.DisplayError(ErrorMessage: string);

begin
   if ipwPing1.RemoteHost <> '' then
      lTrack.Items.Add('Error accessing ' + ipwPing1.RemoteHost +
                       ': ' + ErrorMessage)
   else
      lTrack.Items.Add('Error accessing ' + tHostName.Text +
                       ': ' + ErrorMessage);
end;

procedure TFormPing.DisplayInfo;
begin
   lTrack.Items.Add('Remote Host: ' + ipwPing1.RemoteHost);
   lTrack.Items.Add('Source: ' + ipwPing1.ResponseSource);
   lTrack.Items.Add('Response Time: ' + IntToStr(ipwPing1.ResponseTime));
end;


procedure TFormPing.PingButtonClick(Sender: TObject);
begin
   try
      ipwPing1.PacketSize := 32;
      ipwPing1.Timeout := 1; // Abandon ping attempt after one second
      lTrack.Items.Add ('sending 32 bytes to ' +
            tHostName.text + '...');
      ipwPing1.PingHost(tHostName.Text);
      DisplayInfo;
   except on E: EIPWorks do
      DisplayError(e.Message);
   end;
end;


// GUI Procedures

procedure TFormPing.tHostNameKeyPress(Sender: TObject; var Key: Char);
begin
   if Key = #13 then
     begin
    	Key := #0;
    	PingButton.OnClick(Sender);
     end;
end;

procedure TFormPing.lTrackMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   if Button = mbRight then
      lTrack.Clear;
end;

procedure TFormPing.ClearButtonClick(Sender: TObject);
begin
   lTrack.Clear;
end;

end.

