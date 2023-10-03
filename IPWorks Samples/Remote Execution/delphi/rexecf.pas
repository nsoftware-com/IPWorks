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
unit rexecf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ipwcore, ipwtypes,  ipwRexec, ExtCtrls;

type
  TFormRexec = class(TForm)
    mTrack: TMemo;
    REXec1: TipwRexec;
    Label5: TLabel;
    Button1: TButton;
    eCommand: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    eUser: TEdit;
    Label3: TLabel;
    ePasswd: TEdit;
    Label2: TLabel;
    eHost: TEdit;
    Label6: TLabel;
    procedure Rexec1Connected(Sender: TObject; StatusCode: Integer;
      const Description: String);
    procedure Rexec1Disconnected(Sender: TObject; StatusCode: Integer;
      const Description: String);
    procedure Rexec1Error(Sender: TObject; ErrorCode: Integer;
      const Description: String);
    procedure Rexec1Stderr(Sender: TObject; Text: string; TextB: TArray<System.Byte>;
      EOL: Boolean);
    procedure Rexec1StdOut(Sender: TObject; Text: string; TextB: TArray<System.Byte>;
      EOL: Boolean);
    procedure eCommandKeyPress(Sender: TObject; var Key: Char);
    procedure mTrackKeyPress(Sender: TObject; var Key: Char);
    procedure mTrackMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRexec: TFormRexec;

implementation

{$R *.DFM}

procedure TFormRexec.Rexec1Connected(Sender: TObject; StatusCode: Integer;
  const Description: String);
begin
    if StatusCode <> 0
    then mTrack.Lines.Add('CONNECTION FAILED: ' + Description);
end;

procedure TFormRexec.Rexec1Disconnected(Sender: TObject; StatusCode: Integer;
  const Description: String);
begin
    if StatusCode <> 0 then
    begin
    	mTrack.Lines.Add('DISCONNECTED: ' + Description);
    	mTrack.Lines.Add('LAST ERROR: ' + Rexec1.ErrorMessage);
    end;
end;

procedure TFormRexec.Rexec1Error(Sender: TObject; ErrorCode: Integer;
  const Description: String);
begin
    mTrack.Lines.Add('ERROR: ' + Description);
end;

procedure TFormRexec.Rexec1Stderr(Sender: TObject; Text: string; TextB: TArray<System.Byte>;
  EOL: Boolean);
begin
    mTrack.Lines.Add('STDERR: ' + Text);
end;

procedure TFormRexec.Rexec1StdOut(Sender: TObject; Text: string; TextB: TArray<System.Byte>;
  EOL: Boolean);
begin
    mTrack.Lines.Add(Text);
end;

procedure TFormRexec.eCommandKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #13 then
    begin
        Key := #0;
        Button1Click(Sender);
    end;
end;

procedure TFormRexec.mTrackKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #13 then
    begin
        Rexec1.Stdin := mTrack.Lines[mTrack.Lines.Count-1] + Rexec1.EOL;
    end;
end;

procedure TFormRexec.mTrackMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    if mbRight = Button
    then (Sender as TMemo).Clear;
end;

procedure TFormRexec.Button1Click(Sender: TObject);
begin
    if eCommand.Text = ''
    then Rexec1.Command := ''
    else begin
        Rexec1.RemoteHost := eHost.Text;
        Rexec1.RemoteUser := eUser.Text;
        Rexec1.RemotePassword := ePasswd.Text;
        Rexec1.Command := eCommand.Text; {don't append an EOL }
    end;
end;



end.

