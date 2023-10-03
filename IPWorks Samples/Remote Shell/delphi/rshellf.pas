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
unit rshellf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ipwcore, ipwtypes, 
   ipwRshell, ExtCtrls;

type
  TFormRshell = class(TForm)
    mTrack: TMemo;
    RSHell1: TipwRSHell;
    Label5: TLabel;
    Button1: TButton;
    eCommand: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    eHost: TEdit;
    Label3: TLabel;
    Label7: TLabel;
    eRemoteUser: TEdit;
    eLocalUser: TEdit;
    procedure Rshell1Connected(Sender: TObject; StatusCode: Integer;
      const Description: String);
    procedure Rshell1Disconnected(Sender: TObject; StatusCode: Integer;
      const Description: String);
    procedure Rshell1Error(Sender: TObject; ErrorCode: Integer;
      const Description: String);
    procedure Rshell1Stderr(Sender: TObject; Text: string; TextB: TArray<System.Byte>;
      EOL: Boolean);
    procedure Rshell1StdOut(Sender: TObject; Text: string; TextB: TArray<System.Byte>;
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
  FormRshell: TFormRshell;

implementation

{$R *.DFM}

procedure TFormRshell.Rshell1Connected(Sender: TObject; StatusCode: Integer;
  const Description: String);
begin
    mTrack.Lines.Add('CONNECTED: ' + Description);
end;

procedure TFormRshell.Rshell1Disconnected(Sender: TObject; StatusCode: Integer;
  const Description: String);
begin
    mTrack.Lines.Add('DISCONNECTED: ' + Description);
    if (StatusCode <> 0) or (Rshell1.ErrorMessage <> '')
    then mTrack.Lines.Add('LAST ERROR: ' + Rshell1.ErrorMessage);
end;

procedure TFormRshell.Rshell1Error(Sender: TObject; ErrorCode: Integer;
  const Description: String);
begin
    mTrack.Lines.Add('ERROR: ' + Description);
end;

procedure TFormRshell.Rshell1Stderr(Sender: TObject; Text: string; TextB: TArray<System.Byte>;
  EOL: Boolean);
begin
    mTrack.Lines.Add('STDERR: ' + Text);
end;

procedure TFormRshell.Rshell1StdOut(Sender: TObject; Text: string; TextB: TArray<System.Byte>;
  EOL: Boolean);
begin
    mTrack.Lines.Add({'STDOUT:  + '}Text);
end;

procedure TFormRshell.eCommandKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #13 then
    begin
        Key := #0;
		Button1Click(Sender);
    end;
end;

procedure TFormRshell.mTrackKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #13 then
    begin
        Rshell1.Stdin := mTrack.Lines[mTrack.Lines.Count-1]+Rshell1.EOL;
    end;
end;

procedure TFormRshell.mTrackMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    if Button = mbRight
    then (Sender as TMemo).Clear;
end;

procedure TFormRshell.Button1Click(Sender: TObject);
begin
    if eCommand.Text <> '' then
    begin{new connection, assign new host etc...}
        Rshell1.LocalUser := eLocalUser.Text;
        Rshell1.RemoteUser := eRemoteUser.Text;
        Rshell1.RemoteHost := eHost.Text;
    end;
    {new command or Break if empty}
    Rshell1.Command := eCommand.Text;
end;



end.

