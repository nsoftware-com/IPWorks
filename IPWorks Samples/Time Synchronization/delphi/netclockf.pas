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
unit netclockf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ipwcore, ipwtypes, ipwnetclock, StdCtrls, ExtCtrls;

type
  TFormNetclock = class(TForm)
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    txtServerTime: TEdit;
    txtSystemTime: TEdit;
    bSynchronize: TButton;
    ipwNetClock1: TipwNetClock;
    Timer1: TTimer;
    txtTimeServer: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure cboServersClick(Sender: TObject);
    procedure bSynchronizeClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNetclock: TFormNetclock;

implementation

{$R *.DFM}

procedure TFormNetclock.FormCreate(Sender: TObject);
begin

    ipwNetClock1.TimeServer := txtTimeServer.Text; //links to additional servers can be found here http://support.microsoft.com/kb/262680

    Timer1.Interval := 5000; //This server will reject rapid sequential requests
    Timer1.Enabled := true;

    Timer1Timer(Sender);
end;

procedure TFormNetclock.cboServersClick(Sender: TObject);
begin
    while (ipwNetClock1.Idle = false) do begin
        ipwNetClock1.DoEvents();
        end;
    ipwNetClock1.TimeServer := txtTimeServer.Text;
end;

procedure TFormNetclock.bSynchronizeClick(Sender: TObject);
begin
        ipwNetClock1.GetAndSetTime();
        txtSystemTime.Text := TimeToStr(Time());
        txtServerTime.Text := ipwNetClock1.LocalTime;
end;

procedure TFormNetclock.Timer1Timer(Sender: TObject);
begin
        if(ipwNetClock1.Idle) then begin
                ipwNetClock1.GetTime();
                txtSystemTime.Text := TimeToStr(Time());
                txtServerTime.Text := ipwNetClock1.LocalTime;
        end;
end;

end.

