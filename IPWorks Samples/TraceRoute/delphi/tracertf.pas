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
unit tracertf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ipwcore, ipwtypes, ipwtraceroute;

type
  TFormTracert = class(TForm)
    Label1: TLabel;
    txtHost: TEdit;
    btnTrace: TButton;
    Label2: TLabel;
    memoResults: TMemo;
    TraceRoute1: TipwTraceRoute;
    procedure btnTraceClick(Sender: TObject);
    procedure TraceRoute1Hop(Sender: TObject; HopNumber: Integer;
      const HostAddress: String; Duration: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTracert: TFormTracert;

implementation

{$R *.dfm}

procedure TFormTracert.btnTraceClick(Sender: TObject);
begin
memoResults.Lines.Clear;
TraceRoute1.TraceTo(txtHost.Text);
memoResults.Lines.Add('Trace Complete.');
end;

procedure TFormTracert.TraceRoute1Hop(Sender: TObject; HopNumber: Integer;
  const HostAddress: String; Duration: Integer);
begin
memoResults.Lines.Add('Hop '+ IntToStr(HopNumber) + ':' + #9 + HostAddress + ' in ' +
        IntToStr(Duration) + 'ms');
end;

end.

