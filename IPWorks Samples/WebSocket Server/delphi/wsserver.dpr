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

program wsserver;

uses
  Forms,
  wscertf in 'wscertf.pas'   {FormWscertf},
  wsserverf in 'wsserverf.pas' {FormWsserver};

begin
  Application.Initialize;

  Application.CreateForm(TFormWsserver, FormWsserver);
  Application.CreateForm(TFormWscert, FormWscert);

  Application.Run;
end.


         
