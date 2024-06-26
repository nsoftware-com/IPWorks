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

program netcode;

uses
  Forms,
  converterf in 'converterf.pas'   {FormConverterf},
  netcodef in 'netcodef.pas' {FormNetcode};

begin
  Application.Initialize;

  Application.CreateForm(TFormNetcode, FormNetcode);
  Application.CreateForm(TFormConverter, FormConverter);

  Application.Run;
end.


         
