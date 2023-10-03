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

program snpp;

uses
  Forms,
  snppf in 'snppf.pas' {FormSnpp};

begin
  Application.Initialize;

  Application.CreateForm(TFormSnpp, FormSnpp);
  Application.Run;
end.


         
