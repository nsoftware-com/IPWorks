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

program s3;

uses
  Forms,
  s3f in 's3f.pas' {FormS3};

begin
  Application.Initialize;

  Application.CreateForm(TFormS3, FormS3);
  Application.Run;
end.


         
