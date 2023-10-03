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

program popclient;

uses
  Forms,
  composef in 'composef.pas'   {FormComposef},
  loginf in 'loginf.pas'   {FormLoginf},
  progressf in 'progressf.pas'   {FormProgressf},
  popclientf in 'popclientf.pas' {FormPopclient};

begin
  Application.Initialize;

  Application.CreateForm(TFormPopclient, FormPopclient);
  Application.CreateForm(TFormCompose, FormCompose);

  Application.CreateForm(TFormLogin, FormLogin);

  Application.CreateForm(TFormProgress, FormProgress);

  Application.Run;
end.


         
