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

program certmgr;

uses
  Forms,
  createcertf in 'createcertf.pas'   {FormCreatecertf},
  generatecsrf in 'generatecsrf.pas'   {FormGeneratecsrf},
  importcsrf in 'importcsrf.pas'   {FormImportcsrf},
  signcsrf in 'signcsrf.pas'   {FormSigncsrf},
  certmgrf in 'certmgrf.pas' {FormCertmgr};

begin
  Application.Initialize;

  Application.CreateForm(TFormCertmgr, FormCertmgr);
  Application.CreateForm(TFormCreatecert, FormCreatecert);

  Application.CreateForm(TFormGeneratecsr, FormGeneratecsr);

  Application.CreateForm(TFormImportcsr, FormImportcsr);

  Application.CreateForm(TFormSigncsr, FormSigncsr);

  Application.Run;
end.


         
