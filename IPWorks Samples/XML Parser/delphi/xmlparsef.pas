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
unit xmlparsef;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ipwcore, ipwtypes, ipwhttp, ipwxml;

type
  TFormXMLParse = class(TForm)
    Label1: TLabel;
    rbHttp: TRadioButton;
    rbFile: TRadioButton;
    rbString: TRadioButton;
    txtURL: TEdit;
    txtFile: TEdit;
    Browse: TButton;
    memString: TMemo;
    chkValidate: TCheckBox;
    btnParse: TButton;
    Label2: TLabel;
    Label3: TLabel;
    memEvents: TMemo;
    treeXPath: TTreeView;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    txtXPath: TEdit;
    txtElement: TEdit;
    txtValue: TEdit;
    OpenDialog1: TOpenDialog;
    ipwHTTP1: TipwHTTP;
    ipwXML1: TipwXML;
    procedure BrowseClick(Sender: TObject);
    procedure btnParseClick(Sender: TObject);
    procedure treeXPathClick(Sender: TObject);
    procedure ipwXML1StartElement(Sender: TObject; const Namespace, Element,
      QName: string; IsEmpty: Boolean);
    procedure ipwXML1EndElement(Sender: TObject; const Namespace, Element,
      QName: string; IsEmpty: Boolean);
    procedure ipwHTTP1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
  private
    { Private declarations }
    function GetNodePath(node: TTreenode): String;
    function ChildExists(node: TTreenode; childNodeText: String): Boolean;
  public
    { Public declarations }
  end;

var
  FormXMLParse: TFormXMLParse;

implementation

{$R *.dfm}

procedure TFormXMLParse.btnParseClick(Sender: TObject);
var
  numchildren, I, sameChildTextIndex: Integer;
  root: String;
  parentNode: TTreeNode;
begin
  ipwXML1.Reset();
  memEvents.Clear();
  treeXPath.Items.Clear();

  // first, run through events
  ipwXML1.Validate := chkValidate.Checked;
  if (rbFile.Checked) then begin
    ipwXML1.InputFile := txtFile.Text;
  end
  else if (rbString.Checked) then begin
    ipwXML1.InputData := memString.Text;
  end
  else if (rbHttp.Checked) then begin
    ipwHTTP1.FollowRedirects := TipwHttpFollowRedirects.frAlways;
    ipwHTTP1.TransferredDataLimit := 0;
    ipwHTTP1.Get(txtURL.Text);
    ipwXML1.InputData := ipwHTTP1.TransferredData;
  end;

  ipwXML1.Parse();

  //if validate is true, traverse through the XPath
  if (chkValidate.Checked) then begin
    ipwXML1.BuildDOM := true;
    ipwXML1.XPath := '/';
    root := ipwXML1.XElement;
    ipwXML1.XPath := '/' + root;
    parentNode := treeXPath.Items.AddChild(treeXPath.Items.GetFirstNode, root);
    treeXPath.Select(parentNode);

    sameChildTextIndex := 2;  // Start at index 2 since the first item will be added without an index
    numchildren := ipwXML1.XChildCount;
    for I := 1 to numchildren do begin
      ipwXML1.XPath := '/' + root + '/[' + inttostr(I) + ']';

      if (ChildExists(parentNode, ipwXML1.XElement)) then begin
        treeXPath.Items.AddChild(parentNode, ipwXML1.XElement + '[' + inttostr(sameChildTextIndex) + ']');
        sameChildTextIndex := sameChildTextIndex + 1;
      end
      else begin
        treeXPath.Items.AddChild(parentNode, ipwXML1.XElement);
        sameChildTextIndex := 2; // Reset the index, since the child does not already exist
      end;
    end;

    parentNode.Expand(true);
  end;
end;

procedure TFormXMLParse.treeXPathClick(Sender: TObject);
var
  parent: String;
  numchildren, I, J, sameChildTextIndex: Integer;
  parentNode, childNode: TTreeNode;
begin
  if (treeXPath.Items.Count = 0) then exit;

  parentNode := treeXPath.Selected;
  ipwXML1.XPath := GetNodePath(parentNode);
  txtXPath.Text := ipwXML1.XPath;
  txtElement.Text := ipwXML1.XElement;
  txtValue.Text := ipwXML1.XText;

  parent := ipwXML1.XPath;

  // If child nodes were not already added
  if (parentNode.Count = 0) then begin
    sameChildTextIndex := 2;  // Start at index 2 since the first item will be added without an index
    numchildren := ipwXML1.XChildCount;
    for I := 1 to numchildren do begin
      ipwXML1.XPath := parent + '/[' + inttostr(I) + ']';

      if (ChildExists(parentNode, ipwXML1.XElement)) then begin
          treeXPath.Items.AddChild(parentNode, ipwXML1.XElement + '[' + inttostr(sameChildTextIndex) + ']');
          sameChildTextIndex := sameChildTextIndex + 1;
      end
      else begin
        treeXPath.Items.AddChild(parentNode, ipwXML1.XElement);
        sameChildTextIndex := 2; // Reset the index, since the child does not already exist
      end;
    end;
  end;
end;

procedure TFormXMLParse.BrowseClick(Sender: TObject);
begin
  OpenDialog1.Execute();
  txtFile.Text := OpenDialog1.FileName;
end;

function TFormXMLParse.GetNodePath(node: TTreenode): String;
begin
  Result := '';
  while Assigned(node) do
  begin
  Result := '/' + node.Text + Result;
  node := node.Parent;
  end;
end;

procedure TFormXMLParse.ipwHTTP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormXMLParse.ipwXML1EndElement(Sender: TObject; const Namespace, Element,
  QName: string; IsEmpty: Boolean);
begin
  memEvents.Lines.Add('END ELEMENT: ' + Element);
end;

procedure TFormXMLParse.ipwXML1StartElement(Sender: TObject; const Namespace, Element,
  QName: string; IsEmpty: Boolean);
begin
  memEvents.Lines.Add('START ELEMENT: ' + Element);
end;

function TFormXMLParse.ChildExists(node: TTreenode; childNodeText: String): Boolean;
var
  lastIndex, I: Integer;
begin
  Result := false;
  lastIndex := node.GetLastChild.Index;
  for I := 0 to lastIndex do begin
    if (node.Item[I].Text = childNodeText) then begin
      Result := true;
      exit;
    end;
  end;
end;

end.

