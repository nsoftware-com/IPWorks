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
unit jsonf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ipwcore, ipwtypes, ipwjson, StrUtils;

type
  TFormJson = class(TForm)
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    rbtnFile: TRadioButton;
    rbtnString: TRadioButton;
    OpenDialog1: TOpenDialog;
    txtFile: TEdit;
    memoString: TMemo;
    btnParse: TButton;
    btnSelectFile: TButton;
    Label2: TLabel;
    memoEvents: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    txtJPath: TEdit;
    txtElement: TEdit;
    txtValue: TEdit;
    tvwXPath: TTreeView;
    ipwJSON1: TipwJSON;
    procedure btnSelectFileClick(Sender: TObject);
    procedure btnParseClick(Sender: TObject);
    procedure ipwJSON1EndElement(Sender: TObject; const Element: string);
    procedure ipwJSON1StartElement(Sender: TObject; const Element: string);
    procedure tvwXPathClick(Sender: TObject);

//    function GetPath(aNode: TTreeNode): string;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormJson: TFormJson;

implementation

{$R *.dfm}

procedure TFormJson.btnParseClick(Sender: TObject);
var
inputString: String;
root: String;
I: Integer;
elemName: String;
numChildren: Integer;
anode: TTreeNode;
begin
  Try
    ipwJSON1.Reset();
    memoEvents.Clear();
    tvwXPath.Items.Clear();

    if(rbtnFile.Checked) Then
      ipwJSON1.InputFile := txtFile.Text;
    begin
      inputString := '';
      for I := 0 to memoString.Lines.Count -1 do
        begin
          inputString := inputString + memoString.Lines[I];
        end;
        inputString := LeftStr(inputString, Length(inputString) - 2);
        ipwJSON1.InputData := inputString;
    end;

    ipwJSON1.Parse();
    ipwJSON1.XPath := '/';
    root := ipwJSON1.XElement;
    ipwJSON1.XPath := '/' + root;
    tvwXPath.Items.AddFirst(nil, ipwJSON1.XPath);
    numChildren := ipwJSON1.XChildCount;

    for I := 1 to numChildren do
    begin
      ipwJSON1.XPath := '/' + root + '/[' + IntToStr(I) + ']';
      try
          elemName := ipwJSON1.XElement;
          if(Length(elemName) = 0) Then
            elemName := '[' + IntToStr(I) + ']';
          tvwXPath.Items.AddChild(tvwXPath.Items.Item[0], elemName);
        finally

        end;
    end;
    for I := 0 to tvwXPath.Items.Count -1 do
      tvwXPath.Items[I].Expanded := True;
  Finally

  End;
end;

procedure TFormJson.btnSelectFileClick(Sender: TObject);
begin
  OpenDialog1 := TOpenDialog.Create(self);
  OpenDialog1.InitialDir := GetCurrentDir;
  OpenDialog1.Filter := 'All Files (*.*)|*.*|JSON Files (*.js)|*.js';
  OpenDialog1.Options := [ofFileMustExist];

  if(OpenDialog1.Execute) Then
    txtFile.Text := openDialog1.FileName;

  OpenDialog1.Free();
end;

procedure TFormJson.ipwJSON1EndElement(Sender: TObject; const Element: string);
begin
  memoEvents.Lines.Add('End Element: ' + Element);
end;


procedure TFormJson.ipwJSON1StartElement(Sender: TObject; const Element: string);
begin
  memoEvents.Lines.Add('Start Element: ' + Element);
end;

procedure TFormJson.tvwXPathClick(Sender: TObject);
var
parent: String;
numChildren: Integer;
I: Integer;
elemName: String;
node: TTreeNode;
nodePath: String;
begin
  if tvwXPath.Selected.Parent <> nil then
  begin
    nodePath := tvwXPath.Selected.Text;

  if tvwXPath.Selected <> nil then
    if tvwXPath.Selected.Level = 0 then
      nodePath := tvwXPath.Selected.Text
    else
    begin
      node := tvwXPath.Selected;
      while(node <> nil) and (node.Level <> 0) do
      begin
        node := node.Parent;
        nodePath := node.Text + '/' + nodePath;
      end;
    end;
    ipwJSON1.XPath := nodePath;
    txtJPath.Text := ipwJSON1.XPath;
    txtElement.Text := ipwJSON1.XElement;
    txtValue.Text := ipwJSON1.XText;

    parent := nodePath;
    numChildren := ipwJSON1.XChildCount;
    tvwXPath.Selected.DeleteChildren();

    for I := 1 to numChildren do
      begin
        ipwJSON1.XPath := parent + '/[' + IntToStr(I) + ']';
        try
          elemName := ipwJSON1.XElement;
          if(Length(elemName) = 0) Then
            elemName := '[' + IntToStr(I) + ']';
          tvwXPath.Items.AddChild(tvwXPath.Selected, elemName);
        finally

        end;
      end;
    for I := 0 to tvwXPath.Items.Count -1 do
      tvwXPath.Items[I].Expanded := True;
  end;
end;

Function GetPath(const aNode: TTreeNode): string;
var
nodePath: String;
node: TTreeNode;
begin
  nodePath := aNode.Text;

  if aNode <> nil then
    if aNode.Level = 0 then
      result := aNode.Text
    else
    begin
      node := aNode;
      while(node <> nil) and (node.Level <> 0) do
      begin
        node := node.Parent;
        nodePath := node.Text + '/' + nodePath;
      end;
    end;
    result := nodePath;
end;
end.

