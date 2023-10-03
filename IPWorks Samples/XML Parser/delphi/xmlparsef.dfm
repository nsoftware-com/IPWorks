object FormXMLParse: TFormXMLParse
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'XML Parser Demo'
  ClientHeight = 717
  ClientWidth = 770
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 741
    Height = 26
    Caption = 
      'This demo shows how to parse XML via the events or by traversing' +
      ' the XPath property of the component.  Select from either HTTP r' +
      'etrieval, Parse file, or Input of string values.'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 10
    Top = 280
    Width = 66
    Height = 13
    Caption = 'Event Results'
  end
  object Label3: TLabel
    Left = 292
    Top = 280
    Width = 74
    Height = 13
    Caption = 'Traverse XPath'
  end
  object Label4: TLabel
    Left = 289
    Top = 637
    Width = 32
    Height = 13
    Caption = 'XPath:'
  end
  object Label5: TLabel
    Left = 288
    Top = 664
    Width = 42
    Height = 13
    Caption = 'Element:'
  end
  object Label6: TLabel
    Left = 288
    Top = 691
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object rbHttp: TRadioButton
    Left = 8
    Top = 48
    Width = 106
    Height = 17
    Caption = 'Input From HTTP'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object rbFile: TRadioButton
    Left = 8
    Top = 71
    Width = 97
    Height = 17
    Caption = 'Input From File'
    TabOrder = 1
  end
  object rbString: TRadioButton
    Left = 8
    Top = 94
    Width = 105
    Height = 17
    Caption = 'Input From String'
    TabOrder = 2
  end
  object txtURL: TEdit
    Left = 120
    Top = 46
    Width = 641
    Height = 21
    TabOrder = 3
    Text = 'http://www.nsoftware.com/rss/'
  end
  object txtFile: TEdit
    Left = 119
    Top = 73
    Width = 561
    Height = 21
    TabOrder = 4
  end
  object Browse: TButton
    Left = 686
    Top = 73
    Width = 75
    Height = 25
    Caption = 'Browse'
    TabOrder = 5
    OnClick = BrowseClick
  end
  object memString: TMemo
    Left = 8
    Top = 117
    Width = 753
    Height = 75
    Lines.Strings = (
      '<foo><first>hello</first></foo>')
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object chkValidate: TCheckBox
    Left = 8
    Top = 205
    Width = 161
    Height = 17
    Caption = 'Validate (Uncheck for HTML)'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object btnParse: TButton
    Left = 8
    Top = 231
    Width = 75
    Height = 25
    Caption = 'Parse'
    TabOrder = 8
    OnClick = btnParseClick
  end
  object memEvents: TMemo
    Left = 8
    Top = 299
    Width = 274
    Height = 409
    ScrollBars = ssBoth
    TabOrder = 9
  end
  object treeXPath: TTreeView
    Left = 288
    Top = 299
    Width = 473
    Height = 326
    Indent = 19
    TabOrder = 10
    OnClick = treeXPathClick
  end
  object txtXPath: TEdit
    Left = 336
    Top = 634
    Width = 425
    Height = 21
    TabOrder = 11
  end
  object txtElement: TEdit
    Left = 336
    Top = 661
    Width = 425
    Height = 21
    TabOrder = 12
  end
  object txtValue: TEdit
    Left = 336
    Top = 688
    Width = 425
    Height = 21
    TabOrder = 13
  end
  object OpenDialog1: TOpenDialog
    Filter = 'All Files (*.*)|*.*|XML Files (*.xml)|*.xml'
    Left = 504
    Top = 216
  end
  object ipwHTTP1: TipwHTTP
    SSLCertStore = 'MY'
    TransferredDataLimit = 65536
    OnSSLServerAuthentication = ipwHTTP1SSLServerAuthentication
    Left = 384
    Top = 216
  end
  object ipwXML1: TipwXML
    OnEndElement = ipwXML1EndElement
    OnStartElement = ipwXML1StartElement
    Left = 608
    Top = 224
  end
end


