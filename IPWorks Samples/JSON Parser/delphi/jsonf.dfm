object FormJson: TFormJson
  Left = 0
  Top = 0
  Caption = 'JSON Demo'
  ClientHeight = 626
  ClientWidth = 781
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
    Width = 571
    Height = 13
    Caption = 
      'This demo shows how to use the JSON component to parse JSON data' +
      '.  JSON data can be parsed from a File or String.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMenuHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 239
    Width = 66
    Height = 13
    Caption = 'Event Results'
  end
  object Label3: TLabel
    Left = 416
    Top = 239
    Width = 76
    Height = 13
    Caption = 'XPath Traversal'
  end
  object Label4: TLabel
    Left = 416
    Top = 546
    Width = 31
    Height = 13
    Caption = 'JPath:'
  end
  object Label5: TLabel
    Left = 405
    Top = 573
    Width = 42
    Height = 13
    Caption = 'Element:'
  end
  object Label6: TLabel
    Left = 417
    Top = 600
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 27
    Width = 765
    Height = 190
    Caption = 'Input JSON'
    TabOrder = 0
  end
  object rbtnFile: TRadioButton
    Left = 16
    Top = 46
    Width = 113
    Height = 17
    Caption = 'Input from File'
    TabOrder = 1
  end
  object rbtnString: TRadioButton
    Left = 16
    Top = 71
    Width = 113
    Height = 17
    Caption = 'Input from String'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object txtFile: TEdit
    Left = 135
    Top = 44
    Width = 589
    Height = 21
    TabOrder = 3
  end
  object memoString: TMemo
    Left = 16
    Top = 94
    Width = 745
    Height = 76
    Lines.Strings = (
      '{'
      '    "store": {'
      '        "book": ['
      '            {'
      '                "category": "reference",'
      '                "author": "Nigel Rees",'
      '                "title": "Sayings of the Century",'
      '                "price": 8.95'
      '            },'
      '            {'
      '                "category": "fiction",'
      '                "author": "Evelyn Waugh",'
      '                "title": "Sword of Honour",'
      '                "price": 12.99'
      '            },'
      '            {'
      '                "category": "fiction",'
      '                "author": "Herman Melville",'
      '                "title": "Moby Dick",'
      '                "isbn": "0-553-21311-3",'
      '                "price": 8.99'
      '            },'
      '            {'
      '                "category": "fiction",'
      '                "author": "J. R. R. Tolkien",'
      '                "title": "The Lord of the Rings",'
      '                "isbn": "0-395-19395-8",'
      '                "price": 22.99'
      '            }'
      '        ],'
      '        "bicycle": {'
      '            "color": "red",'
      '            "price": 19.95'
      '        }'
      '    },'
      '}')
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object btnParse: TButton
    Left = 16
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Parse'
    TabOrder = 5
    OnClick = btnParseClick
  end
  object btnSelectFile: TButton
    Left = 730
    Top = 42
    Width = 31
    Height = 25
    Caption = '...'
    TabOrder = 6
    OnClick = btnSelectFileClick
  end
  object memoEvents: TMemo
    Left = 8
    Top = 258
    Width = 393
    Height = 360
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object txtJPath: TEdit
    Left = 453
    Top = 543
    Width = 308
    Height = 21
    TabOrder = 8
  end
  object txtElement: TEdit
    Left = 453
    Top = 570
    Width = 308
    Height = 21
    TabOrder = 9
  end
  object txtValue: TEdit
    Left = 453
    Top = 597
    Width = 308
    Height = 21
    TabOrder = 10
  end
  object tvwXPath: TTreeView
    Left = 407
    Top = 258
    Width = 366
    Height = 279
    Indent = 19
    TabOrder = 11
    OnClick = tvwXPathClick
  end
  object OpenDialog1: TOpenDialog
    Left = 728
    Top = 208
  end
  object ipwJSON1: TipwJSON
    OnEndElement = ipwJSON1EndElement
    OnStartElement = ipwJSON1StartElement
    Left = 592
    Top = 208
  end
end


