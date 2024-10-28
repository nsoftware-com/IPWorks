object FormDns: TFormDns
  Left = 258
  Top = 139
  Width = 434
  Height = 377
  Caption = 'DNS Record Query'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    426
    350)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 0
    Width = 377
    Height = 26
    Caption = 
      'This demo shows how to use the DNS component to query name serve' +
      'rs for 21 different types of resource records, including Address' +
      ' and MX records.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 10
    Top = 36
    Width = 60
    Height = 13
    Caption = 'DNS Server:'
  end
  object Label3: TLabel
    Left = 10
    Top = 60
    Width = 54
    Height = 13
    Caption = 'Host name:'
  end
  object tbDNS: TEdit
    Left = 72
    Top = 32
    Width = 193
    Height = 21
    TabOrder = 0
  end
  object txtDomain: TEdit
    Left = 72
    Top = 56
    Width = 193
    Height = 21
    TabOrder = 1
    Text = 'microsoft.com'
  end
  object bQuery: TButton
    Left = 288
    Top = 56
    Width = 57
    Height = 21
    Caption = 'Query'
    TabOrder = 2
    OnClick = bQueryClick
  end
  object lvwRecords: TListView
    Left = 0
    Top = 88
    Width = 425
    Height = 265
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = 'ID'
      end
      item
        AutoSize = True
        Caption = 'Type'
      end
      item
        AutoSize = True
        Caption = 'Field'
      end
      item
        AutoSize = True
        Caption = 'Value'
      end>
    TabOrder = 3
    ViewStyle = vsReport
  end
  object dns1: TipwDNS
    OnResponse = dns1_OnResponse
    Left = 288
    Top = 24
  end
end


