object FormWhois: TFormWhois
  Left = 199
  Top = 151
  HorzScrollBar.Visible = False
  BorderStyle = bsSingle
  Caption = 'Domain Name Lookup'
  ClientHeight = 348
  ClientWidth = 408
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 60
    Width = 65
    Height = 13
    Caption = '&Domain name'
    Color = clBtnFace
    FocusControl = tDomain
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 13
    Width = 391
    Height = 26
    Caption = 
      'Clicking the button or pressing <Return> will query the whois.in' +
      'ternic.net to retrieve information about the specified domain'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object mResponse: TMemo
    Left = 8
    Top = 97
    Width = 391
    Height = 243
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object tDomain: TEdit
    Left = 102
    Top = 60
    Width = 211
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = 'domain.com'
  end
  object bLookUp: TButton
    Left = 331
    Top = 58
    Width = 68
    Height = 23
    Caption = 'Query'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = bLookUpClick
  end
  object ipwWhois1: TipwWhois
    DefaultServer = 'whois.internic.net'
    Left = 264
    Top = 24
  end
end


