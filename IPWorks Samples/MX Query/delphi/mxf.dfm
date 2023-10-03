object FormMx: TFormMx
  Left = 236
  Top = 130
  Caption = 'IPWorks MX Demo'
  ClientHeight = 385
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  DesignSize = (
    430
    385)
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 8
    Top = 140
    Width = 67
    Height = 13
    Caption = 'Email Servers:'
  end
  object Label3: TLabel
    Left = 8
    Top = 108
    Width = 69
    Height = 13
    Caption = 'Email Address:'
  end
  object Label2: TLabel
    Left = 8
    Top = 77
    Width = 60
    Height = 13
    Caption = 'DNS Server:'
  end
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 395
    Height = 39
    Caption = 
      'With the MX control, you may search for the names of the servers' +
      ' that handle a particular email address.  In this demo, simply s' +
      'upply the name of the DNS Server to request info from, and the e' +
      'mail address to search for the mail servers for.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object ListServer: TMemo
    Left = 0
    Top = 161
    Width = 430
    Height = 222
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object EditDNSServer: TEdit
    Left = 88
    Top = 72
    Width = 217
    Height = 21
    TabOrder = 1
    Text = 'EditDNSServer'
  end
  object EditEmail: TEdit
    Left = 88
    Top = 104
    Width = 217
    Height = 21
    TabOrder = 2
    Text = 'billg@microsoft.com'
  end
  object ButtonQuery: TButton
    Left = 320
    Top = 72
    Width = 89
    Height = 23
    Caption = '&List Servers'
    TabOrder = 3
    OnClick = ButtonQueryClick
  end
  object ipwMX1: TipwMX
    OnResponse = ipwMX1Response
    Left = 320
    Top = 104
  end
end


