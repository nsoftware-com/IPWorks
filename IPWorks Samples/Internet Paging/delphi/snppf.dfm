object FormSnpp: TFormSnpp
  Left = 266
  Top = 104
  BorderStyle = bsSingle
  Caption = 'SNPP Pager'
  ClientHeight = 381
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    422
    381)
  PixelsPerInch = 96
  TextHeight = 13
  object Label7: TLabel
    Left = 8
    Top = 286
    Width = 33
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Status:'
  end
  object Label3: TLabel
    Left = 8
    Top = 92
    Width = 16
    Height = 13
    Caption = 'To:'
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 43
    Height = 13
    Caption = 'Caller ID:'
  end
  object Label1: TLabel
    Left = 8
    Top = 36
    Width = 70
    Height = 13
    Caption = 'Paging Server:'
  end
  object Label6: TLabel
    Left = 8
    Top = 8
    Width = 299
    Height = 13
    Caption = 'This demo can be used to send a page via the SNPP protocol. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object MemoMessage: TMemo
    Left = 8
    Top = 132
    Width = 407
    Height = 145
    Lines.Strings = (
      'Testing the pager')
    TabOrder = 3
  end
  object ListBoxPITrail: TListBox
    Left = 8
    Top = 305
    Width = 407
    Height = 68
    ItemHeight = 13
    TabOrder = 4
  end
  object ComboServer: TComboBox
    Left = 84
    Top = 32
    Width = 201
    Height = 21
    TabOrder = 0
    Text = 'pagemart.net'
    Items.Strings = (
      'snpp.airtouch.com'
      'snpp.metrocall.com'
      'pecos.nextel.com'
      'airmessage.net'
      'epage.porta-phone.com'
      'page.propage.net'
      'snpp.skytel.com:7777'
      'alphanow.net')
  end
  object EditFrom: TEdit
    Left = 84
    Top = 60
    Width = 200
    Height = 21
    TabOrder = 1
  end
  object EditNumber: TEdit
    Left = 84
    Top = 88
    Width = 200
    Height = 21
    TabOrder = 2
    Text = '9195551212'
  end
  object ButtonPage: TButton
    Left = 308
    Top = 88
    Width = 75
    Height = 23
    Caption = 'S&end Page'
    TabOrder = 5
    OnClick = ButtonPageClick
  end
  object ipwSNPP1: TipwSNPP
    FirewallPort = 80
    SSLCertStore = 'MY'
    OnError = ipwSNPP1Error
    OnPITrail = ipwSNPP1PITrail
    OnSSLServerAuthentication = ipwSNPP1SSLServerAuthentication
    Left = 300
    Top = 52
  end
end


