object FormJabber: TFormJabber
  Left = 255
  Top = 136
  BorderStyle = bsSingle
  Caption = 'XMPP Jabber Demo'
  ClientHeight = 346
  ClientWidth = 462
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    462
    346)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 411
    Height = 26
    Caption = 
      'This demo shows how to use the XMPP component to create a simple' +
      ' Jabber client for instant messaging.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 25
    Height = 13
    Caption = 'User:'
  end
  object Label3: TLabel
    Left = 16
    Top = 72
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object Label4: TLabel
    Left = 16
    Top = 324
    Width = 44
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Send To:'
  end
  object Label5: TLabel
    Left = 200
    Top = 72
    Width = 34
    Height = 13
    Caption = 'Server:'
  end
  object tbUser: TEdit
    Left = 72
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object tbPass: TEdit
    Left = 72
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object cbContacts: TComboBox
    Left = 64
    Top = 320
    Width = 297
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 4
  end
  object bConnect: TButton
    Left = 376
    Top = 64
    Width = 65
    Height = 21
    Caption = 'Connect'
    TabOrder = 3
    OnClick = bConnectClick
  end
  object bSend: TButton
    Left = 368
    Top = 320
    Width = 65
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Send'
    TabOrder = 6
    OnClick = bSendClick
  end
  object tbHistory: TMemo
    Left = 8
    Top = 94
    Width = 446
    Height = 171
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 7
  end
  object tbInput: TMemo
    Left = 8
    Top = 272
    Width = 446
    Height = 41
    Anchors = [akLeft, akRight, akBottom]
    Lines.Strings = (
      '<ENTER TEXT TO SEND HERE>')
    TabOrder = 5
  end
  object tbServer: TEdit
    Left = 240
    Top = 64
    Width = 129
    Height = 21
    TabOrder = 2
    Text = 'jabber.com'
  end
  object XMPP1: TipwXMPP
    AuthMethods = '*'
    Resource = 'IPWorks XMPP Agent'
    SSLCertStore = 'MY'
    OnConnected = XMPP1Connected
    OnDisconnected = XMPP1Disconnected
    OnMessageIn = XMPP1MessageIn
    OnPresence = XMPP1Presence
    OnSSLServerAuthentication = XMPP1SSLServerAuthentication
    Left = 408
    Top = 8
  end
end


