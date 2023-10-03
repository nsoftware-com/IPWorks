object FormSmpp: TFormSmpp
  Left = 192
  Top = 114
  BorderStyle = bsSingle
  Caption = 'SMPP Demo'
  ClientHeight = 312
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 358
    Height = 26
    Caption = 
      'This demo uses the SMPP component to send SMS messages.  The use' +
      ' of your own server is required.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 34
    Height = 13
    Caption = 'Server:'
  end
  object Label3: TLabel
    Left = 8
    Top = 72
    Width = 25
    Height = 13
    Caption = 'User:'
  end
  object Label4: TLabel
    Left = 192
    Top = 72
    Width = 26
    Height = 13
    Caption = 'Pass:'
  end
  object Label5: TLabel
    Left = 8
    Top = 96
    Width = 51
    Height = 13
    Caption = 'Recipient::'
  end
  object txtServer: TEdit
    Left = 64
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object txtUser: TEdit
    Left = 64
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object txtPassword: TEdit
    Left = 232
    Top = 64
    Width = 140
    Height = 21
    TabOrder = 2
  end
  object txtRecipient: TEdit
    Left = 64
    Top = 88
    Width = 308
    Height = 21
    TabOrder = 3
    Text = '<Enter Recipient Address or Phone Number Here>'
  end
  object txtMessage: TMemo
    Left = 8
    Top = 152
    Width = 364
    Height = 152
    Lines.Strings = (
      'Enter message to send here...'
      '')
    TabOrder = 4
  end
  object bSend: TButton
    Left = 152
    Top = 120
    Width = 65
    Height = 25
    Caption = 'Send'
    TabOrder = 5
    OnClick = bSendClick
  end
  object SMPP1: TipwSMPP
    SSLCertStore = 'MY'
    OnDisconnected = SMPP1Disconnected
    OnSSLServerAuthentication = SMPP1SSLServerAuthentication
    Left = 248
    Top = 120
  end
end


