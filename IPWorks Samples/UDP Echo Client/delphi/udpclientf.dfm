object FormUdpclient: TFormUdpclient
  Left = 318
  Top = 119
  Caption = 'UDP Echo Client'
  ClientHeight = 360
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  DesignSize = (
    421
    360)
  TextHeight = 16
  object Label2: TLabel
    Left = 256
    Top = 58
    Width = 66
    Height = 13
    Caption = 'Remote Port'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 8
    Top = 58
    Width = 62
    Height = 13
    Caption = 'Remote Host'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 109
    Width = 106
    Height = 13
    Caption = 'Response from server:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Intro: TLabel
    Left = 8
    Top = 8
    Width = 405
    Height = 41
    AutoSize = False
    Caption = 
      'This demo show how to use the UDP component to set up a simple U' +
      'DP client. Simply set the remote host and port, input the data y' +
      'ou would like to send, and click the Send button.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lTrack: TListBox
    Left = 8
    Top = 128
    Width = 405
    Height = 224
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 4
    OnDblClick = lTrackDblClick
  end
  object Button1: TButton
    Left = 339
    Top = 82
    Width = 74
    Height = 23
    Caption = '&Send'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = Button1Click
  end
  object tEcho: TEdit
    Left = 8
    Top = 82
    Width = 325
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = 'Hi There :)'
  end
  object tPort: TEdit
    Left = 328
    Top = 55
    Width = 85
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = '777'
  end
  object tHost: TEdit
    Left = 88
    Top = 55
    Width = 162
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = '255.255.255.255'
  end
  object UDPClient1: TipwUDP
    OnDataIn = UDPClient1DataIn
    OnError = UDPClient1Error
    Left = 280
    Top = 280
  end
end


