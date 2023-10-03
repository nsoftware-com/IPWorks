object FormEchoclient: TFormEchoclient
  Left = 318
  Top = 119
  Caption = 'TCP Echo Client'
  ClientHeight = 360
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnDestroy = FormDestroy
  DesignSize = (
    425
    360)
  PixelsPerInch = 96
  TextHeight = 16
  object Label2: TLabel
    Left = 8
    Top = 104
    Width = 59
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
    Top = 72
    Width = 71
    Height = 13
    Caption = 'Remote Server'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lTrack: TListBox
    Left = 184
    Top = 72
    Width = 233
    Height = 258
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 1
    OnMouseDown = lTrackMouseDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 425
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label3: TLabel
      Left = 8
      Top = 8
      Width = 417
      Height = 49
      AutoSize = False
      Caption = 
        'This is a demo to show how to connect to a remote echo server, s' +
        'end data, and receive the echoed response.  Simply fill in the t' +
        'he server to connect to and the port it is using.  Then input th' +
        'e data you would like to send and SEND it.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label4: TLabel
      Left = 216
      Top = 56
      Width = 125
      Height = 13
      Caption = 'Data received from server:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
  object GroupBoxStatus: TGroupBox
    Left = 8
    Top = 240
    Width = 169
    Height = 113
    Caption = 'Status'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object ListStatus: TListBox
      Left = 10
      Top = 16
      Width = 151
      Height = 89
      BorderStyle = bsNone
      Color = clBtnFace
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object bEcho: TButton
    Left = 57
    Top = 208
    Width = 75
    Height = 23
    Caption = '&Send'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = bEchoClick
  end
  object tEcho: TEdit
    Left = 8
    Top = 175
    Width = 169
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    Text = 'Hi There :)'
    OnKeyPress = tEchoKeyPress
  end
  object bConnect: TButton
    Left = 8
    Top = 138
    Width = 81
    Height = 23
    Caption = '&Connect'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = bConnectClick
  end
  object Disconnect: TButton
    Left = 96
    Top = 138
    Width = 81
    Height = 23
    Caption = '&Disconnect'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = DisconnectClick
  end
  object EditPort: TEdit
    Left = 96
    Top = 105
    Width = 41
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    Text = '777'
    OnKeyPress = tHostKeyPress
  end
  object tHost: TEdit
    Left = 96
    Top = 73
    Width = 81
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    Text = 'Localhost'
    OnKeyPress = tHostKeyPress
  end
  object TCPClient1: TipwTCPClient
    FirewallPort = 80
    SSLCertStore = 'MY'
    OnConnected = TCPClient1Connected
    OnDataIn = TCPClient1DataIn
    OnDisconnected = TCPClient1Disconnected
    OnError = TCPClient1Error
    OnReadyToSend = TCPClient1ReadyToSend
    OnSSLServerAuthentication = TCPClient1SSLServerAuthentication
    Left = 11
    Top = 202
  end
end


