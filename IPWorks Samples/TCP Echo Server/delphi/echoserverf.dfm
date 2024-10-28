object FormEchoserver: TFormEchoserver
  Left = 321
  Top = 119
  Caption = 'TCP Echo Service Server'
  ClientHeight = 305
  ClientWidth = 399
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  DesignSize = (
    399
    305)
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 99
    Width = 377
    Height = 26
    Caption = 
      'If you get an "address already in use" error then your system --' +
      'for instance Windows NT-- has already implemented this service. ' +
      'Choose another port to test'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 25
    Height = 13
    Caption = 'Port: '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 377
    Height = 39
    Caption = 
      'This demo shows how to set up an echo server running on your com' +
      'puter.  You need to specify the port that you would like to have' +
      ' the server listening to and then START the server.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lTrack: TListBox
    Left = 0
    Top = 129
    Width = 402
    Height = 183
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 3
    OnDblClick = lTrackDblClick
  end
  object ButtonStart: TButton
    Left = 192
    Top = 64
    Width = 97
    Height = 23
    Caption = '&Start'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = ButtonStartClick
  end
  object ButtonStop: TButton
    Left = 304
    Top = 64
    Width = 89
    Height = 23
    Caption = 'S&top'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = ButtonStopClick
  end
  object EditLocalPort: TEdit
    Left = 56
    Top = 64
    Width = 105
    Height = 21
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = '777'
  end
  object TCPServer1: TipwTCPServer
    SSLCertStore = 'MY'
    OnConnected = TCPServer1Connected
    OnDataIn = TCPServer1DataIn
    OnDisconnected = TCPServer1Disconnected
    OnError = TCPServer1Error
    Left = 135
    Top = 33
  end
end


