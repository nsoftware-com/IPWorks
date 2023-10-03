object FormWSClient: TFormWSClient
  Left = 0
  Top = 0
  Caption = 'WebSocketClient Demo'
  ClientHeight = 277
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  DesignSize = (
    554
    277)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 45
    Width = 23
    Height = 13
    Caption = 'URL:'
  end
  object Label3: TLabel
    Left = 8
    Top = 109
    Width = 136
    Height = 13
    Caption = 'Enter the text to send here:'
  end
  object Label2: TLabel
    Left = 261
    Top = 45
    Width = 149
    Height = 13
    Caption = 'Data received from the server:'
  end
  object Label4: TLabel
    Left = 8
    Top = 208
    Width = 35
    Height = 13
    Caption = 'Status:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblStatus: TLabel
    Left = 8
    Top = 227
    Width = 247
    Height = 42
    AutoSize = False
    Caption = 'Not Connected'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 518
    Height = 26
    Caption = 
      'This is a demo to show how to connect to a web socket echo serve' +
      'r,  send data, and receive the response. Simply fill in the serv' +
      'er to connect to. Then input the data you would like to send and' +
      ' click the '#39'Send'#39' button.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object txtServer: TEdit
    Left = 37
    Top = 42
    Width = 204
    Height = 21
    TabOrder = 0
    Text = 'ws://localhost:777'
  end
  object btnConnect: TButton
    Left = 34
    Top = 69
    Width = 79
    Height = 25
    Caption = 'Connect'
    TabOrder = 1
    OnClick = btnConnectClick
  end
  object btnDisconnect: TButton
    Left = 135
    Top = 69
    Width = 79
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 2
    OnClick = btnDisconnectClick
  end
  object txtData: TEdit
    Left = 8
    Top = 128
    Width = 233
    Height = 21
    TabOrder = 3
    Text = 'Hello, World!'
  end
  object btnSend: TButton
    Left = 81
    Top = 180
    Width = 80
    Height = 25
    Caption = 'Send'
    TabOrder = 4
    OnClick = btnSendClick
  end
  object txtReceived: TMemo
    Left = 261
    Top = 64
    Width = 285
    Height = 205
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object ipwWSClient1: TipwWSClient
    SSLCertStore = 'MY'
    OnConnected = WSClient1Connected
    OnDataIn = WSClient1DataIn
    OnDisconnected = WSClient1Disconnected
    OnError = WSClient1Error
    OnSSLServerAuthentication = WSClient1SSLServerAuthentication
    Left = 192
    Top = 224
  end
end


