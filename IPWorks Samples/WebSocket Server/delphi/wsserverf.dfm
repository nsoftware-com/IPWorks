object FormWSServer: TFormWSServer
  Left = 0
  Top = 0
  Caption = 'WebSocketServer Demo'
  ClientHeight = 398
  ClientWidth = 462
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    462
    398)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 51
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object Label2: TLabel
    Left = 8
    Top = 3
    Width = 446
    Height = 39
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This demo shows how to set up a web socket echo server.  Simply ' +
      'specify the port for the server to listen on and click the '#39'Star' +
      't'#39' button.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object txtPort: TEdit
    Left = 38
    Top = 48
    Width = 59
    Height = 21
    TabOrder = 0
    Text = '777'
  end
  object btnStart: TButton
    Left = 182
    Top = 46
    Width = 81
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 269
    Top = 46
    Width = 81
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 3
    OnClick = btnStopClick
  end
  object txtLog: TMemo
    Left = 8
    Top = 77
    Width = 446
    Height = 313
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object chkUseSSL: TCheckBox
    Left = 103
    Top = 46
    Width = 73
    Height = 25
    Caption = 'Use SSL?'
    TabOrder = 1
  end
  object ipwWSServer1: TipwWSServer
    SSLCertStore = 'MY'
    OnConnected = WSServer1Connected
    OnDataIn = WSServer1DataIn
    OnDisconnected = WSServer1Disconnected
    OnError = WSServer1Error
    Left = 160
    Top = 312
  end
end


