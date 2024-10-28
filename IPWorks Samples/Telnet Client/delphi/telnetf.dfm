object FormTelnet: TFormTelnet
  Left = 228
  Top = 118
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderStyle = bsSingle
  Caption = 'Telnet Demo'
  ClientHeight = 402
  ClientWidth = 412
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
    Width = 53
    Height = 13
    Caption = 'Host Name'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 15
    Width = 396
    Height = 26
    Caption = 
      'This is a demo of how to use the TELNET control to set up a teln' +
      'et client to interact with a remote host.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Memo1: TMemo
    Left = 8
    Top = 88
    Width = 396
    Height = 251
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
    OnKeyPress = Memo1KeyPress
    OnMouseDown = Memo2MouseDown
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 344
    Width = 396
    Height = 52
    Caption = 'Status'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object Memo2: TMemo
      Left = 2
      Top = 15
      Width = 392
      Height = 35
      Align = alClient
      BorderStyle = bsNone
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Lines.Strings = (
        '')
      ParentFont = False
      TabOrder = 0
      OnMouseDown = Memo2MouseDown
      ExplicitWidth = 412
    end
  end
  object tHost: TEdit
    Left = 73
    Top = 57
    Width = 246
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnKeyPress = tHostKeyPress
  end
  object bLookUp: TButton
    Left = 336
    Top = 59
    Width = 68
    Height = 23
    Caption = '&Connect'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = bLookUpClick
  end
  object Telnet1: TipwTelnet
    FirewallPort = 80
    SSLCertStore = 'MY'
    OnCommand = Telnet1Command
    OnConnected = Telnet1Connected
    OnDataIn = Telnet1DataIn
    OnDisconnected = Telnet1Disconnected
    OnDo = Telnet1Do
    OnDont = Telnet1Dont
    OnSSLServerAuthentication = Telnet1SSLServerAuthentication
    OnSubOption = Telnet1SubOption
    OnWill = Telnet1Will
    OnWont = Telnet1Wont
    Left = 105
    Top = 30
  end
end


