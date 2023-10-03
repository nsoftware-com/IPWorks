object FormHttpurl: TFormHttpurl
  Left = 235
  Top = 148
  Caption = 'HTTP Get Demo'
  ClientHeight = 484
  ClientWidth = 579
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    579
    484)
  PixelsPerInch = 96
  TextHeight = 16
  object Label2: TLabel
    Left = 8
    Top = 72
    Width = 37
    Height = 13
    Caption = '&Diskfile:'
    FocusControl = tDiskfile
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 8
    Top = 40
    Width = 28
    Height = 13
    Caption = '&URL: '
    FocusControl = tURL
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 12
    Width = 343
    Height = 13
    Caption = 
      'Example of using the HTTP control to save the content of a URL t' +
      'o disk.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lTrack: TListBox
    Left = 0
    Top = 137
    Width = 571
    Height = 320
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 8
    Top = 104
    Width = 425
    Height = 25
    Caption = '0%'
    TabOrder = 1
    object Shape1: TShape
      Left = 0
      Top = 0
      Width = 45
      Height = 25
      Brush.Color = clHighlight
    end
  end
  object tDiskfile: TEdit
    Left = 80
    Top = 68
    Width = 257
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = 'c:\index.htm'
  end
  object tURL: TEdit
    Left = 80
    Top = 36
    Width = 257
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Text = 'http://www.google.com/'
  end
  object Button1: TButton
    Left = 350
    Top = 36
    Width = 81
    Height = 23
    Caption = '&Get File'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 350
    Top = 68
    Width = 81
    Height = 23
    Caption = '&Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = Button2Click
  end
  object HTTP1: TipwHTTP
    SSLCertStore = 'MY'
    OnConnected = HTTP1Connected
    OnDisconnected = HTTP1Disconnected
    OnEndTransfer = HTTP1EndTransfer
    OnHeader = HTTP1Header
    OnSSLServerAuthentication = HTTP1SSLServerAuthentication
    OnStartTransfer = HTTP1StartTransfer
    OnTransfer = HTTP1Transfer
    Left = 440
    Top = 200
  end
end


