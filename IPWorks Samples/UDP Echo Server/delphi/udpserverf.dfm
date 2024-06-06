object FormUdpserver: TFormUdpserver
  Left = 321
  Top = 119
  Caption = 'UDP Echo Server'
  ClientHeight = 373
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  DesignSize = (
    465
    373)
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 82
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
    Top = 56
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
    Width = 449
    Height = 39
    Caption = 
      'This demo shows how to set up an echo server using the UDP compo' +
      'nent. Simply specify the port for the server to listen on and cl' +
      'ick the Start button. The server will echo back any data receive' +
      'd.'
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
    Top = 114
    Width = 449
    Height = 251
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
    Left = 265
    Top = 53
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
    Left = 368
    Top = 53
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
    Left = 39
    Top = 53
    Width = 122
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
  object UDPServer1: TipwUDP
    OnDataIn = UDPServer1DataIn
    OnError = UDPServer1Error
    Left = 312
    Top = 240
  end
end


