object FormPing: TFormPing
  Left = 227
  Top = 162
  BorderStyle = bsSingle
  Caption = 'Ping Demo'
  ClientHeight = 339
  ClientWidth = 432
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    432
    339)
  PixelsPerInch = 96
  TextHeight = 16
  object Label2: TLabel
    Left = 8
    Top = 80
    Width = 307
    Height = 26
    Caption = 
      '(If you receive a "Socket type not supported error.", then your ' +
      ' Winsock implementation does not support ICMP (RAW) sockets).'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 8
    Top = 51
    Width = 22
    Height = 13
    Caption = '&Host'
    FocusControl = tHostName
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 267
    Height = 26
    Caption = 
      'Click on "Ping" to send a PING (ICMP ECHO) message. Rightclick t' +
      'o clear.'
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
    Top = 120
    Width = 416
    Height = 211
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 0
    OnMouseDown = lTrackMouseDown
  end
  object PingButton: TButton
    Left = 345
    Top = 47
    Width = 79
    Height = 23
    Caption = 'Ping'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = PingButtonClick
  end
  object tHostName: TEdit
    Left = 38
    Top = 48
    Width = 136
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = 'localhost'
    OnKeyPress = tHostNameKeyPress
  end
  object ClearButton: TButton
    Left = 345
    Top = 80
    Width = 79
    Height = 23
    Caption = 'Clear'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = ClearButtonClick
  end
  object ipwPing1: TipwPing
    Left = 336
    Top = 8
  end
end


