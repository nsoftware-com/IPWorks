object FormUdpecho: TFormUdpecho
  Left = 321
  Top = 118
  BorderStyle = bsSingle
  Caption = 'ECHO Broadcasts'
  ClientHeight = 295
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 16
  object Label3: TLabel
    Left = 8
    Top = 6
    Width = 395
    Height = 27
    AutoSize = False
    Caption = 
      'Clicking on the button will broadcast a request on the ECHO port' +
      ' to all the hosts on your LAN.  The roundtrip times will be show' +
      'n below.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lTrack: TListBox
    Left = 8
    Top = 73
    Width = 395
    Height = 214
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 0
    OnDblClick = lTrackDblClick
  end
  object Button1: TButton
    Left = 162
    Top = 39
    Width = 75
    Height = 23
    Caption = '&Search'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object UDP1: TipwUDP
    OnDataIn = UDP1DataIn
    OnError = UDP1Error
    Left = 8
    Top = 42
  end
end


