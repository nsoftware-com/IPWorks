object FormNetclock: TFormNetclock
  Left = 192
  Top = 114
  Width = 414
  Height = 205
  Caption = 'Demo of NetClock'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 11
    Width = 369
    Height = 37
    AutoSize = False
    Caption = 
      'This sample demonstrates the NetClock controls ability to get th' +
      'e time from a specified time server and then set the current tim' +
      'e on the client computer.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 60
    Height = 13
    Caption = 'Time Server:'
  end
  object Label3: TLabel
    Left = 8
    Top = 80
    Width = 60
    Height = 13
    Caption = 'Server Time:'
  end
  object Label4: TLabel
    Left = 8
    Top = 104
    Width = 63
    Height = 13
    Caption = 'System Time:'
  end
  object txtServerTime: TEdit
    Left = 80
    Top = 80
    Width = 305
    Height = 21
    TabOrder = 0
  end
  object txtSystemTime: TEdit
    Left = 80
    Top = 104
    Width = 305
    Height = 21
    TabOrder = 1
  end
  object bSynchronize: TButton
    Left = 296
    Top = 128
    Width = 89
    Height = 25
    Caption = 'Synchronize'
    TabOrder = 2
    OnClick = bSynchronizeClick
  end
  object txtTimeServer: TEdit
    Left = 80
    Top = 56
    Width = 305
    Height = 21
    TabOrder = 3
    Text = 'time.nist.gov'
  end
  object ipwNetClock1: TipwNetClock
    TimeServer = 'time.nist.gov'
    Left = 64
    Top = 128
  end
  object Timer1: TTimer
    Interval = 5000
    OnTimer = Timer1Timer
    Left = 96
    Top = 128
  end
end


