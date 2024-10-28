object FormTracert: TFormTracert
  Left = 192
  Top = 107
  BorderStyle = bsSingle
  Caption = 'TraceRoute Demo'
  ClientHeight = 378
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 413
    Height = 26
    Caption = 
      'This is a demo of the TraceRoute control.  This will allow you t' +
      'o trace the path of IP packets along the internet.  Just fill in' +
      ' the remote host name and click on '#39'Trace'#39' to start.'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 65
    Height = 13
    Caption = 'Remote Host:'
  end
  object txtHost: TEdit
    Left = 88
    Top = 48
    Width = 265
    Height = 21
    TabOrder = 0
    Text = 'www.yahoo.com'
  end
  object btnTrace: TButton
    Left = 368
    Top = 48
    Width = 74
    Height = 25
    Caption = 'Trace'
    TabOrder = 1
    OnClick = btnTraceClick
  end
  object memoResults: TMemo
    Left = 8
    Top = 80
    Width = 434
    Height = 289
    TabOrder = 2
  end
  object TraceRoute1: TipwTraceRoute
    OnHop = TraceRoute1Hop
    Left = 336
    Top = 48
  end
end


