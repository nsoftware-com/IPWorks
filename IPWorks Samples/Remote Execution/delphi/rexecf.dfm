object FormRexec: TFormRexec
  Left = 356
  Top = 202
  BorderStyle = bsSingle
  Caption = 'Rexec Demo'
  ClientHeight = 387
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    422
    387)
  PixelsPerInch = 96
  TextHeight = 16
  object Label5: TLabel
    Left = 8
    Top = 174
    Width = 401
    Height = 27
    AutoSize = False
    Caption = 
      'The following window has Std&Out, StdErr, and StdIIn. Enter text' +
      ' as appropriate. Right-click below to clear.'
    Color = clBtnFace
    FocusControl = mTrack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 8
    Top = 107
    Width = 47
    Height = 13
    Caption = '&Command'
    FocusControl = eCommand
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 8
    Top = 77
    Width = 22
    Height = 13
    Caption = '&User'
    FocusControl = eUser
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 226
    Top = 77
    Width = 46
    Height = 13
    Caption = '&Password'
    FocusControl = ePasswd
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 46
    Width = 22
    Height = 13
    Caption = '&Host'
    FocusControl = eHost
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 8
    Top = 16
    Width = 393
    Height = 13
    Caption = 
      'This demo will allow to send commands to a remote system with th' +
      'e REXEC control.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object mTrack: TMemo
    Left = 8
    Top = 209
    Width = 406
    Height = 170
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 5
    OnKeyPress = mTrackKeyPress
    OnMouseDown = mTrackMouseDown
  end
  object Button1: TButton
    Left = 179
    Top = 136
    Width = 86
    Height = 23
    Caption = 'E&xecute'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = Button1Click
  end
  object eCommand: TEdit
    Left = 64
    Top = 104
    Width = 350
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Text = 'ls -l'
    OnKeyPress = eCommandKeyPress
  end
  object eUser: TEdit
    Left = 64
    Top = 74
    Width = 129
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object ePasswd: TEdit
    Left = 294
    Top = 74
    Width = 120
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    PasswordChar = '*'
    TabOrder = 2
  end
  object eHost: TEdit
    Left = 64
    Top = 43
    Width = 350
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object REXec1: TipwRexec
    FirewallPort = 80
    OnConnected = Rexec1Connected
    OnDisconnected = Rexec1Disconnected
    OnError = Rexec1Error
    OnStderr = Rexec1Stderr
    OnStdout = Rexec1StdOut
    Left = 11
    Top = 136
  end
end


