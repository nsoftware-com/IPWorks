object FormRshell: TFormRshell
  Left = 331
  Top = 189
  BorderStyle = bsSingle
  Caption = 'RShell Demo'
  ClientHeight = 413
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    436
    413)
  PixelsPerInch = 96
  TextHeight = 16
  object Label5: TLabel
    Left = 8
    Top = 182
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
    Top = 115
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
    Top = 85
    Width = 62
    Height = 13
    Caption = 'Remote &User'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 54
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
  object Label3: TLabel
    Left = 242
    Top = 85
    Width = 51
    Height = 13
    Caption = '&Local User'
    FocusControl = eLocalUser
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label7: TLabel
    Left = 8
    Top = 16
    Width = 405
    Height = 26
    Caption = 
      'This demo will allow to send commands to a remote system with th' +
      'e RSHELL control.  Remember to add "<localhost> <localuser>" to ' +
      'the .rhosts file of the remote host'
    Font.Charset = ANSI_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object mTrack: TMemo
    Left = 8
    Top = 217
    Width = 417
    Height = 188
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
    Top = 144
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
    Left = 80
    Top = 112
    Width = 345
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
  object eHost: TEdit
    Left = 80
    Top = 51
    Width = 346
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object eLocalUser: TEdit
    Left = 310
    Top = 82
    Width = 116
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
  object eRemoteUser: TEdit
    Left = 80
    Top = 82
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
  object RSHell1: TipwRshell
    FirewallPort = 80
    OnConnected = Rshell1Connected
    OnDisconnected = Rshell1Disconnected
    OnError = Rshell1Error
    OnStdout = Rshell1StdOut
    Left = 8
    Top = 145
  end
end


