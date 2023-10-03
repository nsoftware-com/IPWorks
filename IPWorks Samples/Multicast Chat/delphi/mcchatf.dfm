object FormMcchat: TFormMcchat
  Left = 243
  Top = 142
  BorderStyle = bsSingle
  Caption = 'Multicast Demo'
  ClientHeight = 449
  ClientWidth = 508
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    508
    449)
  PixelsPerInch = 96
  TextHeight = 16
  object Label2: TLabel
    Left = 9
    Top = 92
    Width = 75
    Height = 13
    Caption = '&Multicast group:'
    FocusControl = eGroup
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 8
    Top = 60
    Width = 54
    Height = 13
    Caption = 'Your &name:'
    FocusControl = eName
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 7
    Top = 8
    Width = 448
    Height = 39
    Caption = 
      'Start different instances of this demo and talk with your friend' +
      's using the Multicast control.  You must first join the multicas' +
      't group before you can send a message.  Please see the help file' +
      ' for more information.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label3: TLabel
    Left = 8
    Top = 426
    Width = 46
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Me&ssage:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object mMessages: TMemo
    Left = 8
    Top = 120
    Width = 492
    Height = 289
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object eGroup: TEdit
    Left = 113
    Top = 92
    Width = 176
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = '231.31.31.31'
  end
  object eName: TEdit
    Left = 113
    Top = 60
    Width = 176
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = 'User 1'
  end
  object btnCommand1: TButton
    Left = 312
    Top = 60
    Width = 73
    Height = 23
    Caption = '&Join'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = btnCommand1Click
  end
  object eLine: TEdit
    Left = 64
    Top = 422
    Width = 355
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    Text = 'Hi folks :)'
  end
  object btnSend: TButton
    Left = 426
    Top = 422
    Width = 74
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '&Send'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = btnSendClick
  end
  object MCast1: TipwMCast
    OnDataIn = MCast1DataIn
    Left = 320
    Top = 88
  end
end


