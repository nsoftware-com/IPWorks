object FormResolveip: TFormResolveip
  Left = 359
  Top = 292
  BorderStyle = bsSingle
  Caption = 'Resolve IP Demo'
  ClientHeight = 501
  ClientWidth = 508
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    508
    501)
  PixelsPerInch = 96
  TextHeight = 16
  object Label3: TLabel
    Left = 8
    Top = 79
    Width = 82
    Height = 13
    Caption = 'P&ending requests'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 141
    Top = 79
    Width = 40
    Height = 13
    Caption = '&Answers'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 8
    Top = 42
    Width = 93
    Height = 13
    Caption = '&Host name/address'
    FocusControl = tHost
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 8
    Top = 11
    Width = 191
    Height = 13
    Caption = 'Query host names, address and services'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lPending: TListBox
    Left = 8
    Top = 104
    Width = 128
    Height = 389
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ItemHeight = 13
    ParentFont = False
    TabOrder = 0
  end
  object lTrack: TListBox
    Left = 138
    Top = 104
    Width = 362
    Height = 389
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ItemHeight = 13
    ParentFont = False
    TabOrder = 1
  end
  object tHost: TEdit
    Left = 124
    Top = 38
    Width = 196
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnKeyPress = tHostKeyPress
  end
  object Button1: TButton
    Left = 326
    Top = 38
    Width = 72
    Height = 23
    Caption = '&Look Up'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 413
    Top = 38
    Width = 76
    Height = 23
    Caption = '&Reset'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = Button3Click
  end
  object IPInfo1: TipwIPInfo
    OnRequestComplete = IPInfo1RequestComplete
    Left = 414
    Top = 68
  end
end


