object FormNntppost: TFormNntppost
  Left = 273
  Top = 120
  Width = 399
  Height = 349
  Caption = 'Post Message'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object MemoMessage: TMemo
    Left = 0
    Top = 65
    Width = 391
    Height = 208
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 391
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 29
      Height = 13
      Caption = 'From: '
    end
    object Label2: TLabel
      Left = 8
      Top = 40
      Width = 42
      Height = 13
      Caption = 'Subject: '
    end
    object EditFrom: TEdit
      Left = 69
      Top = 8
      Width = 300
      Height = 21
      TabOrder = 0
    end
    object EditSubject: TEdit
      Left = 69
      Top = 36
      Width = 300
      Height = 21
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 273
    Width = 391
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Panel3: TPanel
      Left = 238
      Top = 0
      Width = 153
      Height = 42
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object ButtonPost: TButton
        Left = 62
        Top = 8
        Width = 75
        Height = 23
        Caption = 'Post'
        ModalResult = 1
        TabOrder = 0
      end
    end
  end
end
