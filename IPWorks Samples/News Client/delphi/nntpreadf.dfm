object FormNntpread: TFormNntpread
  Left = 214
  Top = 112
  BorderStyle = bsSingle
  Caption = 'News Reader'
  ClientHeight = 488
  ClientWidth = 525
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnResize = FormResize
  DesignSize = (
    525
    488)
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 8
    Top = 100
    Width = 29
    Height = 13
    Caption = 'Group'
  end
  object Label2: TLabel
    Left = 8
    Top = 72
    Width = 28
    Height = 13
    Caption = 'User: '
  end
  object Label3: TLabel
    Left = 244
    Top = 72
    Width = 52
    Height = 13
    Caption = 'Password: '
  end
  object Label1: TLabel
    Left = 8
    Top = 44
    Width = 37
    Height = 13
    Caption = 'Server: '
  end
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 481
    Height = 26
    Caption = 
      'Please specify a valid news server and group and click '#39'Connect'#39 +
      '.  After connecting, click '#39'List Articles'#39' to list the current n' +
      'ews articles.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object EditGroup: TEdit
    Left = 58
    Top = 96
    Width = 459
    Height = 21
    TabOrder = 3
    Text = 'mozilla.test'
  end
  object EditUser: TEdit
    Left = 60
    Top = 68
    Width = 175
    Height = 21
    TabOrder = 1
  end
  object EditPassword: TEdit
    Left = 298
    Top = 68
    Width = 219
    Height = 21
    TabOrder = 2
  end
  object EditServer: TEdit
    Left = 60
    Top = 40
    Width = 175
    Height = 21
    TabOrder = 0
    Text = 'nntp.aioe.org'
  end
  object ListBoxMessage: TListBox
    Left = 8
    Top = 304
    Width = 509
    Height = 176
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 9
  end
  object ListViewArticles: TListView
    Left = 8
    Top = 152
    Width = 509
    Height = 145
    Anchors = [akLeft, akTop, akRight]
    Columns = <
      item
        Caption = 'Article'
      end
      item
        Caption = 'Subject'
        Width = 150
      end
      item
        Caption = 'From'
        Width = 150
      end
      item
        Caption = 'Date'
        Width = 150
      end>
    RowSelect = True
    TabOrder = 8
    ViewStyle = vsReport
    OnClick = ListViewArticlesClick
  end
  object ButtonConnect: TButton
    Left = 8
    Top = 123
    Width = 75
    Height = 23
    Caption = 'Connect'
    TabOrder = 4
    OnClick = ButtonConnectClick
  end
  object ButtonGo: TButton
    Left = 269
    Top = 123
    Width = 75
    Height = 23
    Caption = 'List Articles'
    Enabled = False
    TabOrder = 5
    OnClick = ButtonGoClick
  end
  object ButtonCompose: TButton
    Left = 349
    Top = 123
    Width = 78
    Height = 23
    Caption = 'Compose'
    Enabled = False
    TabOrder = 6
    OnClick = ButtonComposeClick
  end
  object ButtonReply: TButton
    Left = 430
    Top = 123
    Width = 87
    Height = 23
    Caption = 'Reply'
    Enabled = False
    TabOrder = 7
    OnClick = ButtonReplyClick
  end
  object ipwNNTP1: TipwNNTP
    SSLCertStore = 'MY'
    OnGroupOverview = ipwNNTP1GroupOverview
    OnHeader = ipwNNTP1Header
    OnSSLServerAuthentication = ipwNNTP1SSLServerAuthentication
    OnTransfer = ipwNNTP1Transfer
    Left = 260
    Top = 4
  end
end


