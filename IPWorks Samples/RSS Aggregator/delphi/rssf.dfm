object FormRss: TFormRss
  Left = 203
  Top = 198
  BorderStyle = bsSingle
  Caption = 'RSS Reader'
  ClientHeight = 485
  ClientWidth = 698
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
    Left = 8
    Top = 8
    Width = 580
    Height = 13
    Caption = 
      'This application demonstrates the use of the RSS component.  Ent' +
      'er the URL for an RSS document to browse its contents.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 31
    Width = 68
    Height = 13
    Caption = 'Feed Address:'
  end
  object lblChannelDescription: TLabel
    Left = 8
    Top = 64
    Width = 65
    Height = 13
    Caption = 'Channel Title:'
  end
  object txtURL: TEdit
    Left = 80
    Top = 28
    Width = 545
    Height = 21
    TabOrder = 0
    Text = 'http://www.nsoftware.com/rss/default.rsb'
  end
  object lvwItems: TListView
    Left = 8
    Top = 80
    Width = 681
    Height = 145
    Columns = <
      item
        Caption = 'Title'
        Width = 500
      end
      item
        Caption = 'Date'
        Width = 177
      end>
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnClick = lvwItemsClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 232
    Width = 681
    Height = 57
    TabOrder = 2
    object Label4: TLabel
      Left = 8
      Top = 16
      Width = 27
      Height = 13
      Caption = 'Feed:'
    end
    object Label5: TLabel
      Left = 8
      Top = 32
      Width = 91
      Height = 13
      Caption = 'Link to Article Text:'
    end
    object lblFeed: TLabel
      Left = 48
      Top = 16
      Width = 3
      Height = 13
    end
    object lblLink: TLabel
      Left = 112
      Top = 32
      Width = 3
      Height = 13
      Cursor = crHandPoint
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = lblLinkClick
    end
  end
  object txtData: TMemo
    Left = 8
    Top = 296
    Width = 681
    Height = 181
    TabOrder = 3
  end
  object bRefresh: TButton
    Left = 632
    Top = 28
    Width = 57
    Height = 25
    Caption = 'Get Feed'
    TabOrder = 4
    OnClick = bRefreshClick
  end
  object RSS1: TipwRSS
    ChannelDocs = 'http://backend.userland.com/rss2'
    ChannelGenerator = 'IPWorks Version 8.0 RSS Component'
    FirewallPort = 80
    SSLCertStore = 'MY'
    OnSSLServerAuthentication = RSS1SSLServerAuthentication
    Left = 600
    Top = 48
  end
end


