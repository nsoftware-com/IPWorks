object FormAtom: TFormAtom
  Left = 192
  Top = 109
  Caption = 'Atom Reader'
  ClientHeight = 470
  ClientWidth = 696
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    696
    470)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 558
    Height = 13
    Caption = 
      'This application demonstrates the use of the Atom component.  En' +
      'ter the URL for an Atom feed to browse its contents.'
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
    Top = 32
    Width = 68
    Height = 13
    Caption = 'Feed Address:'
  end
  object lblChannelDescription: TLabel
    Left = 8
    Top = 64
    Width = 66
    Height = 13
    Caption = '[Feed Title]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object txtURL: TEdit
    Left = 80
    Top = 24
    Width = 545
    Height = 21
    TabOrder = 0
    Text = 'https://news.google.com/news?ned=us&topic=h&output=atom'
  end
  object lvwItems: TListView
    Left = 8
    Top = 80
    Width = 681
    Height = 145
    Anchors = [akLeft, akTop, akRight]
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
    Anchors = [akLeft, akTop, akRight]
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
      Width = 23
      Height = 13
      Caption = 'Title:'
    end
    object lblFeed: TLabel
      Left = 40
      Top = 16
      Width = 3
      Height = 13
      Cursor = crHandPoint
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      OnClick = lblFeedClick
    end
    object lblTitle: TLabel
      Left = 40
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
      OnClick = lblTitleClick
    end
  end
  object txtData: TMemo
    Left = 8
    Top = 296
    Width = 681
    Height = 169
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
  object bRefresh: TButton
    Left = 632
    Top = 24
    Width = 57
    Height = 25
    Caption = 'Get Feed'
    TabOrder = 4
    OnClick = bRefreshClick
  end
  object ipwAtom1: TipwAtom
    SSLCertStore = 'MY'
    OnSSLServerAuthentication = ipwAtom1SSLServerAuthentication
    Left = 592
    Top = 48
  end
end


