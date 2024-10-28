object FormImap: TFormImap
  Left = 281
  Top = 159
  Caption = 'IMAP Mail'
  ClientHeight = 507
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    553
    507)
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 529
    Height = 26
    Caption = 
      'Use this demo to connect to an IMAP server and check email.  Cli' +
      'ck the '#39'Login'#39' button to download the folder list from the IMAP ' +
      'server.  Then select a mailbox from the Tree and the List Box wi' +
      'll be populated.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label4: TLabel
    Left = 244
    Top = 276
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 244
    Top = 260
    Width = 3
    Height = 13
  end
  object Label1: TLabel
    Left = 184
    Top = 260
    Width = 32
    Height = 13
    Caption = 'From:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 184
    Top = 276
    Width = 48
    Height = 13
    Caption = 'Subject:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ListViewMessages: TListView
    Left = 175
    Top = 88
    Width = 385
    Height = 169
    Anchors = [akLeft, akTop, akRight]
    Columns = <
      item
        Caption = '#'
        Width = 45
      end
      item
        Caption = 'From'
        Width = 100
      end
      item
        Caption = 'Subject'
        Width = 100
      end
      item
        Caption = 'Date'
        Width = 100
      end
      item
        Caption = 'Size'
        Width = 100
      end>
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnClick = ListViewMessagesClick
  end
  object ListBoxMessage: TListBox
    Left = 175
    Top = 296
    Width = 385
    Height = 216
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object TreeViewMailboxes: TTreeView
    Left = 8
    Top = 88
    Width = 157
    Height = 424
    Anchors = [akLeft, akTop, akBottom]
    Indent = 19
    ReadOnly = True
    TabOrder = 2
    OnClick = TreeViewMailboxesClick
  end
  object ButtonCompose: TButton
    Left = 88
    Top = 52
    Width = 75
    Height = 23
    Caption = 'Compose'
    Enabled = False
    TabOrder = 3
    OnClick = ButtonComposeClick
  end
  object ButtonLogin: TButton
    Left = 8
    Top = 52
    Width = 75
    Height = 23
    Caption = 'Login'
    TabOrder = 4
    OnClick = ButtonLoginClick
  end
  object ipwIMAP1: TipwIMAP
    Mailbox = 'Inbox'
    SSLCertStore = 'MY'
    OnMailboxList = ipwIMAP1MailboxList
    OnMessageInfo = ipwIMAP1MessageInfo
    OnMessagePart = ipwIMAP1MessagePart
    OnSSLServerAuthentication = ipwIMAP1SSLServerAuthentication
    OnTransfer = ipwIMAP1Transfer
    Left = 492
    Top = 48
  end
  object ipwSMTP1: TipwSMTP
    SSLCertStore = 'MY'
    OnSSLServerAuthentication = ipwSMTP1SSLServerAuthentication
    Left = 432
    Top = 40
  end
  object ipwMIME1: TipwMIME
    Left = 372
    Top = 48
  end
end


