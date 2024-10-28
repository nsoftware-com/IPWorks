object FormPopclient: TFormPopclient
  Left = 231
  Top = 113
  Caption = 'MailBox'
  ClientHeight = 462
  ClientWidth = 510
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnResize = FormResize
  DesignSize = (
    510
    462)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 16
    Width = 393
    Height = 26
    Caption = 
      'This is a demo of building a simple email client applications us' +
      'ing  POP and SMTP.  Click the '#39'Login" button to connect to a POP' +
      ' Server.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 8
    Top = 264
    Width = 65
    Height = 13
    Caption = 'Attachments: '
  end
  object ListBoxMessage: TListBox
    Left = 8
    Top = 287
    Width = 494
    Height = 167
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 6
  end
  object ListViewMailbox: TListView
    Left = 8
    Top = 81
    Width = 494
    Height = 151
    Anchors = [akLeft, akTop, akRight]
    Columns = <
      item
        Caption = 'Number'
      end
      item
        Caption = 'From'
        Width = 150
      end
      item
        Caption = 'Subject'
        Width = 150
      end
      item
        Caption = 'Date'
        Width = 150
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 5
    ViewStyle = vsReport
    OnClick = ListViewMailboxClick
  end
  object ButtonLoginLogout: TButton
    Left = 8
    Top = 52
    Width = 75
    Height = 23
    Caption = 'Login'
    Default = True
    TabOrder = 0
    OnClick = ButtonLoginLogoutClick
  end
  object ButtonRetrieve: TButton
    Left = 104
    Top = 52
    Width = 75
    Height = 23
    Caption = 'Retrieve'
    Enabled = False
    TabOrder = 1
    OnClick = ButtonRetrieveClick
  end
  object ButtonCompose: TButton
    Left = 184
    Top = 52
    Width = 75
    Height = 23
    Caption = 'Compose'
    Enabled = False
    TabOrder = 2
    OnClick = ButtonComposeClick
  end
  object ButtonReply: TButton
    Left = 264
    Top = 52
    Width = 75
    Height = 23
    Caption = 'Reply'
    Enabled = False
    TabOrder = 3
    OnClick = ButtonReplyClick
  end
  object ButtonDelete: TButton
    Left = 344
    Top = 52
    Width = 75
    Height = 23
    Caption = 'Delete'
    Enabled = False
    TabOrder = 4
    OnClick = ButtonDeleteClick
  end
  object ButtonSave: TButton
    Left = 242
    Top = 262
    Width = 75
    Height = 23
    Caption = 'Save'
    TabOrder = 7
    OnClick = ButtonSaveClick
  end
  object ComboBoxAttachments: TComboBox
    Left = 84
    Top = 262
    Width = 145
    Height = 21
    TabOrder = 8
  end
  object ComboBoxFileNames: TComboBox
    Left = 328
    Top = 262
    Width = 174
    Height = 21
    TabOrder = 9
    Visible = False
  end
  object ipwMIME1: TipwMIME
    Left = 112
    Top = 120
  end
  object ipwPOP1: TipwPOP
    FirewallPort = 80
    SSLCertStore = 'MY'
    OnHeader = ipwPOP1Header
    OnSSLServerAuthentication = ipwPOP1SSLServerAuthentication
    Left = 176
    Top = 120
  end
  object ipwSMTP1: TipwSMTP
    FirewallPort = 80
    SSLCertStore = 'MY'
    OnEndTransfer = ipwSMTP1EndTransfer
    OnPITrail = ipwSMTP1PITrail
    OnSSLServerAuthentication = ipwSMTP1SSLServerAuthentication
    OnStartTransfer = ipwSMTP1StartTransfer
    Left = 252
    Top = 120
  end
  object SaveDialog1: TSaveDialog
    Left = 452
    Top = 24
  end
end


