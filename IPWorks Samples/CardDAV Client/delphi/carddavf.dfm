object FormCarddav: TFormCarddav
  Left = 710
  Top = 327
  Caption = 'CardDAV Demo'
  ClientHeight = 445
  ClientWidth = 647
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  TextHeight = 13
  object demoInfoLabel: TLabel
    Left = 16
    Top = 16
    Width = 614
    Height = 25
    AutoSize = False
    Caption = 
      'This demo shows how to use the CardDAV component to list contact' +
      's from an existing Google or NextCloud addressbook. You can also' +
      ' create a new contact, delete a contact, or export a contact to ' +
      'a vcf file.'
    WordWrap = True
  end
  object setupInfoBox: TGroupBox
    Left = 16
    Top = 47
    Width = 614
    Height = 114
    Caption = 'Setup Information'
    TabOrder = 0
    object usernameLabel: TLabel
      Left = 24
      Top = 24
      Width = 52
      Height = 13
      Caption = 'Username:'
    end
    object urlLabel: TLabel
      Left = 24
      Top = 83
      Width = 23
      Height = 13
      Caption = 'URL:'
    end
    object secretLabel: TLabel
      Left = 320
      Top = 54
      Width = 102
      Height = 13
      Caption = 'OAuth Client Secret: '
    end
    object idLabel: TLabel
      Left = 320
      Top = 24
      Width = 82
      Height = 13
      ParentCustomHint = False
      Caption = 'OAuth Client ID: '
    end
    object passwordLabel: TLabel
      Left = 23
      Top = 51
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object txtUsername: TEdit
      Left = 96
      Top = 21
      Width = 137
      Height = 21
      TabOrder = 0
    end
    object txtURL: TEdit
      Left = 96
      Top = 80
      Width = 481
      Height = 21
      TabOrder = 4
    end
    object txtSecret: TEdit
      Left = 432
      Top = 49
      Width = 145
      Height = 21
      TabOrder = 3
    end
    object txtID: TEdit
      Left = 432
      Top = 22
      Width = 145
      Height = 21
      TabOrder = 2
    end
    object txtPassword: TEdit
      Left = 96
      Top = 48
      Width = 137
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
  end
  object btnReport: TButton
    Left = 16
    Top = 177
    Width = 75
    Height = 25
    Caption = '&Get Contacts'
    TabOrder = 1
    OnClick = btnReportClick
  end
  object btnAddContact: TButton
    Left = 97
    Top = 177
    Width = 75
    Height = 25
    Caption = '&Add Contact'
    TabOrder = 2
    OnClick = btnAddContactClick
  end
  object btnDeleteContact: TButton
    Left = 178
    Top = 177
    Width = 75
    Height = 25
    Caption = '&Delete Contact'
    TabOrder = 3
    OnClick = btnDeleteContactClick
  end
  object btnExportVCF: TButton
    Left = 259
    Top = 177
    Width = 75
    Height = 25
    Caption = '&Export VCF'
    TabOrder = 4
    OnClick = btnExportVCFClick
  end
  object contactDetailsBox: TGroupBox
    Left = 16
    Top = 216
    Width = 614
    Height = 214
    Caption = 'Contact Details'
    TabOrder = 5
    object lvwContactDetails: TListView
      Left = 3
      Top = 16
      Width = 608
      Height = 195
      Columns = <
        item
          Caption = 'Formatted Name'
          Width = 150
        end
        item
          Caption = 'Phone Number'
          Width = 120
        end
        item
          Caption = 'Email'
          Width = 120
        end
        item
          Caption = 'ResourceURI'
          Width = 210
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object saveDialog: TSaveDialog
    Left = 480
    Top = 176
  end
  object ipwOAuth1: TipwOAuth
    SSLAcceptServerCertStore = 'MY'
    SSLCertStore = 'MY'
    WebServerSSLCertStore = 'MY'
    Left = 560
    Top = 176
  end
  object ipwCardDAV1: TipwCardDAV
    SSLAcceptServerCertStore = 'MY'
    SSLCertStore = 'MY'
    OnContactDetails = ipwCardDAV1ContactDetails
    OnSSLServerAuthentication = ipwCardDAV1SSLServerAuthentication
    Left = 392
    Top = 176
  end
end


