object FormCaldav: TFormCaldav
  Left = 715
  Top = 332
  Caption = 'CalDAV Demo'
  ClientHeight = 438
  ClientWidth = 639
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 614
    Height = 25
    AutoSize = False
    Caption = 
      'This demo shows how to use the CalDAV component to list upcoming' +
      ' events from an existing Google or Yahoo calendar. You can also ' +
      'create a new event, delete an event, or export an event to an ic' +
      's file.'
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 47
    Width = 614
    Height = 114
    Caption = 'Setup Information'
    TabOrder = 0
    object Label2: TLabel
      Left = 24
      Top = 24
      Width = 52
      Height = 13
      Caption = 'Username:'
    end
    object Label4: TLabel
      Left = 24
      Top = 83
      Width = 23
      Height = 13
      Caption = 'URL:'
    end
    object Label6: TLabel
      Left = 320
      Top = 54
      Width = 102
      Height = 13
      Caption = 'OAuth Client Secret: '
    end
    object Label5: TLabel
      Left = 320
      Top = 24
      Width = 82
      Height = 13
      Caption = 'OAuth Client ID: '
    end
    object txtUsername: TEdit
      Left = 96
      Top = 21
      Width = 137
      Height = 21
      TabOrder = 0
      OnChange = txtUsernameChange
    end
    object txtURL: TEdit
      Left = 96
      Top = 80
      Width = 481
      Height = 21
      TabOrder = 3
    end
    object txtSecret: TEdit
      Left = 432
      Top = 49
      Width = 145
      Height = 21
      TabOrder = 2
      OnChange = txtSecretChange
      OnExit = txtSecretChange
    end
    object txtID: TEdit
      Left = 432
      Top = 22
      Width = 145
      Height = 21
      TabOrder = 1
      OnChange = txtIDChange
      OnExit = txtIDChange
    end
  end
  object btnReport: TButton
    Left = 16
    Top = 177
    Width = 75
    Height = 25
    Caption = '&Get Event'
    TabOrder = 1
    OnClick = btnReportClick
  end
  object btnAddEvent: TButton
    Left = 97
    Top = 177
    Width = 75
    Height = 25
    Caption = '&Add Event'
    TabOrder = 2
    OnClick = btnAddEventClick
  end
  object btnDeleteEvent: TButton
    Left = 178
    Top = 177
    Width = 75
    Height = 25
    Caption = '&Delete Event'
    TabOrder = 3
    OnClick = btnDeleteEventClick
  end
  object btnExportICS: TButton
    Left = 259
    Top = 177
    Width = 75
    Height = 25
    Caption = '&Export ICS'
    TabOrder = 4
    OnClick = btnExportICSClick
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 216
    Width = 614
    Height = 214
    Caption = 'Event Details'
    TabOrder = 5
    object lvwEventDetails: TListView
      Left = 3
      Top = 16
      Width = 608
      Height = 195
      Columns = <
        item
          Caption = 'Summary'
          Width = 150
        end
        item
          Caption = 'Start Date'
          Width = 120
        end
        item
          Caption = 'Location'
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
  object SaveDialog1: TSaveDialog
    Left = 480
    Top = 176
  end
  object ipwCalDAV1: TipwCalDAV
    LockType = 'write'
    SSLCertStore = 'MY'
    OnEventDetails = ipwCalDAV1EventDetails
    OnSSLServerAuthentication = ipwCalDAV1SSLServerAuthentication
    Left = 368
    Top = 176
  end
  object ipwOAuth1: TipwOAuth
    SSLCertStore = 'MY'
    WebServerSSLCertStore = 'MY'
    OnSSLServerAuthentication = ipwCalDAV1SSLServerAuthentication
    Left = 560
    Top = 176
  end
end


