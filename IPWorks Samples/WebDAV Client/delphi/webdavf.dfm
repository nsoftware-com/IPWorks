object FormWebdav: TFormWebdav
  Left = 67
  Top = 124
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'WebDAV - Remote File Manager'
  ClientHeight = 474
  ClientWidth = 778
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 24
    Width = 335
    Height = 39
    Caption = 
      'This demo shows how to use the WebDAV component to manage remote' +
      ' files on a web server.  You copy files back and forth by clicki' +
      'ng on the buttons.'
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
    Top = 88
    Width = 50
    Height = 13
    Caption = 'Local Files'
  end
  object Label3: TLabel
    Left = 373
    Top = 88
    Width = 61
    Height = 13
    Caption = 'Remote Files'
  end
  object LabelPassword: TLabel
    Left = 373
    Top = 51
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object LabelUsername: TLabel
    Left = 373
    Top = 24
    Width = 54
    Height = 13
    Caption = 'Username: '
  end
  object tbURL: TEdit
    Left = 440
    Top = 85
    Width = 273
    Height = 21
    TabOrder = 0
    Text = 'http://localhost/DAVTest/'
  end
  object lvwLocal: TListView
    Left = 8
    Top = 113
    Width = 321
    Height = 353
    Columns = <
      item
        Caption = 'File Name'
        Width = 200
      end
      item
        Caption = 'Size'
        Width = 117
      end>
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = lvwLocalDblClick
  end
  object lvwRemote: TListView
    Left = 369
    Top = 112
    Width = 401
    Height = 354
    Columns = <
      item
        Caption = 'Name'
        Width = 200
      end
      item
        Caption = 'Size'
        Width = 109
      end>
    TabOrder = 2
    ViewStyle = vsReport
  end
  object bDownload: TButton
    Left = 336
    Top = 176
    Width = 25
    Height = 33
    Caption = '<<'
    TabOrder = 3
    OnClick = bDownloadClick
  end
  object bUpload: TButton
    Left = 336
    Top = 215
    Width = 25
    Height = 33
    Caption = '>>'
    TabOrder = 4
    OnClick = bUploadClick
  end
  object bGo: TButton
    Left = 719
    Top = 87
    Width = 49
    Height = 17
    Caption = 'Go'
    TabOrder = 5
    OnClick = bGoClick
  end
  object tbUsername: TEdit
    Left = 433
    Top = 21
    Width = 168
    Height = 21
    TabOrder = 6
  end
  object tbPassword: TEdit
    Left = 433
    Top = 48
    Width = 168
    Height = 21
    PasswordChar = '*'
    TabOrder = 7
  end
  object Webdav1: TipwWebDAV
    LockType = 'write'
    SSLCertStore = 'MY'
    OnDirList = Webdav1DirList
    OnSSLServerAuthentication = WebDAV1SSLServerAuthentication
    Left = 536
    Top = 328
  end
end


