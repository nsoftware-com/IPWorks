object FormFtp: TFormFtp
  Left = 201
  Top = 149
  ActiveControl = ComboBoxLocHistory
  BorderStyle = bsSingle
  Caption = 'IPWorks FTP Demo'
  ClientHeight = 404
  ClientWidth = 594
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    594
    404)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 4
    Top = 8
    Width = 265
    Height = 241
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Local System '
    TabOrder = 0
    DesignSize = (
      265
      241)
    object ComboBoxLocHistory: TComboBox
      Left = 8
      Top = 16
      Width = 251
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = ComboBoxLocHistoryChange
    end
    object ListBoxLocFiles: TListBox
      Left = 8
      Top = 40
      Width = 200
      Height = 193
      Anchors = [akLeft, akTop, akBottom]
      ExtendedSelect = False
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
      OnDblClick = ListBoxLocFilesDblClick
    end
    object ButtonLocChgDir: TButton
      Left = 208
      Top = 40
      Width = 50
      Height = 25
      Caption = 'ChgDir'
      TabOrder = 2
      OnClick = ButtonLocChgDirClick
    end
    object ButtonLocMkDir: TButton
      Left = 208
      Top = 64
      Width = 50
      Height = 25
      Caption = 'MkDir'
      TabOrder = 3
      OnClick = ButtonLocMkDirClick
    end
    object ButtonLocRename: TButton
      Left = 208
      Top = 88
      Width = 50
      Height = 25
      Caption = 'Rename'
      TabOrder = 4
      OnClick = ButtonLocRenameClick
    end
    object ButtonLocDelete: TButton
      Left = 208
      Top = 112
      Width = 50
      Height = 25
      Caption = 'Delete'
      TabOrder = 5
      OnClick = ButtonLocDeleteClick
    end
    object ButtonLocRefresh: TButton
      Left = 208
      Top = 136
      Width = 50
      Height = 25
      Caption = 'Refresh'
      TabOrder = 6
      OnClick = ButtonLocRefreshClick
    end
  end
  object ButtonDownload: TButton
    Left = 276
    Top = 68
    Width = 25
    Height = 25
    Caption = '<--'
    TabOrder = 1
    OnClick = ButtonDownloadClick
  end
  object ButtonUpload: TButton
    Left = 276
    Top = 100
    Width = 25
    Height = 25
    Caption = '-->'
    TabOrder = 2
    OnClick = ButtonUploadClick
  end
  object ListBoxPiTrail: TListBox
    Left = 4
    Top = 296
    Width = 594
    Height = 77
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
  object ButtonConnectDisconnect: TButton
    Left = 4
    Top = 380
    Width = 75
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = 'Connect'
    TabOrder = 4
    OnClick = ButtonConnectDisconnectClick
  end
  object ButtonCancel: TButton
    Left = 88
    Top = 380
    Width = 75
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = ButtonCancelClick
  end
  object ButtonExit: TButton
    Left = 172
    Top = 380
    Width = 75
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = 'Exit'
    TabOrder = 6
    OnClick = ButtonExitClick
  end
  object GroupBox2: TGroupBox
    Left = 308
    Top = 8
    Width = 290
    Height = 241
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Remote Host '
    TabOrder = 7
    DesignSize = (
      290
      241)
    object ComboBoxRemHistory: TComboBox
      Left = 8
      Top = 16
      Width = 251
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = ComboBoxRemHistoryChange
    end
    object ListBoxRemFiles: TListBox
      Left = 8
      Top = 40
      Width = 225
      Height = 193
      Anchors = [akLeft, akTop, akRight, akBottom]
      ExtendedSelect = False
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
      OnDblClick = ListBoxRemFilesDblClick
    end
    object ButtonRemChgDir: TButton
      Left = 233
      Top = 40
      Width = 50
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'ChgDir'
      TabOrder = 2
      OnClick = ButtonRemChgDirClick
    end
    object ButtonRemMkDir: TButton
      Left = 233
      Top = 64
      Width = 50
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'MkDir'
      TabOrder = 3
      OnClick = ButtonRemMkDirClick
    end
    object ButtonRemRename: TButton
      Left = 233
      Top = 88
      Width = 50
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Rename'
      TabOrder = 4
      OnClick = ButtonRemRenameClick
    end
    object ButtonRemDelete: TButton
      Left = 233
      Top = 112
      Width = 50
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Delete'
      TabOrder = 5
      OnClick = ButtonRemDeleteClick
    end
    object ButtonRemRefresh: TButton
      Left = 233
      Top = 136
      Width = 50
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Refresh'
      TabOrder = 6
      OnClick = ButtonRemRefreshClick
    end
  end
  object RadioGroupTransferMode: TRadioGroup
    Left = 168
    Top = 252
    Width = 245
    Height = 37
    Anchors = [akLeft, akBottom]
    Caption = ' Transfer Mode '
    Columns = 3
    Items.Strings = (
      'ASCII'
      'Binary'
      'Auto')
    TabOrder = 8
    OnClick = RadioGroupTransferModeClick
  end
  object ipwFTP1: TipwFTP
    SSLCertStore = 'MY'
    OnDirList = ipwFTP1DirList
    OnEndTransfer = ipwFTP1EndTransfer
    OnPITrail = ipwFTP1PITrail
    OnSSLServerAuthentication = ipwFTP1SSLServerAuthentication
    OnStartTransfer = ipwFTP1StartTransfer
    OnTransfer = ipwFTP1Transfer
    Left = 276
    Top = 12
  end
end


