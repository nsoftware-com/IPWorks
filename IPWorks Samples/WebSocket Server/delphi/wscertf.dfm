object FormWscert: TFormWscert
  Left = 0
  Top = 0
  Caption = 'Certificate Browser'
  ClientHeight = 366
  ClientWidth = 506
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 2
    Top = 206
    Width = 77
    Height = 13
    Caption = 'Certificate Info:'
  end
  object Label2: TLabel
    Left = 2
    Top = 54
    Width = 105
    Height = 13
    Caption = 'Available Certificates:'
  end
  object Label1: TLabel
    Left = 8
    Top = 14
    Width = 83
    Height = 13
    Caption = 'Certificate Store:'
  end
  object txtInfo: TMemo
    Left = 10
    Top = 222
    Width = 495
    Height = 143
    Lines.Strings = (
      'txtInfo')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object lCerts: TListBox
    Left = 8
    Top = 72
    Width = 497
    Height = 131
    ItemHeight = 13
    TabOrder = 1
    OnClick = lCertsClick
  end
  object bSelectCert: TButton
    Left = 394
    Top = 4
    Width = 99
    Height = 29
    Caption = 'Select'
    TabOrder = 2
    OnClick = bSelectCertClick
  end
  object cbStores: TListBox
    Left = 88
    Top = 12
    Width = 295
    Height = 19
    ItemHeight = 13
    TabOrder = 3
    OnClick = cbStoresClick
  end
  object ipwCertMgr1: TipwCertMgr
    CertStore = 'MY'
    ExportFormat = 'PFX'
    OnCertList = ipwCertMgr1CertList
    OnStoreList = ipwCertMgr1StoreList
    Left = 304
    Top = 280
  end
end
