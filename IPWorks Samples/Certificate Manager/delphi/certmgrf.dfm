object FormCertmgr: TFormCertmgr
  Left = 350
  Top = 236
  Caption = 'IPWorks Certificate Manager - www.nsoftware.com'
  ClientHeight = 440
  ClientWidth = 782
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  DesignSize = (
    782
    440)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 749
    Height = 265
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Available Certificates and Keys'
    TabOrder = 0
    DesignSize = (
      749
      265)
    object pgCertsAndKeys: TPageControl
      Left = 9
      Top = 24
      Width = 730
      Height = 233
      ActivePage = tabUserStores
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      OnChange = pgCertsAndKeysChange
      object tabUserStores: TTabSheet
        Caption = 'User Certificate Stores'
        DesignSize = (
          722
          205)
        object Label1: TLabel
          Left = 8
          Top = 12
          Width = 129
          Height = 13
          Caption = 'Available Certificate Stores:'
        end
        object Label2: TLabel
          Left = 232
          Top = 12
          Width = 83
          Height = 13
          Caption = 'Store Certificates:'
        end
        object tvUserStores: TTreeView
          Left = 4
          Top = 32
          Width = 222
          Height = 165
          Anchors = [akLeft, akTop, akBottom]
          Indent = 19
          TabOrder = 0
          OnChange = tvUserStoresChange
        end
        object lvUserCerts: TListView
          Left = 232
          Top = 32
          Width = 484
          Height = 165
          Anchors = [akLeft, akTop, akRight]
          Columns = <
            item
              AutoSize = True
              Caption = 'Certificate Subject'
            end
            item
              Caption = 'CertStore'
              Width = 0
            end
            item
              Caption = 'CertEncoded'
              Width = 0
            end>
          ColumnClick = False
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          TabOrder = 1
          ViewStyle = vsReport
          OnClick = lvUserCertsClick
        end
      end
      object tabMachineStores: TTabSheet
        Caption = 'Machine Certificate Stores'
        ImageIndex = 1
        DesignSize = (
          722
          205)
        object Label4: TLabel
          Left = 8
          Top = 12
          Width = 129
          Height = 13
          Caption = 'Available Certificate Stores:'
        end
        object Label3: TLabel
          Left = 232
          Top = 12
          Width = 83
          Height = 13
          Caption = 'Store Certificates:'
        end
        object tvMachineStores: TTreeView
          Left = 4
          Top = 32
          Width = 222
          Height = 165
          Anchors = [akLeft, akTop, akBottom]
          Indent = 19
          TabOrder = 0
          OnChange = tvMachineStoresChange
        end
        object lvMachineCerts: TListView
          Left = 232
          Top = 32
          Width = 484
          Height = 165
          Anchors = [akLeft, akTop, akRight]
          Columns = <
            item
              AutoSize = True
              Caption = 'Certificate Subject'
            end
            item
              Caption = 'CertStore'
              Width = 0
            end
            item
              Caption = 'CertEncoded'
              Width = 0
            end>
          ColumnClick = False
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          TabOrder = 1
          ViewStyle = vsReport
          OnClick = lvMachineCertsClick
        end
      end
      object tabPFXStores: TTabSheet
        Caption = 'Certificate Files (PFX)'
        ImageIndex = 2
        object Label5: TLabel
          Left = 8
          Top = 16
          Width = 42
          Height = 13
          Caption = 'PFX File:'
        end
        object lPFXPassword: TLabel
          Left = 520
          Top = 16
          Width = 137
          Height = 13
          AutoSize = False
          Visible = False
        end
        object lvPFXCerts: TListView
          Left = 4
          Top = 40
          Width = 701
          Height = 161
          Columns = <
            item
              AutoSize = True
              Caption = 'Certificate Subject'
            end
            item
              Caption = 'CertStore'
              Width = 0
            end
            item
              Caption = 'CertEncoded'
              Width = 0
            end>
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          TabOrder = 0
          ViewStyle = vsReport
          OnClick = lvPFXCertsClick
        end
        object tPFXFile: TEdit
          Left = 64
          Top = 12
          Width = 270
          Height = 21
          TabOrder = 1
        end
        object btnBrowsePFX: TButton
          Left = 424
          Top = 11
          Width = 73
          Height = 24
          Caption = 'Browse...'
          TabOrder = 2
          OnClick = btnBrowsePFXClick
        end
        object btnLoadPFX: TButton
          Left = 344
          Top = 11
          Width = 73
          Height = 24
          Caption = 'Load'
          Default = True
          TabOrder = 3
          OnClick = btnLoadPFXClick
        end
      end
      object tabKeys: TTabSheet
        Caption = 'Available Keys'
        ImageIndex = 3
        DesignSize = (
          722
          205)
        object lvKeys: TListView
          Left = 4
          Top = 4
          Width = 715
          Height = 197
          Anchors = [akLeft, akTop, akRight]
          Columns = <
            item
              AutoSize = True
              Caption = 'Key Container'
            end
            item
              AutoSize = True
              Caption = 'Key Type'
            end
            item
              AutoSize = True
              Caption = 'Key Length'
            end
            item
              AutoSize = True
              Caption = 'Algorithm'
            end>
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 280
    Width = 749
    Height = 100
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Selected Certificate Info'
    TabOrder = 1
    DesignSize = (
      749
      100)
    object lvCertInfo: TListView
      Left = 8
      Top = 24
      Width = 730
      Height = 65
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Field'
          Width = 150
        end
        item
          AutoSize = True
          Caption = 'Value'
        end>
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object spStatus: TStatusBar
    Left = 0
    Top = 418
    Width = 782
    Height = 22
    Panels = <>
    SimplePanel = True
  end
  object MainMenu1: TMainMenu
    Left = 528
    object File1: TMenuItem
      Caption = '&File'
      object DeleteCertificate1: TMenuItem
        Caption = '&Delete Certificate'
        OnClick = DeleteCertificate1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ImportCertificate1: TMenuItem
        Caption = '&Import Certificate...'
        OnClick = ImportCertificate1Click
      end
      object ExportCertificate1: TMenuItem
        Caption = '&Export Certificate...'
        OnClick = ExportCertificate1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Refresh1: TMenuItem
        Caption = '&Refresh'
        OnClick = Refresh1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = '&Exit'
        OnClick = Exit1Click
      end
    end
    object Certificates1: TMenuItem
      Caption = '&Certificates'
      object CreateSignCertificate1: TMenuItem
        Caption = 'Create (&Sign) Certificate...'
        OnClick = CreateSignCertificate1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object CreateKey1: TMenuItem
        Caption = 'Create &Key...'
        OnClick = CreateKey1Click
      end
      object GenerateCSR1: TMenuItem
        Caption = '&Generate CSR...'
        OnClick = GenerateCSR1Click
      end
      object ImportSignedCSR1: TMenuItem
        Caption = '&Import Signed CSR...'
        OnClick = ImportSignedCSR1Click
      end
    end
    object CAFunctions1: TMenuItem
      Caption = 'C&A Functions'
      object SignCSR1: TMenuItem
        Caption = '&Sign Certificate Request...'
        OnClick = SignCSR1Click
      end
    end
    object A1: TMenuItem
      Caption = 'About...'
      OnClick = A1Click
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'pfx'
    FileName = '*.pfx'
    Filter = '*.pfx'
    Left = 488
  end
  object ipwCertMgr1: TipwCertMgr
    CertStore = 'MY'
    OnCertList = ipwCertMgr1CertList
    OnKeyList = ipwCertMgr1KeyList
    OnStoreList = ipwCertMgr1StoreList
    Left = 613
    Top = 16
  end
end


