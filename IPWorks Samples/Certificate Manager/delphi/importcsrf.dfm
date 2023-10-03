object FormImportcsr: TFormImportcsr
  Left = 349
  Top = 130
  Caption = 'Import Signed CSR'
  ClientHeight = 254
  ClientWidth = 434
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 450
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  DesignSize = (
    434
    254)
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 8
    Top = 56
    Width = 191
    Height = 13
    Caption = 'Please paste signed CSR contents here:'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 49
    Height = 13
    Caption = 'Key Name'
  end
  object tCSR: TMemo
    Left = 8
    Top = 72
    Width = 425
    Height = 147
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object bOK: TButton
    Left = 357
    Top = 228
    Width = 78
    Height = 22
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    TabOrder = 1
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 269
    Top = 228
    Width = 81
    Height = 22
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = bCancelClick
  end
  object cbKey: TComboBox
    Left = 8
    Top = 24
    Width = 361
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 12
    TabOrder = 3
  end
  object ipwCertMgr1: TipwCertMgr
    CertStore = 'MY'
    OnKeyList = ipwCertMgr1KeyList
    Left = 392
    Top = 24
  end
end
