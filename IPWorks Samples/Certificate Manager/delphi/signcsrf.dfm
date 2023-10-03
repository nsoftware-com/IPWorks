object FormSigncsr: TFormSigncsr
  Left = 366
  Top = 121
  Caption = 'Sign CSR'
  ClientHeight = 500
  ClientWidth = 444
  Color = clBtnFace
  Constraints.MinHeight = 480
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
    444
    500)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 86
    Height = 13
    Caption = 'Paste CSR below:'
  end
  object Label3: TLabel
    Left = 8
    Top = 192
    Width = 78
    Height = 13
    Caption = 'Issuer Certificate'
  end
  object Label2: TLabel
    Left = 8
    Top = 240
    Width = 69
    Height = 13
    Caption = 'Serial Number '
  end
  object Label4: TLabel
    Left = 8
    Top = 296
    Width = 161
    Height = 13
    Caption = 'Signed CSR will be provided here:'
  end
  object tCSR: TMemo
    Left = 8
    Top = 32
    Width = 418
    Height = 153
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object tSignedCSR: TMemo
    Left = 8
    Top = 312
    Width = 418
    Height = 115
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object bSign: TButton
    Left = 169
    Top = 438
    Width = 81
    Height = 22
    Anchors = [akRight, akBottom]
    Caption = 'Sign'
    TabOrder = 2
    OnClick = bSignClick
  end
  object bOK: TButton
    Left = 343
    Top = 438
    Width = 78
    Height = 22
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    TabOrder = 3
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 255
    Top = 438
    Width = 81
    Height = 22
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 4
    OnClick = bCancelClick
  end
  object cbIssuer: TComboBox
    Left = 8
    Top = 208
    Width = 320
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 12
    TabOrder = 5
  end
  object tSerialNumber: TEdit
    Left = 8
    Top = 256
    Width = 113
    Height = 21
    TabOrder = 6
    Text = '0'
    OnChange = tSerialNumberChange
  end
  object ipwCertMgr1: TipwCertMgr
    CertStore = 'MY'
    OnCertList = ipwCertMgr1CertList
    Left = 256
    Top = 248
  end
end
