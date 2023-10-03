object FormGeneratecsr: TFormGeneratecsr
  Left = 170
  Top = 268
  Caption = 'Generate CSR'
  ClientHeight = 321
  ClientWidth = 434
  Color = clBtnFace
  Constraints.MinHeight = 360
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
    321)
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 8
    Top = 120
    Width = 169
    Height = 13
    Caption = 'CSR contents will be provided here:'
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 84
    Height = 13
    Caption = 'Certficate Subject'
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 49
    Height = 13
    Caption = 'Key Name'
  end
  object tCSR: TMemo
    Left = 8
    Top = 136
    Width = 409
    Height = 114
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object bSign: TButton
    Left = 240
    Top = 259
    Width = 96
    Height = 22
    Anchors = [akRight, akBottom]
    Caption = 'Generate CSR'
    Default = True
    TabOrder = 1
    OnClick = bSignClick
  end
  object bOK: TButton
    Left = 341
    Top = 259
    Width = 78
    Height = 22
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Close'
    TabOrder = 2
    OnClick = bOKClick
  end
  object tSubject: TEdit
    Left = 8
    Top = 24
    Width = 409
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'CN='
  end
  object cbKey: TComboBox
    Left = 8
    Top = 80
    Width = 201
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 12
    TabOrder = 4
  end
  object ipwCertMgr1: TipwCertMgr
    CertStore = 'MY'
    OnKeyList = ipwCertMgr1KeyList
    Left = 256
    Top = 72
  end
end
