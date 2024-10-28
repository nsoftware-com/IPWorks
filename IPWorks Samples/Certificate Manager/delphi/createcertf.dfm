object FormCreatecert: TFormCreatecert
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Create Certificate'
  ClientHeight = 293
  ClientWidth = 425
  Color = clBtnFace
  Constraints.MaxWidth = 431
  Constraints.MinHeight = 300
  Constraints.MinWidth = 431
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  DesignSize = (
    425
    293)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 84
    Height = 13
    Caption = 'Certficate Subject'
  end
  object Label2: TLabel
    Left = 16
    Top = 64
    Width = 69
    Height = 13
    Caption = 'Serial Number '
  end
  object tSubject: TEdit
    Left = 16
    Top = 32
    Width = 392
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'CN='
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 128
    Width = 391
    Height = 129
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Certificate Signing Options'
    TabOrder = 1
    DesignSize = (
      391
      129)
    object Label3: TLabel
      Left = 32
      Top = 72
      Width = 78
      Height = 13
      Caption = 'Issuer Certificate'
    end
    object cbIssuer: TComboBox
      Left = 32
      Top = 88
      Width = 335
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 12
      TabOrder = 0
    end
    object rbSelfSigned: TRadioButton
      Left = 16
      Top = 24
      Width = 233
      Height = 17
      Caption = 'Create Self-Signed Certificate'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object rbSigned: TRadioButton
      Left = 16
      Top = 48
      Width = 257
      Height = 17
      Caption = 'Sign Using The Following Certificate:'
      TabOrder = 2
    end
  end
  object bOK: TButton
    Left = 304
    Top = 264
    Width = 102
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Create Certificate'
    Default = True
    TabOrder = 2
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 216
    Top = 264
    Width = 81
    Height = 22
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = bCancelClick
  end
  object tSerialNumber: TEdit
    Left = 16
    Top = 80
    Width = 113
    Height = 21
    TabOrder = 4
    Text = '0'
  end
  object ipwCertMgr1: TipwCertMgr
    CertStore = 'MY'
    OnCertList = ipwCertMgr1CertList
    Left = 216
    Top = 72
  end
end
