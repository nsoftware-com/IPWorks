object FormIpinfo: TFormIpinfo
  Left = 191
  Top = 114
  Caption = 'IPInfo Demo'
  ClientHeight = 356
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    473
    356)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 463
    Height = 26
    Caption = 
      'This sample demonstrates the use of the IPInfo component for ret' +
      'rieving information about network adapters of the system.'
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
    Top = 48
    Width = 90
    Height = 13
    Caption = 'Network Interfaces'
  end
  object Label3: TLabel
    Left = 8
    Top = 128
    Width = 92
    Height = 13
    Caption = 'Adapter Information'
  end
  object lbNIC: TListBox
    Left = 8
    Top = 64
    Width = 369
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbNICClick
  end
  object bRelease: TButton
    Left = 384
    Top = 64
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'DHCP Release'
    TabOrder = 1
    OnClick = bReleaseClick
  end
  object bRenew: TButton
    Left = 383
    Top = 96
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'DHCP Renew'
    TabOrder = 2
    OnClick = bRenewClick
  end
  object lvwInfo: TListView
    Left = 8
    Top = 144
    Width = 465
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Width = 200
      end
      item
        Width = 260
      end>
    ColumnClick = False
    TabOrder = 3
    ViewStyle = vsReport
  end
  object IPInfo1: TipwIPInfo
    Left = 288
    Top = 32
    RegHnd = {}
  end
end


