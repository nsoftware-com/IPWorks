object FormProxyfirewall: TFormProxyfirewall
  Left = 192
  Top = 109
  BorderStyle = bsSingle
  Caption = 'Proxy/Firewall Demo'
  ClientHeight = 583
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    690
    583)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 681
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'This demo shows how to use the TCP/IP components with HTTP Tunne' +
      'ling or SOCKS proxies. This demo uses the HTTP component, but th' +
      'e same properties are used in all TCP/IP components in the toolk' +
      'it.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 671
  end
  object Label7: TLabel
    Left = 8
    Top = 216
    Width = 25
    Height = 13
    Caption = 'URL:'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 48
    Width = 673
    Height = 161
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Proxy/Firewall Connection Details'
    TabOrder = 0
    DesignSize = (
      673
      161)
    object rbAuto: TRadioButton
      Left = 24
      Top = 24
      Width = 193
      Height = 17
      Caption = 'Auto-Detect'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object gbManual: TGroupBox
      Left = 24
      Top = 48
      Width = 297
      Height = 97
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      DesignSize = (
        297
        97)
      object Label2: TLabel
        Left = 16
        Top = 24
        Width = 27
        Height = 13
        Caption = 'Type:'
      end
      object Label3: TLabel
        Left = 16
        Top = 56
        Width = 25
        Height = 13
        Caption = 'Host:'
      end
      object Label4: TLabel
        Left = 176
        Top = 56
        Width = 22
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Port:'
      end
      object cbProxyType: TComboBox
        Left = 56
        Top = 24
        Width = 209
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 0
        Text = 'None'
        OnSelect = cbProxyTypeSelect
        Items.Strings = (
          'None'
          'HTTP Tunneling'
          'SOCKS 4'
          'SOCKS 5')
      end
      object tHost: TEdit
        Left = 56
        Top = 56
        Width = 113
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object tPort: TEdit
        Left = 208
        Top = 56
        Width = 57
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 2
      end
    end
    object rbManual: TRadioButton
      Left = 24
      Top = 48
      Width = 57
      Height = 17
      Caption = 'Manual'
      TabOrder = 2
    end
    object gbAuthenticate: TGroupBox
      Left = 336
      Top = 24
      Width = 321
      Height = 121
      Anchors = [akTop, akRight]
      TabOrder = 3
      object Label5: TLabel
        Left = 24
        Top = 32
        Width = 25
        Height = 13
        Caption = 'User:'
      end
      object Label6: TLabel
        Left = 24
        Top = 64
        Width = 26
        Height = 13
        Caption = 'Pass:'
      end
      object tUser: TEdit
        Left = 64
        Top = 32
        Width = 217
        Height = 21
        TabOrder = 0
      end
      object tPass: TEdit
        Left = 64
        Top = 64
        Width = 217
        Height = 21
        TabOrder = 1
      end
    end
    object cbAuthenticate: TCheckBox
      Left = 336
      Top = 24
      Width = 81
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Authenticate'
      TabOrder = 4
    end
  end
  object tURL: TEdit
    Left = 40
    Top = 216
    Width = 569
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'http://www.nsoftware.com/default.aspx'
  end
  object bGet: TButton
    Left = 616
    Top = 216
    Width = 65
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Get'
    TabOrder = 2
    OnClick = bGetClick
  end
  object tResults: TMemo
    Left = 8
    Top = 248
    Width = 673
    Height = 296
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 8
    Top = 550
    Width = 673
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
    object Shape1: TShape
      Left = 0
      Top = 0
      Width = 1
      Height = 25
      Brush.Color = clBlue
    end
  end
  object HTTP1: TipwHTTP
    SSLCertStore = 'MY'
    TransferredDataLimit = 65536
    OnSSLServerAuthentication = HTTP1SSLServerAuthentication
    OnTransfer = HTTP1Transfer
    Left = 24
    Top = 88
  end
end


