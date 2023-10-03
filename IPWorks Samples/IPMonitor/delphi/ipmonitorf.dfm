object FormIpmonitor: TFormIpmonitor
  Left = 212
  Top = 117
  Caption = 'IPMonitor Demo'
  ClientHeight = 372
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    600
    372)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 508
    Height = 39
    Caption = 
      'This demo shows the use of IPMonitor to "sniff" network traffic ' +
      'on a network interface.  Set the Localhost property to the IP of' +
      ' the NIC you want to listen to, and set active = true to start l' +
      'istening to packets sent and received through that interface.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object bStart: TButton
    Left = 528
    Top = 8
    Width = 73
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = bStartClick
  end
  object txtPacketData: TMemo
    Left = 8
    Top = 256
    Width = 289
    Height = 113
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'txtPacketData')
    ScrollBars = ssHorizontal
    TabOrder = 1
    WantTabs = True
  end
  object txtPayload: TMemo
    Left = 304
    Top = 255
    Width = 297
    Height = 113
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object lvwPackets: TListView
    Left = 8
    Top = 56
    Width = 593
    Height = 193
    Anchors = [akLeft, akTop, akRight]
    Columns = <
      item
        AutoSize = True
        Caption = 'Packet ID'
      end
      item
        AutoSize = True
        Caption = 'Protocol'
      end
      item
        AutoSize = True
        Caption = 'Source Address'
      end
      item
        AutoSize = True
        Caption = 'Source Port'
      end
      item
        AutoSize = True
        Caption = 'Dest Address'
      end
      item
        AutoSize = True
        Caption = 'Dest Port'
      end
      item
        AutoSize = True
        Caption = 'IP Version'
      end>
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
    OnClick = lvwPacketsClick
  end
  object IPMonitor1: TipwIPMonitor
    OnIPPacket = IPMonitor1IPPacket
    Left = 496
    Top = 8
  end
end


