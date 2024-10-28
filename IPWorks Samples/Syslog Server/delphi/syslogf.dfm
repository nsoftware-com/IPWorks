object FormSyslog: TFormSyslog
  Left = 197
  Top = 190
  BorderStyle = bsSingle
  Caption = 'Syslog Demo'
  ClientHeight = 410
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 660
    Height = 26
    Caption = 
      'This application demonstrates the use of the Syslog component fo' +
      'r receiving and/or sending syslog datagrams.  To act as a syslog' +
      ' server, click on "Listen".  To send out a datagram, click on "S' +
      'end".'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object bListen: TButton
    Left = 512
    Top = 40
    Width = 73
    Height = 25
    Caption = 'Listen'
    TabOrder = 0
    OnClick = bListenClick
  end
  object bSend: TButton
    Left = 592
    Top = 40
    Width = 73
    Height = 25
    Caption = 'Send'
    TabOrder = 1
    OnClick = bSendClick
  end
  object lvwPackets: TListView
    Left = 8
    Top = 72
    Width = 657
    Height = 330
    Columns = <
      item
        Caption = 'Source Address'
        Width = 100
      end
      item
        Caption = 'Hostname'
        Width = 100
      end
      item
        Caption = 'Message'
        Width = 167
      end
      item
        Caption = 'Facility'
        Width = 100
      end
      item
        Caption = 'Severity'
        Width = 100
      end
      item
        Caption = 'Timestamp'
        Width = 100
      end>
    TabOrder = 2
    ViewStyle = vsReport
  end
  object SysLog1: TipwSysLog
    RemoteHost = '255.255.255.255'
    OnPacketIn = SysLog1PacketIn
    Left = 472
    Top = 40
  end
end


