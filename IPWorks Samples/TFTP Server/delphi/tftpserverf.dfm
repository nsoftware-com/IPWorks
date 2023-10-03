object FormTFTPServer: TFormTFTPServer
  Left = 0
  Top = 0
  Caption = 'TFTP Server Demo'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 24
    Top = 8
    Width = 529
    Height = 30
    Caption = 
      'This demo shows how to use the TFTP Server component to create a' +
      ' simple TFTP Server.  Specify the listening port and local direc' +
      'tory, then click START to start the server.'
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 56
    Width = 608
    Height = 97
    Caption = 'Server Configuration'
    TabOrder = 0
    object Label2: TLabel
      Left = 16
      Top = 24
      Width = 28
      Height = 15
      Caption = 'Port: '
    end
    object Label3: TLabel
      Left = 16
      Top = 61
      Width = 85
      Height = 15
      Caption = 'Local Directory: '
    end
    object EditLocalPort: TEdit
      Left = 136
      Top = 16
      Width = 457
      Height = 23
      TabOrder = 0
      Text = '69'
    end
    object EditLocalDir: TEdit
      Left = 136
      Top = 53
      Width = 457
      Height = 23
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 208
    Width = 608
    Height = 225
    Caption = 'Event Log'
    TabOrder = 1
    object EventLog: TListBox
      Left = 16
      Top = 24
      Width = 577
      Height = 185
      ItemHeight = 15
      TabOrder = 0
    end
  end
  object Start: TButton
    Left = 8
    Top = 167
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = StartClick
  end
  object Stop: TButton
    Left = 120
    Top = 167
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 3
    OnClick = StopClick
  end
  object ipwTFTPServer1: TipwTFTPServer
    OnConnected = ipwTFTPServer1Connected
    OnConnectionRequest = ipwTFTPServer1ConnectionRequest
    OnDisconnected = ipwTFTPServer1Disconnected
    OnEndTransfer = ipwTFTPServer1EndTransfer
    OnError = ipwTFTPServer1Error
    OnStartTransfer = ipwTFTPServer1StartTransfer
    OnTransfer = ipwTFTPServer1Transfer
    Left = 536
    Top = 168
  end
end


