object FormBulkmail: TFormBulkmail
  Left = 200
  Top = 130
  BorderStyle = bsSingle
  Caption = 'Bulk Mail Demo'
  ClientHeight = 487
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object lblHeading: TLabel
    Left = 10
    Top = 8
    Width = 492
    Height = 52
    Caption = 
      'This is a demo of a Bulk Emailer using the HTMLMailer object.  I' +
      'nstead of one email with a BCC or CC list, this demo sends the s' +
      'ame email to a large group of direct recipients.  You need to fi' +
      'll in the mail server that you would like to use to send the ema' +
      'il, the from field with your email address, and then all the rec' +
      'ipients of the email separated by commas.'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 10
    Top = 80
    Width = 36
    Height = 13
    Caption = 'Server:'
  end
  object Label2: TLabel
    Left = 10
    Top = 107
    Width = 28
    Height = 13
    Caption = 'From:'
  end
  object Label3: TLabel
    Left = 10
    Top = 136
    Width = 163
    Height = 13
    Caption = 'Recipient List: (comma separated)'
  end
  object Label4: TLabel
    Left = 10
    Top = 250
    Width = 40
    Height = 13
    Caption = 'Subject:'
  end
  object lblBody: TLabel
    Left = 10
    Top = 280
    Width = 65
    Height = 13
    Caption = 'Body (HTML):'
  end
  object txtMailServer: TEdit
    Left = 80
    Top = 77
    Width = 424
    Height = 21
    TabOrder = 0
  end
  object txtFrom: TEdit
    Left = 80
    Top = 104
    Width = 424
    Height = 21
    TabOrder = 1
  end
  object txtRecipients: TMemo
    Left = 80
    Top = 155
    Width = 424
    Height = 86
    TabOrder = 2
  end
  object txtSubject: TEdit
    Left = 80
    Top = 247
    Width = 424
    Height = 21
    TabOrder = 3
  end
  object txtBody: TMemo
    Left = 8
    Top = 299
    Width = 496
    Height = 142
    Lines.Strings = (
      '<b>Hello!</b><p>This is a test message!</p>')
    TabOrder = 4
  end
  object btnSend: TButton
    Left = 213
    Top = 450
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 5
    OnClick = btnSendClick
  end
  object ipwHTMLMailer1: TipwHTMLMailer
    SSLCertStore = 'MY'
    OnSSLServerAuthentication = ipwHTMLMailer1SSLServerAuthentication
    Left = 32
    Top = 192
  end
end


