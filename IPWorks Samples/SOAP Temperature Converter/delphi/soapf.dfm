object FormSoap: TFormSoap
  Left = 187
  Top = 125
  BorderStyle = bsSingle
  Caption = 'SOAP Temperature Converter'
  ClientHeight = 123
  ClientWidth = 477
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 446
    Height = 26
    Caption = 
      'This demo shows how to use the SOAP component, with a freely ava' +
      'ilable web service,  to do temperature conversion.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 48
    Width = 457
    Height = 65
    Caption = 'Unit Conversion'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Label2: TLabel
      Left = 13
      Top = 30
      Width = 53
      Height = 13
      Caption = 'Fahrenheit:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 278
      Top = 30
      Width = 36
      Height = 13
      Caption = 'Celsius:'
    end
    object txtFahrenheit: TEdit
      Left = 72
      Top = 27
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object btnFahrenheit: TButton
      Left = 200
      Top = 24
      Width = 33
      Height = 25
      Caption = '<-'
      TabOrder = 1
      OnClick = btnFahrenheitClick
    end
    object btnCelsius: TButton
      Left = 239
      Top = 24
      Width = 33
      Height = 25
      Caption = '->'
      TabOrder = 2
      OnClick = btnCelsiusClick
    end
    object txtCelsius: TEdit
      Left = 320
      Top = 27
      Width = 121
      Height = 21
      TabOrder = 3
    end
  end
  object SOAP1: TipwSOAP
    ActionURI = '#'
    SOAPEncoding = 'http://schemas.xmlsoap.org/soap/encoding/'
    SSLCertStore = 'MY'
    OnSSLServerAuthentication = SOAP1SSLServerAuthentication
    Left = 440
    Top = 24
  end
end


