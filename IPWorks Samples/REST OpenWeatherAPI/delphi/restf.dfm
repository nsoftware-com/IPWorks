object FormRest: TFormRest
  Left = 0
  Top = 0
  Caption = 'Rest Demo'
  ClientHeight = 408
  ClientWidth = 463
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 15
  object DescriptionLabel: TLabel
    Left = 8
    Top = 8
    Width = 445
    Height = 45
    Caption = 
      'This demo shows how to use the REST component to get weather inf' +
      'ormation using OpenWeather API web service. You'#39'll need to sign ' +
      'up for an API key before you can use this demo. Get an API key: ' +
      'https://home.openweathermap.org/users/sign_up'
    WordWrap = True
  end
  object AuthenticationLabel: TLabel
    Left = 24
    Top = 80
    Width = 79
    Height = 15
    Caption = 'Authentication'
  end
  object APIKeyLabel: TLabel
    Left = 39
    Top = 104
    Width = 46
    Height = 15
    Caption = 'Api Key:'
  end
  object LocationLabel: TLabel
    Left = 24
    Top = 160
    Width = 46
    Height = 15
    Caption = 'Location'
  end
  object CityLabel: TLabel
    Left = 59
    Top = 184
    Width = 26
    Height = 15
    Caption = 'City:'
  end
  object StateLabel: TLabel
    Left = 54
    Top = 205
    Width = 31
    Height = 15
    Caption = 'State:'
  end
  object CountryLabel: TLabel
    Left = 39
    Top = 226
    Width = 46
    Height = 15
    Caption = 'Country:'
  end
  object LatitudeLabel: TLabel
    Left = 272
    Top = 184
    Width = 49
    Height = 15
    Caption = 'Latitude:'
  end
  object LongitudeLabel: TLabel
    Left = 264
    Top = 205
    Width = 57
    Height = 15
    Caption = 'Longitude:'
  end
  object WeatherLabel: TLabel
    Left = 24
    Top = 280
    Width = 44
    Height = 15
    Caption = 'Weather'
  end
  object ConditionsLabel: TLabel
    Left = 38
    Top = 301
    Width = 47
    Height = 15
    Caption = 'Weather:'
  end
  object TempLabel: TLabel
    Left = 53
    Top = 322
    Width = 32
    Height = 15
    Caption = 'Temp:'
  end
  object PressureLabel: TLabel
    Left = 38
    Top = 343
    Width = 47
    Height = 15
    Caption = 'Pressure:'
  end
  object HumidityLabel: TLabel
    Left = 32
    Top = 364
    Width = 53
    Height = 15
    Caption = 'Humidity:'
  end
  object KelvinsLabel: TLabel
    Left = 256
    Top = 322
    Width = 36
    Height = 15
    Caption = 'kelvins'
  end
  object hPaLabel: TLabel
    Left = 256
    Top = 343
    Width = 20
    Height = 15
    Caption = 'hPa'
  end
  object PercentLabel: TLabel
    Left = 256
    Top = 364
    Width = 10
    Height = 15
    Caption = '%'
  end
  object APIKeyEdit: TEdit
    Left = 91
    Top = 101
    Width = 364
    Height = 20
    TabOrder = 0
  end
  object CityEdit: TEdit
    Left = 91
    Top = 181
    Width = 145
    Height = 20
    TabOrder = 1
    Text = 'Chapel Hill'
  end
  object StateEdit: TEdit
    Left = 91
    Top = 202
    Width = 145
    Height = 20
    TabOrder = 2
    Text = 'NC'
  end
  object CountryEdit: TEdit
    Left = 91
    Top = 223
    Width = 145
    Height = 20
    TabOrder = 3
    Text = 'US'
  end
  object LatitudeEdit: TEdit
    Left = 327
    Top = 181
    Width = 97
    Height = 20
    Enabled = False
    TabOrder = 4
  end
  object LongitudeEdit: TEdit
    Left = 327
    Top = 205
    Width = 97
    Height = 20
    Enabled = False
    TabOrder = 5
  end
  object ConditionsEdit: TEdit
    Left = 91
    Top = 296
    Width = 230
    Height = 20
    TabOrder = 6
  end
  object TempEdit: TEdit
    Left = 91
    Top = 319
    Width = 145
    Height = 20
    TabOrder = 7
  end
  object PressureEdit: TEdit
    Left = 91
    Top = 340
    Width = 145
    Height = 20
    TabOrder = 8
  end
  object HumidityEdit: TEdit
    Left = 91
    Top = 361
    Width = 145
    Height = 20
    TabOrder = 9
  end
  object SearchButton: TButton
    Left = 336
    Top = 297
    Width = 105
    Height = 61
    Caption = 'Get Weather'
    TabOrder = 10
    OnClick = SearchButtonClick
  end
  object ipwNetCode1: TipwNetCode
    Format = fmtURL
    Mode = '0755'
    Left = 400
    Top = 376
  end
  object ipwREST1: TipwREST
    SSLCertStore = 'MY'
    Left = 432
    Top = 376
  end
end






