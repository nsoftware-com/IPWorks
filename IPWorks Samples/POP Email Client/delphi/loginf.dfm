object FormLogin: TFormLogin
  Left = 389
  Top = 269
  BorderStyle = bsSingle
  Caption = 'Login'
  ClientHeight = 172
  ClientWidth = 272
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 59
    Height = 13
    Caption = 'POP Server:'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 28
    Height = 13
    Caption = 'User: '
  end
  object Label3: TLabel
    Left = 8
    Top = 80
    Width = 52
    Height = 13
    Caption = 'Password: '
  end
  object Label4: TLabel
    Left = 8
    Top = 112
    Width = 70
    Height = 13
    Caption = 'SMTP Server: '
  end
  object EditUser: TEdit
    Left = 88
    Top = 44
    Width = 175
    Height = 21
    TabOrder = 1
  end
  object EditPassword: TEdit
    Left = 88
    Top = 76
    Width = 175
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
  end
  object EditPOPServer: TEdit
    Left = 88
    Top = 12
    Width = 175
    Height = 21
    TabOrder = 0
  end
  object EditSMTPServer: TEdit
    Left = 88
    Top = 108
    Width = 175
    Height = 21
    TabOrder = 3
  end
  object ButtonOK: TButton
    Left = 108
    Top = 140
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object ButtonCancel: TButton
    Left = 192
    Top = 140
    Width = 71
    Height = 23
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
