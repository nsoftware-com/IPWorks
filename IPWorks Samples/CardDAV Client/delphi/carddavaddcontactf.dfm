object FormCarddavAddContact: TFormCarddavAddContact
  Left = 0
  Top = 0
  Caption = 'CardDAV Demo'
  ClientHeight = 248
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 178
    Height = 13
    Caption = 'Specify details of the contact to add.'
    WordWrap = True
  end
  object fnLabel: TLabel
    Left = 24
    Top = 57
    Width = 84
    Height = 13
    Caption = 'Formatted Name:'
  end
  object phoneNumberLabel: TLabel
    Left = 24
    Top = 89
    Width = 74
    Height = 13
    Caption = 'Phone Number:'
  end
  object emailLabel: TLabel
    Left = 24
    Top = 121
    Width = 28
    Height = 13
    Caption = 'Email:'
  end
  object addressLabel: TLabel
    Left = 24
    Top = 152
    Width = 39
    Height = 13
    Caption = 'Address'
  end
  object txtFormattedName: TEdit
    Left = 116
    Top = 54
    Width = 256
    Height = 21
    TabOrder = 0
  end
  object txtPhoneNumber: TEdit
    Left = 116
    Top = 86
    Width = 256
    Height = 21
    TabOrder = 1
  end
  object txtEmail: TEdit
    Left = 116
    Top = 113
    Width = 256
    Height = 21
    TabOrder = 2
  end
  object txtAddress: TEdit
    Left = 116
    Top = 149
    Width = 256
    Height = 21
    TabOrder = 3
  end
  object btnOK: TButton
    Left = 116
    Top = 197
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 212
    Top = 197
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
