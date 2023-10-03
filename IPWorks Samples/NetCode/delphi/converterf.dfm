object FormConverter: TFormConverter
  Left = 397
  Top = 252
  Width = 224
  Height = 133
  Caption = 'Convert'
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
    Width = 104
    Height = 13
    Caption = 'Name of file to create:'
  end
  object EditFilename: TEdit
    Left = 8
    Top = 36
    Width = 200
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 48
    Top = 68
    Width = 75
    Height = 23
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 132
    Top = 68
    Width = 75
    Height = 23
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
