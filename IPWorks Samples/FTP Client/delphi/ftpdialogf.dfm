object FormFtpdialog: TFormFtpdialog
  Left = 430
  Top = 385
  ActiveControl = ButtonOk
  BorderStyle = bsDialog
  Caption = 'FormFtpdialog'
  ClientHeight = 81
  ClientWidth = 285
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object LabelWhat: TLabel
    Left = 12
    Top = 8
    Width = 52
    Height = 13
    Caption = 'LabelWhat'
    Layout = tlCenter
  end
  object ButtonOk: TButton
    Left = 120
    Top = 56
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object ButtonCancel: TButton
    Left = 200
    Top = 56
    Width = 75
    Height = 23
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object EditLine: TEdit
    Left = 12
    Top = 28
    Width = 265
    Height = 21
    MaxLength = 32767
    TabOrder = 2
  end
end
