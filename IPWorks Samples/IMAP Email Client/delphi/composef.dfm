object FormCompose: TFormCompose
  Left = 284
  Top = 133
  Width = 500
  Height = 488
  Caption = 'Compose Email'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 32
    Top = 124
    Width = 39
    Height = 13
    Caption = 'Subject:'
  end
  object Label3: TLabel
    Left = 32
    Top = 96
    Width = 16
    Height = 13
    Caption = 'Cc:'
  end
  object Label1: TLabel
    Left = 32
    Top = 68
    Width = 16
    Height = 13
    Caption = 'To:'
  end
  object Label4: TLabel
    Left = 32
    Top = 40
    Width = 29
    Height = 13
    Caption = 'From: '
  end
  object Label6: TLabel
    Left = 8
    Top = 8
    Width = 338
    Height = 13
    Caption = 
      'Please fill in the required fields and click the '#39'Send'#39' button t' +
      'o send email.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 422
    Width = 62
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Attachments:'
  end
  object MemoMessage: TMemo
    Left = 0
    Top = 152
    Width = 492
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      '')
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object EditSubject: TEdit
    Left = 100
    Top = 120
    Width = 300
    Height = 21
    TabOrder = 3
  end
  object EditCc: TEdit
    Left = 100
    Top = 92
    Width = 300
    Height = 21
    TabOrder = 2
  end
  object EditTo: TEdit
    Left = 100
    Top = 64
    Width = 300
    Height = 21
    TabOrder = 1
  end
  object EditFrom: TEdit
    Left = 100
    Top = 36
    Width = 301
    Height = 21
    TabOrder = 0
  end
  object ComboBoxAttachments: TComboBox
    Left = 80
    Top = 422
    Width = 116
    Height = 21
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    TabOrder = 5
  end
  object ButtonRemove: TButton
    Left = 201
    Top = 430
    Width = 64
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '&Remove'
    TabOrder = 7
    OnClick = ButtonRemoveClick
  end
  object ButtonAdd: TButton
    Left = 201
    Top = 406
    Width = 64
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '&Add'
    TabOrder = 6
    OnClick = ButtonAddClick
  end
  object ButtonSend: TButton
    Left = 314
    Top = 418
    Width = 84
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'S&end'
    ModalResult = 1
    TabOrder = 8
  end
  object ButtonCancel: TButton
    Left = 401
    Top = 418
    Width = 84
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object OpenDialog1: TOpenDialog
    Left = 408
    Top = 36
  end
end
