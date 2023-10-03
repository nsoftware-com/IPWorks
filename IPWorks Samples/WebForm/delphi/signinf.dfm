object FormSignin: TFormSignin
  Left = 192
  Top = 108
  BorderStyle = bsDialog
  Caption = 'HTTP Post Demo'
  ClientHeight = 387
  ClientWidth = 490
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
    Top = 12
    Width = 467
    Height = 39
    Caption = 
      'This demo shows how to use the HTTP control to allow users to si' +
      'gn your guest book.  Simply fill in the requested information an' +
      'd click SUBMIT.  The information will be sent to the /n software' +
      ' server.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 37
    Height = 13
    Caption = 'Name:  '
  end
  object Label3: TLabel
    Left = 8
    Top = 92
    Width = 34
    Height = 13
    Caption = 'Email:  '
  end
  object Label4: TLabel
    Left = 8
    Top = 124
    Width = 191
    Height = 13
    Caption = 'Where did you hear about our products?'
  end
  object Label5: TLabel
    Left = 8
    Top = 172
    Width = 186
    Height = 26
    Caption = 'What industry publications do you read the most?'
    WordWrap = True
  end
  object Label6: TLabel
    Left = 280
    Top = 172
    Width = 200
    Height = 26
    Caption = 
      'What VB and/or C++ related Internet Sites do you frequent the mo' +
      'st?'
    WordWrap = True
  end
  object Label7: TLabel
    Left = 60
    Top = 311
    Width = 386
    Height = 26
    Caption = 
      'Clicking on Submit!! below will send the information you supplie' +
      'd to the /n software WWW server.  Thank You!!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Shape1: TShape
    Left = 240
    Top = 172
    Width = 1
    Height = 125
  end
  object Shape2: TShape
    Left = 8
    Top = 308
    Width = 473
    Height = 2
    Pen.Width = 2
  end
  object Shape3: TShape
    Left = 8
    Top = 380
    Width = 473
    Height = 2
    Pen.Width = 2
  end
  object EditName: TEdit
    Left = 56
    Top = 60
    Width = 425
    Height = 21
    TabOrder = 0
  end
  object EditEmail: TEdit
    Left = 56
    Top = 88
    Width = 425
    Height = 21
    TabOrder = 1
  end
  object MemoPublications: TMemo
    Left = 8
    Top = 208
    Width = 200
    Height = 89
    TabOrder = 3
  end
  object MemoInternet: TMemo
    Left = 280
    Top = 208
    Width = 201
    Height = 89
    TabOrder = 4
  end
  object ButtonSubmit: TButton
    Left = 206
    Top = 347
    Width = 75
    Height = 23
    Caption = 'Submit!!'
    TabOrder = 5
    OnClick = ButtonSubmitClick
  end
  object EditWhere: TEdit
    Left = 8
    Top = 140
    Width = 473
    Height = 21
    TabOrder = 2
  end
  object ipwWebForm1: TipwWebForm
    SSLCertStore = 'MY'
    OnSSLServerAuthentication = ipwWebForm1SSLServerAuthentication
    OnTransfer = ipwWebForm1Transfer
    Left = 416
    Top = 320
  end
end


