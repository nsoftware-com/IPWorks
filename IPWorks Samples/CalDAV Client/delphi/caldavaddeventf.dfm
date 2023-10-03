object FormCaldavAddEvent: TFormCaldavAddEvent
  Left = 0
  Top = 0
  Caption = 'CalDAV Demo'
  ClientHeight = 248
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 352
    Height = 26
    Caption = 
      'Specify details of the event to add. Dates must be supplied in t' +
      'he format "yyyyMMddTHHmmss"'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 24
    Top = 57
    Width = 54
    Height = 13
    Caption = 'Start Date:'
  end
  object Label3: TLabel
    Left = 24
    Top = 89
    Width = 48
    Height = 13
    Caption = 'End Date:'
  end
  object Label4: TLabel
    Left = 24
    Top = 121
    Width = 48
    Height = 13
    Caption = 'Summary:'
  end
  object Label5: TLabel
    Left = 24
    Top = 152
    Width = 57
    Height = 13
    Caption = 'Description:'
  end
  object Label6: TLabel
    Left = 24
    Top = 183
    Width = 44
    Height = 13
    Caption = 'Location:'
  end
  object txtStartDate: TEdit
    Left = 116
    Top = 54
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object txtEndDate: TEdit
    Left = 116
    Top = 86
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object txtSummary: TEdit
    Left = 116
    Top = 118
    Width = 256
    Height = 21
    TabOrder = 2
    Text = 'Project Meeting'
  end
  object txtDescription: TEdit
    Left = 116
    Top = 149
    Width = 256
    Height = 21
    TabOrder = 3
    Text = 'Meeting to discuss project status.'
  end
  object txtLocation: TEdit
    Left = 116
    Top = 180
    Width = 256
    Height = 21
    TabOrder = 4
    Text = 'Conference Room'
  end
  object btnOK: TButton
    Left = 116
    Top = 213
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 212
    Top = 213
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 6
  end
end
