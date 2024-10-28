object FormNetcode: TFormNetcode
  Left = 249
  Top = 172
  Width = 503
  Height = 404
  Caption = 'Netcode'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    495
    377)
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 75
    Height = 13
    Caption = 'Encoding Type:'
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 41
    Width = 481
    Height = 330
    ActivePage = TabSheet2
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabIndex = 1
    TabOrder = 0
    TabWidth = 100
    object TabSheet1: TTabSheet
      Caption = 'File to File'
      object Label1: TLabel
        Left = 8
        Top = 80
        Width = 56
        Height = 13
        Caption = 'Source File:'
      end
      object Label2: TLabel
        Left = 8
        Top = 168
        Width = 53
        Height = 13
        Caption = 'Target File:'
      end
      object EditSource: TEdit
        Left = 8
        Top = 96
        Width = 350
        Height = 21
        TabOrder = 0
      end
      object ButtonGetSource: TButton
        Left = 362
        Top = 96
        Width = 75
        Height = 23
        Caption = 'Browse'
        TabOrder = 1
        OnClick = ButtonGetSourceClick
      end
      object EditTarget: TEdit
        Left = 8
        Top = 184
        Width = 350
        Height = 21
        TabOrder = 2
      end
      object ButtonGetTarget: TButton
        Left = 362
        Top = 184
        Width = 75
        Height = 23
        Caption = 'Browse'
        TabOrder = 3
        OnClick = ButtonGetTargetClick
      end
      object ButtonConvert: TButton
        Left = 176
        Top = 240
        Width = 75
        Height = 23
        Caption = 'Convert'
        TabOrder = 4
        OnClick = ButtonConvertClick
      end
      object RadioGroupAction: TRadioGroup
        Left = 8
        Top = 16
        Width = 145
        Height = 45
        Caption = ' Action '
        Columns = 2
        Items.Strings = (
          'Encode'
          'Decode')
        TabOrder = 5
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'String to String'
      DesignSize = (
        473
        302)
      object Label3: TLabel
        Left = 0
        Top = 12
        Width = 70
        Height = 13
        Caption = 'Decoded Data'
      end
      object Label4: TLabel
        Left = 248
        Top = 12
        Width = 69
        Height = 13
        Caption = 'Encoded Data'
      end
      object Label6: TLabel
        Left = 204
        Top = 88
        Width = 37
        Height = 13
        Caption = 'Encode'
      end
      object Label7: TLabel
        Left = 203
        Top = 156
        Width = 38
        Height = 13
        Caption = 'Decode'
      end
      object MemoDecoded: TMemo
        Left = 0
        Top = 32
        Width = 195
        Height = 278
        Anchors = [akLeft, akTop, akBottom]
        TabOrder = 0
      end
      object MemoEncoded: TMemo
        Left = 248
        Top = 32
        Width = 260
        Height = 278
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 1
      end
      object ButtonEncode: TButton
        Left = 208
        Top = 108
        Width = 25
        Height = 23
        Caption = '-->'
        TabOrder = 2
        OnClick = ButtonEncodeClick
      end
      object ButtonDecode: TButton
        Left = 208
        Top = 176
        Width = 25
        Height = 23
        Caption = '<--'
        TabOrder = 3
        OnClick = ButtonDecodeClick
      end
    end
  end
  object ComboBoxEncType: TComboBox
    Left = 88
    Top = 4
    Width = 120
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = 'Base 64'
    OnChange = ComboBoxEncTypeChange
    Items.Strings = (
      'UUEncode'
      'Base 64'
      'Quoted-Printable'
      'URL'
      'JIS'
      'YEnc'
      'MD5 Hash'
      'SHA1 Hash'
      'Hex')
  end
  object ProgressBarConvert: TProgressBar
    Left = 249
    Top = 8
    Width = 200
    Height = 16
    Min = 0
    Max = 100
    TabOrder = 2
  end
  object OpenDialog1: TOpenDialog
    Left = 148
    Top = 60
  end
  object ipwNetCode1: TipwNetCode
    Mode = '0755'
    OnProgress = ipwNetCode1Progress
    Left = 112
    Top = 60
  end
end


