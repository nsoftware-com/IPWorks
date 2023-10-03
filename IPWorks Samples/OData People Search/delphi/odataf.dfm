object FormOdata: TFormOdata
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'OData Demo'
  ClientHeight = 520
  ClientWidth = 778
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 9
    Width = 440
    Height = 13
    Caption = 
      'This application demonstrates using the OData component to query' +
      ' a test OData V4 service'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 5
    Top = 30
    Width = 364
    Height = 485
    Caption = 'Panel1'
    TabOrder = 0
    object Label2: TLabel
      Left = 5
      Top = 13
      Width = 96
      Height = 13
      Caption = 'Filter by Last Name:'
    end
    object txtSearch: TEdit
      Left = 107
      Top = 12
      Width = 166
      Height = 21
      TabOrder = 0
    end
    object lvwSearchResults: TListView
      Left = 5
      Top = 39
      Width = 352
      Height = 431
      Columns = <
        item
          AutoSize = True
          Caption = 'First Name'
          MaxWidth = 413
          MinWidth = 1
        end
        item
          Caption = 'Last Name'
          MaxWidth = 413
          MinWidth = 50
          Width = 178
        end>
      RowSelect = True
      TabOrder = 2
      ViewStyle = vsReport
      OnSelectItem = lvwSearchResultsSelectItem
    end
    object btnSearch: TButton
      Left = 279
      Top = 9
      Width = 75
      Height = 23
      Caption = 'Search'
      TabOrder = 1
      OnClick = btnSearchClick
    end
  end
  object Panel2: TPanel
    Left = 375
    Top = 85
    Width = 394
    Height = 429
    TabOrder = 1
    object Label3: TLabel
      Left = 24
      Top = 32
      Width = 55
      Height = 13
      Caption = 'First Name:'
    end
    object Label4: TLabel
      Left = 24
      Top = 80
      Width = 28
      Height = 13
      Caption = 'Email:'
    end
    object Label5: TLabel
      Left = 24
      Top = 136
      Width = 39
      Height = 13
      Caption = 'Gender:'
    end
    object Label6: TLabel
      Left = 24
      Top = 191
      Width = 23
      Height = 13
      Caption = 'Age:'
    end
    object Label7: TLabel
      Left = 24
      Top = 248
      Width = 43
      Height = 13
      Caption = 'Address:'
    end
    object Label8: TLabel
      Left = 24
      Top = 304
      Width = 23
      Height = 13
      Caption = 'City:'
    end
    object Label9: TLabel
      Left = 24
      Top = 360
      Width = 43
      Height = 13
      Caption = 'Country:'
    end
    object lblUsername: TLabel
      Left = 232
      Top = 32
      Width = 58
      Height = 13
      Caption = 'lblUsername'
      Visible = False
    end
    object lblEmail: TLabel
      Left = 232
      Top = 80
      Width = 34
      Height = 13
      Caption = 'lblEmail'
      Visible = False
    end
    object lblGender: TLabel
      Left = 232
      Top = 136
      Width = 45
      Height = 13
      Caption = 'lblGender'
      Visible = False
    end
    object lblAge: TLabel
      Left = 232
      Top = 191
      Width = 29
      Height = 13
      Caption = 'lblAge'
      Visible = False
    end
    object lblAddress: TLabel
      Left = 232
      Top = 248
      Width = 49
      Height = 13
      Caption = 'lblAddress'
      Visible = False
    end
    object lblCity: TLabel
      Left = 232
      Top = 304
      Width = 29
      Height = 13
      Caption = 'lblCity'
      Visible = False
    end
    object lblCountry: TLabel
      Left = 232
      Top = 360
      Width = 49
      Height = 13
      Caption = 'lblCountry'
      Visible = False
    end
  end
  object ipwOData1: TipwOData
    SSLCertStore = 'MY'
    Left = 448
    Top = 40
  end
end


