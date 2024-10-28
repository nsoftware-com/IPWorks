object FormS3: TFormS3
  Left = 171
  Top = 103
  Caption = 'IPWorks S3 Demo'
  ClientHeight = 638
  ClientWidth = 502
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PixelsPerInch = 96
  DesignSize = (
    502
    638)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 453
    Height = 39
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'This demo shows how to use the IPWorks S3 component to interact ' +
      'with S3 compatible services. It is assumed that you'#39've already s' +
      'igned up for a compatible service and obtained your Access Key a' +
      'nd Secret Key.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 53
    Width = 486
    Height = 116
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Authentication'
    TabOrder = 0
    DesignSize = (
      486
      116)
    object Label2: TLabel
      Left = 8
      Top = 25
      Width = 59
      Height = 13
      Caption = 'Access Key:'
    end
    object Label3: TLabel
      Left = 8
      Top = 52
      Width = 55
      Height = 13
      Caption = 'Secret Key:'
    end
    object Label4: TLabel
      Left = 8
      Top = 79
      Width = 81
      Height = 13
      Caption = 'Service Provider:'
    end
    object tAccessKey: TEdit
      Left = 95
      Top = 25
      Width = 319
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object tSecretKey: TEdit
      Left = 95
      Top = 52
      Width = 319
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object bGo: TButton
      Left = 421
      Top = 25
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Go'
      TabOrder = 2
      OnClick = bGoClick
    end
    object cboProvider: TComboBox
      Left = 95
      Top = 79
      Width = 319
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      Text = 'Amazon S3'
      Items.Strings = (
        'Amazon S3'
        'Digital Ocean'
        'Google Cloud Storage'
        'Wasabi'
        'Backblaze B2'
        'Huawei'
        'Alibaba'
        'IBM Cloud'
        'Oracle Cloud'
        'Linode Object Storage'
        'Custom')
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 175
    Width = 486
    Height = 202
    Anchors = [akLeft, akTop, akRight]
    Caption = 'S3 Buckets'
    TabOrder = 1
    DesignSize = (
      486
      202)
    object lvwBuckets: TListView
      Left = 8
      Top = 16
      Width = 406
      Height = 177
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Name'
          Width = 185
        end
        item
          Caption = 'Creation Date'
          Width = 200
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = lvwBucketsSelectItem
    end
    object bNewBucket: TButton
      Left = 421
      Top = 16
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&New'
      TabOrder = 1
      OnClick = bNewBucketClick
    end
    object bDelete: TButton
      Left = 421
      Top = 48
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Delete'
      TabOrder = 2
      OnClick = bDeleteClick
    end
  end
  object gbObjects: TGroupBox
    Left = 8
    Top = 383
    Width = 486
    Height = 250
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Objects'
    TabOrder = 2
    DesignSize = (
      486
      250)
    object lvwObjects: TListView
      Left = 8
      Top = 16
      Width = 406
      Height = 226
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Name'
          Width = 185
        end
        item
          Caption = 'Last Modified'
          Width = 100
        end
        item
          Caption = 'Size'
        end
        item
          Caption = 'Owner'
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
    object bNewObject: TButton
      Left = 421
      Top = 16
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Add'
      TabOrder = 1
      OnClick = bNewObjectClick
    end
    object bDeleteObject: TButton
      Left = 421
      Top = 48
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Remove'
      TabOrder = 2
      OnClick = bDeleteObjectClick
    end
    object bGetFile: TButton
      Left = 421
      Top = 80
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Get File'
      TabOrder = 3
      OnClick = bGetFileClick
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 440
    Top = 112
  end
  object OpenDialog1: TOpenDialog
    Left = 440
    Top = 112
  end
  object ipwS31: TipwS3
    Region = 'us-east-1'
    SSLCertStore = 'MY'
    OnBucketList = ipwS31BucketList
    OnObjectList = ipwS31ObjectList
    Left = 208
    Top = 88
  end
end


