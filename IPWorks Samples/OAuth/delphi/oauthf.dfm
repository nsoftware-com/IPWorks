object FormOauth: TFormOauth
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'OAuth Demo'
  ClientHeight = 449
  ClientWidth = 865
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
    Top = 8
    Width = 837
    Height = 39
    Caption = 
      'This application demonstrates how to use the OAuth component to ' +
      'authenticate with Google using OAuth 2.0 (Application Client Pro' +
      'file).  It also demonstrates how to use the retrieved Authorizat' +
      'ion String with the HTTP and JSON components to retrieve user in' +
      'formation.   Below are steps describing how to perform authoriza' +
      'tion using OAuth.  Please see the Introduction page within the h' +
      'elp for more detailed instructions.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Panel1: TPanel
    Left = 8
    Top = 64
    Width = 417
    Height = 81
    TabOrder = 0
    object Label2: TLabel
      Left = 18
      Top = 16
      Width = 45
      Height = 13
      Caption = 'Client ID:'
    end
    object Label3: TLabel
      Left = 16
      Top = 43
      Width = 65
      Height = 13
      Caption = 'Client Secret:'
    end
    object txtClientID: TEdit
      Left = 96
      Top = 16
      Width = 313
      Height = 21
      TabOrder = 0
      Text = '723966830965.apps.googleusercontent.com'
    end
    object txtClientSecret: TEdit
      Left = 96
      Top = 43
      Width = 313
      Height = 21
      TabOrder = 1
      Text = '_bYMDLuvYkJeT_99Q-vkP1rh'
    end
  end
  object Panel2: TPanel
    Left = 8
    Top = 151
    Width = 417
    Height = 106
    TabOrder = 1
    object Label4: TLabel
      Left = 18
      Top = 16
      Width = 84
      Height = 13
      Caption = 'Server Auth URL:'
    end
    object Label5: TLabel
      Left = 18
      Top = 43
      Width = 90
      Height = 13
      Caption = 'Server Token URL:'
    end
    object Label6: TLabel
      Left = 18
      Top = 70
      Width = 100
      Height = 13
      Caption = 'Authorization Scope:'
    end
    object txtServerAuthURL: TEdit
      Left = 124
      Top = 16
      Width = 285
      Height = 21
      TabOrder = 0
      Text = 'https://accounts.google.com/o/oauth2/auth'
    end
    object txtServerTokenURL: TEdit
      Left = 124
      Top = 43
      Width = 285
      Height = 21
      TabOrder = 1
      Text = 'https://accounts.google.com/o/oauth2/token'
    end
    object txtAuthScope: TEdit
      Left = 124
      Top = 70
      Width = 285
      Height = 21
      TabOrder = 2
      Text = 'https://www.googleapis.com/auth/userinfo.email'
    end
  end
  object btnAuthorize: TButton
    Left = 160
    Top = 263
    Width = 97
    Height = 25
    Caption = 'Authorize'
    TabOrder = 2
    OnClick = btnAuthorizeClick
  end
  object Panel3: TPanel
    Left = 8
    Top = 294
    Width = 417
    Height = 41
    TabOrder = 3
    object Label7: TLabel
      Left = 16
      Top = 8
      Width = 99
      Height = 13
      Caption = 'Authorization String:'
    end
    object txtAuthString: TEdit
      Left = 124
      Top = 8
      Width = 285
      Height = 21
      TabOrder = 0
    end
  end
  object btnGetUserInfo: TButton
    Left = 160
    Top = 341
    Width = 97
    Height = 25
    Caption = 'Get User Info'
    TabOrder = 4
    OnClick = btnGetUserInfoClick
  end
  object Panel4: TPanel
    Left = 8
    Top = 390
    Width = 417
    Height = 51
    TabOrder = 5
    object Label8: TLabel
      Left = 16
      Top = 8
      Width = 28
      Height = 13
      Caption = 'Email:'
    end
    object Label9: TLabel
      Left = 16
      Top = 27
      Width = 40
      Height = 13
      Caption = 'Verified:'
    end
    object lblEmail: TLabel
      Left = 71
      Top = 8
      Width = 3
      Height = 13
    end
    object lblVerified: TLabel
      Left = 71
      Top = 27
      Width = 3
      Height = 13
    end
  end
  object Panel5: TPanel
    Left = 440
    Top = 64
    Width = 417
    Height = 377
    TabOrder = 6
    object Label12: TLabel
      Left = 8
      Top = 14
      Width = 90
      Height = 16
      Caption = 'OAuth Steps:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label13: TLabel
      Left = 8
      Top = 36
      Width = 391
      Height = 52
      Caption = 
        '1. Obtain and set your Client ID and Client Secret.  For Google,' +
        ' these values can be found in the API Console: https://code.goog' +
        'le.com/apis/console#access. The preset values are from a Google ' +
        'test account that we have setup for you to easily run this demo.'
      WordWrap = True
    end
    object Label14: TLabel
      Left = 8
      Top = 95
      Width = 383
      Height = 39
      Caption = 
        '2.  You can also set Server Auth URL, Server Token URL, and Auth' +
        'orization Scope to the values desired.  These are preset to valu' +
        'es for Google'#39's User Info service.'
      WordWrap = True
    end
    object Label15: TLabel
      Left = 8
      Top = 143
      Width = 392
      Height = 52
      Caption = 
        '3. Click the '#39'Authorize'#39' button.  This will open a web browser a' +
        'nd allow the user to authenticate to the service.  Upon the user' +
        ' successfully authenticating and allowing access, the user will ' +
        'be redirected back to an embedded web server within the componen' +
        't.'
      WordWrap = True
    end
    object Label16: TLabel
      Left = 8
      Top = 207
      Width = 359
      Height = 26
      Caption = 
        '4. Upon successful authentication, the Authorization String text' +
        'box will be populated.'
      WordWrap = True
    end
    object Label17: TLabel
      Left = 8
      Top = 239
      Width = 378
      Height = 39
      Caption = 
        '5. Clicking '#39'Get User Info'#39' will set the Authorization String to' +
        ' the '#39'Authorization'#39' property of the HTTP component and retrieve' +
        ' the user info for the authenticated client.'
      WordWrap = True
    end
  end
  object ipwOAuth1: TipwOAuth
    SSLCertStore = 'MY'
    OnLaunchBrowser = ipwOAuth1LaunchBrowser
    OnReturnURL = ipwOAuth1ReturnURL
    OnSSLServerAuthentication = ipwOAuth1SSLServerAuthentication
    Left = 312
    Top = 344
  end
  object ipwJSON1: TipwJSON
    Left = 368
    Top = 352
  end
  object ipwHTTP1: TipwHTTP
    SSLCertStore = 'MY'
    OnSSLServerAuthentication = ipwHTTP1SSLServerAuthentication
    Left = 48
    Top = 352
  end
end


