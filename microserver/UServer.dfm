object FServer: TFServer
  Left = 0
  Top = 0
  Caption = 'FServer'
  ClientHeight = 378
  ClientWidth = 889
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Title: TLabel
    Left = 9
    Top = 5
    Width = 170
    Height = 33
    Caption = 'Micro Server '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Century Gothic'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 564
    Top = 8
    Width = 246
    Height = 30
    Caption = 'clients connected :  '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Century Gothic'
    Font.Style = []
    ParentFont = False
  end
  object clients_connected: TLabel
    Left = 874
    Top = 9
    Width = 7
    Height = 30
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 12615680
    Font.Height = -24
    Font.Name = 'Century Gothic'
    Font.Style = []
    ParentFont = False
  end
  object btn_start: TButton
    Left = 8
    Top = 333
    Width = 137
    Height = 33
    Cursor = crHandPoint
    Caption = 'Start Server'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clInactiveCaption
    Font.Height = -17
    Font.Name = 'Century Gothic'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = btn_startClick
  end
  object btn_stop: TButton
    Left = 151
    Top = 333
    Width = 137
    Height = 33
    Cursor = crHandPoint
    Caption = 'Stop Server'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Century Gothic'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btn_stopClick
  end
  object btn_clear: TButton
    Left = 294
    Top = 333
    Width = 137
    Height = 33
    Cursor = crHandPoint
    Caption = 'Clear Log'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Century Gothic'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = btn_clearClick
  end
  object Panel1: TPanel
    Left = 8
    Top = 44
    Width = 873
    Height = 283
    BevelOuter = bvNone
    Caption = 'Panel1'
    Enabled = False
    TabOrder = 3
    object messagesLog: TMemo
      Left = 1
      Top = 1
      Width = 872
      Height = 279
      BorderStyle = bsNone
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -17
      Font.Name = 'Courier New'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
  end
  object btn_send: TButton
    Left = 437
    Top = 333
    Width = 137
    Height = 33
    Cursor = crHandPoint
    Caption = 'Send File'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Century Gothic'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = btn_sendClick
  end
end
