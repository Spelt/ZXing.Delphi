object Form2: TForm2
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Form2'
  ClientHeight = 605
  ClientWidth = 646
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 56
    Width = 425
    Height = 373
    Proportional = True
    Stretch = True
  end
  object Label1: TLabel
    Left = 8
    Top = 14
    Width = 442
    Height = 19
    Caption = 'webcam libs from https://github.com/heise/GRBLize/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 456
    Top = 56
    Width = 173
    Height = 25
    Caption = 'Enable video'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 456
    Top = 96
    Width = 173
    Height = 25
    Caption = 'Disable video'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 442
    Width = 630
    Height = 155
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
