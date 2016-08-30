object mainFrm: TmainFrm
  Left = 0
  Top = 0
  Caption = 'mainFrm'
  ClientHeight = 436
  ClientWidth = 644
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object image: TImage
    Left = 0
    Top = 0
    Width = 453
    Height = 395
    Align = alClient
    AutoSize = True
    Center = True
    Proportional = True
    Stretch = True
  end
  object Splitter_38A8D14A: TSplitter
    Left = 453
    Top = 0
    Height = 395
    Align = alRight
  end
  object Log: TMemo
    Left = 456
    Top = 0
    Width = 188
    Height = 395
    Align = alRight
    TabOrder = 0
  end
  object bottomPanel: TPanel
    Left = 0
    Top = 395
    Width = 644
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btnLoadFromFile: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Caption = 'btnLoad'
      TabOrder = 0
      OnClick = btnLoadFromFileClick
    end
  end
  object openDlg: TOpenDialog
    Left = 80
    Top = 328
  end
end
