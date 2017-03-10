object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Mozart Dice'
  ClientHeight = 472
  ClientWidth = 652
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 453
    Width = 652
    Height = 19
    Panels = <>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 652
    Height = 67
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label3: TLabel
      Left = 305
      Top = 10
      Width = 41
      Height = 14
      Caption = 'Volume'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbl_Synth: TLabel
      Left = 11
      Top = 10
      Width = 62
      Height = 14
      Alignment = taRightJustify
      Caption = 'Synthesizer'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object cbSynthesizer: TComboBox
      Left = 79
      Top = 8
      Width = 212
      Height = 21
      Style = csDropDownList
      ImeName = 'Microsoft Office IME 2007'
      ItemHeight = 13
      TabOrder = 0
      TabStop = False
      OnChange = cbSynthesizerChange
    end
    object tbVolume: TTrackBar
      Left = 352
      Top = 8
      Width = 57
      Height = 21
      Max = 21
      TabOrder = 1
      TickStyle = tsNone
      OnChange = tbVolumeChange
    end
    object PositionBar: TProgressBar
      Left = 143
      Top = 39
      Width = 266
      Height = 17
      Cursor = crHandPoint
      Align = alCustom
      Smooth = True
      TabOrder = 2
    end
    object btnPlay: TButton
      Left = 11
      Top = 35
      Width = 60
      Height = 25
      Caption = 'Play'
      TabOrder = 3
      OnClick = btnPlayClick
    end
    object btnStop: TButton
      Left = 77
      Top = 35
      Width = 60
      Height = 25
      Caption = 'Stop'
      TabOrder = 4
      OnClick = btnStopClick
    end
    object btnPause: TButton
      Left = 543
      Top = 10
      Width = 84
      Height = 25
      Caption = 'Pause'
      TabOrder = 5
      Visible = False
      OnClick = btnPauseClick
    end
    object Button1: TButton
      Left = 543
      Top = 34
      Width = 84
      Height = 25
      Caption = 'Tempo/Pitch'
      TabOrder = 6
      Visible = False
    end
    object Button2: TButton
      Left = 424
      Top = 10
      Width = 113
      Height = 46
      Caption = 'Make Music!!'
      TabOrder = 7
      OnClick = Button2Click
    end
  end
  object ScrollBox: TScrollBox
    Left = 0
    Top = 67
    Width = 652
    Height = 386
    Align = alClient
    Color = clWhite
    Ctl3D = False
    ParentColor = False
    ParentCtl3D = False
    TabOrder = 2
    OnResize = ScrollBoxResize
    ExplicitTop = 97
    ExplicitHeight = 356
    object MinuetLabel: TLabel
      Left = 4
      Top = 3
      Width = 39
      Height = 13
      Caption = 'Minuet'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object TrioLabel: TLabel
      Left = 4
      Top = 179
      Width = 22
      Height = 13
      Caption = 'Trio'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object XPManifest1: TXPManifest
    Left = 115
    Top = 132
  end
end
