object SelectFrame: TSelectFrame
  Left = 0
  Top = 0
  Width = 157
  Height = 35
  Color = clWhite
  ParentColor = False
  TabOrder = 0
  object NoLabel: TLabel
    Left = 8
    Top = 9
    Width = 14
    Height = 13
    Caption = 'No'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object RandomBtn: TButton
    Left = 100
    Top = 6
    Width = 23
    Height = 22
    Caption = 'R'
    TabOrder = 1
    OnClick = RandomBtnClick
  end
  object PlayBtn: TButton
    Left = 125
    Top = 6
    Width = 23
    Height = 22
    Caption = 'P'
    TabOrder = 2
    OnClick = PlayBtnClick
  end
  object NoteCombo: TComboBox
    Left = 32
    Top = 6
    Width = 63
    Height = 22
    Style = csOwnerDrawFixed
    ItemHeight = 16
    TabOrder = 0
    OnChange = NoteComboChange
    OnClick = NoteComboClick
  end
end
