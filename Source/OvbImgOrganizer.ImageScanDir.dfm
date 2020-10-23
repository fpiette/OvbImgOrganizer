object ScanDirForm: TScanDirForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select directory to scan'
  ClientHeight = 99
  ClientWidth = 503
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 23
    Width = 179
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Directory to scan'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 179
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Include subdirectories'
  end
  object ScanDirEdit: TEdit
    Left = 193
    Top = 20
    Width = 274
    Height = 21
    TabOrder = 0
    Text = 'ScanDirEdit'
  end
  object ScanRecursiveCheckBox: TCheckBox
    Left = 193
    Top = 55
    Width = 21
    Height = 17
    TabOrder = 2
  end
  object OKButton: TButton
    Left = 339
    Top = 60
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 3
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 420
    Top = 60
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = CancelButtonClick
  end
  object BrowseButton: TButton
    Left = 472
    Top = 19
    Width = 23
    Height = 23
    Caption = #9642#9642#9642
    TabOrder = 1
    OnClick = BrowseButtonClick
  end
end
