object HelpForm: THelpForm
  Left = 0
  Top = 0
  Caption = 'OvbImgOrganizer - Help'
  ClientHeight = 297
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    635
    297)
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 552
    Top = 266
    Width = 79
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    TabOrder = 0
    OnClick = OKButtonClick
  end
  object RichEdit1: TRichEdit
    Left = 4
    Top = 4
    Width = 627
    Height = 256
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'RichEdit1')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    Zoom = 100
  end
end
