object SlideShowForm: TSlideShowForm
  Left = 0
  Top = 0
  ClientHeight = 468
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = SlideShowPopupMenu
  Position = poDesigned
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 417
    Width = 635
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    OnMouseActivate = ToolsPanelMouseActivate
    OnResize = ToolsPanelResize
    object ButtonPanel: TPanel
      Left = 100
      Top = 6
      Width = 421
      Height = 41
      TabOrder = 0
      object NextBitBtn: TBitBtn
        Left = 227
        Top = 4
        Width = 76
        Height = 33
        Hint = 'Next'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE45D01ED6606EE6503FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          DD5A00F66F09F97814F06908FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD65601F46D07FF7708FC7C16F37412F0
          6401FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          CE5000E46A11FD821EFF7306FF7B12F97F1BF26A08FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDC6C22F88D3BFF7B16FF
          7A0DFC8724F67515F36502FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFD56018EB8B48FE8224FF760AFE8B27F88423F46606FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDE
          6314F96D05FF7000FF8117FDAC60F79143F76602FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFC74900DB5F0BFA750FFF801CFFBD7DFACCA2EF84
          3AF16100FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC14C0BE57C2DFE
          9437FFB064FDD9AFEDAF7FE05D12FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFA63200C45F24F2A65EFFB86CFFD39EF6D2AFDB7E48D04902FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9F3302CD814CFDD396FFD89EFEE4BFE5
          B08CC45318FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          9A330AE3BA88FFFED7F4D9BEC97C56B13802FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9A340CE1BB9AE2C4ACAB4A22FFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          9A3505A74D259B3611FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = NextBitBtnClick
      end
      object PreviousBitBtn: TBitBtn
        Left = 119
        Top = 4
        Width = 75
        Height = 33
        Hint = 'Previous'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFED6303F56C06F76A01FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE46208F87814FA72
          0AF26500FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFCB4F01E36A12FB7D18FF7608F96E06EA6201FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC65107EB771CFE7D14FF7307FE821EEC6E
          13E25C00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAB3B01CD5E14F5
          8524FF7E10FF7C14F58C39DB6E24FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFA83A05DC7523FD8E29FF7A0CFD8121E28649C85A1BFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF962E02BA6D40F3A961FF861AFF7201F5
          6B03C35312B6460EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFB25F37ECC5A0FFC382FF841FF6740EC2510DAE3B00FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF98320FCE9A7BFBDBB0FF
          B568FD9437DC782DB5460CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF902400B26443ECCDADFFD7A1FFBA6DF1A65FC56227B039
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9C
          3A15D5A488FDE4C0FFD99EFDD496D5884FB33F03FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF922602B86F51F1D6BBFFFFD8E9BF
          8BAF3F0AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFA0411EDFBEA8E7C09DA93C0DFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9A350FA84E
          24A23905FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = PreviousBitBtnClick
      end
      object FullScreenBitBtn: TBitBtn
        Left = 194
        Top = 4
        Width = 33
        Height = 33
        Hint = 'Full screen'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          C38944D7814CC28232C28232C28232C28232C28232C28232C28232C28232D781
          4CC38944FFFFFFFFFFFFFFFFFFFFFFFFC18946DF8E21DE8E21DE8E21DE8E21DE
          8E21DE8E21DE8E21DE8E21DE8E21DF8E21C18946FFFFFFFFFFFFFFFFFFFFFFFF
          C08846DC8A24D77D0FD77D0FD77D0FD77D0FD77D0FD77D0FD77D0FD77D0FDC8A
          24C08846FFFFFFFFFFFFFFFFFFFFFFFFBF8546D88524D3770FD3770FD3770FD3
          770FD3770FD3770FD3770FD3770FD88524BF8546FFFFFFFFFFFFFFFFFFFFFFFF
          BD8445D47F23CE6F0DCE6F0DCE6F0DCE6F0DCE6F0DCE6F0DCE6F0DCE6F0DD47F
          23BD8445FFFFFFFFFFFFFFFFFFFFFFFFBC8244D37C21CD6E0DCD6E0DCD6E0DCD
          6E0DCD6E0DCD6E0DCD6E0DCD6E0DD37C21BC8244FFFFFFFFFFFFFFFFFFFFFFFF
          BA8044E19126E28E14E28E14E28E14E28E14E28E14E28E14E28E14E28E14E191
          26BA8044FFFFFFFFFFFFFFFFFFFFFFFFB87E44E79C28ED9F18ED9F18ED9F18ED
          9F18ED9F18ED9F18ED9F18ED9F18E79C28B87E44FFFFFFFFFFFFFFFFFFFFFFFF
          B77B43EDA42AF6AC1BF6AC1BF6AC1BF6AC1BF6AC1BF6AC1BF6AC1BF6AC1BEDA4
          2AB77B43FFFFFFFFFFFFFFFFFFFFFFFFB67A42F3AC2BFFBA1EFFBA1EFFBA1EFF
          BA1EFFBA1EFFBA1EFFBA1EFFBA1EF3AC2BB67A42FFFFFFFFFFFFFFFFFFFFFFFF
          B47742F3AC29FFBF1CFFBF1CFFBF1CFFBF1CFFBF1CFFBF1CFFBF1CFFBF1CF3AC
          29B47742FFFFFFFFFFFFFFFFFFFFFFFFB37541BD712FBD712FBD712FBD712FBD
          712FBD712FBD712FBD712FBD712FBD712FB37541FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = FullScreenBitBtnClick
      end
      object RotateLeftBitBtn: TBitBtn
        Left = 307
        Top = 4
        Width = 33
        Height = 33
        Hint = 'Rotate left (Counter clockwise)'
        Glyph.Data = {
          F6060000424DF606000000000000360000002800000018000000180000000100
          180000000000C0060000120B0000120B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFF9F6F5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA87B5D9C6946FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFB8A7955B31975E37
          C6A995FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE2D5CD8F
          562CB8855BB078508F552CD2C2B7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFEDE8E5905831B17C52B88457AA7042A871478D552DD2CBC7FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFBFAF99F6E4DA77147BB875DB27747A46534AE774BA16A409663
          3FDFDDDCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFBA977E9E663EBC895FB47B4CB17747A46534
          A56737B37E5599623BAE866CF3F3F3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD7C1B3955E36BC8A61BE8D65BF
          8E66B17747A46534AC7346B58159B5825A945C34CFB7A7FFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEADFD8874A20874A
          20874A20884B22BE8E66B27949A46534B27D5491572E874A20874A20874A20E6
          D9D1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFF8F4F28B5128C08F68B3794AA66736B481598D512AF7F2
          EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAF8F6A1704FAF8467FFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8A4F26BE8D65B57C4EA86B3B
          B37E55955C36ECE2DBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAB8163975B
          30894D24FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9E6C49B3
          7E55B88257AD7141AC7246A9744C9C6946F1EAE5FFFFFFFFFFFFFFFFFFF0E9E4
          AC806192592FA165379A6742FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFCFB5A49E673FBF8F66B27849A56836B48057A8744E9056309D6A47A2
          714F97603C935B33A8754FC297749A6139D0B9A9FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFAF7F5935C34B27F57BC8960AE7343A56836B079
          4EB78660B68864B48662B88966BC8D68C69B79B38560A27150FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE0D1C68F562EB4825A
          C09069B57D50A96C3CA86A3AAA6C3CB07649B9865CC59975AE7F5A9A6641F5F0
          EDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFDAC7BA8D552CA26C44B88761C39774C59975C69B78C09471B2825C97603A
          B38A70F9F6F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFF6F1EEC0A08A99643F894E248C51278A4E259A
          6641BD9B82EDE3DDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFBF9F8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = RotateLeftBitBtnClick
      end
      object RotateRightBitBtn: TBitBtn
        Left = 339
        Top = 4
        Width = 33
        Height = 33
        Hint = 'Rotate right (clockwise)'
        Glyph.Data = {
          F6060000424DF606000000000000360000002800000018000000180000000100
          180000000000C0060000120B0000120B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9F6F5FF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF9C6946A87B5DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFC6A995975E37955B31CFB8A7FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFD2C2B78F552CB07850B8855B8F562CE2D5CD
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD2CBC78D552DA87147AA7042B8
          8457B17C52905831EDE8E5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDDDC96633FA16A
          40AE774BA46534B27747BB875DA771479F6E4DFBFAF9FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3F3F3
          AE866C99623BB37E55A56737A46534B17747B47B4CBC895F9E663EBA977EFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFCFB7A7945C34B5825AB58159AC7346A46534B17747BF8E66BE8D65
          BC8A61955E36D7C1B3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFE6D9D1874A20874A20874A2091572EB27D54A46534B2
          7949BE8E66884B22874A20874A20874A20EADFD8FFFFFFFFFFFFFFFFFFFFFFFF
          AF8467A1704FFAF8F6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F2EF8D51
          2AB48159A66736B3794AC08F688B5128F8F4F2FFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF894D24975B30AB8163FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFECE2DB955C36B37E55A86B3BB57C4EBE8D658A4F26FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9A6742A1653792592FAC8061F0E9E4FF
          FFFFFFFFFFFFFFFFF1EAE59C6946A9744CAC7246AD7141B88257B37E559E6C49
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD0B9A99A6139C297
          74A8754F935B3397603CA2714F9D6A47905630A8744EB48057A56836B27849BF
          8F669E673FCFB5A4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFA27150B38560C69B79BC8D68B88966B48662B68864B78660B0794EA568
          36AE7343BC8960B27F57935C34FAF7F5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFF5F0ED9A6641AE7F5AC59975B9865CB07649AA6C3C
          A86A3AA96C3CB57D50C09069B4825A8F562EE0D1C6FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9F6F4B38A7097603AB2
          825CC09471C69B78C59975C39774B88761A26C448D552CDAC7BAFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFEDE3DDBD9B829A66418A4E258C5127894E2499643FC0A08AF6F1EEFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBF9F8FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = RotateRightBitBtnClick
      end
      object FlipVerticalBitBtn: TBitBtn
        Left = 49
        Top = 4
        Width = 33
        Height = 33
        Hint = 'Flip vertical (Top/Bottom)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        Glyph.Data = {
          F6060000424DF606000000000000360000002800000018000000180000000100
          180000000000C0060000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFF1EAE5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFAF9F8A57045BA9271E5D5C9FFFEFEFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBF9F8BA8F6CEAD7C7C6A181B88E6ADF
          CCBCFDFCFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFAF9BC916DF2E1
          D3F0DDCDEAD2BFCCA687B98C68DAC2AFFBF8F6FFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FCFBFABE936FF1DED0EEDAC8ECD5C2EBD2BEE9CFB9D2AF90BC8F6BD5BBA4F8F3
          EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFCFCFBC19672F0DCCCEDD7C4EBD2BDEACFB8E9CDB6EACFB9
          EBD2BDDBBBA1C29774D2B399F4ECE6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFCFCC39874EFD0B9EE985BEAD0B9E8
          CAB2E8CAB3EBD3BFEFDBCBF2E2D4F4E7DDE7D4C3CBA586D0AD91F0E5DCFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFDC98B5BEE75
          21EF6E14EDB991EEDACAF1DED0F2E1D4F3E5D9F5E8DDF6EAE2F8EEE7F8F1EBF0
          E3D8D3B398CEA98BECDDD1FFFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          F7CDAFED6E16EF6D11F38A42F4A066F3E2D4F3E3D5F3E5D8F5E7DCF6EAE1F6ED
          E5F8F0E9F9F3EDFAF6F1FCF8F6F7F0EADCC2ABCFA888E8D6C6FDFCFCFFFFFFFF
          FFFFFFFEFDF7B78AF0741DF0721AF17824F5995AF7A973EEBA94E7CFBBE7D0BD
          E8D2C0E9D4C2EAD6C6EAD8C9EBDBCDECDCCFEDDED2EEE1D5F0E3D8ECDED1D5B3
          96CBA17DE5CFBDFDFCFBF6AE7BF17A27F17A26F28132F3873BF59E61F8B07EF0
          A874E4C6ADE1C8B4E1C8B4E1C9B4E1C8B4E1C9B5E3C9B6E2CAB5E2CAB6E2CAB6
          E3CBB6E3CBB7E3CBB7E3CBB7E4CCB8FBF7F4FBDAC2F7B486FCE3D1F7B281F491
          4BF7BA8EF7B382FBC7A1F9D3B7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8CBA7
          F29B57F38F48F49450F29B58FDF3EAF9C098F6AD78FFFEFDF8CAA6F19954FFFE
          FDF8CAA6F19954FFFEFDF8CAA6F19954FFFEFDF8CAA6F19954FFFEFDF8CAA6F1
          9954FFFFFFFFFFFFFCF3EDF38B42F5A269FFFFFFFFFFFFFDFBFAFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8EBE2F4914CDD9A6CABA6A5A8A5A5A7
          A4A4A6A3A3A6A3A3A4A2A2A4A0A0A3A0A0A29F9FA19E9EA19D9D9F9C9C9E9B9B
          9E9B9B9E9A9AA09C9CA3A0A0A6A2A2F2F1F1FFFFFFFFFFFFFAF1EAF59654F4B3
          85F2F2F2F2F2F2F2F2F2F2F2F2F2F2F2F2F2F2F1F1F1F1F1F1F1F1F1F1F1F1F1
          F1F1F1F1F1F0F0F0F0F0F0D7D6D6A9A5A5B1ADADE6E5E5FFFFFFFFFFFFFFFFFF
          FEFEFEEF9B5EF6AB76F1EFEEF2F2F2F1F1F1F1F1F1F0F0F0EFEFEFEFEFEFEFEF
          EFEFEFEFEFEFEFEFEFEFEDEDEDCDCBCBA19E9EBAB6B6EEEEEEFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFD2976EF8AF7CF0DBCBEFEFEFF1F1F1F1F1F1F2F2F2
          EFEFEFEDEDEDEDEDEDEDEDEDEAEAEAC3C2C29E9A9AC3C0C0F5F5F5FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEAA9080F8B07DF3CCAFECECECEE
          EEEEEFEFEFF1F1F1F2F2F2F0F0F0E3E3E3B9B7B79D9999CCCACAFAFAFAFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFE9E9B9BECDB
          D0E9E9E9E9E9E9E9E9E9E9E9E9EDEDEDE2E2E2B2B0B0A19D9DD5D4D4FDFDFDFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FEFEFE9C9A9AE6E6E6E6E6E6E5E5E5E5E5E5D2D2D2A6A2A2A8A4A4DFDEDEFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFDFDFD9A9797E2E2E2E3E3E3C8C7C79E9A9AAFADADE8E7E7
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFCFC969494C0BEBE999595B9B7B7F0
          F0F0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDFD908C8CC3C0
          C0F6F6F6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        OnClick = FlipVerticalBitBtnClick
      end
      object FlipHorizBitBtn: TBitBtn
        Left = 81
        Top = 4
        Width = 33
        Height = 33
        Hint = 'Flip horizontal (Left/Right)'
        Glyph.Data = {
          F6060000424DF606000000000000360000002800000018000000180000000100
          180000000000C0060000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBDA
          C2F6AE7BFFFEFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFF8CBA7F7B486F17A27F7B78AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFDFDFDFCFCFCFDFDFDFEFEFEFEFEFEFEFEFEFFFFFFFE
          FEFEFAF1EAF8EBE2FCF3EDF29B57FCE3D1F17A26F0741DF7CDAFFEFDFDFDFCFC
          FCFCFBFCFBFAFBFAF9FBF9F8FAF9F8FFFFFF908C8C9694949A97979C9A9A9E9B
          9BAA9080D2976EEF9B5EF59654F4914CF38B42F38F48F7B281F28132F0721AED
          6E16C98B5BC39874C19672BE936FBC916DBA8F6CA57045F1EAE5C3C0C0C0BEBE
          E2E2E2E6E6E6ECDBD0F8B07DF8AF7CF6AB76F4B385DD9A6CF5A269F49450F491
          4BF3873BF17824EF6D11EE7521EFD0B9F0DCCCF1DED0F2E1D3EAD7C7BA9271FF
          FFFFF6F6F6999595E3E3E3E6E6E6E9E9E9F3CCAFF0DBCBF1EFEEF2F2F2ABA6A5
          FFFFFFF29B58F7BA8EF59E61F5995AF38A42EF6E14EE985BEDD7C4EEDAC8F0DD
          CDC6A181E5D5C9FFFFFFFFFFFFB9B7B7C8C7C7E5E5E5E9E9E9ECECECEFEFEFF2
          F2F2F2F2F2A8A5A5FFFFFFFDF3EAF7B382F8B07EF7A973F4A066EDB991EAD0B9
          EBD2BDECD5C2EAD2BFB88E6AFFFEFEFFFFFFFFFFFFF0F0F09E9A9AE5E5E5E9E9
          E9EEEEEEF1F1F1F1F1F1F2F2F2A7A4A4FDFBFAF9C098FBC7A1F0A874EEBA94F3
          E2D4EEDACAE8CAB2EACFB8EBD2BECCA687DFCCBCFFFFFFFFFFFFFFFFFFFFFFFF
          AFADADD2D2D2E9E9E9EFEFEFF1F1F1F1F1F1F2F2F2A6A3A3FFFFFFF6AD78F9D3
          B7E4C6ADE7CFBBF3E3D5F1DED0E8CAB3E9CDB6E9CFB9B98C68FDFCFCFFFFFFFF
          FFFFFFFFFFFFFFFFE8E7E7A6A2A2EDEDEDF1F1F1F2F2F2F0F0F0F2F2F2A6A3A3
          FFFFFFFFFEFDFFFFFFE1C8B4E7D0BDF3E5D8F2E1D4EBD3BFEACFB9D2AF90DAC2
          AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA8A4A4E2E2E2F2F2F2EFEFEFEF
          EFEFF2F2F2A4A2A2FFFFFFF8CAA6FFFFFFE1C8B4E8D2C0F5E7DCF3E5D9EFDBCB
          EBD2BDBC8F6BFBF8F6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDEDEB2B0
          B0F0F0F0EDEDEDEFEFEFF1F1F1A4A0A0FFFFFFF19954FFFFFFE1C9B4E9D4C2F6
          EAE1F5E8DDF2E2D4DBBBA1D5BBA4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFA19D9DE3E3E3EDEDEDEFEFEFF1F1F1A3A0A0FFFFFFFFFEFDFFFF
          FFE1C8B4EAD6C6F6EDE5F6EAE2F4E7DDC29774F8F3EFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFD5D4D4B9B7B7EDEDEDEFEFEFF1F1F1A29F9F
          FFFFFFF8CAA6FFFFFFE1C9B5EAD8C9F8F0E9F8EEE7E7D4C3D2B399FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDFD9D9999EAEAEAEF
          EFEFF1F1F1A19E9EFFFFFFF19954FFFFFFE3C9B6EBDBCDF9F3EDF8F1EBCBA586
          F4ECE6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFCCCACAC3C2C2EFEFEFF1F1F1A19D9DFFFFFFFFFEFDFFFFFFE2CAB5ECDCCFFA
          F6F1F0E3D8D0AD91FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFAFAFA9E9A9AEDEDEDF1F1F19F9C9CFFFFFFF8CAA6FFFF
          FFE2CAB6EDDED2FCF8F6D3B398F0E5DCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3C0C0CDCBCBF0F0F09E9B9B
          FFFFFFF19954FFFFFFE2CAB6EEE1D5F7F0EACEA98BFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5F5F5A1
          9E9EF0F0F09E9B9BFFFFFFFFFEFDFFFFFFE3CBB6F0E3D8DCC2ABECDDD1FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFBAB6B6D7D6D69E9A9AFFFFFFF8CAA6FFFFFFE3CBB7ECDED1CF
          A888FFFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEEEEEEA9A5A5A09C9CFFFFFFF19954FFFF
          FFE3CBB7D5B396E8D6C6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB1ADADA3A0A0
          FFFFFFFFFEFDFFFFFFE3CBB7CBA17DFDFCFCFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFE6E5E5A6A2A2FFFFFFF8CAA6FFFFFFE4CCB8E5CFBDFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFF2F1F1FFFFFFF19954FFFFFFFBF7F4FDFCFBFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        OnClick = FlipHorizBitBtnClick
      end
      object ZoomRealSizeBitBtn: TBitBtn
        Left = 378
        Top = 4
        Width = 33
        Height = 33
        Hint = 'Zoom real size'
        Glyph.Data = {
          F6060000424DF606000000000000360000002800000018000000180000000100
          180000000000C0060000120B0000120B00000000000000000000C7C7C8C7C7C8
          C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7
          C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7
          C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8
          C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7
          C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7
          C7C8C7C7C8C7C7C8C8C7C8D0B8A8C1A18AC7C7C8C7C7C8C7C7C8C7C7C8C7C7C8
          C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7
          C8C7C7C8C7C7C8C7C7C8C7C7C8CAC8C7E3D4CB90552D97603BE0D1C6C7C7C8C7
          C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8
          C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8A27352AE794EB682
          5799643FC7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7
          C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C6AB99
          874A20874A20874A20874A20BB9B85C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7
          C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8EEEEEEDCDCDCDD
          DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD9D9D9
          D8D8D8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7
          C8EDEDED9797978D8D8D8D8D8D8D8D8D8D8D8D8D8D8D8D8D8D8D8D8D8D8D8D8D
          8D8D8D8D8D979797D6D6D6C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8
          C7C7C8C7C7C8C7C7C8FBFBFB8D8D8DEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
          EEEEEEEEEEEEEEEEEEEEEEEEEE8D8D8DEDEDEDC7C7C8C7C7C8C7C7C8C7C7C8C7
          C7C8C7C7C8C7C7C8C7C7C8E0D1C69D6B48FFFFFF8D8D8DF0F0F0E4E4E4E4E4E4
          E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4F0F0F08D8D8DFFFFFF8B5028CCB2
          A0C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C1A18A96613B874B21FFFFFF8D8D8DF3
          F3F3E9E9E9E9E9E9E9E9E9E9E9E9E9E9E9E9E9E9E9E9E9E9E9E9F3F3F38D8D8D
          FFFFFF874A209A623AAD8163E3D6CCC7C7C8C7C7C8AB806290562DC79669874A
          20FFFFFF8D8D8DF6F6F6EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
          EEEEF5F5F58D8D8DFFFFFF874A20C7966990562DA97C5EC7C7C8C7C7C8E3D6CC
          AA7D5F9A623A874A20FFFFFF8D8D8DF8F8F8F3F3F3F3F3F3F3F3F3F3F3F3F3F3
          F3F3F3F3F3F3F3F3F3F3F8F8F88D8D8DFFFFFF874A208E542CC1A18AC7C7C8C7
          C7C8C7C7C8C7C7C8C7C7C8CCB2A08E542DFFFFFF8D8D8DFBFBFB9E6E4C9E6E4C
          9E6E4C9E6E4C9E6E4C9E6E4C9E6E4C9F704EFBFBFB8D8D8DFFFFFF996541E6D9
          D1C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8E0D1C7FDFDFC8D8D8DFD
          FDFDB9967DB9967DB9967DB9967DB9967DB9967DB9967DBA977FFDFDFD8D8D8D
          FFFFFFC7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7
          C8FFFFFF8D8D8DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFF8D8D8DFFFFFFC7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8
          C7C7C8C7C7C8C7C7C8FFFFFF9E9E9E8D8D8D8D8D8D8D8D8D8D8D8D8D8D8D8D8D
          8D8D8D8D8D8D8D8D8D8D8D8D8D9E9E9EFFFFFFC7C7C8C7C7C8C7C7C8C7C7C8C7
          C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7C7C8C7C7
          C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7
          C7C8C7C7C8B1896E874A20874A20874A20874A20DDCBBFC7C7C8C7C7C8C7C7C8
          C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7
          C8C7C7C8C7C7C8C7C7C8C7C7C8E1D2C890572FBF8D60A56F46B58E74C7C7C8C7
          C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8
          C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8D6C1B28F542A945C
          35E1D3C8C9C8C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7
          C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8
          C7C7C8AF866ADFCFC4C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7
          C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7
          C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8
          C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7
          C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7
          C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8C7C7C8}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        OnClick = ZoomRealSizeBitBtnClick
      end
      object ZoomBitBtn: TBitBtn
        Left = 11
        Top = 4
        Width = 33
        Height = 33
        Hint = 'Show zoom slider'
        Glyph.Data = {
          F6060000424DF606000000000000360000002800000018000000180000000100
          180000000000C0060000D70D0000D70D00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF5C605E696C6BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF6064629FA2A0737876595D5BFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF717472B6B8B7848A87777A
          79595D5BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          666A68BCBFBD848A87777A79595D5BFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFFFF
          FFFFFFFFFFFFFFFFFFFFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFF666A68BCBFBD848A87787B7A5A5E5CFFFFFFFFFFFF
          FFFFFFCDCFCDA5A8A68D918F8D918FA3A6A4C3C5C3FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF666A68BCBFBDB6BCB987
          8D8AA8AAA9A4A7A59FA29FD2CFCBE2E1DEEBEBEAEBEBEAE2E1DED2CFCAA0A29F
          AEB1AEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FF676967A6AAA89FA5A2898D8ACBC7C1EBEBE9EAE2D8EADBCDE8D6C5E8D6C4EA
          DBCDE9E1D7EBEBE9CBC6C0929593FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF898D8AE2DED9EAE5DFE5CEB9E4CAB2E5CB
          B3E5CBB3E5CBB3E4CAB2E4C9B0E4CEB7EAE4DEE1DED8929592FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB0B2AFC9C3BBE9E3DCE0C1A5
          E0C2A6E1C3A8D2AD8CBB8960BB8960D2AC8BE0C2A6E0C1A4DFC0A2E9E2DBC9C3
          BBB0B1AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9FA09DEA
          E9E7DDC0A4DCBA9ADDBB9CDDBC9DBA875CF5EBE2F5EBE2BA865CDDBB9CDCBA9A
          DBB898DCBEA2EAE9E69FA09DF5F6F5FDFDFDFFFFFFFFFFFFFFFFFFFFFFFFFCFC
          FCCDCECDCDC6BCE2D3C3D8B28ED9B390D9B491DAB593B47D51F4E9DFF4E9DFB4
          7D51D9B491D9B390D8B28ED7B08BE2D2C1CDC5BCD5D6D5F7F8F7FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFA9AAA7DED8CFDDC0A5D5AB84C99B71B78155B37C4FB47D
          50F5EAE0F5EBE1B57F54B47E52B88357CA9B72D4A981DCBFA4DED7CFA6A7A3FF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8E918DE9E7E4D3AC87D1A57AB78255
          F5EAE1F6EDE4F6EDE4F6EEE6F7EEE6F7EFE7F7EFE7F7EFE7BC8A62E0C2A6D8B6
          95E9E7E48E918DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8E918DE9E7E4D5
          AF8BDAB593B9865BF5EBE2F5ECE3F6EDE5F6EEE6F7EEE7F7EFE7F7EFE7F7EFE8
          BC8B63E4C9B0D9B796E9E7E48E918DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFA6A7A2DDD6CDDBBDA1DDBA9AD0A784BB885EB68157B78258F7EFE7F7F0E9B8
          845CB8855CBD8D65D6B497E6CDB5DCBFA3DDD6CDA7A8A3FFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFC6C8C6CBC2B7DFCBB8DDBC9CE0C1A3E1C4A8E3C7ADB783
          5AF8F0EAF8F1EBB8865EEAD5C1E9D4BFE8D1BCE4CAB1DFCBB8CBC2B7CACCCAF9
          F9F9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFB9F9F9AE9E7E4D8B593E1C2A6
          E2C6ABE4C9B0BD8C64F8F1EAF8F2ECBE8F69EBD6C3E9D3BFE8D0BAD9B695E9E7
          E49F9F9AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6
          BDB2E6DBD0DAB794E3C6ACE4C9B0D6B294BD8D65BD8D66D7B69AE8D1BBE7CFB8
          DCBA99E6DBD0C6BDB2AFB1AEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF919490DED7CEE6DBD0D9B695E2C5A9E4CAB0E5CBB3E5CCB4E5
          CCB4E3C7ADD9B695E6DBD0DED7CE919490FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF919490C6BDB2E9E7E4DFCCB8DCBF
          A4D9B898D9B897DCBFA3DFCCB8E9E7E4C6BDB2919490FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFB1AE
          9F9F9ACBC2B7DDD6CDE9E7E4E9E7E4DDD6CDCBC2B79F9F9AAFB1AEFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFC1C3C1A6A7A28E918D8E918DA6A7A2C6C8C6FDFDFD
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFCFCFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        OnClick = ZoomBitBtnClick
      end
    end
  end
  object ShowPanel: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 417
    Align = alClient
    BevelOuter = bvNone
    Color = clMedGray
    ParentBackground = False
    TabOrder = 1
  end
  object SlideShowPopupMenu: TPopupMenu
    Left = 212
    Top = 216
    object NextPopupMnu: TMenuItem
      Caption = 'Next'
      OnClick = NextPopupMnuClick
    end
    object PreviousPopupMnu: TMenuItem
      Caption = 'Previous'
      OnClick = PreviousPopupMnuClick
    end
    object FullScreenPopupMnu: TMenuItem
      Caption = 'Full screen'
      OnClick = FullScreenPopupMnuClick
    end
    object SlowSpeedPopupMnu: TMenuItem
      Caption = 'Slow speed'
      OnClick = SlowSpeedPopupMnuClick
    end
    object MediumSpeedPopupMnu: TMenuItem
      Caption = 'Medium speed'
      OnClick = MediumSpeedPopupMnuClick
    end
    object HighSpeedPopupMnu: TMenuItem
      Caption = 'High speed'
      OnClick = HighSpeedPopupMnuClick
    end
    object SuspendPopupMnu: TMenuItem
      Caption = 'Suspend'
      OnClick = SuspendPopupMnuClick
    end
    object ShowInExplorerPopupMnu: TMenuItem
      Caption = 'Show in Explorer'
      OnClick = ShowInExplorerPopupMnuClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ZoomFitWindowPopupMnu: TMenuItem
      Caption = 'Zoom to fit window'
      OnClick = ZoomFitWindowPopupMnuClick
    end
    object ZoomRealSizePopupMnu: TMenuItem
      Caption = 'Zoom real size'
      OnClick = ZoomRealSizePopupMnuClick
    end
    object PanCenterPopupMnu: TMenuItem
      Caption = 'Pan center'
      OnClick = PanCenterPopupMnuClick
    end
    object RotateRightPopupMnu: TMenuItem
      Caption = 'Rotate right'
      OnClick = RotateRightPopupMnuClick
    end
    object RotateLeftPopupMnu: TMenuItem
      Caption = 'Rotate left'
      OnClick = RotateLeftPopupMnuClick
    end
    object FlipHorizontalPopupMnu: TMenuItem
      Caption = 'Flip horizontal'
      OnClick = FlipHorizontalPopupMnuClick
    end
    object FlipVerticalPopupMnu: TMenuItem
      Caption = 'Flip vertical'
      OnClick = FlipVerticalPopupMnuClick
    end
  end
  object SlideShowTimer: TTimer
    Enabled = False
    OnTimer = SlideShowTimerTimer
    Left = 380
    Top = 216
  end
end