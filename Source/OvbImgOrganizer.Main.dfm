object OvbImgOrganizerMainForm: TOvbImgOrganizerMainForm
  Left = 2111
  Top = 115
  Caption = 'OverByte Image Organizer'
  ClientHeight = 770
  ClientWidth = 818
  Color = clBtnFace
  CustomTitleBar.Control = TitleBarPanel1
  CustomTitleBar.Enabled = True
  CustomTitleBar.Height = 31
  CustomTitleBar.ShowCaption = False
  CustomTitleBar.BackgroundColor = clWhite
  CustomTitleBar.ForegroundColor = 65793
  CustomTitleBar.InactiveBackgroundColor = clWhite
  CustomTitleBar.InactiveForegroundColor = 10066329
  CustomTitleBar.ButtonForegroundColor = 65793
  CustomTitleBar.ButtonBackgroundColor = clWhite
  CustomTitleBar.ButtonHoverForegroundColor = 65793
  CustomTitleBar.ButtonHoverBackgroundColor = 16053492
  CustomTitleBar.ButtonPressedForegroundColor = 65793
  CustomTitleBar.ButtonPressedBackgroundColor = 15395562
  CustomTitleBar.ButtonInactiveForegroundColor = 10066329
  CustomTitleBar.ButtonInactiveBackgroundColor = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  GlassFrame.Enabled = True
  GlassFrame.Top = 31
  KeyPreview = True
  OldCreateOrder = False
  StyleElements = [seFont, seClient]
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object DisplaySplitter: TSplitter
    Left = 0
    Top = 607
    Width = 818
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 41
    ExplicitWidth = 354
  end
  object CollectionRightSplitter: TSplitter
    Left = 691
    Top = 63
    Width = 4
    Height = 544
    Align = alRight
    ExplicitLeft = 692
    ExplicitTop = 33
    ExplicitHeight = 554
  end
  object TopPanel: TPanel
    Left = 0
    Top = 30
    Width = 818
    Height = 33
    Hint = 'Help'
    Align = alTop
    DoubleBuffered = True
    ParentBackground = False
    ParentColor = True
    ParentDoubleBuffered = False
    TabOrder = 0
    DesignSize = (
      818
      33)
    object Label2: TLabel
      Left = 387
      Top = 9
      Width = 54
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Order by'
    end
    object CollectionSearchEdit: TEdit
      Left = 4
      Top = 6
      Width = 301
      Height = 21
      Hint = 'Enter tags separated by a space. Leave empty to view all.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'CollectionSearchEdit'
    end
    object CollectionSearchBitBtn: TBitBtn
      Left = 308
      Top = 5
      Width = 75
      Height = 23
      Caption = 'Search'
      Glyph.Data = {
        26040000424D2604000000000000360000002800000012000000120000000100
        180000000000F0030000C30E0000C30E00000000000000000000FAF3EEF7F1EC
        C7C1BDBAB6B2F8F1ECFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3
        EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EE0000FAF3EEBDB7B46D6C6B605F5F918D
        8AF8F1ECFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFA
        F3EEFAF3EEFAF3EE0000FAF3EE9996937172738B8C8C54535385827FF7F0EBFA
        F3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EE
        0000FAF3EEF3EDE87C7A787C7C7C787878494847817E7BF7F0EBFBF4EFFAF3EE
        FAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EE0000FAF3EEFAF3EE
        F8F1EC7F7C7A6868686364643C3C3C767271F5EEE9FAF3EEFAF3EEFAF3EEFBF4
        EFFAF4EFFBF4EFFAF3EEFAF3EEFAF3EE0000FAF3EEFAF3EEFAF3EEFAF3EE8784
        825A5A5A39393A09090A6B6865ECE5E0A29E9B777471797573A49F9BF6EFEAFA
        F4EFFCF5EFFAF3EE0000FAF3EEFAF3EEFAF3EEFAF3EEF9F2ED928E8B2B2B2B00
        00000706062524220C0C0C0201020102020D0D0C4A4745B9B4B0FCF5F0FAF3EF
        0000FAF3EEFAF3EEFAF3EEFAF3EEFAF3EEF9F2ED979491131413000000000000
        4F4C4B8985837D7977383736000000242323BCB6B2FAF4EF0000FAF3EEFAF3EE
        FAF3EEFAF3EEFAF3EEFAF3EEF5EEE92D2C2B040404868380F1EAE6F7F0ECE6E1
        DEBCB8B66767661718195C5A57F6EFEA0000FAF3EEFAF3EEFAF3EEFAF3EEFAF3
        EEFAF3EEB1ACA80B0A0A4E4C4AEAE3DFF8F1EDDFDAD7D5D1CFD6D3D1C8C6C480
        7F80494848B7B1AE0000FAF3EEFAF3EEFAF3EEFAF3EEFBF4EFFBF4EF85817E00
        0000817E7CEAE5E0D8D4D1CCC9C7C8C6C4CAC8C8D9D8D7C8C8C766666697928F
        0000FAF3EEFAF3EEFAF3EEFAF3EEFBF4EFFBF3EE817D7B000000757472D9D6D2
        CDCBC9C8C6C5CAC8C7D0D0CFE2E1E0E2E2E27F7F7F938F8D0000FAF3EEFAF3EE
        FAF3EEFAF3EEFAF3EEFAF4EF9F9A97010101545352CFCCCACECCCBCCCAC9D3D2
        D2E0E0DFF4F4F4E5E5E5828181ABA6A30000FAF3EEFAF3EEFAF3EEFAF3EEFAF3
        EEFAF3EEDCD5D12C2B2B141315A09E9EDAD9D8DCDBDBE8E7E7F7F7F8F7F7F8BA
        BBBB868382D8D2CC0000FAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEA5
        A19E060606232424A8A8A8DFDFDFECECECE7E8E8BCBCBC818180B2ADAAFBF4EF
        0000FAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EE8B88851F1E1D
        4545448989889696968D8D8D7F7D7BA6A19EF3ECE7FAF3EE0000FAF3EEFAF3EE
        FAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEFBF4EFB0ABA75957554847464C4B
        49686765B7B1AEFAF2EDFBF4EFFAF3EE0000FAF3EEFAF3EEFAF3EEFAF3EEFAF3
        EEFAF3EEFAF3EEFAF3EEFAF3EEFAF3EEF2ECE7C8C3BEC8C2BEF3EDE7FAF3EEFB
        F4EFFAF3EEFAF3EE0000}
      TabOrder = 1
      OnClick = CollectionSearchBitBtnClick
    end
    object OrderByComboBox: TComboBox
      Left = 445
      Top = 5
      Width = 124
      Height = 21
      Hint = 'Select result ordering'
      Style = csDropDownList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object OrderByDescendingCheckBox: TCheckBox
      Left = 580
      Top = 6
      Width = 37
      Height = 17
      Hint = 'Check to have descending order'
      Caption = #9660
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object LoadMoreButton: TButton
      Left = 624
      Top = 4
      Width = 75
      Height = 23
      Caption = 'Load more'
      Enabled = False
      TabOrder = 4
      OnClick = LoadMoreButtonClick
    end
    object HelpBitBtn: TBitBtn
      Left = 786
      Top = 2
      Width = 29
      Height = 29
      Hint = 'Display help'
      Anchors = [akTop, akRight]
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFE2E0DCA9A39788806F675D47675D4788806FA9A397E2E0DCFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFCFCAFAA9E625741A19A8DCDC7B9EE
        EAE1EEEAE1CDC7B9A19A8D625741AFAA9EFCFCFCFFFFFFFFFFFFFFFFFFFEFEFD
        847C6A7C7361EAE5DDF5E3C0F5C979F1B74BF1B74BF5C979F5E3C0EAE5DD7C73
        61847C6AFEFEFDFFFFFFFFFFFFB2ACA17C7361F7F5EFF3C36AEDA217EDA31AF6
        D390F5CA7CEDA217EDA217F3C36AF7F5EF7C7361B2ACA1FFFFFFE2E0DC625741
        EAE5DDF3C36AEDA217EDA217F1B84EFFFFFFFFFFFEEEA725EDA217EDA217F3C3
        6AEAE5DD625741E2E0DCA9A397A19A8DF5E3C0EDA217EDA217EDA217EEA51EF7
        D699F5CB7DEDA217EDA217EDA217EDA217F5E3C0A19A8DA9A39788806FCDC7B9
        F5C979EDA217EDA217EDA217EDA217F7D392F4C56FEDA217EDA217EDA217EDA2
        17F5C979CDC6B888806F675D47EEEAE1F1B74BEDA217EDA217EDA217EEA826FF
        FFFEFBECD0EDA41CEDA217EDA217EDA217F1B74BEEEAE1675D47675D47EEEAE1
        F1B74BEDA217EDA217EDA217EDA217F6D391FFFFFFF9E1B4EDA41CEDA217EDA2
        17F1B74BEEEAE1675D4788806FCDC7B9F5C979EDA217EDA217EDA217EDA217ED
        A31AFAE7C3FFFFFFF5CA7AEDA217EDA217F5C979CDC6B888806FA9A397A19A8D
        F5E3C0EDA217EDA217F0B443F9E0B1F1B94FFAE3BAFFFFFFF5CC81EDA217EDA2
        17F5E3C0A19A8DA9A397E2E0DC625741EAE5DDF3C36AEDA217F4C46CFFFEFBFF
        FFFFFFFFFFFDF6E8EEA621EDA217F3C36AEAE5DD625741E2E0DCFFFFFFB2ACA1
        7C7361F7F5EFF3C36AEDA217EFAE35F5CA7BF3BF60EEA622EDA217F3C36AF7F5
        EF7C7361B2ACA1FFFFFFFFFFFFFEFEFD867E6C7C7361EAE5DDF5E3C0F5C979F1
        B74BF1B74BF5C979F5E3C0EAE5DD7C7361867E6CFEFEFDFFFFFFFFFFFFFFFFFF
        FEFEFDB2ACA1625741A19A8DCDC7B9EEEAE1EEEAE1CDC7B9A19A8D625741B2AC
        A1FEFEFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE2E0DCA9A39788806F67
        5D47675D4788806FA9A397E2E0DCFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = HelpBitBtnClick
    end
    object SlidShowBitBtn: TBitBtn
      Left = 754
      Top = 2
      Width = 29
      Height = 29
      Hint = 'Full screen mode'
      Anchors = [akTop, akRight]
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000F3F3F3F3F3F3
        F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3
        F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3
        F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3
        F3F3F3F3F3F3F3F3F3000000000000000000000000000000F3F3F3F3F3F3F3F3
        F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F300
        0000F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3
        F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3000000F3F3F3F3F3F3F3F3F3F3F3F3F3F3
        F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F300
        0000F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3
        BFBFBF000000000000000000000000000000000000000000000000000000BFBF
        BFF3F3F3F3F3F3F3F3F3F3F3F3F3F3F3000000FFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3
        000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
        00F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3000000FFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3
        000000FFFFFFE8A200E8A200E8A200E8A200E8A200E8A200E8A200FFFFFF0000
        00F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3000000FFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000F3F3F3F3F3F3F3F3F3F3F3F3000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000F3F3F3F3F3F3F3F3F300000000000000000000000000000000000000
        0000000000000000000000000000000000000000F3F3F3F3F3F3F3F3F3F3F3F3
        F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3
        F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3
        F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3}
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = SlidShowBitBtnClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 611
    Width = 818
    Height = 159
    TabStop = False
    Align = alBottom
    Constraints.MinHeight = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object RightPanel: TPanel
    Left = 695
    Top = 63
    Width = 123
    Height = 544
    Align = alRight
    BevelOuter = bvNone
    DoubleBuffered = False
    ParentDoubleBuffered = False
    TabOrder = 2
    object Splitter1: TSplitter
      Left = 0
      Top = 169
      Width = 123
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitWidth = 385
    end
    object CurrentImageTagsPanel: TPanel
      Left = 0
      Top = 0
      Width = 123
      Height = 169
      Align = alTop
      TabOrder = 0
      DesignSize = (
        123
        169)
      object CurrentImageTagsLabel: TLabel
        Left = 6
        Top = 2
        Width = 96
        Height = 13
        Caption = 'Current image tags:'
      end
      object ImageTagListBox: TListBox
        Left = 1
        Top = 19
        Width = 120
        Height = 150
        Hint = 'Focused image tags'
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        ParentShowHint = False
        PopupMenu = ImageTagListBoxPopupMenu
        ShowHint = True
        TabOrder = 0
        OnDblClick = ImageTagListBoxDblClick
        OnMouseDown = ImageTagListBoxMouseDown
      end
    end
    object AvailableTagsPanel: TPanel
      Left = 0
      Top = 172
      Width = 123
      Height = 372
      Align = alClient
      TabOrder = 1
      DesignSize = (
        123
        372)
      object Label1: TLabel
        Left = 8
        Top = 2
        Width = 71
        Height = 13
        Caption = 'Available tags:'
      end
      object TagsVirtualStringTree: TVirtualStringTree
        Left = 2
        Top = 20
        Width = 119
        Height = 350
        Anchors = [akLeft, akTop, akRight, akBottom]
        Colors.BorderColor = 15987699
        Colors.DisabledColor = clGray
        Colors.DropMarkColor = 15385233
        Colors.DropTargetColor = 15385233
        Colors.DropTargetBorderColor = 15987699
        Colors.FocusedSelectionColor = 15385233
        Colors.FocusedSelectionBorderColor = clWhite
        Colors.GridLineColor = 15987699
        Colors.HeaderHotColor = clBlack
        Colors.HotColor = clBlack
        Colors.SelectionRectangleBlendColor = 15385233
        Colors.SelectionRectangleBorderColor = 15385233
        Colors.SelectionTextColor = clBlack
        Colors.TreeLineColor = 9471874
        Colors.UnfocusedColor = clGray
        Colors.UnfocusedSelectionColor = 15385233
        Colors.UnfocusedSelectionBorderColor = 15385233
        DragOperations = [doMove]
        Header.AutoSizeIndex = 0
        Header.MainColumn = -1
        PopupMenu = TagTreePopupMenu
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoChangeScale]
        OnDragAllowed = TagsVirtualStringTreeDragAllowed
        OnDragOver = TagsVirtualStringTreeDragOver
        OnDragDrop = TagsVirtualStringTreeDragDrop
        OnExpanding = TagsVirtualStringTreeExpanding
        OnFreeNode = TagsVirtualStringTreeFreeNode
        OnGetText = TagsVirtualStringTreeGetText
        OnInitNode = TagsVirtualStringTreeInitNode
        OnMouseDown = TagsVirtualStringTreeMouseDown
        OnNodeDblClick = TagsVirtualStringTreeNodeDblClick
        Columns = <>
      end
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 63
    Width = 691
    Height = 544
    ActivePage = CollectionTabSheet
    Align = alClient
    TabOrder = 3
    object NewImageTabSheet: TTabSheet
      Caption = 'New Image'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object DropDialogSplitter: TSplitter
        Left = 441
        Top = 0
        Height = 516
        Align = alRight
        ExplicitLeft = 508
        ExplicitTop = 132
        ExplicitHeight = 100
      end
      object DropDialogPanel: TPanel
        Left = 444
        Top = 0
        Width = 239
        Height = 516
        Align = alRight
        PopupMenu = DropDialogPanelPopupMenu
        TabOrder = 0
        DesignSize = (
          239
          516)
        object DropImageCountLabel: TLabel
          Left = 8
          Top = 16
          Width = 107
          Height = 13
          Caption = 'DropImageCountLabel'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object ImageDropCancelButton: TButton
          Left = 8
          Top = 346
          Width = 109
          Height = 25
          Caption = 'Cancel'
          TabOrder = 2
          OnClick = ImageDropCancelButtonClick
        end
        object ImageDropAutoProcessCheckBox: TCheckBox
          Left = 8
          Top = 256
          Width = 221
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Auto process all remaining images'
          TabOrder = 5
        end
        object ImageDropAddToCollectionButton: TButton
          Left = 8
          Top = 284
          Width = 109
          Height = 25
          Caption = 'Add to collection'
          TabOrder = 0
          OnClick = ImageDropAddToCollectionButtonClick
        end
        object ImageDropSkipButton: TButton
          Left = 8
          Top = 315
          Width = 109
          Height = 25
          Caption = 'Skip'
          TabOrder = 1
          OnClick = ImageDropSkipButtonClick
        end
        object ImageDropTagListBox: TListBox
          Left = 8
          Top = 56
          Width = 224
          Height = 173
          ItemHeight = 13
          PopupMenu = ImageDropTagListPopupMenu
          TabOrder = 3
          OnDblClick = ImageDropTagListBoxDblClick
          OnMouseDown = ImageDropTagListBoxMouseDown
        end
        object DropImageViewCollectionButton: TButton
          Left = 8
          Top = 377
          Width = 109
          Height = 25
          Caption = 'View collection'
          TabOrder = 4
          Visible = False
          OnClick = DropImageViewCollectionButtonClick
        end
      end
    end
    object CollectionTabSheet: TTabSheet
      Caption = 'Collection'
      ImageIndex = 1
      OnResize = CollectionTabSheetResize
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object CollectionScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 683
        Height = 497
        Hint = 'Slide show'
        HorzScrollBar.Visible = False
        Align = alClient
        AutoScroll = False
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        ParentShowHint = False
        PopupMenu = ScrollBoxPopupMenu
        ShowHint = True
        TabOrder = 0
        object CollectionFlowPanel: TFlowPanel
          Left = 0
          Top = 0
          Width = 683
          Height = 208
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          Caption = 'CollectionFlowPanel'
          Color = clSilver
          ParentBackground = False
          ShowCaption = False
          TabOrder = 0
        end
      end
      object StatusBar1: TStatusBar
        Left = 0
        Top = 497
        Width = 683
        Height = 19
        Panels = <
          item
            Width = 140
          end
          item
            Width = 100
          end
          item
            Width = 50
          end>
      end
    end
  end
  object TitleBarPanel1: TTitleBarPanel
    Left = 0
    Top = 0
    Width = 818
    Height = 30
    CustomButtons = <>
    object ActionMainMenuBar1: TActionMainMenuBar
      Left = 36
      Top = 3
      Width = 165
      Height = 23
      UseSystemFont = False
      ActionManager = ActionManager1
      Align = alNone
      Caption = 'ActionMainMenuBar1'
      Color = clMenuBar
      ColorMap.DisabledFontColor = 7171437
      ColorMap.HighlightColor = clWhite
      ColorMap.BtnSelectedFont = clBlack
      ColorMap.UnusedColor = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      Spacing = 0
    end
  end
  object CollectionPopupMenu: TPopupMenu
    Left = 356
    Top = 444
    object OpenImagePopupMnu: TMenuItem
      Caption = 'Open image'
      OnClick = OpenImagePopupMnuClick
    end
    object OpenAllSelectedImagesPopupMnu: TMenuItem
      Caption = 'Open all selected images'
      OnClick = OpenAllSelectedImagesPopupMnuClick
    end
    object ShowInExplorerPopupMnu: TMenuItem
      Caption = 'Show in Explorer'
      OnClick = ShowInExplorerPopupMnuClick
    end
    object SelectImagePopupMnu: TMenuItem
      Caption = 'Select image'
      OnClick = SelectImagePopupMnuClick
    end
    object SelectAllImagesPopupMnu: TMenuItem
      Caption = 'Select all images'
      OnClick = SelectAllImagesPopupMnuClick
    end
    object DeselectImagePopupMnu: TMenuItem
      Caption = 'Deselect image'
      OnClick = DeselectImagePopupMnuClick
    end
    object DeselectAllImagesPopupMnu: TMenuItem
      Caption = 'Deselect all images'
      OnClick = DeselectAllImagesPopupMnuClick
    end
    object RemoveImageFromViewPopupMnu: TMenuItem
      Caption = 'Remove image from view'
      OnClick = RemoveImageFromViewPopupMnuClick
    end
    object RemoveImageFromCollectionPopupMnu: TMenuItem
      Caption = 'Remove image from collection'
      OnClick = RemoveImageFromCollectionPopupMnuClick
    end
    object RemoveAllSelectedImagesFromCollectionPopupMnu: TMenuItem
      Caption = 'Remove all selected images from collection'
      OnClick = RemoveAllSelectedImagesFromCollectionPopupMnuClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object CollectionPastePopupMnu: TMenuItem
      Caption = 'Paste images from clipboard'
      OnClick = CollectionPastePopupMnuClick
    end
    object CollectionCopyToClipboardPopupMnu: TMenuItem
      Caption = 'Copy selected images to clipboard'
      OnClick = CollectionCopyToClipboardPopupMnuClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object RecreateThumbnailPopupMnu: TMenuItem
      Caption = 'Recreate thumbnail'
      OnClick = RecreateThumbnailPopupMnuClick
    end
    object RecreateThumbnailsForSelectedImagesPopupMnu: TMenuItem
      Caption = 'Recreate thumbnails for all selected images'
      OnClick = RecreateThumbnailsForSelectedImagesPopupMnuClick
    end
    object PropertiesPopupMnu: TMenuItem
      Caption = 'Properties'
      OnClick = PropertiesPopupMnuClick
    end
  end
  object ImageTagListBoxPopupMenu: TPopupMenu
    OnPopup = ImageTagListBoxPopupMenuPopup
    Left = 208
    Top = 444
    object RemoveTagFromFocusedImagePopupMnu: TMenuItem
      Caption = 'Remove tag from focused image'
      OnClick = RemoveTagFromFocusedImagePopupMnuClick
    end
    object RemoveTagFromAllSelectedImagesPopupMnu: TMenuItem
      Caption = 'Remove tag from all selected images'
      OnClick = RemoveTagFromAllSelectedImagesPopupMnuClick
    end
  end
  object ScrollBoxPopupMenu: TPopupMenu
    Left = 64
    Top = 444
    object ScrollBoxPastePopupMnu: TMenuItem
      Caption = 'Paste'
      OnClick = ScrollBoxPastePopupMnuClick
    end
  end
  object DropDialogPanelPopupMenu: TPopupMenu
    Left = 356
    Top = 372
    object DropDialogPanelPastePopupMnu: TMenuItem
      Caption = 'Paste'
      OnClick = DropDialogPanelPastePopupMnuClick
    end
  end
  object ImageDropTagListPopupMenu: TPopupMenu
    Left = 208
    Top = 372
    object ImageDropRemoveTagFromListPopupMnu: TMenuItem
      Caption = 'Remove tag from this list'
      OnClick = ImageDropRemoveTagFromListPopupMnuClick
    end
    object ImageDropClearTagListPopupMnu: TMenuItem
      Caption = 'Clear tag list'
      OnClick = ImageDropClearTagListPopupMnuClick
    end
  end
  object TagTreePopupMenu: TPopupMenu
    OnPopup = TagTreePopupMenuPopup
    Left = 64
    Top = 372
    object TagTreeAddNewAsChildTagPopupMnu: TMenuItem
      Caption = 'Add a new tag as child'
      OnClick = TagTreeAddNewAsChildTagPopupMnuClick
    end
    object AddTagToAllSelectedImagesTreePopupMnu: TMenuItem
      Caption = 'Add tag to all selected images'
      OnClick = AddTagToAllSelectedImagesTreePopupMnuClick
    end
    object AddNewTagAsSiblingTagTreePopupMnu: TMenuItem
      Caption = 'Add a new tag having same parent'
      OnClick = AddNewTagAsSiblingTagTreePopupMnuClick
    end
    object RemoveTagFromTagTreePopupMnuClick: TMenuItem
      Caption = 'Remove tag from the tree'
      OnClick = RemoveTagFromTagTreePopupMnuClickClick
    end
    object RenameTagTagTreePopupMnu: TMenuItem
      Caption = 'Rename tag'
      OnClick = RenameTagTagTreePopupMnuClick
    end
    object MoveTagToOtherBranchTagTreePopupMnu: TMenuItem
      Caption = 'Move tag to other branch'
      OnClick = MoveTagToOtherBranchTagTreePopupMnuClick
    end
    object ReloadTreeTagTreePopupMnu: TMenuItem
      Caption = 'Reload tree'
      OnClick = ReloadTreeTagTreePopupMnuClick
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 356
    Top = 508
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Action = FileExitAction
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = CollectionAddImagesAction
                Caption = '&Add images to collection'
              end
              item
                Action = CollectionSearchByTagsAction
                Caption = '&Search by tags'
              end>
            Caption = '&Collection'
          end
          item
            Items = <
              item
                Action = ToolsScanDirAction
                Caption = '&Scan dir'
              end
              item
                Action = BackupDatabaseAction
                Caption = '&Backup database'
              end>
            Caption = '&Tools'
          end>
      end
      item
        Items = <
          item
            Items = <
              item
                Action = FileExitAction
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = CollectionAddImagesAction
                Caption = '&Add images to collection'
              end
              item
                Action = CollectionSearchByTagsAction
                Caption = '&Search by tags'
              end>
            Caption = '&Collection'
          end
          item
            Items = <
              item
                Action = BackupDatabaseAction
                Caption = '&Backup database'
              end
              item
                Action = ToolsScanDirAction
                Caption = '&Scan dir'
              end>
            Caption = '&Tools'
          end>
        ActionBar = ActionMainMenuBar1
      end>
    Left = 208
    Top = 508
    StyleName = 'Platform Default'
    object CollectionAddImagesAction: TAction
      Category = 'Collection'
      Caption = 'Add images to collection'
      OnExecute = CollectionAddImagesActionExecute
    end
    object FileExitAction: TAction
      Category = 'File'
      Caption = 'E&xit'
      OnExecute = FileExitActionExecute
    end
    object CollectionSearchByTagsAction: TAction
      Category = 'Collection'
      Caption = 'Search by tags'
      OnExecute = CollectionSearchByTagsActionExecute
    end
    object BackupDatabaseAction: TAction
      Category = 'Brol'
      Caption = 'Backup database'
      OnExecute = BackupDatabaseActionExecute
    end
    object ToolsScanDirAction: TAction
      Category = 'Brol'
      Caption = 'Scan dir'
      OnExecute = ToolsScanDirActionExecute
    end
  end
end
