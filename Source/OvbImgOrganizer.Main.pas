{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE @ OverByte
Creation:     May 17, 2020
Description:  Simple Image Organizer
License:      This program is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Dependencies: DxGetText     https://sourceforge.net/projects/dxgettext/
              VirtualTrees  https://github.com/Virtual-TreeView/Virtual-TreeView
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OvbImgOrganizer.Main;

interface

uses
    Winapi.Windows, Winapi.Messages, Winapi.ShlObj, Winapi.ActiveX,
    System.Types, System.Classes,
    System.IniFiles, System.Math, System.DateUtils, System.IOUtils,
    System.TypInfo, System.Actions,
    Vcl.Controls, Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons,
    Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ClipBrd, Vcl.Menus, Vcl.ComCtrls,
    Vcl.TitleBarCtrls, Vcl.ActnList, Vcl.FileCtrl,
    Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ToolWin,
    Vcl.ActnCtrls, Vcl.ActnMenus, Vcl.StdActns,
    System.SysUtils, // Must be seen after Vcl.FileCtrl
    Generics.Collections,
    DropHandler,
    GnuGetText,
    OvbImgOrganizer.PaintPanel,
    OvbImgOrganizer.DataTypes,
    OvbImgOrganizer.ImagePanel,
    OvbImgOrganizer.SlideShow,
    VirtualTrees;

const
    WM_APP_STARTUP            = WM_USER + 1;
    WM_AUTO_ADD_TO_COLLECTION = WM_USER + 2;
var
    CompanyFolder : String = 'OverByte';

type
    // Data to handle full screen or windowed view
    TOvbImgViewInfo = record
        NormalPos              : TRect;
        DisplayMemoHeight      : Integer;
        DisplaySplitterHeight  : Integer;
        TopPanelHeight         : Integer;
        ViewRegion             : THandle;
    end;

    // Data associated with each TreeView node
    TOvbImgTreeData = record
        Obj : TOvbImgTagObject;
    end;
    POvbImgTreeData = ^TOvbImgTreeData;

    TOvbImgCollection = class
        Count              : Integer;   // Total number of item loaded
        WCount             : Integer;   // Number of items on the width
        HCount             : Integer;   // Number of items on the height
        NHCount            : Integer;   // Number of visible items on height
        Selected           : TObjectList<TOvbImagePanel>;
        FocusedObject      : TOvbImagePanel;
        LastFocusedObject  : TOvbImagePanel;
        PrevFocusedObject  : TOvbImagePanel;
        RecCnt             : Integer;
        Limit              : Integer;
        LimitOffset        : Integer;
        constructor Create;
        destructor  Destroy; override;
    end;

    // Used to handle image drag&drop from Windows Explorer
    TOvbImgOrganizerDropInfo = class
    public
        FileNames     : TStringList;
        FileName      : String;
        Index         : Integer;
        CountLoaded   : Integer;
        CountSkipped  : Integer;
        CountDropped  : Integer;
        ImageID       : Integer;
        ThumbFileName : String;
        constructor Create;
        destructor  Destroy; override;
        procedure Clear;
    end;

    TOvbImgOrganizerMainForm = class(TForm)
        TopPanel: TPanel;
        DisplaySplitter: TSplitter;
        DisplayMemo: TMemo;
        RightPanel: TPanel;
        PageControl1: TPageControl;
        NewImageTabSheet: TTabSheet;
        CollectionTabSheet: TTabSheet;
        CollectionScrollBox: TScrollBox;
        StatusBar1: TStatusBar;
        CollectionFlowPanel: TFlowPanel;
        CollectionPopupMenu: TPopupMenu;
        OpenImagePopupMnu: TMenuItem;
        DeselectAllImagesPopupMnu: TMenuItem;
        ShowInExplorerPopupMnu: TMenuItem;
        SelectImagePopupMnu: TMenuItem;
        DeselectImagePopupMnu: TMenuItem;
        OpenAllSelectedImagesPopupMnu: TMenuItem;
        DropDialogPanel: TPanel;
        DropDialogSplitter: TSplitter;
        DropImageCountLabel: TLabel;
        ImageDropCancelButton: TButton;
        ImageDropAutoProcessCheckBox: TCheckBox;
        ImageDropAddToCollectionButton: TButton;
        ImageDropSkipButton: TButton;
        ImageDropTagListBox: TListBox;
        ImageTagListBoxPopupMenu: TPopupMenu;
        RemoveTagFromFocusedImagePopupMnu: TMenuItem;
        RemoveImageFromCollectionPopupMnu: TMenuItem;
        RemoveAllSelectedImagesFromCollectionPopupMnu: TMenuItem;
        RemoveTagFromAllSelectedImagesPopupMnu: TMenuItem;
        SelectAllImagesPopupMnu: TMenuItem;
        CollectionSearchEdit: TEdit;
        CollectionSearchBitBtn: TBitBtn;
        Label2: TLabel;
        OrderByComboBox: TComboBox;
        OrderByDescendingCheckBox: TCheckBox;
        LoadMoreButton: TButton;
        DropImageViewCollectionButton: TButton;
        RemoveImageFromViewPopupMnu: TMenuItem;
        ScrollBoxPopupMenu: TPopupMenu;
        ScrollBoxPastePopupMnu: TMenuItem;
        N1: TMenuItem;
        CollectionPastePopupMnu: TMenuItem;
        DropDialogPanelPopupMenu: TPopupMenu;
        DropDialogPanelPastePopupMnu: TMenuItem;
        CollectionCopyToClipboardPopupMnu: TMenuItem;
        ImageDropTagListPopupMenu: TPopupMenu;
        ImageDropRemoveTagFromListPopupMnu: TMenuItem;
        ImageDropClearTagListPopupMnu: TMenuItem;
        TagTreePopupMenu: TPopupMenu;
        TagTreeAddNewAsChildTagPopupMnu: TMenuItem;
        AddTagToAllSelectedImagesTreePopupMnu: TMenuItem;
        RemoveTagFromTagTreePopupMnuClick: TMenuItem;
        AddNewTagAsSiblingTagTreePopupMnu: TMenuItem;
        RenameTagTagTreePopupMnu: TMenuItem;
        MoveTagToOtherBranchTagTreePopupMnu: TMenuItem;
        ReloadTreeTagTreePopupMnu: TMenuItem;
        OpenDialog1: TOpenDialog;
        CollectionRightSplitter: TSplitter;
        CurrentImageTagsPanel: TPanel;
        CurrentImageTagsLabel: TLabel;
        ImageTagListBox: TListBox;
        Splitter1: TSplitter;
        AvailableTagsPanel: TPanel;
        Label1: TLabel;
        TagsVirtualStringTree: TVirtualStringTree;
        TitleBarPanel1: TTitleBarPanel;
        ActionManager1: TActionManager;
        CollectionAddImagesAction: TAction;
        FileExitAction: TAction;
        CollectionSearchByTagsAction: TAction;
        RecreateThumbnailPopupMnu: TMenuItem;
        RecreateThumbnailsForSelectedImagesPopupMnu: TMenuItem;
        N2: TMenuItem;
        BackupDatabaseAction: TAction;
        ToolsScanDirAction: TAction;
        HelpBitBtn: TBitBtn;
        SlidShowBitBtn: TBitBtn;
        ActionMainMenuBar1: TActionMainMenuBar;
        PropertiesPopupMnu: TMenuItem;
        procedure CollectionTabSheetResize(Sender: TObject);
        procedure FormKeyDown(Sender  : TObject;
                              var Key : Word;
                              Shift   : TShiftState);
        procedure FormKeyUp(Sender  : TObject;
                            var Key : Word;
                            Shift   : TShiftState);
        procedure DeselectAllImagesPopupMnuClick(Sender: TObject);
        procedure CollectionSearchBitBtnClick(Sender: TObject);
        procedure OpenImagePopupMnuClick(Sender: TObject);
        procedure ShowInExplorerPopupMnuClick(Sender: TObject);
        procedure SelectImagePopupMnuClick(Sender: TObject);
        procedure DeselectImagePopupMnuClick(Sender: TObject);
        procedure OpenAllSelectedImagesPopupMnuClick(Sender: TObject);
        procedure ImageDropCancelButtonClick(Sender: TObject);
        procedure ImageDropAddToCollectionButtonClick(Sender: TObject);
        procedure ImageDropSkipButtonClick(Sender: TObject);
        procedure ImageTagListBoxPopupMenuPopup(Sender: TObject);
        procedure ImageTagListBoxMouseDown(Sender : TObject;
                                           Button : TMouseButton;
                                           Shift  : TShiftState;
                                           X, Y   : Integer);
        procedure RemoveTagFromFocusedImagePopupMnuClick(Sender: TObject);
        procedure RemoveImageFromCollectionPopupMnuClick(Sender: TObject);
        procedure RemoveAllSelectedImagesFromCollectionPopupMnuClick(
                                           Sender: TObject);
        procedure RemoveTagFromAllSelectedImagesPopupMnuClick(Sender: TObject);
        procedure SelectAllImagesPopupMnuClick(Sender: TObject);
        procedure FormMouseWheel(Sender      : TObject;
                                 Shift       : TShiftState;
                                 WheelDelta  : Integer;
                                 MousePos    : TPoint;
                                 var Handled : Boolean);
        procedure LoadMoreButtonClick(Sender: TObject);
        procedure DropImageViewCollectionButtonClick(Sender: TObject);
        procedure RemoveImageFromViewPopupMnuClick(Sender: TObject);
        procedure ScrollBoxPastePopupMnuClick(Sender: TObject);
        procedure CollectionPastePopupMnuClick(Sender: TObject);
        procedure DropDialogPanelPastePopupMnuClick(Sender: TObject);
        procedure CollectionCopyToClipboardPopupMnuClick(Sender: TObject);
        procedure ImageDropTagListBoxMouseDown(Sender : TObject;
                                               Button : TMouseButton;
                                               Shift  : TShiftState;
                                               X, Y   : Integer);
        procedure ImageDropRemoveTagFromListPopupMnuClick(Sender: TObject);
        procedure ImageDropClearTagListPopupMnuClick(Sender: TObject);
        procedure TagsVirtualStringTreeGetPopupMenu(
                                        Sender          : TBaseVirtualTree;
                                        Node            : PVirtualNode;
                                        Column          : TColumnIndex;
                                        const P         : TPoint;
                                        var   AskParent : Boolean;
                                        var   PopupMenu : TPopupMenu);
        procedure TagsVirtualStringTreeGetText(
                                        Sender       : TBaseVirtualTree;
                                        Node         : PVirtualNode;
                                        Column       : TColumnIndex;
                                        TextType     : TVSTTextType;
                                        var CellText : string);
        procedure TagsVirtualStringTreeMouseDown(
                                        Sender : TObject;
                                        Button : TMouseButton;
                                        Shift  : TShiftState;
                                        X, Y   : Integer);
        procedure TagsVirtualStringTreeExpanding(
                                        Sender      : TBaseVirtualTree;
                                        Node        : PVirtualNode;
                                        var Allowed : Boolean);
        procedure TagsVirtualStringTreeFreeNode(
                                        Sender : TBaseVirtualTree;
                                        Node   : PVirtualNode);
        procedure TagsVirtualStringTreeNodeDblClick(
                                        Sender        : TBaseVirtualTree;
                                        const HitInfo : THitInfo);
        procedure TagTreePopupMenuPopup(Sender: TObject);
        procedure TagTreeAddNewAsChildTagPopupMnuClick(Sender: TObject);
        procedure AddTagToAllSelectedImagesTreePopupMnuClick(Sender: TObject);
        procedure RemoveTagFromTagTreePopupMnuClickClick(Sender: TObject);
        procedure AddNewTagAsSiblingTagTreePopupMnuClick(Sender: TObject);
        procedure RenameTagTagTreePopupMnuClick(Sender: TObject);
        procedure MoveTagToOtherBranchTagTreePopupMnuClick(Sender: TObject);
        procedure TagsVirtualStringTreeDragOver(
                                        Sender      : TBaseVirtualTree;
                                        Source      : TObject;
                                        Shift       : TShiftState;
                                        State       : TDragState;
                                        Pt          : TPoint;
                                        Mode        : TDropMode;
                                        var Effect  : Integer;
                                        var Accept  : Boolean);
        procedure TagsVirtualStringTreeDragDrop(
                                        Sender      : TBaseVirtualTree;
                                        Source      : TObject;
                                        DataObject  : IDataObject;
                                        Formats     : TFormatArray;
                                        Shift       : TShiftState;
                                        Pt          : TPoint;
                                        var Effect  : Integer;
                                        Mode        : TDropMode);
        procedure TagsVirtualStringTreeDragAllowed(
                                        Sender      : TBaseVirtualTree;
                                        Node        : PVirtualNode;
                                        Column      : TColumnIndex;
                                        var Allowed : Boolean);
        procedure ReloadTreeTagTreePopupMnuClick(Sender: TObject);
        procedure TagsVirtualStringTreeInitNode(
                                        Sender            : TBaseVirtualTree;
                                        ParentNode        : PVirtualNode;
                                        Node              : PVirtualNode;
                                        var InitialStates : TVirtualNodeInitStates);
        procedure ImageDropTagListBoxDblClick(Sender: TObject);
        procedure FileExitMenuItemClick(Sender: TObject);
        procedure CollectionAddImagesActionExecute(Sender: TObject);
        procedure FileExitActionExecute(Sender: TObject);
        procedure CollectionSearchByTagsActionExecute(Sender: TObject);
        procedure RecreateThumbnailPopupMnuClick(Sender: TObject);
        procedure RecreateThumbnailsForSelectedImagesPopupMnuClick(Sender: TObject);
        procedure ImageTagListBoxDblClick(Sender: TObject);
        procedure BackupDatabaseActionExecute(Sender: TObject);
        procedure ToolsScanDirActionExecute(Sender: TObject);
        procedure HelpBitBtnClick(Sender: TObject);
        procedure SlidShowBitBtnClick(Sender: TObject);
        procedure PropertiesPopupMnuClick(Sender: TObject);
    protected
        procedure CreateParams(var Params: TCreateParams); override;
        procedure DoShow; override;
        procedure DoClose(var Action: TCloseAction); override;
    private
        FDropPreviewImagePanel        : TOvbImagePanel;
        FLocalAppData                 : String;
        FCommonAppData                : String;
        FAppName                      : String;
        FIniFileName                  : String;
        FInitialized                  : Boolean;
        FIniSection                   : String;
        FIniSectionData               : String;
        FParentedFlag                 : Boolean;
        FFullScreen                   : Boolean;
        FFullScreenFlag               : Boolean;
        FViewInfo                     : TOvbImgViewInfo;
        FCollection                   : TOvbImgCollection;
        FDropTarget                   : TDropTarget; // Drag&Drop from Windows Explorer
        FDropFileNames                : array of String;
        FDropAllowed                  : Boolean;
        FImageTagListRightClickAt     : TPoint;
        FDataFile                     : String;
        FLoadCollectionLastSqlText    : String;
        FDropInfo                     : TOvbImgOrganizerDropInfo;
        FFlagReturnKeyDown            : Boolean;
        FLastBackupLocation           : String;
        FLastScanDir                  : String;
        FLastScanRecursive            : Boolean;
        FLastProgressTime             : Cardinal;
        FSlideShowList                : TObjectList<TSlideShowForm>;
        procedure WMAppStartup(var Msg: TMessage); message WM_APP_STARTUP;
        procedure WMAutoAddToCollection(var Msg: TMessage); message WM_AUTO_ADD_TO_COLLECTION;
        procedure DoFullScreen(AFullScreen: Boolean);
        procedure Display(const Msg : String); overload;
        procedure Display(const Msg : String; Args : array of const); overload;
        procedure DisplayHandler(Sender : TObject; const Msg : String);
        procedure FindDatabaseFile;
        function  CollectionLoad : Integer; overload;
        function  CollectionLoad(const SqlText : String;
                                 const More    : Boolean = FALSE) : Integer; overload;
        function  CollectionLoadMore : Integer;
        procedure CollectionReload;
        procedure CollectionSearch(const TagsString: String);
        procedure CollectionUpdateSelectedStatus;
        procedure CollectionImagePanelFormKeyUp(Sender  : TObject;
                                                var Key : Word;
                                                Shift   : TShiftState);
        procedure ThumbnailRecreate(AImagePanel: TOvbImagePanel);
        procedure ThumbnailDelete(const Thumbnail: String);
        function  TagTreeLoad : Boolean;
        procedure TagSelectedImages(const TagNames: TStringDynArray);
        procedure TagRemoveFromFocused(Ctrl: TWinControl);
        function  TagRemove(const TagName: String): Boolean;
        function  TagsAddNew(const AValues    : array of String;
                             const ATagParent : String): Boolean; overload;
        function  TagsAddNew(const AValues    : array of String;
                             const AParentID  : Integer): Boolean; overload;
        function  TagTreeFindAndExpandNode(const FromNode : PVirtualNode;
                                           const TagName  : String) : PVirtualNode;
        procedure ImagesSelectAll;
        procedure ImageRemoveFromView(ImagePanel: TOvbImagePanel);
        procedure PasteFilesFromClipBoard;
        procedure PasteFileNameList(const FileNames: array of String);
        procedure CopyFilesToClipboard(FileNames : array of String);
        procedure FlowLayoutImageDblClickHandler(Sender: TObject);
        procedure FlowLayoutImageMouseDownHandler(Sender : TObject;
                                                  Button : TMouseButton;
                                                  Shift  : TShiftState;
                                                  X, Y   : Integer);
        procedure ImageDragLeaveHandler(Sender: TObject);
        procedure ImageDragOverHandler(Sender            : TObject;
                                       const GrfKeyState : Longint;
                                       const Pt          : TPoint;
                                       var   Allowed     : Boolean);
        procedure ImageDropAllowedHandler(Sender            : TObject;
                                          const FileNames   : array of String;
                                          const GrfKeyState : Integer;
                                          const Pt          : TPoint;
                                          var Allowed       : Boolean);
        // Handler called when an image is dropped from Windows Explorer
        procedure ImageDropHandler(Sender          : TObject;
                                   const DropPoint : TPoint;
                                   const FileNames : array of String);
        procedure ImageDropProcessNext;
        procedure DropPreviewImagePanelLoad(const FileName      : String;
                                            out   ThumbFilename : String);
        procedure FlowLayoutImageClickHandler(Sender : TObject);
        procedure FlowLayoutCheckBoxClickHandler(Sender: TObject);
        procedure FlowLayoutImageDestroyHandler(Sender : TObject);
        procedure ActiveControlChangeHandler(Sender: TObject);
        procedure DropPanelButtonFormKeyUp(Sender  : TObject;
                                           var Key : Word;
                                           Shift   : TShiftState);
        function  IsChildOf(Ctrl, ParentCtrl: TWinControl): Boolean;
        procedure DataRecordHandler(Sender           : TObject;
                                    var   Index      : Integer;
                                    const DataRecord : TOvbImgDataRecord);
        procedure AskYesOrNoHandler(Sender         : TObject;
                                    const Question : String;
                                    const Caption  : String;
                                    var   Answer   : Integer);
        procedure TranslateStringHandler(Sender       : TObject;
                                         const Msg    : String;
                                         var   Xlated : String);
        procedure CompareImageIndexWidthDirContentProgressHandler(
                                    Sender    : TObject;
                                    Context   : Integer;
                                    Count     : Integer;
                                    var Abort : Boolean);
        procedure SlidShowFormDestroyHandler(Sender: TObject);
        procedure ImagePanelShow(Ctrl: TControl);
        procedure CenterChildForm(Form: TCustomForm);
        procedure DropPreviewImagePaneDblClickHandler(Sender: TObject);
    public
        constructor Create(AOwner : TComponent); override;
        constructor CreateParented(ParentWindow: HWnd); virtual;
        destructor  Destroy; override;
        procedure SetFullScreen(AFullScreen : Boolean);
        property IniFileName            : String     read  FIniFileName
                                                     write FIniFileName;
        property IniSection             : String     read  FIniSection
                                                     write FIniSection;
        property IniSectionData         : String     read  FIniSectionData
                                                     write FIniSectionData;
        property LocalAppData           : String     read  FLocalAppData
                                                     write FLocalAppData;
        property AppName                : String     read  FAppName
                                                     write FAppName;
    end;

var
    OvbImgOrganizerMainForm : TOvbImgOrganizerMainForm;

implementation

{$R *.dfm}

uses
    System.StrUtils,
    WinApi.ShellAPI,
    SuperMessageForm,
    OvbImgOrganizer.Utils,
    OvbImgOrganizer.DataModule,
    OvbImgOrganizer.Help,
    OvbImgOrganizer.ImageScanDir;

const
    ProgName   = 'OvbImgOrganizer';
var
    ProgVer    : String;   // Actual value is read from VerInfo resource

const
    MAX_DISPLAY_LINES         = 4000;
    THUMB_WIDTH               = 128;
    THUMB_HEIGHT              = (THUMB_WIDTH * 2) div 3;
    THUMB_CACHE_DIR           = 'Thumbnails';

const
    SectionWindow            = 'Window';
    SectionData              = 'Data';
    KeyTop                   = 'Top';
    KeyLeft                  = 'Left';
    KeyWidth                 = 'Width';
    KeyHeight                = 'Height';
    KeyDisplayMemo           = 'DisplayHeight';
    KeyRightPanel            = 'RightPanelWidth';
    KeyDropDialogPanel       = 'DropDialogPanelWidth';
    KeyCurrentImageTagsPanel = 'KeyCurrentImageTagsPanelHeight';
    KeyDataFile              = 'DataBaseFile';
    KeySearchText            = 'SearchText';
    KeyOrderBy               = 'OrderBy';
    KeyDescending            = 'OrderByDescending';
    KeyLastBackupLocation    = 'LastBackupLocation';
    KeyLastScanDir           = 'LastScanDir';
    KeyLastScanRecursive     = 'LastScanRecursive';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ImagePanelShellOpen(
    Handle : HWND;
    Ctrl   : TControl);
var
    FileName   : String;
    ImageInfo  : TOvbImgImageInfo;
begin
    if not Assigned(Ctrl) then
        Exit;
    if Ctrl is TOvbImagePanel then begin
        ImageInfo  := TOvbImagePanel(Ctrl).TagObject as TOvbImgImageInfo;
        if Assigned(ImageInfo) then begin
            FileName   := ImageInfo.ImagePath + ImageInfo.ImageName;
            ShellExecute(Handle,
                         'open',   // Do not localize
                         PChar(FileName), '', '', SW_SHOW);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ImagePanelShellProperties(
    Handle : HWND;
    Ctrl   : TControl);
var
    FileName   : String;
    ImageInfo  : TOvbImgImageInfo;
    ExecInfo   : TShellExecuteInfo;
begin
    if not Assigned(Ctrl) then
        Exit;
    if Ctrl is TOvbImagePanel then begin
        ImageInfo  := TOvbImagePanel(Ctrl).TagObject as TOvbImgImageInfo;
        if Assigned(ImageInfo) then begin
            FileName   := ImageInfo.ImagePath + ImageInfo.ImageName;

            ZeroMemory(@ExecInfo, SizeOf(ExecInfo));
            ExecInfo.cbSize := SizeOf(TShellExecuteInfo);
            ExecInfo.Wnd    := Handle;
            ExecInfo.lpVerb := 'properties';     // Do not localize
            ExecInfo.lpFile := PChar(FileName);
            ExecInfo.nShow  := SW_NORMAL;
            ExecInfo.fMask  := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_INVOKEIDLIST;
            ShellExecuteEx(@ExecInfo);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImagePanelShow(Ctrl : TControl);
var
    ImageInfo  : TOvbImgImageInfo;
    Form       : TSlideShowForm;
begin
    if not Assigned(Ctrl) then
        Exit;
    if Ctrl is TOvbImagePanel then begin
        ImageInfo  := TOvbImagePanel(Ctrl).TagObject as TOvbImgImageInfo;
        if Assigned(ImageInfo) then begin
            Form           := TSlideShowForm.Create(Self);
            Form.OnDestroy := SlidShowFormDestroyHandler;
            Form.OnDisplay := DisplayHandler;
            Form.Filenames.Add(ImageInfo.ImagePath + ImageInfo.ImageName);
            FSlideShowList.Add(Form);
            CenterChildForm(Form);
            Form.Show;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ImagePanelShellOpenContainingFolder(Ctrl : TControl);
var
    ImageInfo  : TOvbImgImageInfo;
begin
    if not Assigned(Ctrl) then
        Exit;
    if Ctrl is TOvbImagePanel then begin
        ImageInfo  := TOvbImagePanel(Ctrl).TagObject as TOvbImgImageInfo;
        if Assigned(ImageInfo) then
            ShellExecute(
                0, nil,
                'explorer.exe',         // Do not localize
                PChar('/select,' +      // Do not localize
                      '"' + ImageInfo.ImagePath + ImageInfo.ImageName + '"'),
                '', SW_SHOW);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TOvbImgOrganizerMainForm }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TOvbImgOrganizerMainForm.Create(AOwner: TComponent);
var
    CommonPath   : array [0..MAX_PATH] of Char;
    LocalPath    : array [0..MAX_PATH] of Char;
    LangFileName : String;
begin
    SHGetFolderPath(0, CSIDL_COMMON_APPDATA, 0, SHGFP_TYPE_CURRENT, @CommonPath[0]);
    SHGetFolderPath(0, CSIDL_LOCAL_APPDATA,  0, SHGFP_TYPE_CURRENT, @LocalPath[0]);
    FIniSection     := SectionWindow;
    FIniSectionData := SectionData;
    FAppName        := ChangeFileExt(ExtractFileName(Application.ExeName), '');
    FCommonAppData  := IncludeTrailingPathDelimiter(CommonPath) +
                       CompanyFolder + '\' + FAppName + '\';
    FLocalAppData   := IncludeTrailingPathDelimiter(LocalPath) +
                       CompanyFolder + '\' + FAppName + '\';
    FIniFileName    := FLocalAppData + FAppName + '.ini';
    ForceDirectories(FCommonAppData);
    ForceDirectories(FLocalAppData);
    ForceDirectories(FCommonAppData + THUMB_CACHE_DIR + '\');
    FFullScreen     := FALSE;
    inherited Create(AOwner);

    // The next lines will change the folder with the language file so
    // that is uses the executable folder and the same name as the executable
    // with .mo file extension.
    // When developing, the language file is stored in the source folder and
    // has a slightly different filename.
    LangFileName := ExtractFilePath(Application.ExeName) +
                    '..\..\..\Source\' + FAppName + 'Language.mo';
    if not FileExists(LangFileName) then
        LangFileName := ChangeFileExt(Application.ExeName, '.mo');

    DefaultInstance.TextDomain(ChangeFileExt(ExtractFileName(Application.ExeName), ''));
    DefaultInstance.BindTextDomainToFile(GetCurrentTextDomain, LangFileName);

    try
        TranslateComponent(Self);
    except

    end;

    KeyPreview := TRUE;       // To intercept F11 for full screen
    if FParentedFlag then
        BorderStyle := bsNone;
    DisplayMemo.Clear;
    Caption := ProgName + ' V' + ProgVer;
{$IF DEFINED(WIN32)}
    Display(Caption + ' (32 bit)');
{$ELSEIF DEFINED(WIN64)}
    Display(Caption + ' (64 bit)');
{$ELSE}
    Display(Caption);
{$ENDIF}

    FCollection                            := TOvbImgCollection.Create;
    FDropInfo                              := TOvbImgOrganizerDropInfo.Create;
    FDropPreviewImagePanel                 := TOvbImagePanel.Create(Self);
    FDropPreviewImagePanel.Parent          := NewImageTabSheet;
    FDropPreviewImagePanel.Align           := alClient;
    FDropPreviewImagePanel.BackColor       := clLtGray;
    FDropPreviewImagePanel.IsThumbnail     := TRUE;
    FDropPreviewImagePanel.OnDisplay       := DisplayHandler;
    FDropPreviewImagePanel.OnImageDblClick := DropPreviewImagePaneDblClickHandler;
    FDropPreviewImagePanel.Visible         := TRUE;

    // Init Drag&Drop from Windows Explorer
    FDropTarget                      := TDropTarget.Create;
    FDropTarget.OnDropAllowed        := ImageDropAllowedHandler;
    FDropTarget.OnDrop               := ImageDropHandler;
    FDropTarget.OnDragOver           := ImageDragOverHandler;
    FDropTarget.OnDragLeave          := ImageDragLeaveHandler;
    FDropTarget.Register(Handle);

    CollectionUpdateSelectedStatus;
    NewImageTabSheet.TabVisible   := FALSE;
    CollectionTabSheet.TabVisible := FALSE;
    PageControl1.ActivePage       := CollectionTabSheet;

    OrderByComboBox.Items.Add(_('FileName'));         // Index 0
    OrderByComboBox.Items.Add(_('FileDateTime'));     // Index 1
    OrderByComboBox.Items.Add(_('IndexDateTime'));    // Index 2
    OrderByComboBox.ItemIndex := 0;

    FSlideShowList                := TObjectList<TSlideShowForm>.Create(FALSE);

    Screen.OnActiveControlChange  := ActiveControlChangeHandler;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TOvbImgOrganizerMainForm.CreateParented(ParentWindow: HWnd);
begin
    FParentedFlag := TRUE;
    inherited CreateParented(ParentWindow);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.CreateParams(var Params: TCreateParams);
begin
    inherited CreateParams(Params);
    if not FParentedFlag then begin
        // This will make the form have an icon on taskbar and make it
        // separate from the main window.
        // This is not desirable if the window is created parented,
        // hence the flag.
        Params.ExStyle   := Params.ExStyle or WS_EX_APPWINDOW;
        Params.WndParent := GetDesktopwindow;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TOvbImgOrganizerMainForm.Destroy;
var
    Form : TSlideShowForm;
    I    : Integer;
begin
    Screen.OnActiveControlChange  := nil;
    FreeAndNil(FDropTarget);
    FreeAndNil(FCollection);
    FreeAndNil(FDropInfo);
    if Assigned(FSlideShowList) then begin
        for I := FSlideShowList.Count - 1 downto 0 do begin
            Form := FSlideShowList[I];
            FSlideShowList[I] := nil;
            FreeAndNil(Form);
        end;
        FreeAndNil(FSlideShowList);
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.DoShow;
var
    IniFile : TIniFile;
    I       : Integer;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        ForceDirectories(ExtractFilePath(FIniFileName));
        IniFile := TIniFile.Create(FIniFileName);
        try
            if not FParentedFlag then begin
                Width  := IniFile.ReadInteger(FIniSection, KeyWidth,  Width);
                Height := IniFile.ReadInteger(FIniSection, KeyHeight, Height);
                Top    := IniFile.ReadInteger(FIniSection, KeyTop,
                                              (Screen.Height - Height) div 2);
                Left   := IniFile.ReadInteger(FIniSection, KeyLeft,
                                              (Screen.Width  - Width)  div 2);
            end;
            DisplayMemo.Height                := IniFile.ReadInteger(
                                                      FIniSectionData,
                                                      KeyDisplayMemo,
                                                      DisplayMemo.Height);
            RightPanel.Width                  := IniFile.ReadInteger(
                                                      FIniSectionData,
                                                      KeyRightPanel,
                                                      RightPanel.Width);
            DropDialogPanel.Width             := IniFile.ReadInteger(
                                                      FIniSectionData,
                                                      KeyDropDialogPanel,
                                                      DropDialogPanel.Width);
            CurrentImageTagsPanel.Height      := IniFile.ReadInteger(
                                                      FIniSectionData,
                                                      KeyCurrentImageTagsPanel,
                                                      CurrentImageTagsPanel.Height);
            FDataFile                         := IniFile.ReadString(
                                                      FIniSectionData,
                                                      KeyDataFile,
                                                      FCommonAppData +
                                                      'OvbImgOrganizer.sqlite3');
            CollectionSearchEdit.Text         :=  IniFile.ReadString(
                                                      FIniSectionData,
                                                      KeySearchText,
                                                      '');
            OrderByComboBox.ItemIndex         := IniFile.ReadInteger(
                                                      FIniSectionData,
                                                      KeyOrderBy,
                                                      2);
            OrderByDescendingCheckBox.Checked := IniFile.ReadBool(
                                                      FIniSectionData,
                                                      KeyDescending,
                                                      OrderByDescendingCheckBox.Checked);
            FLastBackupLocation               := IniFile.ReadString(
                                                      FIniSectionData,
                                                      KeyLastBackupLocation,
                                                      FLastBackupLocation);
            FLastScanDir                      := IniFile.ReadString(
                                                      FIniSectionData,
                                                      KeyLastScanDir,
                                                      FLastScanDir);
            FLastScanRecursive                := IniFile.ReadBool(
                                                      FIniSectionData,
                                                      KeyLastScanRecursive,
                                                      FLastScanRecursive);
        finally
            IniFile.Destroy;
        end;

        if not FParentedFlag then begin
            // Check if form is on an existing monitor
            I := 0;
            while I < Screen.MonitorCount do begin
                if (Top >= Screen.Monitors[I].Top) and
                   (Top <= (Screen.Monitors[I].Top +
                                 Screen.Monitors[I].Height)) and
                   (Left >= Screen.Monitors[I].Left) and
                   (Left <= (Screen.Monitors[I].Left +
                                  Screen.Monitors[I].Width)) then
                    break;
                Inc(I);
            end;
            if I >= Screen.MonitorCount then begin
                // Form is outside of any monitor. Move to center of main monitor
                Top  := (Screen.Height - Height) div 2;
                Left := (Screen.Width  - Width)  div 2;
            end;
        end;
        DoFullScreen(FFullScreenFlag);
        PostMessage(Handle, WM_APP_STARTUP, 0, 0);
    end;
    inherited DoShow;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.DropDialogPanelPastePopupMnuClick(
    Sender: TObject);
begin
    PasteFilesFromClipBoard;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.DropImageViewCollectionButtonClick(
  Sender: TObject);
begin
    // We will display collection by indexDateTime first, that is the
    // images we just inserted in the top.
    CollectionSearchEdit.Text         := '';
    OrderByComboBox.ItemIndex         := 2;    // IndexDateTime
    OrderByDescendingCheckBox.Checked := TRUE;
    CollectionSearchBitBtnClick(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.DoClose(var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    try
        if FFullScreen then
            DoFullScreen(FALSE);

        IniFile := TIniFile.Create(FIniFileName);
        try
            if not FParentedFlag then begin
                IniFile.WriteInteger(FIniSection, KeyTop,    Top);
                IniFile.WriteInteger(FIniSection, KeyLeft,   Left);
                IniFile.WriteInteger(FIniSection, KeyWidth,  Width);
                IniFile.WriteInteger(FIniSection, KeyHeight, Height);
            end;
            IniFile.WriteInteger(
                        FIniSectionData,
                        KeyDisplayMemo,
                        DisplayMemo.Height);
            IniFile.WriteInteger(
                        FIniSectionData,
                        KeyRightPanel,
                        RightPanel.Width);
            IniFile.WriteInteger(
                        FIniSectionData,
                        KeyDropDialogPanel,
                        DropDialogPanel.Width);
            IniFile.WriteInteger(
                        FIniSectionData,
                        KeyCurrentImageTagsPanel,
                        CurrentImageTagsPanel.Height);
            IniFile.WriteString(
                        FIniSectionData,
                        KeyDataFile,
                        FDataFile);
            IniFile.WriteString(
                        FIniSectionData,
                        KeySearchText,
                        CollectionSearchEdit.Text);
            IniFile.WriteInteger(
                        FIniSectionData,
                        KeyOrderBy,
                        OrderByComboBox.ItemIndex);
            IniFile.WriteBool(
                        FIniSectionData,
                        KeyDescending,
                        OrderByDescendingCheckBox.Checked);
            IniFile.WriteString(
                        FIniSectionData,
                        KeyLastBackupLocation,
                        FLastBackupLocation);
            IniFile.WriteString(
                        FIniSectionData,
                        KeyLastScanDir,
                        FLastScanDir);
            IniFile.WriteBool(
                        FIniSectionData,
                        KeyLastScanRecursive,
                        FLastScanRecursive);
        finally
            IniFile.Destroy;
        end;
    except
        // Ignore any exception when saving window size and position
    end;
    inherited DoClose(Action);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.Display(const Msg: String);
begin
    if csDestroying in ComponentState then
        Exit;
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > MAX_DISPLAY_LINES then begin
            while DisplayMemo.Lines.Count > MAX_DISPLAY_LINES do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(FormatDateTime('YYYMMDD HHNNSS ', Now) + Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.Display(
    const Msg : String;
    Args      : array of const);
begin
    Display(Format(Msg, Args));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.DisplayHandler(
    Sender    : TObject;
    const Msg : String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.SetFullScreen(AFullScreen: Boolean);
begin
    FFullScreenFlag := AFullScreen;
    if Visible then
        DoFullScreen(FFullScreenFlag);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.DoFullScreen(AFullScreen: Boolean);
var
    CurMonitor   : TMonitor;
    Rc           : TRect;
    TopMargin    : Integer;
    LeftMargin   : Integer;
    BottomMargin : Integer;
    RightMargin  : Integer;
begin
    if FFullScreen = AFullScreen then
        Exit;
    FFullScreen  := AFullScreen;
    CurMonitor   := Screen.MonitorFromWindow(Handle);
    TopMargin    := GetSystemMetrics(SM_CYCAPTION) +
                    GetSystemMetrics(SM_CYBORDER);
    LeftMargin   := GetSystemMetrics(SM_CXBORDER);
    RightMargin  := LeftMargin;
    BottomMargin := GetSystemMetrics(SM_CYBORDER);


    if FFullScreen then begin
        // Currently normal windowed view, switch to full screen
        FViewInfo.NormalPos             := BoundsRect;
        FViewInfo.TopPanelHeight        := TopPanel.Height;
        FViewInfo.DisplayMemoHeight     := DisplayMemo.Height;
        FViewInfo.DisplaySplitterHeight := DisplaySplitter.Height;
        BorderStyle            := bsDialog;
        BorderIcons            := [];
        Constraints.MaxHeight  := CurMonitor.Height + TopMargin + BottomMargin;
        Rc                     := CurMonitor.BoundsRect;
        Rc.Top                 := Rc.Top - TopMargin;
        Rc.Left                := Rc.Left - LeftMargin;
        Rc.Bottom              := Rc.Bottom + BottomMargin + TopMargin;
        Rc.Right               := Rc.Right + RightMargin;
        BoundsRect             := Rc;
        if FViewInfo.ViewRegion <> 0 then begin
            SetWindowRGN(Handle, 0, FALSE);
            DeleteObject(FViewInfo.ViewRegion);
        end;
        FViewInfo.ViewRegion := CreateRectRgn(LeftMargin, TopMargin,
                                 Width - LeftMargin, Height - LeftMargin);
        SetWindowRGN(Handle, FViewInfo.ViewRegion, TRUE);
        TopPanel.Height := ClientHeight - FViewInfo.DisplayMemoHeight - FViewInfo.DisplaySplitterHeight;
    end
    else begin
        // Currently full screen view, switch back to windowed
        SetWindowRGN(Handle, 0, TRUE);
        DeleteObject(FViewInfo.ViewRegion);
        FViewInfo.ViewRegion  := 0;
        Constraints.MaxHeight := CurMonitor.Height;
        TopPanel.Height       := FViewInfo.TopPanelHeight;
        BorderStyle           := bsSizeable;
        BorderIcons           := [biSystemMenu, biMinimize, biMaximize];
        BoundsRect            := FViewInfo.NormalPos;
    end
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.WMAppStartup(var Msg: TMessage);
var
    Status : Integer;
    R      : TRect;
    ErrMsg : String;
begin
    OvbImgOrganizerDataModule.OnDisplay         := DisplayHandler;
    OvbImgOrganizerDataModule.OnAskYesOrNo      := AskYesOrNoHandler;
    OvbImgOrganizerDataModule.OnTranslateString := TranslateStringHandler;
    OvbImgOrganizerDataModule.CommonAppData     := FCommonAppData;
    OvbImgOrganizerDataModule.ProgVer           := ProgVer;
    OvbImgOrganizerDataModule.DataFile          := FDataFile;

    R := Rect(10,
              RightPanel.Top - 10,
              RightPanel.Left - 10,
              RightPanel.Top + RightPanel.Height - 10);

    while not FileExists(FDataFile) do begin
        Status := TMessageForm.ShowMessage(
                      _('Data file doesn''t exists!') + CRLF +
                      FDataFile + CRLF +
                      _('Do you want to locate it on disk?'),
                      Caption, nil,
                      [mffFailed, mffYesNoCancel, mffAutoWidth]);
        if Status = IDCANCEL then begin
            Close;
            Exit;
        end;

        if Status = IDNO then begin
            Status := TMessageForm.ShowMessage(
                          _('Data file doesn''t exists!') + CRLF +
                          FDataFile + CRLF +
                          _('Do you want to create a new one?'),
                          Caption, nil,
                          [mffYesNo, mffAutoWidth]);
            if Status = IDYES then begin
                if not OvbImgOrganizerDataModule.DBCreate(ErrMsg) then
                    TMessageForm.ShowMessage(ErrMsg, _('Create DB'),
                                             nil, [mffFailed]);
                continue;
            end
            else begin
                Close;
                Exit;
            end;
        end;

        if Status = IDYES then
            FindDatabaseFile;
    end;

    if not OvbImgOrganizerDataModule.DBConnect(ErrMsg) then begin
        TMessageForm.ShowMessage(ErrMsg, Caption, nil,
                                 [mffFailed, mffAutoWidth]);
        Close;
        Exit;
    end;

    TagTreeLoad;
    CollectionSearchBitBtnClick(nil);
    OvbImgOrganizerDataModule.DBDisconnect;

    Display(_('This program is published under MOZILLA PUBLIC LICENSE V2.0.'));
    Display(_('You may not use this file except in compliance with the License.'));
    Display(_('You may obtain a copy of the License at'));
    Display('https://www.mozilla.org/en-US/MPL/2.0/');
    Display(_('Ready'));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.WMAutoAddToCollection(var Msg: TMessage);
begin
    if ImageDropAutoProcessCheckBox.Checked then
        ImageDropAddToCollectionButtonClick(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.FileExitMenuItemClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.FindDatabaseFile;
var
    Path : String;
begin
    Path := ExtractFilePath(FDataFile);
    if not System.SysUtils.DirectoryExists(Path) then
        Path := FCommonAppData;
    OpenDialog1.InitialDir := Path;
    OpenDialog1.DefaultExt := '.sqlite3';
    OpenDialog1.Filter     := _('Data file') + ' (*.sqlite3)|*.sqlite3|';
    OpenDialog1.Options    := [ofFileMustExist];
    if not OpenDialog1.Execute then
        Exit;
    FDataFile := OpenDialog1.FileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.CollectionAddImagesActionExecute(
    Sender: TObject);
var
    OpenDialog1 : TOpenDialog;
begin
    OpenDialog1 := TOpenDialog.Create(Self);
    try
        OpenDialog1.Filter := _('JPG files') + ' (*.jpg)|*.JPG|' +
                              _('Any file')  + ' (*.*)|*.*';
        OpenDialog1.Options := [ofAllowMultiSelect,
                                ofPathMustExist,
                                ofFileMustExist,
                                ofEnableSizing];
        if not OpenDialog1.Execute then
            Exit;
        FDropInfo.FileNames.AddStrings(OpenDialog1.Files);
    finally
        FreeAndNil(OpenDialog1);
    end;
    PageControl1.ActivePage := NewImageTabSheet;
    ImageDropAutoProcessCheckBox.Checked := FALSE;
    ImageDropProcessNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.AddNewTagAsSiblingTagTreePopupMnuClick(
    Sender: TObject);
var
    APrompts   : array of String;
    AValues    : array of String;
    Node       : PVirtualNode;
    Data       : POvbImgTreeData;
    Pt         : TPoint;
    TagID      : Integer;
    ParentID   : Integer;
    ErrMsg     : String;
    DCaption   : String;
begin
    Pt   := TagsVirtualStringTree.ScreenToClient(TagTreePopupMenu.PopupPoint);
    Node := TagsVirtualStringTree.GetNodeAt(Pt);
    if not Assigned(Node) then begin
        ParentID := 1;   // Root
        DCaption := _('Top level tag');
    end
    else begin
        Data := TagsVirtualStringTree.GetNodeData(Node);
        if not Assigned(Data) then
            Exit;
        if not Assigned(Data.Obj) then
            Exit;
        TagID := OvbImgOrganizerDataModule.DBGetTagId(Data.Obj.TagName, ParentID, ErrMsg);
        if TagID < 0 then begin
            TMessageForm.ShowMessage(ErrMsg, Caption,
                                     nil, [mffFailed]);
            Exit;    // Error searching for tag
        end;
        if TagID = 0 then begin
            TMessageForm.ShowMessage(
                 Format(_('Tag "%s" not found'), [Data.Obj.TagName]),
                 Caption, nil, [mffFailed]);
            Exit;
        end;
        DCaption := Format(_('New Tag (Sibling of %s)'), [Data.Obj.TagName]);
    end;

    SetLength(APrompts, 1);
    SetLength(AValues, 1);
    APrompts[0] := _('Enter tag names');
    AValues[0]  := '';
    if not InputQuery(DCaption, APrompts, AValues) then
        Exit;
    TagsAddNew(AValues, ParentID);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagTreeAddNewAsChildTagPopupMnuClick(
    Sender: TObject);
var
    APrompts     : array of String;
    AValues      : array of String;
    Node         : PVirtualNode;
    Data         : POvbImgTreeData;
    Pt           : TPoint;
    ParentTag    : String;
begin
    Pt   := TagsVirtualStringTree.ScreenToClient(TagTreePopupMenu.PopupPoint);
    Node := TagsVirtualStringTree.GetNodeAt(Pt);
    if Assigned(Node) then begin
        // There is a node under cursor at popup point
        // Get which TagName is there
        Data := TagsVirtualStringTree.GetNodeData(Node);
        if not Assigned(Data) then
            Exit;
        if not Assigned(Data.Obj) then
            Exit;
        ParentTag := Data.Obj.TagName;
    end
    else begin
        // Use clicked outside of any node, create at top level
        ParentTag := '';
    end;

    SetLength(APrompts, 1);
    SetLength(AValues, 1);
    APrompts[0] := _('Enter tag names');
    AValues[0]  := '';
    if not InputQuery(Format(_('New Tag (Child of %s)'), [ParentTag]),
                      APrompts, AValues) then
        Exit;
    TagsAddNew(AValues, ParentTag);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerMainForm.TagTreeFindAndExpandNode(
    const FromNode : PVirtualNode;
    const TagName  : String): PVirtualNode;
var
    Node         : PVirtualNode;
    Data         : POvbImgTreeData;
    DataFrom     : POvbImgTreeData;
begin
    Result := nil;
    if Assigned(FromNode) then begin
        DataFrom := TagsVirtualStringTree.GetNodeData(FromNode);
        if not DataFrom.Obj.Loaded then
            TagsVirtualStringTree.Expanded[FromNode] := TRUE;
    end;

    Node := TagsVirtualStringTree.GetFirstChild(FromNode);
    while Assigned(Node) do begin
        Data := TagsVirtualStringTree.GetNodeData(Node);
        if Assigned(Data) and Assigned(Data.Obj) then begin
            if SameText(Data.Obj.TagName, ':DUMMY') then
                Exit;
            if SameText(Data.Obj.TagName, TagName) then begin
                Result := Node;
                Exit;
            end;
            // Recursively search in children
            Result := TagTreeFindAndExpandNode(Node, TagName);
            if Assigned(Result) then
                Exit;
        end;
        Node := TagsVirtualStringTree.GetNextSibling(Node);
    end;
    TagsVirtualStringTree.Expanded[FromNode] := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerMainForm.TagsAddNew(
    const AValues    : array of String;
    const ATagParent : String) : Boolean;
var
    ParentID     : Integer;
    ErrMsg       : String;
begin
    if ATagParent = '' then
        ParentID := 1
    else begin
        ParentID := OvbImgOrganizerDataModule.DBGetTagID(ATagParent, ErrMsg);
        if ParentID < 0 then begin
            TMessageForm.ShowMessage(ErrMsg, Caption,
                                     nil, [mffFailed]);
            Result := FALSE;
            Exit;
        end;
    end;
    Result := TagsAddNew(Avalues, ParentID);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerMainForm.TagsAddNew(
    const AValues    : array of String;
    const AParentID  : Integer) : Boolean;
var
    TagNames     : TStringDynArray;
    TagName      : String;
    I            : Integer;
    ID           : Int64;
    WasConnected : Boolean;
    Status       : Integer;
    ErrMsg       : String;
begin
    Result := FALSE;
    if not OvbImgOrganizerDataModule.DBConnect(WasConnected, ErrMsg) then begin
        TMessageForm.ShowMessage(ErrMsg, Caption,
                                 nil, [mffFailed]);
        Exit;
    end;
    try
        TagNames := SplitString(Trim(AValues[0]), ' '#9#13#10);
        for I := Low(TagNames) to High(TagNames) do begin
            TagName := Trim(TagNames[I]);
            if not OvbImgOrganizerDataModule.ValidateTagName(TagName, ErrMsg) then begin
                TMessageForm.ShowMessage(ErrMsg, Caption,
                                         nil, [mffFailed]);
                continue;
            end;

            // Check if tag name already exists
            Status := OvbImgOrganizerDataModule.DBGetTagID(TagName, ErrMsg);
            if Status < 0 then begin
                ShowMessage(ErrMsg);
                continue;
            end;
            if Status > 0 then begin
                // Tag already exists, that's OK
                continue;
            end;

            ID := OvbImgOrganizerDataModule.DBInsertTag(TagName, AParentID, ErrMsg);
            if ID < 0 then begin
                ShowMessage(ErrMsg);
                Exit;
            end;

//            Display('Tag "%s" inserted. ID=%d', [TagName, ID]);

            // If we are in image drop feature, add also the tag to that list
            if PageControl1.ActivePage = NewImageTabSheet then
                ImageDropTagListBox.Items.Add(TagName);
        end;

        // Reload the tag tree
        TagTreeLoad;

        // Select first tag in the list or tree
        if Length(TagNames) > 0 then begin
            TagTreeFindAndExpandNode(nil, TagNames[0]);
        end;

        Result := TRUE;
    finally
        if not WasConnected then
            OvbImgOrganizerDataModule.DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.AddTagToAllSelectedImagesTreePopupMnuClick(
    Sender: TObject);
var
    TagName      : String;
    TagID        : Integer;
    ImagePanel   : TOvbImagePanel;
    ImageInfo    : TOvbImgImageInfo;
    Node         : PVirtualNode;
    Data         : POvbImgTreeData;
    Pt           : TPoint;
    ErrMsg       : String;
begin
    Pt   := TagsVirtualStringTree.ScreenToClient(TagTreePopupMenu.PopupPoint);
    Node := TagsVirtualStringTree.GetNodeAt(Pt);
    if not Assigned(Node) then
        Exit;
    Data := TagsVirtualStringTree.GetNodeData(Node);
    if not Assigned(Data) then
        Exit;
    if not Assigned(Data.Obj) then
        Exit;
    TagName := Data.Obj.TagName;
    TagID   := OvbImgOrganizerDataModule.DBGetTagID(TagName, ErrMsg);
    if TagID < 0 then begin
        ShowMessage(ErrMsg);
        Exit;
    end;
    for ImagePanel in FCollection.Selected do begin
        ImageInfo := ImagePanel.TagObject as TOvbImgImageInfo;
        if not Assigned(ImageInfo) then
            continue;
        if OvbImgOrganizerDataModule.DBInsertTagForImage(ImageInfo.ImageID,
                                           TagID, ErrMsg) < 0 then
            ShowMessage(ErrMsg);
        ImageInfo.TagFetched := FALSE;
    end;
    if Assigned(FCollection.LastFocusedObject) then begin
        ImageInfo := FCollection.LastFocusedObject.TagObject  as TOvbImgImageInfo;
        OvbImgOrganizerDataModule.DBFetchImageTagNames(ImageInfo.ImageID, ImageInfo.TagNames, ErrMsg);
        ImageTagListBox.Items := ImageInfo.TagNames;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageDropAllowedHandler(
    Sender            : TObject;
    const FileNames   : array of String;
    const GrfKeyState : Integer;
    const Pt          : TPoint;
    var   Allowed     : Boolean);
var
    I : Integer;
begin
    SetLength(FDropFileNames, Length(FileNames));
    for I := Low(FileNames) to High(FileNames) do
        FDropFileNames[I] := FileNames[I];
    FDropAllowed   := IsFileDropAllowed(FDropFileNames);
    Allowed        := FDropAllowed;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// This handler is called when an image is dropped from Windows Explorer
procedure TOvbImgOrganizerMainForm.ImageDropHandler(
    Sender          : TObject;
    const DropPoint : TPoint;
    const FileNames : array of String);
begin
    PasteFileNameList(FileNames);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.PasteFileNameList(
    const FileNames : array of String);
var
    I : Integer;
begin
    FDropInfo.CountDropped := FDropInfo.CountDropped + Length(FileNames);
    PageControl1.ActivePage := NewImageTabSheet;
    DropImageCountLabel.Caption := Format(_('Loading %d image'),
                                          [Length(FileNames)]);
    for I := Low(FileNames) to High(FileNames) do begin
        if IsFileDropAllowed(FileNames[I]) then
            FDropInfo.FileNames.Add(FileNames[I])
        else
            Display(_('Ignoring image "%s"'), [FileNames[I]]);
    end;
    ImageDropAutoProcessCheckBox.Checked := FALSE;
    ImageDropProcessNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageDropProcessNext;
var
    ThumbFilename    : String;
    TagList          : TStringList;
    TagName          : String;
    ImagePath        : String;
    DummyImageID     : Integer;
    ErrMsg           : String;
    ExistingFile     : String;
    ExistingFileName : String;
    ExistingFileExt  : String;
    ExistingFilePath : String;
    WarnMsg          : String;
begin
    if FDropInfo.FileNames.Count = 0 then begin
        FDropPreviewImagePanel.CloseImage;
        FDropPreviewImagePanel.Hide;
        ImageDropCancelButton.Visible          := FALSE;
        ImageDropAddToCollectionButton.Visible := FALSE;
        ImageDropSkipButton.Visible            := FALSE;
        ImageDropTagListBox.Visible            := FALSE;
        ImageDropAutoProcessCheckBox.Visible   := FALSE;
        ImageDropAutoProcessCheckBox.Checked   := FALSE;
        DropImageViewCollectionButton.Visible  := TRUE;
        ActiveControl                          := DropImageViewCollectionButton;
        Display(_('ImageDrop loaded %d file, skipped %d file'),
                [FDropInfo.CountLoaded, FDropInfo.CountSkipped]);
        Display(_('ImageDrop done'));
        DropImageCountLabel.Font.Color := clWindowText;
        DropImageCountLabel.Font.Style := [];
        DropImageCountLabel.Caption :=
               Format(_('Done, loaded %d image, %d skipped image'),
                      [FDropInfo.CountLoaded, FDropInfo.CountSkipped]);
        FDropInfo.Clear;
        Exit;
    end;
    DropImageViewCollectionButton.Visible  := FALSE;
    ImageDropAutoProcessCheckBox.Visible   := TRUE;
    ImageDropAddToCollectionButton.Visible := TRUE;
    ImageDropCancelButton.Visible          := TRUE;
    ImageDropSkipButton.Visible            := TRUE;
    ImageDropTagListBox.Visible            := TRUE;
    ImageTagListBox.Clear;
    FDropPreviewImagePanel.Show;
    FDropInfo.FileName := FDropInfo.FileNames[0];
    Display(_('ImageDrop adding file %s'), [FDropInfo.FileName]);
    DropPreviewImagePanelLoad(FDropInfo.FileName, ThumbFilename);
    FDropInfo.ThumbFilename := ThumbFilename;
    ActiveControl := ImageDropAddToCollectionButton;

    FDropInfo.ImageID := OvbImgOrganizerDataModule.DBGetImageID(
                  ChangeFileExt(ExtractFileName(FDropInfo.FileName), ''),
                  ExtractFileExt(FDropInfo.FileName),
                  ImagePath,
                  ErrMsg);
    if FDropInfo.ImageID < 0 then begin
        ShowMessage(ErrMsg);
        Exit;
    end;

    if FDropInfo.ImageID = 0 then begin
        // Image not in database, that is OK
        DropImageCountLabel.Font.Color := clWindowText;
        DropImageCountLabel.Font.Style := [];
        DropImageCountLabel.Caption :=
              Format(_('New Image %d/%d'),
                     [1 + FDropInfo.CountDropped - FDropInfo.FileNames.Count,
                      FDropInfo.CountDropped]);
        ImageDropAddToCollectionButton.Caption := _('Add to collection');
    end
    else if FDropInfo.ImageID > 0 then begin
        DropImageCountLabel.Font.Color := clRed;
        DropImageCountLabel.Font.Style := [fsBold];
        DropImageCountLabel.Caption :=
              Format(_('Image already exists in DB %d/%d'),
                     [1 + FDropInfo.CountDropped - FDropInfo.FileNames.Count,
                      FDropInfo.CountDropped]);

        OvbImgOrganizerDataModule.DBFetchImageInfo(FDropInfo.ImageID,
                                     ExistingFileName,
                                     ExistingFileExt,
                                     ExistingFilePath,
                                     ErrMsg);
        ExistingFile     := ExistingFilePath + ExistingFileName + ExistingFileExt;

        if not SameText(FDropInfo.FileName, ExistingFile) then begin
            WarnMsg := _('Image already exists in DB but not at same place!');
            if CompareFileContent(FDropInfo.FileName, ExistingFile) then
                WarnMsg := WarnMsg + CRLF +
                           _('Both have the same content.')
            else
                WarnMsg := WarnMsg + CRLF +
                           _('Those are two different files.');
            WarnMsg := WarnMsg + CRLF + CRLF +
                       _('Existing:')     + ' ' + ExistingFile + CRLF +
                       _('Dropped file:') + ' ' + FDropInfo.FileName;

            TMessageForm.ShowMessage(WarnMsg, Caption, nil,
                                     [mffWarning, mffOK, mffAutoWidth]);
            ImageDropSkipButtonClick(nil);
            Exit;
        end;

        ImageDropAddToCollectionButton.Caption := _('Update tags');
        TagList := TStringList.Create;
        try
            if not OvbImgOrganizerDataModule.DBFetchImageTagNames(FDropInfo.FileName,
                                                    DummyImageID,
                                                    TagList,
                                                    ErrMsg) then begin
                ShowMessage(ErrMsg);
                Exit;
            end;
            ImageTagListBox.Items := TagList;
            for TagName in TagList do begin
                // If not already there, add the tag to the list
                if ImageDropTagListBox.Items.IndexOf(TagName) < 0 then
                    ImageDropTagListBox.Items.Add(TagName);
            end;
        finally
            FreeAndNil(TagList);
        end;
    end;
    if ImageDropAutoProcessCheckBox.Checked then
        PostMessage(Handle, WM_AUTO_ADD_TO_COLLECTION, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageDropRemoveTagFromListPopupMnuClick(
    Sender: TObject);
var
    Index : Integer;
begin
    Index := ImageDropTagListBox.ItemIndex;
    if Index < 0 then
        Exit;
    ImageDropTagListBox.Items.Delete(Index);
    if Index >= ImageDropTagListBox.Items.Count then
        Index := ImageDropTagListBox.Items.Count - 1;
    ImageDropTagListBox.ItemIndex := Index;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageDropSkipButtonClick(Sender: TObject);
begin
    if FDropInfo.FileNames.Count > 0 then
        FDropInfo.FileNames.Delete(0);
    Inc(FDropInfo.CountSkipped);
    FDropInfo.FileName      := '';
    FDropInfo.ThumbFileName := '';
    ImageDropProcessNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageDropTagListBoxDblClick(Sender: TObject);
var
    Index : Integer;
begin
    Index := ImageDropTagListBox.ItemIndex;
    if Index < 0 then
        Exit;
    ImageDropTagListBox.Items.Delete(Index);
    if Index >= ImageDropTagListBox.Items.Count then
        Index := ImageDropTagListBox.Items.Count - 1;
    ImageDropTagListBox.ItemIndex := Index;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageDropTagListBoxMouseDown(
    Sender : TObject;
    Button : TMouseButton;
    Shift  : TShiftState;
    X, Y   : Integer);
var
    Index : Integer;
begin
    if Button = mbRight then begin
        Index := ImageDropTagListBox.ItemAtPos(Point(X, Y), TRUE);
        ImageDropTagListBox.ItemIndex := Index;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageDropAddToCollectionButtonClick(
    Sender: TObject);
var
    TagNames     : TStrings;
    TagName      : String;
    WasConnected : Boolean;
    TagIdList    : TOvbImgListOfInteger;
    TagId        : Integer;
    ImageDesc    : TOvbImgImageDesc;
    ErrMsg       : String;
begin
    if FDropInfo.FileNames.Count <= 0 then
        Exit;

    TagNames  := ImageDropTagListBox.Items;
    TagIdList := TOvbImgListOfInteger.Create;
    try
        if not OvbImgOrganizerDataModule.DBConnect(WasConnected, ErrMsg) then begin
            ShowMessage(ErrMsg);
            Exit;
        end;
        try
            for TagName in TagNames do begin
                TagID := OvbImgOrganizerDataModule.DBGetTagID(TagName, ErrMsg);
                if TagID < 0 then begin
                    ShowMessage(ErrMsg);
                    Exit;
                end;
                if TagID = 0 then begin
                    ShowMessage(Format(_('Tag "%s" doesn''t exist'),
                                       [TagName]));
                    continue;
                end;
                TagIdList.Add(TagID);
            end;

            ImageDesc.Clear;
            ImageDesc.FileName      := FDropInfo.FileName;
            ImageDesc.ThumbFilename := FDropInfo.ThumbFilename;
            ImageDesc.Latitude      := FDropPreviewImagePanel.GpsCoord.Lat;
            ImageDesc.Longitude     := FDropPreviewImagePanel.GpsCoord.Lon;
            ImageDesc.Description   := '';
            FDropInfo.ImageID       := OvbImgOrganizerDataModule.DBInsertImage(
                                               ImageDesc,
                                               ErrMsg);
            if FDropInfo.ImageID < 0 then begin
                ShowMessage(ErrMsg);
                Exit;
            end;
            for TagId in TagIdList do begin
                if OvbImgOrganizerDataModule.DBInsertTagForImage(FDropInfo.ImageID,
                                                   TagId,
                                                   ErrMsg) < 0 then begin
                    ShowMessage(ErrMsg);
                    exit;
                end;
            end;
        finally
            if not WasConnected then
                OvbImgOrganizerDataModule.DBDisconnect;
        end;
        Inc(FDropInfo.CountLoaded);
        FDropInfo.FileName      := '';
        FDropInfo.ThumbFileName := '';
        FDropInfo.FileNames.Delete(0);
        ImageDropProcessNext;
    finally
        FreeAndNil(TagIdList);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageDropCancelButtonClick(Sender: TObject);
begin
    FDropPreviewImagePanel.CloseImage;
    FDropPreviewImagePanel.Hide;
    FDropInfo.Clear;
    ImageDropCancelButton.Visible        := FALSE;
    ImageDropAutoProcessCheckBox.Checked := FALSE;
    PageControl1.ActivePage              := CollectionTabSheet;
    Display(_('ImageDrop canceled'));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageDropClearTagListPopupMnuClick(
    Sender: TObject);
begin
    ImageDropTagListBox.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageDragOverHandler(
    Sender            : TObject;
    const GrfKeyState : Longint;
    const Pt          : TPoint;
    var   Allowed     : Boolean);
begin
    Allowed := FDropAllowed;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Event handler called when cursor leave the window while dragging an
// image from Windows Explorer.
procedure TOvbImgOrganizerMainForm.ImageDragLeaveHandler(Sender: TObject);
begin
    // Intentionaly left empty
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.DropPreviewImagePanelLoad(
    const FileName      : String;
    out   ThumbFilename : String);
var
    ImageName    : String;
    ImageExt     : String;
begin
    if not FDropPreviewImagePanel.LoadImage(FileName) then begin
        Display(_('LoadImage failed for %s'), [FileName]);
        FDropPreviewImagePanel.Caption := _('Empty');
        Exit;
    end;

    ImageName                      := ExtractFileName(FileName);
    ImageExt                       := ExtractFileExt(ImageName);
    ImageName                      := ChangeFileExt(ImageName, '');
    FDropPreviewImagePanel.Caption := ImageName;
    if ImageExt = '' then
        ImageExt    := '_'
    else
        ImageExt[1] := '_';

    // We use CRC16 as hash code for ImageName and use that hash code as
    // a subdirectory name (4 hex digits).
    // This is to avoid writing thousands of files in a unique directory
    // which is slow on some file systems.
    // The file is not created now. The filename will be stored in the database
    // and will be used to create the actual thumbnail when the image has
    // to be shown.
    ThumbFilename := THUMB_CACHE_DIR + '\' + IntToHex(CRC16(ImageName), 4) +
                     '\' + ImageName + ImageExt + '.jpg';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageTagListBoxDblClick(Sender: TObject);
begin
    TagRemoveFromFocused(ImageTagListBox);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageTagListBoxMouseDown(
    Sender : TObject;
    Button : TMouseButton;
    Shift  : TShiftState;
    X, Y   : Integer);
var
    Index : Integer;
begin
    if Button = mbRight then begin
        FImageTagListRightClickAt := Point(X, Y);
        Index                     := ImageTagListBox.ItemAtPos(FImageTagListRightClickAt, TRUE);
        ImageTagListBox.ItemIndex := Index;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageTagListBoxPopupMenuPopup(
    Sender: TObject);
begin
    ImageTagListBox.SetFocus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ScrollBoxPastePopupMnuClick(Sender: TObject);
begin
    PasteFilesFromClipBoard;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImagesSelectAll;
var
    I          : Integer;
    ImagePanel : TOvbImagePanel;
begin
    for I := CollectionFlowPanel.ControlCount - 1 downto 0 do begin
        ImagePanel := CollectionFlowPanel.Controls[I] as TOvbImagePanel;
        ImagePanel.CheckBox.Checked := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.SelectAllImagesPopupMnuClick(
    Sender: TObject);
begin
    ImagesSelectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.SelectImagePopupMnuClick(Sender: TObject);
var
    Pt         : TPoint;
    Ctrl       : TControl;
    ImagePanel : TOvbImagePanel;
begin
    Pt   := CollectionFlowPanel.ScreenToClient(CollectionPopupMenu.PopupPoint);
    Ctrl := CollectionFlowPanel.ControlAtPos(Pt, TRUE, TRUE, FALSE);
    if not (Ctrl is TOvbImagePanel) then
        Exit;
    ImagePanel := TOvbImagePanel(Ctrl);
    ImagePanel.CheckBox.Checked := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.DeselectImagePopupMnuClick(Sender: TObject);
var
    Pt         : TPoint;
    Ctrl       : TControl;
    ImagePanel : TOvbImagePanel;
begin
    Pt   := CollectionFlowPanel.ScreenToClient(CollectionPopupMenu.PopupPoint);
    Ctrl := CollectionFlowPanel.ControlAtPos(Pt, TRUE, TRUE, FALSE);
    if not (Ctrl is TOvbImagePanel) then
        Exit;
    ImagePanel := TOvbImagePanel(Ctrl);
    ImagePanel.CheckBox.Checked := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.OpenAllSelectedImagesPopupMnuClick(
    Sender: TObject);
var
    I          : Integer;
begin
    for I := 0 to FCollection.Selected.Count - 1 do
        ImagePanelShellOpen(Handle, FCollection.Selected[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ShowInExplorerPopupMnuClick(
    Sender: TObject);
var
    Pt         : TPoint;
    Ctrl       : TControl;
begin
    Pt   := CollectionFlowPanel.ScreenToClient(CollectionPopupMenu.PopupPoint);
    Ctrl := CollectionFlowPanel.ControlAtPos(Pt, TRUE, TRUE, FALSE);
    ImagePanelShellOpenContainingFolder(Ctrl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.SlidShowFormDestroyHandler(Sender: TObject);
begin
    FSlideShowList.Remove(Sender as TSlideShowForm);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.SlidShowBitBtnClick(Sender: TObject);
var
    Form       : TSlideShowForm;
    ImagePanel : TOvbImagePanel;
    ImageInfo  : TOvbImgImageInfo;
begin
    Form           := TSlideShowForm.Create(Self);
    Form.OnDestroy := SlidShowFormDestroyHandler;
    Form.OnDisplay := DisplayHandler;
    FSlideShowList.Add(Form);

    if FCollection.Selected.Count > 0 then begin
        for ImagePanel in FCollection.Selected do begin
            ImageInfo := ImagePanel.TagObject as TOvbImgImageInfo;
            if not Assigned(ImageInfo) then
                continue;
            Form.Filenames.Add(ImageInfo.ImagePath + ImageInfo.ImageName);
        end;
    end;

    if Form.Filenames.Count = 0 then begin
        if (not Assigned(FCollection.FocusedObject)) or
           (FCollection.FocusedObject.TagObject = nil) then begin
            TMessageForm.ShowMessage(_('No image selected'),
                                     Caption, nil,
                                     [mffWarning, mffAutoWidth]);
            Exit;
        end;
        ImageInfo := FCollection.FocusedObject.TagObject as TOvbImgImageInfo;
        Form.Filenames.Add(ImageInfo.ImagePath + ImageInfo.ImageName);
    end;

    CenterChildForm(Form);
    Form.Show;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.CenterChildForm(Form : TCustomForm);
var
    FormCenter : TPoint;
begin
    // Center the form on the main form, but shift each new form a
    // little bit so that they are not all at the same place. Make
    // sure the title bar is visible (no negative coordinates)
    FormCenter.X := Left + Width  div 2;
    FormCenter.Y := Top  + Height div 2;
    Form.Left    := FormCenter.X - Form.Width  div 2;
    Form.Top     := FormCenter.Y - Form.Height div 2;
    if Form.Top < 0 then
        Form.Top := 0;
    if Form.Left < 0 then
        Form.Left := 0;
    Form.Left    := Form.Left + (FSlideShowList.Count - 1) * GetSystemMetrics(SM_CYMENU);
    Form.Top     := Form.Top  + (FSlideShowList.Count - 1) * GetSystemMetrics(SM_CYMENU);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.OpenImagePopupMnuClick(Sender: TObject);
var
    Pt         : TPoint;
    Ctrl       : TControl;
begin
    Pt   := CollectionFlowPanel.ScreenToClient(CollectionPopupMenu.PopupPoint);
    Ctrl := CollectionFlowPanel.ControlAtPos(Pt, TRUE, TRUE, FALSE);
    ImagePanelShellOpen(Handle, Ctrl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.RemoveAllSelectedImagesFromCollectionPopupMnuClick(
    Sender: TObject);
var
    Status      : Integer;
    ImageIDList : TOvbImgListOfInteger;
    ImagePanel  : TOvbImagePanel;
    ImageInfo   : TOvbImgImageInfo;
    ErrMsg      : String;
begin
    if FCollection.Selected.Count <= 0 then
        Exit;
    Status := TMessageForm.ShowMessage(
                  Format(_('Remove ALL (%d) selected images from collection?'),
                         [FCollection.Selected.Count]),
                  Caption, nil,
                  [mffYesNo, mffAutoWidth]);
    if Status <> IDYES then
        Exit;
    ImageIDList := TOvbImgListOfInteger.Create;
    try
        for ImagePanel in FCollection.Selected do begin
            ImageInfo  := ImagePanel.TagObject as TOvbImgImageInfo;
            if Assigned(ImageInfo) then
                ImageIDList.Add(ImageInfo.ImageID);
            ThumbnailDelete(ImageInfo.ImageThumb);
        end;

        if not OvbImgOrganizerDataModule.DBDeleteImages(ImageIDList, ErrMsg) then
            ShowMessage(ErrMsg);
    finally
        FreeAndNil(ImageIDList);
    end;
    CollectionReload;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ThumbnailDelete(
    const Thumbnail : String);
var
    FileName : String;
    FileDir  : String;
begin
    FileName := FCommonAppData + Thumbnail;
    FileDir  := ExcludeTrailingPathDelimiter(ExtractFilePath(FileName));
    DeleteFile(FileName);  // No need to check for success
    RemoveDir(FileDir);    // No need to check for success
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.RemoveImageFromCollectionPopupMnuClick(
    Sender: TObject);
var
    Pt         : TPoint;
    Ctrl       : TControl;
    ImagePanel : TOvbImagePanel;
    ImageInfo  : TOvbImgImageInfo;
    Status     : Integer;
    ErrMsg     : String;
begin
    Pt   := CollectionFlowPanel.ScreenToClient(CollectionPopupMenu.PopupPoint);
    Ctrl := CollectionFlowPanel.ControlAtPos(Pt, TRUE, TRUE, FALSE);
    if not (Ctrl is TOvbImagePanel) then
        Exit;
    ImagePanel := Ctrl as TOvbImagePanel;
    ImageInfo  := ImagePanel.TagObject as TOvbImgImageInfo;
    Status := TMessageForm.ShowMessage(
                  Format(_('Remove image "%s" from collection?'),
                         [ImageInfo.ImageName]),
                  Caption, nil,
                  [mffYesNo, mffAutoWidth]);
    if Status <> IDYES then
        Exit;
    ThumbnailDelete(ImageInfo.ImageThumb);
    if not OvbImgOrganizerDataModule.DBDeleteImage(ImageInfo.ImageID, ErrMsg) then
        ShowMessage(ErrMsg);
    CollectionReload;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.RemoveImageFromViewPopupMnuClick(
    Sender: TObject);
var
    Pt          : TPoint;
    Ctrl        : TControl;
    ImagePanel  : TOvbImagePanel;
begin
    Pt   := CollectionFlowPanel.ScreenToClient(CollectionPopupMenu.PopupPoint);
    Ctrl := CollectionFlowPanel.ControlAtPos(Pt, TRUE, TRUE, FALSE);
    if not (Ctrl is TOvbImagePanel) then
        Exit;

    ImagePanel := TOvbImagePanel(Ctrl);
    ImageRemoveFromView(ImagePanel);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ImageRemoveFromView(
    ImagePanel  : TOvbImagePanel);
var
    ImagePanel2 : TOvbImagePanel;
    Index       : Integer;
    I           : Integer;
begin
    ImagePanel.HasFocus := FALSE;
    Index               := ImagePanel.Index;
    // We remove an image from the FlowPanel, we must fix all indexes
    for I := Index + 1 to CollectionFlowPanel.ControlCount - 1 do
        (CollectionFlowPanel.Controls[I] as TOvbImagePanel).Index := I - 1;

    if Index >= (CollectionFlowPanel.ControlCount - 1) then
        Dec(Index);
    FreeAndNil(ImagePanel);
    if Index >= 0 then begin
        ImagePanel2   := CollectionFlowPanel.Controls[Index] as TOvbImagePanel;
        ActiveControl := ImagePanel2.CheckBox;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.RemoveTagFromAllSelectedImagesPopupMnuClick(
    Sender: TObject);
var
    Index        : Integer;
    TagName      : String;
    TagID        : Integer;
    ImagePanel   : TOvbImagePanel;
    ImageInfo    : TOvbImgImageInfo;
    Status       : Integer;
    ErrMsg       : String;
begin
    Index := ImageTagListBox.ItemIndex;
    if Index < 0 then
        Exit;
    TagName := ImageTagListBox.Items[Index];
    TagID   := OvbImgOrganizerDataModule.DBGetTagID(TagName, ErrMsg);
    if TagID < 0 then begin
        Showmessage(ErrMsg);
        Exit;
    end;
    if TagID = 0 then begin
        ShowMessage(Format(_('Tag "%s" doesn''t exist'), [TagName]));
        Exit;
    end;
    if FCollection.Selected.Count <= 0 then begin
        ShowMessage(_('No image selected'));
        Exit;
    end;

    Status := TMessageForm.ShowMessage(
                  Format(_('Remove tag "%s" from %d images.'),
                         [TagName, FCollection.Selected.Count]) + CRLF +
                  _('Are you sure?'),
                  Caption, nil,
                  [mffYesNo, mffAutoWidth]);
    if Status <> IDYES then
        Exit;

    for ImagePanel in FCollection.Selected do begin
        ImageInfo := ImagePanel.TagObject as TOvbImgImageInfo;
        if not Assigned(ImageInfo) then
            continue;
        if OvbImgOrganizerDataModule.DBDeleteTagForImage(ImageInfo.ImageID,
                                               TagID,
                                               ErrMsg) < 0 then
            ShowMessage(ErrMsg);
        ImageInfo.TagFetched := FALSE;
    end;
    if Assigned(FCollection.LastFocusedObject) then begin
        ImageInfo := FCollection.LastFocusedObject.TagObject  as TOvbImgImageInfo;
        if not OvbImgOrganizerDataModule.DBFetchImageTagNames(ImageInfo.ImageID,
                                                ImageInfo.TagNames,
                                                ErrMsg) then
            ShowMessage(ErrMsg);
        ImageTagListBox.Items := ImageInfo.TagNames;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.RemoveTagFromFocusedImagePopupMnuClick(
    Sender: TObject);
begin
    TagRemoveFromFocused(ImageTagListBox);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagRemoveFromFocused(Ctrl : TWinControl);
var
    Index        : Integer;
    TagName      : String;
    TagID        : Integer;
    Status       : Integer;
    ImagePanel   : TOvbImagePanel;
    ImageInfo    : TOvbImgImageInfo;
    WasConnected : Boolean;
    ErrMsg       : String;
    ListBox      : TListBox;
begin
    if not Assigned(FCollection.LastFocusedObject) then
        Exit;
    if not (Ctrl is TListBox) then
        Exit;
    ListBox := TListBox(Ctrl);

    Index := ListBox.ItemIndex;
    if Index < 0 then
        Exit;
    TagName := ListBox.Items[Index];
    TagID   := OvbImgOrganizerDataModule.DBGetTagID(TagName, ErrMsg);
    if TagID < 0 then begin
        ShowMessage(ErrMsg);
        Exit;
    end;
    if TagID = 0 then begin
        ShowMessage(Format(_('Tag "%s" doesn''t exist'), [TagName]));
        Exit;
    end;

    ImagePanel := FCollection.LastFocusedObject;
    ImageInfo  := ImagePanel.TagObject as TOvbImgImageInfo;

    Status := TMessageForm.ShowMessage(
                  Format(_('Remove tag "%s" from image "%s".'),
                         [TagName, ImageInfo.ImageName]) + CRLF +
                  _('Are you sure?'),
                  Caption, nil,
                  [mffYesNo, mffAutoWidth]);
    if Status <> IDYES then
        Exit;

    if not OvbImgOrganizerDataModule.DBConnect(WasConnected, ErrMsg) then begin
        ShowMessage(ErrMsg);
        Exit;
    end;
    try
        OvbImgOrganizerDataModule.DBDeleteTagForImage(ImageInfo.ImageID, TagID, ErrMsg);
        OvbImgOrganizerDataModule.DBFetchImageTagNames(ImageInfo.ImageID, ImageInfo.TagNames, ErrMsg);
        ListBox.Items := ImageInfo.TagNames;
        if Index < ListBox.Items.Count then
            ListBox.ItemIndex := Index;
    finally
        if not WasConnected then
            OvbImgOrganizerDataModule.DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerMainForm.TagRemove(const TagName : String) : Boolean;
var
    Status       : Integer;
    UseCount     : Integer;
    ImagePanel   : TOvbImagePanel;
    ImageInfo    : TOvbImgImageInfo;
    WasConnected : Boolean;
    ErrMsg       : String;
begin
    Result := FALSE;
    Display(_('Removing tag "%s"'), [TagName]);
    if not OvbImgOrganizerDataModule.DBConnect(WasConnected, ErrMsg) then begin
        ShowMessage(ErrMsg);
        Exit;
    end;
    try
        UseCount := OvbImgOrganizerDataModule.DBGetTagUseCount(TagName, ErrMsg);
        if UseCount < 0 then begin
            ShowMessage(ErrMsg);
            Exit;
        end;
        if UseCount > 0 then begin
            Status := TMessageForm.ShowMessage(
                          Format(_('Tag "%s" is used by %d images'),
                                 [TagName, UseCount]) + CRLF +
                          _('Are you sure you want to delete this tag?'),
                          Caption, nil,
                          [mffYesNo, mffAutoWidth]);
            if Status <> IDYES then
                Exit;
        end
        else begin
            Status := TMessageForm.ShowMessage(
                Format(_('Are you sure you want to delete the (unused) tag "%s"?'),
                       [TagName]),
                Caption, nil,
                [mffYesNo, mffAutoWidth]);
            if Status <> IDYES then
                Exit;
        end;
        OvbImgOrganizerDataModule.DBDeleteTag(TagName, ErrMsg);
        // Refresh tag tree
        TagTreeLoad;

        for ImagePanel in FCollection.Selected do begin
            ImageInfo := ImagePanel.TagObject  as TOvbImgImageInfo;
            if Assigned(ImageInfo) then begin
                if ImageInfo.TagNames.IndexOf(TagName) >= 0 then
                    OvbImgOrganizerDataModule.DBFetchImageTagNames(ImageInfo.ImageID, ImageInfo.TagNames, ErrMsg);
            end;
        end;

        if Assigned(FCollection.LastFocusedObject) then begin
            ImageInfo := FCollection.LastFocusedObject.TagObject  as TOvbImgImageInfo;
            if Assigned(ImageInfo) then begin
                OvbImgOrganizerDataModule.DBFetchImageTagNames(ImageInfo.ImageID, ImageInfo.TagNames, ErrMsg);
                ImageTagListBox.Items := ImageInfo.TagNames;
            end;
        end;
        Result := TRUE;
    finally
        if not WasConnected then
            OvbImgOrganizerDataModule.DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.RemoveTagFromTagTreePopupMnuClickClick(
    Sender: TObject);
var
    Node         : PVirtualNode;
    NodeNext     : PVirtualNode;
    NodeChild    : PVirtualNode;
    Data         : POvbImgTreeData;
    DataNext     : POvbImgTreeData;
    Pt           : TPoint;
    TagName      : String;
    TagNameNext  : String;
begin
    Pt   := TagsVirtualStringTree.ScreenToClient(TagTreePopupMenu.PopupPoint);
    Node := TagsVirtualStringTree.GetNodeAt(Pt);
    if not Assigned(Node) then
        Exit;
    Data := TagsVirtualStringTree.GetNodeData(Node);
    if not Assigned(Data) then
        Exit;
    if not Assigned(Data.Obj) then
        Exit;
    TagName := Data.Obj.TagName;
    if Node.ChildCount > 0 then begin
//        Display('ChildCount=%d', [Node.ChildCount]);
        if Node.ChildCount >= 1 then begin
            NodeChild := Node.FirstChild;
            Data      := TagsVirtualStringTree.GetNodeData(NodeChild);
            if (Node.ChildCount > 1) or
               (Data.Obj.TagName <> ':DUMMY') then begin
                ShowMessage(_('Can''t remove a tag having one or more child'));
                Exit;
            end;
        end;
    end;

    // Try to find something interesting to show after deletion
    TagNameNext := '';
    NodeNext := TagsVirtualStringTree.GetNextSibling(Node);
    if not Assigned(NodeNext) then begin
        NodeNext := TagsVirtualStringTree.GetPreviousSibling(Node);
        if not Assigned(NodeNext) then
            NodeNext := TagsVirtualStringTree.NodeParent[Node];
    end;
    if Assigned(NodeNext) then begin
        DataNext    := TagsVirtualStringTree.GetNodeData(NodeNext);
        TagNameNext := DataNext.Obj.TagName;
    end;

    TagRemove(TagName);

    if TagNameNext <> '' then
       TagTreeFindAndExpandNode(nil, TagNameNext);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.MoveTagToOtherBranchTagTreePopupMnuClick(
    Sender: TObject);
var
    Node         : PVirtualNode;
    Data         : POvbImgTreeData;
    Pt           : TPoint;
    TagName      : String;
    NewParentTag : String;
    APrompts     : array of String;
    AValues      : array of String;
    ErrMsg       : String;
begin
    Pt   := TagsVirtualStringTree.ScreenToClient(TagTreePopupMenu.PopupPoint);
    Node := TagsVirtualStringTree.GetNodeAt(Pt);
    if not Assigned(Node) then
        Exit;
    Data := TagsVirtualStringTree.GetNodeData(Node);
    if not Assigned(Data) then
        Exit;
    if not Assigned(Data.Obj) then
        Exit;
    TagName := Data.Obj.TagName;

    SetLength(APrompts, 1);
    SetLength(AValues, 1);
    APrompts[0] := Format(_('Enter name for new parent of "%s"'), [TagName]);
    AValues[0]  := '';
    if not InputQuery(_('Rename tag'), APrompts, AValues) then
        Exit;
    NewParentTag := Trim(AValues[0]);
    if NewParentTag = '' then
        NewParentTag := ROOT_TAG_NAME;
    if not OvbImgOrganizerDataModule.DBMoveTag(TagName, NewParentTag, ErrMsg) then begin
        ShowMessage(ErrMsg);
        Exit;
    end;
    TagTreeLoad;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.RenameTagTagTreePopupMnuClick(
    Sender: TObject);
var
    Node         : PVirtualNode;
    Data         : POvbImgTreeData;
    Pt           : TPoint;
    TagName      : String;
    NewTagName   : String;
    APrompts     : array of String;
    AValues      : array of String;
    ErrMsg       : String;
begin
    Pt   := TagsVirtualStringTree.ScreenToClient(TagTreePopupMenu.PopupPoint);
    Node := TagsVirtualStringTree.GetNodeAt(Pt);
    if not Assigned(Node) then
        Exit;
    Data := TagsVirtualStringTree.GetNodeData(Node);
    if not Assigned(Data) then
        Exit;
    if not Assigned(Data.Obj) then
        Exit;
    TagName := Data.Obj.TagName;

    SetLength(APrompts, 1);
    SetLength(AValues, 1);
    APrompts[0] := Format(_('Enter new name for %s'), [TagName]);
    AValues[0]  := '';
    if not InputQuery(_('Rename tag'), APrompts, AValues) then
        Exit;
    NewTagName := Trim(AValues[0]);
    if NewTagName = '' then
        Exit;
    if not OvbImgOrganizerDataModule.DBRenameTag(TagName, NewTagName, ErrMsg) then begin
        ShowMessage(ErrMsg);
        Exit;
    end;
    TagTreeLoad;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerMainForm.CollectionLoad : Integer;
var
    ErrMsg : String;
    Sql    : String;
begin
    Sql := OvbImgOrganizerDataModule.DBSearchGenerateSql('', ErrMsg);  // Empty string returns ALL records
    if Sql <> '' then
        Result := CollectionLoad(Sql)
    else begin
        TMessageForm.ShowMessage(ErrMsg, _('LoadCollection'),
                                 nil, [mffFailed]);
        Result := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.CollectionCopyToClipboardPopupMnuClick(
    Sender: TObject);
var
    FileNames : array of String;
    I         : Integer;
    ImageInfo : TOvbImgImageInfo;
begin
    if FCollection.Selected.Count <= 0 then
        Exit;
    SetLength(FileNames, FCollection.Selected.Count);
    for I := 0 to FCollection.Selected.Count - 1 do begin
        ImageInfo    := FCollection.Selected[I].TagObject as TOvbImgImageInfo;
        FileNames[I] := ImageInfo.ImagePath + ImageInfo.ImageName;
    end;
    CopyFilesToClipboard(FileNames);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.CollectionPastePopupMnuClick(
    Sender: TObject);
begin
    PasteFilesFromClipBoard;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.CollectionSearch(
    const TagsString : String);
var
    Sql    : String;
    ErrMsg : String;
begin
    Sql := OvbImgOrganizerDataModule.DBSearchGenerateSql(Trim(TagsString), ErrMsg);
    if Sql <> '' then
        CollectionLoad(Sql)
    else
        TMessageForm.ShowMessage(ErrMsg, _('SearchGenerateSql'),
                                 nil, [mffFailed]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.CollectionSearchBitBtnClick(Sender: TObject);
begin
    CollectionSearch(CollectionSearchEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.CollectionSearchByTagsActionExecute(
  Sender: TObject);
var
    APrompts     : array of String;
    AValues      : array of String;
begin
    SetLength(APrompts, 1);
    SetLength(AValues, 1);
    APrompts[0] := _('Enter tag names');
    AValues[0]  := '';
    if not InputQuery(_('Search by tags'), APrompts, AValues) then
        Exit;
    CollectionSearch(AValues[0]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.RecreateThumbnailPopupMnuClick(
    Sender: TObject);
var
    Pt         : TPoint;
    Ctrl       : TControl;
begin
    Pt   := CollectionFlowPanel.ScreenToClient(CollectionPopupMenu.PopupPoint);
    Ctrl := CollectionFlowPanel.ControlAtPos(Pt, TRUE, TRUE, FALSE);
    if Ctrl is TOvbImagePanel then
        ThumbnailRecreate(TOvbImagePanel(Ctrl));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ThumbnailRecreate(
    AImagePanel : TOvbImagePanel);
var
    ImageInfo  : TOvbImgImageInfo;
    ImagePanel : TOvbImagePanel;
begin
    ImageInfo  := AImagePanel.TagObject as TOvbImgImageInfo;
    Display(_('Recreate thumbnail for %s'), [ImageInfo.ImageName]);

    ImagePanel := TOvbImagePanel.Create(nil);
    try
        ImagePanel.IsThumbnail := TRUE;
        ImagePanel.OnDisplay   := DisplayHandler;
        ImagePanel.LoadImage(ImageInfo.ImagePath + ImageInfo.ImageName);
        try
            ImagePanel.CreateThumbnail(FCommonAppData + ImageInfo.ImageThumb,
                                       THUMB_WIDTH,
                                       THUMB_HEIGHT);
        finally
            ImagePanel.CloseImage;
        end;
    finally
        FreeAndNil(ImagePanel);
    end;
    AImagePanel.LoadImage(FCommonAppData + ImageInfo.ImageThumb);
    AImagePanel.CloseImage;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.CompareImageIndexWidthDirContentProgressHandler(
    Sender    : TObject;
    Context   : Integer;
    Count     : Integer;
    var Abort : Boolean);
begin
    if (FLastProgressTime = 0) or
       (GetTickCount > FLastProgressTime) then begin
        FLastProgressTime := GetTickCount + 1000;
        StatusBar1.Panels[0].Text := Format('%d %d', [Context, Count]);
        Application.ProcessMessages;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ToolsScanDirActionExecute(Sender: TObject);
var
    ErrMsg : String;
    Form   : TScanDirForm;
begin
    Form := TScanDirForm.Create(Self);
    try
        Form.ScanDir           := FLastScanDir;
        Form.Recursive         := FLastScanRecursive;
        if Form.ShowModal <> mrOK then
            Exit;
        if Form.ScanDir = '' then
            Exit;
        if not TDirectory.Exists(Form.ScanDir) then begin
            TMessageForm.ShowMessage(
                _('Failed: You must select a directory to scan.'),
                _('Scan dir'), nil,
                [mffFailed, mffAutoWidth]);
            Exit;
        end;
        FLastScanDir       := IncludeTrailingPathDelimiter(Form.ScanDir);
        FLastScanRecursive := Form.Recursive;
    finally
        FreeAndNil(Form);
    end;

    if DisplayMemo.Height < 150 then
        DisplayMemo.Height := 150;
    if not OvbImgOrganizerDataModule.DBCompareImageIndexWidthDirContent(
         FLastScanDir,
         'TempImageIndex',
         FLastScanRecursive,
         CompareImageIndexWidthDirContentProgressHandler,
         ErrMsg) then
        Display(ErrMsg);
    Display(_('Done.'));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.RecreateThumbnailsForSelectedImagesPopupMnuClick(
     Sender: TObject);
var
    ImagePanel  : TOvbImagePanel;
begin
    if FCollection.Selected.Count <= 0 then
        Exit;
    for ImagePanel in FCollection.Selected do
        ThumbnailRecreate(ImagePanel);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.CollectionReload;
begin
    if FLoadCollectionLastSqlText <> '' then
        CollectionLoad(FLoadCollectionLastSqlText)
    else
        CollectionLoad;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ReloadTreeTagTreePopupMnuClick(
    Sender: TObject);
begin
    TagTreeLoad;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerMainForm.CollectionLoadMore : Integer;
begin
    if FLoadCollectionLastSqlText <> '' then
        Result := CollectionLoad(FLoadCollectionLastSqlText, TRUE)
    else
        Result := CollectionLoad;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.LoadMoreButtonClick(Sender: TObject);
begin
    CollectionLoadMore;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerMainForm.TagTreeLoad: Boolean;
var
    TagList    : TOvbImgTagList;
    TagRec     : TOvbImgTagObject;
    NodeRoot   : PVirtualNode;
    NodeChild  : PVirtualNode;
    Data       : POvbImgTreeData;
    ErrMsg     : String;
begin
    Result := FALSE;
    TagsVirtualStringTree.Clear;
    TagsVirtualStringTree.NodeDataSize := SizeOf(TOvbImgTreeData);
    TagList := TOvbImgTagList.Create(TRUE);
    try
        // Get all tags which are at the root level (ID = 1)
        if not OvbImgOrganizerDataModule.DBGetTagList(1, TagList, ErrMsg) then begin
            ShowMessage(ErrMsg);
            Exit;
        end;

        // Create a root node for each of the tags found
        for TagRec in TagList do begin
            NodeRoot           := TagsVirtualStringTree.AddChild(nil);
            Data               := TagsVirtualStringTree.GetNodeData(NodeRoot);
            Data.Obj.ID        := TagRec.ID;
            Data.Obj.ParentID  := TagRec.ParentID;
            Data.Obj.TagName   := TagRec.TagName;
            // The next line is required to have drop target correctly shown
            TagsVirtualStringTree.FocusedNode := NodeRoot;

            // Create an empty node as place holder for childs so that
            // a "+" is displayed on the root above.
            NodeChild          := TagsVirtualStringTree.AddChild(NodeRoot);
            Data               := TagsVirtualStringTree.GetNodeData(NodeChild);
            Data.Obj.ParentID  := TagRec.ParentID;
            Data.Obj.TagName   := ':DUMMY';
        end;

        Result := TRUE;
    finally
        FreeAndNil(TagList);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.DataRecordHandler(
    Sender           : TObject;
    var   Index      : Integer;
    const DataRecord : TOvbImgDataRecord);
var
    ImageFileName   : String;
    ThumbFileName   : String;
    ImagePanel      : TOvbImagePanel;
    ImageInfo       : TOvbImgImageInfo;
begin
    ImageFileName   := DataRecord.ImagePath + DataRecord.ImageName;
    ThumbFileName   := FCommonAppData + DataRecord.ImageThumb;
    if not FileExists(ImageFileName) then
        Exit;

    if Index >= CollectionFlowPanel.ControlCount then begin
        ImagePanel                  := TOvbImagePanel.Create(Self);
        ImagePanel.Name             := 'ImagePanel_' + IntToStr(Index);
        ImagePanel.IsThumbnail      := TRUE;
        ImagePanel.Width            := THUMB_WIDTH;
        ImagePanel.Height           := THUMB_HEIGHT; //ImagePanel.Width * 2 div 3;
        ImagePanel.Parent           := CollectionFlowPanel;
        ImagePanel.OnImageClick     := FlowLayoutImageClickHandler;
        ImagePanel.OnImageDblClick  := FlowLayoutImageDblClickHandler;
        ImagePanel.OnImageMouseDown := FlowLayoutImageMouseDownHandler;
        ImagePanel.OnCheckBoxClick  := FlowLayoutCheckBoxClickHandler;
        ImagePanel.OnDestroy        := FlowLayoutImageDestroyHandler;
        ImagePanel.OnDisplay        := DisplayHandler;
        ImagePanel.TagObject        := TOvbImgImageInfo.Create;
        ImagePanel.TagObjectOwned   := TRUE;
        ImagePanel.PopupMenu        := CollectionPopupMenu;
        ImagePanel.HadFocus         := FALSE;
        ImagePanel.HasFocus         := FALSE;
        ImagePanel.BackColor        := clLtGray;
        ImagePanel.Visible          := TRUE;
    end
    else
        ImagePanel := CollectionFlowPanel.Controls[Index] as TOvbImagePanel;
    if not FileExists(ThumbFileName) then begin
        ImagePanel.LoadImage(ImageFileName);
        try
            ImagePanel.CreateThumbnail(ThumbFileName,
                                       THUMB_WIDTH,
                                       THUMB_HEIGHT);
        finally
            ImagePanel.CloseImage;
        end;
    end;
    ImagePanel.LoadImage(ThumbFileName);
    ImagePanel.CloseImage;
    ImagePanel.Caption          := ChangeFileExt(DataRecord.ImageName, '');
    ImagePanel.Index            := Index;
    ImagePanel.Selected         := FALSE;
    ImagePanel.HadFocus         := FALSE;
    ImagePanel.HasFocus         := FALSE;
    ImagePanel.CheckBox.Checked := FALSE;
    ImagePanel.Visible          := TRUE;
    ImageInfo                   := ImagePanel.TagObject as TOvbImgImageInfo;
    ImageInfo.Clear;
    ImageInfo.ImageName         := DataRecord.ImageName;
    ImageInfo.ImagePath         := DataRecord.ImagePath;
    ImageInfo.ImageThumb        := DataRecord.ImageThumb;
    ImageInfo.ImageID           := DataRecord.ImageID;
    ImageInfo.DropImagePanel    := ImagePanel;
    ImageInfo.ImageDateTime     := TTimeZone.Local.ToLocalTime(DataRecord.ImageDateTime);
    ImageInfo.IndexDateTime     := TTimeZone.Local.ToLocalTime(DataRecord.IndexDateTime);
    ImageInfo.Description       := DataRecord.Description;
    ImageInfo.Latitude          := DataRecord.Latitude;
    ImageInfo.Longitude         := DataRecord.Longitude;
    ImageInfo.Latitude2         := DataRecord.Latitude2;
    ImageInfo.Longitude2        := DataRecord.Longitude2;
    Inc(Index);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Return the number of records read from database
function TOvbImgOrganizerMainForm.CollectionLoad(
    const SqlText : String;
    const More    : Boolean = FALSE) : Integer;
var
    ImagePanel      : TOvbImagePanel;
    Index           : Integer;
    I               : Integer;
    ErrMsg          : String;
begin
    PageControl1.ActivePage      := CollectionTabSheet;
    FCollection.LastFocusedObject := nil;
    FCollection.PrevFocusedObject := nil;
    StatusBar1.Panels[2].Text    := '';
    FLoadCollectionLastSqlText   := SqlText;
    Result                       := 0;

    // Determine how many thumbnails are visible so that we query the database
    // with just enough records for the visible part.
    FCollection.Limit := FCollection.WCount * FCollection.NHCount;
    if not More then
        FCollection.LimitOffset   := 0;

    // First, hide all ImagePanel (Better looking screen while loading)
    if not More then begin
        for I := CollectionFlowPanel.ControlCount - 1 downto 0 do begin
            ImagePanel := CollectionFlowPanel.Controls[I] as TOvbImagePanel;
            ImagePanel.Visible := FALSE;
        end;
    end;
    Update;

    if More then
        Index := CollectionFlowPanel.ControlCount
    else
        Index := 0;

    OvbImgOrganizerDataModule.DBLoadCollection(SqlText,
                                 More,
                                 FCollection.Limit,
                                 FCollection.LimitOffset,
                                 OrderByComboBox.ItemIndex,
                                 OrderByDescendingCheckBox.Checked,
                                 FCollection.RecCnt,
                                 Index,
                                 ErrMsg,
                                 DataRecordHandler);

    FCollection.LimitOffset := FCollection.LimitOffset + FCollection.Limit;
    FCollection.WCount := Trunc((CollectionFlowPanel.Width - 1) / THUMB_WIDTH);
    FCollection.HCount := 1 + (CollectionFlowPanel.ControlCount - 1) div FCollection.WCount;
    FCollection.Count  := Index;
//    Display('Collection %d %dx%d', [FCollectionCount, FCollectionWCount, FCollectionHCount]);
    StatusBar1.Panels[1].Text := Format(_('%d/%d images'),
                                        [FCollection.Count, FCollection.RecCnt]);
    CollectionScrollBox.VertScrollBar.Range     := FCollection.HCount * THUMB_HEIGHT;
    CollectionScrollBox.VertScrollBar.Increment := THUMB_HEIGHT;
    FCollection.Selected.Clear;
    CollectionUpdateSelectedStatus;
    LoadMoreButton.Enabled := FCollection.Count < FCollection.RecCnt;
    // If collection was reduced, destroy in excess ImagePanel
    for I := CollectionFlowPanel.ControlCount - 1 downto Index do begin
        ImagePanel := CollectionFlowPanel.Controls[Index] as TOvbImagePanel;
        ImagePanel.Free;
    end;

    if (not More) and (CollectionFlowPanel.ControlCount > 0) then
        ActiveControl := (CollectionFlowPanel.Controls[0] as TOvbImagePanel).CheckBox;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagsVirtualStringTreeDragAllowed(
    Sender      : TBaseVirtualTree;
    Node        : PVirtualNode;
    Column      : TColumnIndex;
    var Allowed : Boolean);
var
    Data   : POvbImgTreeData;
begin
    Data    := Sender.GetNodeData(Node);
    Allowed := Assigned(Data) and Assigned(Data.Obj) and
               (Data.Obj.TagName <> ':DUMMY');
//    Display('DragAllowed Allowed=%d', [Ord(Allowed)]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagsVirtualStringTreeDragDrop(
    Sender     : TBaseVirtualTree;
    Source     : TObject;
    DataObject : IDataObject;
    Formats    : TFormatArray;
    Shift      : TShiftState;
    Pt         : TPoint;
    var Effect : Integer;
    Mode       : TDropMode);
var
    SourceNode     : PVirtualNode;
    SourceData     : POvbImgTreeData;
    TargetNode     : PVirtualNode;
    TargetData     : POvbImgTreeData;
    SourceTagName  : String;
    TargetTagName  : String;
    TargetTagID    : Integer;
    NewParentID    : Integer;
    Buf            : String;
    ErrMsg         : String;
begin
    SourceNode := TVirtualStringTree(Source).FocusedNode;
    if not Assigned(SourceNode) then
        Exit;
    SourceData    := TVirtualStringTree(Source).GetNodeData(SourceNode);
    SourceTagName := SourceData.Obj.TagName;

    TargetNode := Sender.DropTargetNode;
    if not Assigned(TargetNode) then begin
        TargetTagName := '';  // Empty string is for root
    end
    else begin
        if SourceNode = TargetNode then
            Exit;
        TargetData    := TVirtualStringTree(Source).GetNodeData(TargetNode);
        TargetTagName := TargetData.Obj.TagName;
    end;

    if Mode = dmNowhere then begin
        // Move the node to the root
    end
    else if Mode = dmOnNode then begin
        // TargetNode is the new parent
    end
    else if Mode = dmAbove then begin
        // New parent is parent of target node
        TargetTagID   := OvbImgOrganizerDataModule.DBGetTagID(TargetTagName, NewParentID, ErrMsg);
        if TargetTagID < 0 then begin
            ShowMessage(ErrMsg);
            Exit;
        end;
        TargetTagName := OvbImgOrganizerDataModule.DBGetTagName(NewParentID, ErrMsg);
    end
    else if Mode = dmBelow then begin
        // New parent is parent of target node
        TargetTagID   := OvbImgOrganizerDataModule.DBGetTagID(TargetTagName, NewParentID, ErrMsg);
        if TargetTagID < 0 then begin
            ShowMessage(ErrMsg);
            Exit;
        end;
        TargetTagName := OvbImgOrganizerDataModule.DBGetTagName(NewParentID, ErrMsg);
    end;

    if TargetTagName = '' then
        Buf := 'Root'
    else
        Buf := TargetTagName;

    Display('DragDrop %s -> %s  (Mode = %s)',
            [SourceTagName, Buf,
             GetEnumName(TypeInfo(TDropMode), Ord(Mode))]);

    if not OvbImgOrganizerDataModule.DBMoveTag(SourceTagName, TargetTagName, ErrMsg) then begin
        ShowMessage(ErrMsg);
        Exit;
    end;

    TagTreeLoad;

    TagTreeFindAndExpandNode(nil, SourceTagName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagsVirtualStringTreeDragOver(
    Sender     : TBaseVirtualTree;
    Source     : TObject;
    Shift      : TShiftState;
    State      : TDragState;
    Pt         : TPoint;
    Mode       : TDropMode;
    var Effect : Integer;
    var Accept : Boolean);
var
    SourceNode : PVirtualNode;
    TargetNode : PVirtualNode;
    TargetData : POvbImgTreeData;
begin
    if not Assigned(Source) then
        Accept := FALSE
    else begin
        SourceNode := TVirtualStringTree(Source).FocusedNode;
        TargetNode := Sender.DropTargetNode;
        if not Assigned(TargetNode) then begin
//             Display('DragOver TargetNode=nil');
             Accept := TRUE;
        end
        else begin
            TargetData := TVirtualStringTree(Source).GetNodeData(TargetNode);

            Accept := (Source = Sender) and (SourceNode <> TargetNode) and
              (Assigned(TargetData)) and (Assigned(TargetData.Obj)) and
              (TargetData.Obj.TagName <> ':DUMMY');
        end;
    end;
//    Display('DragOver Accept=%d', [Ord(Accept)]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagsVirtualStringTreeExpanding(
    Sender      : TBaseVirtualTree;
    Node        : PVirtualNode;
    var Allowed : Boolean);
var
    Data           : POvbImgTreeData;
    ParentID       : Integer;
    NodeChild      : PVirtualNode;
    NodeChild2     : PVirtualNode;
    TagList        : TOvbImgTagList;
    TagRec         : TOvbImgTagObject;
    ErrMsg         : String;
begin
    TagsVirtualStringTree.FocusedNode    := Node;
    TagsVirtualStringTree.Selected[Node] := TRUE;

    Data     := Sender.GetNodeData(Node);
//    Display('Expanding %s', [Data.Obj.TagName]);
    if Data.Obj.Loaded then
        Exit;
    Data.Obj.Loaded := TRUE;
    ParentID        := Data.Obj.ID;
    TagList         := TOvbImgTagList.Create(TRUE);
    try
        if OvbImgOrganizerDataModule.DBGetTagList(ParentID, TagList, ErrMsg) then begin
            if TagList.Count = 0 then begin
                Data.Obj.Loaded := FALSE;
                Allowed         := FALSE;
                Exit;
            end;
            NodeChild := Node.FirstChild;
            for TagRec in TagList do begin
                if not Assigned(NodeChild) then
                    NodeChild := TagsVirtualStringTree.AddChild(Node);
                Data               := TagsVirtualStringTree.GetNodeData(NodeChild);
                Data.Obj.ID        := TagRec.ID;
                Data.Obj.ParentID  := TagRec.ParentID;
                Data.Obj.TagName   := TagRec.TagName;
                Data.Obj.Loaded    := FALSE;

                // Create an empty node as place holder for childs so that
                // a "+" is displayed on the root above.
                NodeChild2         := TagsVirtualStringTree.AddChild(NodeChild);
                Data               := TagsVirtualStringTree.GetNodeData(NodeChild2);
                Data.Obj.ParentID  := TagRec.ParentID;
                Data.Obj.TagName   := ':DUMMY';
                Data.Obj.Loaded    := FALSE;

                NodeChild          := NodeChild.NextSibling;
            end;
        end;
    finally
        FreeAndNil(TagList);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagsVirtualStringTreeFreeNode(
    Sender : TBaseVirtualTree;
    Node   : PVirtualNode);
var
    Data       : POvbImgTreeData;
begin
    Data := TagsVirtualStringTree.GetNodeData(Node);
    if Data <> nil then
        FreeAndNil(Data.Obj);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagsVirtualStringTreeGetPopupMenu(
    Sender        : TBaseVirtualTree;
    Node          : PVirtualNode;
    Column        : TColumnIndex;
    const P       : TPoint;
    var AskParent : Boolean;
    var PopupMenu : TPopupMenu);
begin
    PopupMenu := TagTreePopupMenu;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagsVirtualStringTreeGetText(
    Sender       : TBaseVirtualTree;
    Node         : PVirtualNode;
    Column       : TColumnIndex;
    TextType     : TVSTTextType;
    var CellText : string);
var
    Data     : POvbImgTreeData;
begin
    Data := Sender.GetNodeData(Node);
    if not Assigned(Data) then
        CellText := _('No data assigned')
    else if not Assigned(Data.Obj) then
        CellText := ''
    else begin
        CellText := Data.Obj.TagName;
        if CellText = ':DUMMY' then
            CellText := _('<empty>');
    end;
//    Display('GetText "%s"', [CellText]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagsVirtualStringTreeInitNode(
    Sender            : TBaseVirtualTree;
    ParentNode        : PVirtualNode;
    Node              : PVirtualNode;
    var InitialStates : TVirtualNodeInitStates);
var
    Data : POvbImgTreeData;
begin
//    Display('OnInitNode');
    Data               := TagsVirtualStringTree.GetNodeData(Node);
    Data.Obj           := TOvbImgTagObject.Create(DisplayHandler, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagsVirtualStringTreeMouseDown(
    Sender : TObject;
    Button : TMouseButton;
    Shift  : TShiftState;
    X, Y   : Integer);
var
    Pt   : TPoint;
    Node : PVirtualNode;
    VST  : TVirtualStringTree;
begin
    // When right click, select the node where clicked, if any
    if Button = mbRight then begin
        VST  := Sender as TVirtualStringTree;
        Pt   := VST.ClientToScreen(Point(X, Y));
        Node := VST.GetNodeAt(X, Y);
        if Assigned(Node) then
            VST.Selected[Node] := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagsVirtualStringTreeNodeDblClick(
    Sender        : TBaseVirtualTree;
    const HitInfo : THitInfo);
var
    Data     : POvbImgTreeData;
    TagNames : TStringDynArray;
    TagName  : String;
begin
    Data := TagsVirtualStringTree.GetNodeData(HitInfo.HitNode);
    if not Assigned(Data) then
        Exit;
    if not Assigned(Data.Obj) then
        Exit;

//    Display('TreeDblClick %s', [Data.Obj.TagName]);

    if PageControl1.ActivePage = CollectionTabSheet then begin
        if FCollection.Selected.Count <= 0 then begin
            ShowMessage(_('No image selected'));
            Exit;
        end;
        SetLength(TagNames, 1);
        TagNames[0] := Data.Obj.TagName;
        Display(TagNames[0]);
        TagSelectedImages(TagNames);
        // Set focus to the last item selected
        if FCollection.Selected.Count > 0 then
            FCollection.Selected[FCollection.Selected.Count - 1].SetFocus;
    end
    else if PageControl1.ActivePage = NewImageTabSheet then begin
        TagName := Data.Obj.TagName;
        if ImageDropTagListBox.Items.IndexOf(TagName) >= 0 then begin
            ShowMessage(Format(_('Tag was "%s" already in the list'), [TagName]));
            Exit;
        end;
        ImageDropTagListBox.Items.Add(TagName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagTreePopupMenuPopup(Sender: TObject);
var
    Node     : PVirtualNode;
    Data     : POvbImgTreeData;
    Pt       : TPoint;
    TagName  : String;
begin
    Pt   := TagsVirtualStringTree.ScreenToClient(TagTreePopupMenu.PopupPoint);
    Node := TagsVirtualStringTree.GetNodeAt(Pt);
    if Assigned(Node) then begin
        Data := TagsVirtualStringTree.GetNodeData(Node);
        if not Assigned(Data) then
            Exit;
        if not Assigned(Data.Obj) then
            Exit;
        TagName := Data.Obj.TagName;
        Display(_('Popup on %s'), [TagName]);
        MoveTagToOtherBranchTagTreePopupMnu.Visible   := TRUE;
        MoveTagToOtherBranchTagTreePopupMnu.Caption   :=
            Format(_('Move tag "%s" to another branch'), [TagName]);
        RemoveTagFromTagTreePopupMnuClick.Visible     := TRUE;
        RemoveTagFromTagTreePopupMnuClick.Caption     :=
            Format(_('Remove tag "%s" from tree'), [TagName]);
        RenameTagTagTreePopupMnu.Visible              := TRUE;
        RenameTagTagTreePopupMnu.Caption              :=
            Format(_('Rename tag "%s"'), [TagName]);
        AddTagToAllSelectedImagesTreePopupMnu.Visible := TRUE;
        AddTagToAllSelectedImagesTreePopupMnu.Caption :=
            Format(_('Add tag "%s" to all selected images'), [TagName]);
        TagTreeAddNewAsChildTagPopupMnu.Visible       := TRUE;
        TagTreeAddNewAsChildTagPopupMnu.Caption       :=
            Format(_('Add a new tag as child of "%s"'), [TagName]);
        AddNewTagAsSiblingTagTreePopupMnu.Visible := TRUE;
        AddNewTagAsSiblingTagTreePopupMnu.Caption :=
            Format(_('Add a new tag having the same parent as "%s"'), [TagName]);
    end
    else begin
        MoveTagToOtherBranchTagTreePopupMnu.Visible   := FALSE;
        RemoveTagFromTagTreePopupMnuClick.Visible     := FALSE;
        RenameTagTagTreePopupMnu.Visible              := FALSE;
        AddTagToAllSelectedImagesTreePopupMnu.Visible := FALSE;
        TagTreeAddNewAsChildTagPopupMnu.Visible       := FALSE;
        AddNewTagAsSiblingTagTreePopupMnu.Visible     := TRUE;
        AddNewTagAsSiblingTagTreePopupMnu.Caption     :=
            _('Add a new tag at the top level');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.FlowLayoutCheckBoxClickHandler(
    Sender : TObject);
var
    ImagePanel    : TOvbImagePanel;
    ShiftKey      : Boolean;
//    CtrlKey       : Boolean;
    IndexPrev     : Integer;
    IndexCur      : Integer;
    Index1        : Integer;
    Index2        : Integer;
    Index         : Integer;
    TmpClick      : TNotifyEvent;
begin
    ShiftKey := ((GetKeyState(VK_SHIFT)   and $8000) <> 0);
//    CtrlKey  := ((GetKeyState(VK_CONTROL) and $8000) <> 0);
//    Display('CheckBoxClick %d %d',
//            [Ord(ShiftKey), Ord(CtrlKey)]);

    if not Assigned(Sender) then
        Exit;
    if not (Sender is TOvbImagePanel) then
        Exit;
    ImagePanel  := TOvbImagePanel(Sender);
    IndexCur    := ImagePanel.Index;

    if ShiftKey and Assigned(FCollection.PrevFocusedObject) then begin
        // Select all images between current and previous focused object
        IndexPrev     := FCollection.PrevFocusedObject.Index;
        Index1 := FCollection.PrevFocusedObject.Index;
        Index2 := ImagePanel.Index;
        if Index1 > Index2 then begin
            Index  := Index1;
            Index1 := Index2;
            Index2 := Index;
        end;
        for Index := Index1 to Index2 do begin
            if Index = IndexPrev then
                continue;
            ImagePanel := CollectionFlowPanel.Controls[Index] as TOvbImagePanel;
            TmpClick                    := ImagePanel.OnCheckBoxClick;
            ImagePanel.OnCheckBoxClick  := nil;
            if Index = IndexCur then
                ImagePanel.CheckBox.Checked := ImagePanel.CheckBox.Checked
            else
                ImagePanel.CheckBox.Checked := not ImagePanel.CheckBox.Checked;
            ImagePanel.Selected         := ImagePanel.CheckBox.Checked;
            ImagePanel.OnCheckBoxClick  := TmpClick;
            if not ImagePanel.CheckBox.Checked then
                FCollection.Selected.Remove(ImagePanel)
            else
                FCollection.Selected.Add(ImagePanel);
        end;
    end
    else begin
        if ImagePanel.Selected and ImagePanel.CheckBox.Checked then
            Exit;   // No change

        ImagePanel.Selected := ImagePanel.CheckBox.Checked;

        if not ImagePanel.CheckBox.Checked then
            FCollection.Selected.Remove(ImagePanel)
        else
            FCollection.Selected.Add(ImagePanel);
    end;
    CollectionUpdateSelectedStatus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.CollectionUpdateSelectedStatus;
begin
    if Assigned(StatusBar1) and Assigned(FCollection.Selected) then
        StatusBar1.Panels[0].Text := Format(_('%d image selected'),
                                            [FCollection.Selected.Count]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.FlowLayoutImageMouseDownHandler(
    Sender : TObject;
    Button : TMouseButton;
    Shift  : TShiftState;
    X, Y   : Integer);
begin
    if not (Sender is TOvbImagePanel) then
        Exit;
    if Button = mbRight then
        ActiveControl := TOvbImagePanel(Sender).CheckBox;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.FlowLayoutImageDblClickHandler(
    Sender : TObject);
begin
    if Sender is TControl then
        ImagePanelShow(TControl(Sender));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.FlowLayoutImageClickHandler(
    Sender : TObject);
var
    ImagePanel  : TOvbImagePanel;
begin
    if not Assigned(Sender) then
        Exit;
    if not (Sender is TOvbImagePanel) then
        Exit;

    ImagePanel  := TOvbImagePanel(Sender);
    ImagePanel.SetFocus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.DropPreviewImagePaneDblClickHandler(
    Sender : TObject);
var
    Form           : TSlideShowForm;
begin
    Form           := TSlideShowForm.Create(Self);
    Form.OnDestroy := SlidShowFormDestroyHandler;
    Form.OnDisplay := DisplayHandler;
    Form.Filenames.Add(FDropInfo.FileName);
    FSlideShowList.Add(Form);
    CenterChildForm(Form);
    Form.Show;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.FormKeyDown(
    Sender  : TObject;
    var Key : Word;
    Shift   : TShiftState);
var
    FocusObject : TWinControl;
begin
    FocusObject := Screen.ActiveControl;
//    Display('Form KeyDown   %d %s %s %x', [Key, FocusObject.ClassName, FocusObject.Name, FocusObject.Handle]);
    if Key = VK_RETURN then begin
        if FocusObject = CollectionSearchEdit then begin
            Key := 0;
            if Assigned(CollectionSearchBitBtn.OnClick) then
                CollectionSearchBitBtn.OnClick(CollectionSearchBitBtn);
            ActiveControl := CollectionSearchEdit;
        end
        else
            FFlagReturnKeyDown := TRUE;
        Exit;
    end;
    FFlagReturnKeyDown := FALSE;

    if (Key = VK_F11) and (Shift = []) then begin
        Key := 0;
        SetFullScreen(BorderStyle = bsSizeable);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.FormKeyUp(
    Sender  : TObject;
    var Key : Word;
    Shift   : TShiftState);
var
    FocusObject : TWinControl;
begin
    FocusObject := Screen.ActiveControl;
//    Display('Form KeyUp   %d %s %s %x', [Key, FocusObject.ClassName, FocusObject.Name, FocusObject.Handle]);
    if Key = VK_RETURN then begin
        if not FFlagReturnKeyDown then
            Exit;
        FFlagReturnKeyDown := FALSE;
    end;

    // Check if focused object in an ImagePanel in the CollectionFlowPanel
    // In an ImagePanel, it is the checkbox which as focus
    if (FocusObject is TCheckBox) and
       (IsChildOf(FocusObject, CollectionFlowPanel)) then begin
        CollectionImagePanelFormKeyUp(Sender, Key, Shift);
        Exit;
    end;

    if (FocusObject is TButton) and
       (FocusObject.Parent = DropDialogPanel) then begin
        DropPanelButtonFormKeyUp(Sender, Key, Shift);
        Exit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerMainForm.IsChildOf(
    Ctrl       : TWinControl;
    ParentCtrl : TWinControl) : Boolean;
begin
    if (not Assigned(Ctrl)) or (not Assigned(ParentCtrl)) then begin
        Result := FALSE;
        Exit;
    end;
    if Ctrl.Parent = ParentCtrl then
        Result := TRUE
    else
        Result := IsChildOf(Ctrl.Parent, ParentCtrl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Handle FormKeyUp when the focus in an ImagePanel in the collection
procedure TOvbImgOrganizerMainForm.DropPanelButtonFormKeyUp(
    Sender  : TObject;
    var Key : Word;
    Shift   : TShiftState);
begin
    case Key of
    Ord('V'):
        begin
            if ssCtrl in Shift then begin
                // CTRL-V => Insert from clipboard
                PasteFilesFromClipBoard;
                Key := 0;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Handle FormKeyUp when the focus in an ImagePanel in the collection
procedure TOvbImgOrganizerMainForm.CollectionImagePanelFormKeyUp(
    Sender  : TObject;
    var Key : Word;
    Shift   : TShiftState);
var
    ImagePanel  : TOvbImagePanel;
    Row, Col    : Integer;
    Index       : Integer;
    Pt1, Pt2    : TPoint;
begin
    ImagePanel := FCollection.FocusedObject;
    Col        := ImagePanel.Index mod FCollection.WCount;
    Row        := ImagePanel.Index div FCollection.WCount;
//    Display('Row=%d  Col=%d', [Row, col]);
    case Key of
    VK_DOWN :
        begin
            if Row >= (FCollection.HCount - 1) then
                CollectionLoadMore;
            if Row < (FCollection.HCount - 1) then
                Inc(Row);
            // Last line could be incomplete
            Index := Col + Row * FCollection.WCount;
            while Index >= FCollection.Count do begin
                Dec(Col);
                Index := Col + Row * FCollection.WCount;
            end;
        end;
    VK_UP :
        begin
            if Row > 0 then
                Dec(Row);
        end;
    VK_LEFT :
        begin
            if Col > 0 then
                Dec(Col);
        end;
    VK_RIGHT :
        begin
            if Col < (FCollection.WCount - 1) then
               Inc(Col);
        end;
    VK_NEXT : // PageDown
        begin
            Inc(Row, FCollection.NHCount);
            Index := Col + Row * FCollection.WCount;
            if Index >= FCollection.Count then
                CollectionLoadMore;
            if Index >= FCollection.Count then begin
                Col   := 0;
                Index := Col + Row * FCollection.WCount;
            end;
            while (Index >= FCollection.Count) and (Row > 0) do begin
                Dec(Row);
                Index := Col + Row * FCollection.WCount;
            end;
        end;
    VK_PRIOR : // PageUp
        begin
            Dec(Row, FCollection.NHCount);
            if Row < 0 then
                Row := 0;
        end;
    VK_HOME :
        begin
            Col := 0;
            // CTRL-HOME move to top
            if ssCtrl in Shift then
                Row := 0;
        end;
    VK_END :
        begin
            Col := FCollection.WCount - 1;
            // CTRL-END move to last image
            if ssCtrl in Shift then begin
                while CollectionLoadMore > 0 do begin
                    // Should probably check that we don't try to load
                    // thousands of records!
                end;
                Row := FCollection.HCount - 1;
            end;
            // Last line could be incomplete
            Index := Col + Row * FCollection.WCount;
            while Index >= FCollection.Count do begin
                Dec(Col);
                Index := Col + Row * FCollection.WCount;
            end;
        end;
    VK_DELETE,
    Ord('A'),
    Ord('V'),
    VK_RETURN,
    VK_SPACE :
        begin

        end;
    else
        Exit;
    end;
    Index := Col + Row * FCollection.WCount;
    case Key of
    VK_RETURN :
        begin
            ImagePanelShellOpen(Handle, CollectionFlowPanel.Controls[Index]);
            Key := 0;
            Exit;
        end;
    VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_NEXT, VK_PRIOR:
        begin
            if Row = 0 then
                CollectionScrollBox.VertScrollBar.Position := 0;
            if Index <> ImagePanel.Index then begin
                ImagePanel.HasFocus := FALSE;
                ImagePanel := CollectionFlowPanel.Controls[Index] as TOvbImagePanel;
                ActiveControl := ImagePanel.CheckBox;
            end;
            // We have to make sure that when the selected item is in the top
            // visible rown, it is completely visible
            Pt1 := CollectionFlowPanel.ClientToScreen(ImagePanel.BoundsRect.TopLeft);
            Pt2 := CollectionScrollBox.ClientToScreen(Point(0, 0));
            if (Pt1.Y - Pt2.Y) < 0 then
                 CollectionScrollBox.VertScrollBar.Position :=
                     CollectionScrollBox.VertScrollBar.Position - Pt2.Y+ Pt1.Y;
            Key := 0;
            Exit;
        end;
    VK_DELETE :
        begin
            ImageRemoveFromView(ImagePanel);
            Key := 0;
        end;
    Ord('A'):
        begin
            if ssCtrl in Shift then begin
                // CTRL-A => Select all
                ImagesSelectAll;
                Key := 0;
            end;
        end;
    Ord('V'):
        begin
            if ssCtrl in Shift then begin
                // CTRL-V => Insert from clipboard
                PasteFilesFromClipBoard;
                Key := 0;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.PasteFilesFromClipBoard;
var
    DropHandle     : THandle;
    FileNameBuffer : array [0..MAX_PATH] of Char;
    I, FileCount   : Integer;
    FileNames      : array of String;
begin
    if not Clipboard.HasFormat(CF_HDROP) then
        Exit;
    Clipboard.Open;
    try
        DropHandle := Clipboard.GetAsHandle(CF_HDROP);
        if DropHandle <> 0 then begin
            FileCount := DragQueryFile(DropHandle, $FFFFFFFF, nil, 0);
            SetLength(FileNames, FileCount);
            for I := 0 to FileCount - 1 do begin
               FileNameBuffer[0] := #0;
               DragQueryFile(DropHandle, I, FileNameBuffer, SizeOf(FileNameBuffer));
               Display(FileNameBuffer);
               FileNames[I] := FileNameBuffer;
            end;
            PasteFileNameList(FileNames);
        end;
    finally
        Clipboard.Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.PropertiesPopupMnuClick(Sender: TObject);
var
    Pt         : TPoint;
    Ctrl       : TControl;
begin
    Pt   := CollectionFlowPanel.ScreenToClient(CollectionPopupMenu.PopupPoint);
    Ctrl := CollectionFlowPanel.ControlAtPos(Pt, TRUE, TRUE, FALSE);
    ImagePanelShellProperties(Handle, Ctrl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.CopyFilesToClipboard(
    FileNames : array of String);
var
    FileList  : String;
    DropFiles : PDropFiles;
    hGlobal   : THandle;
    iLen      : Integer;
    I         : Integer;
begin
    if Length(FileNames) = 0 then
        Exit;
    // Construct a FileList have all file names separated by a nul char and
    // having a double nul char at the end.
    FileList := '';
    for I := Low(FileNames) to High(FileNames) do
        FileList := FileList + FileNames[I] + #0;
    FileList := FileList + #0;

    iLen    := Length(FileList);
    hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT,
                           SizeOf(TDropFiles) + ((iLen + 2) * SizeOf(Char)));
    if hGlobal = 0 then
        raise Exception.Create(_('Could not allocate  memory.'));
    try
        DropFiles := GlobalLock(hGlobal);
        if DropFiles = nil then
            raise Exception.Create(_('Could not access  allocated memory.'));
        try
            DropFiles^.pFiles := SizeOf(TDropFiles);
            DropFiles^.fWide := True;
            if FileList <> '' then
                Move(FileList[1], (PByte(DropFiles) + SizeOf(TDropFiles))^, iLen *  SizeOf(Char));
        finally
            GlobalUnlock(hGlobal);
        end;
        Clipboard.SetAsHandle(CF_HDROP, hGlobal);
    except
        GlobalFree(hGlobal);
    end;
 end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.FormMouseWheel(
    Sender      : TObject;
    Shift       : TShiftState;
    WheelDelta  : Integer;
    MousePos    : TPoint;
    var Handled : Boolean);
begin
//    Display('%d FormMouseWheel %d (%d, %d)',
//            [GetTickCount, WheelDelta, MousePos.X, MousePos.Y]);
    if CollectionScrollBox.BoundsRect.Contains(CollectionScrollBox.ScreenToClient(MousePos)) then begin
        if WheelDelta > 0 then
            CollectionScrollBox.VertScrollBar.Position :=
                CollectionScrollBox.VertScrollBar.Position
                    - CollectionScrollBox.VertScrollBar.Increment
        else
            CollectionScrollBox.VertScrollBar.Position :=
                CollectionScrollBox.VertScrollBar.Position
                    + CollectionScrollBox.VertScrollBar.Increment;
        Handled := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.FlowLayoutImageDestroyHandler(
    Sender : TObject);
var
    ImagePanel : TOvbImagePanel;
begin
    ImagePanel := Sender as TOvbImagePanel;
    if Assigned(FCollection) then begin
        if Assigned(FCollection.Selected) then
            FCollection.Selected.Remove(ImagePanel);
        if FCollection.FocusedObject = ImagePanel then
            FCollection.FocusedObject := nil;
        if FCollection.LastFocusedObject = ImagePanel then
            FCollection.LastFocusedObject := nil;
        if FCollection.PrevFocusedObject = ImagePanel then
            FCollection.PrevFocusedObject := nil;
    end;
    CollectionUpdateSelectedStatus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.CollectionTabSheetResize(Sender: TObject);
var
    WCount : Integer;
    HCount : Integer;
begin
    FCollection.NHCount := Trunc((CollectionTabSheet.ClientHeight - 1) / THUMB_HEIGHT);
    WCount              := Trunc((CollectionFlowPanel.Width - 1) / THUMB_WIDTH);
    if WCount <= 0 then
        WCount := 1;
    HCount              := 1 + (CollectionFlowPanel.ControlCount - 1) div WCount;
    if HCount <= 0 then
        HCount := 1;
    if WCount <> FCollection.WCount then begin
        CollectionFlowPanel.AutoWrap := FALSE;
        CollectionFlowPanel.AutoWrap := TRUE;
    end;
    FCollection.WCount  := WCount;
    FCollection.HCount  := HCount;
//    Display('%dx%d %dx%d %d %f %d',
//            [FCollectionWCount, FCollectionNHCount,
//             WCount, HCount,
//             CollectionFlowPanel.Width,
//             (CollectionFlowPanel.Width - 1) / THUMB_WIDTH,
//             Trunc((CollectionFlowPanel.Width - 1) / THUMB_WIDTH)]);
    CollectionScrollBox.VertScrollBar.Range := HCount * THUMB_HEIGHT;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.FileExitActionExecute(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.ActiveControlChangeHandler(
    Sender : TObject);
var
    FocusObject : TWinControl;
    ImageInfo   : TOvbImgImageInfo;
    ErrMsg      : String;
begin
    FocusObject := Screen.ActiveControl;
    if not Assigned(FocusObject) then
        Exit;
    if (FocusObject is TCheckBox) and
       Assigned(FocusObject.Parent) and
       Assigned(FocusObject.Parent.Parent) and
       Assigned(FocusObject.Parent.Parent.Parent) then begin
        if FocusObject.Parent.Parent.Parent.Name = CollectionFlowPanel.Name then begin
            if Assigned(FCollection.FocusedObject) then
                FCollection.FocusedObject.HasFocus := FALSE;
            FCollection.PrevFocusedObject      := FCollection.FocusedObject;
            FCollection.FocusedObject          := FocusObject.Parent.Parent as TOvbImagePanel;
            FCollection.FocusedObject.HasFocus := TRUE;
            FCollection.FocusedObject.HadFocus := FALSE;

            if Assigned(FCollection.LastFocusedObject) then
                FCollection.LastFocusedObject.HadFocus := FALSE;
            FCollection.LastFocusedObject      := FCollection.FocusedObject;
            ImageInfo := FCollection.FocusedObject.TagObject as TOvbImgImageInfo;
            if not ImageInfo.TagFetched then begin
                OvbImgOrganizerDataModule.DBFetchImageTagNames(ImageInfo.ImageID, ImageInfo.TagNames, ErrMsg);
                ImageInfo.TagFetched := TRUE;
            end;
            ImageTagListBox.Items     := ImageInfo.TagNames;
            StatusBar1.Panels[2].Text := ImageInfo.ImagePath + ImageInfo.ImageName;
        end;
    end
    else begin
        if Assigned(FCollection.FocusedObject) then begin
            FCollection.FocusedObject.HadFocus := TRUE;
            FCollection.FocusedObject.HasFocus := FALSE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TagSelectedImages(
    const TagNames : TStringDynArray);
var
    TagName      : String;
    TagID        : Integer;
    WasConnected : Boolean;
    I            : Integer;
    Image        : TOvbImagePanel;
    ImageInfo    : TOvbImgImageInfo;
    ErrMsg       : String;
    Status       : Integer;
begin
    if FCollection.Selected.Count <= 0 then begin
        ShowMessage(_('No image selected'));
        Exit;
    end;

    if not OvbImgOrganizerDataModule.DBConnect(WasConnected, ErrMsg) then begin
        ShowMessage(ErrMsg);
        Exit;
    end;
    try
        for I := Low(TagNames) to High(TagNames) do begin
            TagName := Trim(TagNames[I]);
            if TagName = '' then
                continue;
            if Length(TagName) < 3 then begin
                ShowMessage(_('Tag must have at least 3 characters, ignoring'));
                continue;
            end;

            // Check the tag ID
            TagID := OvbImgOrganizerDataModule.DBGetTagID(TagName, ErrMsg);
            if TagID < 0 then begin
                ShowMessage(ErrMsg);
                Exit;
            end;
            if TagID = 0 then begin
                ShowMessage(Format(_('Tag "%s" doesn''t exist'), [TagName]));
                continue;
            end;
//            Display('Tag "%s" has ID %d', [TagName, TagID]);

            for Image in FCollection.Selected do begin
                ImageInfo := Image.TagObject as TOvbImgImageInfo;
                ImageInfo.TagFetched := FALSE;
                Status := OvbImgOrganizerDataModule.DBInsertTagForImage(ImageInfo.ImageID, TagID, ErrMsg);
                if Status < 0 then
                    ShowMessage(ErrMsg)
                else if Status = 1 then
                    Display(_('Image "%s" now tagged with "%s"'),
                            [ImageInfo.ImageName, TagName])
                else
                    Display(_('Image "%s" was already tagged with "%s"'),
                            [ImageInfo.ImageName, TagName]);
            end;
        end;
    finally
        if not WasConnected then
            OvbImgOrganizerDataModule.DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.DeselectAllImagesPopupMnuClick(Sender: TObject);
var
    I : Integer;
begin
    for I := FCollection.Selected.Count - 1 downto 0 do begin
        FCollection.Selected[I].CheckBox.Checked := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.AskYesOrNoHandler(
    Sender         : TObject;
    const Question : String;
    const Caption  : String;
    var   Answer   : Integer);
begin
    Answer := TMessageForm.ShowMessage(Question, Caption, nil,
                                       [mffYesNo, mffWarning]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.BackupDatabaseActionExecute(Sender: TObject);
var
    Dir         : String;
    ErrMsg      : String;
    ZipFileName : String;
begin
    Dir := FLastBackupLocation;
    if not Vcl.FileCtrl.SelectDirectory(
                 _('Select destination for your backup'),
                 '',
                 Dir,
                 [sdNewFolder, sdShowEdit, sdShowShares,
                  sdNewUI, sdShowFiles, sdValidateDir
                  ],
                 nil) then
        Exit;
    if not TDirectory.Exists(Dir) then begin
        TMessageForm.ShowMessage(
            _('Failed: You must select a directory as destination'),
            _('Database backup'), nil,
            [mffFailed, mffAutoWidth]);
        Exit;
    end;

    if not OvbImgOrganizerDataModule.DBCreateBackup(Dir, ZipFileName, ErrMsg) then begin
        ShowMessage(ErrMsg);
        Exit;
    end;
    FLastBackupLocation := Dir;
        TMessageForm.ShowMessage(
             Format(_('Backup done to %s'), [ZipFileName]),
             _('Database backup'), nil,
             [mffOKTick, mffAutoWidth]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.HelpBitBtnClick(Sender: TObject);
var
    Form : THelpForm;
begin
    Form := THelpForm.Create(Self);
    try
        Form.ShowModal;
    finally
        FreeAndNil(Form);
    end;
    ActiveControl := CollectionSearchEdit;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerMainForm.TranslateStringHandler(
    Sender       : TObject;
    const Msg    : String;
    var   Xlated : String);
begin
    Xlated := GnuGetText.DefaultInstance.GetText_NoExtract(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TOvbImgOrganizerDropInfo }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TOvbImgOrganizerDropInfo.Create;
begin
    inherited Create;
    FileNames := TStringList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TOvbImgOrganizerDropInfo.Destroy;
begin
    FreeAndNil(FileNames);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerDropInfo.Clear;
begin
    FileNames.Clear;
    FileName      := '';
    Index         := 0;
    CountLoaded   := 0;
    CountSkipped  := 0;
    CountDropped  := 0;
    ImageID       := 0;
    ThumbFileName := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TOvbImgCollection }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TOvbImgCollection.Create;
begin
    inherited Create;
    Selected := TObjectList<TOvbImagePanel>.Create(FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TOvbImgCollection.Destroy;
begin
    FreeAndNil(Selected);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization

GetBuildInfo(GetModuleFileName(0), ProgVer, 2);

end.
