{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE @ Balteau-NDT
Creation:     May 17, 2020
Description:  Simple Image Organizer - Slide show
License:      This program is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OvbImgOrganizer.SlideShow;

interface

uses
    Winapi.Windows, Winapi.Messages, Winapi.ShellAPI,
    System.Variants, System.Classes, System.Types, System.Math,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Menus,
    Generics.Collections,
    GnuGetText,
    OvbImgOrganizer.ImagePanel,
    OvbImgOrganizer.DataTypes, Vcl.ComCtrls;

const
    WM_SETFULLSCREEN     = WM_USER + 1;
    PAN_SPEED            = 40;    // Steps to pan fully using keyboard
    ZOOM_SPEED           = 1.05;
    ZOOM_TRACKBAR_FACTOR = 16.0;


type
    // Data to handle full screen or windowed view
    TViewInfo = record
        NormalPos              : TRect;
        ViewRegion             : THandle;
    end;

    // TBlankForm is used to make the secondary completely black while
    // showing the picture on the other screen.
    TBlankForm = class;   // Actual definition somewhere below

    // TOvbTrackBar inherit from the standard TrackBar and add an OnEndTrack
    // event which is triggered when the user stop tracking the slider. This
    // event is also generated when the track bar loose focus.
    // A new property IgnoreNextChange has been added to the the next OnChange
    // event is ignored. Useful when setting an initial position and don't
    // want to have the change event triggered.
    TOvbTrackBar = class(TTrackBar)
    private
        FIgnoreNextChange  : Boolean;
        FOnEndTrack        : TNotifyEvent;
        FOnDisplay         : TOvbDisplayEvent;
        procedure CNVScroll(var Msg: TWMVScroll); message CN_VSCROLL;
        procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
        procedure Display(const Msg : String); overload;
        procedure Display(const Fmt : String; Args : array of const); overload;
    protected
        procedure Changed; override;
    public
        property IgnoreNextChange  : Boolean          read  FIgnoreNextChange
                                                      write FIgnoreNextChange;
    published
        property OnDisplay         : TOvbDisplayEvent read  FOnDisplay
                                                      write FOnDisplay;
        property OnEndTrack        : TNotifyEvent     read  FOnEndTrack
                                                      write FOnEndTrack;
    end;

    // TSlideShowForm is a form which is used to display one or more pictures.
    // It has been designed to looks like Windows 7 PhotoViewer.dll with
    // some enhancements.
    TSlideShowForm = class(TForm)
        ToolsPanel: TPanel;
        ButtonPanel: TPanel;
        NextBitBtn: TBitBtn;
        PreviousBitBtn: TBitBtn;
        ShowPanel: TPanel;
        FullScreenBitBtn: TBitBtn;
        SlideShowPopupMenu: TPopupMenu;
        NextPopupMnu: TMenuItem;
        PreviousPopupMnu: TMenuItem;
        FullScreenPopupMnu: TMenuItem;
        SlowSpeedPopupMnu: TMenuItem;
        SuspendPopupMnu: TMenuItem;
        SlideShowTimer: TTimer;
        MediumSpeedPopupMnu: TMenuItem;
        HighSpeedPopupMnu: TMenuItem;
        ShowInExplorerPopupMnu: TMenuItem;
        N1: TMenuItem;
        ZoomFitWindowPopupMnu: TMenuItem;
        FlipHorizontalPopupMnu: TMenuItem;
        FlipVerticalPopupMnu: TMenuItem;
        RotateRightPopupMnu: TMenuItem;
        RotateLeftPopupMnu: TMenuItem;
        RotateLeftBitBtn: TBitBtn;
        RotateRightBitBtn: TBitBtn;
        FlipVerticalBitBtn: TBitBtn;
        FlipHorizBitBtn: TBitBtn;
        ZoomRealSizeBitBtn: TBitBtn;
        ZoomRealSizePopupMnu: TMenuItem;
        PanCenterPopupMnu: TMenuItem;
        ZoomBitBtn: TBitBtn;
        procedure ToolsPanelResize(Sender: TObject);
        procedure NextBitBtnClick(Sender: TObject);
        procedure PreviousBitBtnClick(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FullScreenBitBtnClick(Sender: TObject);
        procedure NextPopupMnuClick(Sender: TObject);
        procedure PreviousPopupMnuClick(Sender: TObject);
        procedure FullScreenPopupMnuClick(Sender: TObject);
        procedure SlowSpeedPopupMnuClick(Sender: TObject);
        procedure SlideShowTimerTimer(Sender: TObject);
        procedure SuspendPopupMnuClick(Sender: TObject);
        procedure MediumSpeedPopupMnuClick(Sender: TObject);
        procedure HighSpeedPopupMnuClick(Sender: TObject);
        procedure ShowInExplorerPopupMnuClick(Sender: TObject);
        procedure ZoomFitWindowPopupMnuClick(Sender: TObject);
        procedure FlipHorizontalPopupMnuClick(Sender: TObject);
        procedure FlipVerticalPopupMnuClick(Sender: TObject);
        procedure RotateRightPopupMnuClick(Sender: TObject);
        procedure RotateLeftPopupMnuClick(Sender: TObject);
        procedure RotateLeftBitBtnClick(Sender: TObject);
        procedure RotateRightBitBtnClick(Sender: TObject);
        procedure FlipVerticalBitBtnClick(Sender: TObject);
        procedure FlipHorizBitBtnClick(Sender: TObject);
        procedure ZoomRealSizeBitBtnClick(Sender: TObject);
        procedure ZoomRealSizePopupMnuClick(Sender: TObject);
        procedure PanCenterPopupMnuClick(Sender: TObject);
        procedure ZoomBitBtnClick(Sender: TObject);
        procedure ToolsPanelMouseActivate(Sender            : TObject;
                                          Button            : TMouseButton;
                                          Shift             : TShiftState;
                                          X, Y              : Integer;
                                          HitTest           : Integer;
                                          var MouseActivate : TMouseActivate);
        procedure FormKeyPress(Sender: TObject; var Key: Char);
    private
    protected
        FFileNames          : TStrings;
        FCurrentImagePanel  : TOvbImagePanel;
        FCurrentIndex       : Integer;
        FViewInfo           : TViewInfo;
        FFullScreen         : Boolean;
        FBlankFormList      : TObjectList<TBlankForm>;
        FZoomTrackBar       : TOvbTrackBar;
        FOnDisplay          : TOvbImgDisplayEvent;
        procedure CreateParams(var Params: TCreateParams) ; override;
        procedure DoShow; override;
        procedure DoClose(var Action: TCloseAction); override;
        procedure Display(const Msg: String); overload;
        procedure Display(const Msg : String; Args : array of const); overload;
        procedure DisplayHandler(Sender : TObject; const Msg : String);
        procedure SetFileNames(const Value: TStrings);
        procedure LoadCurrentImage;
        procedure DoFullScreen(AFullScreen: Boolean);
        procedure SetFullScreen(AFullScreen : Boolean);
        procedure ImageClickHandler(Sender : TObject);
        procedure DialogKey(var Msg: TWMKey); message CM_DIALOGKEY;
        procedure WMSetFullScreen(var Msg: TMessage); message WM_SETFULLSCREEN;
        procedure BlankSecondMonitor;
        procedure BlankFormDestroyHandler(Sender: TObject);
        procedure DeleteAllBlankForms;
        procedure BlankFormClose(Sender: TObject; var Action: TCloseAction);
        procedure RotateLeft;
        procedure RotateRight;
        procedure FlipHorizontal;
        procedure FlipVertical;
        procedure ZoomFit;
        procedure ZoomRealSize;
        procedure ZoomTrackBarEndTrack(Sender: TObject);
        procedure ZoomTrackBarChange(Sender: TObject);
        procedure HideZoomTrackBar;
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        property Filenames                    : TStrings
                                                read  FFileNames
                                                write SetFileNames;
        property OnDisplay                    : TOvbImgDisplayEvent
                                                read  FOnDisplay
                                                write FOnDisplay;
    end;

    // TBlankForm is used to make the secondary completely black while
    // showing the picture on the other screen.
    TBlankForm = class(TCustomForm)
    protected
        FBottomPanel : TPanel;
        procedure CreateParams(var Params : TCreateParams); override;
    public
        constructor CreateNew(AOwner : TComponent; Dummy : Integer = 0); override;
    end;

implementation

uses
    System.SysUtils;

{$R *.dfm}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.FullScreenBitBtnClick(Sender: TObject);
begin
    SetFullScreen(TRUE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSlideShowForm.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    try
        TranslateComponent(Self);
    except

    end;
    FFileNames     := TStringList.Create;
    FCurrentIndex  := -1;
    KeyPreview     := TRUE;       // To intercept F11 for full screen
    FBlankFormList := TObjectList<TBlankForm>.Create(FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.CreateParams(var Params: TCreateParams);
begin
    inherited;
    Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
    Params.WndParent := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSlideShowForm.Destroy;
begin
    DeleteAllBlankForms;
    FreeAndNil(FBlankFormList);
    FreeAndNil(FFileNames);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.DoClose(var Action: TCloseAction);
begin
    Action := caFree;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.DoShow;
var
    Flag : Boolean;
begin
    Flag                        := FFileNames.Count > 1;
    NextBitBtn.Visible          := Flag;
    PreviousBitBtn.Visible      := Flag;
    NextPopupMnu.Visible        := Flag;
    PreviousPopupMnu.Visible    := Flag;
    SlowSpeedPopupMnu.Visible   := Flag;
    MediumSpeedPopupMnu.Visible := Flag;
    HighSpeedPopupMnu.Visible   := Flag;
    SuspendPopupMnu.Visible     := Flag;
    inherited DoShow;
    NextBitBtnClick(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// This is required to catch keys to avoid changing active control
procedure TSlideShowForm.DialogKey(var Msg: TWMKey);
begin
//    OutputDebugString(PChar(Format('DialogChar %d', [Msg.CharCode])));
    case Msg.CharCode of
    VK_RIGHT, VK_LEFT, VK_UP, VK_DOWN, VK_ESCAPE:
        begin
            Msg.Result := 1;
            if Assigned(OnKeyDown) then
                OnKeyDown(Self, Msg.CharCode, KeyDataToShiftState(Msg.KeyData));
        end;
    else
        inherited
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.DeleteAllBlankForms;
var
    Form : TBlankForm;
    I    : Integer;
begin
    if Assigned(FBlankFormList) then begin
        for I := FBlankFormList.Count - 1 downto 0 do begin
            Form := FBlankFormList[I];
            if Assigned(Form) then begin
                Form.OnDestroy    := nil;
                Form.OnClose      := nil;
                Form.OnKeyDown    := nil;
                FBlankFormList[I] := nil;
                FreeAndNil(Form);
            end;
            FBlankFormList.Delete(I);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.Display(const Msg: String);
begin
    if Assigned(OnDisplay) then
        OnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.Display(const Msg: String; Args: array of const);
begin
    Display(Format(Msg, Args));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.DisplayHandler(
    Sender    : TObject;
    const Msg : String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.ZoomFitWindowPopupMnuClick(Sender: TObject);
begin
    ZoomFit;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.ZoomFit;
begin
    FCurrentImagePanel.ZoomFit;
    FCurrentImagePanel.PanCenter;
    FCurrentImagePanel.ZoomManual := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.ZoomRealSize;
begin
    FCurrentImagePanel.ZoomRealSize;
    FCurrentImagePanel.PanCenter;
    FCurrentImagePanel.ZoomManual := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.ZoomRealSizeBitBtnClick(Sender: TObject);
begin
    ZoomRealSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.ZoomRealSizePopupMnuClick(Sender: TObject);
begin
    ZoomRealSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.FlipHorizBitBtnClick(Sender: TObject);
begin
    FlipHorizontal;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.FlipHorizontalPopupMnuClick(Sender: TObject);
begin
    FlipHorizontal;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.FlipHorizontal;
begin
    FCurrentImagePanel.FlipHorizontal :=
        not FCurrentImagePanel.FlipHorizontal;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.FlipVerticalBitBtnClick(Sender: TObject);
begin
    FlipVertical;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.FlipVerticalPopupMnuClick(Sender: TObject);
begin
    FlipVertical;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.FlipVertical;
begin
    FCurrentImagePanel.FlipVertical :=
        not FCurrentImagePanel.FlipVertical;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.FormKeyDown(
    Sender  : TObject;
    var Key : Word;
    Shift   : TShiftState);
begin
    // To intercept VK_LEFT, VK_RIGHT, VK_DOWN and VK_UP, those keys must
    // also be trapped in DialogChar procedure.
    case Key of
    VK_F11:
        begin
            if Shift = [] then begin
                PostMessage(Handle, WM_SETFULLSCREEN, Ord(BorderStyle = bsSizeable), 0);
                Key := 0;
            end;
        end;
    VK_ESCAPE:
        begin
            if FFullScreen then
                PostMessage(Handle, WM_SETFULLSCREEN, 0, 0);
            Key := 0;
        end;
    VK_SPACE:
        begin
            SuspendPopupMnuClick(nil);
            Key := 0;
        end;
    VK_RIGHT:
        begin
            if not (ssShift in Shift) then
                NextBitBtnClick(nil)
            else
                FCurrentImagePanel.PanHorizontal(FCurrentImagePanel.Width div PAN_SPEED);
            Key := 0;
        end;
    VK_LEFT:
        begin
            if not (ssShift in Shift) then
                PreviousBitBtnClick(nil)
            else
                FCurrentImagePanel.PanHorizontal(-FCurrentImagePanel.Width div PAN_SPEED);
            Key := 0;
        end;
    VK_UP:
        begin
            if ssShift in Shift then
                FCurrentImagePanel.PanVertical(-FCurrentImagePanel.Height div PAN_SPEED);
            Key := 0;
        end;
    VK_DOWN:
        begin
            if ssShift in Shift then
                FCurrentImagePanel.PanVertical(FCurrentImagePanel.Height div PAN_SPEED);
            Key := 0;
        end;
    VK_ADD:
        begin
            FCurrentImagePanel.ZoomFactor :=
                FCurrentImagePanel.ZoomFactor * ZOOM_SPEED;
        end;
    VK_SUBTRACT:
        begin
            FCurrentImagePanel.ZoomFactor :=
                FCurrentImagePanel.ZoomFactor / ZOOM_SPEED;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    case Key of
    '+':
        begin
            FCurrentImagePanel.ZoomFactor :=
                FCurrentImagePanel.ZoomFactor * ZOOM_SPEED;
            Key := #0;
        end;
    '-':
        begin
            FCurrentImagePanel.ZoomFactor :=
                FCurrentImagePanel.ZoomFactor / ZOOM_SPEED;
            Key := #0;
        end;
    '.':
        begin
            FCurrentImagePanel.ZoomFactor := 1.0;
            Key := #0;
        end;
    '=':
        begin
            FCurrentImagePanel.ZoomFit;
            FCurrentImagePanel.PanCenter;
            Key := #0;
        end;
    'r':
        begin
            FCurrentImagePanel.RotateFactor :=
                FCurrentImagePanel.RotateFactor + 90.0;
            Key := #0;
        end;
    'R':
        begin
            FCurrentImagePanel.RotateFactor :=
                FCurrentImagePanel.RotateFactor - 90.0;
            Key := #0;
        end;
    'f':
        begin
            FCurrentImagePanel.FlipHorizontal :=
                not FCurrentImagePanel.FlipHorizontal;
            Key := #0;
        end;
    'F':
        begin
            FCurrentImagePanel.FlipVertical :=
                not FCurrentImagePanel.FlipVertical;
            Key := #0;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.FormKeyUp(
    Sender   : TObject;
    var Key  : Word;
    Shift    : TShiftState);
begin
//    OutputDebugString(PChar(Format('KeyUp %s %d',
//                          [(Sender as TWinControl).ClassName, Key])));
    case Key of
    VK_F11,
    VK_ESCAPE,
    VK_SPACE,
    VK_RIGHT,
    VK_LEFT,
    VK_UP,
    VK_DOWN,
    VK_ADD,
    VK_SUBTRACT:
            Key := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.SetFileNames(const Value: TStrings);
begin
    FFileNames.Assign(Value);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.SetFullScreen(AFullScreen: Boolean);
begin
    if FFullScreen = AFullScreen then
        Exit;
    FFullScreen := AFullScreen;
    if Visible then
        DoFullScreen(FFullScreen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.ShowInExplorerPopupMnuClick(Sender: TObject);
begin
    if FCurrentIndex >= 0 then begin
        ShellExecute(0, nil, 'explorer.exe',
                     PChar('/select,"' + FFileNames[FCurrentIndex] + '"'),
                     '', SW_SHOW);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.DoFullScreen(AFullScreen: Boolean);
var
    CurMonitor   : TMonitor;
    Rc           : TRect;
    TopMargin    : Integer;
    LeftMargin   : Integer;
    BottomMargin : Integer;
    RightMargin  : Integer;
begin
    FFullScreen  := AFullScreen;
    CurMonitor   := Screen.MonitorFromWindow(Handle);
    TopMargin    := GetSystemMetrics(SM_CYCAPTION) +
                    GetSystemMetrics(SM_CYBORDER);
    LeftMargin   := GetSystemMetrics(SM_CXBORDER);
    RightMargin  := LeftMargin;
    BottomMargin := GetSystemMetrics(SM_CYBORDER);


    if FFullScreen then begin
        // Currently normal windowed view, switch to full screen
        FViewInfo.NormalPos    := BoundsRect;
        BorderStyle            := bsNone;//bsDialog;
        BorderIcons            := [];
        Constraints.MaxHeight  := CurMonitor.Height + TopMargin + BottomMargin;
        Rc                     := CurMonitor.BoundsRect;
        Rc.Top                 := Rc.Top    - TopMargin;
        Rc.Left                := Rc.Left   - LeftMargin;
        Rc.Bottom              := Rc.Bottom + BottomMargin + TopMargin;
        Rc.Right               := Rc.Right  + RightMargin;
        BoundsRect             := Rc;
        if FViewInfo.ViewRegion <> 0 then begin
            SetWindowRGN(Handle, 0, FALSE);
            DeleteObject(FViewInfo.ViewRegion);
        end;
        FViewInfo.ViewRegion := CreateRectRgn(LeftMargin, TopMargin,
                                              Width - LeftMargin,
                                              Height - LeftMargin);
        SetWindowRGN(Handle, FViewInfo.ViewRegion, TRUE);
        FCurrentImagePanel.SetBottomPanelColor(clBlack);
        ToolsPanel.Visible := FALSE;
        BlankSecondMonitor;
    end
    else begin
        // Currently full screen view, switch back to windowed
        SetWindowRGN(Handle, 0, TRUE);
        DeleteObject(FViewInfo.ViewRegion);
        SlideShowTimer.Enabled  := FALSE;
        FViewInfo.ViewRegion    := 0;
        Constraints.MaxHeight   := CurMonitor.Height;
        BorderStyle             := bsSizeable;
        BorderIcons             := [biSystemMenu, biMinimize, biMaximize];
        BoundsRect              := FViewInfo.NormalPos;
        FCurrentImagePanel.SetBottomPanelColor(clLtGray);
        ToolsPanel.Visible      := TRUE;
        DeleteAllBlankForms;
    end
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.ToolsPanelMouseActivate(
    Sender            : TObject;
    Button            : TMouseButton;
    Shift             : TShiftState;
    X, Y              : Integer;
    HitTest           : Integer;
    var MouseActivate : TMouseActivate);
begin
    HideZoomTrackBar;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.ToolsPanelResize(Sender: TObject);
begin
    ButtonPanel.Left := (ToolsPanel.ClientWidth  - ButtonPanel.Width)  div 2;
    ButtonPanel.Top  := (ToolsPanel.ClientHeight - ButtonPanel.Height) div 2;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.WMSetFullScreen(var Msg: TMessage);
begin
    SetFullScreen(Msg.WParam <> 0);
    ZoomFit;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.NextBitBtnClick(Sender: TObject);
begin
    if FCurrentIndex >= (FFileNames.Count - 1) then
        FCurrentIndex := 0
    else
        Inc(FCurrentIndex);
    LoadCurrentImage;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.PanCenterPopupMnuClick(Sender: TObject);
begin
    FCurrentImagePanel.PanCenter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.PreviousBitBtnClick(Sender: TObject);
begin
    if FCurrentIndex <= 0 then
        FCurrentIndex := FFileNames.Count - 1
    else
        Dec(FCurrentIndex);
    LoadCurrentImage;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.PreviousPopupMnuClick(Sender: TObject);
begin
    PreviousBitBtnClick(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.RotateLeftBitBtnClick(Sender: TObject);
begin
    RotateLeft;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.RotateLeftPopupMnuClick(Sender: TObject);
begin
    RotateLeft;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.RotateLeft;
begin
    FCurrentImagePanel.RotateFactor :=
        FCurrentImagePanel.RotateFactor - 90.0;
    if not FCurrentImagePanel.ZoomManual then begin
        FCurrentImagePanel.ZoomFit;
        FCurrentImagePanel.PanCenter;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.RotateRightBitBtnClick(Sender: TObject);
begin
    RotateRight;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.RotateRightPopupMnuClick(Sender: TObject);
begin
    RotateRight;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.RotateRight;
begin
    FCurrentImagePanel.RotateFactor :=
        FCurrentImagePanel.RotateFactor + 90.0;
    if not FCurrentImagePanel.ZoomManual then begin
        FCurrentImagePanel.ZoomFit;
        FCurrentImagePanel.PanCenter;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.NextPopupMnuClick(Sender: TObject);
begin
    NextBitBtnClick(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.FullScreenPopupMnuClick(Sender: TObject);
begin
    PostMessage(Handle, WM_SETFULLSCREEN, Ord(BorderStyle = bsSizeable), 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.SlideShowTimerTimer(Sender: TObject);
begin
    NextBitBtnClick(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.SlowSpeedPopupMnuClick(Sender: TObject);
begin
    SlideShowTimer.Interval := 5000;
    SlideShowTimer.Enabled  := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.MediumSpeedPopupMnuClick(Sender: TObject);
begin
    SlideShowTimer.Interval := 2500;
    SlideShowTimer.Enabled  := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.HighSpeedPopupMnuClick(Sender: TObject);
begin
    SlideShowTimer.Interval := 1000;
    SlideShowTimer.Enabled  := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.ImageClickHandler(Sender: TObject);
begin
    HideZoomTrackBar;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.SuspendPopupMnuClick(Sender: TObject);
begin
    SlideShowTimer.Enabled  := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.LoadCurrentImage;
var
    ImagePanel : TOvbImagePanel;
begin
    if not Assigned(FFileNames.Objects[FCurrentIndex]) then begin
        ImagePanel              := TOvbImagePanel.Create(Self);
        ImagePanel.Parent       := ShowPanel;
        ImagePanel.Align        := alClient;
        ImagePanel.OnDisplay    := DisplayHandler;
        ImagePanel.OnKeyDown    := FormKeyDown;
        ImagePanel.OnKeyUp      := FormKeyUp;
        ImagePanel.OnImageClick := ImageClickHandler;
        ImagePanel.ShowCheckbox(FALSE);
        ImagePanel.LoadImage(FFileNames[FCurrentIndex]);
        FFileNames.Objects[FCurrentIndex] := ImagePanel;
    end;

    ImagePanel := FFileNames.Objects[FCurrentIndex] as TOvbImagePanel;
    if FFullScreen then
        ImagePanel.SetBottomPanelColor(clBlack)
    else
        ImagePanel.SetBottomPanelColor(clLtGray);

    if ImagePanel <> FCurrentImagePanel then begin
         ImagePanel.Visible := TRUE;
         if Assigned(FCurrentImagePanel) then
             FCurrentImagePanel.Visible := FALSE;
         FCurrentImagePanel             := ImagePanel;
         Caption                        := FFileNames[FCurrentIndex];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.BlankFormDestroyHandler(Sender: TObject);
begin
    FBlankFormList.Remove(Sender as TBlankForm);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.ZoomTrackBarEndTrack(Sender: TObject);
begin
    HideZoomTrackBar;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.HideZoomTrackBar;
begin
    if Assigned(FZoomTrackBar) then
        FZoomTrackBar.Visible := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.ZoomTrackBarChange(Sender: TObject);
var
    Z : Extended;
begin
    Z := Power(ZOOM_SPEED, -FZoomTrackBar.Position / ZOOM_TRACKBAR_FACTOR);
    FCurrentImagePanel.ZoomFactor := Z;
    FCurrentImagePanel.PanCenter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.ZoomBitBtnClick(Sender: TObject);
var
    Pt   : TPoint;
    Z    : Extended;
begin
    if Assigned(FZoomTrackBar) and FZoomTrackBar.Visible then begin
        HideZoomTrackBar;
        Exit;
    end;

    // Get current mouse coordinates
    GetCursorPos(Pt);
    // Convert to ZoomBitBtn parent coordinates
    Pt := ZoomBitBtn.Parent.ScreenToclient(Pt);

    // Check if the mouse cursor in on the ZoomBitButton, that is the use
    // has clicked on the button with the mouse.
    if ZoomBitBtn.BoundsRect.Contains(Pt) then begin
        // Get current mouse position
        GetCursorPos(Pt);
        // Convert to form's position
        Pt := ScreenToclient(Pt);
    end
    else
        Pt := ScreenToClient(ZoomBitBtn.ClientToScreen(Point(0, ZoomBitBtn.Height)));

    if not Assigned(FZoomTrackBar) then begin
        FZoomTrackBar              := TOvbTrackBar.Create(Self);
        FZoomTrackBar.Parent       := Self;
        FZoomTrackBar.Orientation  := trVertical;
        FZoomTrackBar.Height       := 200;
        FZoomTrackBar.ShowSelRange := FALSE;
        FZoomTrackBar.TickStyle    := tsNone;
        FZoomTrackBar.Max          := Round(50 * ZOOM_TRACKBAR_FACTOR);
        FZoomTrackBar.Min          := -FZoomTrackBar.Max;
        FZoomTrackBar.OnEndTrack   := ZoomTrackBarEndTrack;
        FZoomTrackBar.OnChange     := ZoomTrackBarChange;
        FZoomTrackBar.OnDisplay    := DisplayHandler;
    end;

    FZoomTrackBar.IgnoreNextChange := TRUE;
    FZoomTrackBar.Top          := Pt.Y - FZoomTrackBar.Height + 20;
    FZoomTrackBar.Left         := Pt.X;
    Z := -Ln(FCurrentImagePanel.ZoomFactor) / Ln(ZOOM_SPEED);
    FZoomTrackBar.Position := Round(ZOOM_TRACKBAR_FACTOR * Z);
    FZoomTrackBar.Visible  := TRUE;
    ActiveControl          := FZoomTrackBar;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.BlankFormClose(
    Sender     : TObject;
    var Action : TCloseAction);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSlideShowForm.BlankSecondMonitor;
var
    I : Integer;
    CurrentMonitor : TMonitor;
    Mon            : TMonitor;
    Form           : TBlankForm;
begin
    if Screen.MonitorCount <= 1 then
        Exit;
    CurrentMonitor := Screen.MonitorFromWindow(Handle);
    for I := 0 to Screen.MonitorCount - 1 do begin
        Mon := Screen.Monitors[I];
        if Mon = CurrentMonitor then
            continue;
        Form             := TBlankForm.CreateNew(Application);
        Form.OnDestroy   := BlankFormDestroyHandler;
        Form.OnKeyDown   := FormKeyDown;
        Form.OnClose     := BlankFormClose;
        Form.PopupMenu   := SlideShowPopupMenu;
        Form.Show;
        Form.Top         := Mon.Top;
        Form.Left        := Mon.Left;
        Form.Width       := Mon.Width;
        Form.Height      := Mon.Height;
        Form.WindowState := wsMaximized;
        FBlankFormList.Add(Form);
    end;
    BringToFront;
    SetFocus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TBlankForm }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TBlankForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
    inherited CreateNew(AOwner, Dummy);
    BorderStyle                   := bsNone;
    TabStop                       := TRUE;
    KeyPreview                    := TRUE;

    FBottomPanel                  := TPanel.Create(Self);
    FBottomPanel.Parent           := Self;
    FBottomPanel.Height           := 16;
    FBottomPanel.Align            := alClient;
    FBottomPanel.ParentBackground := FALSE;
    FBottomPanel.ParentColor      := FALSE;
    FBottomPanel.Color            := clBlack;
    FBottomPanel.BevelOuter       := bvNone;
    FBottomPanel.ShowCaption      := FALSE;
    FBottomPanel.Visible          := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBlankForm.CreateParams(var Params: TCreateParams);
begin
    inherited CreateParams(Params);
    Params.WndParent := GetDesktopWindow;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TOvbTrackBar }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbTrackBar.Changed;
begin
    if FIgnoreNextChange then begin
       FIgnoreNextChange := FALSE;
       Exit;
    end;

    inherited Changed;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbTrackBar.CNVScroll(var Msg: TWMVScroll);
begin
    inherited;
    Display('CNVScroll %d', [Msg.ScrollCode]);
    if Msg.ScrollCode = Ord(TScrollCode.scEndScroll) then begin
        if Assigned(FOnEndTrack) then
            FOnEndTrack(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbTrackBar.Display(const Fmt: String; Args: array of const);
begin
    Display(Format(Fmt, Args));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbTrackBar.WMKillFocus(var Msg: TMessage);
begin
    inherited;
    Display('TrackBar focus lost');
    if Assigned(FOnEndTrack) then
        FOnEndTrack(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbTrackBar.Display(const Msg: String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
