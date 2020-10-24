{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE @ OverByte
Creation:     May 18, 2020
Description:  TOvbImagePanel is a form looking like a panel to display an
              image using TOvbCustomPaintPanel which is a Direct2D panel.
License:      This program is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OvbImgOrganizer.ImagePanel;

{$POINTERMATH ON}

interface

uses
    WinApi.Messages, WinApi.GdipApi, WinApi.GdipObj,
    WinApi.GdipUtil, WinApi.ActiveX, Winapi.D2D1,
    Winapi.DXGIFormat,
    System.UITypes, System.Types, System.Classes,
{$IFDEF USE_DIRECT2D_1}
    {$MESSAGE WARN 'Direct2D 1.1 not working yet'}
    Winapi.D2D1_1,
    Vcl.Direct2D_1,
{$ELSE}
    Vcl.Direct2D,
{$ENDIF}
    Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
    Vcl.Forms, Vcl.Menus,
    OvbImgOrganizer.PaintPanel;

type
    TOvbImageClickEvent = procedure (Sender    : TObject;
                                     Image     : TObject) of object;
    TOvbDisplayEvent    = procedure (Sender    : TObject;
                                     const Msg : String) of object;

    TOvbGpsCoord = record
        Lat    : Double;
        Lon    : Double;
        LatRef : Char;
        LonRef : Char;
        procedure LatFromRational(PropItemValue : Pointer);
        procedure LonFromRational(PropItemValue : Pointer);
    end;
    POvbGpsCoord = ^TOvbGpsCoord;

    TOvbCustomImage = class(TOvbCustomPaintPanel)
    private
        FFileName         : String;
        FGPBitmap         : TGpBitmap;
        FBitmapSize       : TD2D1SizeU;
        FBitmapToPaint    : ID2D1Bitmap;
        FZoomFactor       : Double;
        FZoomManual       : Boolean;
        FPanLeft          : Integer;
        FPanTop           : Integer;
        FFlipHoriz        : Boolean;
        FFlipVert         : Boolean;
        FOrientation      : WORD;
        FRotateFactor     : Double;
        FGpsCoord         : TOvbGpsCoord;
        FBackColor        : TColor;
        FTransform        : TD2DMatrix3X2F;
        FOnDisplay        : TOvbDisplayEvent;
        function  GetImageOrientation(AGPImage : TGPImage): WORD;
        procedure SetImageOrientation(AGPImage : TGPImage; Value : WORD);
        procedure SetBackColor(const Value: TColor);
        function  GetImageGpsCoord(AGPImage     : TGPImage;
                                   out GpsCoord : TOvbGpsCoord): Boolean;
        function  InitBitmapToPaint(AGPImage: TGPBitmap) : Boolean; overload;
        procedure Swap<T>(var IA, IB: T);
        function  ComputeTransform: TD2DMatrix3X2F;
        procedure SetFlipHorizontal(Value: Boolean);
        procedure SetFlipVertical(Value: Boolean);
        procedure SetPanLeft(const Value: Integer);
        procedure SetPanTop(const Value: Integer);
        procedure SetZoomFactor(const Value: Double);
        procedure SetRotateFactor(const Value: Double);
    protected
        procedure Display(const Msg : String); overload;
        procedure Display(const Fmt : String; Args : array of const); overload;
        procedure Resize; override;
{$IFDEF USE_DIRECT2D_1}
        procedure D2DCanvasReinitResources(Sender: TObject); override;
{$ENDIF}
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        function  LoadImage(const FileName : String) : Boolean;
        procedure CloseImage;
        function  CreateThumbnail(const Filename : String;
                                  const ThWidth  : Integer;
                                  const ThHeight : Integer): Boolean;
        procedure ZoomFit;
        procedure ZoomRealSize;
        procedure PanCenter;
        procedure PanHorizontal(PixelOffset : Integer);
        procedure PanVertical(PixelOffset: Integer);
        procedure Paint; override;
        property FlipHorizontal   : Boolean          read  FFlipHoriz
                                                     write SetFlipHorizontal;
        property FlipVertical     : Boolean          read  FFlipVert
                                                     write SetFlipVertical;
        property BackColor        : TColor           read  FBackColor
                                                     write SetBackColor;
        property Orientation      : WORD             read  FOrientation;
        property ZoomFactor       : Double           read  FZoomFactor
                                                     write SetZoomFactor;
        property ZoomManual       : Boolean          read  FZoomManual
                                                     write FZoomManual;
        property RotateFactor     : Double           read  FRotateFactor
                                                     write SetRotateFactor;
        property PanLeft          : Integer          read  FPanLeft
                                                     write SetPanLeft;
        property PanTop           : Integer          read  FPanTop
                                                     write SetPanTop;
        property OnDisplay        : TOvbDisplayEvent read  FOnDisplay
                                                     write FOnDisplay;
    end;
    TOvbImage = class(TOvbCustomImage)
    end;

    TOvbImagePanel = class(TCustomForm)
    private
        FImage            : TOvbImage;
        FBottomPanel      : TPanel;
        FLabel            : TLabel;
        FCheckBox         : TCheckBox;
        FSelected         : Boolean;
        FHasFocus         : Boolean;
        FHadFocus         : Boolean;
        FTagObject        : TObject;
        FTagObjectOwned   : Boolean;
        FIndex            : Integer;
        FIsThumbnail      : Boolean;
        FMouseDownFlag    : Boolean;
        FMouseMoveXY      : TPoint;
        FOnImageClick     : TNotifyEvent;
        FOnImageDblClick  : TNotifyEvent;
        FOnImageMouseDown : TMouseEvent;
        FOnCheckBoxClick  : TNotifyEvent;
        FOnDestroy        : TNotifyEvent;
        FOnDisplay        : TOvbDisplayEvent;
        procedure ImageReinitResourcesHandler(Sender: TObject);
        procedure ImageClickHandler(Sender : TObject);
        procedure ImageMouseDownHandler(Sender : TObject;
                                        Button : TMouseButton;
                                        Shift  : TShiftState;
                                        X, Y   : Integer);
        procedure ImageMouseUpHandler(Sender : TObject;
                                        Button : TMouseButton;
                                        Shift  : TShiftState;
                                        X, Y   : Integer);
        procedure ImageMouseMoveHandler(Sender : TObject;
                                        Shift  : TShiftState;
                                        X, Y   : Integer);
        procedure ImageDblClickHandler(Sender: TObject);
        procedure CheckBoxClickHandler(Sender : TObject);
        function  GetHasFocus: Boolean;
        procedure SetHasFocus(const Value: Boolean);
        function  GetHadFocus: Boolean;
        procedure SetHadFocus(const Value: Boolean);
        procedure SetBottomPanelColor; overload;
        procedure SetOnDisplay(const Value: TOvbDisplayEvent);
        function  GetOrientation: WORD;
        function  GetGpsCoord: POvbGpsCoord;
        procedure TranslateCoordMouseToBitmap(
                                        const XMouse,  YMouse : Integer;
                                        var   XBitmap, YBitmap : Integer);
        function  GetZoomFactor: Double;
        procedure SetZoomFactor(const Value: Double);
        function  GetZoomManual: Boolean;
        procedure SetZoomManual(const Value: Boolean);
        function  GetRotateFactor: Double;
        procedure SetRotateFactor(const Value: Double);
        function  GetFlipHorizontal: Boolean;
        function  GetFlipVertical: Boolean;
        procedure SetFlipHorizontal(const Value: Boolean);
        procedure SetFlipVertical(const Value: Boolean);
    protected
        procedure SetName(const Value: TComponentName); override;
        procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
        function  DoMouseWheel(Shift      : TShiftState;
                               WheelDelta : Integer;
                               MousePos   : TPoint): Boolean; override;
        procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
        procedure Display(const Msg : String); overload;
        procedure Display(const Fmt : String; Args : array of const); overload;
        procedure KeyDown(var Key: Word; Shift: TShiftState); override;
        procedure SetSelected(const Value : Boolean);
        function  GetSelected : Boolean;
        procedure SetBackColor(Value : TColor);
        function  GetBackColor : TColor;
    public
        constructor Create(AOwner : TComponent); override;
        constructor CreateNew(AOwner : TComponent; Dummy : Integer = 0); override;
        destructor  Destroy; override;
        function  LoadImage(const FileName : String) : Boolean; overload;
        procedure CloseImage;
        function  CreateThumbnail(const Filename : String;
                                  const ThWidth  : Integer;
                                  const ThHeight : Integer): Boolean;
        procedure ShowCheckbox(const Value : Boolean);
        procedure SetBottomPanelColor(Value : TColor); overload;
        procedure ZoomFit;
        procedure ZoomRealSize;
        procedure PanCenter;
        procedure PanHorizontal(PixelOffset: Integer);
        procedure PanVertical(PixelOffset: Integer);
        property Image            : TOvbImage        read  FImage;
        property ZoomFactor       : Double           read  GetZoomFactor
                                                     write SetZoomFactor;
        property ZoomManual       : Boolean          read  GetZoomManual
                                                     write SetZoomManual;
        property RotateFactor     : Double           read  GetRotateFactor
                                                     write SetRotateFactor;
        property FlipHorizontal   : Boolean          read  GetFlipHorizontal
                                                     write SetFlipHorizontal;
        property FlipVertical     : Boolean          read  GetFlipVertical
                                                     write SetFlipVertical;
        property TagObject        : TObject          read  FTagObject
                                                     write FTagObject;
        property Orientation      : WORD             read  GetOrientation;
        property GpsCoord         : POvbGpsCoord     read  GetGpsCoord;
    published
        property TagObjectOwned   : Boolean          read  FTagObjectOwned
                                                     write FTagObjectOwned;
        property CheckBox         : TCheckBox        read  FCheckBox;
        property BackColor        : TColor           read  GetBackColor
                                                     write SetBackColor;
        property Selected         : Boolean          read  GetSelected
                                                     write SetSelected;
        property HasFocus         : Boolean          read  GetHasFocus
                                                     write SetHasFocus;
        property HadFocus         : Boolean          read  GetHadFocus
                                                     write SetHadFocus;
        property Index            : Integer          read  FIndex
                                                     write FIndex;
        property IsThumbnail      : Boolean          read  FIsThumbnail
                                                     write FIsThumbnail;
        property PopupMenu;
        property OnImageClick     : TNotifyEvent     read  FOnImageClick
                                                     write FOnImageClick;
        property OnImageDblClick  : TNotifyEvent     read  FOnImageDblClick
                                                     write FOnImageDblClick;
        property OnImageMouseDown : TMouseEvent      read  FOnImageMouseDown
                                                     write FOnImageMouseDown;
        property OnCheckBoxClick  : TNotifyEvent     read  FOnCheckBoxClick
                                                     write FOnCheckBoxClick;
        property OnDestroy        : TNotifyEvent     read  FOnDestroy
                                                     write FOnDestroy;
        property OnDisplay        : TOvbDisplayEvent read  FOnDisplay
                                                     write SetOnDisplay;
        property OnKeyDown;
        property OnKeyUp;
        property OnKeyPress;
    end;



implementation

uses
    WinApi.Windows, System.SysUtils;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TOvbImagePanel }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TOvbImagePanel.Create(AOwner: TComponent);
begin
    CreateNew(AOwner);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TOvbImagePanel.CreateNew(AOwner: TComponent; Dummy : Integer = 0);
begin
    inherited CreateNew(AOwner, Dummy);
    KeyPreview                    := TRUE;
    BorderStyle                   := bsNone;
    TabStop                       := TRUE;

    FBottomPanel                  := TPanel.Create(Self);
    FBottomPanel.Parent           := Self;
    FBottomPanel.Height           := 16;
    FBottomPanel.Align            := alBottom;
    FBottomPanel.ParentBackground := FALSE;
    FBottomPanel.ParentColor      := FALSE;
    FBottomPanel.Color            := clLtGray;
    FBottomPanel.BevelOuter       := bvNone;
    FBottomPanel.ShowCaption      := FALSE;
    FBottomPanel.Visible          := TRUE;
    FImage                        := TOvbImage.Create(Self);
    FImage.OnDisplay              := FOnDisplay;
    FImage.Parent                 := Self;
    FImage.Align                  := alClient;
    FImage.BackColor              := clLtGray;
    FImage.Visible                := TRUE;
    FImage.OnClick                := ImageClickHandler;
    FImage.OnDblClick             := ImageDblClickHandler;
    FImage.OnMouseDown            := ImageMouseDownHandler;
    FImage.OnMouseUp              := ImageMouseUpHandler;
    FImage.OnMouseMove            := ImageMouseMoveHandler;
    FImage.OnReinitResources      := ImageReinitResourcesHandler;

    FCheckBox                     := TCheckBox.Create(Self);
    FCheckBox.Parent              := FBottomPanel;
    FCheckBox.Left                := 4;
    FCheckBox.Top                 := (FBottomPanel.Height - FCheckBox.Height) div 2;
    FCheckBox.Width               := FCheckBox.Height - 2;
    FCheckBox.OnClick             := CheckBoxClickHandler;
    FCheckBox.Visible             := TRUE;

    FLabel                        := TLabel.Create(Self);
    FLabel.Parent                 := FBottomPanel;
    FLabel.Left                   := FCheckBox.Width + 4;
    FLabel.Width                  := FBottomPanel.Width - 8;
    FLabel.Top                    := 2 + (FBottomPanel.Height - FLabel.Height) div 2;
    FLabel.AutoSize               := FALSE;
    FLabel.Anchors                := [akLeft, akTop, akRight, akBottom];
    FLabel.Caption                := Caption;
    FLabel.Alignment              := taCenter;
    FLabel.Visible                := TRUE;

    SetBottomPanelColor;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImagePanel.CreateThumbnail(
    const Filename : String;
    const ThWidth  : Integer;
    const ThHeight : Integer): Boolean;
begin
    Result := FImage.CreateThumbnail(FileName, ThWidth, ThHeight);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TOvbImagePanel.Destroy;
begin
    if Assigned(FOnDestroy) then
        FOnDestroy(Self);
    if Assigned(FTagObject) and (FTagObjectOwned) then
        FreeAndNil(FTagObject);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.CMTextChanged(var Msg: TMessage);
begin
    inherited;
    if Assigned(FLabel) then
        FLabel.Caption := Caption;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.Display(const Fmt: String; Args: array of const);
begin
    Display(Format(Fmt, Args));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImagePanel.DoMouseWheel(
    Shift      : TShiftState;
    WheelDelta : Integer;
    MousePos   : TPoint): Boolean;    // Screen coordinates
var
    XBitmap1, YBitmap1 : Integer;     // Bitmap coordinates
    XYMouse            : TPoint;      // Form coordinates
begin
    XYMouse := ScreenToClient(MousePos);
    TranslateCoordMouseToBitmap(XYMouse.X, XYMouse.Y, XBitmap1, YBitmap1);
    if WheelDelta > 0 then
        FImage.ZoomFactor := FImage.ZoomFactor * 1.05
    else
        FImage.ZoomFactor := FImage.ZoomFactor / 1.05;
    FImage.PanLeft    := XYMouse.X - Round(XBitmap1 * FImage.ZoomFactor);
    FImage.PanTop     := XYMouse.Y - Round(YBitmap1 * FImage.ZoomFactor);
    FImage.ZoomManual := TRUE;
    Result            := TRUE;
    if Assigned(OnMouseWheel) then
        OnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.Display(const Msg: String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.TranslateCoordMouseToBitmap(
    const XMouse,  YMouse  : Integer;
    var   XBitmap, YBitmap : Integer);
begin
    XBitmap := Round((XMouse - FImage.PanLeft) / FImage.ZoomFactor);
    YBitmap := Round((YMouse - FImage.PanTop)  / FImage.ZoomFactor);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.ZoomFit;
begin
    if Assigned(FImage) then
        FImage.ZoomFit;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.ZoomRealSize;
begin
    if Assigned(FImage) then
        FImage.ZoomRealSize;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.ImageReinitResourcesHandler(Sender: TObject);
begin

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.ImageClickHandler(Sender: TObject);
begin
    if Assigned(FOnImageClick) then
        FOnImageClick(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.ImageDblClickHandler(Sender: TObject);
begin
    if Assigned(FOnImageDblClick) then
        FOnImageDblClick(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.ImageMouseDownHandler(
    Sender : TObject;
    Button : TMouseButton;
    Shift  : TShiftState;
    X, Y   : Integer);
begin
    FMouseDownFlag := TRUE;
    FMouseMoveXY   := Point(X, Y);
    if Assigned(FOnImageMouseDown) then
        FOnImageMouseDown(Self, Button, Shift, X, Y);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.ImageMouseMoveHandler(
    Sender : TObject;
    Shift  : TShiftState;
    X, Y   : Integer);
var
    XBitmap, YBitmap : Integer;
begin
    if FIsThumbnail then
        Exit;
    TranslateCoordMouseToBitmap(X, Y, XBitmap, YBitmap);

    if FMouseDownFlag then begin
        FImage.PanLeft := FImage.PanLeft + X - FMouseMoveXY.X;
        FImage.PanTop  := FImage.PanTop  + Y - FMouseMoveXY.Y;
    end;
    FMouseMoveXY   := Point(X, Y);

//    Display('TOvbImagePanel.ImageMouseMove  Pos=(%d, %d) (%d, %d) Pan=(%d, %d) Z=%f',
//            [X, Y, XBitmap, YBitmap, FImage.PanLeft, FImage.PanTop,
//             FImage.ZoomFactor]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.ImageMouseUpHandler(
    Sender : TObject;
    Button : TMouseButton;
    Shift  : TShiftState;
    X, Y   : Integer);
begin
    FMouseDownFlag := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
//    Display('KeyDown');
    inherited KeyDown(Key, Shift);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImagePanel.LoadImage(const FileName: String): Boolean;
begin
    Result := FImage.LoadImage(FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
    Display('TOvbImagePanel.MouseMove (%d, %d)', [X, Y]);
    inherited MouseMove(Shift, X, Y);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.PanCenter;
begin
    if Assigned(FImage) then
        FImage.PanCenter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.PanHorizontal(PixelOffset : Integer);
begin
    if Assigned(FImage) then
        FImage.PanHorizontal(PixelOffset);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.PanVertical(PixelOffset : Integer);
begin
    if Assigned(FImage) then
        FImage.PanVertical(PixelOffset);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.CheckBoxClickHandler(Sender: TObject);
begin
    if Assigned(FOnCheckBoxClick) then
        FOnCheckBoxClick(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.CloseImage;
begin
    if Assigned(FImage) then
        FImage.CloseImage;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.SetName(const Value: TComponentName);
begin
    inherited SetName(Value);
    FCheckBox.Name    := Value + '_CheckBox';
    FLabel.Name       := Value + '_Label';
    FBottomPanel.Name := Value + '_BottomPanel';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.SetOnDisplay(const Value: TOvbDisplayEvent);
begin
    FOnDisplay := Value;
    if Assigned(FImage) then
        FImage.OnDisplay := FOnDisplay;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.SetRotateFactor(const Value: Double);
begin
    if Assigned(FImage) then
        FImage.RotateFactor := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.SetHadFocus(const Value: Boolean);
begin
    if FHadFocus = Value then
        Exit;
    FHadFocus := Value;
    SetBottomPanelColor;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.SetHasFocus(const Value: Boolean);
begin
    if FHasFocus = Value then
        Exit;
    FHasFocus := Value;
    SetBottomPanelColor;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImagePanel.GetGpsCoord: POvbGpsCoord;
begin
    if Assigned(FImage) then
        Result := @(FImage.FGpsCoord)
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImagePanel.GetHadFocus: Boolean;
begin
    Result := FHadFocus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImagePanel.GetHasFocus: Boolean;
begin
    Result := FHasFocus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImagePanel.GetOrientation: WORD;
begin
    if Assigned(FImage) then
        Result := FImage.Orientation
    else
        Result := 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImagePanel.GetRotateFactor: Double;
begin
    if Assigned(FImage) then
        Result := FImage.RotateFactor
    else
        Result := 1.0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImagePanel.GetSelected: Boolean;
begin
    Result := FSelected;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.SetZoomFactor(const Value: Double);
begin
    if Assigned(FImage) then
        FImage.ZoomFactor := Value
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImagePanel.GetZoomFactor: Double;
begin
    if Assigned(FImage) then
        Result := FImage.ZoomFactor
    else
        Result := 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.SetZoomManual(const Value: Boolean);
begin
    if Assigned(FImage) then
        FImage.ZoomManual:= Value
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImagePanel.GetZoomManual: Boolean;
begin
    if Assigned(FImage) then
        Result := FImage.ZoomManual
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.SetBackColor(Value : TColor);
begin
    if Assigned(FImage) then
        FImage.BackColor := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOvbImagePanel.GetBackColor : TColor;
begin
    if Assigned(FImage) then
        Result := FImage.BackColor
    else
        Result := clBlack;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImagePanel.GetFlipHorizontal: Boolean;
begin
    if Assigned(FImage) then
       Result := FImage.FlipHorizontal
   else
       Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImagePanel.GetFlipVertical: Boolean;
begin
    if Assigned(FImage) then
       Result := FImage.FlipVertical
   else
       Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.ShowCheckbox(const Value: Boolean);
begin
    FCheckBox.Visible    := Value;
    FBottomPanel.Visible := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.SetSelected(const Value: Boolean);
begin
    if FSelected = Value then
        Exit;
    FSelected := Value;
    SetBottomPanelColor;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.SetBottomPanelColor(Value: TColor);
begin
    FBottomPanel.Color := Value;
    FImage.BackColor   := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.SetFlipHorizontal(const Value: Boolean);
begin
    if Assigned(FImage) then
       FImage.FlipHorizontal := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.SetFlipVertical(const Value: Boolean);
begin
    if Assigned(FImage) then
       FImage.FlipVertical := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImagePanel.SetBottomPanelColor;
begin
    if not Assigned(FBottomPanel) then
        Exit;
    if FHasFocus then begin
        FBottomPanel.Color := clBlue;
        FLabel.Font.Color  := clWhite;
    end
    else if FHadFocus then begin
        FBottomPanel.Color := clMaroon;
        FLabel.Font.Color  := clWhite;
    end
    else begin
        if FSelected then begin
            FBottomPanel.Color := clDkGray;
            FLabel.Font.Color  := clWhite;
        end
        else begin
            FBottomPanel.Color := clLtGray;
            FLabel.Font.Color  := clBlack;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TOvbGpsCoord }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbGpsCoord.LatFromRational(PropItemValue: Pointer);
var
    Degree, Minute, Second : Double;
begin
    Degree   := PUINT32(PropItemValue)[0] /
                PUINT32(PropItemValue)[1];
    Minute   := PUINT32(PropItemValue)[2] /
                PUINT32(PropItemValue)[3];
    Second   := PUINT32(PropItemValue)[4] /
                PUINT32(PropItemValue)[5];
    Lat      := Degree + Minute / 60 + Second / 3600;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbGpsCoord.LonFromRational(PropItemValue: Pointer);
var
    Degree, Minute, Second : Double;
begin
    Degree   := PUINT32(PropItemValue)[0] /
                PUINT32(PropItemValue)[1];
    Minute   := PUINT32(PropItemValue)[2] /
                PUINT32(PropItemValue)[3];
    Second   := PUINT32(PropItemValue)[4] /
                PUINT32(PropItemValue)[5];
    Lon      := Degree + Minute / 60 + Second / 3600;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TOvbCustomImage }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TOvbCustomImage.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FZoomFactor := 1.0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TOvbCustomImage.Destroy;
begin
    FreeAndNil(FGPBitmap);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.Display(const Msg: String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.Display(const Fmt: String; Args: array of const);
begin
    Display(Format(Fmt, Args));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.SetFlipHorizontal(Value : Boolean);
begin
    FFlipHoriz := Value;
    FTransform := ComputeTransform;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.SetFlipVertical(Value : Boolean);
begin
    FFlipVert  := Value;
    FTransform := ComputeTransform;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbCustomImage.GetImageGpsCoord(
    AGPImage     : TGPImage;
    out GpsCoord : TOvbGpsCoord): Boolean;
var
    PropItem       : PPropertyItem;
    PropertyIdList : array of TPropID;
    FoundLat       : Boolean;
    FoundLon       : Boolean;
    Status         : TStatus;
begin
    Result          := FALSE;
    GpsCoord.Lat    := 0;
    GpsCoord.Lon    := 0;
    GpsCoord.LatRef := ' ';
    GpsCoord.LonRef := ' ';
    FoundLat        := FALSE;
    FoundLon        := FALSE;
    if not Assigned(AGPImage) then
        Exit;
    SetLength(PropertyIdList, AGPImage.GetPropertyCount);
    Status := AGPImage.GetPropertyIdList(Length(PropertyIdList),
                                  @PropertyIdList[0]);
    if Status <> TStatus.OK then
        Exit;
    GetMem(PropItem, 2048);
    try
        for var PropId in PropertyIdList do begin
//            Display('PropId = %-5d  %04X', [PropId, PropId]);
            case PropId of
            PropertyTagGpsLatitudeRef:
                begin
                    Status := AGPImage.GetPropertyItem(PropId,
                                             AGPImage.GetPropertyItemSize(PropId),
                                             PropItem);
                    if (Status = TStatus.OK) and
                       (PropItem.Type_ = PropertyTagTypeASCII) and
                       (PropItem.Length    = 2) then
                        GpsCoord.LatRef := Char(PAnsiChar(PropItem.Value)^);
                end;
            PropertyTagGpsLatitude:
                begin
                    Status := AGPImage.GetPropertyItem(PropId,
                                             AGPImage.GetPropertyItemSize(PropId),
                                             PropItem);
                    if (Status = TStatus.OK) and
                       (PropItem.Type_ = PropertyTagTypeRational) and
                       (PropItem.Length    = 24) then begin
                        GpsCoord.LatFromRational(PropItem.Value);
                        FoundLat     := TRUE;
                    end;
                end;
            PropertyTagGpsLongitude:
                begin
                    Status := AGPImage.GetPropertyItem(PropId,
                                             AGPImage.GetPropertyItemSize(PropId),
                                             PropItem);
                    if (Status = TStatus.OK) and
                       (PropItem.Type_ = PropertyTagTypeRational) and
                       (PropItem.Length    = 24) then begin
                        GpsCoord.LonFromRational(PropItem.Value);
                        FoundLon     := TRUE;
                    end;
                end;
            PropertyTagGpsLongitudeRef:
                begin
                    AGPImage.GetPropertyItem(PropId,
                                             AGPImage.GetPropertyItemSize(PropId),
                                             PropItem);
                    if (Status = TStatus.OK) and
                       (PropItem.Type_ = PropertyTagTypeASCII) and
                       (PropItem.Length    = 2) then
                        GpsCoord.LonRef := Char(PAnsiChar(PropItem.Value)^);
                end;
            end;
            if FoundLat and FoundLon then begin
                Result := TRUE;
                break;
            end;
        end;
    finally
        FreeMem(PropItem);
    end;
    if Result then begin
        if GpsCoord.LatRef = 'W' then
            GpsCoord.Lat := -GpsCoord.Lat;
        if GpsCoord.LonRef = 'S' then
            GpsCoord.Lon := -GpsCoord.Lon;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbCustomImage.GetImageOrientation(AGPImage: TGPImage): WORD;
var
    ValueType      : WORD;
    PropItem       : PPropertyItem;
    PropertyIdList : array of TPropID;
begin
    Result         := 1;
    if not Assigned(AGPImage) then
        Exit;
    SetLength(PropertyIdList, AGPImage.GetPropertyCount);
    if AGPImage.GetPropertyIdList(Length(PropertyIdList),
                                  @PropertyIdList[0]) <> TStatus.OK then
        Exit;
    GetMem(PropItem, 2048);
    try
        for var PropId in PropertyIdList do begin
            if PropId = PropertyTagOrientation then begin
                AGPImage.GetPropertyItem(PropertyTagOrientation,
                                         AGPImage.GetPropertyItemSize(PropId),
                                         PropItem);
                ValueType   := PropItem.type_;
                if ValueType = PropertyTagTypeShort then begin
                    Result := PWORD(PropItem.Value)^;
                    Exit;
                end;
            end;
        end;
    finally
        FreeMem(PropItem);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.SetBackColor(const Value: TColor);
begin
    if FBackColor = Value then
        Exit;
    FBackColor := Value;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.SetImageOrientation(AGPImage: TGPImage; Value: WORD);
var
    PropItem : TPropertyItem;
begin
    if not Assigned(AGPImage) then
        Exit;
    PropItem.Id            := PropertyTagOrientation;
    PropItem.Length        := SizeOf(WORD);
    PropItem.Type_         := PropertyTagTypeShort;
    PropItem.Value         := @Value;
    AGPImage.SetPropertyItem(PropItem);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.SetPanLeft(const Value: Integer);
begin
    FPanLeft   := Value;
    FTransform := ComputeTransform;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.SetPanTop(const Value: Integer);
begin
    FPanTop    := Value;
    FTransform := ComputeTransform;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.SetRotateFactor(const Value: Double);
begin
    FRotateFactor := Value;
    while FRotateFactor >= 360.0 do
        FRotateFactor := FRotateFactor - 360.0;
    while FRotateFactor < 0.0 do
        FRotateFactor := FRotateFactor + 360.0;
    FTransform    := ComputeTransform;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.SetZoomFactor(const Value: Double);
begin
    FZoomFactor := Value;
    Display('Zoom=%f', [FZoomFactor]);
    FTransform  := ComputeTransform;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbCustomImage.CreateThumbnail(
    const Filename : String;
    const ThWidth  : Integer;
    const ThHeight : Integer): Boolean;
var
    DX, DY     : Integer;
    R1, R2     : Double;
    TmpImage   : TGPImage;
    ClsId      : TGUID;
begin
    Result := FALSE;
    if FGPBitmap = nil then
        Exit;

    R1 := ThWidth / ThHeight;
    R2 := FGPBitmap.GetWidth / FGPBitmap.GetHeight;
    if R1 <= R2 then begin
        DX := ThWidth;
        DY := Round(FGPBitmap.GetHeight / FGPBitmap.GetWidth * ThWidth);
    end
    else begin
        DX := Round(FGPBitmap.GetWidth / FGPBitmap.GetHeight * ThHeight);
        DY := ThHeight;
    end;

    TmpImage := FGPBitmap.GetThumbnailImage(DX, DY, nil, nil);
    try
        SetImageOrientation(TmpImage, GetImageOrientation(FGPBitmap));
        ForceDirectories(ExtractFilePath(FileName));
        ClsId := TGUID.Empty;
        if GetEncoderClsid('image/jpeg', ClsId) < 0 then
            Exit;
        if TmpImage.Save(FileName, ClsId) <> TStatus.Ok then
            Exit;
    finally
        FreeAndNil(TmpImage);
    end;
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CreateDirect2DBitmap(
    RenderTarget : ID2D1RenderTarget;
    GPBitmap     : TGPBitmap): ID2D1Bitmap; overload;
var
    BitmapBuf        : array of Byte;          // ARGB buffer
    BitmapProperties : TD2D1BitmapProperties;
    BmpData          : TBitmapData;
    HR               : HRESULT;
begin
    Result := nil;
    if (GPBitmap.GetHeight = 0) or (GPBitmap.GetWidth = 0) then
        Exit;

    SetLength(BitmapBuf, GPBitmap.GetHeight * GPBitmap.GetWidth * 4);
    if GPBitmap.LockBits(MakeRect(0, 0,
                                  Integer(GPBitmap.GetWidth),
                                  Integer(GPBitmap.GetHeight)),
                         ImageLockModeRead,
                         PixelFormat32bppARGB, BmpData) <> TStatus.Ok then
        raise Exception.Create('Bitmap.LockBits failed');
    try
        BitmapProperties.dpiX := 0;
        BitmapProperties.dpiY := 0;
        BitmapProperties.pixelFormat.format := DXGI_FORMAT_B8G8R8A8_UNORM;
    //    if (GPBitmap.PixelFormat <> pf32bit) or (GPBitmap.AlphaFormat = afIgnored) then
    //        BitmapProperties.pixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE
    //    else
            BitmapProperties.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;

        // CreateBitmap will fails under VMware Workstation 15.5.6 for images
        // larger than 8192x4096.
        HR := RenderTarget.CreateBitmap(
                              D2D1SizeU(GPBitmap.GetWidth, GPBitmap.GetHeight),
                              BmpData.Scan0,
                              BmpData.Stride,
                              BitmapProperties,
                              Result);
        if HR <> S_OK then
            raise Exception.CreateFmt(
                      'RenderTarget.CreateBitmap failed with error 0x%X', [HR]);
    finally
        GPBitmap.UnlockBits(BmpData);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbCustomImage.ComputeTransform : TD2DMatrix3X2F;
var
    Scaling      : TD2DMatrix3X2F;
    Translation  : TD2DMatrix3X2F;
    Rotation1    : TD2DMatrix3X2F;
    Rotation2    : TD2DMatrix3X2F;
    FlippingH    : TD2DMatrix3X2F;
    FlippingHT   : TD2DMatrix3X2F;
    FlippingV    : TD2DMatrix3X2F;
    FlippingVT   : TD2DMatrix3X2F;
const
    FlipH : D2D_MATRIX_3X2_F = (
      _11: -1.0; _12: 0.0;
      _21:  0.0; _22: 1.0;
      _31:  0.0; _32: 0.0; );
    FlipV : D2D_MATRIX_3X2_F = (
      _11: 1.0; _12:  0.0;
      _21: 0.0; _22: -1.0;
      _31: 0.0; _32:  0.0; );
begin
    if Abs(FZoomFactor - 1.0) <= 1E-5 then
        Scaling := TD2DMatrix3x2F.Identity
    else
        Scaling := TD2DMatrix3x2F.Scale(D2D1SizeF(FZoomFactor, FZoomFactor),
                                        D2D1PointF(0, 0));
    Translation := TD2DMatrix3x2F.Translation(FPanLeft, FPanTop);

    if FOrientation in [6] then begin
        Rotation1 := TD2DMatrix3x2F.Rotation(90.0,
                                             FBitmapSize.Width  div 2,
                                             FBitmapSize.Height div 2)
    end
    else if FOrientation in [8] then begin
        Rotation1 := TD2DMatrix3x2F.Rotation(270.0,
                                             FBitmapSize.Width  div 2,
                                             FBitmapSize.Height div 2)
    end
    else
        Rotation1 := TD2DMatrix3x2F.Identity;

    Rotation2 := TD2DMatrix3x2F.Rotation(FRotateFactor,
                                         FBitmapSize.Width  div 2,
                                         FBitmapSize.Height div 2);

    if not FFlipHoriz then begin
        FlippingH     := TD2DMatrix3x2F.Identity;
        FlippingHT    := TD2DMatrix3x2F.Identity;
    end
    else begin
        FlippingH     := FlipH;
        FlippingHT    := TD2DMatrix3x2F.Translation(-FBitmapSize.Width, 0);
    end;

    if not FFlipVert then begin
        FlippingV     := TD2DMatrix3x2F.Identity;
        FlippingVT    := TD2DMatrix3x2F.Identity;
    end
    else begin
        FlippingV     := FlipV;
        FlippingVT    := TD2DMatrix3x2F.Translation(0, -FBitmapSize.Height);
    end;

    Result := FlippingVT * FlippingV *
              FlippingHT * FlippingH *
              Rotation1  * Rotation2 *
              Scaling    * Translation;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.Paint;
begin
    inherited Paint;
    if (FBitmapToPaint = nil) and (FGPBitmap <> nil) then begin
        InitBitmapToPaint(FGPBitmap);
        ZoomFit;
        PanCenter;
    end;

    // Paint background
    RenderTarget.Clear(D2D1ColorF(FBackColor));
    // Paint bitmap, if any
    if FBitmapToPaint <> nil then begin
        RenderTarget.SetTransform(FTransform);
        RenderTarget.DrawBitmap(FBitmapToPaint, nil, 1.0,
                                D2D1_BITMAP_INTERPOLATION_MODE_LINEAR,
//                                D2D1_BITMAP_INTERPOLATION_MODE(Ord(Winapi.D2D1_1.D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC)),
                                nil);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.PanCenter;
var
    Size   : D2D1_SIZE_U;
begin
    if Assigned(FBitmapToPaint) then begin
        FBitmapToPaint.GetPixelSize(Size);
        FPanLeft   := (Width  - Round(Size.Width * FZoomFactor))  div 2;
        FPanTop    := (Height - Round(Size.Height * FZoomFactor)) div 2;
        FTransform := ComputeTransform;
        Invalidate;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.PanHorizontal(PixelOffset: Integer);
begin
    FPanLeft   := FPanLeft + PixelOffset;
    FTransform := ComputeTransform;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.PanVertical(PixelOffset: Integer);
begin
    FPanTop    := FPanTop + PixelOffset;
    FTransform := ComputeTransform;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.Swap<T>(var IA, IB : T);
var
    TT : T;
begin
    TT := IA;
    IA := IB;
    IB := TT;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF USE_DIRECT2D_1}
procedure TOvbCustomImage.D2DCanvasReinitResources(Sender: TObject);
begin
    inherited;
    InitBitmapToPaint(FGPBitmap);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.ZoomFit;
var
    Size   : D2D1_SIZE_U;
    RW, RH : Double;
begin
    if Assigned(FBitmapToPaint) then begin
        FBitmapToPaint.GetPixelSize(Size);
        if FOrientation in [6, 8] then
            Swap<System.UINT32>(Size.Width, Size.Height);
        if (RotateFactor = 90.0) or (RotateFactor = 270.0) then
            Swap<System.UINT32>(Size.Width, Size.Height);
        RW := Width  / Size.Width;
        RH := Height / Size.Height;
        if RW < RH then
            FZoomFactor := RW
        else
            FZoomFactor := RH;
        FTransform := ComputeTransform;
    end;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.ZoomRealSize;
begin
    FZoomFactor := 1.0;
    FTransform  := ComputeTransform;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.Resize;
begin
    if not FZoomManual then begin
        ZoomFit;
        PanCenter;
    end;
    inherited Resize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbCustomImage.InitBitmapToPaint(AGPImage : TGPBitmap) : Boolean;
begin
    if (D2DCanvas = nil) or (AGPImage = nil) then
        FBitmapToPaint := nil
    else begin
        FBitmapToPaint := CreateDirect2DBitmap(D2DCanvas.RenderTarget,
                                               AGPImage);
        Invalidate;
    end;
    Result := TRUE;
//    Display('Bitmap loaded');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbCustomImage.LoadImage(const FileName : String) : Boolean;
begin
    try
        FGPBitmap.Free;
        FFileName          := '';
        FGPBitmap          := TGPBitmap.Create(FileName);
        FBitmapSize.Width  := FGPBitmap.GetWidth;
        FBitmapSize.Height := FGPBitmap.GetHeight;
        FOrientation       := GetImageOrientation(FGPBitmap);
        FTransform         := ComputeTransform;
        GetImageGpsCoord(FGPBitmap, FGpsCoord);

        Result := InitBitmapToPaint(FGPBitmap);
        if Result then begin
            FFileName := FileName;
            ZoomFit;
            PanCenter;
        end;
    except
        on E:Exception do begin
            Display(E.ClassName + ': ' + E.Message);
            Result := FALSE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomImage.CloseImage;
begin
    FreeAndNil(FGPBitmap);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
