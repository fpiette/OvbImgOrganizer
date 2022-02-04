{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE (francois.piette@overbyte.be)
Creation:     Aug 09, 2020
Description:  A Direct2D_1 VCL canvas.
License:      This program is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.0
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Vcl.Direct2D_1;

interface

uses
    Winapi.Windows, Winapi.Messages, WinApi.D2D1, Winapi.D2D1_1,
    Winapi.D3DCommon, Winapi.D3D11, Winapi.D3D11_1,
    Winapi.DXGIFormat, Winapi.DXGI, Winapi.DXGI1_2, Winapi.DWrite_1,
    System.Types, System.UITypes, System.SysUtils, System.Classes,
    System.Generics.Collections, System.Win.ComObj, System.Math,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Consts;

type
    TDirect2D_1Canvas    = class;
    EDirect2D_1Exception = class(Exception);

    TDirect2D_1GraphicsObject = class(TPersistent)
    private
        FOnChange : TNotifyEvent;
        FOwner    : TDirect2D_1Canvas;
    protected
        procedure Changed; dynamic;
        procedure ReleaseHandle; dynamic;
    public
        constructor Create(AOwner: TDirect2D_1Canvas); dynamic;
        destructor  Destroy; override;

        property Owner    : TDirect2D_1Canvas read  FOwner;
        property OnChange : TNotifyEvent      read  FOnChange
                                              write FOnChange;
    end;

    TDirect2D_1Brush = class(TDirect2D_1GraphicsObject)
    private
        FBitmap  : TBitmap;
        FBrush   : ID2D1Brush;
        FColor   : TColor;
        FStyle   : TBrushStyle;
        function  GetBitmap: TBitmap;
        procedure SetBitmap(const Value: TBitmap);
        procedure BitmapChanged(Sender: TObject);
    protected
        function  GetColor: TColor;
        function  GetHandle: ID2D1Brush;
        function  GetStyle: TBrushStyle;
        procedure SetColor(const Value: TColor);
        procedure SetHandle(const Value: ID2D1Brush);
        procedure SetStyle(const Value: TBrushStyle);
        procedure ReleaseHandle; override;
    public
        constructor Create(Owner: TDirect2D_1Canvas); override;
        destructor  Destroy; override;

        procedure Assign(Source: TPersistent); override;
    published
        property Bitmap  : TBitmap     read GetBitmap write SetBitmap;
        property Color   : TColor      read GetColor  write SetColor;
        property Style   : TBrushStyle read GetStyle  write SetStyle;
        property Handle  : ID2D1Brush  read GetHandle write SetHandle;
    end;

    TDirect2D_1Pen = class(TDirect2D_1GraphicsObject)
    private
        FBrush       : TDirect2D_1Brush;
        FStrokeStyle : ID2D1StrokeStyle;
        FStyle       : TPenStyle;
        FWidth       : Integer;
    protected
        function  GetBrush: TDirect2D_1Brush;
        function  GetColor: TColor;
        function  GetStyle: TPenStyle;
        procedure SetBrush(const Value: TDirect2D_1Brush);
        procedure SetColor(Color: TColor);
        procedure SetStyle(const Value: TPenStyle);
        procedure ReleaseHandle; override;
    public
        constructor Create(Owner: TDirect2D_1Canvas); override;
        destructor  Destroy; override;

        procedure Assign(Source: TPersistent); override;
    published
        property Brush       : TDirect2D_1Brush read    GetBrush
                                                write   SetBrush;
        property Color       : TColor           read    GetColor
                                                write   SetColor
                                                default clBlack;
        property StrokeStyle : ID2D1StrokeStyle read    FStrokeStyle;
        property Style       : TPenStyle        read    GetStyle
                                                write   SetStyle
                                                default psSolid;
        property Width       : Integer          read    FWidth
                                                write   FWidth
                                                default 1;
    end;

    TDirect2D_1Font = class(TDirect2D_1GraphicsObject)
    private
        FBrush         : TDirect2D_1Brush;
        FHeight        : Integer;
        FName          : TFontName;
        FOrientation   : Integer;
        FPitch         : TFontPitch;
        FStyle         : TFontStyles;
        FPixelsPerInch : Integer;
    protected
        function  GetBrush: TDirect2D_1Brush;
        function  GetColor: TColor;
        function  GetHandle: IDWriteTextFormat;
        function  GetHeight: Integer;
        function  GetSize: Integer;
        procedure SetHeight(const Value: Integer);
        procedure SetSize(const Value: Integer);
        procedure SetBrush(const Value: TDirect2D_1Brush);
        procedure SetColor(const Value: TColor);
        procedure ReleaseHandle; override;
    public
        constructor Create(AOwner: TDirect2D_1Canvas); override;
        destructor Destroy; override;
        procedure  Assign(Source: TPersistent); override;
        property Handle: IDWriteTextFormat read GetHandle;
    published
        property Color         : TColor           read    GetColor
                                                  write   SetColor;
        property Brush         : TDirect2D_1Brush read    GetBrush
                                                  write   SetBrush;
        property PixelsPerInch : Integer          read    FPixelsPerInch
                                                  write   FPixelsPerInch;
        property Height        : Integer          read    GetHeight
                                                  write   SetHeight;
        property Name          : TFontName        read    FName
                                                  write   FName;
        property Orientation   : Integer          read    FOrientation
                                                  write   FOrientation
                                                  default 0;
        property Pitch         : TFontPitch       read    FPitch
                                                  write   FPitch
                                                  default fpDefault;
        property Size          : Integer          read    GetSize
                                                  write   SetSize;
        property Style         : TFontStyles      read    FStyle
                                                  write   FStyle;
    end;

    TDirect2D_1Canvas = class(TCustomCanvas)
    strict private
    class var
        FSupported             : TUncertainState;
        FDefaultDrawTextOption : D2D1_DRAW_TEXT_OPTIONS;
    private
        // Direct2D factory access (remove '1' for Direct2D 1.0)
        FDirect2D                : ID2D1Factory1;
        // Direct3D device
        FDirect3D                : ID3D11Device1;
        // The render target (window surface) to access features of Direct2D.
        FRenderTarget            : ID2D1DeviceContext;
        // Direct3D device context
        FScreen3D                : ID3D11DeviceContext1;
        // Direct2D device
        FDirect2DDevice          : ID2D1Device;
        FEnable3D                : Boolean;
        FMinFeatureLevel         : D3D_FEATURE_LEVEL;
        FEnableMSAA              : Boolean;
        FEnableFullScreen        : Boolean;
        FEnableModeSwitch        : Boolean;
        FFullScreen              : Boolean;
        Fm_hwnd_rt_windowed      : HWND;
        Fm_hwnd_rt_fullscreen    : HWND;
        // DXGI swap chain (currently active, points to either the windowed or full-screen swap chain - no reference count held)
        FDXGISwapChain           : IDXGISwapChain1;
        FDXGISwapChainWindowed   : IDXGISwapChain1;
        FDXGISwapChainFullScreen : IDXGISwapChain1;
        // Direct2D target rendering bitmap
        // (linked to DXGI back buffer which is linked to Direct3D pipeline)
        FDirect2DBackBuffer      : ID2D1Bitmap1;
        // Scaled/stretched size of render target as shown on the user's display
        FRenderTargetW           : Integer;
        FRenderTargetH           : Integer;
        FD2DObjects              : TList<TDirect2D_1GraphicsObject>;
        FDrawTextOption          : D2D1_DRAW_TEXT_OPTIONS;
        FPen                     : TDirect2D_1Pen;
        FPenPos                  : TPoint;
        FFont                    : TDirect2D_1Font;
        FBrush                   : TDirect2D_1Brush;
        FBackgroundColor         : TColor;
        FOnReinitResources       : TNotifyEvent;
        procedure InitResources;
        function  CreateDeviceIndependentResources() : HRESULT;
        function  CreateDeviceResources() : HRESULT;
        function  CreateDirect3DResources() : HRESULT;
    protected
        class constructor Create;

        function  GetBrush: TDirect2D_1Brush;
        function  GetFont: TDirect2D_1Font;
        function  GetPen: TDirect2D_1Pen;
        function  GetPenPos: TPoint; override;
        function  GetRenderTarget : ID2D1DeviceContext;
        procedure SetBrush(const Value: TDirect2D_1Brush);
        procedure SetFont(const Value: TDirect2D_1Font);
        procedure SetPen(const Value: TDirect2D_1Pen);
        procedure SetPenPos(Value: TPoint); override;
        // Unimplemented Getters/Setters for Direct2D
        function  GetCanvasOrientation: TCanvasOrientation; override;
        function  GetClipRect: TRect; override;
        function  GetPixel(X, Y: Integer): TColor; override;
        procedure SetPixel(X, Y: Integer; Value: TColor); override;

        procedure Notification(AD2DObject : TDirect2D_1GraphicsObject;
                               Operation  : TOperation);
        procedure RequiredState(ReqState: TCanvasState); override;
        procedure CreateRenderTarget;
    public
        class function Supported: Boolean;
        constructor Create(Handle : HWND);
        destructor  Destroy;  override;

        function CreateBrush(Color: TColor): ID2D1Brush; overload;
        function CreateBrush(Bitmap: TBitmap): ID2D1Brush; overload;
        function CreateBrush(Colors     : array of TColor;
                             StartPoint : TD2D1Point2F;
                             EndPoint   : TD2D1Point2F): ID2D1Brush; overload;
        function CreateBrush(Colors     : array of TColor;
                             Center     : TD2D1Point2F;
                             Offset     : TD2D1Point2F;
                             RadiusX    : Single;
                             RadiusY    : Single): ID2D1Brush; overload;

        function CreateBitmap(Bitmap: TBitmap): ID2D1Bitmap;

        procedure BeginDraw();
        function  EndDraw(DontPresentFrame : Boolean = FALSE) : HRESULT;
        procedure PresentFrame(SyncInterval  : UINT = 0;
                               PresentFlags  : UINT = 0;
                               PresentParams : PDXGIPresentParameters = nil);

        procedure DrawEllipse(const Ellipse: TD2D1Ellipse);
        procedure DrawGeometry(Geometry: ID2D1Geometry);
        procedure DrawLine(Point0, Point1: TD2DPoint2f);
        procedure DrawRectangle(const Rect: TD2D1RectF);
        procedure DrawRoundedRectangle(const RoundedRect: TD2D1RoundedRect);

        procedure FillEllipse(const Ellipse: TD2D1Ellipse);
        procedure FillGeometry(Geometry: ID2D1Geometry);
        procedure FillRectangle(const Rect: TD2D1RectF);
        procedure FillRoundedRectangle(const roundedRect: TD2D1RoundedRect);

        // Override common functions from TCustomCanvas
        procedure Arc(X1: Integer; Y1: Integer;
                      X2: Integer; Y2: Integer;
                      X3: Integer; Y3: Integer;
                      X4: Integer; Y4: Integer); override;
        procedure ArcTo(X1: Integer; Y1: Integer;
                        X2: Integer; Y2: Integer;
                        X3: Integer; Y3: Integer;
                        X4: Integer; Y4: Integer); override;
        procedure AngleArc(X, Y       : Integer;
                           Radius     : Cardinal;
                           StartAngle : Single;
                           SweepAngle : Single); override;
        procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); override;
        procedure FillRect(const Rect: TRect); override;
        procedure FrameRect(const Rect: TRect); override;
        procedure Ellipse(X1, Y1, X2, Y2: Integer); override;
        procedure Rectangle(X1, Y1, X2, Y2: Integer); override;
        procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer); override;
        procedure Draw(X, Y: Integer; Graphic: TGraphic); overload; override;
        procedure Draw(X, Y: Integer; Graphic: TGraphic; Opacity: Byte); overload; override;
        procedure StretchDraw(const Rect: TRect; Graphic: TGraphic); overload; override;
        procedure StretchDraw(const Rect: TRect; Graphic: TGraphic; Opacity: Byte); reintroduce; overload;
        procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); override;
        procedure PolyBezier(const Points: array of TPoint); override;
        procedure Polygon(const Points: array of TPoint); override;
        procedure Polyline(const Points: array of TPoint); override;

        procedure LineTo(X, Y: Integer); override;
        procedure MoveTo(X, Y: Integer); override;
        procedure PolyBezierTo(const Points: array of TPoint); override;
        procedure Refresh; override;

        function  TextExtent(const Text: String): TSize; override;
        procedure TextOut(X, Y: Integer; const Text: String); override;
        procedure TextRect(var Rect: TRect; var Text: String; TextFormat: TTextFormat = []); overload; override;
        procedure TextRect(Rect: TRect; X, Y: Integer; const Text: String); overload; override;

        // Operations from TCustomCanvas that are invalid on a TDirect2DCanvas
        procedure BrushCopy(const Dest: TRect; Bitmap: TBitmap;
                            const Source: TRect; Color: TColor); override;
        procedure DrawFocusRect(const Rect: TRect); override;  // Doesn't have a concept of XOR brushes/pens
        procedure FloodFill(X, Y: Integer; Color: TColor;
                            FillStyle: TFillStyle); override;

        // Draw background with BackgroundColor
        procedure EraseBackground; overload;
        procedure EraseBackground(Color : TColor); overload;

        // Direct2D factory access
        property Direct2D                : ID2D1Factory1
                                           read  FDirect2D
                                           write FDirect2D;
        // Direct3D device
        property Direct3D                : ID3D11Device1
                                           read  FDirect3D
                                           write FDirect3D;
        // The render target (window surface) to access features of Direct2D.
        // In Direct2D 1.1 and later it is a ID2D1DeviceContext
        property RenderTarget            : ID2D1DeviceContext
                                           read  GetRenderTarget;
        // Direct3D device context
        property Screen3D                : ID3D11DeviceContext1
                                           read  FScreen3D
                                           write FScreen3D;
        // Direct2D device
        property Direct2DDevice          : ID2D1Device
                                           read  FDirect2DDevice
                                           write FDirect2DDevice;
        // Direct2D target rendering bitmap
        // (linked to DXGI back buffer which is linked to Direct3D pipeline)
        property Direct2DBackBuffer      : ID2D1Bitmap1
                                           read  FDirect2DBackBuffer
                                           write FDirect2DBackBuffer;
        property Enable3D                : Boolean
                                           read  FEnable3D
                                           write FEnable3D;
        property EnableMSAA              : Boolean
                                           read  FEnableMSAA
                                           write FEnableMSAA;
        property EnableFullScreen        : Boolean
                                           read  FEnableFullScreen
                                           write FEnableFullScreen;
        property EnableModeSwitch        : Boolean
                                           read  FEnableModeSwitch
                                           write FEnableModeSwitch;
        property FullScreen              : Boolean
                                           read  FFullScreen
                                           write FFullScreen;
        property MinFeatureLevel         : D3D_FEATURE_LEVEL
                                           read  FMinFeatureLevel
                                           write FMinFeatureLevel;
        property m_hwnd_rt_windowed      : HWND
                                           read  Fm_hwnd_rt_windowed
                                           write Fm_hwnd_rt_windowed;
        property m_hwnd_rt_fullscreen    : HWND
                                           read  Fm_hwnd_rt_fullscreen
                                           write Fm_hwnd_rt_fullscreen;
        // DXGI swap chain (currently active, points to either the windowed or full-screen swap chain - no reference count held)
        property DXGISwapChain           : IDXGISwapChain1
                                           read  FDXGISwapChain
                                           write FDXGISwapChain;
        property DXGISwapChainWindowed   : IDXGISwapChain1
                                           read  FDXGISwapChainWindowed
                                           write FDXGISwapChainWindowed;
        property DXGISwapChainFullScreen : IDXGISwapChain1
                                           read  FDXGISwapChainFullScreen
                                           write FDXGISwapChainFullScreen;
        // Scaled/stretched size of render target as shown on the user's display
        property RenderTargetW           : Integer
                                           read  FRenderTargetW
                                           write FRenderTargetW;
        property RenderTargetH           : Integer
                                           read  FRenderTargetH
                                           write FRenderTargetH;
        property BackgroundColor         : TColor
                                           read  FBackgroundColor
                                           write FBackgroundColor;
        property DrawTextOption          : D2D1_DRAW_TEXT_OPTIONS
                                           read  FDrawTextOption
                                           write FDrawTextOption;
        property Brush                   : TDirect2D_1Brush
                                           read  GetBrush
                                           write SetBrush;
        property Pen                     : TDirect2D_1Pen
                                           read  GetPen
                                           write SetPen;
        property Font                    : TDirect2D_1Font
                                           read  GetFont
                                           write SetFont;
        // Event to recreate resources whic depends on the RenderTarget
        property OnReinitResources       : TNotifyEvent
                                           read  FOnReinitResources
                                           write FOnReinitResources;
    end;

    // Create an alias type for ease of porting old code to this version.
    TDirect2DCanvas         = TDirect2D_1Canvas;
    TDirect2DBrush          = TDirect2D_1Brush;
    TDirect2DPen            = TDirect2D_1Pen;
    TDirect2DFont           = TDirect2D_1Font;
    TDirect2DGraphicsObject = TDirect2D_1GraphicsObject;

function D2DFactory(
    FactoryType    : TD2D1FactoryType    = D2D1_FACTORY_TYPE_SINGLE_THREADED;
    FactoryOptions : PD2D1FactoryOptions = nil): ID2D1Factory1;
function DWriteFactory(
    FacType: TDWriteFactoryType = DWRITE_FACTORY_TYPE_SHARED): IDWriteFactory1;

function D2D1ColorF(const AColor: TColor; Alpha: Single): TD2D1ColorF; overload;
function D2D1ColorF(const AColor: TColor): TD2D1ColorF; overload;
function D3DColorF(const AColor: TColor): D3DCOLORVALUE; overload;

implementation

const
    ErrorCaption = 'TDirect2D_1Canvas error';
var
    vSyncClamp              : Integer = 0;
    FpDefaultDrawTextOption : PD2D1DrawTextOptions = nil;

// Helper function for Arc, Pie, Chord
function PointsToArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer;
  out Center, A, B: TD2D1Point2F): TD2D1ArcSegment; forward;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TDirect2D_1GraphicsObject }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TDirect2D_1GraphicsObject.Create(AOwner: TDirect2D_1Canvas);
begin
    if AOwner = nil then
        raise EDirect2D_1Exception.CreateFmt(SNoOwner, [Self.ClassName]);
    inherited Create;
    FOwner := AOwner;
    FOwner.Notification(Self, opInsert);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TDirect2D_1GraphicsObject.Destroy;
begin
    FOwner.Notification(Self, opRemove);
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1GraphicsObject.Changed;
begin
    if Assigned(FOnChange) then
        FOnChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1GraphicsObject.ReleaseHandle;
begin
    // Intentionally left blank
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TD2DBrush }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Brush.BitmapChanged(Sender: TObject);
begin
    FBrush := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TDirect2D_1Brush.Create(Owner: TDirect2D_1Canvas);
begin
    inherited Create(Owner);
    FStyle := bsSolid;
    FColor := clWhite;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TDirect2D_1Brush.Destroy;
begin
    if Assigned(FBitmap) then
        FreeAndNil(FBitmap);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Brush.Assign(Source: TPersistent);
begin
    if Source is TBrush then begin
        FBrush := nil;

        if TBrush(Source).Bitmap <> nil then begin
            if Assigned(FBitmap) then
                FreeAndNil(FBitmap);
            FBitmap := TBitmap.Create;
            FBitmap.Assign(TBrush(Source).Bitmap);
        end;

        Color := TBrush(Source).Color;
        if FStyle in [bsSolid, bsClear] then
          FStyle := TBrush(Source).Style;
    end
    else if Source is TDirect2D_1Brush then begin
        if FOwner <> TDirect2D_1Brush(Source).FOwner then
            raise EDirect2D_1Exception.Create(SRequireSameOwner);

        if TDirect2D_1Brush(Source).FBitmap <> nil then begin
            if Assigned(FBitmap) then
                FreeAndNil(FBitmap);
            FBitmap := TBitmap.Create;
            FBitmap.Assign(TDirect2D_1Brush(Source).FBitmap);
            FBitmap.OnChange := BitmapChanged;
        end;

        FColor := TDirect2D_1Brush(Source).FColor;
        FStyle := TDirect2D_1Brush(Source).FStyle;
        FBrush := TDirect2D_1Brush(Source).FBrush;
    end
    else
        inherited Assign(Source);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Brush.GetHandle: ID2D1Brush;
begin
    if FBrush = nil then begin
        if FBitmap <> nil then
            FBrush := FOwner.CreateBrush(FBitmap)
        else
            FBrush := FOwner.CreateBrush(FColor);
    end;
    Result := FBrush;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Brush.GetBitmap: TBitmap;
begin
    Result := FBitmap;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Brush.GetColor: TColor;
begin
    if not Assigned(FBrush) or Supports(FBrush, ID2D1SolidColorBrush) then
        Result := FColor
    else
        raise EDirect2D_1Exception.Create(SDirect2DInvalidSolidBrush);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Brush.GetStyle: TBrushStyle;
begin
    Result := FStyle;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Brush.SetHandle(const Value: ID2D1Brush);
var
    LBrush : ID2D1SolidColorBrush;
    LColor : TD2D1ColorF;
begin
    if Supports(Value, ID2D1SolidColorBrush, LBrush) then begin
        LBrush.GetColor(LColor);
        FColor := Trunc(LColor.r * $FF) +
                  (Trunc(LColor.g * $FF) shl 8) +
                  (Trunc(LColor.b * $FF) shl 16);
    end;
    FStyle := bsSolid;
    FBrush := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Brush.SetBitmap(const Value: TBitmap);
begin
    if Assigned(FBitmap) then
        FreeAndNil(FBitmap);
    FBrush := nil;
    FBitmap := TBitmap.Create;
    FBitmap.Assign(Value);
    FBitmap.OnChange := BitmapChanged;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Brush.SetColor(const Value: TColor);
var
    LBrush: ID2D1SolidColorBrush;
begin
    FColor := Value;
    if Supports(FBrush, ID2D1SolidColorBrush, LBrush) then begin
        LBrush.SetColor(D3DColorF(Value));
    end
    else
        FBrush := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Brush.SetStyle(const Value: TBrushStyle);
begin
    case Value of
    bsSolid..bsClear:
        FStyle := Value;
    bsHorizontal..bsDiagCross:
        raise EDirect2D_1Exception.Create(SDirect2DInvalidBrushStyle);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Brush.ReleaseHandle;
begin
    inherited ReleaseHandle;
    FBrush := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TD2DPen }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TDirect2D_1Pen.Create(Owner: TDirect2D_1Canvas);
begin
    inherited Create(Owner);
    FWidth       := 1;
    FBrush       := TDirect2D_1Brush.Create(Owner);
    FBrush.Color := clBlack;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TDirect2D_1Pen.Destroy;
begin
    FreeAndNil(FBrush);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Pen.Assign(Source: TPersistent);
begin
    if Source is TPen then begin
        FWidth := TPen(Source).Width;
        Color  := TPen(Source).Color;
        Style  := TPen(Source).Style;
    end
    else if Source is TDirect2D_1Pen then begin
        FBrush.Assign(TDirect2D_1Pen(Source).FBrush);
        FStyle       := TDirect2D_1Pen(Source).FStyle;
        FStrokeStyle := TDirect2D_1Pen(Source).FStrokeStyle;
        FWidth       := TDirect2D_1Pen(Source).FWidth;
    end
    else
        inherited Assign(Source);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Pen.GetBrush: TDirect2D_1Brush;
begin
    Result := FBrush;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Pen.GetColor: TColor;
begin
    Result := Brush.Color;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Pen.GetStyle: TPenStyle;
begin
    Result := FStyle;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Pen.SetBrush(const Value: TDirect2D_1Brush);
begin
    if Assigned(Value) and (Value.FOwner = FOwner) then
        FBrush.Assign(Value)
    else
        raise EDirect2D_1Exception.Create(SRequireSameOwner);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Pen.SetColor(Color: TColor);
begin
    FBrush.Color := Color;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Pen.SetStyle(const Value: TPenStyle);
begin
    FStrokeStyle := nil;
    FStyle       := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Pen.ReleaseHandle;
begin
    inherited ReleaseHandle;
    FBrush.ReleaseHandle;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TD2DFont }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TDirect2D_1Font.Create(AOwner: TDirect2D_1Canvas);
var
    DpiX, DpiY: Single;
begin
    inherited Create(AOwner);
    FBrush         := TDirect2D_1Brush.Create(AOwner);
    FBrush.FColor  := clWindowText;

    FName          := TFontName(DefFontData.Name);
    FHeight        := DefFontData.Height;
    FOrientation   := DefFontData.Orientation;
    FPitch         := DefFontData.Pitch;
    FStyle         := DefFontData.Style;
    AOwner.RenderTarget.GetDpi(DpiX, DpiY);
    FPixelsPerInch := Trunc(DpiX);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TDirect2D_1Font.Destroy;
begin
    FBrush.Free;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Font.Assign(Source: TPersistent);
begin
    if Source is TFont then begin
        Name        := TFont(Source).Name;
        Height      := TFont(Source).Height;
        Orientation := TFont(Source).Orientation;
        Pitch       := TFont(Source).Pitch;
        Color       := TFont(Source).Color;
        Style       := TFont(Source).Style;
    end
    else if Source is TDirect2D_1Font then begin
        if FOwner <> TDirect2D_1Font(Source).FOwner then
            raise EDirect2D_1Exception.Create(SRequireSameOwner);

        FBrush.Assign(TDirect2D_1Font(Source).FBrush);

        FHeight        := TDirect2D_1Font(Source).FHeight;
        FName          := TDirect2D_1Font(Source).FName;
        FOrientation   := TDirect2D_1Font(Source).FOrientation;
        FPitch         := TDirect2D_1Font(Source).FPitch;
        FStyle         := TDirect2D_1Font(Source).FStyle;
        FPixelsPerInch := TDirect2D_1Font(Source).FPixelsPerInch;
    end
    else
        inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Font.GetBrush: TDirect2D_1Brush;
begin
    Result := FBrush;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Font.GetColor: TColor;
begin
    Result := Brush.Color;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Font.GetHandle: IDWriteTextFormat;
var
    LWeight : TDWriteFontWeight;
    LStyle  : TDWriteFontStyle;
begin
    if (fsItalic in FStyle) then
        LStyle := DWRITE_FONT_STYLE_ITALIC
    else
        LStyle := DWRITE_FONT_STYLE_NORMAL;
    if (fsBold in FStyle) then
        LWeight := DWRITE_FONT_WEIGHT_BOLD
    else
        LWeight := DWRITE_FONT_WEIGHT_NORMAL;

    DWriteFactory.CreateTextFormat(PWideChar(FName),
                                   nil,
                                   LWeight,
                                   LStyle,
                                   DWRITE_FONT_STRETCH_NORMAL,
                                   MulDiv(Size,
                                   FPixelsPerInch,
                                   72),
                                   'en-us',
                                   Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Font.GetHeight: Integer;
begin
    Result := FHeight;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Font.GetSize: Integer;
begin
    Result := -MulDiv(FHeight, 72, FPixelsPerInch);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Font.SetBrush(const Value: TDirect2D_1Brush);
begin
    FBrush.Assign(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Font.SetColor(const Value: TColor);
begin
    FBrush.Color := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Font.SetHeight(const Value: Integer);
begin
    FHeight := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Font.SetSize(const Value: Integer);
begin
    FHeight := -MulDiv(Value, FPixelsPerInch, 72);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Font.ReleaseHandle;
begin
    inherited ReleaseHandle;
    FBrush.ReleaseHandle;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TDirect2D_1Canvas }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Helper function for Arc, Pie, Chord
function PointsToArc(
    X1, Y1, X2, Y2, X3, Y3, X4, Y4 : Integer;
    out Center, A, B               : TD2D1Point2F): TD2D1ArcSegment;
var
    TL, BR             : TD2D1Point2F;
    A1, A2             : Single;
    DAngle             : Single;
    AA, BB             : Single;
    Slope              : Single;
    SavedExceptionMask : TArithmeticExceptionMask;
begin
    SavedExceptionMask := GetExceptionMask;
    SetExceptionMask(exAllArithmeticExceptions);
    try
        TL.x   := Min(X1, X2);
        TL.y   := Min(Y1, Y2);
        BR.x   := Max(X1, X2);
        BR.y   := Max(Y1, Y2);
        Center := D2D1PointF((BR.X + TL.X) / 2, (BR.Y + TL.Y) / 2);

        A1     := ArcTan2(Center.Y - Y3 - 0.5, Center.X - X3 - 0.5);
        A2     := ArcTan2(Center.Y - Y4 - 0.5, Center.X - X4 - 0.5);
        A1     := Pi - A1;
        A2     := Pi - A2;
        DAngle := A2-A1;
        if (DAngle < 0) and (-DAngle > 0.001) then
            DAngle := DAngle + 2 * PI;
        Result.rotationAngle := 0;
        if DAngle > PI then
            Result.arcSize := D2D1_ARC_SIZE_LARGE
        else
            Result.arcSize := D2D1_ARC_SIZE_SMALL;
        Result.sweepDirection := D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE;
        Result.size := D2D1SizeF((BR.X - TL.X -1) / 2, (BR.Y - TL.Y - 1) / 2);

        AA    := Result.size.width  * Result.size.width;
        BB    := Result.size.height * Result.size.height;
        Slope := ((Y3 - Center.y) * (Y3 - Center.y))/
                 ((X3 - Center.x) * (X3 - Center.x));
        A.x   := sqrt(AA * BB / (BB + AA * Slope));
        A.y   := sqrt(BB * (1 - A.x * A.x / AA));

        if (A1 < Pi/2) or (A1 > 3*PI/2) then
            A.x := Center.x + A.x
        else
            A.x := Center.x - A.x;

        if A1 > PI then
            A.y := Center.y + A.y
        else
            A.y := Center.y - A.y;
        Slope := ((Y4 - Center.y) * (Y4 - Center.y))/
                 ((X4 - Center.x) * (X4 - Center.x));
        B.x   := sqrt(AA * BB /(BB + AA * Slope));
        B.y   := sqrt(BB * (1 - B.x * B.x / AA));

        if (A2 < Pi/2) or (A2 > 3*PI/2) then
            B.x := Center.x + B.x
        else
            B.x := Center.x - B.x;

        if A2 > PI then
            B.y := Center.y + B.y
        else
            B.y := Center.y - B.y;

        Result.point := B;
    finally
        SetExceptionMask(SavedExceptionMask);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
type
    ID2D1Factory5 = interface // from d2d1_3.h
        ['{c4349994-838e-4b0f-8cab-44997d9eeacc}']
    end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class constructor TDirect2D_1Canvas.Create;
begin
    FDefaultDrawTextOption  := D2D1_DRAW_TEXT_OPTIONS_NONE;
    FpDefaultDrawTextOption := @FDefaultDrawTextOption;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TDirect2D_1Canvas.Create(Handle : HWND);
begin
    inherited Create;
    Fm_hwnd_rt_windowed      := Handle;
    FMinFeatureLevel         := D3D_FEATURE_LEVEL_10_1;
    FEnable3D                := FALSE;
    FEnableMSAA              := FALSE;
    FEnableFullScreen        := FALSE;
    FEnableModeSwitch        := FALSE;
    FBackgroundColor         := clDkGray;
    FPenPos                  := Point(0,0);
    FDrawTextOption          := FDefaultDrawTextOption;
    InitResources;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TDirect2D_1Canvas.Destroy;
begin
    FreeAndNil(FPen);
    FreeAndNil(FFont);
    FreeAndNil(FBrush);
    FreeAndNil(FD2DObjects);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.InitResources;
var
    HR       : HRESULT;
    GrObject : TDirect2D_1GraphicsObject;
begin
    HR := CreateDeviceIndependentResources;
    if HR <> 0 then begin
        MessageBox(0,
                   'There was a problem initializing device independent resources',
                   ErrorCaption,
                   MB_OK);
        Exit;
    end;

    // Initialize application graphics
    // This must come before we display the window otherwise
    // CreateDeviceResources() could be called from WndProc() before
    // we get chance to finish here!
    HR := CreateDeviceResources();
    if HR <> 0 then begin
        MessageBox(0,
                   'There was a problem initializing device''s resources',
                   ErrorCaption,
                   MB_OK);
        Exit;
    end;

    if not Assigned(FD2DObjects) then begin
        FD2DObjects     := TList<TDirect2D_1GraphicsObject>.Create();
        FFont           := TDirect2D_1Font.Create(Self);
        FBrush          := TDirect2D_1Brush.Create(Self);
        FPen            := TDirect2D_1Pen.Create(Self);
    end
    else begin
        for GrObject in FD2DObjects do begin
            if GrObject is TDirect2D_1Brush then begin
                GrObject.ReleaseHandle;
                TDirect2D_1Brush(GrObject).Handle;
            end;
        end;
    end;

    if Assigned(FOnReinitResources) then
        FOnReinitResources(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Create Direct2D resources
// Device-independent resources last for the lifetime of the application
// Device-dependent resources are associated with a particular rendering device and will cease to function if that device is removed
function TDirect2D_1Canvas.CreateDeviceIndependentResources() : HRESULT;
var
    CreationFlags        : UINT;
    Device               : ID3D11Device;
    Context              : ID3D11DeviceContext;
    ReturnedFeatureLevel : D3D_FEATURE_LEVEL;
    MSAAQuality          : UINT;
    DXGIDevice           : IDXGIDevice;
    DXGIAdapter          : IDXGIAdapter;
    DXGIFactory          : IDXGIFactory2;
    SwapChainDesc        : DXGI_SWAP_CHAIN_DESC1;
    MainOutput           : IDXGIOutput;
    Desc                 : DXGI_OUTPUT_DESC;
const
    // Set feature levels supported by our application
    FeatureLevels : array [0..6] of D3D_FEATURE_LEVEL = (
        D3D_FEATURE_LEVEL_11_1,
        D3D_FEATURE_LEVEL_11_0,
        D3D_FEATURE_LEVEL_10_1,
        D3D_FEATURE_LEVEL_10_0,
        D3D_FEATURE_LEVEL_9_3,
        D3D_FEATURE_LEVEL_9_2,
        D3D_FEATURE_LEVEL_9_1);
begin
    // Create a Direct2D factory
    if Direct2D = nil then
        Direct2D := D2DFactory;

    // Create a Windows Imaging factory
//    if (!ImageFactory)
//        HRReturnOnFail(
//            CoCreateInstance(CLSID_WICImagingFactory, NULL, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&ImageFactory)),
//            L"There was a problem setting up the WIC Imaging factory");

    // This flag adds support for surfaces with a different color channel ordering
    // than the API default. It is required for compatibility with Direct2D.
    CreationFlags := D3D11_CREATE_DEVICE_BGRA_SUPPORT;

    // Create Direct3D Device and Context
    Result := D3D11CreateDevice(nil,
                                D3D_DRIVER_TYPE_HARDWARE,
                                0,
                                CreationFlags,
                                @featureLevels[0],
                                Length(featureLevels),
                                D3D11_SDK_VERSION,
                                Device,
                                ReturnedFeatureLevel,
                                Context);
    if Result <> 0 then begin
        MessageBox(0,
                   'Could not create the Direct3D device',
                   ErrorCaption, MB_OK);
        Exit;
   end;

    // Direct3D-specific: Check for minimum feature level support
    if Enable3D then begin
        if ReturnedFeatureLevel < minFeatureLevel then begin
            Context := nil;
            Device  := nil;
            MessageBox(0,
                'Your graphics card does not support the minimum requirements for this application',
                ErrorCaption, MB_OK);
            Result := E_FAIL;
            Exit;
        end;
    end;

    // Get the underlying versioned Device and Context interfaces
    Device.QueryInterface(IID_ID3D11Device1, FDirect3D);
    Context.QueryInterface(IID_ID3D11DeviceContext1, FScreen3D);

    // Clean up temporary references
    Context := nil;
    Device  := nil;

    // Get MSAA quality
    if Enable3D then begin
        Result := Direct3D.CheckMultisampleQualityLevels(
                              DXGI_FORMAT_B8G8R8A8_UNORM,
                              4,
                              MSAAQuality);
        if Result <> 0 then begin
            MessageBox(0,
                'Could not query graphics card for multi-sampling/anti-aliasing support',
                ErrorCaption, MB_OK);
            Exit;
        end;

        if MSAAQuality <= 0 then begin
            MessageBox(0,
                'Your graphics card does not support 4x MSAA',
                ErrorCaption, MB_OK);
            Result := E_FAIL;
            Exit;
        end;
    end;

    // Get the underlying DXGI Device of the Direct3D Device
    Result := Direct3D.QueryInterface(IID_IDXGIDevice, DXGIDevice);
    if Result <> 0 then begin
        MessageBox(0,
                   'Could not create the DXGI device',
                   ErrorCaption, MB_OK);
        Exit;
    end;

    // Create 2D Device object
    Result := Direct2D.CreateDevice(DXGIDevice, FDirect2DDevice);
    if Result <> 0 then begin
        MessageBox(0,
                   'Could not create the Direct2D device',
                   ErrorCaption, MB_OK);
        Exit;
    end;

    // Get the GPU we are using
    Result := DXGIDevice.GetAdapter(DXGIAdapter);
    if Result <> 0 then begin
        MessageBox(0,
                   'Could not get DXGI adapter',
                   ErrorCaption, MB_OK);
        Exit;
    end;

    // Get the DXGI factory instance
    Result := DXGIAdapter.GetParent(IID_IDXGIFactory2, DXGIFactory);
    if Result <> 0 then begin
        MessageBox(0,
                   'Could not get DXGI factory instance',
                   ErrorCaption, MB_OK);
        Exit;
    end;

    // Describe Windows 7-compatible Windowed swap chain (DXGI_SCALING_NONE is not allowed in Windows 7)
    ZeroMemory(@SwapChainDesc, SizeOf(SwapChainDesc));
    SwapChainDesc.Width  := 0;
    SwapChainDesc.Height := 0;
    SwapChainDesc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
    SwapChainDesc.Stereo := FALSE;

    if Enable3D and EnableMSAA then begin
        SwapChainDesc.SampleDesc.Count   := 4;
        SwapChainDesc.SampleDesc.Quality := MSAAQuality - 1;
    end
    else begin
        SwapChainDesc.SampleDesc.Count   := 1;
        SwapChainDesc.SampleDesc.Quality := 0;
    end;

    SwapChainDesc.BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
    SwapChainDesc.BufferCount := 2;
    SwapChainDesc.Scaling     := DXGI_SCALING_STRETCH;
    SwapChainDesc.SwapEffect  := DXGI_SWAP_EFFECT_DISCARD;
    if EnableModeSwitch then
        SwapChainDesc.Flags := Ord(DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH);

    // Create DXGI swap chain targeting a window handle (the only Windows 7-compatible option)
    Result := DXGIFactory.CreateSwapChainForHwnd(
                    Direct3D,
                    m_hwnd_rt_windowed,
                    @SwapChainDesc,
                    nil,
                    nil,
                    FDXGISwapChainWindowed);
    if Result <> 0 then begin
        MessageBox(0,
                   'Could not create windowed swap chain',
                   ErrorCaption, MB_OK);
        Exit;
    end;

    Result := DXGIFactory.MakeWindowAssociation(m_hwnd_rt_windowed,
                                                DXGI_MWA_NO_ALT_ENTER);
    if Result <> 0 then begin
        MessageBox(0,
                   'Could not MakeWindowAssociation',
                   ErrorCaption, MB_OK);
        Exit;
    end;

    // Create DXGI swap chain for full-screen mode if needed
    if EnableFullScreen then begin
        Result := DXGIFactory.CreateSwapChainForHwnd(
                      Direct3D,
                      m_hwnd_rt_fullscreen,
                      @SwapChainDesc,
                      nil,
                      nil,
                      FDXGISwapChainFullScreen);
        if Result <> 0 then begin
            MessageBox(0,
                       'Could not create full-screen swap chain',
                       ErrorCaption, MB_OK);
            Exit;
        end;

        // Disable Alt+Enter on this window - we'll deal with it ourselves in the window message handler
        // If we don't use NO_WINDOW_CHANGES here, we don't receive WM_MENUCHAR messages in full-screen mode
        DXGIFactory.MakeWindowAssociation(m_hwnd_rt_fullscreen,
                                          DXGI_MWA_NO_WINDOW_CHANGES);

        // Make full-screen if we are starting in full-screen mode
        // NOTE: This will change the display resolution
        if FullScreen then begin
            // The input focus will screw up if we don't include this line
            ShowWindow(m_hwnd_rt_fullscreen, SW_SHOWNORMAL);
            DXGISwapChainFullScreen.SetFullscreenState(TRUE, nil);
            // Get monitor
            DXGISwapChainFullScreen.GetContainingOutput(MainOutput);
            // Get desktop resolution of monitor
            MainOutput.GetDesc(Desc);
            // Set informational variables
            RenderTargetW := Desc.DesktopCoordinates.right - Desc.DesktopCoordinates.left;
            RenderTargetH := Desc.DesktopCoordinates.bottom - Desc.DesktopCoordinates.top;

            // Release monitor
            MainOutput := nil;
        end;
    end;

    // Active swap chain depends on whether we start windowed or full-screen
    if FullScreen and EnableFullScreen then
        DXGISwapChain := DXGISwapChainFullScreen
    else
        DXGISwapChain := DXGISwapChainWindowed;

    // Clean up
    DXGIFactory := nil;
    DXGIAdapter := nil;
    DXGIDevice  := nil;

    Result := S_OK;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Create a render target and device-specific resources
function TDirect2D_1Canvas.CreateDeviceResources() : HRESULT;
var
    DXGIBackBuffer   : IDXGISurface;
    DPIX, DPIY       : Single;
    BitmapProperties : D2D1_BITMAP_PROPERTIES1;
begin
    // Window-size-dependent resources
    Direct2DBackBuffer := nil;
    FRenderTarget      := nil;

    // Create 2D device context (replaces ID2D1RenderTarget sub-classes)
    Result := Direct2DDevice.CreateDeviceContext(
                                 D2D1_DEVICE_CONTEXT_OPTIONS_NONE,
                                 FRenderTarget);
    if Result <> 0 then begin
        MessageBox(0,
                   'Could not CreateDeviceContext',
                   ErrorCaption, MB_OK);
        Exit;
    end;

    // Get the back buffer as an IDXGISurface (Direct2D doesn't accept an ID3D11Texture2D directly as a render target)
    Result := DXGISwapChain.GetBuffer(0, IID_IDXGISurface, DXGIBackBuffer);
    if Result <> 0 then begin
        MessageBox(0,
                   'Could not DXGISwapChain.GetBuffer',
                   ErrorCaption, MB_OK);
        Exit;
    end;

    var Desc : DXGI_SURFACE_DESC;
    Result := DXGIBackBuffer.GetDesc(Desc);
    if Result <> 0 then begin
        MessageBox(0,
                   'dxgiBackBuffer.GetDesc failed',
                   ErrorCaption, MB_OK);
        Exit;
    end;

    // Get screen DPI
    Direct2D.GetDesktopDpi(DPIX, DPIY);

    // Create a Direct2D surface (bitmap) linked to the Direct3D texture
    // back buffer via the DXGI back buffer
    ZeroMemory(@BitmapProperties, SizeOf(BitmapProperties));
    BitmapProperties.bitmapOptions         :=
                            Ord(D2D1_BITMAP_OPTIONS_TARGET) +
                            Ord(D2D1_BITMAP_OPTIONS_CANNOT_DRAW);
    BitmapProperties.pixelFormat.format    := DXGI_FORMAT_B8G8R8A8_UNORM;
    BitmapProperties.pixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;
    BitmapProperties.DPIX                  := DPIX;
    BitmapProperties.DPIY                  := DPIY;

    Result := FRenderTarget.CreateBitmapFromDxgiSurface(DXGIBackBuffer,
                                                        @BitmapProperties,
                                                        FDirect2DBackBuffer);
    if Result <> 0 then begin
        MessageBox(0,
                   'Could not CreateBitmapFromDxgiSurface',
                   ErrorCaption, MB_OK);
        Exit;
    end;

    // Set surface as render target in Direct2D device context
    FRenderTarget.SetTarget(ID2D1Image(Direct2DBackBuffer));

    // Sanity check for SetTarget which do not return a status
    var TmpImage : ID2D1Image;
    FRenderTarget.GetTarget(TmpImage);
    if UIntPtr(TmpImage) <> UIntPtr(Direct2DBackBuffer) then begin
        MessageBox(0,
                   'FRenderTarget.SetTarget(Direct2DBackBuffer) failed',
                   ErrorCaption, MB_OK);
        Exit;
    end;

    // Direct3D-specific initialization
    if Enable3D then begin
        Result := CreateDirect3DResources();
        if Result <> 0 then
            Exit;
    end;

    // Clean up
    DXGIBackBuffer := nil;

    Result := S_OK;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Create Direct3D-specific device resources
function TDirect2D_1Canvas.CreateDirect3DResources() : HRESULT;
begin
    if Enable3D then
        raise Exception.Create('CreateDirect3DResources not implemented');
    Result := S_OK;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class function TDirect2D_1Canvas.Supported: Boolean;
var
    LModule : HMODULE;
    LProc   : Pointer;
begin
    if FSupported = TUncertainState.Maybe then begin
        LModule := LoadLibrary(d2d1lib);
        if LModule > 0 then begin
            LProc := GetProcAddress(LModule, 'D2D1CreateFactory'); // Do not localize
            if Assigned(LProc) then
                FSupported := TUncertainState.Yes
            else
                FSupported := TUncertainState.No;
            FreeLibrary(LModule);
        end
        else
            FSupported := TUncertainState.No;
    end;

    Result := FSupported = TUncertainState.Yes;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.CreateBrush(
    Colors     : array of TColor;
    StartPoint : TD2D1Point2F;
    EndPoint   : TD2D1Point2F): ID2D1Brush;
var
    GradientStops: array of TD2D1GradientStop;
    GradientStopCollection: ID2D1GradientStopCollection;
    I: Integer;
    LBrush: ID2D1LinearGradientBrush;
begin
    SetLength(GradientStops, Length(Colors));
    for I := 0 to Length(Colors) - 1 do begin
        GradientStops[I].color := D2D1ColorF(Colors[I]);
        GradientStops[I].position := I / (Length(Colors) - 1);
    end;

    RenderTarget.CreateGradientStopCollection(
                      @GradientStops[0],
                      Length(GradientStops),
                      D2D1_GAMMA_2_2,
                      D2D1_EXTEND_MODE_CLAMP,
                      GradientStopCollection);
    RenderTarget.CreateLinearGradientBrush(
                      D2D1LinearGradientBrushProperties(StartPoint, EndPoint),
                      nil,
                      GradientStopCollection,
                      LBrush);

    Result := LBrush;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.CreateBitmap(Bitmap: TBitmap): ID2D1Bitmap;
var
    BitmapInfo: TBitmapInfo;
    buf: array of Byte;
    BitmapProperties: TD2D1BitmapProperties;
    Hbmp: HBitmap;
begin
    FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
    BitmapInfo.bmiHeader.biSize := Sizeof(BitmapInfo.bmiHeader);
    BitmapInfo.bmiHeader.biHeight := -Bitmap.Height;
    BitmapInfo.bmiHeader.biWidth := Bitmap.Width;
    BitmapInfo.bmiHeader.biPlanes := 1;
    BitmapInfo.bmiHeader.biBitCount := 32;

    SetLength(buf, Bitmap.Height * Bitmap.Width * 4);
    // Forces evaluation of Bitmap.Handle before Bitmap.Canvas.Handle
    Hbmp := Bitmap.Handle;
    GetDIBits(Bitmap.Canvas.Handle, Hbmp, 0, Bitmap.Height, @buf[0], BitmapInfo, DIB_RGB_COLORS);

    BitmapProperties.dpiX := 0;
    BitmapProperties.dpiY := 0;
    BitmapProperties.pixelFormat.format := DXGI_FORMAT_B8G8R8A8_UNORM;
    if (Bitmap.PixelFormat <> pf32bit) or (Bitmap.AlphaFormat = afIgnored) then
    BitmapProperties.pixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE
    else
    BitmapProperties.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;


    RenderTarget.CreateBitmap(D2D1SizeU(Bitmap.Width, Bitmap.Height), @buf[0], 4*Bitmap.Width, BitmapProperties, Result)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.CreateBrush(
    Colors  : array of TColor;
    Center  : TD2D1Point2F;
    Offset  : TD2D1Point2F;
    RadiusX : Single;
    RadiusY : Single): ID2D1Brush;
var
    GradientStops          : array of TD2D1GradientStop;
    GradientStopCollection : ID2D1GradientStopCollection;
    I                      : Integer;
    LBrush                 : ID2D1RadialGradientBrush;
begin
    SetLength(GradientStops, Length(Colors));
    for I := 0 to Length(Colors) - 1 do begin
        GradientStops[I].color := D2D1ColorF(Colors[I]);
        GradientStops[I].position := I / (Length(Colors) - 1);
    end;

    RenderTarget.CreateGradientStopCollection(
                      @GradientStops[0],
                      Length(GradientStops),
                      D2D1_GAMMA_2_2,
                      D2D1_EXTEND_MODE_CLAMP,
                      GradientStopCollection);
    RenderTarget.CreateRadialGradientBrush(
                      D2D1RadialGradientBrushProperties(Center, Offset,
                                                        RadiusX, RadiusY),
                      nil,
                      GradientStopCollection,
                      LBrush);
    Result := LBrush;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.CreateBrush(Bitmap: TBitmap): ID2D1Brush;
var
    LBitmap         : ID2D1Bitmap;
    LBrush          : ID2D1BitmapBrush;
    BrushProperties : TD2D1BitmapBrushProperties;
begin
    BrushProperties.extendModeX       := D2D1_EXTEND_MODE_WRAP;
    BrushProperties.extendModeY       := D2D1_EXTEND_MODE_WRAP;
    BrushProperties.interpolationMode := D2D1_BITMAP_INTERPOLATION_MODE_NEAREST_NEIGHBOR;

    LBitmap := CreateBitmap(Bitmap);
    RenderTarget.CreateBitmapBrush(LBitmap, @BrushProperties, nil, LBrush);
    Result  := LBrush;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.CreateRenderTarget;
begin
    InitResources;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.BeginDraw;
begin
    FRenderTarget.BeginDraw;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.EndDraw(DontPresentFrame : Boolean = FALSE) : HRESULT;
begin
    Result := FRenderTarget.EndDraw;
    if DontPresentFrame = FALSE then
        PresentFrame;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.GetBrush: TDirect2D_1Brush;
begin
    Result := FBrush;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.GetFont: TDirect2D_1Font;
begin
    Result := FFont;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.GetPen: TDirect2D_1Pen;
begin
    Result := FPen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.GetPenPos: TPoint;
begin
    Result := FPenPos;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.GetRenderTarget: ID2D1DeviceContext;
begin
    if not Assigned(FRenderTarget) then
        InitResources;

    Result := FRenderTarget;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.SetBrush(const Value: TDirect2D_1Brush);
begin
    if Assigned(Value) and (Value.Owner = Self) then
        FBrush.Assign(Value)
    else
        raise EDirect2D_1Exception.CreateFmt(SDirect2DInvalidOwner,
                                           [Value.ClassName]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.SetFont(const Value: TDirect2D_1Font);
begin
    if Assigned(Value) and (Value.Owner = Self) then
        FFont.Assign(Value)
    else
        raise EDirect2D_1Exception.CreateFmt(SDirect2DInvalidOwner,
                                           [Value.ClassName]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.SetPen(const Value: TDirect2D_1Pen);
begin
    if Assigned(Value) and (Value.Owner = Self) then begin
        FreeAndNil(FPen);
        FPen := Value;
    end
    else
        raise EDirect2D_1Exception.CreateFmt(SDirect2DInvalidOwner,
                                           [Value.ClassName]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.SetPenPos(Value: TPoint);
begin
    FPenPos := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.Notification(
    AD2DObject : TDirect2D_1GraphicsObject;
    Operation  : TOperation);
begin
    if Operation = opInsert then
        FD2DObjects.Add(AD2DObject)
    else
        FD2DObjects.Remove(AD2DObject);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.RequiredState(ReqState: TCanvasState);
const
    Styles: array [psDash..psDashDotDot] of TD2D1DashStyle = (
                                                D2D1_DASH_STYLE_DASH,
                                                D2D1_DASH_STYLE_DOT,
                                                D2D1_DASH_STYLE_DASH_DOT,
                                                D2D1_DASH_STYLE_DASH_DOT_DOT);
begin
    if (csHandleValid in ReqState) and (FRenderTarget = nil) then
        CreateRenderTarget;
    if (csBrushValid in ReqState) and (FBrush.FBrush = nil) then
        FBrush.Handle;
    if (csFontValid in ReqState) and (FFont.FBrush.FBrush = nil) then
        FFont.FBrush.Handle;
    if (csPenValid in ReqState) then begin
        if (FPen.FBrush.FBrush = nil) then
            FPen.FBrush.Handle;
        if (not (FPen.FStyle in [psSolid,psClear, psInsideFrame])) and
           (FPen.FStrokeStyle = nil) then begin
            Direct2D.CreateStrokeStyle(D2D1StrokeStyleProperties(
                                          D2D1_CAP_STYLE_FLAT,
                                          D2D1_CAP_STYLE_FLAT,
                                          D2D1_CAP_STYLE_ROUND,
                                          D2D1_LINE_JOIN_ROUND,
                                          10,
                                          Styles[FPen.FStyle]),
                                       nil,
                                       0,
                                       FPen.FStrokeStyle);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.DrawEllipse(const Ellipse: TD2D1Ellipse);
begin
    RequiredState([csHandleValid, csPenValid]);
    RenderTarget.DrawEllipse(Ellipse,
                             FPen.FBrush.Handle,
                             FPen.FWidth,
                             FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.DrawGeometry(Geometry: ID2D1Geometry);
begin
    RequiredState([csHandleValid, csPenValid]);
    RenderTarget.DrawGeometry(Geometry,
                              FPen.FBrush.Handle,
                              FPen.FWidth,
                              FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.DrawLine(Point0, Point1: TD2DPoint2f);
begin
    RequiredState([csHandleValid, csPenValid]);
    RenderTarget.DrawLine(Point0,
                          Point1,
                          FPen.FBrush.Handle,
                          FPen.FWidth,
                          FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.DrawRectangle(const Rect: TD2D1RectF);
begin
//    RequiredState([csHandleValid, csPenValid]);
    RenderTarget.DrawRectangle(Rect,
                               FPen.FBrush.Handle,
                               FPen.FWidth,
                               FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.DrawRoundedRectangle(const RoundedRect: TD2D1RoundedRect);
begin
    RequiredState([csHandleValid, csPenValid]);
    RenderTarget.DrawRoundedRectangle(RoundedRect,
                                      FPen.FBrush.Handle,
                                      FPen.FWidth,
                                      FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.FillEllipse(const Ellipse: TD2D1Ellipse);
begin
    RequiredState([csHandleValid, csBrushValid]);
     RenderTarget.FillEllipse(Ellipse, FBrush.Handle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.FillGeometry(Geometry: ID2D1Geometry);
begin
    RequiredState([csHandleValid, csBrushValid]);
    RenderTarget.FillGeometry(Geometry, FBrush.Handle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.FillRectangle(const Rect: TD2D1RectF);
begin
    //RequiredState([csHandleValid, csBrushValid]);
    if FBrush.FStyle <> bsClear then
        RenderTarget.FillRectangle(Rect, FBrush.Handle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.FillRoundedRectangle(
    const roundedRect: TD2D1RoundedRect);
begin
    RequiredState([csHandleValid, csBrushValid]);
    RenderTarget.FillRoundedRectangle(roundedRect, FBrush.Handle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.Draw(
    X, Y    : Integer;
    Graphic : TGraphic);
begin
    StretchDraw(Rect(X, Y, X + Graphic.Width, Y + Graphic.Height),
                Graphic,
                255);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.Draw(
    X, Y    : Integer;
    Graphic : TGraphic;
    Opacity : Byte);
begin
    StretchDraw(Rect(X, Y, X + Graphic.Width, Y + Graphic.Height),
                Graphic,
                Opacity);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.StretchDraw(
    const Rect : TRect;
    Graphic    : TGraphic);
begin
    StretchDraw(Rect, Graphic, 255);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.StretchDraw(
    const Rect : TRect;
    Graphic    : TGraphic;
    Opacity    : Byte);
var
    D2DBitmap : ID2D1Bitmap;
    D2DRect   : TD2DRectF;
    Bitmap    : TBitmap;
begin
    Bitmap := TBitmap.Create;
    try
        Bitmap.Assign(Graphic);

        D2DBitmap := CreateBitmap(Bitmap);

        D2DRect.Left   := Rect.Left;
        D2DRect.Right  := Rect.Right;
        D2DRect.Top    := Rect.Top;
        D2DRect.Bottom := Rect.Bottom;
        RenderTarget.DrawBitmap(D2DBitmap, @D2DRect, Opacity/255);
    finally
        Bitmap.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
var
      A, B, Center : TD2D1Point2F;
      Geometry     : ID2D1PathGeometry;
      Sink         : ID2D1GeometrySink;
      ArcSegment   : TD2D1ArcSegment;
begin
    ArcSegment := PointsToArc(X1,Y1, X2,Y2, X3,Y3, X4,Y4, Center, A, B);
    Direct2D.CreatePathGeometry(Geometry);
    Geometry.Open(Sink);
    try
        Sink.BeginFigure(A, D2D1_FIGURE_BEGIN_FILLED);
        try
            Sink.AddArc(ArcSegment);
        finally
            Sink.EndFigure(D2D1_FIGURE_END_OPEN);
        end;
    finally
        Sink.Close;
    end;
    RequiredState([csHandleValid, csPenValid, csBrushValid]);
    RenderTarget.DrawGeometry(Geometry,
                              FPen.FBrush.Handle,
                              FPen.FWidth,
                              FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.ArcTo(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
var
    A,B, Center : TD2D1Point2F;
    Geometry    : ID2D1PathGeometry;
    Sink        : ID2D1GeometrySink;
    ArcSegment  : TD2D1ArcSegment;
begin
    ArcSegment := PointsToArc(X1,Y1, X2,Y2, X3,Y3, X4,Y4, Center, A, B);
    Direct2D.CreatePathGeometry(Geometry);
    Geometry.Open(Sink);
    try
        Sink.BeginFigure(D2D1PointF(FPenPos.X + 0.5, FPenPos.Y + 0.5),
                         D2D1_FIGURE_BEGIN_FILLED);
        try
            Sink.AddLine(A);
            Sink.AddArc(ArcSegment);
        finally
            Sink.EndFigure(D2D1_FIGURE_END_OPEN);
        end;
        FPenPos := TPoint(B);
    finally
        Sink.Close;
    end;
    RequiredState([csHandleValid, csPenValid, csBrushValid]);
    RenderTarget.DrawGeometry(Geometry, FPen.FBrush.Handle, FPen.FWidth, FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.AngleArc(
    X, Y       : Integer;
    Radius     : Cardinal;
    StartAngle : Single;
    SweepAngle : Single);
var
    A, B       : TD2D1Point2F;
    Geometry   : ID2D1PathGeometry;
    Sink       : ID2D1GeometrySink;
    ArcSegment : TD2D1ArcSegment;
    LSin, LCos : Single;
begin
    ArcSegment.size := D2D1SizeF(Radius, Radius);
    ArcSegment.rotationAngle := SweepAngle;
    if Abs(SweepAngle) > 180 then
        ArcSegment.arcSize := D2D1_ARC_SIZE_LARGE
    else
        ArcSegment.arcSize := D2D1_ARC_SIZE_SMALL;
    SinCos(StartAngle * PI / 180, LSin, LCos);
    A := D2D1PointF(LCos * Radius + X, Y - LSin * Radius);
    SinCos((StartAngle + SweepAngle) * PI / 180, LSin, LCos);
    B := D2D1PointF(LCos * Radius + X, Y - LSin * Radius);
    ArcSegment.point := B;
    if SweepAngle > 0 then
        ArcSegment.sweepDirection := D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE
    else
        ArcSegment.sweepDirection := D2D1_SWEEP_DIRECTION_CLOCKWISE;

    Direct2D.CreatePathGeometry(Geometry);
    Geometry.Open(Sink);
    try
        Sink.BeginFigure(D2D1PointF(FPenPos.X + 0.5, FPenPos.Y + 0.5),
                         D2D1_FIGURE_BEGIN_FILLED);
        try
            Sink.AddLine(A);
            Sink.AddArc(ArcSegment);
        finally
            Sink.EndFigure(D2D1_FIGURE_END_OPEN);
        end;
        FPenPos := TPoint(B);
    finally
      Sink.Close;
    end;

    RequiredState([csHandleValid, csPenValid, csBrushValid]);
    RenderTarget.DrawGeometry(Geometry,
                              FPen.FBrush.Handle,
                              FPen.FWidth,
                              FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
var
    A,B, Center : TD2D1Point2F;
    Geometry    : ID2D1PathGeometry;
    Sink        : ID2D1GeometrySink;
    ArcSegment  : TD2D1ArcSegment;
begin
    ArcSegment := PointsToArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4, Center, A, B);
    Direct2D.CreatePathGeometry(Geometry);
    Geometry.Open(Sink);
    try
        Sink.BeginFigure(A, D2D1_FIGURE_BEGIN_FILLED);
        try
            Sink.AddArc(ArcSegment);
        finally
            Sink.EndFigure(D2D1_FIGURE_END_CLOSED);
        end;
    finally
        Sink.Close;
    end;

    RequiredState([csHandleValid, csPenValid, csBrushValid]);
    if FBrush.FStyle <>  bsClear then
        RenderTarget.FillGeometry(Geometry, FBrush.Handle);
    RenderTarget.DrawGeometry(Geometry,
                              FPen.FBrush.Handle,
                              FPen.FWidth,
                              FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.Ellipse(X1, Y1, X2, Y2: Integer);
var
    LEllipse: TD2D1Ellipse;
begin
    LEllipse := D2D1Ellipse(D2D1PointF((X2 + X1) / 2, (Y2 + Y1) / 2),
                                       (X2 - X1 - 1) / 2, (Y2 - Y1 - 1) / 2);

    RequiredState([csHandleValid, csPenValid, csBrushValid]);
    if FBrush.FStyle <> bsClear then
        RenderTarget.FillEllipse(LEllipse, FBrush.Handle);
    RenderTarget.DrawEllipse(LEllipse,
                             FPen.FBrush.Handle,
                             FPen.FWidth,
                             FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.FillRect(const Rect: TRect);
begin
    FillRectangle(Rect);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.FrameRect(const Rect: TRect);
begin
    RequiredState([csHandleValid, csBrushValid]);
    if FBrush.Style <> bsClear then
        RenderTarget.DrawRectangle(
            D2D1RectF(Rect.Left + 0.5, Rect.Top + 0.5,
                      Rect.Right - 0.5, Rect.Bottom - 0.5),
            FBrush.Handle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
var
      Geometry     : ID2D1PathGeometry;
      Sink         : ID2D1GeometrySink;
      ArcSegment   : TD2D1ArcSegment;
      A, B, Center : TD2D1Point2F;
begin
    ArcSegment := PointsToArc(x1,y1, x2,y2, x3,y3, x4,y4, Center, A, B);
    Direct2D.CreatePathGeometry(Geometry);
    Geometry.Open(Sink);
    try
        Sink.BeginFigure(Center, D2D1_FIGURE_BEGIN_FILLED);
        try
            Sink.AddLine(A);
            Sink.AddArc(ArcSegment);
        finally
            Sink.EndFigure(D2D1_FIGURE_END_CLOSED);
        end;
    finally
        Sink.Close;
    end;

    RequiredState([csHandleValid, csPenValid, csBrushValid]);
    if FBrush.FStyle <> bsClear then
        RenderTarget.FillGeometry(Geometry, FBrush.Handle);
    RenderTarget.DrawGeometry(Geometry,
                              FPen.FBrush.Handle,
                              FPen.FWidth,
                              FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.PolyBezier(const Points: array of TPoint);
var
    Geometry : ID2D1PathGeometry;
    Sink     : ID2D1GeometrySink;
    I        : Integer;
begin
    if ((Length(Points) - 1) mod 3) > 0 then
        Exit;

    Direct2D.CreatePathGeometry(Geometry);
    OleCheck(Geometry.Open(Sink));
    try
        Sink.BeginFigure(D2D1PointF(Points[0].X+0.5, Points[0].Y+0.5), D2D1_FIGURE_BEGIN_FILLED);
        try
            I := 1;
            while I < Length(Points) do begin
                Sink.AddBezier(D2D1BezierSegment(
                  D2D1PointF(Points[I].X+0.5, Points[I].Y+0.5),
                  D2D1PointF(Points[I+1].X+0.5, Points[I+1].Y+0.5),
                  D2D1PointF(Points[I+2].X+0.5, Points[I+2].Y+0.5)));
                Inc(I, 3);
            end;
        finally
            Sink.EndFigure(D2D1_FIGURE_END_OPEN);
        end;
    finally
        Sink.Close;
    end;
    RequiredState([csHandleValid, csPenValid, csBrushValid]);
    RenderTarget.DrawGeometry(Geometry,
                              FPen.FBrush.Handle,
                              FPen.FWidth,
                              FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.Polygon(const Points: array of TPoint);
var
    Geometry : ID2D1PathGeometry;
    Sink     : ID2D1GeometrySink;
    I        : Integer;
begin
    Direct2D.CreatePathGeometry(Geometry);
    OleCheck(Geometry.Open(Sink));
    try
        Sink.BeginFigure(D2D1PointF(Points[0].X + 0.5, Points[0].Y + 0.5),
                         D2D1_FIGURE_BEGIN_FILLED);
        try
            for I := 1 to Length(Points) - 1 do
                Sink.AddLine(D2D1PointF(Points[I].X + 0.5, Points[I].Y + 0.5));
        finally
            Sink.EndFigure(D2D1_FIGURE_END_CLOSED);
        end;
    finally
        Sink.Close;
    end;

    RequiredState([csHandleValid, csPenValid, csBrushValid]);
    if FBrush.FStyle <> bsClear then
        RenderTarget.FillGeometry(Geometry, FBrush.Handle);
    RenderTarget.DrawGeometry(Geometry,
                              FPen.FBrush.Handle,
                              FPen.FWidth,
                              FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.Polyline(const Points: array of TPoint);
var
    Geometry : ID2D1PathGeometry;
    Sink     : ID2D1GeometrySink;
    I        : Integer;
begin
    Direct2D.CreatePathGeometry(Geometry);
    OleCheck(Geometry.Open(Sink));
    try
        Sink.BeginFigure(D2D1PointF(Points[0].x+0.5, Points[0].y+0.5), D2D1_FIGURE_BEGIN_HOLLOW);
        try
            for I := 1 to Length(Points) - 1 do
                Sink.AddLine(D2D1PointF(Points[I].X+0.5, Points[I].Y+0.5));
        finally
            Sink.EndFigure(D2D1_FIGURE_END_OPEN);
        end;
    finally
        Sink.Close;
    end;

    RequiredState([csHandleValid, csPenValid]);
      RenderTarget.DrawGeometry(Geometry, FPen.FBrush.Handle, FPen.FWidth, FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.Rectangle(X1, Y1, X2, Y2: Integer);
begin
    RequiredState([csHandleValid, csPenValid, csBrushValid]);
    if FBrush.FStyle <> bsClear then
      RenderTarget.FillRectangle(D2D1RectF(X1 + 0.5, Y1 + 0.5,
                                           X2 - 0.5, Y2 - 0.5),
                                 FBrush.Handle);
    RenderTarget.DrawRectangle(D2D1RectF(X1 + 0.5, Y1 + 0.5, X2 - 0.5, Y2 - 0.5),
                               FPen.FBrush.Handle,
                               FPen.FWidth,
                               FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
var
    RoundRect: TD2D1RoundedRect;
begin
    RoundRect := D2D1RoundedRect(D2D1RectF(X1+0.5, Y1+0.5, X2-0.5, Y2-0.5), X3 / 2, Y3 / 2);
    RequiredState([csHandleValid, csPenValid, csBrushValid]);
    if FBrush.FStyle <> bsClear then
        RenderTarget.FillRoundedRectangle(RoundRect, FBrush.Handle);
    RenderTarget.DrawRoundedRectangle(RoundRect,
                                      FPen.FBrush.Handle,
                                      FPen.FWidth,
                                      FPen.FStrokeStyle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.TextExtent(const Text: String): TSize;
var
    TextLayout  : IDWriteTextLayout;
    TextMetrics : TDwriteTextMetrics;
begin
    OleCheck(DWriteFactory.CreateTextLayout(
                                              PWideChar(Text),
                                              Length(Text),
                                              FFont.Handle,
                                              0,
                                              0,
                                              TextLayout));
    TextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP);
    TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
    TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);

    OleCheck(TextLayout.GetMetrics(TextMetrics));

    Result.cx := Round(TextMetrics.widthIncludingTrailingWhitespace);
    Result.cy := Round(TextMetrics.height);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.TextOut(
    X, Y       : Integer;
    const Text : String);
var
    TextRange   : TDwriteTextRange;
    TextLayout  : IDWriteTextLayout;
    TextMetrics : TDWriteTextMetrics;
begin
    OleCheck(DWriteFactory.CreateTextLayout(PWideChar(Text),
                                            Length(Text),
                                            FFont.Handle,
                                            0, 0,
                                            TextLayout));
    TextRange.startPosition := 0;
    TextRange.length        := Length(Text);
    if fsUnderline in FFont.Style then
        TextLayout.SetUnderline(True, TextRange);

    if fsStrikeOut in FFont.Style then
        TextLayout.SetStrikethrough(True, TextRange);

    TextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP);

    RequiredState([csHandleValid,  csFontValid, csBrushValid]);
    if FBrush.FStyle <> bsClear then begin
        TextLayout.GetMetrics(TextMetrics);
        RenderTarget.FillRectangle(
                 D2D1RectF(X, Y,
                           X + TextMetrics.widthIncludingTrailingWhitespace,
                           Y + TextMetrics.height),
                 FBrush.Handle);
    end;

    RenderTarget.DrawTextLayout(
                      D2D1PointF(x - 0.5, y - 0.5),
                      TextLayout,
                      FFont.FBrush.Handle,
                      FDrawTextOption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.TextRect(
    Rect       : TRect;
    X, Y       : Integer;
    const Text : String);
var
    TextRange   : TDwriteTextRange;
    TextLayout  : IDWriteTextLayout;
    TextMetrics : TDwriteTextMetrics;
begin
    OleCheck(DWriteFactory.CreateTextLayout(PWideChar(Text), Length(Text),
    FFont.Handle, 0, 0, TextLayout));
    TextRange.startPosition := 0;
    TextRange.length := Length(Text);
    if fsUnderline in FFont.Style then
    TextLayout.SetUnderline(True, TextRange);

    if fsStrikeOut in FFont.Style then
        TextLayout.SetStrikethrough(True, TextRange);

    TextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP);

        RequiredState([csHandleValid, csFontValid, csBrushValid]);
    RenderTarget.PushAxisAlignedClip(Rect, D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
    try
        if FBrush.FStyle <> bsClear then begin
            TextLayout.GetMetrics(TextMetrics);
            RenderTarget.FillRectangle(Rect, FBrush.Handle);
        end;

        RenderTarget.DrawTextLayout(D2D1PointF(x - 0.5, y - 0.5),
                                    TextLayout,
                                    FFont.FBrush.Handle,
                                    FDrawTextOption);
    finally
        RenderTarget.PopAxisAlignedClip;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.TextRect(
    var Rect   : TRect;
    var Text   : String;
    TextFormat : TTextFormat);
var
    TextRange       : TDwriteTextRange;
    TextLayout      : IDWriteTextLayout;
    LText           : String;
    LRect           : TRect;
    I, J            : Integer;
    TrimmingSign    : IDWriteInlineObject;
    TrimmingOptions : TDwriteTrimming;
    ClusterMetrics  : array of TDwriteClusterMetrics;
    LineMetrics     : array of TDwriteLineMetrics;
    Clusters        : Cardinal;
    Lines           : Cardinal;
    StartCluster    : Cardinal;
    LineWidth       : Single;
    BgRect          : TD2DRectF;
begin
    FillChar(TextRange, SizeOf(TextRange), 0);
    SetLength(LText, Length(Text));
    // Replace && with &, set underline position
    if ([tfNoPrefix] * TextFormat) = [] then begin
        I := 1;
        J := 1;
        while I <= Length(Text) do begin
            if Text[I] = '&' then begin
                Inc(I);
                if Text[I] <> '&' then
                    TextRange.startPosition := J;
                LText[J] := Text[I];
            end
            else
                LText[J] := Text[I];
            Inc(I);
            Inc(J);
        end;
        SetLength(LText, J-1);
    end
    else
        LText := Text;

    LRect := Rect;
    LRect.Right := Max(LRect.Left, LRect.Right);
    LRect.Bottom := Max(LRect.Bottom, LRect.Top);

    OleCheck(DWriteFactory.CreateTextLayout(PWideChar(LText),
                                            Length(LText),
                                            FFont.Handle,
                                            LRect.Right - LRect.Left,
                                            LRect.Bottom - LRect.Top,
                                            TextLayout));
    if TextLayout = nil then
        Exit;

    if TextRange.startPosition <> 0 then begin
        TextRange.startPosition := TextRange.startPosition - 1;
        TextRange.length := 1;
        TextLayout.SetUnderline(True, TextRange);
    end;

    if [tfPathEllipsis, tfEndEllipsis] * TextFormat <> [] then begin
        DWriteFactory.CreateEllipsisTrimmingSign(FFont.Handle, TrimmingSign);
        FillChar(TrimmingOptions, SizeOf(TrimmingOptions),0);
        if tfPathEllipsis in TextFormat then begin
            TrimmingOptions.delimiter := Ord('\');
            TrimmingOptions.delimiterCount := 1;
        end;
        TrimmingOptions.granularity := DWRITE_TRIMMING_GRANULARITY_CHARACTER;
        TextLayout.SetTrimming(TrimmingOptions, TrimmingSign);
    end;

    //Set Underline and Strike out
    TextRange.startPosition := 0;
    TextRange.length := Length(text);

    if fsUnderline in FFont.Style then
        TextLayout.SetUnderline(True, TextRange);

    if fsStrikeOut in FFont.Style then
        TextLayout.SetStrikethrough(True, TextRange);

    if tfCenter in TextFormat then
        TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER)
    else if tfRight in TextFormat then
        TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_TRAILING);

    if not (tfWordBreak in TextFormat) or (tfSingleLine in TextFormat) then
        TextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP);

    if tfSingleLine in TextFormat then begin
        if tfVerticalCenter in TextFormat then
            TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_CENTER)
        else if tfBottom in TextFormat then
            TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_FAR);
    end;

    if not (tfNoClip in TextFormat) then
        RenderTarget.PushAxisAlignedClip(LRect, D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
    try
        if FBrush.FStyle <> bsClear then begin
            TextLayout.GetClusterMetrics(nil, 0, Clusters);
            SetLength(ClusterMetrics, Clusters);
            TextLayout.GetClusterMetrics(@ClusterMetrics[0], Clusters, Clusters);
            TextLayout.GetLineMetrics(nil, 0, Lines);
            SetLength(LineMetrics, Lines);
            TextLayout.GetLineMetrics(@LineMetrics[0], Lines, Lines);
            StartCluster := 0;

            if [tfSingleLine, tfBottom] * TextFormat = [tfSingleLine, tfBottom] then
                BgRect.top := Rect.Bottom - LineMetrics[0].height
            else if [tfSingleLine, tfVerticalCenter] * TextFormat = [tfSingleLine, tfVerticalCenter] then
                BgRect.top := Rect.top + (Rect.Bottom - rect.Top - LineMetrics[0].height) / 2
            else
                BgRect.top := rect.Top;

            for I := 0 to Lines - 1 do begin
                BgRect.bottom := BgRect.top + LineMetrics[I].height;

                LineWidth := 0;
                for J := 0 to LineMetrics[I].length - 1 do
                    LineWidth := LineWidth + ClusterMetrics[Integer(StartCluster)+J].width;

                if tfCenter in TextFormat then begin
                    BgRect.left := Rect.Left+ (Rect.Right - Rect.Left - LineWidth) / 2;
                    BgRect.right := Rect.Left+ (Rect.Right - Rect.Left - LineWidth) / 2 + LineWidth;
                end
                else if tfRight in TextFormat then begin
                    BgRect.left := Rect.Right -LineWidth;
                    BgRect.right := Rect.Right;
                end
                else begin
                    BgRect.left := Rect.left;
                    BgRect.right := Rect.left + LineWidth;
                end;

                RenderTarget.FillRectangle(BgRect, FBrush.Handle);

                StartCluster := StartCluster + LineMetrics[I].length;
                BgRect.top := BgRect.top + LineMetrics[i].height;
            end;
        end;

        RenderTarget.DrawTextLayout(D2D1PointF(Rect.Left - 0.5, Rect.Top - 0.5), TextLayout,
            FFont.FBrush.Handle, FDrawTextOption);
    finally
        if not (tfNoClip in TextFormat) then
            RenderTarget.PopAxisAlignedClip;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.LineTo(X, Y: Integer);
begin
    RequiredState([csHandleValid, csPenValid]);
    RenderTarget.DrawLine(D2D1PointF(FPenPos.X + 0.5, FPenPos.Y + 0.5),
                          D2D1PointF(X + 0.5, Y + 0.5),
                          FPen.FBrush.Handle,
                          FPen.FWidth,
                          FPen.FStrokeStyle);
    FPenPos := Point(X,Y);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.MoveTo(X, Y: Integer);
begin
    FPenPos := Point(X,Y);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.PolyBezierTo(const Points: array of TPoint);
var
    LPoints : array of TPoint;
    I       : Integer;
begin
    SetLength(LPoints, Length(Points)+1);
    LPoints[0] := FPenPos;
    for I := 0 to Length(Points) - 1 do
        LPoints[I+1] := Points[I];
    PolyBezier(LPoints);
    FPenPos := Points[Length(Points)-1];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.Refresh;
begin
    InitResources;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Invalid D2DCanvas operations
procedure TDirect2D_1Canvas.BrushCopy(
    const Dest   : TRect;
    Bitmap       : TBitmap;
    const Source : TRect;
    Color        : TColor);
begin
    raise EInvalidOperation.Create(SInvalidCanvasOperation);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.DrawFocusRect(const Rect: TRect);
begin
    raise EInvalidOperation.Create(SInvalidCanvasOperation);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.FloodFill(
    X, Y      : Integer;
    Color     : TColor;
    FillStyle : TFillStyle);
begin
    raise EInvalidOperation.Create(SInvalidCanvasOperation);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.GetCanvasOrientation: TCanvasOrientation;
begin
    raise EInvalidOperation.Create(SInvalidCanvasOperation);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.GetClipRect: TRect;
begin
    raise EInvalidOperation.Create(SInvalidCanvasOperation);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.GetPixel(X, Y: Integer): TColor;
begin
    raise EInvalidOperation.Create(SInvalidCanvasOperation);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.SetPixel(X, Y: Integer; Value: TColor);
begin
    raise EInvalidOperation.Create(SInvalidCanvasOperation);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.EraseBackground;
begin
    FRenderTarget.Clear(D3DColorF(BackgroundColor));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.EraseBackground(Color : TColor);
begin
    FRenderTarget.Clear(D3DColorF(Color));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDirect2D_1Canvas.PresentFrame(
    SyncInterval  : UINT = 0;
    PresentFlags  : UINT = 0;
    PresentParams : PDXGIPresentParameters = nil);
var
    Params : TDXGIPresentParameters;
begin
    if PresentParams = nil then begin
        ZeroMemory(@Params, SizeOf(Params));
        Params.DirtyRectsCount := 0;
        Params.pDirtyRects     := nil;
        Params.pScrollRect     := nil;
        Params.pScrollOffset   := nil;
        DXGISwapChain.Present1(SyncInterval, PresentFlags, @Params);
    end
    else
        DXGISwapChain.Present1(SyncInterval, PresentFlags, PresentParams);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDirect2D_1Canvas.CreateBrush(Color: TColor): ID2D1Brush;
var
    Brush: ID2D1SolidColorBrush;
    AColor : D3DCOLORVALUE;
begin
    AColor := D3DColorF(Color);
    ID2D1RenderTarget(RenderTarget).CreateSolidColorBrush(AColor, nil, Brush);
    Result := Brush;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ Singleton objects }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
var
  SingletonD2DFactory: ID2D1Factory1;

function D2DFactory(
    FactoryType    : TD2D1FactoryType    = D2D1_FACTORY_TYPE_SINGLE_THREADED;
    FactoryOptions : PD2D1FactoryOptions = nil): ID2D1Factory1;
var
    LD2DFactory : ID2D1Factory;
begin
    if SingletonD2DFactory = nil then begin
        OleCheck(D2D1CreateFactory(FactoryType,
                                   IID_ID2D1Factory,
                                   FactoryOptions,
                                   LD2DFactory));

        if FpDefaultDrawTextOption <> nil then begin
            if Supports(LD2DFactory, ID2D1Factory5) then
                FpDefaultDrawTextOption^ := D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT;
        end;

        if InterlockedCompareExchangePointer(Pointer(SingletonD2DFactory),
                                             Pointer(LD2DFactory), nil) = nil then
            LD2DFactory._AddRef;
    end;
    Result := SingletonD2DFactory;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
var
    SingletonDWriteFactory: IDWriteFactory1;

function DWriteFactory(
    FacType: TDWriteFactoryType = DWRITE_FACTORY_TYPE_SHARED): IDWriteFactory1;
var
    LDWriteFactory : IUnknown;
begin
    if SingletonDWriteFactory = nil then begin
        OleCheck(DWriteCreateFactory(FacType,
                                     IID_IDWriteFactory1,
                                     LDWriteFactory));
        if InterlockedCompareExchangePointer(Pointer(SingletonDWriteFactory),
                                             Pointer(LDWriteFactory),
                                             nil) = nil then
            LDWriteFactory._AddRef;
    end;
    Result := SingletonDWriteFactory;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function D2D1ColorF(const AColor: TColor): TD2D1ColorF; overload;
var
    RGB: Cardinal;
begin
    RGB := ColorToRGB(AColor);
    Result.r :=   RGB         and $FF  / 255;
    Result.g := ((RGB shr  8) and $FF) / 255;
    Result.b := ((RGB shr 16) and $FF) / 255;
    if AColor = clNone then
        Result.a := 0.0
    else
        Result.a := 1.0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function D2D1ColorF(const AColor: TColor; Alpha: Single): TD2D1ColorF; overload;
begin
   Result   := D2D1ColorF(AColor);
   Result.a := Alpha;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function D3DColorF(const AColor: TColor): D3DCOLORVALUE; overload;
var
    RGB: Cardinal;
begin
    RGB      := ColorToRGB(AColor);
    Result.r :=   RGB         and $FF  / 255;
    Result.g := ((RGB shr  8) and $FF) / 255;
    Result.b := ((RGB shr 16) and $FF) / 255;
    if AColor = clNone then
        Result.a := 0.0
    else
        Result.a := 1.0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
