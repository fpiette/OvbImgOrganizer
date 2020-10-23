{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Sep 13, 2016
Description:  A PaintBox like control using Direct2D
              Code based on DocWiki example code at:
              http://docwiki.embarcadero.com/RADStudio/Seattle/en/Using_the_Direct2D_Canvas
License:      This program is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.0
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OvbImgOrganizer.PaintPanel;

interface

uses
    Winapi.Messages, WinApi.D2D1,
    System.Classes,
{$IFDEF USE_DIRECT2D_1}
    Winapi.D2D1_1,
    Vcl.Direct2D_1,
{$ELSE}
    Vcl.Direct2D,
{$ENDIF}
    Vcl.Graphics, Vcl.ExtCtrls;

type
{$IFDEF USE_DIRECT2D_1}
    TMyRenderTarget = ID2D1DeviceContext;
{$ELSE}
    TMyRenderTarget = ID2D1HwndRenderTarget;
{$ENDIF}

    TOvbCustomPaintPanel = class(TCustomPanel)
    private
        FOnPaint               : TNotifyEvent;
        FUseD2D                : Boolean;
        FUseD2DCanvasFont      : Boolean;
        FUseD2DCanvasBrush     : Boolean;
        FD2DCanvas             : TDirect2DCanvas;
        FPrevRenderTarget      : IntPtr;
        FOnReinitResources     : TNotifyEvent;
        function  CreateD2DCanvas: Boolean;
        procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND; // PALOFF
        procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;           // PALOFF
        procedure WMSize(var Msg: TWMSize); message WM_SIZE;              // PALOFF
        procedure SetAccelerated(const Value: Boolean);
        function  GetGDICanvas: TCanvas;
        function  GetOSCanvas: TCustomCanvas;
    protected
        procedure CreateWnd; override;
        function  GetRenderTarget : TMyRenderTarget;
        procedure TriggerReinitResources; virtual;
{$IFDEF USE_DIRECT2D_1}
        procedure D2DCanvasReinitResources(Sender: TObject); virtual;
{$ENDIF}
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy(); override;
        procedure Paint; override;
        property Accelerated           : Boolean
                                                   read  FUseD2D
                                                   write SetAccelerated;
        property Canvas                : TCustomCanvas
                                                   read  GetOSCanvas;
        property GDICanvas             : TCanvas
                                                   read  GetGDICanvas;
        property D2DCanvas             : TDirect2DCanvas
                                                   read  FD2DCanvas;
        property RenderTarget          : TMyRenderTarget
                                                   read  GetRenderTarget;
        property UseD2DCanvasFont      : Boolean   read  FUseD2DCanvasFont
                                                   write FUseD2DCanvasFont;
        property UseD2DCanvasBrush     : Boolean   read  FUseD2DCanvasBrush
                                                   write FUseD2DCanvasBrush;
        property OnPaint               : TNotifyEvent
                                                   read  FOnPaint
                                                   write FOnPaint;
        property OnReinitResources     : TNotifyEvent
                                                   read  FOnReinitResources
                                                   write FOnReinitResources;
//        property OnRenderTargetChanged : TNotifyEvent
//                                                   read  FOnRenderTargetChanged
//                                                   write FOnRenderTargetChanged;
    end;

    TOvbPaintPanel = class(TOvbCustomPaintPanel)
    published
        property Accelerated;
        property Align;
        property Anchors;
        property Canvas;
        property Color;
        property GDICanvas;
        property D2DCanvas;
        property RenderTarget;
        property BevelEdges;
        property BevelInner;
        property BevelOuter;
        property BevelKind;
        property BevelWidth;
        property BorderWidth;
        property Ctl3D;
        property ParentBackground;
        property ParentCtl3D;
        property OnAlignInsertBefore;
        property OnAlignPosition;
        property OnDockDrop;
        property OnDockOver;
        property OnEnter;
        property OnExit;
        property OnGetSiteInfo;
        property OnKeyDown;
        property OnKeyPress;
        property OnKeyUp;
        property OnUnDock;
        property DragCursor;
        property DragMode;
        property ParentBiDiMode;
        property ParentColor;
        property ParentFont;
        property ParentShowHint;
        property PopupMenu;
        property UseD2DCanvasFont;
        property UseD2DCanvasBrush;
        property OnCanResize;
        property OnClick;
        property OnConstrainedResize;
        property OnContextPopup;
        property OnDblClick;
        property OnDragDrop;
        property OnDragOver;
        property OnEndDock;
        property OnEndDrag;
        property OnMouseActivate;
        property OnMouseDown;
        property OnMouseEnter;
        property OnMouseLeave;
        property OnMouseMove;
        property OnMouseUp;
        property OnMouseWheel;
        property OnMouseWheelDown;
        property OnMouseWheelUp;
        property OnResize;
        property OnStartDock;
        property OnStartDrag;
        property OnPaint;
    end;

implementation

uses
    Winapi.Windows, System.SysUtils, Vcl.Controls;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TOvbCustomPaintPanel }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TOvbCustomPaintPanel.Create(AOwner: TComponent);
begin
    FUseD2DCanvasFont  := TRUE;
    FUseD2DCanvasBrush := TRUE;
    inherited Create(AOwner);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TOvbCustomPaintPanel.Destroy;
begin
    FreeAndNil(FD2DCanvas);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbCustomPaintPanel.CreateD2DCanvas: Boolean;
begin
    try
        FD2DCanvas.Free;
        FD2DCanvas    := TDirect2DCanvas.Create(Handle);
{$IFDEF USE_DIRECT2D_1}
        FD2DCanvas.OnReinitResources := D2DCanvasReinitResources;
{$ELSE}
        TriggerReinitResources;
{$ENDIF}
        Result        := TRUE;
    except
        Result        := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomPaintPanel.CreateWnd;
begin
    inherited CreateWnd;
    if (Win32MajorVersion >= 6) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
        FUseD2D := CreateD2DCanvas
    else
        FUseD2D := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// This event handler is called when it is time to reinitialize resources
// depending on the RenderTarget because it has been recreated.
{$IFDEF USE_DIRECT2D_1}
procedure TOvbCustomPaintPanel.D2DCanvasReinitResources(Sender : TObject);
begin
//    if Assigned(FGPBitmap) then
//        FBitmapToPaint := CreateDirect2DBitmap(FD2DCanvas.RenderTarget,
//                                               FGPBitmap);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbCustomPaintPanel.GetGDICanvas: TCanvas;
begin
    if FUseD2D then
        Result := nil
    else
        Result := inherited Canvas;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbCustomPaintPanel.GetOSCanvas: TCustomCanvas;
begin
    if FUseD2D then
        Result := FD2DCanvas
    else
        Result := inherited Canvas;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbCustomPaintPanel.GetRenderTarget: TMyRenderTarget;
begin
    if FD2DCanvas <> nil then begin
        Result := FD2DCanvas.RenderTarget as TMyRenderTarget;
        if FPrevRenderTarget <> IntPtr(Result) then begin
            FPrevRenderTarget := IntPtr(Result);
            TriggerReinitResources;
        end;
    end
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomPaintPanel.SetAccelerated(const Value: Boolean);
begin
    if Value = FUseD2D then
        Exit;
    if not Value then begin
        FUseD2D := FALSE;
        Repaint;
    end
    else begin
        FUseD2D := FD2DCanvas <> nil;
        if FUseD2D then
            Repaint;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomPaintPanel.TriggerReinitResources;
begin
    if Assigned(FOnReinitResources) then
        FOnReinitResources(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomPaintPanel.Paint;
begin
    if FUseD2D then begin
        if FUseD2DCanvasFont then
            D2DCanvas.Font.Assign(Font);
        if FUseD2DCanvasBrush then
            D2DCanvas.Brush.Color := Color;
        if csDesigning in ComponentState then begin
            D2DCanvas.Pen.Style   := psDash;
            D2DCanvas.Brush.Style := bsSolid;
            D2DCanvas.Rectangle(0, 0, Width, Height);
        end;
    end
    else begin
        GDICanvas.Font.Assign(Font);
        GDICanvas.Brush.Color := Color;
         if csDesigning in ComponentState then begin
            GDICanvas.Pen.Style   := psDash;
            GDICanvas.Brush.Style := bsSolid;
            GDICanvas.Rectangle(0, 0, Width, Height);
        end;
    end;
    if Assigned(FOnPaint) then
        FOnPaint(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomPaintPanel.WMEraseBkGnd(var Msg: TMessage);
begin
    Msg.Result := 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomPaintPanel.WMPaint(var Msg: TWMPaint);
var
    PaintStruct: TPaintStruct;
begin
    if FUseD2D then begin
        BeginPaint(Handle, PaintStruct);
        FD2DCanvas.BeginDraw;
        Paint;
        FD2DCanvas.EndDraw;
        EndPaint(Handle, PaintStruct);
    end
    else
        inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbCustomPaintPanel.WMSize(var Msg: TWMSize);
begin
    if FD2DCanvas <> nil then begin
{$IFDEF USE_DIRECT2D_1}
        FD2DCanvas.Refresh;
{$ELSE}
        var Size: D2D1_SIZE_U := D2D1SizeU(Width, Height);
        ID2D1HwndRenderTarget(FD2DCanvas.RenderTarget).Resize(Size);
{$ENDIF}
    end;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
