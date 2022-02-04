{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Description:  Direct2D 1.1 translation for Delphi
              File: D2D1.h
              Copyright (c) Microsoft Corporation.
              All Rights Reserved.

              Translator: François PIETTE @ OverByte
Creation:     Aug 09, 2020
License:      This code is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Winapi.D2D1_1;

interface

uses
    Winapi.Windows,
    Winapi.ActiveX,
    Winapi.D2D1,
    Winapi.D2D1EffectAuthor,
    Winapi.DXGI,
    Winapi.DXGIFormat,
    Winapi.Wincodec,
    Winapi.DCommon,
    Winapi.DocumentTarget;

const
    SID_ID2D1Factory1                = '{bb12d362-daee-4b9a-aa1d-14ba401cfa1f}';
    SID_ID2D1Device                  = '{47dd575d-ac05-4cdd-8049-9b02cd16f44c}';
    SID_ID2D1DeviceContext           = '{e8f7fe7a-191c-466d-ad95-975678bda998}';
    SID_ID2D1StrokeStyle1            = '{10a72a66-e91c-43f4-993f-ddf4b82b0b4a}';
    SID_ID2D1PathGeometry1           = '{62baa2d2-ab54-41b7-b872-787e0106a421}';
    SID_ID2D1DrawingStateBlock1      = '{689f1f85-c72e-4e33-8f19-85754efd5ace}';
    SID_ID2D1GdiMetafile             = '{2f543dc3-cfc1-4211-864f-cfd91c6f3395}';
    SID_ID2D1GdiMetafileSink         = '{82237326-8111-4f7c-bcf4-b5c1175564fe}';
    SID_ID2D1Properties              = '{483473d7-cd46-4f9d-9d3a-3112aa80159d}';
    SID_ID2D1ColorContext            = '{1c4820bb-5771-4518-a581-2fe4dd0ec657}';
    SID_ID2D1Bitmap1                 = '{a898a84c-3873-4588-b08b-ebbf978df041}';
    SID_ID2D1Image                   = '{65019f75-8da2-497c-b32c-dfa34e48ede6}';
    SID_ID2D1Effect                  = '{28211a43-7d89-476f-8181-2d6159b220ad}';
    SID_ID2D1GradientStopCollection1 = '{ae1572f4-5dd0-4777-998b-9279472ae63b}';
    SID_ID2D1ImageBrush              = '{fe9e984d-3f95-407c-b5db-cb94d4e8f87c}';
    SID_ID2D1BitmapBrush1            = '{41343a53-e41a-49a2-91cd-21793bbb62e5}';
    SID_ID2D1CommandList             = '{b4f34a19-2383-4d76-94f6-ec343657c3dc}';
    SID_ID2D1CommandSink             = '{54d7898a-a061-40a7-bec7-e465bcba2c4f}';
    SID_ID2D1PrintControl            = '{2c1d867d-c290-41c8-ae7e-34a98702e9a5}';

    IID_ID2D1Factory1                : TGUID = SID_ID2D1Factory1;
    IID_ID2D1Device                  : TGUID = SID_ID2D1Device;
    IID_ID2D1DeviceContext           : TGUID = SID_ID2D1DeviceContext;
    IID_ID2D1StrokeStyle1            : TGUID = SID_ID2D1StrokeStyle1;
    IID_ID2D1PathGeometry1           : TGUID = SID_ID2D1PathGeometry1;
    IID_ID2D1DrawingStateBlock1      : TGUID = SID_ID2D1DrawingStateBlock1;
    IID_ID2D1GdiMetafile             : TGUID = SID_ID2D1GdiMetafile;
    IID_ID2D1GdiMetafileSink         : TGUID = SID_ID2D1GdiMetafileSink;
    IID_ID2D1Properties              : TGUID = SID_ID2D1Properties;
    IID_ID2D1ColorContext            : TGUID = SID_ID2D1ColorContext;
    IID_ID2D1Bitmap1                 : TGUID = SID_ID2D1Bitmap1;
    IID_ID2D1Effect                  : TGUID = SID_ID2D1Effect;
    IID_ID2D1GradientStopCollection1 : TGUID = SID_ID2D1GradientStopCollection1;
    IID_ID2D1Image                   : TGUID = SID_ID2D1Image;
    IID_ID2D1ImageBrush              : TGUID = SID_ID2D1ImageBrush;
    IID_ID2D1BitmapBrush1            : TGUID = SID_ID2D1BitmapBrush1;
    IID_ID2D1CommandList             : TGUID = SID_ID2D1CommandList;
    IID_ID2D1CommandSink             : TGUID = SID_ID2D1CommandSink;
    IID_ID2D1PrintControl            : TGUID = SID_ID2D1PrintControl;

type
    REFCLSID  = PGUID;
    TRefClsID = TGUID;

    ID2D1ColorContext = interface;
    ID2D1Device       = interface;
    ID2D1Effect       = interface;

    // Missing for d2d1.h translation to Delphi
    // Line 1094 of d2d1.h
    /// <summary>
    /// Represents a producer of pixels that can fill an arbitrary 2D plane.
    /// </summary>
    ID2D1Image = interface(ID2D1Resource)
        ['{65019f75-8da2-497c-b32c-dfa34e48ede6}']
    end;

    // Line 39
    /// <summary>
    /// Function pointer to construct a new effect once registered.
    /// </summary>
    PD2D1_EFFECT_FACTORY = function (
        effectImpl : PIUnknown
        ) : HResult; stdcall;

    // Line 129
    /// <summary>
    /// Specifies how the bitmap can be used.
    /// </summary>
    D2D1_BITMAP_OPTIONS = (
        /// <summary>
        /// The bitmap is created with default properties.
        /// </summary>
        D2D1_BITMAP_OPTIONS_NONE           = $00000000,

        /// <summary>
        /// The bitmap can be specified as a target in ID2D1DeviceContext::SetTarget
        /// </summary>
        D2D1_BITMAP_OPTIONS_TARGET         = $00000001,

        /// <summary>
        /// The bitmap cannot be used as an input to DrawBitmap, DrawImage, in a bitmap
        /// brush or as an input to an effect.
        /// </summary>
        D2D1_BITMAP_OPTIONS_CANNOT_DRAW    = $00000002,

        /// <summary>
        /// The bitmap can be read from the CPU.
        /// </summary>
        D2D1_BITMAP_OPTIONS_CPU_READ       = $00000004,

        /// <summary>
        /// The bitmap works with the ID2D1GdiInteropRenderTarget::GetDC API.
        /// </summary>
        D2D1_BITMAP_OPTIONS_GDI_COMPATIBLE = $00000008,
        D2D1_BITMAP_OPTIONS_FORCE_DWORD    = Integer($ffffffff)
    );


    // Line 167
    /// <summary>
    /// Specifies the composite mode that will be applied.
    /// </summary>
    D2D1_COMPOSITE_MODE = (
        D2D1_COMPOSITE_MODE_SOURCE_OVER         = 0,
        D2D1_COMPOSITE_MODE_DESTINATION_OVER    = 1,
        D2D1_COMPOSITE_MODE_SOURCE_IN           = 2,
        D2D1_COMPOSITE_MODE_DESTINATION_IN      = 3,
        D2D1_COMPOSITE_MODE_SOURCE_OUT          = 4,
        D2D1_COMPOSITE_MODE_DESTINATION_OUT     = 5,
        D2D1_COMPOSITE_MODE_SOURCE_ATOP         = 6,
        D2D1_COMPOSITE_MODE_DESTINATION_ATOP    = 7,
        D2D1_COMPOSITE_MODE_XOR                 = 8,
        D2D1_COMPOSITE_MODE_PLUS                = 9,
        D2D1_COMPOSITE_MODE_SOURCE_COPY         = 10,
        D2D1_COMPOSITE_MODE_BOUNDED_SOURCE_COPY = 11,
        D2D1_COMPOSITE_MODE_MASK_INVERT         = 12,
        D2D1_COMPOSITE_MODE_FORCE_DWORD         = Integer($ffffffff)
    );


    // Line 190
    /// <summary>
    /// This specifies the precision that should be used in buffers allocated by D2D.
    /// </summary>
    D2D1_BUFFER_PRECISION = (
        D2D1_BUFFER_PRECISION_UNKNOWN         = 0,
        D2D1_BUFFER_PRECISION_8BPC_UNORM      = 1,
        D2D1_BUFFER_PRECISION_8BPC_UNORM_SRGB = 2,
        D2D1_BUFFER_PRECISION_16BPC_UNORM     = 3,
        D2D1_BUFFER_PRECISION_16BPC_FLOAT     = 4,
        D2D1_BUFFER_PRECISION_32BPC_FLOAT     = 5,
        D2D1_BUFFER_PRECISION_FORCE_DWORD     = Integer($ffffffff)
    );

    // Line 206
    /// <summary>
    /// This describes how the individual mapping operation should be performed.
    /// </summary>
    D2D1_MAP_OPTIONS = (

        /// <summary>
        /// The mapped pointer has undefined behavior.
        /// </summary>
        D2D1_MAP_OPTIONS_NONE = 0,

        /// <summary>
        /// The mapped pointer can be read from.
        /// </summary>
        D2D1_MAP_OPTIONS_READ = 1,

        /// <summary>
        /// The mapped pointer can be written to.
        /// </summary>
        D2D1_MAP_OPTIONS_WRITE = 2,

        /// <summary>
        /// The previous contents of the bitmap are discarded when it is mapped.
        /// </summary>
        D2D1_MAP_OPTIONS_DISCARD = 4,
        D2D1_MAP_OPTIONS_FORCE_DWORD = Integer($ffffffff)

    );

    // Line 238
    /// <summary>
    /// This is used to specify the quality of image scaling with
    /// ID2D1DeviceContext::DrawImage and with the 2D Affine Transform Effect.
    /// </summary>
    D2D1_INTERPOLATION_MODE = (
        D2D1_INTERPOLATION_MODE_NEAREST_NEIGHBOR    = D2D1_INTERPOLATION_MODE_DEFINITION_NEAREST_NEIGHBOR,
        D2D1_INTERPOLATION_MODE_LINEAR              = D2D1_INTERPOLATION_MODE_DEFINITION_LINEAR,
        D2D1_INTERPOLATION_MODE_CUBIC               = D2D1_INTERPOLATION_MODE_DEFINITION_CUBIC,
        D2D1_INTERPOLATION_MODE_MULTI_SAMPLE_LINEAR = D2D1_INTERPOLATION_MODE_DEFINITION_MULTI_SAMPLE_LINEAR,
        D2D1_INTERPOLATION_MODE_ANISOTROPIC         = D2D1_INTERPOLATION_MODE_DEFINITION_ANISOTROPIC,
        D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC  = D2D1_INTERPOLATION_MODE_DEFINITION_HIGH_QUALITY_CUBIC,
        D2D1_INTERPOLATION_MODE_FORCE_DWORD         = Integer($ffffffff)
    );

    // Line 255
    /// <summary>
    /// This specifies what units should be accepted by the D2D API.
    /// </summary>
    D2D1_UNIT_MODE = (
        D2D1_UNIT_MODE_DIPS = 0,
        D2D1_UNIT_MODE_PIXELS = 1,
        D2D1_UNIT_MODE_FORCE_DWORD = Integer($ffffffff)
    );
    TD2D1UnitMode = D2D1_UNIT_MODE;
    PD2D1UnitMode = ^TD2D1UnitMode;

    // Line 267
    /// <summary>
    /// Defines a color space.
    /// </summary>
    D2D1_COLOR_SPACE = (
        /// <summary>
        /// The color space is described by accompanying data, such as a color profile.
        /// </summary>
        D2D1_COLOR_SPACE_CUSTOM = 0,

        /// <summary>
        /// The sRGB color space.
        /// </summary>
        D2D1_COLOR_SPACE_SRGB = 1,

        /// <summary>
        /// The scRGB color space.
        /// </summary>
        D2D1_COLOR_SPACE_SCRGB = 2,
        D2D1_COLOR_SPACE_FORCE_DWORD = Integer($ffffffff)
    );

    // Line 292
    /// <summary>
    /// This specifies options that apply to the device context for its lifetime.
    /// </summary>
    D2D1_DEVICE_CONTEXT_OPTIONS = (
        D2D1_DEVICE_CONTEXT_OPTIONS_NONE = 0,

        /// <summary>
        /// Geometry rendering will be performed on many threads in parallel, a single
        /// thread is the default.
        /// </summary>
        D2D1_DEVICE_CONTEXT_OPTIONS_ENABLE_MULTITHREADED_OPTIMIZATIONS = 1,
        D2D1_DEVICE_CONTEXT_OPTIONS_FORCE_DWORD = Integer($ffffffff)
    );

    // Line 311
    /// <summary>
    /// Defines how the world transform, dots per inch (dpi), and stroke width affect
    /// the shape of the pen used to stroke a primitive.
    /// </summary>
    D2D1_STROKE_TRANSFORM_TYPE = (
        /// <summary>
        /// The stroke respects the world transform, the DPI, and the stroke width.
        /// </summary>
        D2D1_STROKE_TRANSFORM_TYPE_NORMAL = 0,

        /// <summary>
        /// The stroke does not respect the world transform, but it does respect the DPI and
        /// the stroke width.
        /// </summary>
        D2D1_STROKE_TRANSFORM_TYPE_FIXED = 1,

        /// <summary>
        /// The stroke is forced to one pixel wide.
        /// </summary>
        D2D1_STROKE_TRANSFORM_TYPE_HAIRLINE = 2,
        D2D1_STROKE_TRANSFORM_TYPE_FORCE_DWORD = Integer($ffffffff)
    );
    TD2D1StrokeTranformType = D2D1_STROKE_TRANSFORM_TYPE;
    PD2D1StrokeTranformType = ^TD2D1StrokeTranformType;

    // Line 338
    /// <summary>
    /// A blend mode that applies to all primitives drawn on the context.
    /// </summary>
    D2D1_PRIMITIVE_BLEND = (
        D2D1_PRIMITIVE_BLEND_SOURCE_OVER = 0,
        D2D1_PRIMITIVE_BLEND_COPY        = 1,
        D2D1_PRIMITIVE_BLEND_MIN         = 2,
        D2D1_PRIMITIVE_BLEND_ADD         = 3,
        D2D1_PRIMITIVE_BLEND_MAX         = 4,
        D2D1_PRIMITIVE_BLEND_FORCE_DWORD = Integer($ffffffff)

    );
    TD2D1PrimitiveBlend = D2D1_PRIMITIVE_BLEND;
    PD2D1PrimitiveBlend = ^TD2D1PrimitiveBlend;

    // Line 377
    /// <summary>
    /// This specifies how colors are interpolated.
    /// </summary>
    D2D1_COLOR_INTERPOLATION_MODE = (
        /// <summary>
        /// Colors will be interpolated in straight alpha space.
        /// </summary>
        D2D1_COLOR_INTERPOLATION_MODE_STRAIGHT      = 0,

        /// <summary>
        /// Colors will be interpolated in premultiplied alpha space.
        /// </summary>
        D2D1_COLOR_INTERPOLATION_MODE_PREMULTIPLIED = 1,
        D2D1_COLOR_INTERPOLATION_MODE_FORCE_DWORD   = Integer($ffffffff)
    );

    // Line 400
    /// <summary>
    /// Extended bitmap properties.
    /// </summary>
    D2D1_BITMAP_PROPERTIES1 = record
        pixelFormat : D2D1_PIXEL_FORMAT;
        dpiX        : Single;
        dpiY        : Single;

        /// <summary>
        /// Specifies how the bitmap can be used.
        /// </summary>
        bitmapOptions : UINT; //D2D1_BITMAP_OPTIONS;
        colorContext  : ID2D1ColorContext;
    end;
    TD2D1BitmapProperties1   = D2D1_BITMAP_PROPERTIES1;
    PD2D1BitmapProperties1   = ^TD2D1BitmapProperties1;
    PD2D1_BITMAP_PROPERTIES1 = ^D2D1_BITMAP_PROPERTIES1;

    // Line 418
    /// <summary>
    /// Describes mapped memory from the ID2D1Bitmap1::Map API.
    /// </summary>
    D2D1_MAPPED_RECT = record
        pitch : UINT32;
        bits  : PBYTE;
    end;

    // Line 429
    /// <summary>
    /// This controls advanced settings of the Direct2D imaging pipeline.
    /// </summary>
    D2D1_RENDERING_CONTROLS = record
        /// <summary>
        /// The default buffer precision, used if the precision isn't otherwise specified.
        /// </summary>
        bufferPrecision : D2D1_BUFFER_PRECISION;

        /// <summary>
        /// The size of allocated tiles used to render imaging effects.
        /// </summary>
        tileSize        : D2D1_SIZE_U;
    end;


    /// <summary>
    /// This identifies a certain input connection of a certain effect.
    /// </summary>
    D2D1_EFFECT_INPUT_DESCRIPTION = record
        /// <summary>
        /// The effect whose input connection is being specified.
        /// </summary>
        effect : ID2D1Effect;

        /// <summary>
        /// The index of the input connection into the specified effect.
        /// </summary>
        inputIndex : UINT32;

        /// <summary>
        /// The rectangle which would be available on the specified input connection during
        /// render operations.
        /// </summary>
        inputRectangle : D2D1_RECT_F;
    end;

    // Line 472
    D2D1_MATRIX_4X4_F  = D2D_MATRIX_4X4_F;
    PD2D1_MATRIX_4X4_F = ^D2D1_MATRIX_4X4_F;

    // Line 490
    /// <summary>
    /// Creation properties for an image brush.
    /// </summary>
    D2D1_IMAGE_BRUSH_PROPERTIES = record
        sourceRectangle   : D2D1_RECT_F;
        extendModeX       : D2D1_EXTEND_MODE;
        extendModeY       : D2D1_EXTEND_MODE;
        interpolationMode : D2D1_INTERPOLATION_MODE
    end;
    TD2D1ImageBrushProperties    = D2D1_IMAGE_BRUSH_PROPERTIES;
    PD2D1ImageBrushProperties    = ^TD2D1ImageBrushProperties;
    PD2D1_IMAGE_BRUSH_PROPERTIES = ^D2D1_IMAGE_BRUSH_PROPERTIES;

    // Line 503
    /// <summary>
    /// Describes the extend modes and the interpolation mode of an ID2D1BitmapBrush.
    /// </summary>
    D2D1_BITMAP_BRUSH_PROPERTIES1 = record
        extendModeX       : D2D1_EXTEND_MODE;
        extendModeY       : D2D1_EXTEND_MODE;
        interpolationMode : D2D1_INTERPOLATION_MODE;
    end;
    TD2D1BitmapBrushProperties1    = D2D1_BITMAP_BRUSH_PROPERTIES1;
    PD2D1BitmapBrushProperties1    = ^TD2D1BitmapBrushProperties1;
    PD2D1_BITMAP_BRUSH_PROPERTIES1 = ^D2D1_BITMAP_BRUSH_PROPERTIES1;

    // Line 515
    /// <summary>
    /// This defines how geometries should be drawn and widened.
    /// </summary>
    D2D1_STROKE_STYLE_PROPERTIES1 = record
        startCap   : D2D1_CAP_STYLE;
        endCap     : D2D1_CAP_STYLE;
        dashCap    : D2D1_CAP_STYLE;
        lineJoin   : D2D1_LINE_JOIN;
        miterLimit : Single;
        dashStyle  : D2D1_DASH_STYLE;
        dashOffset : Single;

        /// <summary>
        /// How the nib of the stroke is influenced by the context properties.
        /// </summary>
        transformType : D2D1_STROKE_TRANSFORM_TYPE;
    end;
    TD2D1StrokeStyleProperties1 = D2D1_STROKE_STYLE_PROPERTIES1;
    PD2D1StrokeStyleProperties1 = ^TD2D1StrokeStyleProperties1;

    // Line 536
    /// <summary>
    /// Specifies how the layer contents should be prepared.
    /// </summary>
    D2D1_LAYER_OPTIONS1 = (
        D2D1_LAYER_OPTIONS1_NONE                       = 0,
        D2D1_LAYER_OPTIONS1_INITIALIZE_FROM_BACKGROUND = 1,
        D2D1_LAYER_OPTIONS1_IGNORE_ALPHA               = 2,
        D2D1_LAYER_OPTIONS1_FORCE_DWORD                = Integer($ffffffff)
    );


    // Line 551
    /// <summary>
    /// All parameters related to pushing a layer.
    /// </summary>
    D2D1_LAYER_PARAMETERS1 = record
        contentBounds     : D2D1_RECT_F;
        geometricMask     : ID2D1Geometry;
        maskAntialiasMode : D2D1_ANTIALIAS_MODE;
        maskTransform     : D2D1_MATRIX_3X2_F;
        opacity           : Single;
        opacityBrush      : ID2D1Brush;
        layerOptions      : D2D1_LAYER_OPTIONS1;
    end;


    // Line 567
    /// <summary>
    /// Defines when font resources should be subset during printing.
    /// </summary>
    D2D1_PRINT_FONT_SUBSET_MODE = (
        /// <summary>
        /// Subset for used glyphs, send and discard font resource after every five pages
        /// </summary>
        D2D1_PRINT_FONT_SUBSET_MODE_DEFAULT = 0,

        /// <summary>
        /// Subset for used glyphs, send and discard font resource after each page
        /// </summary>
        D2D1_PRINT_FONT_SUBSET_MODE_EACHPAGE = 1,

        /// <summary>
        /// Do not subset, reuse font for all pages, send it after first page
        /// </summary>
        D2D1_PRINT_FONT_SUBSET_MODE_NONE = 2,
        D2D1_PRINT_FONT_SUBSET_MODE_FORCE_DWORD = Integer($ffffffff)
    );


    // Line 592
    /// <summary>
    /// This describes the drawing state.
    /// </summary>
    D2D1_DRAWING_STATE_DESCRIPTION1 = record
        antialiasMode     : D2D1_ANTIALIAS_MODE;
        textAntialiasMode : D2D1_TEXT_ANTIALIAS_MODE;
        tag1              : D2D1_TAG;
        tag2              : D2D1_TAG;
        transform         : D2D1_MATRIX_3X2_F;
        primitiveBlend    : D2D1_PRIMITIVE_BLEND;
        unitMode          : D2D1_UNIT_MODE;
    end;
    TD2D1DrawingStateDescription1 = D2D1_DRAWING_STATE_DESCRIPTION1;
    PD2D1DrawingStateDescription1 = ^TD2D1DrawingStateDescription1;


    // Line 608
    /// <summary>
    /// The creation properties for a ID2D1PrintControl object.
    /// </summary>
    D2D1_PRINT_CONTROL_PROPERTIES = record
        fontSubset : D2D1_PRINT_FONT_SUBSET_MODE;

        /// <summary>
        /// DPI for rasterization of all unsupported D2D commands or options, defaults to
        /// 150.0
        /// </summary>
        rasterDPI : Single;

        /// <summary>
        /// Color space for vector graphics in XPS package
        /// </summary>
        colorSpace : D2D1_COLOR_SPACE;

    end;
    PD2D1_PRINT_CONTROL_PROPERTIES = ^D2D1_PRINT_CONTROL_PROPERTIES;


    // Line 670
    /// <summary>
    /// User-implementable interface for introspecting on a metafile.
    /// </summary>
    ID2D1GdiMetafileSink = interface(IUnknown)
        [SID_ID2D1GdiMetafileSink]

        /// <summary>
        /// Callback for examining a metafile record.
        /// </summary>
        function ProcessRecord(
            recordType       : DWORD;
            const recordData : Pointer;
            recordDataSize   : DWORD
            ): HResult; stdcall;
    end;

    // Line 687
    /// <summary>
    /// Interface encapsulating a GDI/GDI+ metafile.
    /// </summary>
    ID2D1GdiMetafile = interface(ID2D1Resource)
        /// <summary>
        /// Play the metafile into a caller-supplied sink interface.
        /// </summary>
        function Stream(
            sink : ID2D1GdiMetafileSink
            ): HResult; stdcall;

        /// <summary>
        /// Gets the bounds of the metafile.
        /// </summary>
        function GetBounds(
            out bounds : PD2D1RECTF
            ): HResult; stdcall;
    end;

    // Line 709
    /// <summary>
    /// Caller-supplied implementation of an interface to receive the recorded command
    /// list.
    /// </summary>
    ID2D1CommandSink = interface(IUnknown)
        [SID_ID2D1CommandSink]

        function BeginDraw(
            ): HResult; stdcall;

        function EndDraw(
            ): HResult; stdcall;

        function SetAntialiasMode(
            antialiasMode : D2D1_ANTIALIAS_MODE
            ): HResult; stdcall;

        function SetTags(
            tag1 : D2D1_TAG;
            tag2 : D2D1_TAG
            ): HResult; stdcall;

        function SetTextAntialiasMode(
            textAntialiasMode : D2D1_TEXT_ANTIALIAS_MODE
            ): HResult; stdcall;

        function SetTextRenderingParams(
            textRenderingParams : IDWriteRenderingParams
            ): HResult; stdcall;

        function SetTransform(
            const transform : D2D1_MATRIX_3X2_F
            ): HResult; stdcall;

        function SetPrimitiveBlend(
            primitiveBlend : D2D1_PRIMITIVE_BLEND
            ): HResult; stdcall;

        function SetUnitMode(
            unitMode : D2D1_UNIT_MODE
            ): HResult; stdcall;

        function Clear(
            const color : D2D1_COLOR_F
            ): HResult; stdcall;

        function DrawGlyphRun(
            baselineOrigin            : D2D1_POINT_2F;
            const glyphRun            : DWRITE_GLYPH_RUN;
            const glyphRunDescription : DWRITE_GLYPH_RUN_DESCRIPTION;
            foregroundBrush           : ID2D1Brush;
            measuringMode             : DWRITE_MEASURING_MODE
            ): HResult; stdcall;

        function DrawLine(
            point0      : D2D1_POINT_2F;
            point1      : D2D1_POINT_2F;
            brush       : ID2D1Brush;
            strokeWidth : Single;
            strokeStyle : ID2D1StrokeStyle
            ): HResult; stdcall;

        function DrawGeometry(
            geometry    : ID2D1Geometry;
            brush       : ID2D1Brush;
            strokeWidth : Single;
            strokeStyle : ID2D1StrokeStyle
            ): HResult; stdcall;

        function DrawRectangle(
            const rect  : D2D1_RECT_F;
            brush       : ID2D1Brush;
            strokeWidth : Single;
            strokeStyle : ID2D1StrokeStyle
            ): HResult; stdcall;

        function DrawBitmap(
            bitmap                     : ID2D1Bitmap;
            const destinationRectangle : D2D1_RECT_F;
            opacity                    : Single;
            interpolationMode          : D2D1_INTERPOLATION_MODE;
            const sourceRectangle      : D2D1_RECT_F;
            const perspectiveTransform : D2D1_MATRIX_4X4_F
            ): HResult; stdcall;

        function DrawImage(
            image : ID2D1Image;
            const targetOffset   : D2D1_POINT_2F;
            const imageRectangle : D2D1_RECT_F;
            interpolationMode    : D2D1_INTERPOLATION_MODE;
            compositeMode        : D2D1_COMPOSITE_MODE
            ): HResult; stdcall;

        function DrawGdiMetafile(
            gdiMetafile        : ID2D1GdiMetafile;
            const targetOffset : D2D1_POINT_2F
            ): HResult; stdcall;

        function FillMesh(
            mesh  : ID2D1Mesh;
            brush : ID2D1Brush
            ): HResult; stdcall;

        function FillOpacityMask(
            opacityMask : ID2D1Bitmap;
            brush       : ID2D1Brush;
            const destinationRectangle : D2D1_RECT_F;
            const sourceRectangle      : D2D1_RECT_F
            ): HResult; stdcall;

        function FillGeometry(
            geometry     : ID2D1Geometry;
            brush        : ID2D1Brush;
            opacityBrush : ID2D1Brush
            ): HResult; stdcall;

        function FillRectangle(
            const rect : D2D1_RECT_F;
            brush      : ID2D1Brush
            ): HResult; stdcall;

        function PushAxisAlignedClip(
            const clipRect : D2D1_RECT_F;
            antialiasMode  : D2D1_ANTIALIAS_MODE
            ): HResult; stdcall;

        function PushLayer(
            const layerParameters1 : D2D1_LAYER_PARAMETERS1;
            layer                  : ID2D1Layer
            ): HResult; stdcall;

        function PopAxisAlignedClip(
            ): HResult; stdcall;

        function PopLayer(
            ): HResult; stdcall;
    end;

    // Line 848
    /// <summary>
    /// The commandList interface.
    /// </summary>
    ID2D1CommandList = interface(ID2D1Image)
        [SID_ID2D1CommandList]

        /// <summary>
        /// Play the command list into a caller-supplied sink interface.
        /// </summary>
        function Stream(
            sink : ID2D1CommandSink
            ): HResult; stdcall;

        /// <summary>
        /// Marks the command list as ready for use.
        /// </summary>
        function Close(
            ): HResult; stdcall;
    end;


    // Line 869
    /// <summary>
    /// Converts Direct2D primitives stored in an ID2D1CommandList into a fixed page
    /// representation. The print sub-system then consumes the primitives.
    /// </summary>
    ID2D1PrintControl = interface(IUnknown)
        [SID_ID2D1PrintControl]

        function AddPage(
            commandList : ID2D1CommandList;
            pageSize    : D2D_SIZE_F;
            pagePrintTicketStream : IStream;
            tag1                  : PD2D1TAG;
            tag2                  : PD2D1TAG
            ): HRESULT; stdcall;

        function Close(
            ): HRESULT; stdcall;
    end;


    // Line 889
    /// <summary>
    /// Provides a brush that can take any effect, command list or bitmap and
    // use it to fill a 2D shape.
    /// </summary>
    ID2D1ImageBrush = interface(ID2D1Brush)
        [SID_ID2D1ImageBrush]

        procedure SetImage(
            image : ID2D1Image
            ); stdcall;

        procedure SetExtendModeX(
            extendModeX : D2D1_EXTEND_MODE
            ); stdcall;

        procedure SetExtendModeY(
            extendModeY : D2D1_EXTEND_MODE
            ); stdcall;

        procedure SetInterpolationMode(
            interpolationMode : D2D1_INTERPOLATION_MODE
            ); stdcall;

        procedure SetSourceRectangle(
            const sourceRectangle : D2D1_RECT_F
            );

        procedure GetImage(
            out image : ID2D1Image
            ); stdcall;

        function GetExtendModeX(
            ): D2D1_EXTEND_MODE; stdcall;

        function GetExtendModeY(
            ): D2D1_EXTEND_MODE; stdcall;

        function GetInterpolationMode(
            ): D2D1_INTERPOLATION_MODE; stdcall;

        procedure GetSourceRectangle(
            out sourceRectangle : D2D1_RECT_F
            ); stdcall;
    end;

    // Line 935
    /// <summary>
    /// A bitmap brush allows a bitmap to be used to fill a geometry.  Interpolation
    /// mode is specified with D2D1_INTERPOLATION_MODE
    /// </summary>
    ID2D1BitmapBrush1 = interface(ID2D1BitmapBrush)
        [SID_ID2D1BitmapBrush1]

        /// <summary>
        /// Sets the interpolation mode used when this brush is used.
        /// </summary>
        procedure SetInterpolationMode1(
            interpolationMode : D2D1_INTERPOLATION_MODE
            ); stdcall;

        function GetInterpolationMode1(
            ): D2D1_INTERPOLATION_MODE; stdcall;
    end;


    // Line 954
    /// <summary>
    /// Extends a stroke style to allow nominal width strokes.
    /// </summary>
    ID2D1StrokeStyle1 = interface(ID2D1StrokeStyle)
        [SID_ID2D1StrokeStyle]
        function GetStrokeTransformType : D2D1_STROKE_TRANSFORM_TYPE; stdcall;
    end;

    // Line 965
    /// <summary>
    /// The ID2D1PathGeometry1 interface adds functionality to
    /// ID2D1PathGeometry. In particular, it provides the path geometry-specific
    /// ComputePointAndSegmentAtLength method.
    /// </summary>
    ID2D1PathGeometry1 = interface(ID2D1PathGeometry)
        [SID_ID2D1PathGeometry1]
    end;

    // Line 1020
    /// <summary>
    /// Represents a set of run-time bindable and discoverable properties that allow a
    /// data-driven application to modify the state of a Direct2D effect.
    /// </summary>
    ID2D1Properties = interface(IUnknown)
        [SID_ID2D1Properties]
    end;

/// <summary>
/// The effect interface. Properties control how the effect is rendered. The effect
/// is Drawn with the DrawImage call.
/// </summary>
    ID2D1Effect = interface(ID2D1Properties)
        [SID_ID2D1Effect]
    end;


    // Line 1366
    /// <summary>
    /// Represents a bitmap that can be used as a surface for an ID2D1DeviceContext or
    /// mapped into system memory, and can contain additional color context information.
    /// </summary>
    ID2D1Bitmap1 = interface(ID2D1Bitmap)
        [SID_ID2D1Bitmap1]

        /// <summary>
        /// Retrieves the color context information associated with the bitmap.
        /// </summary>
        procedure GetColorContext(
            out colorContext : ID2D1ColorContext
            );

        /// <summary>
        /// Retrieves the bitmap options used when creating the API.
        /// </summary>
        function GetOptions : D2D1_BITMAP_OPTIONS; stdcall;

        /// <summary>
        /// Retrieves the DXGI surface from the corresponding bitmap, if the bitmap was
        /// created from a device derived from a D3D device.
        /// </summary>
        function GetSurface(
            out dxgiSurface : IDXGISurface
            ): HResult; stdcall;

        /// <summary>
        /// Maps the given bitmap into memory. The bitmap must have been created with the
        /// D2D1_BITMAP_OPTIONS_CPU_READ flag.
        /// </summary>
        function Map(
            options        : D2D1_MAP_OPTIONS;
            out mappedRect : D2D1_MAPPED_RECT
            ): HResult; stdcall;

        /// <summary>
        /// Unmaps the given bitmap from memory.
        /// </summary>
        function Unmap : HResult; stdcall;
    end;

    // Line 1411
    /// <summary>
    /// Represents a color context that can be used with an ID2D1Bitmap1 object.
    /// </summary>
    ID2D1ColorContext = interface(ID2D1Resource)
        /// <summary>
        /// Retrieves the color space of the color context.
        /// </summary>
        function GetColorSpace : D2D1_COLOR_SPACE; stdcall;
        /// <summary>
        /// Retrieves the size of the color profile, in bytes.
        /// </summary>
        function eSize : UINT32; stdcall;

        /// <summary>
        /// Retrieves the color profile bytes.
        /// </summary>
        function GetProfile(
            profile     : PBYTE;
            profileSize : UINT32
            ): HResult; stdcall;
    end;

    // Line 1439
    /// <summary>
    /// Represents an collection of gradient stops that can then be the source resource
    /// for either a linear or radial gradient brush.
    /// </summary>
    ID2D1GradientStopCollection1 = interface(ID2D1GradientStopCollection)
        [SID_ID2D1GradientStopCollection1]
    end;

    // Line 1492
    /// <summary>
    /// Represents drawing state.
    /// </summary>
    ID2D1DrawingStateBlock1 = interface(ID2D1DrawingStateBlock)

    end;

    // Line 1518
    /// <summary>
    /// The device context represents a set of state and a command buffer
    /// that is used to render to a target bitmap.
    /// </summary>
    ID2D1DeviceContext = interface(ID2D1RenderTarget)
        [SID_ID2D1DeviceContext]

        /// <summary>
        /// Creates a bitmap with extended bitmap properties, potentially from a block of
        /// memory.
        /// </summary>
        function CreateBitmap(
            size                   : D2D1_SIZE_U;
            sourceData             : Pointer;
            pitch                  : UINT32;
            const bitmapProperties : D2D1_BITMAP_PROPERTIES1;
            out   bitmap           : ID2D1Bitmap1
            ): HResult; stdcall; overload;

        /// <summary>
        /// Create a D2D bitmap by copying a WIC bitmap.
        /// </summary>
        function CreateBitmapFromWicBitmap(
            wicBitmapSource  : IWICBitmapSource;
            bitmapProperties : PD2D1_BITMAP_PROPERTIES1;
            out bitmap       : ID2D1Bitmap1
            ): HResult; stdcall; overload;

        /// <summary>
        /// Creates a color context from a color space.  If the space is Custom, the context
        /// is initialized from the profile/profileSize arguments.  Otherwise the context is
        /// initialized with the profile bytes associated with the space and
        /// profile/profileSize are ignored.
        /// </summary>
        function CreateColorContext(
            space            : D2D1_COLOR_SPACE;
            profile          : PBYTE;
            profileSize      : UINT32;
            out colorContext : ID2D1ColorContext
            ): HResult; stdcall;

        function CreateColorContextFromFilename(
            filename         : PChar;
            out colorContext : ID2D1ColorContext
            ): HResult; stdcall;

        function CreateColorContextFromWicColorContext(
            wicColorContext  : IWICColorContext;
            out colorContext : ID2D1ColorContext
            ): HResult; stdcall;

        /// <summary>
        /// Creates a bitmap from a DXGI surface with a set of extended properties.
        /// </summary>
        function CreateBitmapFromDxgiSurface(
            surface                : IDXGISurface;
            const bitmapProperties : PD2D1_BITMAP_PROPERTIES1;
            out   bitmap           : ID2D1Bitmap1
            ): HResult; stdcall;

        /// <summary>
        /// Create a new effect, the effect must either be built in or previously registered
        /// through ID2D1Factory1::RegisterEffectFromStream or
        /// ID2D1Factory1::RegisterEffectFromString.
        /// </summary>
        function CreateEffect(
            effectId   : REFCLSID;
            out effect : ID2D1Effect
            ): HResult; stdcall;

        /// <summary>
        /// A gradient stop collection represents a set of stops in an ideal unit length.
        /// This is the source resource for a linear gradient and radial gradient brush.
        /// </summary>
        /// <param name="preInterpolationSpace">Specifies both the input color space and the
        /// space in which the color interpolation occurs.</param>
        /// <param name="postInterpolationSpace">Specifies the color space colors will be
        /// converted to after interpolation occurs.</param>
        /// <param name="bufferPrecision">Specifies the precision in which the gradient
        /// buffer will be held.</param>
        /// <param name="extendMode">Specifies how the gradient will be extended outside of
        /// the unit length.</param>
        /// <param name="colorInterpolationMode">Determines if colors will be interpolated
        /// in straight alpha or premultiplied alpha space.</param>
        function CreateGradientStopCollection(
            straightAlphaGradientStops      : D2D1_GRADIENT_STOP;
            straightAlphaGradientStopsCount : UINT32;
            preInterpolationSpace           : D2D1_COLOR_SPACE;
            postInterpolationSpace          : D2D1_COLOR_SPACE;
            bufferPrecision                 : D2D1_BUFFER_PRECISION;
            extendMode                      : D2D1_EXTEND_MODE;
            colorInterpolationMode          : D2D1_COLOR_INTERPOLATION_MODE;
            out gradientStopCollection1     : ID2D1GradientStopCollection1
            ): HResult; stdcall; overload;

        /// <summary>
        /// Creates an image brush, the input image can be any type of image, including a
        /// bitmap, effect and a command list.
        /// </summary>
        function CreateImageBrush(
            image                : ID2D1Image;
            imageBrushProperties : PD2D1_IMAGE_BRUSH_PROPERTIES;
            brushProperties      : PD2D1BrushProperties;
            out imageBrush       : ID2D1ImageBrush
            ): HResult; stdcall; overload;

        function CreateBitmapBrush(
            bitmap                      : ID2D1Bitmap;
            bitmapBrushProperties       : PD2D1BitmapBrushProperties1;
            brushProperties             : PD2D1BrushProperties;
            out   bitmapBrush           : ID2D1BitmapBrush1
            ): HResult; stdcall; overload;

        /// <summary>
        /// Creates a new command list.
        /// </summary>
        function CreateCommandList(
            out commandList : ID2D1CommandList
            ): HResult; stdcall;

        /// <summary>
        /// Indicates whether the format is supported by D2D.
        /// </summary>
        function IsDxgiFormatSupported(
            format : DXGI_FORMAT
            ): BOOL; stdcall;

        /// <summary>
        /// Indicates whether the buffer precision is supported by D2D.
        /// </summary>
        function IsBufferPrecisionSupported(
            bufferPrecision : D2D1_BUFFER_PRECISION
            ): BOOL; stdcall;

        /// <summary>
        /// This retrieves the local-space bounds in DIPs of the current
        /// image using the device context DPI.
        /// </summary>
        function GetImageLocalBounds(
            image           : ID2D1Image;
            out localBounds : D2D1_RECT_F
            ): HResult; stdcall;

        /// <summary>
        /// This retrieves the world-space bounds in DIPs of the current
        /// image using the device context DPI.
        /// </summary>
        function GetImageWorldBounds(
            image           : ID2D1Image;
            out worldBounds : D2D1_RECT_F
            ): HResult; stdcall;

        /// <summary>
        /// Retrieves the world-space bounds in DIPs of the glyph run using
        /// the device context DPI.
        /// </summary>
        function GetGlyphRunWorldBounds(
            baselineOrigin : D2D1_POINT_2F;
            const glyphRun : DWRITE_GLYPH_RUN;
            measuringMode  : DWRITE_MEASURING_MODE;
            out bounds     : D2D1_RECT_F
            ): HResult; stdcall;

        /// <summary>
        /// Retrieves the device associated with this device context.
        /// </summary>
        procedure GetDevice(
            out device : ID2D1Device
            ); stdcall;

        /// <summary>
        /// Sets the target for this device context to point to the given image. The image
        /// can be a command list or a bitmap created with the D2D1_BITMAP_OPTIONS_TARGET
        /// flag.
        /// </summary>
        procedure SetTarget(
            image : ID2D1Image
            ); stdcall;

        /// <summary>
        /// Gets the target that this device context is currently pointing to.
        /// </summary>
        procedure GetTarget(
            out image : ID2D1Image
            ); stdcall;

        /// <summary>
        /// Sets tuning parameters for internal rendering inside the device context.
        /// </summary>
        procedure SetRenderingControls(
            const renderingControls : D2D1_RENDERING_CONTROLS
            ); stdcall;

        /// <summary>
        /// This retrieves the rendering controls currently selected into the device
        /// context.
        /// </summary>
        procedure GetRenderingControls(
            out renderingControls : D2D1_RENDERING_CONTROLS
            ); stdcall;

        /// <summary>
        /// Changes the primitive blending mode for all of the rendering operations.
        /// </summary>
        procedure SetPrimitiveBlend(
            primitiveBlend : D2D1_PRIMITIVE_BLEND
            ); stdcall;

        /// <summary>
        /// Returns the primitive blend currently selected into the device context.
        /// </summary>
        function GetPrimitiveBlend(
            ): D2D1_PRIMITIVE_BLEND; stdcall;

        /// <summary>
        /// Changes the units used for all of the rendering operations.
        /// </summary>
        procedure SetUnitMode(
            unitMode : D2D1_UNIT_MODE
            ); stdcall;

        /// <summary>
        /// Returns the unit mode currently set on the device context.
        /// </summary>
        function GetUnitMode(
            ): D2D1_UNIT_MODE; stdcall;

        /// <summary>
        /// Draws the glyph run with an extended description to describe the glyphs.
        /// </summary>
        procedure DrawGlyphRun(
            baselineOrigin            : D2D1_POINT_2F;
            const glyphRun            : DWRITE_GLYPH_RUN;
            const glyphRunDescription : DWRITE_GLYPH_RUN_DESCRIPTION;
            foregroundBrush           : ID2D1Brush;
            measuringMode             : DWRITE_MEASURING_MODE = DWRITE_MEASURING_MODE_NATURAL
            ); stdcall; overload;

//        procedure DrawGlyphRun(
//            baselineOrigin        : D2D1_POINT_2F;
//            var glyphRun          : TDWriteGlyphRun;
//            const foregroundBrush : ID2D1Brush;
//            measuringMode         : TDWriteMeasuringMode = DWRITE_MEASURING_MODE_NATURAL
//        ); stdcall; overload;

        /// <summary>
        /// Draw an image to the device context. The image represents either a concrete
        /// bitmap or the output of an effect graph.
        /// </summary>
        procedure DrawImage(
            image             : ID2D1Image;
            targetOffset      : PD2D1_POINT_2F = nil;
            imageRectangle    : PD2D1_RECT_F = nil;
            interpolationMode : D2D1_INTERPOLATION_MODE = D2D1_INTERPOLATION_MODE_LINEAR;
            compositeMode     : D2D1_COMPOSITE_MODE = D2D1_COMPOSITE_MODE_SOURCE_OVER
            ); stdcall;

        /// <summary>
        /// Draw a metafile to the device context.
        /// </summary>
        procedure DrawGdiMetafile(
            gdiMetafile  : ID2D1GdiMetafile;
            targetOffset : PD2D1_POINT_2F = nil
            ); stdcall;

        procedure DrawBitmap(
            bitmap                     : ID2D1Bitmap;
            const destinationRectangle : D2D1_RECT_F;
            opacity                    : Single;
            interpolationMode          : D2D1_INTERPOLATION_MODE;
            sourceRectangle            : PD2D1_RECT_F = nil;
            perspectiveTransform       : PD2D1_MATRIX_4X4_F = nil
            ); stdcall; overload;

//        procedure DrawBitmap(
//            const bitmap         : ID2D1Bitmap;
//            destinationRectangle : PD2D1RectF = nil;
//            opacity              : Single = 1.0;
//            interpolationMode    : TD2D1BitmapInterpolationMode = D2D1_BITMAP_INTERPOLATION_MODE_LINEAR;
//            sourceRectangle      : PD2D1RectF = nil
//        ); stdcall; overload;

        /// <summary>
        /// Push a layer on the device context.
        /// </summary>
        procedure PushLayer(
            var   layerParameters : D2D1_LAYER_PARAMETERS1;
            layer                 : ID2D1Layer
            ); stdcall; overload;

        /// <summary>
        // Start a layer of drawing calls. The way in which the layer must be resolved is
        // specified first as well as the logical resource that stores the layer
        // parameters. The supplied layer resource might grow if the specified content
        // cannot fit inside it. The layer will grow monitonically on each axis.
        /// </summary>
//        procedure PushLayer(
//            var   layerParameters : D2D1_LAYER_PARAMETERS;
//            const layer           : ID2D1Layer
//            ); stdcall; overload;

        /// <summary>
        /// This indicates that a portion of an effect's input is invalid. This method can
        /// be called many times.
        /// </summary>
        function InvalidateEffectInputRectangle(
            effect               : ID2D1Effect;
            input                : UINT32;
            const inputRectangle : D2D1_RECT_F
            ): HResult; stdcall;

        /// <summary>
        /// Gets the number of invalid ouptut rectangles that have accumulated at the
        /// effect.
        /// </summary>
        function GetEffectInvalidRectangleCount(
            effect             : ID2D1Effect;
            out rectangleCount : UINT32
            ): HResult; stdcall;

        /// <summary>
        /// Gets the invalid rectangles that are at the output of the effect.
        /// </summary>
        function GetEffectInvalidRectangles(
            effect          : ID2D1Effect;
            out rectangles  : D2D1_RECT_F;
            rectanglesCount : UINT32
            ): HResult; stdcall;

        /// <summary>
        /// Gets the maximum region of each specified input which would be used during a
        /// subsequent rendering operation
        /// </summary>
        function GetEffectRequiredInputRectangles(
            renderEffect               : ID2D1Effect;
            const renderImageRectangle : D2D1_RECT_F;
            const inputDescriptions    : D2D1_EFFECT_INPUT_DESCRIPTION;
            out   requiredInputRects   : D2D1_RECT_F;
            inputCount                 : UINT32
            ): HResult; stdcall;

        /// <summary>
        /// Fill using the alpha channel of the supplied opacity mask bitmap. The brush
        /// opacity will be modulated by the mask. The render target antialiasing mode must
        /// be set to aliased.
        /// </summary>
        procedure FillOpacityMask(
            opacityMask          : ID2D1Bitmap;
            brush                : ID2D1Brush;
            destinationRectangle : PD2D1_RECT_F = nil;
            sourceRectangle      : PD2D1_RECT_F = nil
            ); stdcall; overload;

        /// <summary>
        // Fill using the opacity channel of the supplied bitmap as a mask. The alpha
        // channel of the bitmap is used to represent the coverage of the geometry at each
        // pixel, and this is filled appropriately with the brush. The render target
        // antialiasing mode must be set to aliased.
        /// </summary>
//        procedure FillOpacityMask(
//            opacityMask          : ID2D1Bitmap;
//            brush                : ID2D1Brush;
//            content              : TD2D1OpacityMaskContent;
//            destinationRectangle : PD2D1RectF = nil;
//            sourceRectangle      : PD2D1RectF = nil
//            ); stdcall; overload;
    end;

    // Line 2160
    /// <summary>
    /// The device defines a resource domain whose objects and device contexts
    /// can be used together.
    /// </summary>
    ID2D1Device  = interface(ID2D1Resource)
        [SID_ID2D1Device]
        /// <summary>
        /// Creates a new device context with no initially assigned target.
        /// </summary>
        function CreateDeviceContext(
            options           : D2D1_DEVICE_CONTEXT_OPTIONS;
            out deviceContext : ID2D1DeviceContext
            ): HRESULT; stdcall;

        /// <summary>
        /// Creates a D2D print control.
        /// </summary>
        function CreatePrintControl(
            wicFactory             : IWICImagingFactory;
            documentTarget         : IPrintDocumentPackageTarget;
            printControlProperties : PD2D1_PRINT_CONTROL_PROPERTIES;
            out printControl       : ID2D1PrintControl
            ): HRESULT; stdcall;

        /// <summary>
        /// Sets the maximum amount of texture memory to maintain before
        /// evicting caches.
        /// </summary>
        procedure SetMaximumTextureMemory(
            maximumInBytes : UINT64
            ); stdcall;

        /// <summary>
        /// Gets the maximum amount of texture memory to maintain before
        /// evicting caches.
        /// </summary>
        function GetMaximumTextureMemory(
            ): UINT64; stdcall;

        /// <summary>
        /// Clears all resources that are cached but not held in use by the
        /// application through an interface reference.
        /// </summary>
        procedure ClearResources(
            millisecondsSinceUse : UINT32
            ); stdcall;
    end;

    // Line 2220
    /// <summary>
    /// Creates Direct2D resources.
    /// </summary>
    ID2D1Factory1 = interface(ID2D1Factory)
        [SID_ID2D1Factory1]

        /// <summary>
        /// This creates a new Direct2D device from the given IDXGIDevice.
        /// </summary>
        function CreateDevice(dxgiDevice    : IDXGIDevice;
                              out d2dDevice : ID2D1Device): HResult; stdcall;

        /// <summary>
        /// This creates a stroke style with the ability to preserve stroke width in various
        /// ways.
        /// </summary>
        function CreateStrokeStyle(
            const strokeStyleProperties : TD2D1StrokeStyleProperties1;
            const dashes                : PSingle;
            dashesCount                 : UINT32;
            out strokeStyle             : ID2D1StrokeStyle1
            ): HResult; stdcall; overload;

        /// <summary>
        /// Allows a non-default stroke style to be specified for a given
        /// geometry at draw time.
        /// </summary>
        function CreateStrokeStyle(
            const strokeStyleProperties : TD2D1StrokeStyleProperties;
            const dashes                : PSingle;
            dashesCount                 : UINT32;
            out strokeStyle             : ID2D1StrokeStyle
            ): HResult; stdcall; overload;

        /// <summary>
        /// Creates a path geometry with new operational methods.
        /// </summary>
        function CreatePathGeometry(
            out pathGeometry : ID2D1PathGeometry1
            ): HResult; stdcall; overload;

        /// <summary>
        /// Returns an initially empty path geometry interface. A geometry
        /// sink is created off the interface to populate it.
        /// </summary>
        function CreatePathGeometry(
            out pathGeometry: ID2D1PathGeometry
            ): HResult; stdcall; overload;

        /// <summary>
        /// Creates a new drawing state block, this can be used in subsequent
        /// SaveDrawingState and RestoreDrawingState operations on the render target.
        /// </summary>
        function CreateDrawingStateBlock(
            const drawingStateDescription : PD2D1DrawingStateDescription1;
            const textRenderingParams     : IDWriteRenderingParams;
            out   drawingStateBlock       : ID2D1DrawingStateBlock1
            ): HResult; stdcall; overload;

        /// <summary>
        /// Creates a new drawing state block, this can be used in subsequent
        /// SaveDrawingState and RestoreDrawingState operations on the
        /// render target.
        /// </summary>
        function CreateDrawingStateBlock(
             drawingStateDescription   : PD2D1DrawingStateDescription;
             const textRenderingParams : IDWriteRenderingParams;
             out   drawingStateBlock   : ID2D1DrawingStateBlock
          ): HResult; stdcall; overload;

        /// <summary>
        /// Creates a new GDI metafile.
        /// </summary>
        function CreateGdiMetafile(
            metafileStream : IStream;
            out metafile   : ID2D1GdiMetafile
            ): HResult; stdcall; overload;

        /// <summary>
        /// This globally registers the given effect. The effect can later be instantiated
        /// by using the registered class id. The effect registration is reference counted.
        /// </summary>
        function RegisterEffectFromStream(
            classId             : REFCLSID;
            propertyXml         : IStream;
            bindings            : PD2D1PropertyBinding;
            bindingsCount       : UINT32;
            effectFactory       : PD2D1_EFFECT_FACTORY
            ): HResult; stdcall;

        /// <summary>
        /// This globally registers the given effect. The effect can later be instantiated
        /// by using the registered class id. The effect registration is reference counted.
        /// </summary>
        function RegisterEffectFromString(
            classId       : REFCLSID;
            propertyXml   : PChar;
            bindings      : PD2D1_PROPERTY_BINDING;
            bindingsCount : UINT32;
            effectFactory : PD2D1_EFFECT_FACTORY
            ): HResult; stdcall;

        /// <summary>
        /// This unregisters the given effect by its class id, you need to call
        /// UnregisterEffect for every call to ID2D1Factory1::RegisterEffectFromStream and
        /// ID2D1Factory1::RegisterEffectFromString to completely unregister it.
        /// </summary>
        function UnregisterEffect(
            classId : REFCLSID
            ): HResult; stdcall;

        /// <summary>
        /// This returns all of the registered effects in the process, including any
        /// built-in effects.
        /// </summary>
        /// <param name="effectsReturned">The number of effects returned into the passed in
        /// effects array.</param>
        /// <param name="effectsRegistered">The number of effects currently registered in
        /// the system.</param>
        function GetRegisteredEffects(
            effects               : PCLSID;
            effectsCount          : UINT32;
            out effectsReturned   : UINT32;
            out effectsRegistered : UINT32
            ): HResult; stdcall;

        /// <summary>
        /// This retrieves the effect properties for the given effect, all of the effect
        /// properties will be set to a default value since an effect is not instantiated to
        /// implement the returned property interface.
        /// </summary>
        function GetEffectProperties(
            effectId       : REFCLSID;
            out properties : ID2D1Properties
            ): HResult; stdcall;
    end;


implementation

end.
