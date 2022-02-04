{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Description:  DWrite 1.1 translation for Delphi
              File: DWrite_1.h
              Copyright (c) Microsoft Corporation.
              All Rights Reserved.

              Translator: François PIETTE @ OverByte
Creation:     Aug 15, 2020
License:      This code is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Winapi.DWrite_1;

interface

uses
    Winapi.Windows,
    Winapi.D2D1;

const
    SID_IDWriteFactory1              = '{30572f99-dac6-41db-a16e-0486307e606a}';
    SID_IDWriteRenderingParams1      = '{94413cf4-a6fc-4248-8b50-6674348fcad3}';

    IID_IDWriteFactory1              : TGUID = SID_IDWriteFactory1;
    IID_IDWriteRenderingParams1      : TGUID = SID_IDWriteRenderingParams1;

type
    IDWriteRenderingParams1 = interface;

    // Line 1015
    /// <summary>
    /// The root factory interface for all DWrite objects.
    /// </summary>
    IDWriteFactory1 = interface(IDWriteFactory)
        [SID_IDWriteFactory1]
        /// <summary>
        /// Gets a font collection representing the set of end-user defined
        /// custom fonts.
        /// </summary>
        /// <param name="fontCollection">Receives a pointer to the EUDC font
        ///     collection object, or NULL in case of failure.</param>
        /// <param name="checkForUpdates">If this parameter is nonzero, the
        ///     function performs an immediate check for changes to the set of
        ///     EUDC fonts. If this parameter is FALSE, the function will still
        ///     detect changes, but there may be some latency. For example, an
        ///     application might specify TRUE if it has itself just modified a
        ///     font and wants to be sure the font collection contains that font.
        ///     </param>
        /// <returns>
        /// Standard HRESULT error code. Note that if no EUDC is set on the system,
        /// the returned collection will be empty, meaning it will return success
        /// but GetFontFamilyCount will be zero.
        /// </returns>
        /// <remarks>
        /// Querying via IDWriteFontCollection::FindFamilyName for a specific
        /// family (like MS Gothic) will return the matching family-specific EUDC
        /// font if one exists. Querying for "" will return the global EUDC font.
        /// For example, if you were matching an EUDC character within a run of
        /// the base font PMingLiu, you would retrieve the corresponding EUDC font
        /// face using GetEudcFontCollection, then FindFamilyName with "PMingLiu",
        /// followed by GetFontFamily and CreateFontFace.
        ///
        /// Be aware that eudcedit.exe can create placeholder empty glyphs that
        /// have zero advance width and no glyph outline. Although they are present
        /// in the font (HasCharacter returns true), you are best to ignore
        /// these and continue on with font fallback in your layout if the metrics
        /// for the glyph are zero.
        /// </remarks>
        function GetEudcFontCollection(
            out fontCollection : IDWriteFontCollection;
            checkForUpdates    : BOOL = FALSE
            ): HResult; stdcall;

        /// <summary>
        /// Creates a rendering parameters object with the specified properties.
        /// </summary>
        /// <param name="gamma">The gamma value used for gamma correction, which must be greater than zero and cannot exceed 256.</param>
        /// <param name="enhancedContrast">The amount of contrast enhancement, zero or greater.</param>
        /// <param name="enhancedContrastGrayscale">The amount of contrast enhancement to use for grayscale antialiasing, zero or greater.</param>
        /// <param name="clearTypeLevel">The degree of ClearType level, from 0.0f (no ClearType) to 1.0f (full ClearType).</param>
        /// <param name="pixelGeometry">The geometry of a device pixel.</param>
        /// <param name="renderingMode">Method of rendering glyphs. In most cases, this should be DWRITE_RENDERING_MODE_DEFAULT to automatically use an appropriate mode.</param>
        /// <param name="renderingParams">Holds the newly created rendering parameters object, or NULL in case of failure.</param>
        /// <returns>
        /// Standard HRESULT error code.
        /// </returns>
        function CreateCustomRenderingParams(
            gamma                     : Single;
            enhancedContrast          : Single;
            enhancedContrastGrayscale : Single;
            clearTypeLevel            : Single;
            pixelGeometry             : DWRITE_PIXEL_GEOMETRY;
            renderingMode             : DWRITE_RENDERING_MODE;
            out renderingParams       : IDWriteRenderingParams1
            ): HResult; stdcall; overload;

        function CreateCustomRenderingParams(
            gamma                     : Single;
            enhancedContrast          : Single;
            clearTypeLevel            : Single;
            pixelGeometry             : DWRITE_PIXEL_GEOMETRY;
            renderingMode             : DWRITE_RENDERING_MODE;
            out renderingParams       : IDWriteRenderingParams
        ): HResult; stdcall; overload;
    end;

    // Line 1407
    /// <summary>
    /// The interface that represents text rendering settings for glyph rasterization and filtering.
    /// </summary>
    IDWriteRenderingParams1 = interface(IDWriteRenderingParams)
        [SID_IDWriteRenderingParams1]
        /// <summary>
        /// Gets the amount of contrast enhancement to use for grayscale antialiasing.
        /// Valid values are greater than or equal to zero.
        /// </summary>
        function GetGrayscaleEnhancedContrast(
        ): Single; stdcall;
    end;


implementation

end.
