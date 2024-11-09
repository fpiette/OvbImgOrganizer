{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Description:  Public API definitions shared by DWrite, D2D, and DImage
              translation for Delphi.
              File: dcommon.h
              Copyright (c) Microsoft Corporation.
              All Rights Reserved.

              Translator: François PIETTE @ OverByte
Creation:     Aug 12, 2020
License:      This code is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Winapi.DCommon;

interface

type
    // Line 347
    /// <summary>
    /// Represents a 4-by-4 matrix.
    /// </summary>
    D2D_MATRIX_4X4_F = record
        case Boolean of
        TRUE: (
                _11, _12, _13, _14 : Single;
                _21, _22, _23, _24 : Single;
                _31, _32, _33, _34 : Single;
                _41, _42, _43, _44 : Single;
            );
        FALSE: (
            m : array [0..3, 0..3] of Single;
            )
    end;

    // Line 172
    /// <summary>
    /// Represents an x-coordinate and y-coordinate pair in two-dimensional space.
    /// </summary>
    D2D_POINT_2F = record
        x : Single;
        y : Single;
    end;

    // Line 220
    /// <summary>
    /// Represents a rectangle defined by the coordinates of the upper-left corner
    /// (left, top) and the coordinates of the lower-right corner (right, bottom).
    /// </summary>
    D2D_RECT_F = record
        left   : Single;
        top    : Single;
        right  : Single;
        bottom : Single;
    end;

    // Line 390
    D2D1_POINT_2F  = D2D_POINT_2F;
    PD2D1_POINT_2F = ^D2D1_POINT_2F;

    // Line 393
    D2D1_RECT_F    = D2D_RECT_F;
    PD2D1_RECT_F   = ^D2D1_RECT_F;

implementation

end.
