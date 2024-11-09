{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Description:  Direct2D 1.1 translation for Delphi
              File: D2D1EffectAuthor.h
              Copyright (c) Microsoft Corporation.
              All Rights Reserved.

              Translator: François PIETTE @ OverByte
Creation:     Aug 10, 2020
License:      This code is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Winapi.D2D1EffectAuthor;

interface

uses
    Winapi.Windows;

type
    // Line 20
    /// <summary>
    /// Function pointer that sets a property on an effect.
    /// </summary>
    PD2D1_PROPERTY_SET_FUNCTION = function (
        effect     : IUnknown;
        data       : PBYTE;
        dataSize   : UINT32
        ) : HResult; stdcall;

    // Line 29
    /// <summary>
    /// Function pointer that gets a property from an effect.
    /// </summary>
    PD2D1_PROPERTY_GET_FUNCTION = function (
        const effect   : IUnknown;
        data           : PBYTE;
        dataSize       : UINT32;
        actualSize     : PUINT32
        ): HResult; stdcall;

    // Line 248
    /// <summary>
    /// Defines a property binding to a function. The name must match the property
    /// defined in the registration schema.
    /// </summary>
    D2D1_PROPERTY_BINDING = record

        /// <summary>
        /// The name of the property.
        /// </summary>
        propertyName : PChar;

        /// <summary>
        /// The function that will receive the data to set.
        /// </summary>
        setFunction : PD2D1_PROPERTY_SET_FUNCTION;

        /// <summary>
        /// The function that will be asked to write the output data.
        /// </summary>
        getFunction : PD2D1_PROPERTY_GET_FUNCTION;

    end;
    PD2D1_PROPERTY_BINDING = ^D2D1_PROPERTY_BINDING;
    TD2D1PropertyBinding   = D2D1_PROPERTY_BINDING;
    PD2D1PropertyBinding   = ^TD2D1PropertyBinding;


implementation

end.
