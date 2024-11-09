{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Description:  Direct2D 1.1 translation for Delphi
              File: DocumentTarget.h
              Copyright (c) Microsoft Corporation.
              All Rights Reserved.

              Translator: François PIETTE @ OverByte
Creation:     Aug 21, 2020
License:      This code is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Winapi.DocumentTarget;

interface

const
    SID_IPrintDocumentPackageTarget  = '{1b8efec4-3019-4c27-964e-367202156906}';

    IID_IPrintDocumentPackageTarget  : TGUID = SID_IPrintDocumentPackageTarget;

type
    REFGUID = TGUID;
    REFIID  = TGUID;

    // Line 146
    IPrintDocumentPackageTarget = interface(IUnknown)
        [SID_IPrintDocumentPackageTarget]

        function GetPackageTargetTypes(
            out targetCount : UINT32;
            out targetTypes : PGUID         // Array of GUIDs
            ): HRESULT; stdcall;

        function GetPackageTarget(
            guidTargetType  : REFGUID;
            riid            : REFIID;
            const ppvTarget : Pointer
            ): HRESULT; stdcall;

        function Cancel(
            ): HRESULT; stdcall;

    end;


implementation

end.
