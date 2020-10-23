{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  Author:       François PIETTE
  Creation:     Feb 11, 2012
  Description:  Event driven object to implement Drag&Drop files from other
                applications such as Windows Explorer.
                Based on code given by David Heffernan when answering a
                question on StackOverflow:
                http://stackoverflow.com/questions/4354071
  Version:      1.00
  History:


  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
unit DropHandler;

interface

uses
    Windows, Types, Classes, ActiveX;

type
    TStringArray      = array of String;

    TDropAllowedEvent = procedure (Sender            : TObject;
                                   const FileNames   : array of String;
                                   const grfKeyState : Longint;
                                   const pt          : TPoint;
                                   var   Allowed     : Boolean)
                                 of object;
    TDragOverEvent    = procedure (Sender            : TObject;
                                   const grfKeyState : Longint;
                                   const pt          : TPoint;
                                   var   Allowed     : Boolean)
                                 of object;
    TDropEvent        =  procedure (Sender           : TObject;
                                    const DropPoint  : TPoint;
                                    const FileNames  : array of String)
                                 of object;

    TDropTarget = class(TObject, IDropTarget)
    private
        FRegisteredHandle : HWND;
        FDropAllowed      : Boolean;
        FOnDropAllowed    : TDropAllowedEvent;
        FOnDrop           : TDropEvent;
        FOnDragOver       : TDragOverEvent;
        FOnDragLeave      : TNotifyEvent;
        procedure GetFileNames(const dataObj : IDataObject;
                               var FileNames : TStringArray);
        function  DragEnter(const dataObj : IDataObject;
                            grfKeyState   : Integer;
                            pt            : TPoint;
                            var dwEffect  : Integer): HResult; stdcall;
        function  DragOver(grfKeyState  : Longint;
                           pt           : TPoint;
                           var dwEffect : Longint): HResult; stdcall;
        function  DragLeave: HResult; stdcall;
        function  Drop(const dataObj : IDataObject;
                       grfKeyState   : Longint;
                       pt            : TPoint;
                       var dwEffect  : Longint): HResult; stdcall;
        function _AddRef: Integer; stdcall;
        function _Release: Integer;  stdcall;
        function QueryInterface(const IID: TGUID; out Obj): HResult;  stdcall;
    public
        destructor  Destroy; override;
        // Call Register() with a window handle so that that window starts
        // accepting dropped files. Events will then be generated.
        function    Register(AHandle : HWnd) : HResult;
        // Stop accepting files dropped on the registered window.
        procedure   Revoke;
        // DropAllowed event is called once when the dragged files are
        // entering the area of the registered window.
        // The event handler must set the Allowed var argument to TRUE if
        // dropping the file(s) is allowed at the given point
        property OnDropAllowed : TDropAllowedEvent read  FOnDropAllowed
                                                   write FOnDropAllowed;
        // DragOver event is called as mouse move above the registered window
        // The event handler must set the Allowed var argument to TRUE if
        // dropping the file(s) is allowed at the given point
        property OnDragOver    : TDragOverEvent    read  FOnDragOver
                                                   write FOnDragOver;
        // Drop event is called when the user drops the files.
        property OnDrop        : TDropEvent        read  FOnDrop
                                                   write FOnDrop;
        // DragLeave event is called when the dragged files leave the
        // registered window area.
        property OnDragLeave   : TNotifyEvent      read  FOnDragLeave
                                                   write FOnDragLeave;
    end;

implementation

uses
    ShellAPI;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDropTarget.Register(AHandle: HWnd): HResult;
begin
    if FRegisteredHandle = AHandle then begin
        Result := S_OK;
        Exit;
    end;
    if FRegisteredHandle <> 0 then
        Revoke;
    FRegisteredHandle := AHandle;
    Result  := ActiveX.RegisterDragDrop(FRegisteredHandle, Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDropTarget.Revoke;
begin
    if FRegisteredHandle <> 0 then begin
        ActiveX.RevokeDragDrop(FRegisteredHandle);
        FRegisteredHandle := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TDropTarget.Destroy;
begin
    Revoke;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDropTarget.GetFileNames(
    const dataObj : IDataObject;
    var FileNames : TStringArray);
var
    I           : Integer;
    FormatetcIn : TFormatEtc;
    Medium      : TStgMedium;
    DropHandle  : HDROP;
begin
    FileNames            := nil;
    FormatetcIn.cfFormat := CF_HDROP;
    FormatetcIn.ptd      := nil;
    FormatetcIn.dwAspect := DVASPECT_CONTENT;
    FormatetcIn.lindex   := -1;
    FormatetcIn.tymed    := TYMED_HGLOBAL;
    if dataObj.GetData(FormatetcIn, Medium) = S_OK then begin
        DropHandle := HDROP(Medium.hGlobal);
        SetLength(FileNames, DragQueryFile(DropHandle, $FFFFFFFF, nil, 0));
        for I := 0 to high(FileNames) do begin
            SetLength(FileNames[I], DragQueryFile(DropHandle, I, nil, 0));
            DragQueryFile(DropHandle, I, @FileNames[I][1],
                          Length(FileNames[I]) + 1);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDropTarget.DragEnter(
    const dataObj : IDataObject;
    grfKeyState   : Integer;
    pt            : TPoint;
    var dwEffect  : Integer): HResult;
var
    FileNames: TStringArray;
begin
    Result := S_OK;
    try
        GetFileNames(dataObj, FileNames);
        if (Length(FileNames) > 0) and Assigned(FOnDropAllowed) then begin
            FDropAllowed := FALSE;
            FOnDropAllowed(Self, FileNames, grfKeyState, pt, FDropAllowed);
        end;
        if FDropAllowed then
            dwEffect := DROPEFFECT_COPY
        else
            dwEffect := DROPEFFECT_NONE;
    except
        Result := E_UNEXPECTED;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDropTarget.DragLeave: HResult;
begin
    if Assigned(FOnDragLeave) then
        FOnDragLeave(Self);
    Result := S_OK;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDropTarget.DragOver(
    grfKeyState  : Integer;
    pt           : TPoint;
    var dwEffect : Integer): HResult;
begin
    Result := S_OK;
    try
        if Assigned(FOnDragOver) then
            FOnDragOver(Self, grfKeyState, pt, FDropAllowed);
        if FDropAllowed then
            dwEffect := DROPEFFECT_COPY
        else
            dwEffect := DROPEFFECT_NONE;
    except
        Result := E_UNEXPECTED;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDropTarget.Drop(
    const dataObj : IDataObject;
    grfKeyState   : Integer;
    pt            : TPoint;
    var dwEffect  : Integer): HResult;
var
    FileNames: TStringArray;
begin
    Result := S_OK;
    try
        GetFileNames(dataObj, FileNames);
        if (Length(FileNames) > 0) and Assigned(FOnDrop) then
            FOnDrop(Self, Pt, FileNames);
    except
        // Silently ignore any exception bacsue if required, they should
        // be handled in OnDrop event handler.
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDropTarget.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
    if GetInterface(IID, Obj) then
        Result := 0
    else
        Result := E_NOINTERFACE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDropTarget._AddRef: Integer;
begin
    // We don't use reference counting in this object
    // We need _AddRef because RegisterDragDrop API call it
    Result := 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDropTarget._Release: Integer;
begin
    // We don't use reference counting in this object
    // We need _Release because RevokeDragDrop API call it
    Result := 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

