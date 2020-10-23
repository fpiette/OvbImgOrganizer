{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE @ Balteau-NDT
Creation:     May 17, 2020
Description:  Simple Image Organizer - Data types
License:      This program is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OvbImgOrganizer.DataTypes;

interface

uses
    System.Types, System.Classes,
    Generics.Collections;

type
    TOvbImgDisplayEvent         = procedure (Sender         : TObject;
                                             const Msg      : String)
                                             of object;
    TOvbImgAskYesOrNoEvent      = procedure (Sender         : TObject;
                                             const Question : String;
                                             const Caption  : String;
                                             var   Answer   : Integer)
                                             of object;
    TOvbImgTranslateStringEvent = procedure (Sender         : TObject;
                                             const Msg      : String;
                                             var   Xlated   : String)
                                             of object;
    TOvbImgProgressProc         = procedure (Sender         : TObject;
                                             Context        : Integer;
                                             Count          : Integer;
                                             var Abort      : Boolean)
                                             of object;

    TOvbImgListOfInteger = class(TList<Integer>);

    TOvbImgTagObject = class
    public
        TagName   : String;
        ID        : Integer;
        ParentID  : Integer;
        Loaded    : Boolean;
        OnDisplay : TOvbImgDisplayEvent;
        constructor Create(DisplayHandler : TOvbImgDisplayEvent;
                           TagID          : Integer);
        destructor  Destroy; override;
        procedure Display(const Msg : String);
    end;
    TOvbImgTagList = class(TObjectList<TOvbImgTagObject>);

    TOvbImgDataRecord = record
        Index         : Integer;
        ImageID       : Integer;
        ImageThumb    : String;
        ThumbFileName : String;
        ImageName     : String;
        ImagePath     : String;
        ImageDateTime : TDateTime;
        IndexDateTime : TDateTime;
        Description   : String;
        Latitude      : Double;
        Longitude     : Double;
        Latitude2     : Double;
        Longitude2    : Double;
    end;

    TOvbImgDataRecordEvent = procedure (Sender           : TObject;
                                        var   Index      : Integer;
                                        const DataRecord : TOvbImgDataRecord)
                                        of object;

    // Used to store additional information for each loaded image
    TOvbImgImageInfo = class
    private
        FTagNames : TStrings;
        procedure SetTagNames(const Value: TStrings);
    public
        DropImagePanel : TObject; // TOvbImagePanel;
        ImageName      : String;             // Include extension
        ImagePath      : String;
        ImageThumb     : String;             // Partial path to thumbnail
        ImageID        : Integer;
        ImageDateTime  : TDateTime;
        IndexDateTime  : TDateTime;
        Description    : String;
        Latitude       : Double;
        Longitude      : Double;
        Latitude2      : Double;
        Longitude2     : Double;
        TagFetched     : Boolean;
        constructor Create;
        destructor  Destroy; override;
        procedure Clear;
        property TagNames        : TStrings         read  FTagNames
                                                    write SetTagNames;
    end;

    // Record for DBInsertImage
    TOvbImgImageDesc = record
        FileName      : String;
        ThumbFilename : String;
        Description   : String;
        Latitude      : Double;
        Longitude     : Double;
        Latitude2     : Double;
        Longitude2    : Double;
        procedure Clear;
    end;

implementation

uses
    System.SysUtils;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TOvbImgTagObject }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TOvbImgTagObject.Create(
    DisplayHandler : TOvbImgDisplayEvent;
    TagID          : Integer);
begin
    inherited Create;
    OnDisplay := DisplayHandler;
    Loaded    := FALSE;
    ID        := TagID;
//    Display('TOvbImgTagObject.Create  ID=' + IntToStr(TagID) + '  Addr=0x' + IntToHex(UIntPtr(Self), 8));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TOvbImgTagObject.Destroy;
begin
//    Display('TOvbImgTagObject.Destroy ID=' + IntToStr(ID) + '  Addr=0x' + IntToHex(UIntPtr(Self), 8));
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgTagObject.Display(const Msg: String);
begin
    if Assigned(OnDisplay) then
        OnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TOvbImdImageInfo }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgImageInfo.Clear;
begin
    ImageName      := '';
    ImagePath      := '';
    ImageID        := -1;
    DropImagePanel := nil;
    TagFetched     := FALSE;
    FTagNames.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TOvbImgImageInfo.Create;
begin
    inherited Create;
    FTagNames := TStringList.Create;
    Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TOvbImgImageInfo.Destroy;
begin
    FreeAndNil(FTagNames);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgImageInfo.SetTagNames(const Value: TStrings);
begin
    if Assigned(Value) then
        FTagNames.Assign(Value)
    else
        FTagNames.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TOvbImgImageDesc }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgImageDesc.Clear;
begin
    FileName      := '';
    ThumbFilename := '';
    Description   := '';
    Latitude      := 0;
    Longitude     := 0;
    Latitude2     := 0;
    Longitude2    := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
