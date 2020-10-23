{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE @ Balteau-NDT
Creation:     May 17, 2020
Description:  Simple Image Organizer - Data Module (All DB stuff)
License:      This program is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OvbImgOrganizer.DataModule;

interface

uses
    System.Types,               System.StrUtils,        System.UITypes,
    System.Classes,             System.IOUtils,         System.DateUtils,
    Data.DB,
    FireDAC.Stan.Intf,          FireDAC.Stan.Option,    FireDAC.Stan.Error,
    FireDAC.UI.Intf,            FireDAC.Phys.Intf,      FireDAC.Stan.Def,
    FireDAC.Stan.Pool,          FireDAC.Stan.Async,     FireDAC.Phys,
    FireDAC.Phys.SQLite,        FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
    FireDAC.Phys.SQLiteWrapper, FireDAC.Stan.Param,     FireDAC.VCLUI.Wait,
    FireDAC.ConsoleUI.Wait,     FireDAC.Comp.Client,
    FireDAC.DApt,               FireDAC.Phys.SQLiteWrapper.Stat,
    System.Zip,
    OvbImgOrganizer.DataTypes;

const
    CRLF                                = #13#10;
    ROOT_TAG_NAME                       = 'Root';
    TagInvalidChars   : String          = '+-=:;,&"''`()^{}*[]~?!<>\%$¨#';
    AllowedExtentions : array of String = ['.jpg', '.jpeg', '.tif',
                                           '.bmp', '.png'];
    SpecialTagNames   : array of String = ['FileName:', 'FileExt:',
                                           'FileDate:', 'IndexDate:'];

type
    TOvbImgOrganizerDataModule = class(TDataModule)
        FDConnection1: TFDConnection;
        FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    protected
        FDataFile                     : String;
        FCommonAppData                : String;
        FProgVer                      : String;
        FUnknownError                 : String;
        FOnDisplay                    : TOvbImgDisplayEvent;
        FOnAskYesOrNo                 : TOvbImgAskYesOrNoEvent;
        FOnTranslateString            : TOvbImgTranslateStringEvent;
        function _(const S : String) : String;
        function  SplitSearchString(const SearchString: String): TStringDynArray;
        function  GenerateSqlForDates(const TagParam  : String;
                                      const FieldName : String): String;
        function  GenerateSqlForFileName(const TagParam: String): String;
        function  ParseTagSpecial(const TagName    : String;
                                  out   TagSpecial : String;
                                  out   TagParam   : String): Boolean;
        function  GenerateSqlForFileExt(const TagParam: String): String;
        function  AskYesOrNo(const Question : String;
                             const Caption  : String) : Integer;
    private
    public
        constructor Create(AOwner : TComponent); override;
        procedure Display(const Msg: String); overload;
        procedure Display(const Msg : String; Args : array of const); overload;
        procedure DisplayHandler(Sender : TObject; const Msg : String);
        function  DBCreateBackup(const Destination : String;
                                 out   ZipFileName : String;
                                 var   ErrMsg      : String) : Boolean;
        function  DBCreate(var ErrMsg : String) : Boolean;
        function  DBCreateTable(const Query            : TFDQuery;
                                const TableName        : String;
                                const SqlToCreateTable : String;
                                const ConfirmDelete    : Boolean;
                                var   ErrMsg           : String): Boolean;
        function  DBConnect(out WasConnected : Boolean;
                            var ErrMsg       : String): Boolean; overload;
        function  DBConnect(var ErrMsg : String): Boolean; overload;
        function  DBConnected: Boolean;
        function  DBDisconnect: Boolean;
        function  DBInsertImage(const ImageDesc : TOvbImgImageDesc;
                                var   ErrMsg    : String) : Integer;
        function  DBInsertTag(const TagName     : String;
                              const TagParent   : String;
                              var   ErrMsg      : String): Integer; overload;
        function  DBInsertTag(const TagName     : String;
                              const TagParentID : Integer;
                              var   ErrMsg      : String): Integer; overload;
        function  DBInsertTagForImage(ImageID      : Integer;
                                      TagID        : Integer;
                                      var   ErrMsg : String): Integer;
        function  DBDeleteTagForImage(ImageID    : Integer;
                                      TagID      : Integer;
                                      var ErrMsg : String): Integer;
        function  DBDeleteImage(ImageID    : Integer;
                                var ErrMsg : String): Boolean;
        function  DBDeleteImages(ImageIDList : TOvbImgListOfInteger;
                                 var ErrMsg  : String): Boolean;
        function  DBDeleteTag(const TagName : String;
                              var   ErrMsg  : String) : Boolean;
        function  DBGetTagUseCount(const TagName : String;
                                   var   ErrMsg  : String): Integer;
        function  DBGetTagID(const TagName  : String;
                             var   ParentID : Integer;
                             var   ErrMsg : String): Integer; overload;
        function  DBGetTagID(const TagName  : String;
                             var   ErrMsg : String): Integer; overload;
        function  DBGetTagName(TagID      : Integer;
                               var ErrMsg : String) : String;
        function  DBGetTagList(ParentID   : Integer;
                               TagList    : TOvbImgTagList;
                               var ErrMsg : String) : Boolean;
        function  DBGetParentTagID(const TagID  : Integer;
                                   var   ErrMsg : String): Integer;
        function  DBGetImageID(const ImageName : String;
                               const ImageExt  : String;
                               out   ImagePath : String;
                               var   ErrMsg    : String): Integer;
        function  DBLoadTagList(TagNames   : TStrings;
                                var ErrMsg : String): Boolean;
        function  DBSearchGenerateSql(const SearchString : String;
                                      var   ErrMsg       : String): String; overload;
        function  DBSearchGenerateSql(TagsPlusList     : TStrings;
                                      TagsMinusList    : TStrings;
                                      SpecialPlusList  : TStrings;
                                      SpecialMinusList : TStrings;
                                      var ErrMsg       : String): String; overload;
        function  DBSearchGenerateOrderBySql(
                      OrderByIndex      : Integer;
                      OrderByDescending : Boolean;
                      var ErrMsg        : String): String;
        function  DBRenameTag(const OldTagName : String;
                              const NewTagName : String;
                              out   ErrMsg     : String) : Boolean;
        function  DBMoveTag(const TagName      : String;
                            const NewParentTag : String;
                            out   ErrMsg       : String): Boolean;
        function  DBFetchImageInfo(const ImageID : Integer;
                                   out FileName  : String;
                                   out FileExt   : String;
                                   out FilePath  : String;
                                   var ErrMsg    : String) : Boolean; overload;
        function  DBFetchImageTagNames(ImageID     : Integer;
                                       OutTagNames : TStrings;
                                       var ErrMsg  : String) : Boolean; overload;
        function  DBFetchImageTagNames(const FileName : String;
                                       out   ImageID  : Integer;
                                       OutTagNames    : TStrings;
                                       var ErrMsg     : String) : Boolean; overload;
        function  DBLoadCollection(const SqlText               : String;
                                   const More                  : Boolean;
                                   const CollectionLimit       : Integer;
                                   const CollectionLimitOffset : Integer;
                                   const OrderByIndex          : Integer;
                                   const OrderByDescending     : Boolean;
                                   out   FCollectionRecCnt     : Integer;
                                   var   Index                 : Integer;
                                   var   ErrMsg                : String;
                                   DataRecordEvent             : TOvbImgDataRecordEvent) : Integer;
        function DBCompareImageIndexWidthDirContent(
                                   const SrcDir       : String;
                                   const TableName    : String;
                                   const Recursive    : Boolean;
                                   const ProgressProc : TOvbImgProgressProc;
                                   var   ErrMsg       : String): Boolean;
        function DBScanDir(const SrcDir       : String;
                           const TableName    : String;
                           const Recursive    : Boolean;
                           const Query        : TFDQuery;
                           const ProgressProc : TOvbImgProgressProc;
                           var   Count        : Integer;
                           var   ErrMsg       : String): Boolean;
        function  ValidateTagName(const TagName : String;
                                  var   ErrMsg  : String): boolean;
        property OnDisplay                    : TOvbImgDisplayEvent
                                                read  FOnDisplay
                                                write FOnDisplay;
        property OnAskYesOrNo                 : TOvbImgAskYesOrNoEvent
                                                read  FOnAskYesOrNo
                                                write FOnAskYesOrNo;
        property OnTranslateString            : TOvbImgTranslateStringEvent
                                                read  FOnTranslateString
                                                write FOnTranslateString;
        property CommonAppData                : String
                                                read  FCommonAppData
                                                write FCommonAppData;
        property DataFile                     : String
                                                read  FDataFile
                                                write FDataFile;
        property ProgVer                      : String
                                                read  FProgVer
                                                write FProgVer;
    end;

function IsFileDropAllowed(
    const FileName : String) : Boolean; overload;
function  IsFileDropAllowed(
    const FileNames: array of String): Boolean; overload;

var
  OvbImgOrganizerDataModule: TOvbImgOrganizerDataModule;

implementation

uses
    WinApi.Windows, System.SysUtils;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.AskYesOrNo(
    const Question : String;
    const Caption  : String): Integer;
begin
    Result := IDNO;
    if Assigned(FOnAskYesOrNo) then
        FOnAskYesOrNo(Self, Question, Caption, Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TOvbImgOrganizerDataModule.Create(AOwner: TComponent);
begin
    FUnknownError := _('Unknown error');
    inherited Create(AOwner);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBConnect(var ErrMsg : String) : Boolean;
var
    WasConnected : Boolean;
begin
    Result := DBConnect(WasConnected, ErrMsg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBConnect(
    out WasConnected : Boolean;
    var ErrMsg       : String) : Boolean;
begin
    try
        WasConnected := DBConnected;
        Result       := WasConnected;
        if Result then
            Exit;
        FDConnection1.DriverName                := 'SQLITE';
        FDConnection1.Params.Values['Database'] := FDataFile;
        FDConnection1.Open;
        Result := TRUE;
    except
        on E:Exception do begin
            ErrMsg := Format(_('DBConnect raised an exception %s'),
                             [E.ClassName + ': ' + E.Message]);
            Display(ErrMsg);
            Result := FALSE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBConnected : Boolean;
begin
    Result := FDConnection1.Connected;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBDisconnect: Boolean;
begin
    try
        FDConnection1.Close;
        Result := not FDConnection1.Connected;
    except
        on E:Exception do begin
            Display(_('DBDisconnect raised an exception %s'),
                    [E.ClassName + ': ' + E.Message]);
            Result := FALSE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBCreateTable(
    const Query            : TFDQuery;
    const TableName        : String;
    const SqlToCreateTable : String;
    const ConfirmDelete    : Boolean;
    var   ErrMsg           : String) : Boolean;
begin
    Result := FALSE;
    ErrMsg := FUnknownError;
    Query.SQL.Text := 'SELECT name ' +
                      'FROM sqlite_master ' +
                      'WHERE type=''table'' AND name=:TableName';
    Query.ParamByName('TableName').AsString  := TableName;
    Query.Open();
    if not Query.Eof then begin
        if ConfirmDelete then begin
            if AskYesOrNo(Format(_('Table %s already exists'), [TableName]) + CRLF +
                          _('Do you want to overwrite it?'),
                          _('Create Table')) <> IDYES then begin
                ErrMsg := _('Canceled');
                Exit;
            end;
        end;
        Query.SQL.Text := 'DROP TABLE ' + TableName;
        Query.ExecSQL;
    end;

    Query.SQL.Text := SqlToCreateTable;
    Query.ExecSQL;
    Display(_('Table %s created'), [TableName]);
    ErrMsg := '';
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBCreate(
    var ErrMsg : String) : Boolean;
var
    Query: TFDQuery;
begin
    Result := FALSE;
    ErrMsg := FUnknownError;
    if (not DBConnected) and (not DBConnect(ErrMsg)) then
        Exit;

    Query := TFDQuery.Create(nil);
    try
        try
            Query.Connection := FDConnection1;

            if not DBCreateTable(
                Query,
                'ImageIndex',
                'CREATE TABLE ImageIndex (' +
                'ID            INTEGER primary key autoincrement, ' +
                'FileName      VARCHAR (80)  not null COLLATE NOCASE, ' + // Without extension
                'FileExt       VARCHAR (16)           COLLATE NOCASE, ' + // Include dot
                'Description   VARCHAR (80), ' +                          // Free text
                'FilePath      VARCHAR (256) not null COLLATE NOCASE, ' + // End with backslash
                'FileDateTime  DATETIME      not null, ' + // UTC
                'IndexDateTime DATETIME      not null, ' + // UTC
                'Latitude      DECIMAL (20, 17), ' +       // GeoTag
                'Longitude     DECIMAL (20, 17), ' +       // GeoTag
                'Latitude2     DECIMAL (20, 17), ' +       // If GeoTag is an area
                'Longitude2    DECIMAL (20, 17), ' +       // If GeoTag is an area
                'Thumb         VARCHAR (80) not null, ' +  // Thumb partial file path
                'UNIQUE(FileName, FileExt))',
                TRUE,
                ErrMsg) then
                Exit;

            if not DBCreateTable(
                Query,
                'TagIndex',
                'CREATE TABLE TagIndex (' +
                'ID        Integer primary key autoincrement, ' +
                'ParentID  Integer not null, ' +
                'TagName   varchar(80) not null COLLATE NOCASE)',
                TRUE,
                ErrMsg) then
                Exit;

            if not DBCreateTable(
                Query,
                'ImageTag',
                'CREATE TABLE ImageTag (' +
                'ImageID        Integer not null, ' +
                'TagID          Integer not null, ' +
                'UNIQUE(ImageID, TagID))',
                TRUE,
                ErrMsg) then
                Exit;

            DBInsertTag(ROOT_TAG_NAME, 0, ErrMsg);

            ErrMsg := '';
            Result := TRUE;
        except
            on E:Exception do begin
                Display(E.ClassName + ': ' + E.Message);
                ErrMsg := E.ClassName + ': ' + E.Message;
                Result := FALSE;
            end;
        end;
    finally
        Query.Close;
        Query.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBCreateBackup(
    const Destination : String;
    out   ZipFileName : String;
    var   ErrMsg      : String): Boolean;
var
    Zip            : TZipFile;
    ZipTmpFilename : String;
    ReadMeData     : TBytes;
    TimeStamp      : String;
begin
    Result := FALSE;
    if not DBDisconnect() then begin
        ErrMsg := _('DBDisconnect failed');
        Exit;
    end;
    TimeStamp      := FormatDateTime('_YYYYMMDD_HHNNSS', Now);
    ZipFilename    := IncludeTrailingPathDelimiter(Destination) +
                      'OvbImgBackup' + TimeStamp + '.zip';
    ZipTmpFilename := IncludeTrailingPathDelimiter(Destination) +
                      'OvbImgBackup' + TimeStamp + '_Temp.zip';
    ReadMeData := TEncoding.Unicode.GetBytes(
                      'This is a backup for OvbImgImageOriganizer.' + CRLF +
                      'Version ' + ProgVer + CRLF +
                      'Created on ' + DateTimeToStr(Now) + '.'  + CRLF);
    DeleteFile(ZipTmpFilename);
    Zip := TZipFile.Create;
    try
        Zip.Open(ZipTmpFilename, zmWrite);
        try
            Zip.Add(FDataFile);
            Zip.Add(ReadMeData, 'ReadMe.txt');
        except
            on E:Exception do begin
                ErrMsg := E.ClassName + ': ' + E.Message;
                try
                    FreeandNil(Zip);
                except
                    // Ignore any exception here
                end;
                DeleteFile(ZipTmpFilename);
                Exit;
            end;
        end;
    finally
        FreeandNil(Zip);
    end;

    if FileExists(ZipFilename) then begin
        DeleteFile(ZipFilename);
        if FileExists(ZipFilename) then begin
            Errmsg := _('Unable to delete existing backup file');
            Exit;
        end;
    end;
    if not RenameFile(ZipTmpFilename, ZipFilename) then begin
        Errmsg := _('Unable to rename temporary file to backup filename');
        Exit;
    end;

    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Return -1 in case of error
// If success, return image ID
function TOvbImgOrganizerDataModule.DBInsertImage(
    const ImageDesc : TOvbImgImageDesc;
    var   ErrMsg    : String) : Integer;
var
    ImageName     : String;
    ImageExt      : String;
    ImagePath     : String;
    Query         : TFDQuery;
    ID            : Int64;
    WasConnected  : Boolean;
    ImageDateTime : TDateTime;
    IndexDateTime : TDateTime;
begin
    Result := -1;
    ErrMsg := FUnknownError;
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    ImageExt      := ExtractFileExt(ImageDesc.FileName);
    ImageName     := ChangeFileExt(ExtractFileName(ImageDesc.FileName), '');
    ImagePath     := ExtractFilePath(ImageDesc.FileName);
    ImageDateTime := TFile.GetCreationTimeUtc(ImageDesc.FileName);
    IndexDateTime := TTimeZone.Local.ToUniversalTime(Now);
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            Query.SQL.Text := 'SELECT ID, FilePath ' +
                              'FROM ImageIndex ' +
                              'WHERE (FileName = :FileName) ' + (*COLLATE NOCASE*) ' ' +
                                'AND (FileExt  = :FileExt)  ' + (*COLLATE NOCASE*) '';
            Query.ParamByName('FileName').AsString := ImageName;
            Query.ParamByName('FileExt').AsString  := ImageExt;
            Query.Open();
            if not Query.Eof then begin
                // We have found an image with same name
                ID := Query.FieldByName('ID').AsInteger;
                Display('ImageName already exists. ID=%d', [ID]);
                Result := ID;
                Exit;
            end;
            Query.SQL.Text := 'INSERT INTO ImageIndex ' +
                              '(ID, FilePath, FileName, FileExt, Thumb, ' +
                               'FileDateTime, IndexDateTime, ' +
                               'Latitude, Longitude, ' +
                               'Latitude2, Longitude2, ' +
                               'Description ' +
                              ') ' +
                              ' VALUES ' +
                              '(:ID, :FilePath, :FileName, :FileExt, :Thumb, ' +
                               ':FileDateTime, :IndexDateTime, ' +
                               ':Latitude,  :Longitude, ' +
                               ':Latitude2, :Longitude2, ' +
                               ':Description ' +
                              ')';

            // ID is an autoincrement column,
            // we must insert NULL to make it work!
            Query.ParamByName('ID').DataType := ftInteger;
            Query.ParamByName('ID').Clear;
            Query.ParamByName('Thumb').AsString           := ImageDesc.ThumbFilename;
            Query.ParamByName('FilePath').AsString        := ImagePath;
            Query.ParamByName('FileName').AsString        := ImageName;
            Query.ParamByName('FileExt').AsString         := ImageExt;
            Query.ParamByName('FileDateTime').AsDateTime  := ImageDateTime;
            Query.ParamByName('IndexDateTime').AsDateTime := IndexDateTime;
            Query.ParamByName('Description').AsString     := ImageDesc.Description;
            Query.ParamByName('Latitude').AsFloat         := ImageDesc.Latitude;
            Query.ParamByName('Longitude').AsFloat        := ImageDesc.Longitude;
            Query.ParamByName('Latitude2').AsFloat        := ImageDesc.Latitude2;
            Query.ParamByName('Longitude2').AsFloat       := ImageDesc.Longitude2;
            Query.ExecSQL;
            // Fetch the ID assigned to the inserted record
            ID := Int64((TObject(FDConnection1.CliObj) as TSQLiteDatabase).LastInsertRowid);
            Display('Image %s inserted. ID=%d', [ImageName, ID]);
            Result := ID;
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Return TagID
// Return -1 in case of error
function TOvbImgOrganizerDataModule.DBInsertTag(
    const TagName     : String;
    const TagParentID : Integer;
    var   ErrMsg      : String) : Integer;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
begin
    Result := -1;
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            Query.SQL.Text := 'INSERT INTO ' +
                              'TagIndex (ID, ParentID, TagName) ' +
                              'VALUES (:ID, :ParentID, :TagName)';
            // ID is an autoincrement column,
            // we must insert NULL to make it work!
            Query.ParamByName('ID').DataType := ftInteger;
            Query.ParamByName('ID').Clear;
            Query.ParamByName('ParentID').AsInteger := TagParentID;
            Query.ParamByName('TagName').AsString   := TagName;
            Query.ExecSQL;
            // Fetch the ID assigned to the inserted record
            Result := Int64((TObject(FDConnection1.CliObj) as TSQLiteDatabase).LastInsertRowid);
            ErrMsg := '';
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Return TagID
// Return -1 in case of error
function TOvbImgOrganizerDataModule.DBInsertTag(
    const TagName   : String;
    const TagParent : String;    // Use empty string for root
    var   ErrMsg    : String) : Integer;
var
    ParentID     : Integer;
    WasConnected : Boolean;
begin
    Result := -1;
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        if TagParent = '' then
            ParentID := 1          // TagID 1 is Root record
        else begin
            ParentID := DBGetTagID(TagParent, ErrMsg);
            if ParentID < 0 then
                Exit;              // Error looking for requested parent tag
            if ParentID = 0 then begin
                // Requested parent tag doesn't exists
                Display('Tag parent "%" not found, inserting', [TagParent]);
                ParentID := DBInsertTag(TagName, '', ErrMsg);
                if ParentID < 0 then
                    Exit;
            end;
        end;
        Result := DBInsertTag(TagName, ParentID, ErrMsg);
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Return the ImageID when found in database
// Return 0 if image is not in database
// Return -1 if case of error
function TOvbImgOrganizerDataModule.DBGetImageID(
    const ImageName : String;
    const ImageExt  : String;
    out   ImagePath : String;
    var   ErrMsg    : String) : Integer;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
begin
    Result    := -1;
    ErrMsg    := FUnknownError;
    ImagePath := '';
    // Check if tag is used on images
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            Query.SQL.Text := 'SELECT ID, FilePath ' +
                              'FROM ImageIndex ' +
                              'WHERE FileName = :FileName ' + (*COLLATE NOCASE*) ' ' +
                                'AND FileExt  = :FileExt  ' + (*COLLATE NOCASE*) '';
            Query.ParamByName('FileName').AsString := ImageName;
            Query.ParamByName('FileExt').AsString  := ImageExt;
            Query.Open();
            if Query.Eof then begin
                ErrMsg := Format(_('Image %s%s not in database'),
                                 [ImageName, ImageExt]);
                Result := 0;
                Exit;
            end;
            Result    := Query.FieldByName('ID').AsInteger;
            ImagePath := Query.FieldByName('FilePath').AsString;
            ErrMsg    := '';
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Return ParentTagID
// Return -1 in case of error
// Return 0 if tag not found
function TOvbImgOrganizerDataModule.DBGetParentTagID(
    const TagID  : Integer;
    var   ErrMsg : String) : Integer;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
begin
    Result := -1;
    ErrMsg := FUnknownError;
    // Check if tag is used on images
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            Query.SQL.Text := 'SELECT ParentID ' +
                              'FROM TagIndex ' +
                              'WHERE ID = :TagID';
            Query.ParamByName('TagID').AsInteger := TagID;
            Query.Open();
            if Query.Eof then begin
                Display('TagID %d not in database', [TagID]);
                Result := 0;
            end
            else
                Result := Query.FieldByName('ParentID').AsInteger;
            ErrMsg := '';
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Return TagID
// Return -1 in case of error
// Retunr 0 if tag not found
function TOvbImgOrganizerDataModule.DBGetTagID(
    const TagName  : String;
    var   ErrMsg   : String) : Integer;
var
    ParentID : Integer;
begin
    Result := DBGetTagID(TagName, ParentID, ErrMsg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Return TagID
// Return -1 in case of error
// Retunr 0 if tag not found
function TOvbImgOrganizerDataModule.DBGetTagID(
    const TagName  : String;
    var   ParentID : Integer;
    var   ErrMsg   : String) : Integer;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
begin
    Result := -1;
    // Check if tag is used on images
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            Query.SQL.Text := 'SELECT ID, ParentID ' +
                              'FROM TagIndex ' +
                              'WHERE TagName = :TagName ' + (*COLLATE NOCASE*) '';
            Query.ParamByName('TagName').AsString := TagName;
            Query.Open();
            if Query.Eof then begin
                Display('Tag %s not in database', [TagName]);
                ParentID := 0;
                Result   := 0;
                Exit;
            end;
            Result   := Query.FieldByName('ID').AsInteger;
            ParentID := Query.FieldByName('ParentID').AsInteger;
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Return TagName if success
// Return empty string if error
function TOvbImgOrganizerDataModule.DBGetTagName(TagID: Integer; var ErrMsg: String): String;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
begin
    Result := '';
    // Check if tag is used on images
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            Query.SQL.Text := 'SELECT TagName ' +
                              'FROM TagIndex ' +
                              'WHERE ID = :TagID ';
            Query.ParamByName('TagID').AsInteger := TagID;
            Query.Open();
            if Query.Eof then begin
                Display('TagID %d not in database', [TagID]);
                Result   := '';
            end
            else
                Result   := Query.FieldByName('TagName').AsString;
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBGetTagList(
    ParentID   : Integer;
    TagList    : TOvbImgTagList;
    var ErrMsg : String): Boolean;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
    TagRec       : TOvbImgTagObject;
begin
//    Display('DBGetTagList ParentID=%d', [ParentID]);
    Result := FALSE;
    ErrMsg := FUnknownError;
    if not Assigned(TagList) then
        Exit;
    TagList.Clear;
    // Check if tag is used on images
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            Query.SQL.Text := 'SELECT ID, ParentID, TagName ' +
                              'FROM TagIndex ' +
                              'WHERE ParentID = :ParentID ' +
                              'ORDER BY TagName';
            Query.ParamByName('ParentID').AsInteger := ParentID;
            Query.Open();
            while not Query.Eof do begin
                TagRec           := TOvbImgTagObject.Create(
                                        DisplayHandler,
                                        Query.FieldByName('ID').AsInteger);
                TagRec.ParentID  := Query.FieldByName('ParentID').AsInteger;
                TagRec.TagName   := Query.FieldByName('TagName').AsString;
                TagList.Add(TagRec);
                Query.Next;
            end;
//            Display('DBGetTagList TagList.Count=%d', [TagList.Count]);
            Result := TRUE;
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBDeleteTag(
    const TagName : String;
    var   ErrMsg  : String) : Boolean;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
    TagID        : Integer;
    TagDeleted   : Integer;
begin
    Result := FALSE;
    ErrMsg := FUnknownError;
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            // Check if tag is used on images
            Query.Connection := FDConnection1;
            TagID := DBGetTagID(TagName, ErrMsg);
            if TagID < 0 then
                Exit;         // Error looking for tag
            if TagID = 0 then begin
                // Tag not found, don't even try to delete it. It is OK
                Result := TRUE;
                ErrMsg := '';
                Exit;
            end;

            // Delete the association of this tag with images
            Query.SQL.Text := 'DELETE FROM ImageTag ' +
                              'WHERE TagID = :TagID';
            Query.ParamByName('TagID').AsInteger := TagID;
            Query.ExecSQL;
            TagDeleted := Query.RowsAffected;
            Display('%d image/tag associations deleted for %s',
                    [TagDeleted, TagName]);

            // Now delete the tag from TagIndex
            Query.SQL.Text := 'DELETE FROM TagIndex ' +
                              'WHERE ID = :ID';
            Query.ParamByName('ID').AsInteger := TagID;
            Query.ExecSQL;
            TagDeleted := Query.RowsAffected;
            Display('%d tag %s deleted from TagIndex',
                    [TagDeleted, TagName]);
            ErrMsg := '';
            Result := TRUE;
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Returns -1 in case of error
// Returns the number of tag associations.
function TOvbImgOrganizerDataModule.DBGetTagUseCount(
    const TagName : String;
    var   ErrMsg  : String) : Integer;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
    TagID        : Integer;
begin
    Result := -1;
    ErrMsg := FUnknownError;
    // Check if tag is used on images
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;

            // Fetch TagID
            Query.SQL.Text := 'SELECT ID ' +
                              'FROM TagIndex ' +
                              'WHERE TagName = :TagName ' + (*COLLATE NOCASE*) '';
            Query.ParamByName('TagName').AsString := TagName;
            Query.Open();
            if Query.Eof then begin
                ErrMsg := Format(_('Tag "%s" not in database'), [TagName]);
                Exit;
            end;
            TagID := Query.FieldByName('ID').AsInteger;

            Query.SQL.Text := 'SELECT Count(0) ' +
                              'FROM ImageTag ' +
                              'WHERE TagID = :TagID';
            Query.ParamByName('TagID').AsInteger := TagID;
            Query.Open();
            Result := Query.Fields.FieldByNumber(1).AsInteger;
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Returns -1 in case of error
// Returns 1 if image has been tagged
// Returns 0 if image was already tagged
function TOvbImgOrganizerDataModule.DBInsertTagForImage(
    ImageID    : Integer;
    TagID      : Integer;
    var ErrMsg : String) : Integer;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
begin
    Result := -1;
    ErrMsg := FUnknownError;
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            // First, check if image already tagged with that tag
            Query.SQL.Text := 'SELECT Count(*) FROM ImageTag ' +
                              'WHERE (ImageID = :ImageID) AND ' +
                                    '(TagID   = :TagID)';
            Query.ParamByName('ImageID').AsInteger := ImageID;
            Query.ParamByName('TagID').AsInteger   := TagID;
            Query.Open;
            if Query.Fields.FieldByNumber(1).AsInteger = 0 then begin
                // Not already tagged, insert
                Query.SQL.Text := 'INSERT INTO ' +
                                  'ImageTag (ImageID, TagID) ' +
                                  'VALUES (:ImageID, :TagID)';
                Query.ParamByName('ImageID').AsInteger := ImageID;
                Query.ParamByName('TagID').AsInteger   := TagID;
                Query.ExecSQL;
                Result := 1;
            end
            else
                Result := 0;
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBDeleteImage(
    ImageID    : Integer;
    var ErrMsg : String): Boolean;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
begin
    Result := FALSE;
    ErrMsg := FUnknownError;
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            // First delete all tag for the image
            Query.SQL.Text := 'DELETE FROM ImageTag ' +
                              'WHERE ImageID = :ImageID';
            Query.ParamByName('ImageID').AsInteger := ImageID;
            Query.ExecSQL;
            // Then delete image from index
            Query.SQL.Text := 'DELETE FROM ImageIndex ' +
                              'WHERE ID = :ImageID';
            Query.ParamByName('ImageID').AsInteger := ImageID;
            Query.ExecSQL;
            Result := TRUE;
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBDeleteImages(
    ImageIDList : TOvbImgListOfInteger;
    var ErrMsg  : String): Boolean;
var
    ImageIDs     : String;
    ImageID      : Integer;
    Query        : TFDQuery;
    WasConnected : Boolean;
begin
    Result := FALSE;
    ErrMsg := FUnknownError;
    if ImageIDList.Count <= 0 then begin
        // It is not an error to have an empty list of images to delete
        Result := TRUE;
        ErrMsg := '';
        Exit;
    end;

    ImageIDs := '';
    for ImageID in ImageIDList do
        ImageIDs := ImageIDs + ',' + IntToStr(ImageID);
    Delete(ImageIDs, 1, 1);
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            // First delete all tag for the image
            Query.SQL.Text := 'DELETE FROM ImageTag ' +
                              'WHERE ImageID in (' + ImageIDs + ')';
            Query.ExecSQL;
            Display(_('%d image/tag associations deleted'), [Query.RowsAffected]);
            // Then delete image from index
            Query.SQL.Text := 'DELETE FROM ImageIndex ' +
                              'WHERE ID in (' + ImageIDs + ')';
            Query.ExecSQL;
            Display(_('%d images removed'), [Query.RowsAffected]);
            Result := TRUE;
            ErrMsg := '';
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Returns number of deleted rows (Should be 0 or 1) or -1 if error
function TOvbImgOrganizerDataModule.DBDeleteTagForImage(
    ImageID    : Integer;
    TagID      : Integer;
    var ErrMsg : String): Integer;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
begin
    Result := -1;
    ErrMsg := '';
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            Query.SQL.Text := 'DELETE FROM ImageTag ' +
                              'WHERE (ImageID = :ImageID) ' +
                                'AND (TagID = :TagID)';
            Query.ParamByName('ImageID').AsInteger := ImageID;
            Query.ParamByName('TagID').AsInteger   := TagID;
            Query.ExecSQL;
            Result := Query.RowsAffected;
            ErrMsg := '';
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBLoadTagList(
    TagNames   : TStrings;
    var ErrMsg : String) : Boolean;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
begin
    Result := FALSE;
    ErrMsg := FUnknownError;
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        TagNames.Clear;
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            Query.SQL.Text := 'SELECT TagName ' +
                              'FROM TagIndex ' +
                              'WHERE ID <> 1 ' +    // Do not load Root
                              'ORDER BY TagName';
            Query.Open();
            while not Query.Eof do begin
                TagNames.Add(Query.FieldByName('TagName').AsString);
                Query.Next;
            end;
            Result := TRUE;
            ErrMsg := '';
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.SplitSearchString(
    const SearchString : String) : TStringDynArray;
var
    I, J : Integer;
    Tag  : String;
    N    : Integer;
    More : Boolean;
begin
//    Result := SplitString(SearchString, ' ');
    N := 0;
    SetLength(Result, 0);
    I := 1;
    while I <= Length(SearchString) do begin
        // Skip spaces, tabs, CR, LF
        while (I <= Length(SearchString)) and
              (CharInSet(SearchString[I], [' ', #9, #13, #10])) do
            Inc(I);
        J := I;
        // Skip to next space not included in double quotes
        while (I <= Length(SearchString)) and
              (not CharInSet(SearchString[I], [' ', #9, #13, #10])) do begin
            if SearchString[I] = '"' then begin
                Inc(I);  // Skip double quote
                // Skip to closing double quote
                More := TRUE;
                while More do begin
                    More := FALSE;
                    while (I <= Length(SearchString)) and
                          (SearchString[I] <> '"') do
                        Inc(I);
                    if I <= Length(SearchString) then begin
                        Inc(I);  // Skip double quote
                        // If we have a double quote immediately followed by
                        // another double quote, it not the terminator
                        if (I <= Length(SearchString)) and
                           (SearchString[I] = '"') then begin
                           More := TRUE;
                           Inc(I);  // Skip double quote
                           continue;
                        end;
                    end;
                end;
                continue;
            end;
            // Normal character, continue to parse
            Inc(I);
        end;
        Tag := Copy(SearchString, J, I - J);
        Display('"%s"', [Tag]);
        SetLength(Result, N + 1);
        Result[N] := Tag;
        Inc(N);
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSpecialTag(const ATagName : String) : Boolean;
var
    I          : Integer;
    TagName    : String;
    TagSpecial : String;
begin
    Result := FALSE;
    I := Pos(':', ATagName);
    if I < 0 then
        Exit;
    TagSpecial := Copy(ATagName, 1, I);
    for TagName in SpecialTagNames do begin
        if SameText(TagName, TagSpecial) then begin
            Result := TRUE;
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Generate SQL to do a database search. Search is based on a search string.
// The search string is a list of tags (words) separated by a space. Each tag
// can be prefixed by a plus or minus sign. No prefix is the same as having
// a plus sign prefix.
// The SQL generated will extract all images that are tagged with a '+'
// prefixed tags but has none of the '-' prefixed tags.
// There is a special tag '-*' which means all images not tagged at all. This
// tag must be used alone.
// Return empty string in case of error.
function TOvbImgOrganizerDataModule.DBSearchGenerateSql(
    const SearchString : String;
    var   ErrMsg       : String): String;
var
    Tags             : TStringDynArray;
    TagsPlusList     : TStringList;
    TagsMinusList    : TStringList;
    SpecialPlusList  : TStringList;
    SpecialMinusList : TStringList;
    TagName          : String;
    I                : Integer;
    TagsPlus         : String;
    TagsMinus        : String;
    FlagAllNotTagged : Boolean;
    function AddPlusTag(const ATagName : String) : Boolean;
    begin
        Result := FALSE;
        if ATagName <> '' then begin
            if Pos(':', ATagName) > 0 then begin
                 if not IsSpecialTag(ATagName) then begin
                     ErrMsg := Format(_('Unknown special tag "%s"'), [ATagName]);
                     Exit;
                 end
                 else if SpecialPlusList.IndexOf(ATagName) < 0 then
                     SpecialPlusList.Add(ATagName);
            end
            else if TagsPlusList.IndexOf(ATagName) < 0 then // No duplicate
                TagsPlusList.Add(ATagName);
        end;
        Result := TRUE;
    end;
    function AddMinusTag(const ATagName : String) : Boolean;
    begin
        Result := FALSE;
        if ATagName = '*' then
            FlagAllNotTagged := TRUE
        else if ATagName <> '' then begin
            if Pos(':', ATagName) > 0 then begin
                 if not IsSpecialTag(ATagName) then begin
                     ErrMsg := Format(_('Unknown special tag "%s"'), [ATagName]);
                     Exit;
                 end
                 else if SpecialMinusList.IndexOf(ATagName) < 0 then
                     SpecialMinusList.Add(ATagName);
            end
            else if TagsMinusList.IndexOf(ATagName) < 0 then
                TagsMinusList.Add(ATagName);
        end;
        Result := TRUE;
    end;
begin
    ErrMsg           := FUnknownError;
    Result           := '';
    TagsPlusList     := nil;
    TagsMinusList    := nil;
    SpecialPlusList  := nil;
    SpecialMinusList := nil;
    try
        FlagAllNotTagged := FALSE;
        TagsPlusList     := TStringList.Create;
        TagsMinusList    := TStringList.Create;
        SpecialPlusList  := TStringList.Create;
        SpecialMinusList := TStringList.Create;
        Tags             := SplitSearchString(SearchString); //, ' ');
        for I := Low(Tags) to High(Tags) do begin
            TagName := Trim(Tags[I]);
            if TagName = '' then
                continue;
            if TagName[1] = '-' then begin
                Delete(TagName, 1, 1);
                if not AddMinusTag(TagName) then
                    Exit;
            end
            else if TagName[1] = '+' then begin
                Delete(TagName, 1, 1);
                if not AddPlusTag(TagName) then
                    Exit;
            end
            else begin
                if not AddPlusTag(TagName) then
                    Exit;
            end;
        end;

        if FlagAllNotTagged then begin
            if (TagsMinusList.Count = 0) and (TagsPlusList.Count = 0) then
                TagsMinusList.Add('*')
            else begin
                ErrMsg := _('-* can only be used alone');
                Exit;
            end;
        end;

        if TagsPlusList.Count = 0 then
            TagsPlus := ''
        else begin
            TagsPlusList.Delimiter := ',';
            TagsPlus := '''' + StringReplace(TagsPlusList.DelimitedText, ',', ''',''', [rfReplaceAll]) + '''';
        end;
        if TagsMinusList.Count = 0 then
            TagsMinus := ''
        else begin
            TagsMinusList.Delimiter := ',';
            TagsMinus := '''' + StringReplace(TagsMinusList.DelimitedText, ',', ''',''', [rfReplaceAll]) + '''';
        end;
//        Display('TagsPlus=%s    TagsMinus=%s', [TagsPlus, TagsMinus]);

        Result := DBSearchGenerateSql(TagsPlusList, TagsMinusList,
                                      SpecialPlusList, SpecialMinusList,
                                      ErrMsg);
    finally
        FreeAndNil(TagsPlusList);
        FreeAndNil(TagsMinusList);
        FreeAndNil(SpecialPlusList);
        FreeAndNil(SpecialMinusList);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBSearchGenerateOrderBySql(
    OrderByIndex      : Integer;
    OrderByDescending : Boolean;
    var ErrMsg        : String) : String;
var
    Descending       : String;
begin
    ErrMsg := '';
    if OrderByDescending then
        Descending := 'DESC '
    else
        Descending := '';
    case OrderByIndex of
    0:  Result := 'ORDER BY ImgIndex.FileName '      + Descending +
                         ', ImgIndex.FileExt '       + Descending;
    1:  Result := 'ORDER BY ImgIndex.FileDateTime '  + Descending;
    2:  Result := 'ORDER BY ImgIndex.IndexDateTime ' + Descending;
    else
        Result := '';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Syntax examples: FileDate:"13-07-2020"
//                  FileDate:">13-07-2020"
//                  FileDate:"<13-07-2020"
//                  FileDate:">10-07-2020<13-07-2020"
// Operators: <, <=, =, >=, >, <>. No operator means equal operator.
// If only one date, then returned value is 0.
function ParseFileDateTagParam(
    const TagParam   : String;
    out   FirstOp    : String;
    out   FirstDate  : TDateTime;
    out   SecondOp   : String;
    out   SecondDate : TDateTime) : Boolean;
var
    I : Integer;
    S : String;
begin
    Result     := FALSE;
    FirstOp    := '';
    FirstDate  := 0;
    SecondOp   := '';
    SecondDate := 0;
    try
        I := 1;
        while (I <= Length(TagParam)) and
              CharInSet(TagParam[I], ['<', '=', '>']) do begin
            FirstOp := FirstOp + TagParam[I];
            Inc(I);
        end;
        if FirstOp = '' then
            FirstOp := '=';
        S := '';
        while (I <= Length(TagParam)) and
              CharInSet(TagParam[I], ['0'..'9', FormatSettings.DateSeparator]) do begin
            S := S + TagParam[I];
            Inc(I);
        end;
        if S <> '' then begin
            if not TryStrToDate(S, FirstDate) then begin
                FirstDate := 0;
                Exit;
            end;
        end;

        while (I <= Length(TagParam)) and
              CharInSet(TagParam[I], ['<', '=', '>']) do begin
            SecondOp := SecondOp + TagParam[I];
            Inc(I);
        end;
        if SecondOp = '' then
            SecondOp := '=';
        S := '';
        while (I <= Length(TagParam)) and
              CharInSet(TagParam[I], ['0'..'9', FormatSettings.DateSeparator]) do begin
            S := S + TagParam[I];
            Inc(I);
        end;
        if S <> '' then begin
            if not TryStrToDate(S, SecondDate) then begin
                SecondDate := 0;
                Exit;
            end;
        end;

        Result := TRUE;
    except
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.GenerateSqlForDates(
    const TagParam  : String;
    const FieldName : String) : String;
var
    FirstOp          : String;
    FirstDate        : TDateTime;
    SecondOp         : String;
    SecondDate       : TDateTime;
const
    ISO8601DateTimeFmt = 'YYYY-MM-DD HH:NN:SS:ZZZ';  // Used by SQLite
begin
    // Syntax: FileDate:"13-07-2020"
    //         FileDate:">13-07-2020"
    //         FileDate:"<13-07-2020"
    //         FileDate:">10-07-2020<13-07-2020"
    if not ParseFileDateTagParam(TagParam,
                             FirstOp,  FirstDate,
                             SecondOp, SecondDate) then
        // Bad syntax, generate SQL which evaluates to FALSE
        Result := '(1=2)'
    else begin
        if FirstOp = '=' then
            Result :=
               '((' + FieldName + ' >= "' +
                 FormatDateTime(ISO8601DateTimeFmt, FirstDate) + '")AND' +
               '(' + FieldName + ' < "' +
                 FormatDateTime(ISO8601DateTimeFmt, FirstDate + 1) + '"))'
        else if FirstOp = '<>' then
            Result :=
               '((' + FieldName + ' < "' +
                 FormatDateTime(ISO8601DateTimeFmt, FirstDate) + '")OR' +
               '(' + FieldName + ' >= "' +
                 FormatDateTime(ISO8601DateTimeFmt, FirstDate + 1) + '"))'
        else if (FirstOp = '>=') or (FirstOp = '>') or
                 (FirstOp = '<=') or (FirstOp = '<') then
            Result :=
               '(' + FieldName + ' ' + FirstOp + ' "' +
                 FormatDateTime(ISO8601DateTimeFmt, FirstDate) + '")'
        else
            Result := '(1=3)';
        if SecondDate <> 0 then begin
            Result := 'AND';
            if SecondOp = '=' then
                Result := Result +
                   '((' + FieldName + ' >= "' +
                     FormatDateTime(ISO8601DateTimeFmt, SecondDate) + '")AND' +
                   '(' + FieldName + ' < "' +
                     FormatDateTime(ISO8601DateTimeFmt, SecondDate + 1) + '"))'
            else if SecondOp = '<>' then
                Result := Result +
                   '((' + FieldName + ' < "' +
                     FormatDateTime(ISO8601DateTimeFmt, SecondDate) + '")OR' +
                   '(' + FieldName + ' >= "' +
                     FormatDateTime(ISO8601DateTimeFmt, SecondDate + 1) + '"))'
            else if (SecondOp = '>=') or (SecondOp = '>') or
                     (SecondOp = '<=') or (SecondOp = '<') then
                Result := Result +
                   '(' + FieldName + ' ' + SecondOp + ' "' +
                     FormatDateTime(ISO8601DateTimeFmt, SecondDate) + '")'
            else
                Result := Result + '(1=4)';
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.GenerateSqlForFileName(
    const TagParam : String) : String;
begin
    Result := '((ImgIndex.FileName = "' + ChangeFileExt(TagParam, '') + '")' +
              'AND' +
              '(ImgIndex.FileExt  = "' + ExtractFileExt(TagParam) + '"))';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.GenerateSqlForFileExt(
    const TagParam : String) : String;
begin
    Result := '(ImgIndex.FileExt = "' + ExtractFileExt(TagParam) + '")';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.ParseTagSpecial(
    const TagName : String;
    out   TagSpecial : String;
    out   TagParam   : String) : Boolean;
var
    I : Integer;
begin
    I := Pos(':', TagName);
    if I < 2 then begin
        Result := FALSE;
        Exit;
    end;
    TagSpecial := Copy(TagName, 1, I);
    TagParam   := Trim(Copy(TagName, I + 1, MAXINT));
    if (TagParam[1] = '"') then begin
        Delete(TagParam, 1, 1);
        if TagParam[Length(TagParam)] = '"' then
            Delete(TagParam, Length(TagParam), 1);
    end;
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Generate SQL to do a database search. Search is based of two string lists:
// One string list with all tags that an image must have.
// One string list with all tags that an image must not have.
// Return empty string in case of error.
function TOvbImgOrganizerDataModule.DBSearchGenerateSql(
    TagsPlusList     : TStrings;
    TagsMinusList    : TStrings;
    SpecialPlusList  : TStrings;
    SpecialMinusList : TStrings;
    var   ErrMsg     : String) : String;
var
    Query            : TFDQuery;
    WasConnected     : Boolean;
    TagsPlusIDList   : TStringList;
    TagsMinusIDList  : TStringList;
    TagID            : Integer;
    TagName          : String;
    TagSpecial       : String;
    TagParam         : String;
    I                : Integer;
    Alias            : String;
    Sql              : String;
    Flag             : Boolean;
begin
    Result := '';
    ErrMsg := FUnknownError;
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        TagsPlusIDList  := nil;
        TagsMinusIDList := nil;
        try
            Query := TFDQuery.Create(nil);
            try
                Query.Connection := FDConnection1;

                TagsPlusIDList  := TStringList.Create;
                TagsPlusIDList.Delimiter := '|';
                if TagsPlusList.Count > 0 then begin
                    for TagName in TagsPlusList do begin
                        TagID := DBGetTagID(TagName, ErrMsg);
                        if TagID < 0 then
                            Exit;     // Error while searching for tag
                        if TagID = 0 then begin
                            ErrMsg := Format(_('Tag "%s" doesn''t exist'),
                                             [TagName]);
                            Exit;
                        end;
                        TagsPlusIDList.Add(IntToStr(TagID));
                    end;
                end;

                TagsMinusIDList := TStringList.Create;
                TagsMinusIDList.Delimiter := '|';
                if TagsMinusList.Count > 0 then begin
                    for TagName in TagsMinusList do begin
                        if TagName = '*' then begin
                            if (TagsMinusIDList.Count <> 0) or
                               (TagsPlusIDList.Count <> 0) then begin
                                ErrMsg := _('-* can only be used alone');
                                Exit;
                            end
                            else
                                TagsMinusIDList.Add('*')
                        end
                        else begin
                            TagID := DBGetTagID(TagName, ErrMsg);
                            if TagID < 0 then
                                Exit;     // Error while searching for tag
                            if TagID = 0 then begin
                                ErrMsg := Format(_('Tag "%s" doesn''t exist'),
                                                 [TagName]);
                                Exit;
                            end;
                            TagsMinusIDList.Add(IntToStr(TagID));
                        end;
                    end;
                end;

//                Display('TagsPlusID=%s    TagsMinusID=%s',
//                        [TagsPlusIDList.DelimitedText,
//                         TagsMinusIDList.DelimitedText]);
                if (TagsPlusIDList.Count = 0) and (TagsMinusIDList.Count = 0) then
                    // This query will return ALL records
                    Sql := 'FROM ImageIndex ImgIndex WHERE (1=1'
                else if (TagsPlusIDList.Count = 0) and
                        (TagsMinusIDList.Count = 1) and
                        (TagsMinusIDList[0] = '*') then
                    // This request returns all images which are not tagged
                    Sql := 'FROM ImageIndex ImgIndex ' +
                           'LEFT JOIN ImageTag ImgTagM0 ON ImgTagM0.ImageID = ImgIndex.ID ' +
                           'WHERE (ImgTagM0.ImageID IS NULL '
                else begin
                    Sql := 'FROM ImageIndex  ImgIndex ' +
                           'WHERE (ImgIndex.ID in (' +
                               'SELECT DISTINCT ImgTag.ImageID ' +
                               'FROM ImageTag ImgTag ';
                    if TagsPlusIDList.Count > 0 then begin
                        // Add all records having a TagName in the plus list
                        for I := 0 to TagsPlusIDList.Count - 1 do begin
                            TagName := TagsPlusIDList[I];
                            Alias   := 'ImgTagP' + IntToStr(I);
                            Sql     := Sql +
                                'JOIN ImageTag ' + Alias +
                                  ' ON ' + Alias + '.ImageID = ImgTag.ImageID ' +
                                     'AND ' + Alias + '.TagID = ' + TagName + ' ';
                        end;
                    end;
                    if TagsMinusIDList.Count > 0 then begin
                        // Remove all records having a TagName in the minus list
                        for I := 0 to TagsMinusIDList.Count - 1 do begin
                            TagName   := TagsMinusIDList[I];
                            Alias := 'ImgTagM' + IntToStr(I);
                            Sql   := Sql +
                                'LEFT JOIN ImageTag ' + Alias +
                                  ' ON ' + Alias + '.ImageID = ImgTag.ImageID ' +
                                     'AND ' + Alias + '.TagID = ' + TagName + ' ';
                        end;
                        for I := 0 to TagsMinusIDList.Count - 1 do begin
                            Alias := 'ImgTagM' + IntToStr(I);
                            if I = 0 then
                                Sql   := Sql + 'WHERE ' + Alias + '.ImageID IS NULL '
                            else
                                Sql   := Sql +   'AND ' + Alias + '.ImageID IS NULL '
                        end;
                    end;
                    if TagsPlusIDList.Count <= 0 then begin
                        // When the plus list is empty, the above request lacks
                        // the images that are not tagged at all, add those.
                        Sql := Sql +
                               'UNION ' +
                               'SELECT DISTINCT ImgIndex.ID ' +
                               'FROM ImageIndex  ImgIndex ' +
                               'LEFT JOIN ImageTag ImgTagM0 ON ImgTagM0.ImageID = ImgIndex.ID ' +
                               'WHERE ImgTagM0.ImageID IS NULL';
                    end;
                    Sql := Sql + ') ';
                end;
                Sql := Sql + ') ';

                if SpecialPlusList.Count > 0 then begin
                    Flag := FALSE;
                    for TagName in SpecialPlusList do begin
                        if not ParseTagSpecial(TagName, TagSpecial, TagParam) then
                            continue;
                        Sql := Sql + 'AND ';
                        if SameText(TagSpecial, 'FileName:') then
                            Sql := Sql +
                                   GenerateSqlForFileName(TagParam)
                        else if SameText(TagSpecial, 'FileExt:') then
                            Sql := Sql +
                                   GenerateSqlForFileExt(TagParam)
                        else if SameText(TagSpecial, 'FileDate:') then
                            Sql := Sql +
                                   GenerateSqlForDates(TagParam,
                                                       'ImgIndex.FileDateTime')
                        else if SameText(TagSpecial, 'IndexDate:') then
                            Sql := Sql +
                                   GenerateSqlForDates(TagParam,
                                                       'ImgIndex.IndexDateTime');
                    end;
                end;

                if SpecialMinusList.Count > 0 then begin
                    Flag := FALSE;
                    for TagName in SpecialMinusList do begin
                        if not ParseTagSpecial(TagName, TagSpecial, TagParam) then
                            continue;
                        Sql := Sql + 'AND ';
                        if SameText(TagSpecial, 'FileName:') then
                            Sql := Sql + '(NOT(' +
                                   GenerateSqlForFileName(TagParam) +
                                   '))'
                        else if SameText(TagSpecial, 'FileExt:') then
                            Sql := Sql + '(NOT(' +
                                   GenerateSqlForFileExt(TagParam) +
                                   '))'
                        else if SameText(TagSpecial, 'FileDate:') then
                            Sql := Sql + '(NOT(' +
                                   GenerateSqlForDates(TagParam,
                                                       'ImgIndex.FileDateTime') +
                                   '))'
                        else if SameText(TagSpecial, 'IndexDate:') then
                            Sql := Sql + '(NOT(' +
                                   GenerateSqlForDates(TagParam,
                                                       'ImgIndex.IndexDateTime') +
                                   '))';
                    end;
                end;

                // Remove extra condition we added to ease adding many conditions
                Sql := StringReplace(Sql, 'WHERE (1=1) AND ', 'WHERE ', []);
                if (Length(Sql) > 12) and
                   (SameText(Copy(Sql, Length(Sql) - 12, MAXINT),
                             ' WHERE (1=1) ')) then
                    SetLength(Sql, Length(Sql) - 12);

//                Display(Sql);
                Result := Sql;
                ErrMsg := '';
            finally
                Query.Close;
                Query.Free;
            end;
        finally
            FreeAndNil(TagsPlusIDList);
            FreeAndNil(TagsMinusIDList);
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBMoveTag(
    const TagName      : String;
    const NewParentTag : String;
    out   ErrMsg       : String) : Boolean;
var
    NewParentTagID : Integer;
    TagID          : Integer;
    ParentParentID : Integer;
    Query          : TFDQuery;
    WasConnected   : Boolean;
begin
    Result := FALSE;
    if not OvbImgOrganizerDataModule.DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        TagID := OvbImgOrganizerDataModule.DBGetTagID(TagName, ErrMsg);
        if TagID < 0 then
            Exit;   // Error while searching for tag
        if TagID = 0 then begin
            ErrMsg := Format(_('Tag "%s" doesn''t exist'), [TagName]);
            Exit;
        end;

        NewParentTagID := OvbImgOrganizerDataModule.DBGetTagID(NewParentTag, ParentParentID, ErrMsg);
        if NewParentTagID < 0 then
            Exit;   // Error while searching for new parent tag
        if NewParentTagID = 0 then begin
            ErrMsg := Format(_('Tag "%s" doesn''t exist'), [NewParentTag]);
            Exit;
        end;

        if TagID = NewParentTagID then begin
            // No actual move. That's OK.
            ErrMsg := '';
            Result := TRUE;
            Exit;
        end;

        // We must check that NewParentTag is not a child of TagName
        while (ParentParentID > 1) and (ParentParentID <> TagID) do begin
            ParentParentID := OvbImgOrganizerDataModule.DBGetParentTagID(ParentParentID, ErrMsg);
            if ParentParentID < 0 then
                Exit;     // Error while searching for parent of parent
            if ParentParentID = 0 then begin
                ErrMsg := Format(_('Error in child/parent chain of "%s"'),
                                 [NewParentTag]);
                Exit;
            end;
        end;

        if ParentParentID = TagID then begin
            ErrMsg := Format(_('Tag "%s" is a child of tag "%s"'),
                            [NewParentTag, TagName]);
            Exit;
        end;

        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            Query.SQL.Text := 'UPDATE TagIndex ' +
                              'SET ParentID = :NewParentTagID ' +
                              'WHERE ID = :TagID';
            Query.ParamByName('NewParentTagID').AsInteger := NewParentTagID;
            Query.ParamByName('TagID').AsInteger          := TagID;
            Query.ExecSQL;
            if Query.RowsAffected <= 0 then begin
                ErrMsg := Format(_('Tag "%s" not found'), [TagName]);
                Exit;
            end;
            ErrMsg := '';
            Result := TRUE;
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            OvbImgOrganizerDataModule.DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBRenameTag(
    const OldTagName : String;
    const NewTagName : String;
    out   ErrMsg     : String) : Boolean;
var
    NewTagID     : Integer;
    Query        : TFDQuery;
    WasConnected : Boolean;
begin
    Result := FALSE;
    if not ValidateTagName(NewTagName, ErrMsg) then
        Exit;
    if not OvbImgOrganizerDataModule.DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        NewTagId := OvbImgOrganizerDataModule.DBGetTagID(NewTagName, ErrMsg);
        if NewTagID < 0 then
            Exit;   // Error whle searching new tag name
        if NewTagID > 0 then begin
            ErrMsg := Format(_('Tag "%s" already exists'), [NewTagName]);
            Exit;
        end;
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := FDConnection1;
            Query.SQL.Text := 'UPDATE TagIndex ' +
                              'SET TagName = :NewTagName ' +
                              'WHERE TagName = :OldTagName';
            Query.ParamByName('NewTagName').AsString   := NewTagName;
            Query.ParamByName('OldTagName').AsString   := OldTagName;
            Query.ExecSQL;
            if Query.RowsAffected <= 0 then begin
                ErrMsg := Format(_('Tag "%s" not found'), [OldTagName]);
                Exit;
            end;
            ErrMsg := '';
            Result := TRUE;
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            OvbImgOrganizerDataModule.DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.ValidateTagName(
    const TagName : String;
    var   ErrMsg  : String) : boolean;
var
    J : Integer;
begin
    Result := FALSE;
    if Length(TagName) < 3 then begin
        ErrMsg := _('must be at least 3 characters');
        Exit;
    end;
    if Length(TagName) > 80 then begin
        ErrMsg := _('must be at most 80 characters');
        Exit;
    end;
    if Pos(' ', TagName) > 0 then begin
        ErrMsg := 'contain a space character';
        Exit
    end;
    if Pos(#9, TagName) > 0 then begin
        ErrMsg := 'contain a TAB character';
        Exit
    end;
    for J := Low(TagInvalidChars) to High(TagInvalidChars) do begin
        if Pos(TagInvalidChars[J], TagName) > 0 then begin
            ErrMsg := _('cannot contain one of those characters: ') +
                        TagInvalidChars;
            Exit;
        end;
    end;
    if SameText(TagName, ROOT_TAG_NAME) then begin
        ErrMsg := Format(_('A tag can''t be named "%s"'), [ROOT_TAG_NAME]);
        Exit;
    end;

    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule._(const S: String): String;
begin
    if Assigned(FOnTranslateString) then
        FOnTranslateString(Self, S, Result)
    else
        Result := S;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBLoadCollection(
    const SqlText               : String;
    const More                  : Boolean;
    const CollectionLimit       : Integer;
    const CollectionLimitOffset : Integer;
    const OrderByIndex          : Integer;
    const OrderByDescending     : Boolean;
    out   FCollectionRecCnt     : Integer;
    var   Index                 : Integer;
    var   ErrMsg                : String;
    DataRecordEvent             : TOvbImgDataRecordEvent) : Integer;
var
    Query           : TFDQuery;
    WasConnected    : Boolean;
    DataRecord      : TOvbImgDataRecord;
    ImageFileName   : String;
    ThumbFileName   : String;
begin
    Result := -1;
    ErrMsg := FUnknownError;
    if not OvbImgOrganizerDataModule.DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := OvbImgOrganizerDataModule.FDConnection1;

            if not More then begin
                // First get total number of records
                Query.SQL.Text := 'SELECT Count(*) ' + SqlText;
                Query.Open();
                FCollectionRecCnt := Query.Fields.FieldByNumber(1).AsInteger;
            end;

            Query.SQL.Text :=
                'SELECT ImgIndex.ID, ' +
                       'ImgIndex.FileName || ImgIndex.FileExt as FileName, ' +
                       'ImgIndex.FilePath, ImgIndex.Thumb, ' +
                       'ImgIndex.FileDateTime, ImgIndex.IndexDateTime, ' +
                       'ImgIndex.Latitude, ImgIndex.Longitude, ' +
                       'ImgIndex.Latitude2, ImgIndex.Longitude2, ' +
                       'ImgIndex.Description ' +
                SqlText + ' ' +
                OvbImgOrganizerDataModule.DBSearchGenerateOrderBySql(OrderByIndex,
                                                       OrderByDescending,
                                                       ErrMsg) +
                'LIMIT  ' + IntToStr(CollectionLimit) + ' ' +
                'OFFSET ' + IntToStr(CollectionLimitOffset);
            Query.Open();
            Result := 0;
            while not  Query.Eof do begin
                Result                     := Result + 1;
                DataRecord.ImageID         := Query.FieldByName('ID').AsInteger;
                DataRecord.ImageThumb      := Query.FieldByName('Thumb').AsString;
                ThumbFileName              := FCommonAppData + DataRecord.ImageThumb;
                DataRecord.ImageName       := Query.FieldByName('FileName').AsString;
                DataRecord.ImagePath       := Query.FieldByName('FilePath').AsString;
                DataRecord.ImageDateTime   := Query.FieldByName('FileDateTime').AsDateTime;
                DataRecord.IndexDateTime   := Query.FieldByName('IndexDateTime').AsDateTime;
                DataRecord.Description     := Query.FieldByName('Description').AsString;
                DataRecord.Latitude        := Query.FieldByName('Latitude').AsFloat;
                DataRecord.Longitude       := Query.FieldByName('Longitude').AsFloat;
                DataRecord.Latitude2       := Query.FieldByName('Latitude2').AsFloat;
                DataRecord.Longitude2      := Query.FieldByName('Longitude2').AsFloat;
                ImageFileName   := DataRecord.ImagePath + DataRecord.ImageName;
                if FileExists(ImageFileName) then
                    DataRecordEvent(Self, Index, DataRecord);
                Query.Next;
            end;
            ErrMsg := '';
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            OvbImgOrganizerDataModule.DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBFetchImageTagNames(
    ImageID     : Integer;
    OutTagNames : TStrings;
    var ErrMsg  : String) : Boolean;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
    TagName      : String;
begin
    Result := FALSE;
    ErrMsg := FUnknownError;
    if not Assigned(OutTagNames) then begin
        ErrMsg := _('DBFetchImageTagNames missing argument');
        Exit;
    end;
    OutTagNames.Clear;

    if not OvbImgOrganizerDataModule.DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := OvbImgOrganizerDataModule.FDConnection1;

            Query.SQL.Text := 'SELECT TagIndex.TagName ' +
                              'FROM TagIndex, ImageTag ' +
                              'WHERE (ImageTag.TagID   = TagIndex.ID) AND ' +
                                    '(ImageTag.ImageID = :ImageID) ' +
                              'ORDER BY TagIndex.TagName';
            Query.ParamByName('ImageID').AsInteger := ImageID;
            Query.Open();
            while not Query.Eof do begin
                TagName := Query.Fields.FieldByNumber(1).AsString;
                OutTagNames.Add(TagName);
                Query.Next;
            end;
            Result := TRUE;
            ErrMsg := '';
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            OvbImgOrganizerDataModule.DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBFetchImageInfo(
    const ImageID  : Integer;
    out   FileName : String;
    out   FileExt  : String;
    out   FilePath : String;
    var   ErrMsg   : String): Boolean;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
begin
    Result   := FALSE;
    ErrMsg   := FUnknownError;
    FileName := '';
    FileExt  := '';
    FilePath := '';
    if not OvbImgOrganizerDataModule.DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := OvbImgOrganizerDataModule.FDConnection1;

            Query.SQL.Text := 'SELECT FileName, FileExt, FilePath ' +
                              'FROM ImageIndex ' +
                              'WHERE ID = :ImageID';
            Query.ParamByName('ImageID').AsInteger := ImageID;
            Query.Open();
            if Query.Eof then begin
                ErrMsg   := Format(_('ImageID %d not found'), [ImageID]);
                Exit;
            end;
            FileName := Query.Fields.FieldByNumber(1).AsString;
            FileExt  := Query.Fields.FieldByNumber(2).AsString;
            FilePath := Query.Fields.FieldByNumber(3).AsString;
            ErrMsg   := '';
            Result   := TRUE;
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            OvbImgOrganizerDataModule.DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBFetchImageTagNames(
    const FileName : String;
    out   ImageID  : Integer;
    OutTagNames    : TStrings;
    var   ErrMsg   : String) : Boolean;
var
    Query        : TFDQuery;
    WasConnected : Boolean;
    TagName      : String;
begin
    Result := FALSE;
    ErrMsg := FUnknownError;
    OutTagNames.Clear;
    ImageID := -1;
    if not OvbImgOrganizerDataModule.DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            Query.Connection := OvbImgOrganizerDataModule.FDConnection1;

            Query.SQL.Text := 'SELECT TagIndex.TagName, ImageTag.ImageID ' +
                              'FROM ImageIndex, ImageTag ' +
                              'JOIN TagIndex ON TagIndex.ID = ImageTag.TagID ' +
                              'WHERE ImageIndex.FileName = :FileName ' + (*COLLATE NOCASE*) ' ' +
                                'AND ImageIndex.FileExt  = :FileExt  ' + (*COLLATE NOCASE*) ' ' +
                                'AND ImageTag.ImageID    = ImageIndex.ID ' +
                              'ORDER BY TagIndex.TagName';
            Query.ParamByName('FileName').AsString := ChangeFileExt(ExtractFileName(FileName), '');
            Query.ParamByName('FileExt').AsString  := ExtractFileExt(FileName);
            Query.Open();
            if not Query.Eof then begin
                ImageID := Query.Fields.FieldByNumber(2).AsInteger;
                while not Query.Eof do begin
                    TagName := Query.Fields.FieldByNumber(1).AsString;
                    OutTagNames.Add(TagName);
                    Query.Next;
                end;
            end;
            ErrMsg := '';
            Result := TRUE;
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            OvbImgOrganizerDataModule.DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerDataModule.Display(const Msg: String);
begin
    if Assigned(OnDisplay) then
        OnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerDataModule.Display(
    const Msg : String;
    Args      : array of const);
begin
    Display(Format(Msg, Args));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOvbImgOrganizerDataModule.DisplayHandler(
    Sender    : TObject;
    const Msg : String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsFileDropAllowed(
    const FileName : String) : Boolean;
var
    Ext      : String;
begin
    Result := FALSE;
    for Ext in AllowedExtentions do begin
        if SameText(ExtractFileExt(FileName), Ext) then begin
            Result := TRUE;
            break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsFileDropAllowed(
    const FileNames : array of String) : Boolean;
var
    FileName : String;
begin
    Result := FALSE;
    for FileName in FileNames do begin
        if IsFileDropAllowed(FileName) then begin
            Result := TRUE;
            break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBCompareImageIndexWidthDirContent(
    const SrcDir       : String;
    const TableName    : String;
    const Recursive    : Boolean;
    const ProgressProc : TOvbImgProgressProc;
    var   ErrMsg       : String) : Boolean;
var
    WasConnected  : Boolean;
    Query         : TFDQuery;
    Count         : Integer;
    CountConflict : Integer;
    CountRec      : Integer;
    Abort         : Boolean;
begin
    Result := FALSE;
    ErrMsg := FUnknownError;
    Abort  := FALSE;
    if not DBConnect(WasConnected, ErrMsg) then
        Exit;
    try
        Query := TFDQuery.Create(nil);
        try
            try
                Query.Connection := FDConnection1;

                if not DBCreateTable(
                    Query,
                    TableName,
                    'CREATE TABLE ' + TableName + ' (' +
                    'ID            INTEGER primary key autoincrement, ' +
                    'FileName      VARCHAR (80)  not null COLLATE NOCASE, ' + // Without extension
                    'FileExt       VARCHAR (16)           COLLATE NOCASE, ' + // Include dot
                    'FilePath      VARCHAR (256) not null COLLATE NOCASE)',   // End with backslash
                    FALSE,
                    ErrMsg) then
                    Exit;
                Display('Scannig directory "%s"...', [SrcDir]);
                Count         := 0;
                CountConflict := 0;
                CountRec      := 0;
                Result        := DBScanDir(SrcDir, TableName, Recursive, Query,
                                           ProgressProc, Count, ErrMsg);
                if not Result then
                    Exit;
                Display('Found %d images', [Count]);

                Query.SQL.Text :=
                    'SELECT Idx.FilePath CollectionPath, ' +
                            'TmpIdx.FilePath DiskPath, ' +
                            'TmpIdx.FileName, ' +
                            'TmpIdx.FileExt ' +
                    'FROM ImageIndex Idx, ' + TableName + ' TmpIdx ' +
                    'WHERE (Idx.FileName = TmpIdx.FileName) ' +
                    '  AND (Idx.FilePath <> TmpIdx.FilePath) ' +
                    'ORDER BY TmpIdx.FileName';
                Query.Open;
                while not Query.Eof do begin
                    Inc(CountRec);
                    if Assigned(ProgressProc) then begin
                        ProgressProc(Self, 2, CountRec, Abort);
                        if Abort then
                            Exit;
                    end;
                    Inc(CountConflict);
                    Display('%s %s%s%s',
                            [Query.FieldByName('CollectionPath').AsString,
                             Query.FieldByName('DiskPath').AsString,
                             Query.FieldByName('FileName').AsString,
                             Query.FieldByName('FileExt').AsString]);
                    Query.Next;
                end;
                Display(_('Found %d conflict.'), [CountConflict]);
                Query.SQL.Text := 'DROP TABLE ' + TableName;
                Query.ExecSQL;
            except
                on E:Exception do begin
                    Display(E.ClassName + ': ' + E.Message);
                    ErrMsg := E.ClassName + ': ' + E.Message;
                    Result := FALSE;
                end;
            end;
        finally
            Query.Close;
            Query.Free;
        end;
    finally
        if not WasConnected then
            DBDisconnect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOvbImgOrganizerDataModule.DBScanDir(
    const SrcDir       : String;
    const TableName    : String;
    const Recursive    : Boolean;
    const Query        : TFDQuery;
    const ProgressProc : TOvbImgProgressProc;
    var   Count        : Integer;
    var   ErrMsg       : String) : Boolean;
var
    FromDir   : String;
    Buf       : String;
    FD        : TWIN32FindData;
    FH        : THandle;
    More      : Boolean;
    ImagePath : String;
    ImageName : String;
    ImageExt  : String;
    Abort     : Boolean;
begin
    FromDir := IncludeTrailingPathDelimiter(SrcDir);
    Abort   := FALSE;
    Result  := FALSE;

    // Loop on all files
    Buf  := FromDir + '*.*';
    FH   := WinApi.Windows.FindFirstFile(PChar(Buf), FD);
    try
        More := FH <> INVALID_HANDLE_VALUE;
        while More do begin
            if (FD.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
                if Assigned(ProgressProc) then begin
                    ProgressProc(Self, 1, Count, Abort);
                    if Abort then
                        Exit;
                end;
                if IsFileDropAllowed(FD.cFileName) then begin
                    Inc(Count);
//                    Display(FromDir + FD.cFileName);
                    ImagePath := FromDir;
                    ImageName := ChangeFileExt(FD.cFileName, '');
                    ImageExt  := ExtractFileExt(FD.cFileName);
                    Query.SQL.Text := 'INSERT INTO ' + TableName + ' ' +
                                      '(ID, FilePath, FileName, FileExt) ' +
                                      ' VALUES ' +
                                      '(:ID, :FilePath, :FileName, :FileExt)';
                    // ID is an autoincrement column,
                    // we must insert NULL to make it work!
                    Query.ParamByName('ID').DataType := ftInteger;
                    Query.ParamByName('ID').Clear;
                    Query.ParamByName('FilePath').AsString        := ImagePath;
                    Query.ParamByName('FileName').AsString        := ImageName;
                    Query.ParamByName('FileExt').AsString         := ImageExt;
                    Query.ExecSQL;
                end;
            end;
            More := WinApi.Windows.FindNextFile(FH, FD);
        end;
    finally
        WinApi.Windows.FindClose(FH);
    end;

    // Loop on all directries
    if Recursive then begin
        Buf  := IncludeTrailingPathDelimiter(SrcDir) + '*.*';
        FH   := WinApi.Windows.FindFirstFile(PChar(Buf), FD);
        try
            More := FH <> INVALID_HANDLE_VALUE;
            while More do begin
                if (FD.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then begin
                    if (String(FD.cFileName) <> '.') and
                       (String(FD.cFileName) <> '..') then begin
                        Result := DBScanDir(
                                       FromDir + FD.cFileName,
                                       TableName,
                                       Recursive,
                                       Query,
                                       ProgressProc,
                                       Count,
                                       ErrMsg);
                        if not Result then
                            Exit;
                    end
                end;
                More := WinApi.Windows.FindNextFile(FH, FD);
            end;
        finally
            WinApi.Windows.FindClose(FH);
        end;
    end;
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
