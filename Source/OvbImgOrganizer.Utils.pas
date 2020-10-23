{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE @ OverByte
Creation:     May 17, 2020
Description:  Simple Image Organizer -Support functions
License:      This program is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OvbImgOrganizer.Utils;

interface

uses
    WinApi.Windows, System.IOUtils;

function GetModuleFileName(
    Instance : THandle) : String;
function GetBuildInfo(
    const AFilename: String;
    var V1, V2, V3, V4: Word): Boolean; overload;
function GetBuildInfo(
    const AFilename : String;
    var   VerString : String;
    const Len       : Integer = 4): Boolean; overload;
function GetFileSize(const FileName: String): Int64;
function TimeZoneBias : String;
function LocalDateTimeFromUTCDateTime(
    const UTCDateTime: TDateTime): TDateTime;
function CRC16(
    const S: String): Word;
function CompareFileContent(const Filename1, FileName2 : String) : Boolean;
function DoubleQuotes(const S : String) : String;

implementation

uses
    SysUtils;

// Table according to CRC-CCITT (XModem)
// Width=16 poly=0x1021 init=0x0000 refin=false refout=false
// xorout=0x0000 check=0x31c3 name="XMODEM"
// http://antirez.com/misc/lcov-html/redis/src/crc16.c.gcov.html
const
   Crc16Tab: array [0..255] of Word = (
     $0000, $1021, $2042, $3063, $4084, $50a5, $60c6, $70e7, $8108, $9129, $a14a, $b16b, $c18c, $d1ad, $e1ce, $f1ef,
     $1231, $0210, $3273, $2252, $52b5, $4294, $72f7, $62d6, $9339, $8318, $b37b, $a35a, $d3bd, $c39c, $f3ff, $e3de,
     $2462, $3443, $0420, $1401, $64e6, $74c7, $44a4, $5485, $a56a, $b54b, $8528, $9509, $e5ee, $f5cf, $c5ac, $d58d,
     $3653, $2672, $1611, $0630, $76d7, $66f6, $5695, $46b4, $b75b, $a77a, $9719, $8738, $f7df, $e7fe, $d79d, $c7bc,
     $48c4, $58e5, $6886, $78a7, $0840, $1861, $2802, $3823, $c9cc, $d9ed, $e98e, $f9af, $8948, $9969, $a90a, $b92b,
     $5af5, $4ad4, $7ab7, $6a96, $1a71, $0a50, $3a33, $2a12, $dbfd, $cbdc, $fbbf, $eb9e, $9b79, $8b58, $bb3b, $ab1a,
     $6ca6, $7c87, $4ce4, $5cc5, $2c22, $3c03, $0c60, $1c41, $edae, $fd8f, $cdec, $ddcd, $ad2a, $bd0b, $8d68, $9d49,
     $7e97, $6eb6, $5ed5, $4ef4, $3e13, $2e32, $1e51, $0e70, $ff9f, $efbe, $dfdd, $cffc, $bf1b, $af3a, $9f59, $8f78,
     $9188, $81a9, $b1ca, $a1eb, $d10c, $c12d, $f14e, $e16f, $1080, $00a1, $30c2, $20e3, $5004, $4025, $7046, $6067,
     $83b9, $9398, $a3fb, $b3da, $c33d, $d31c, $e37f, $f35e, $02b1, $1290, $22f3, $32d2, $4235, $5214, $6277, $7256,
     $b5ea, $a5cb, $95a8, $8589, $f56e, $e54f, $d52c, $c50d, $34e2, $24c3, $14a0, $0481, $7466, $6447, $5424, $4405,
     $a7db, $b7fa, $8799, $97b8, $e75f, $f77e, $c71d, $d73c, $26d3, $36f2, $0691, $16b0, $6657, $7676, $4615, $5634,
     $d94c, $c96d, $f90e, $e92f, $99c8, $89e9, $b98a, $a9ab, $5844, $4865, $7806, $6827, $18c0, $08e1, $3882, $28a3,
     $cb7d, $db5c, $eb3f, $fb1e, $8bf9, $9bd8, $abbb, $bb9a, $4a75, $5a54, $6a37, $7a16, $0af1, $1ad0, $2ab3, $3a92,
     $fd2e, $ed0f, $dd6c, $cd4d, $bdaa, $ad8b, $9de8, $8dc9, $7c26, $6c07, $5c64, $4c45, $3ca2, $2c83, $1ce0, $0cc1,
     $ef1f, $ff3e, $cf5d, $df7c, $af9b, $bfba, $8fd9, $9ff8, $6e17, $7e36, $4e55, $5e74, $2e93, $3eb2, $0ed1, $1ef0);

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CRC16(const S: String): Word;
var
    I: Integer;
begin
    Result := 0;
    for I := 1 to Length(S) do
        Result := Word(CRC16Tab[((Result shr 8) xor Byte(Ord(S[I]))) and $ff] xor (Result shl 8));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LocalDateTimeFromUTCDateTime(
    const UTCDateTime: TDateTime): TDateTime;
var
    LocalSystemTime : TSystemTime;
    UTCSystemTime   : TSystemTime;
    LocalFileTime   : TFileTime;
    UTCFileTime     : TFileTime;
begin
    DateTimeToSystemTime(UTCDateTime, UTCSystemTime);
    SystemTimeToFileTime(UTCSystemTime, UTCFileTime);
    if FileTimeToLocalFileTime(UTCFileTime, LocalFileTime) and
       FileTimeToSystemTime(LocalFileTime, LocalSystemTime) then
        Result := SystemTimeToDateTime(LocalSystemTime)
    else
        // Default to UTC if any conversion function fails.
        Result := UTCDateTime;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TimeZoneBias : String;
const
    Time_Zone_ID_DayLight = 2;
var
    TZI       : tTimeZoneInformation;
    TZIResult : Integer;
    aBias     : Integer;
begin
    TZIResult := GetTimeZoneInformation(TZI);
    if TZIResult = -1 then
        Result := '-0000'
    else begin
         if TZIResult = Time_Zone_ID_DayLight then   { 10/05/99 }
             aBias := TZI.Bias + TZI.DayLightBias
         else
             aBias := TZI.Bias + TZI.StandardBias;
         Result := Format('-%.2d:%.2d', [Abs(aBias) div 60, Abs(aBias) mod 60]);
         if aBias < 0 then
             Result[1] := '+';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetModuleFileName(Instance : THandle) : String;
begin
    SetLength(Result, 256);
    SetLength(Result, WinApi.Windows.GetModuleFileName(
                          Instance, PChar(Result), Length(Result)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetBuildInfo(
    const AFilename: String;
    var V1, V2, V3, V4: Word): Boolean;
var
    VerInfoSize  : Integer;
    VerValueSize : DWORD;
    Dummy        : DWORD;
    VerInfo      : Pointer;
    VerValue     : PVSFIXEDFILEINFO;
begin
    VerInfoSize := GetFileVersionInfoSize(PChar(AFilename), Dummy);
    Result      := FALSE;
    if VerInfoSize <> 0 then begin
        GetMem(VerInfo, VerInfoSize);
        try
            if GetFileVersionInfo(PChar(AFilename), 0,
                                  VerInfoSize, VerInfo) then begin
                if VerQueryValue(VerInfo, '\', Pointer(VerValue),
                                 VerValueSize) then begin
                    V1 := VerValue.dwFileVersionMS shr 16;
                    V2 := VerValue.dwFileVersionMS and $FFFF;
                    V3 := VerValue.dwFileVersionLS shr 16;
                    V4 := VerValue.dwFileVersionLS and $FFFF;
                    Result := TRUE;
                end;
            end;
        finally
            FreeMem(VerInfo, VerInfoSize);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetBuildInfo(
    const AFilename : String;
    var   VerString : String;
    const Len       : Integer = 4): Boolean;
var
    V1, V2, V3, V4: Word;
begin
    Result := GetBuildInfo(AFileName, V1, V2, V3, V4);
    if Result then begin
        case Len of
        1:
            VerString := IntToStr(V1);
        2:
            VerString := IntToStr(V1) + '.' +
                         IntToStr(V2);
        3:
            VerString := IntToStr(V1) + '.' +
                         IntToStr(V2) + '.' +
                         IntToStr(V3);
        else
            VerString := IntToStr(V1) + '.' +
                         IntToStr(V2) + '.' +
                         IntToStr(V3) + '.' +
                         IntToStr(V4);
        end;
    end
    else
        VerString := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetFileSize(const FileName: String): Int64;
var
    SearchRec: TSearchRec;
begin
    if SysUtils.FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0 then begin
        Result := SearchRec.Size;
    end
    else
        Result := -1;
    SysUtils.FindClose(SearchRec);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CompareFileContent(const Filename1, FileName2 : String) : Boolean;
var
    Data1 : TBytes;
    Data2 : TBytes;
    Size1 : Int64;
    Size2 : Int64;
begin
    Size1 := GetFileSize(FileName1);
    Size2 := GetFileSize(FileName2);
    if Size1 <> Size2 then begin
        Result := FALSE;
        Exit;
    end;

    Data1 := TFile.ReadAllBytes(FileName1);
    Data2 := TFile.ReadAllBytes(FileName2);
    Result := CompareMem(@Data1[0], @Data2[0], Length(Data1));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DoubleQuotes(const S : String) : String;
begin
    Result := StringReplace(S, '"', '""', [rfReplaceAll]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
