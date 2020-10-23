{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE @ OverByte
Creation:     May 17, 2020
Description:  Simple Image Organizer - Directory scan form
              Used to find images with filename already in database but
              not same path.
License:      This program is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OvbImgOrganizer.ImageScanDir;

interface

uses
    Winapi.Messages,
    System.Variants, System.Classes,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
    Vcl.FileCtrl,
    GnuGetText;

type
    TScanDirForm = class(TForm)
        Label1: TLabel;
        Label2: TLabel;
        ScanDirEdit: TEdit;
        ScanRecursiveCheckBox: TCheckBox;
        OKButton: TButton;
        CancelButton: TButton;
        BrowseButton: TButton;
        procedure OKButtonClick(Sender: TObject);
        procedure CancelButtonClick(Sender: TObject);
        procedure BrowseButtonClick(Sender: TObject);
    private
        function  GetScanDir: String;
        procedure SetScanDir(const Value: String);
        function  GetRecursive: Boolean;
        procedure SetRecursive(const Value: Boolean);
    public
        constructor Create(AOwner : TComponent); override;
        property ScanDir                      : String
                                                read  GetScanDir
                                                write SetScanDir;
        property Recursive                    : Boolean
                                                read  GetRecursive
                                                write SetRecursive;
    end;

implementation

uses
    System.SysUtils;

{$R *.dfm}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TScanDirForm.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    try
        TranslateComponent(Self);
    except

    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TScanDirForm.GetRecursive: Boolean;
begin
    Result := ScanRecursiveCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScanDirForm.SetRecursive(const Value: Boolean);
begin
    ScanRecursiveCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TScanDirForm.GetScanDir: String;
begin
    Result := Trim(ScanDirEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScanDirForm.SetScanDir(const Value: String);
begin
    ScanDirEdit.Text := Trim(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScanDirForm.OKButtonClick(Sender: TObject);
begin
    Close;
    ModalResult := mrOK;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScanDirForm.BrowseButtonClick(Sender: TObject);
var
    Dir : String;
begin
    Dir := IncludeTrailingPathDelimiter(Trim(ScanDirEdit.Text));
    if not Vcl.FileCtrl.SelectDirectory(
                 _('Select directory to scan'),
                 '',
                 Dir,
                 [sdNewFolder, sdShowEdit, sdShowShares,
                  sdNewUI, sdShowFiles, sdValidateDir
                  ],
                 nil) then
        Exit;
    ScanDirEdit.Text     := Trim(Dir);
    ScanDirEdit.SelStart := Length(ScanDirEdit.Text);
    ActiveControl        := ScanDirEdit;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScanDirForm.CancelButtonClick(Sender: TObject);
begin
    Close;
    ModalResult := mrCancel;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
