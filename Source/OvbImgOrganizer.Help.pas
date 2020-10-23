{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE @ OverByte
Creation:     May 17, 2020
Description:  Simple Image Organizer - Help form
License:      This program is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OvbImgOrganizer.Help;

interface

uses
    Winapi.Messages,
    System.Variants, System.Classes,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
    GnuGetText;

type
    THelpForm = class(TForm)
        OKButton: TButton;
        RichEdit1: TRichEdit;
        procedure OKButtonClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
    public
        constructor Create(AOwner : TComponent); override;
    end;


implementation

uses
    System.SysUtils;

{$R *.dfm}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THelpForm.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    try
        TranslateComponent(Self);
    except

    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THelpForm.FormShow(Sender: TObject);
var
    FileName : String;
const
    HELP_FILE = 'OvbImgOrganizer.Help.rtf';
begin
    FileName := HELP_FILE;
    // Search for the help file in the current directory or parent
    // up to 3 levels to accomodate with development environment.
    if not FileExists(FileName) then begin
        FileName := '..\' + HELP_FILE;
        if not FileExists(FileName) then begin
            FileName := '..\' + HELP_FILE;
            if not FileExists(FileName) then begin
                FileName := '..\' + HELP_FILE;
                if not FileExists(FileName) then begin
                    FileName := '..\..\..\Source\' + HELP_FILE;
                    if not FileExists(FileName) then begin
                        ShowMessage(Format('Help "%s" file not found',
                                           [HELP_FILE]));
                        Exit;
                    end;
                end;
            end;
        end;
    end;

    RichEdit1.Lines.LoadFromFile(FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THelpForm.OKButtonClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
