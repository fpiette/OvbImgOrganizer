{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE @ OverByte
Creation:     May 05, 2012
Description:  A form acting as an enhanced MessageBox.
License:      This source code is published under MOZILLA PUBLIC LICENSE V2.0;
              you may not use this file except in compliance with the License.
              You may obtain a copy of the License at
              https://www.mozilla.org/en-US/MPL/2.0/
Version:      1.00
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit SuperMessageForm;

interface

uses
    WinApi.Windows, Winapi.Messages,
    System.Variants, System.Classes, System.Math,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms,
    Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls,
    Vcl.Imaging.PngImage,
//    ProgressBarWithText,
    Generics.Collections;

const
    WM_APP_STARTUP       = WM_USER + 1;
    WM_FORCE_CLOSE       = WM_USER + 2;

type
    TStartProc = procedure (Sender : TObject) of object;
    TMessageFormFlag = (mffStop,
                        mffOK,      // OK button alone
                        mffOKTick,  // OK tick button alone
                        mffNoAutoClose,
                        mffYesNo, mffYesNoCancel, mffOkCancel, mffFailed,
                        //mffProgressBar,
                        mffWait,    // Wait icon not clickable
                        mffWaitOK,  // Wait icon clickable
                        mffWarning,
                        mffNoTopMost,
                        mffInfo,    // Show "i" icon on the left
                        mffAutoWidth,
                        mffCourierNew,
                        mffAlignLeft,
                        mffInternalNoTopMost);
    TMessageFormFlags = set of TMessageFormFlag;
    TMessageFormCallbackContext = (mfccStartup, mfccOK, mfccCancel,
                                   mfccYes, mfccNO, mfccOkTick, mfccStop);
    TMessageFormCallback = procedure (Sender        : TObject;
                                      const Context : TMessageFormCallbackContext) of object;
    TMessageFormCheckProc = procedure (Sender   : TObject;
                                       var Done : Boolean;
                                       Tag      : Cardinal) of object;
    TTranslateStringEvent = procedure (Sender  : TObject;
                                       var Msg : String) of object;
    TTranslateComponentEvent = procedure (Sender  : TObject) of object;

    TMessageForm = class(TForm)
        StopImage: TImage;
        OKTickImage: TImage;
        NoBitBtn: TBitBtn;
        YesBitBtn: TBitBtn;
        OKBitBtn: TBitBtn;
        CancelBitBtn: TBitBtn;
        FailedImage: TImage;
        InfoMemo: TMemo;
        FillerPanel: TPanel;
        WaitImage: TImage;
        WarningImage: TImage;
        CheckProcTimer: TTimer;
        TopMostTimer: TTimer;
//        ProgressBarWithText1: TProgressBarWithText;
        procedure OKTickImageClick(Sender: TObject);
        procedure YesBitBtnClick(Sender: TObject);
        procedure NoBitBtnClick(Sender: TObject);
        procedure CancelBitBtnClick(Sender: TObject);
        procedure OKBitBtnClick(Sender: TObject);
        procedure FailedImageClick(Sender: TObject);
        procedure StopImageClick(Sender: TObject);
        procedure WaitImageClick(Sender: TObject);
        procedure CheckProcTimerTimer(Sender: TObject);
        procedure TopMostTimerTimer(Sender: TObject);
    private
        class var FOnTranslateString      : TTranslateStringEvent;
        class var FOnTranslateComponent   : TTranslateComponentEvent;
    private
        FOnStartup       : TNotifyEvent;
        FOnCallBack      : TMessageFormCallback;
        FStartProc       : TStartProc;
        FCheckProc       : TMessageFormCheckProc;
        FFlags           : TMessageFormFlags;
        FResizeItemsFlag : Boolean;
        FInfoText        : TStringlist;
        FData            : UIntPtr;
        procedure WMAppStartup(var Msg: TMessage); message WM_APP_STARTUP;
        procedure WMForceClose(var Msg: TMessage); message WM_FORCE_CLOSE;
        procedure TriggerCallBack(const Context : TMessageFormCallbackContext);
        function  GetInfoText: String;
        procedure SetInfoText(const Value: String);
        procedure ResizeItems;
        function  GetTopMostTimerInterval: Cardinal;
        procedure SetTopMostTimerInterval(const Value: Cardinal);
    protected
        function _(const Msg : String) : String;  // For translation using dxGetText
        procedure DoShow; override;
        procedure DoClose(var Action: TCloseAction); override;
        procedure Resize; override;
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        class var GForm     : TMessageForm;
        class var GForm2    : TMessageForm;
        class var GFormList : TObjectList<TMessageForm>;
        class function ShowMessage(
                           const MsgText      : String;
                           const MsgCaption   : String;
                           const MsgStartProc : TStartProc;
                           const MsgFlags     : TMessageFormFlags;
                           const CallBack     : TMessageFormCallback = nil;
                           const CheckProc    : TMessageFormCheckProc = nil;
                           const TopLeft      : PPoint = nil)
                             : Integer; overload;
        class function ShowMessage(
                           const MsgFormat    : String;
                           const MsgArgs      : array of const;
                           const MsgCaption   : String;
                           const MsgFlags     : TMessageFormFlags)
                             : Integer; overload;
        class function  DisableLastTopMost: TMessageForm; static;
        class procedure EnableTopMostForm(OForm: TMessageForm); static;
        procedure ReplaceButton(const BtnSrc, BtnBy : TMessageFormFlag);
        procedure ForceClose;
        property StartProc  : TStartProc         read  FStartProc
                                                 write FStartProc;
        property CheckProc  : TMessageFormCheckProc
                                                 read  FCheckProc
                                                 write FCheckProc;
        property Flags      : TMessageFormFlags  read  FFlags
                                                 write FFlags;
        property InfoText   : String             read  GetInfoText
                                                 write SetInfoText;
        property Data       : UIntPtr            read  FData
                                                 write FData;
        property TopMostTimerInterval : Cardinal read  GetTopMostTimerInterval
                                                 write SetTopMostTimerInterval;
//        property ProgressBarWithText : TProgressBarWithText
//                                                 read  ProgressBarWithText1
//                                                 write ProgressBarWithText1;
        property OnStartup  : TNotifyEvent       read  FOnStartup
                                                 write FOnStartup;
        property OnCallBack : TMessageFormCallback
                                                 read  FOnCallBack
                                                 write FOnCallBack;
    public
        class property OnTranslateString      : TTranslateStringEvent
                                                read  FOnTranslateString
                                                write FOnTranslateString;
        class property OnTranslateComponent   : TTranslateComponentEvent
                                                read  FOnTranslateComponent
                                                write FOnTranslateComponent;
    end;

implementation

uses
    System.SysUtils;

{$R *.dfm}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMessageForm.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    NoBitBtn.Caption      := _('NO');
    OkBitBtn.Caption      := _('OK');
    YesBitBtn.Caption     := _('YES');
    CancelBitBtn.Caption  := _('Cancel');
    FInfoText             := TStringList.Create;
    if Assigned(GFormList) then
        GFormList.Add(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMessageForm.Destroy;
begin
    if Assigned(GFormList) then
        GFormList.Remove(Self);
    FreeAndNil(FInfoText);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.DoClose(var Action: TCloseAction);
begin
    inherited DoClose(Action);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.FailedImageClick(Sender: TObject);
begin
    TriggerCallBack(mfccOK);
    Close;
    ModalResult := mrOK;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.ForceClose;
begin
    PostMessage(Handle, WM_FORCE_CLOSE, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.Resize;
begin
    ResizeItems;
    inherited Resize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.DoShow;
begin
    ResizeItems;
    TriggerCallBack(mfccStartup);
    PostMessage(Handle, WM_APP_STARTUP, 0, 0);
    inherited DoShow;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.WMAppStartup(var Msg: TMessage);
begin
    Update;
//    ProgressBarWithText1.Min := 0;
//    ProgressBarWithText1.Max := 100;
    try
        if Assigned(FOnStartup) then
            FOnStartup(Self);
    except
        on E:Exception do
            Vcl.Dialogs.ShowMessage(E.ClassName + ': ' + E.Message);
    end;
    if Assigned(FStartProc) then begin
        try
            FStartProc(Self);
        except
            on E:Exception do
                Vcl.Dialogs.ShowMessage(E.ClassName + ': ' + E.Message);
        end;
        if not (mffNoAutoClose in FFlags) then begin
            Close;
            ModalResult := mrOK;
        end;
    end;
    if Assigned(FCheckProc) then begin
        CheckProcTimer.Enabled := TRUE;
        CheckProcTimerTimer(nil);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.WMForceClose(var Msg: TMessage);
begin
    CheckProcTimer.Enabled := FALSE;
    Close;
    ModalResult := mrOK;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.CheckProcTimerTimer(Sender: TObject);
var
    Done : Boolean;
begin
    CheckProcTimer.Enabled := FALSE;
    if Assigned(FCheckProc) then begin
        Done := FALSE;
        FCheckProc(Self, Done, 0);
        if Done then begin
            Close;
            ModalResult := mrOK;
            Exit;
        end;
    end;
    CheckProcTimer.Enabled := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.TopMostTimerTimer(Sender: TObject);
begin
    if mffNoTopMost in FFlags then begin
        SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0,
                     SWP_NoMove or SWP_NoSize);
        TopMostTimer.Enabled := FALSE;
    end
    else begin
        ShowWindow(Handle, SW_RESTORE);
        SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0,
                     SWP_NoMove or SWP_NoSize);
        SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0,
                     SWP_NoMove or SWP_NoSize);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.TriggerCallBack(const Context: TMessageFormCallbackContext);
begin
    try
        if Assigned(FOnCallBack) then
            FOnCallBack(Self, Context);
    except
        on E:Exception do
            Vcl.Dialogs.ShowMessage(E.ClassName + ': ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class function TMessageForm.DisableLastTopMost : TMessageForm;
var
    I : Integer;
begin
    Result := nil;
    for I := GFormList.Count - 1 downto 0 do begin
        if (not (mffNoTopMost         in GFormList[I].Flags)) and
           (not (mffInternalNoTopMost in GFormList[I].Flags)) then begin
            Result                      := GFormList[I];
            Result.TopMostTimer.Enabled := FALSE;
            Result.Flags                := Result.Flags + [mffInternalNoTopMost];
            SetWindowPos(Result.Handle, HWND_NOTOPMOST,
                         0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
            break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class procedure TMessageForm.EnableTopMostForm(OForm : TMessageForm);
begin
    if not Assigned(OForm) then
        Exit;
    if (GFormList.IndexOf(OForm) >= 0) and
       (not (mffNoTopMost in OForm.Flags)) then begin
         OForm.Flags                := OForm.Flags - [mffInternalNoTopMost];
         OForm.TopMostTimer.Enabled := TRUE;
         SetWindowPos(OForm.Handle, HWND_TOPMOST,
                      0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class function TMessageForm.ShowMessage(
    const MsgFormat    : String;
    const MsgArgs      : array of const;
    const MsgCaption   : String;
    const MsgFlags     : TMessageFormFlags): Integer;
begin
    Result := ShowMessage(Format(MsgFormat, MsgArgs), MsgCaption,
                          nil, MsgFlags, nil, nil, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class function TMessageForm.ShowMessage(
    const MsgText      : String;
    const MsgCaption   : String;
    const MsgStartProc : TStartProc;
    const MsgFlags     : TMessageFormFlags;
    const CallBack     : TMessageFormCallback  = nil;
    const CheckProc    : TMessageFormCheckProc = nil;
    const TopLeft      : PPoint                = nil): Integer;
var
    Form  : TMessageForm;
    OForm : TMessageForm;
    MF    : TForm;
begin
    // Avoid having two TopMost forms at the same time
    OForm := DisableLastTopMost;
    try
        Form := TMessageForm.Create(nil);
        try
            GForm2 := GForm;
            GForm  := Form;
            Form.TopMostTimer.Enabled := not (mffNoTopMost in MsgFlags);
            MF                        := Application.MainForm;
            Form.Caption              := MsgCaption;
            Form.Flags                := MsgFlags;
            Form.InfoText             := TrimRight(MsgText);
            if TopLeft = nil then begin
                if Assigned(MF) then begin
                    // Center of mainform
                    Form.Top          := MF.Top  + (MF.Height - Form.Height) div 2;
                    Form.Left         := MF.Left + (MF.Width  - Form.Width)  div 2;
                end
                else begin
                    // Center of screen
                    Form.Top := Screen.Monitors[0].Top  + (Screen.Monitors[0].Height - Form.Height) div 2;
                    Form.Left:= Screen.Monitors[0].Left + (Screen.Monitors[0].Width  - Form.Width)  div 2;
                end;
            end
            else begin
                Form.Top              := TopLeft.Y;
                Form.Left             := TopLeft.X;
            end;
            Form.MakeFullyVisible(nil);
            Form.StartProc            := MsgStartProc;
            Form.OnCallBack           := CallBack;
            Form.CheckProc            := CheckProc;
            Result                    := Form.ShowModal;
        finally
            GForm := GForm2;
            GForm2 := nil;
            FreeAndNil(Form);
        end;
    finally
        EnableTopMostForm(OForm);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.ReplaceButton(const BtnSrc, BtnBy: TMessageFormFlag);
var
    ImgSrc : TImage;
    ImgBy  : TImage;
begin
    if BtnSrc = mffStop then
        ImgSrc := StopImage
    else if BtnSrc = mffOKTick then
        ImgSrc := OKTickImage
    else if BtnSrc = mffFailed then
        ImgSrc := FailedImage
    else if BtnSrc = mffWarning then
        ImgSrc := WarningImage
    else if BtnSrc = mffWait then
        ImgSrc := WaitImage
    else if BtnSrc = mffInfo then
        ImgSrc := WaitImage        // Info is same image as Wait, different position
    else
        ImgSrc := nil;

    if BtnBy = mffStop then
        ImgBy := StopImage
    else if BtnBy = mffOKTick then
        ImgBy := OKTickImage
    else if BtnBy = mffFailed then
        ImgBy := FailedImage
    else if BtnBy = mffWarning then
        ImgBy := WarningImage
    else if BtnBy = mffWait then
        ImgBy := WaitImage
    else if BtnBy = mffInfo then
        ImgBy := WaitImage
    else
        ImgBy := nil;

    if (not Assigned(ImgSrc)) or (not Assigned(ImgBy)) then
        Exit;

    ImgBy.Top      := ImgSrc.Top;
    ImgBy.Left     := ImgSrc.Left;
    ImgSrc.Visible := FALSE;
    ImgBy.Visible  := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.ResizeItems;
var
    LowestComponentBottom : Integer;
begin
    if FResizeItemsFlag then
        Exit;
    FResizeItemsFlag := TRUE;
    try
        if mffNoTopMost in FFlags then
            SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0,
                         SWP_NoMove or SWP_NoSize)
        else
            SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0,
                         SWP_NoMove or SWP_NoSize);

        WarningImage.Visible  := (mffWarning in FFlags) and (not (mffInfo in FFlags));
        WaitImage.Visible     := (mffInfo in FFlags) and (not (mffWarning in FFlags));
        if (mffWarning in FFlags) or (mffInfo in FFlags) then begin
            InfoMemo.Left   := WarningImage.Width;
            InfoMemo.Width  := ClientWidth - WarningImage.Width;
        end
        else begin
            InfoMemo.Width  := ClientWidth;
            InfoMemo.Left   := 0;
        end;

        InfoMemo.Height := InfoMemo.Lines.Count * 33;
        InfoMemo.Top    := FillerPanel.Height;

        if mffWarning in FFlags then begin
            WarningImage.Left  := 0;
            WarningImage.Top   := FillerPanel.Height;
            if InfoMemo.Height < WarningImage.Height then
                 InfoMemo.Height := WarningImage.Height;
        end;

        if mffInfo in FFlags then begin
            // Info image is same as Wait image (A blue circle with letter i)
            WaitImage.Left  := 0;
            WaitImage.Top   := FillerPanel.Height;
            if InfoMemo.Height < WaitImage.Height then
                 InfoMemo.Height := WaitImage.Height;
        end;

        ClientHeight      := InfoMemo.Height + InfoMemo.Top + 4 + YesBitBtn.Height + 4;

//        if mffProgressBar in FFlags then begin
//            ClientHeight := ClientHeight + ProgressBarWithText.Height + 4;
//            ProgressBarWithText.Visible := TRUE;
//            ProgressBarWithText.Left    := (ClientWidth - ProgressBarWithText.Width) div 2;
//            ProgressBarWithText.Top     := InfoMemo.Height + InfoMemo.Top + 4;
//            LowestComponentBottom := ProgressBarWithText.Top + ProgressBarWithText.Height + 4;
//        end
//        else begin
//            ProgressBarWithText.Visible := FALSE;
            LowestComponentBottom := InfoMemo.Height + InfoMemo.Top + 4;
//        end;

        StopImage.Visible := mffStop in FFlags;
        if mffStop in FFlags then begin
            StopImage.Left := (ClientWidth - StopImage.Width) div 2;
            StopImage.Top  := LowestComponentBottom;
        end;
        OKTickImage.Visible   := mffOKTick in FFlags;
        if mffOKTick in FFlags then begin
            OKTickImage.Left := (ClientWidth - OKTickImage.Width) div 2;
            OKTickImage.Top  := LowestComponentBottom;
        end;
        if (mffWait in FFlags) or (mffWaitOK in FFlags) then begin
            WaitImage.Visible := TRUE;
            WaitImage.Left    := (ClientWidth - WaitImage.Width) div 2;
            WaitImage.Top     := LowestComponentBottom;
        end;
        if mffYesNoCancel in FFlags then begin
            YesBitBtn.Visible    := TRUE;
            NoBitBtn.Visible     := TRUE;
            CancelBitBtn.Visible := TRUE;
            OKBitBtn.Visible     := FALSE;
            NoBitBtn.Left        := (ClientWidth - NoBitBtn.Width) div 2;
            YesBitBtn.Left       := NoBitBtn.Left - YesBitBtn.Width - 8;
            CancelBitBtn.Left    := NoBitBtn.Left + NoBitBtn.Width + 8;
            YesBitBtn.Top        := LowestComponentBottom;
            NoBitBtn.Top         := LowestComponentBottom;
            CancelBitBtn.Top     := LowestComponentBottom;
        end
        else begin
            YesBitBtn.Visible := mffYesNo in FFLags;
            NoBitBtn.Visible  := mffYesNo in FFLags;
            if mffYesNo in FFLags then begin
                YesBitBtn.Left := (ClientWidth - YesBitBtn.Width - NoBitBtn.Width - 8) div 2;
                NoBitBtn.Left  := YesBitBtn.Left + YesBitBtn.Width + 8;
                YesBitBtn.Top  := LowestComponentBottom;
                NoBitBtn.Top   := LowestComponentBottom;
            end;
            OkBitBtn.Visible      := (mffOkCancel in FFLags) or (mffOk in FFLags);
            CancelBitBtn.Visible  := mffOkCancel in FFLags;
            if mffOkCancel in FFLags then begin
                OKBitBtn.Left     := (ClientWidth - OKBitBtn.Width - CancelBitBtn.Width - 8) div 2;
                CancelBitBtn.Left := OKBitBtn.Left + OKBitBtn.Width + 8;
                OKBitBtn.Top      := LowestComponentBottom;
                CancelBitBtn.Top  := LowestComponentBottom;
            end
            else if mffOK in FFlags then begin
                if mffFailed in FFlags then begin
                    FailedImage.Left  := (ClientWidth - OKBitBtn.Width - FailedImage.Width - 8) div 2;
                    FailedImage.Top   := LowestComponentBottom;
                    OKBitBtn.Left     := FailedImage.Left + FailedImage.Width + 8;
                    OKBitBtn.Top      := LowestComponentBottom;
                end
                else begin
                    OKBitBtn.Left := (ClientWidth - OKBitBtn.Width) div 2;
                    OKBitBtn.Top  := LowestComponentBottom;
                end;
            end;
        end;
        FailedImage.Visible   := mffFailed in FFlags;
        if (mffFailed in FFlags) and (not (mffOK in FFlags)) then begin
            FailedImage.Left  := (ClientWidth - FailedImage.Width) div 2;
            FailedImage.Top   := LowestComponentBottom;
        end;
    finally
        FResizeItemsFlag := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMessageForm.GetInfoText: String;
begin
    Result := InfoMemo.Text;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMessageForm.GetTopMostTimerInterval: Cardinal;
begin
    Result := TopMostTimer.Interval;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.SetTopMostTimerInterval(const Value: Cardinal);
begin
    TopMostTimer.Enabled := Value > 0;
    if Value > 0 then
        TopMostTimer.Interval := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.CancelBitBtnClick(Sender: TObject);
begin
    TriggerCallBack(mfccCancel);
    Close;
    ModalResult := mrCancel;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.OKBitBtnClick(Sender: TObject);
begin
    TriggerCallBack(mfccOK);
    Close;
    ModalResult := mrOK;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.NoBitBtnClick(Sender: TObject);
begin
    TriggerCallBack(mfccNO);
    Close;
    ModalResult := IDNO;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.OKTickImageClick(Sender: TObject);
begin
    TriggerCallBack(mfccOkTick);
    Close;
    ModalResult := mrCancel;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.SetInfoText(const Value: String);
var
    WWidth   : Integer;
    I        : Integer;
    DC       : HDC;
    SaveFont : HFont;
    Size     : TSize;
    S        : String;
begin
    InfoMemo.Text  := Value;
    FInfoText.Text := Value;
    if mffAlignLeft in FFlags then
        InfoMemo.Alignment := taLeftJustify;
    if mffCourierNew in FFlags then
        InfoMemo.Font.Name := 'courier new';

    if mffAutoWidth in FFlags then begin
        DC       := GetDC(InfoMemo.Handle);
        SaveFont := SelectObject(DC, InfoMemo.Font.Handle);
        WWidth   := 0;
        for I := 0 to FInfoText.Count - 1 do begin
            S := FInfoText[I];
            GetTextExtentPoint32(DC, PChar(S), Length(S), Size);
            if WWidth < Size.cx then
                WWidth := Size.cx;
        end;
        SelectObject(DC, SaveFont);
        ReleaseDC(InfoMemo.Handle, DC);

        if (mffWarning in FFlags) or (mffInfo in FFlags) then
            InfoMemo.Left   := WarningImage.Width
        else
            InfoMemo.Left   := 0;
        InfoMemo.Width := WWidth + 30;
        ClientWidth    := InfoMemo.Left + InfoMemo.Width;
        if Width > Screen.Width then
            Width := Screen.Width;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.StopImageClick(Sender: TObject);
begin
    TriggerCallBack(mfccStop);
    Close;
    ModalResult := mrCancel;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.WaitImageClick(Sender: TObject);
begin
    if mffWaitOK in FFlags then
        OKBitBtnClick(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMessageForm.YesBitBtnClick(Sender: TObject);
begin
    TriggerCallBack(mfccYES);
    Close;
    ModalResult := IDYES;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMessageForm._(const Msg: String): String;
begin
    Result := Msg;
    if Assigned(FOnTranslateString) then
        FOnTranslateString(Self, Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
    TMessageForm.GFormList := TObjectList<TMessageForm>.Create(FALSE);

finalization
    if Assigned(TMessageForm.GFormList) then begin
        if TMessageForm.GFormList.Count > 0 then
            ShowMessage('GMessageFormList.Count = ' +
                        IntToStr(TMessageForm.GFormList.Count));
        FreeAndNil(TMessageForm.GFormList);
    end;

end.
