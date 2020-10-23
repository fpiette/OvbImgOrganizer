program OvbImgOrganizer;

uses
  System.SysUtils,
  Vcl.Forms,
  Vcl.ActnList,
  Vcl.Controls,
  Vcl.Graphics,
  FireDAC.Comp.Client,
  OvbImgOrganizer.Main in 'OvbImgOrganizer.Main.pas' {OvbImgOrganizerMainForm},
  OvbImgOrganizer.Utils in 'OvbImgOrganizer.Utils.pas',
  SuperMessageForm in '..\..\SuperMessageForm\SuperMessageForm.pas',
  DropHandler in '..\..\DropHandler\DropHandler.pas',
  OvbImgOrganizer.ImagePanel in 'OvbImgOrganizer.ImagePanel.pas',
  OvbImgOrganizer.DataModule in 'OvbImgOrganizer.DataModule.pas' {OvbImgOrganizerDataModule: TDataModule},
  OvbImgOrganizer.DataTypes in 'OvbImgOrganizer.DataTypes.pas',
  OvbImgOrganizer.ImageScanDir in 'OvbImgOrganizer.ImageScanDir.pas' {ScanDirForm},
  OvbImgOrganizer.Help in 'OvbImgOrganizer.Help.pas' {HelpForm},
  OvbImgOrganizer.SlideShow in 'OvbImgOrganizer.SlideShow.pas' {SlideShowForm},
  VirtualTrees.AccessibilityFactory in 'D:\Svn\VirtualTreeView\trunk\Source\VirtualTrees.AccessibilityFactory.pas',
  VirtualTrees.Classes in 'D:\Svn\VirtualTreeView\trunk\Source\VirtualTrees.Classes.pas',
  VirtualTrees.ClipBoard in 'D:\Svn\VirtualTreeView\trunk\Source\VirtualTrees.ClipBoard.pas',
  VirtualTrees.Export in 'D:\Svn\VirtualTreeView\trunk\Source\VirtualTrees.Export.pas',
  VirtualTrees.HeaderPopup in 'D:\Svn\VirtualTreeView\trunk\Source\VirtualTrees.HeaderPopup.pas',
  VirtualTrees in 'D:\Svn\VirtualTreeView\trunk\Source\VirtualTrees.pas',
  VirtualTrees.StyleHooks in 'D:\Svn\VirtualTreeView\trunk\Source\VirtualTrees.StyleHooks.pas',
  VirtualTrees.Utils in 'D:\Svn\VirtualTreeView\trunk\Source\VirtualTrees.Utils.pas',
  VirtualTrees.WorkerThread in 'D:\Svn\VirtualTreeView\trunk\Source\VirtualTrees.WorkerThread.pas',
  gnugettext in 'D:\Svn\DxGetText\trunk\dxgettext\sample\gnugettext.pas',
  OvbImgOrganizer.PaintPanel in 'OvbImgOrganizer.PaintPanel.pas',
  Vcl.Direct2D_1 in '..\..\Direct2D_1\Vcl.Direct2D_1.pas',
  Winapi.D2D1_1 in '..\..\Direct2D_1\Winapi.D2D1_1.pas',
  Winapi.D2D1EffectAuthor in '..\..\Direct2D_1\Winapi.D2D1EffectAuthor.pas',
  Winapi.DCommon in '..\..\Direct2D_1\Winapi.DCommon.pas',
  Winapi.DocumentTarget in '..\..\Direct2D_1\Winapi.DocumentTarget.pas',
  Winapi.DWrite_1 in '..\..\Direct2D_1\Winapi.DWrite_1.pas';

{$R *.res}

begin
//  BindTextDomain('OvbImgOrganizer', ExtractFilePath(GetModuleFileName(0)));
//  TextDomain('OvbImgOrganizer');
  // Use delphi.mo for runtime library translations, if it is there
  AddDomainForResourceString('delphi');


  // See here: http://dybdahl.dk/dxgettext/docs/typicalignores.php
  TP_GlobalIgnoreClassProperty(TAction,     'Category');
  TP_GlobalIgnoreClassProperty(TControl,    'HelpKeyword');
  TP_GlobalIgnoreClassProperty(TWinControl, 'ImeName');
  TP_GlobalIgnoreClass(TFont);
  TP_GlobalIgnoreClass(TFDConnection);
  TP_GlobalIgnoreClassProperty(TOvbImgOrganizerMainForm, 'Caption');

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOvbImgOrganizerMainForm, OvbImgOrganizerMainForm);
  Application.CreateForm(TOvbImgOrganizerDataModule, OvbImgOrganizerDataModule);
  Application.Run;
end.
