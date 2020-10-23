# OvbImgOrganizer

OverByte Image Organizer is an application writen with Delphi that maintain an index of your images, allow searching with the tags you associate with each image. It also provides a display feature much like Win7 image preview.

Everything that appears on screen can be translated to other languages, thanks to DxGetText.

# Translating to other languages

I used DxGetText (See dependencies below) to translate the whole application. I provide ExtractStrings.bat batch file to extract the messages from the source code. It will create OvbImgOrganizerLanguage.pot that can be used with PoEdit to update and translate the translation file OvbImgOrganizerLanguage.po source and produce the translated file OvbImgOrganizerLanguage.mo.

To use the translation, you must copy OvbImgOrganizerLanguage.mo to the same folder as the executable file.

# Prebuilt binaries

The repository contains 32 and 64 bits executable. There is no setup required: just copy the exe file where you want and run it. On first run, it will create a SQLite database in a folder of your choice.

# Compiling the application

I provided a project file for Delphi 10.4.1 but the code should compile with recent Delphi versions (Not tried). You need some dependencies (See below) and you may have to change either the location of DropHandler and SuperMessageForm files to match the path in the project file. This is because I use those files in many of my projects and they are stored in a shared folder on my system. The project also mention a number of files in Direct2D_1 folder. Those files are NOT in GitHub because it is a work in progress. You can safely ignore them because they are used only by conditional compilation on USE_DIRECT2D_1 symbol.

# Dependencies

* VirtualTrees     https://github.com/Virtual-TreeView/Virtual-TreeView
* DxGetText        https://sourceforge.net/projects/dxgettext/
