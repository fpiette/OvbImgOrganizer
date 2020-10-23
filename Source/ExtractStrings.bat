c:\Utils\dxgettext -b . --delphi --nonascii --no-wrap -o:msgid -o .
c:\Utils\msgremove default.po -i OvbImgOrganizerLanguageIgnore.po -o OvbImgOrganizerLanguage.pot --no-wrap
del OvbImgOrganizerLanguageDefaultBak.po
ren default.po OvbImgOrganizerLanguageDefaultBak.po
pause

