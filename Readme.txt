Development Enviroment:
=======================

Embarcadero Delphi XE2:
http://www.embarcadero.com/products/delphi


Used packages:
==============

FastMM:
http://sourceforge.net/projects/fastmm/

TPerlRegEx:
http://www.regular-expressions.info/delphi.html

DISQLite3:
http://www.yunqa.de/delphi/doku.php/products/sqlite3/

Unicode SynEdit:
http://synedit.svn.sourceforge.net/viewvc/synedit/

Active Query Builder VCL Edition:
http://www.activequerybuilder.com/

ShellBrowser:
http://www.jam-software.de/shellbrowser_delphi/

Mirkes MPHexEditor:
https://launchpad.net/dcr/

Note: MPHexEditor was developed by Markus Stephany, who stopped his work.
Later on the Delphi Code Revival project worked on it - but seems not
very long. Inside this project there is a own transformation from Delphi 7
to Delphi XE2.


Tools to build the setup program:
=================================

EurekaLog:
http://www.eurekalog.com/

Gawk for Windows:
http://gnuwin32.sourceforge.net/packages/gawk.htm

Ultimate Packer for eXecutables (UPX):
http://upx.sourceforge.net/

Help & Manual:
http://www.ec-software.com/

HTML Help Workshop:
http://go.microsoft.com/fwlink/?LinkId=14188

Inno Setup:
http://www.jrsoftware.org/isinfo.php


Debugging:
==========

Inside Delphi first of all, all packages has to be built (Menu: Project -> Build
All Project).

All Designtime packages needs to be installed. This will be done inside the
IDE with the project manager (Menu: View -> Project Manager). For each
package, you have to make a right click -> Install.

To get detailed memory leak informations while debugging, you have to store
FastMM_FullDebugMode.dll inside the Temp folder. This .dll is inside the
Source\FastMM folder.


Building the setup program:
===========================

The setup program will be built with the Build_Setup.bat file.

With Clean.bat all temporary files can be deleted easily.