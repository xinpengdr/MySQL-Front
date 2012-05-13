Development Enviroment:
=======================

Embarcadero Delphi 2010:
http://www.embarcadero.com/products/delphi


Used packages:
==============

FastMM:
http://sourceforge.net/projects/fastmm/

TPerlRegEx (based on Perl Compatible Regular Expressions):
http://www.regular-expressions.info/delphi.html

DISQLite3:
http://www.yunqa.de/delphi/doku.php/products/sqlite3/

Unicode SynEdit:
http://mh-nexus.de/en/unisynedit.php

Active Query Builder VCL Edition:  (Shareware)
http://www.activequerybuilder.com/

Mirkes MPHexEditor:
https://launchpad.net/dcr/

Note: MPHexEditor was developed by Markus Stephany, who stopped his work.
Later on the Delphi Code Revival project worked on it - but seems not
very long. Inside this project there is a own transformation from Delphi 7
to Delphi 2010.


Tools to build a setup program:
===============================

EurekaLog:  (Shareware)
http://www.eurekalog.com/

Gawk for Windows:
http://gnuwin32.sourceforge.net/packages/gawk.htm

Ultimate Packer for eXecutables (UPX):
http://upx.sourceforge.net/

Help & Manual:  (Shareware)
http://www.ec-software.com/

HTML Help Workshop:  (only required, if Help & Manual is used)
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


Building a setup program:
=========================

The Setup Program will be built with the Built_Setup.bat file.
