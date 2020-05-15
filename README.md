# BGRAGUI

Drawers and Controls made with BGRABitmap for LCL and MSEGUI.

# Lazarus Instructions

* Install BGRABitmapPack.lpk from bgrabitmap repository
* Install bgragui\bgragui.lpk
* Install bgraguicontrols\lcl\bgraguicontrols.lpk.

You will have a new tab BGRAGUI Controls

# MSEIDE Instructions

## Option 1 - Copy

* Copy "bgrabitmap" from bgrabitmap repository into "mseide-msegui\lib\common\bgrabitmap\"
* Copy "bgragui" folder contents into "mseide-msegui\lib\common\bgragui\"
* Copy "bgraguicontrols\msegui\" folder contents into "mseide-msegui\lib\common\bgraguicontrols\"
* Modify "apps\myide\regcomponents.inc" to include "regbgraguicontrols,"
* Build "apps\myide\mymseide.prj" with MSEide

You will have a new tab BGRAGUI Controls

## Option 2 - Defines

* In Settings > Configure MSEide add Global Macros for BGRABITMAPDIR "bgrabitmap\bgrabitmap", BGRAGUIDIR "bgragui\bgragui", BGRAGUICONTROLSDIR "bgragui\bgraguicontrols\msegui\".
* Open mymseide.prj and add ${BGRABITMAPDIR}, ${BGRAGUIDIR}, ${BGRAGUICONTROLSDIR} in Project Options > Make > Directories.
* Add -dBGRABITMAP_USE_MSEGUI in Project Options > Make > Make options.
* Modify "apps\myide\regcomponents.inc" to include "regbgraguicontrols,"
* Build "apps\myide\mymseide.prj" with MSEide

You will have a new tab BGRAGUI Controls
