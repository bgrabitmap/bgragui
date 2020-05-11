# BGRAGUI

Drawers and Controls made with BGRABitmap for LCL and MSEGUI.

# Lazarus Instructions

* Install BGRABitmapPack.lpk from bgrabitmap repository
* Install bgragui\bgragui.lpk
* Install bgraguicontrols\lcl\bgraguicontrols.lpk.

You will have a new tab BGRAGUI Controls

# MSEIDE Instructions

* Copy "bgrabitmap" from bgrabitmap repository into "mseide-msegui\lib\common\bgrabitmap\"
* Copy "bgragui" folder contents into "mseide-msegui\lib\common\bgragui\"
* Copy "bgraguicontrols\msegui\" folder contents into "mseide-msegui\lib\common\bgraguicontrols\"
* Modify "apps\myide\regcomponents.inc" to include "regbgraguicontrols,"
* Build "apps\myide\mymseide.prj" with MSEIDE

You will have a new tab BGRAGUI Controls