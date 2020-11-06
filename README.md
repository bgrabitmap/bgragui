# BGRAGUI

Drawers and Controls made with BGRABitmap for LCL and MSEGUI.

# Lazarus Instructions

* Install BGRABitmapPack.lpk from bgrabitmap repository
* Install bgragui\bgragui.lpk
* Install bgraguicontrols\lcl\bgraguicontrols.lpk.

You will have a new tab BGRAGUI Controls

# MSEIDE Instructions

## Option 1 - Copy

* Copy "bgrabitmap" folder contents into "/mseide-msegui/lib/common/bgrabitmap/"
* Copy "bgragui" folder contents into "/mseide-msegui/lib/common/bgragui/"
* Copy "bgraguicontrols/msegui/" folder contents into "/mseide-msegui/lib/common/bgraguicontrols/"
* Open /mseide-msegui/apps/myide/mymseide.prj
* Add "-dBGRABITMAP_USE_MSEGUI" in Project Options > Make > Make options.
* Add "-dclass_bridge" in Project Options > Make > Make options.
* Modify "/mseide-msegui/apps/myide/regcomponents.inc" to include "regbgraguicontrols,"
* Build "/mseide-msegui/apps/myide/mymseide.prj" with MSEide

You will have a new tab BGRAGUI Controls

## Option 2 - Defines

* In Settings > Configure MSEide, add Global Macros with BGRABITMAPDIR = "/directoryof/bgrabitmap/bgrabitmap", BGRAGUIDIR = "/directoryof/bgragui/bgragui", BGRAGUICONTROLSDIR = "/directoryof/bgragui/bgraguicontrols/msegui".
* Open mseide-msegui/apps/myide/mymseide.prj and add ${BGRABITMAPDIR}, ${BGRAGUIDIR}, ${BGRAGUICONTROLSDIR} in Project Options > Make > Directories.
* Add "-dBGRABITMAP_USE_MSEGUI" in Project Options > Make > Make options.
* Add "-dclass_bridge" in Project Options > Make > Make options.
* Modify "/mseide-msegui/apps/myide/regcomponents.inc" to include "regbgraguicontrols,"
* Build "/mseide-msegui/apps/myide/mymseide.prj" with MSEide

You will have a new tab BGRAGUI Controls
