rem This is like run.bat except that it automatically loads an msl file passed as an argument.
rem You may want to associate the msl file ending with this script.

java -Xmx1024m -cp %~dp0/mmt.jar info.kwarc.mmt.api.frontend.Run --shell file %1
