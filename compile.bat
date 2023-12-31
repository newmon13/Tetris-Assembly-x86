@echo off

set filename=%1

set asmfile=%filename%.asm
set objfile=%filename%.obj
set exefile=%filename%.exe

tasm\tasm.exe /n /t %asmfile%
tasm\tlink.exe /v %objfile%
%exefile%
