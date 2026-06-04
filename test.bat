@echo off
REM Launch APong in Stella configured for paddle control via the mouse.
REM   -bc Paddles      : both controllers are Paddles (APong reads INPT0/INPT1)
REM   -usemouse analog : mouse emulates the analog paddle
REM   -grabmouse 1      : confine cursor so full motion range maps to the paddle
REM %~dp0 = directory this .bat lives in, so it works regardless of CWD.

set STELLA=C:\tools\Stella-7.0c\Stella.exe
set ROM=%~dp0ROM\apong.bin

"%STELLA%" -bc Paddles -usemouse analog -grabmouse 1 "%ROM%"
