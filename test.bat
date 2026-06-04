@echo off
REM Launch APong in Stella configured for paddle control via the mouse.
REM   -bc Paddles      : both controllers are Paddles (APong reads INPT0/INPT1)
REM   -usemouse analog : mouse emulates the analog paddle
REM   -ma 01           : mouse X drives paddle 0, mouse Y drives paddle 1 (both players, one mouse)
REM   -grabmouse 1      : confine cursor so full motion range maps to the paddle
REM Note: needs the right-player difficulty switch in position A (press F7) so APong
REM reads paddles 0/1 (PaddleBase=0); otherwise it reads paddles 2/3 and the mouse does nothing.
REM %~dp0 = directory this .bat lives in, so it works regardless of CWD.

set STELLA=C:\tools\Stella-7.0c\Stella.exe
set ROM=%~dp0ROM\apong.bin

"%STELLA%" -bc Paddles -usemouse analog -ma 01 -grabmouse 1 "%ROM%"
