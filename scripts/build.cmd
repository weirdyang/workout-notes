@echo off
rem Restore and build the whole solution.
rem Double-click to run. Output goes to the window and to scripts\logs\build.log
setlocal
cd /d "%~dp0.."
if not exist scripts\logs mkdir scripts\logs
set LOG=scripts\logs\build.log
echo [%date% %time%] Building > "%LOG%"
echo Building...
dotnet build --nologo >> "%LOG%" 2>&1
set ERR=%ERRORLEVEL%
type "%LOG%"
echo exit code %ERR% >> "%LOG%"
echo.
if %ERR%==0 (echo OK) else (echo FAILED - exit code %ERR%)
timeout /t 15
exit /b %ERR%
