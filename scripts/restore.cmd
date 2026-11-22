@echo off
rem Restore NuGet packages.
rem Double-click to run. Output goes to the window and to scripts\logs\restore.log
setlocal
cd /d "%~dp0.."
if not exist scripts\logs mkdir scripts\logs
set LOG=scripts\logs\restore.log
echo [%date% %time%] Restoring packages > "%LOG%"
echo Restoring packages...
dotnet restore >> "%LOG%" 2>&1
set ERR=%ERRORLEVEL%
type "%LOG%"
echo exit code %ERR% >> "%LOG%"
echo.
if %ERR%==0 (echo OK) else (echo FAILED - exit code %ERR%)
timeout /t 15
exit /b %ERR%
