@echo off
setlocal
cd /d "%~dp0.."
if not exist scripts\logs mkdir scripts\logs
set LOG=scripts\logs\clean.log
echo [%date% %time%] Cleaning > "%LOG%"
echo Cleaning...
dotnet clean --nologo >> "%LOG%" 2>&1
set ERR=%ERRORLEVEL%
type "%LOG%"
echo exit code %ERR% >> "%LOG%"
echo.
if %ERR%==0 (echo OK) else (echo FAILED - exit code %ERR%)
timeout /t 15
exit /b %ERR%
