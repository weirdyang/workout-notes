@echo off
rem Build the web app into dist\ (static files to host anywhere). Needs Node.js.
rem Double-click to run. Output goes to the window and to scripts\logs\publish.log
setlocal
cd /d "%~dp0.."
if not exist scripts\logs mkdir scripts\logs
set LOG=scripts\logs\publish.log
echo [%date% %time%] Building the web app > "%LOG%"
echo Building the web app...
(dotnet tool restore && call npm install && call npm run build) >> "%LOG%" 2>&1
set ERR=%ERRORLEVEL%
type "%LOG%"
echo exit code %ERR% >> "%LOG%"
echo.
if %ERR%==0 (echo OK) else (echo FAILED - exit code %ERR%)
timeout /t 15
exit /b %ERR%
