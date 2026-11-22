@echo off
rem Run the web app locally with live reload. Needs Node.js (https://nodejs.org).
rem Opens on http://localhost:5173 and on your network, so your phone can load it on the same Wi-Fi.
setlocal
cd /d "%~dp0.."
where node >nul 2>&1 || (echo Node.js is not installed. Get it from https://nodejs.org & pause & exit /b 1)
dotnet tool restore
call npm install
call npm run dev
pause
