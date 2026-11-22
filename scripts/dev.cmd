@echo off
setlocal
cd /d "%~dp0.."
where node >nul 2>&1 || (echo Node.js is not installed. Get it from https://nodejs.org & pause & exit /b 1)
dotnet tool restore
call npm install
call npm run dev
pause
