@echo off
setlocal
set rebarscript=%~f0
chcp 65001 > nul
escript.exe "%rebarscript:.cmd=%" %*
