@echo off

set SOURCEPATH="..\src\knowledge"
set FILEPATH="..\src\knowledge_generated.pl"

type NUL > "%FILEPATH%"

FOR /R "%SOURCEPATH%" %%F IN (*.pl) DO (
  REM Put a small header inside the generated file
  ECHO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% >> "%FILEPATH%"
  ECHO %%%% %%~nxF >> "%FILEPATH%"
  ECHO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% >> "%FILEPATH%"
  
  REM Redirect everything but "include()" and whole line comments into the generated file
  findstr /V /R /c:"^[ 	]*%%" /c:"include(.*)" %%F >> "%FILEPATH%"

  REM Add 3 new lines at the end
  (ECHO/ && ECHO/ && ECHO/) >> "%FILEPATH%"
)