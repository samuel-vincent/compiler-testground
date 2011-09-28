
@echo Setting up Windows AMD64 tools environment 
@"C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\bin\amd64\vcvars64.bat" & nmake /F Makefile.nmake %*
