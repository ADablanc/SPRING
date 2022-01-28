set oShell = WScript.CreateObject("WScript.Shell")
    cmd_line = """R-Portable\bin\Rscript.exe"" --vanilla ""rscript.R"" 1>workflow.lipido.log 2>&1"
oShell.Run cmd_line, 0, false
