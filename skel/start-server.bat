@ECHO OFF
CALL rebar.cmd get-deps
CALL rebar.cmd compile
FOR /F "tokens=*" %%i in ('"rebar.cmd boss c=start_dev_cmd ^| findstr werl"') do set myvar=%%i
START "Erlang Window" %myvar%
