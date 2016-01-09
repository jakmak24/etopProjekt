@ECHO
 OFF
START werl -sname pFactorial %*
START werl -sname etop -hidden -s etop start -node pFactorial@TOMASZ-TOSHIBA -s etop config -interval 10 -s erlang halt -output text %*