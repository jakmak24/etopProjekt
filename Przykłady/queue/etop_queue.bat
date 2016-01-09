@ECHO OFF
 
START werl -sname queue -s queue1 init 
START werl -sname etop -hidden -s etop start -node queue@TOMASZ-TOSHIBA -s etop config -interval 10 -s etop config -sort msg_q -s erlang halt -output text %*