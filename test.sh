KINDFILE=out.kind
XMLOUTPUT=out.xml

./run > $KINDFILE
kind2 --input-format native -xml $KINDFILE > $XMLOUTPUT
