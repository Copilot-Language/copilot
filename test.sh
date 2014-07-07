KINDFILE=out.kind

./run > $KINDFILE
kind2 --input-format native $KINDFILE
