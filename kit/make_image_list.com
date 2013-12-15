$ v = 'F$VERIFY (0)
$ IF F$SEARCH(p1) .EQS. "" THEN CREATE 'p1'
$ OPEN/APPEND milout 'p1'
$ i = 0
$Loop:
$ fspec = F$EDIT (F$ELEMENT (i, ",", p2), "TRIM")
$ IF fspec .EQS. "," THEN GOTO Endloop
$ fspec = F$PARSE (fspec,,,"NAME","SYNTAX_ONLY") + F$PARSE (fspec,,,"TYPE","SYNTAX_ONLY")
$ WRITE milout "MX_TMP ''fspec' MX_INSTALL_ROOT:''p3'"
$ i = i + 1
$ GOTO Loop
$Endloop:
$ CLOSE milout
$ EXIT 1+0*F$VERIFY (v)
