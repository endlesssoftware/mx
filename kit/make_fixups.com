$ v = 'F$VERIFY (0)
$ IF F$SEARCH(p1) .EQS. "" THEN CREATE 'p1'
$ OPEN/APPEND mfout 'p1'
$ i = 0
$Loop:
$ fspec = F$EDIT (F$ELEMENT (i, ",", p2), "TRIM")
$ IF fspec .EQS. "," THEN GOTO Endloop
$ fspec = F$PARSE (fspec,,,"NAME","SYNTAX_ONLY") + F$PARSE (fspec,,,"TYPE","SYNTAX_ONLY")
$ WRITE mfout "$ SET PROTECTION=''p3' VMI$KWD:''fspec'"
$ i = i + 1
$ GOTO Loop
$Endloop:
$ CLOSE mfout
$ EXIT 1+0*F$VERIFY (v)
