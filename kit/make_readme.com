$ v = 'F$VERIFY (0)
$! MAKE_README.COM
$! p1=template p2=version.opt, p3=output
$ SET := SET
$ SET SYMBOL/SCOPE=(NOLOCAL,NOGLOBAL)/VERB
$!
$ OPEN/READ in 'p2'
$ READ in line
$ CLOSE in
$ 'line'
$ major = F$INTEGER (F$ELEMENT (0, ".", F$EXTRACT (1, -1, ident)))
$ minor = F$INTEGER (F$EXTRACT (0, 1, F$ELEMENT (1, ".", ident)))
$ vvn   = F$FAO ("!2ZL!1UL", major, minor)
$ OPEN/READ in 'p1'
$ CREATE 'p3'
$ OPEN/APPEND out 'p3'
$Loop:
$ READ/END=endloop/ERR=endloop in line
$ i = F$LOCATE ("$VER$", line)
$ IF i .LT. F$LENGTH (line) THEN -
    line = F$EXTRACT (0, i, line) + ident + F$EXTRACT (i+5, -1, line)
$ i = F$LOCATE ("$V$", line)
$ IF i .LT. F$LENGTH (line) THEN -
    line = F$EXTRACT (0, i, line) + vvn + F$EXTRACT (i+3, -1, line)
$ WRITE out line
$ GOTO Loop
$endloop:
$ CLOSE in
$ CLOSE out
$ EXIT 1+0*F$VERIFY(v)
