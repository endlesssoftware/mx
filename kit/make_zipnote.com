$ v = 'F$VERIFY (0)
$! p1=template p2=version.opt, p3=output
$ SET := SET
$ SET SYMBOL/SCOPE=(NOLOCAL,NOGLOBAL)/VERB
$!
$ OPEN/READ in 'p2'
$ READ in line
$ CLOSE in
$ 'line'
$ OPEN/READ in 'p1'
$ CREATE 'p3'
$ OPEN/APPEND out 'p3'
$ WRITE out "@ (zip file comment below this line)"
$ WRITE out ""
$ WRITE out F$FAO ("Message Exchange (MX) !AS [!11%D]", ident, 0)
$ WRITE out "Copyright ©2008, Matthew Madison.  All Rights Reserved."
$ WRITE out ""
$Loop:
$ READ/END=endloop/ERR=endloop in line
$ WRITE out line
$ GOTO Loop
$endloop:
$ CLOSE in
$ CLOSE out
$ EXIT 1+0*F$VERIFY(v)
