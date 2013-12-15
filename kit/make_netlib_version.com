$ v = 'F$VERIFY(0)
$ CREATE 'p2
$ OPEN/APPEND v__ 'p2
$ WRITE v__ "''p1'"
$ CLOSE v__
$ EXIT 1+0*F$VERIFY(v)
