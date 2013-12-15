$! PROCESS_HOLDING_QUEUE.COM
$!
$!  Command procedure invoked as a detached process for starting SMTP holding queues.
$!
$! Copyright (c) 2008, Matthew Madison.
$! 
$! All rights reserved.
$! 
$! Redistribution and use in source and binary forms, with or without
$! modification, are permitted provided that the following conditions
$! are met:
$! 
$!     * Redistributions of source code must retain the above
$!       copyright notice, this list of conditions and the following
$!       disclaimer.
$!     * Redistributions in binary form must reproduce the above
$!       copyright notice, this list of conditions and the following
$!       disclaimer in the documentation and/or other materials provided
$!       with the distribution.
$!     * Neither the name of the copyright owner nor the names of any
$!       other contributors may be used to endorse or promote products
$!       derived from this software without specific prior written
$!       permission.
$! 
$! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
$! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
$! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
$! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
$! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
$! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
$! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
$! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
$! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
$! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
$! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
$!
$ myproc = F$GETJPI ("","PRCNAM")
$ whichqueue = F$INTEGER (myproc - "MX_HOLD")
$ IF whichqueue .EQ. 0 THEN EXIT
$ smtp := $MX_EXE:MX_SMTP
$!
$ tmp = F$TRNLNM (myproc + "_REQUEST_DELIVERY")
$ IF tmp .EQS. ""
$ THEN do_etrn = "TRUE"
$ ELSE do_etrn = tmp
$ ENDIF
$!
$ IF .NOT. do_etrn THEN GOTO Start_No_Etrn
$ request_names = ""
$ i = 0
$Loop:
$ tmp = F$TRNLNM (myproc + "_HOST_NAME", "LNM$FILE_DEV", i)
$ IF tmp .EQS. "" THEN GOTO Start
$ IF request_names .NES. "" THEN request_names = request_names + ","
$ request_names = request_names + """" + tmp + """"
$ i = i + 1
$ GOTO Loop
$Start:
$ IF request_names .EQS. ""
$ THEN smtp/holding_queue='whichqueue'/request_delivery
$ ELSE smtp/holding_queue='whichqueue'/request_delivery=('request_names')
$ ENDIF
$ GOTO Depart
$Start_No_Etrn:
$ smtp/holding_queue='whichqueue'
$Depart:
$!  Clean up log files
$ PURGE/KEEP=5 MX_SMTP_DIR:PROCESS_HOLDING_QUEUE.LOG
$ EXIT
