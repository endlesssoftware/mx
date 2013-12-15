$ SET NOON
$!
$! You can set up an alternate SMTP server command procedure by copying
$! MX_EXE:SMTP_SERVER.COM to MX_EXE:SMTP_SERVER_xxxx.COM, changing the
$! following logical name definitions to set up the server on an alternate
$! address or port and requiring authentication (or other configuration
$! changes based on logical names), then starting the alternate server with
$! @SYS$STARTUP:MX_STARTUP SMTP_SERVER=xxxx
$!
$! DEFINE MX_SMTP_PORT 25   ! 25 is the default port
$! DEFINE MX_SMTP_SERVER_ADDRESS a.b.c.d
$!
$! If the following logical name is defined as OUTSIDE, only outside clients
$! will be required to authenticate.  If it is defined to any other value,
$! all clients will be required to authenticate.
$! DEFINE MX_SMTP_SERVER_AUTHENTICATION_REQUIRED see-above
$!
$! DEFINE MX_SMTP_SERVER_ADD_DATE 1
$! DEFINE/EXEC MX_FLQ_MAX_ENTRY_SIZE 102400  ! specified in KBytes
$loop:
$ RUN MX_EXE:SMTP_SERVER.EXE
$ status = F$INTEGER ($STATUS) .AND. %XFFFF
$ IF status .EQ. %X0C THEN GOTO loop
$ EXIT 'status'
