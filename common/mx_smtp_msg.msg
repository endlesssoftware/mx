.TITLE		MX_SMTP_MSG
.FACILITY	MX_SMTP,1062/PREFIX=MX_SMTP__
.IDENT		"V1.1-1"
!++
!
! DESCRIPTION:
!
!   These are the message codes used by the MX SMTP programs.
!
! AUTHOR:	M. Madison
!
! Copyright (c) 2008, Matthew Madison.
! 
! All rights reserved.
! 
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions
! are met:
! 
!     * Redistributions of source code must retain the above
!       copyright notice, this list of conditions and the following
!       disclaimer.
!     * Redistributions in binary form must reproduce the above
!       copyright notice, this list of conditions and the following
!       disclaimer in the documentation and/or other materials provided
!       with the distribution.
!     * Neither the name of the copyright owner nor the names of any
!       other contributors may be used to endorse or promote products
!       derived from this software without specific prior written
!       permission.
! 
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
! CREATION DATE: 27-APR-1989
!
! MODIFICATION HISTORY:
!
!   11-DEC-1989	V1.0	Madison	    Initial coding.
!   16-MAR-1994 V1.1	Goatley	    Add GENERIC_SUCCESS and GENERIC_FAILURE.
!   22-JUN-1997	V1.1-1	Madison	    Add GENERIC_RETRY.
!--

.SEVERITY   SUCCESS

SERVICE_READY	    <!AS SMTP service ready at !%T>/FAO=2
SERVICE_CLOSING	    <!AS service closing transmission channel>
ACTION_OKAY 	    <requested mail action okay, completed>
SEND_DATA   	    <start mail input; end with crlf.crlf>
GENERIC_SUCCESS     <unknown success status>

.SEVERITY   INFO

SYSTEM_STATUS	    <!AS>/FAO=1
HELP_MESSAGE	    <!AS>/FAO=1
WILL_FORWARD	    <user not local, will forward to !AS>/FAO=1

.SEVERITY   WARN

SERVICE_NOT_AVAIL   <!AS service not available>/FAO=1
MAILBOX_BUSY	    <action not taken: mailbox temporarily unavailable>
LOCAL_ERROR 	    <action aborted: local processing error>
INSUFF_STORAGE	    <action not taken: insufficient system storage>
GENERIC_RETRY	    <unknown retry-later status>

.SEVERITY   FATAL
CMD_UNRECOGNIZED    <syntax error: command unrecognized>
ARGUMENT_ERROR	    <syntax error in command arguments>
CMD_NOT_IMPLEM	    <command not implemented>
BAD_CMD_SEQUENCE    <bad sequence of commands>
ARG_UNRECOGNIZED    <command parameter not implemented>
MBX_UNAVAILABLE	    <action not taken: mailbox unavailable>
USER_NOT_LOCAL	    <user not local; please try !AS>/FAO=1
MSG_TOO_LARGE	    <action aborted: exceeded storage allocation>
MBX_SYNTAX_ERROR    <action not taken: mailbox name not allowed>
TRANSACTION_FAIL    <transaction failed>
UNKNOWN_REPLY_CODE  <reply code !UL not known>/FAO=1
GENERIC_FAILURE	    <unknown failure status>

.END
