.TITLE	    REJMAN_MSG
.FACILITY   REJMAN,1140/PREFIX=REJMAN__
.IDENT	    "V1.2"
!+
! DESCRIPTION:
!
!   This module contains message codes used by the REJMAN utility.
!
! AUTHOR:
!
!   M. Madison
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
! UPDATE HISTORY:
!
!   07-SEP-1997	V1.0	Madison	    Initial coding.
!   14-OCT-1997	V1.1	Madison	    Add CHECK messages.
!   17-MAY-1998	V1.1-1	Madison	    Remove extraneous apostrophes.
!   03-DEC-2000	V1.1-2	Madison	    INVHDR.
!   17-FEB-2002 V1.2    Madison     INVIDENT, INVREGEX.
!-
.SEVERITY   	ERROR
INVADDR	    	<invalid address specification: !AS>/FAO=1
INVDT	    	<error in date/time specification>/FAO=0

.SEVERITY   	WARNING
NOMATCH	    	<no record matched specified !AS>/FAO=1
ALREADY	    	<matching !AS record already exists>/FAO=1
CANTIND	    	<failed to redirect input stream>
NORDDB 	    	<could not read database file !AS>/FAO=1
NOWRTDB    	<could not write database file !AS>/FAO=1
NOOPNOUT    	<could not open output file !AS>/FAO=1

.SEVERITY   	INFORMATION
READDB	    	<read database file !AS>/FAO=1
WROTEDB    	<wrote database file !AS>/FAO=1
NOPURGE	    	<no rules with last-referenced date prior to !11%D>/FAO=1
PURGED	    	<deleted !UL rules with last-referenced date prior to !11%D>/FAO=2

CKNOMATCH   	<no rejection rule matched parameters>/WARNING
CKMATCH	    	<block found in rejection rule (!AD, !AD, !AD)>/FAO=6/SUCCESS
CKMATCHRW   	<rewrite found in rejection rule (!AD, !AD, !AD)>/FAO=6/SUCCESS
INVHDR	    	<invalid header specification: !AS>/ERROR/FAO=1
INVIDENT        <invalid rule identifier: !AS>/ERROR/FAO=1
INVREGEX        <invalid regular expresion: !AD>/ERROR/FAO=2

.END
