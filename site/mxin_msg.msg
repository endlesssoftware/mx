.TITLE	    MXIN_MSG
.FACILITY   MXIN,1141/PREFIX=MXIN__
.IDENT	    "V1.0"
!+
! DESCRIPTION:
!
!   This module contains message codes used by MX_SITE_IN.
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
!   23-FEB-2002 V1.0    Madison     Initial coding.
!-
QOPENERR        <error opening message queue>/FAO=0/FATAL
QMAXSZERR       <error retrieving message size limit>/FAO=0/FATAL
INVSNDRADDR     <invalid sender address: !AS>/FAO=1/ERROR
RCPTFILERR      <error !AS recipient file>/FAO=1/FATAL
INVRCPTADDR     <recipient address !UL invalid: !AS>/FAO=2/ERROR
ALLOCERR        <memory allocation error>/FAO=0/FATAL
MTXTFILERR      <error !AS message text file>/FAO=1/FATAL
WRTMSGERR       <error write message to message queue>/FAO=0/FATAL
MSGENQERR       <error enqueuing message>/FAO=0/FATAL
MSGENQUEUED     <message enqeueued, entry number !UL>/FAO=1/SUCCESS
NORECIPIENTS    <no recipients specified for message>/FAO=0/FATAL
.END
