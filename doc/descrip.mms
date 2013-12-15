MX_FACILITY = DOC
MG_NATIVE   = 1
.INCLUDE [-]MMSDEFS.MMS

.suffixes
.suffixes : .PS .PDF .TXT .COM .HTML .SDML .GRA .SDML~ .GRA~ .COM~ .HTML~

primary_target : all

DOC = DOCUMENT
DOCFLAGS = /CONTENTS/NOPRINT/OUTPUT=$(MMS$TARGET)$(BATCH)/DEVICE=BLANK_PAGES/SYMBOLS=$(ETCDIR)DYNAMIC_SYMBOLS.SDML
BRFLAGS = /CONTENTS/NOPRINT/OUTPUT=$(MMS$TARGET)$(BATCH)/SYMBOLS=$(ETCDIR)DYNAMIC_SYMBOLS.SDML

.ifdef MG_HAVE_DOCUMENT
.first
    @ set display/create/node=0/transport=local
    @ define doc$html_filesize 1000000
.endif

.sdml.html :
    $(DOC)$(BRFLAGS) $(MMS$SOURCE) SOFTWARE.REFERENCE HTML
    > IF F$SEARCH("$(MMS$TARGET_NAME)_FIGURES.COM") .NES. "" THEN @$(MMS$TARGET_NAME)_FIGURES.COM/OUTPUT=NL:
    > IF F$SEARCH("$(MMS$TARGET_NAME)_FIGURES.COM") .NES. "" THEN DELETE $(MMS$TARGET_NAME)_FIGURES.COM;*

{}.SDML{$(KITDIR)}.HTML :

.GRA~.GRA :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).GRA $(CMSFLAGS) $(CMSCOMMENT)
{$(CMSDIR)}.GRA~{}.GRA :

.HTML~.HTML :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).HTML $(CMSFLAGS) $(CMSCOMMENT)
{$(CMSDIR)}.HTML~{}.HTML :

.sdml.ps :
    $(DOC)$(DOCFLAGS) $(MMS$SOURCE) SOFTWARE.REFERENCE PS
{}.SDML{$(KITDIR)}.PS :

.sdml.txt :
    $(DOC)$(DOCFLAGS) $(MMS$SOURCE) SOFTWARE.REFERENCE MAIL
{}.SDML{$(KITDIR)}.TXT :

!
! This PS->PDF rule sets up the Ghostscript environment itself to avoid
! interference with DOCUMENT which also uses Ghostscript in its backend.
!
.ps.pdf :
    < DEFINE/USER GS_LIB SYS$SYSDEVICE:[GS.LIB],SYS$SYSDEVICE:[GS.FONTS]
    < GS == "$SYS$SYSDEVICE:[GS.BIN]GS.EXE_$(MMSARCH_NAME)
    - PIPE GS "-sDEVICE=pdfwrite" "-dBATCH" "-dNOPAUSE" "-sOutputFile=$(MMS$TARGET)" $(MMS$SOURCE) > $(MMS$TARGET:.pdf=.gs_out)
    > TYPE $(MMS$TARGET:.pdf=.gs_out)
    > IF F$SEARCH("$(MMS$TARGET:.pdf.gs_out)") .NES. "" THEN DELETE/NOLOG $(MMS$TARGET:.pdf=.gs_out);*
    > DELETE/SYMBOL/GLOBAL GS
    > IF F$SEARCH("_TEMP_*.*") .NES. "" THEN DELETE/NOLOG _TEMP_*.*;*

PSFILES = $(KITDIR)mx_install_guide.ps, $(KITDIR)mx_mgmt_guide.ps, $(KITDIR)mx_user_guide.ps,-
    	    	   $(KITDIR)mx_prog_guide.ps,$(KITDIR)mx_mlf_guide.ps

ASCFILES = $(PSFILES:.ps=.txt)

HTMLFILES = $(PSFILES:.ps=.html),$(KITDIR)INDEX.HTML

PDFFILES = $(PSFILES:.ps=.pdf)

psfiles : $(PSFILES)
    @ !

pdffiles : $(PDFFILES)
    @ !

COMMON = ,SYMBOLS.SDML, COPYRIGHT.SDML, $(ETCDIR)DYNAMIC_SYMBOLS.SDML

$(KITDIR)mx_install_guide.ps,-
$(KITDIR)mx_install_guide.txt,-
$(KITDIR)mx_install_guide.html  : mx_install_guide.sdml,-
    	    	    	    	  install_frontmatter.sdml,-
    	    	    	    	  install_preparing.sdml,-
    	    	    	    	  install_installing.sdml,-
    	    	    	    	  install_postinstall.sdml,-
    	    	    	    	  install_kitcontents.sdml,-
    	    	    	    	  install_files.sdml $(COMMON)

$(KITDIR)mx_mgmt_guide.html : mx_mgmt_guide.sdml $(COMMON) $(MX_SRC_MCP)REJMAN_HELP.SDML
    $(DOC)$(BRFLAGS) $(MMS$SOURCE) SOFTWARE.REFERENCE html/INDEX

$(KITDIR)mx_mgmt_guide.ps : mx_mgmt_guide.sdml $(COMMON) $(MX_SRC_MCP)REJMAN_HELP.SDML
    $(DOC)$(DOCFLAGS) $(MMS$SOURCE) SOFTWARE.REFERENCE PS/INDEX

$(KITDIR)mx_mgmt_guide.txt : mx_mgmt_guide.sdml $(COMMON)  $(MX_SRC_MCP)REJMAN_HELP.SDML
    $(DOC)$(DOCFLAGS) $(MMS$SOURCE) SOFTWARE.REFERENCE MAIL/INDEX

$(KITDIR)mx_user_guide.ps,-
$(KITDIR)mx_user_guide.txt,-
$(KITDIR)mx_user_guide.html 	: mx_user_guide.sdml$(COMMON)

$(KITDIR)mx_prog_guide.ps,-
$(KITDIR)mx_prog_guide.txt,-
$(KITDIR)mx_prog_guide.html 	: mx_prog_guide.sdml$(COMMON)

$(KITDIR)mx_mlf_guide.ps,-
$(KITDIR)mx_mlf_guide.txt,-
$(KITDIR)mx_mlf_guide.html  	: mx_mlf_guide.sdml$(COMMON)

$(KITDIR)mx_mgmt_guide.pdf 	: $(KITDIR)mx_mgmt_guide.ps
$(KITDIR)mx_install_guide.pdf 	: $(KITDIR)mx_install_guide.ps
$(KITDIR)mx_user_guide.pdf 	: $(KITDIR)mx_user_guide.ps
$(KITDIR)mx_prog_guide.pdf 	: $(KITDIR)mx_prog_guide.ps
$(KITDIR)mx_mlf_guide.pdf 	: $(KITDIR)mx_mlf_guide.ps


$(ETCDIR)DYNAMIC_SYMBOLS.SDML	: GENERATE_SYMBOLS.COM, $(MX_SRCTOP)VERSION.OPT
    @GENERATE_SYMBOLS $(MX_SRCTOP)VERSION.OPT $(MMS$TARGET)

$(KITDIR)INDEX.HTML	: MAKE_INDEX_HTML.COM, INDEX.HTML, $(MX_SRCTOP)VERSION.OPT
    @MAKE_INDEX_HTML.COM INDEX.HTML $(MX_SRCTOP)VERSION.OPT $(MMS$TARGET)

HTML :	$(HTMLFILES)
	!

.IFDEF MG_HAVE_DOCUMENT
all : $(PSFILES),$(HTMLFILES),$(ASCFILES),$(PDFFILES)
    @ IF F$SEARCH("$(SRCDIR)*.TEX") .NES. "" THEN DELETE/NOLOG $(SRCDIR)*.TEX;*
    @ IF F$SEARCH("$(SRCDIR)*.INT_TEX") .NES. "" THEN DELETE/NOLOG $(SRCDIR)*.INT_TEX;*
    @ IF F$SEARCH("$(SRCDIR)*.DVI_*") .NES. "" THEN DELETE/NOLOG $(SRCDIR)*.DVI_*;*

clean :
    -@ DELETE/NOLOG $(KITDIR)*.PS;*,.TXT;*,.HTML;*

.ELSE
all : mx_user_guide.sdml,mx_prog_guide.sdml,mx_mlf_guide.sdml,generate_symbols.com,-
      symbols.sdml,copyright.sdml,mx_mgmt_guide.sdml,make_index_html.com,index.html,-
      mx_install_guide.sdml,-
    	    	    	    	  install_frontmatter.sdml,-
    	    	    	    	  install_preparing.sdml,-
    	    	    	    	  install_installing.sdml,-
    	    	    	    	  install_postinstall.sdml,-
    	    	    	    	  install_kitcontents.sdml,-
    	    	    	    	  install_files.sdml
    @ WRITE SYS$OUTPUT "Building nothing -- DOCUMENT not available"
clean :
.ENDIF
