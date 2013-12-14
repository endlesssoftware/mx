MX_FACILITY = ALIAS
.INCLUDE [-]MMSDEFS.MMS

MXALIAS_MODULES = MXALIAS,MXALIAS_CLD_TABLES=$(BINDIR)MXALIAS_CLD,-
		MXALIAS_MSG,HG_INPUT=$(BINDIR)HG$GET_INPUT

mxalias :	$(bindir)mxalias.EXE, $(KITDIR)mx_alias_helplib.hlb, $(KITDIR)mxalias_main.hlp
	@ write sys$output "MXALIAS build complete"

PREFETCH :

$(BINDIR)mxalias.EXE	    : $(BINDIR)MXALIAS.OLB($(MXALIAS_MODULES))
	$(LINK)/NOTRACE$(LINKFLAGS) $(BINDIR)MXALIAS.OLB/INCLUDE=(MXALIAS)/LIB,$(MXSHROPT)

$(BINDIR)mxalias.obj 	    : $(MX_SRC_COMMON)MX_LCLDEFS$(L32)

$(KITDIR)mxalias.hlp	    : $(KITDIR)mxalias.rnh
$(KITDIR)mxalias.rnh 	    : $(SRCDIR)mxalias_help.txt, $(SRCDIR)cvthelp.tpu
	edit/tpu/nosection/nodisplay/command=cvthelp.tpu/output=$(MMS$TARGET) $(SRCDIR)mxalias_help.txt

$(KITDIR)mxalias_main.hlp   : $(KITDIR)mxalias_main.rnh
$(KITDIR)mxalias_main.rnh   : $(SRCDIR)mxalias_main_help.txt, $(SRCDIR)cvthelp.tpu
	edit/tpu/nosection/nodisplay/command=$(SRCDIR)cvthelp.tpu/output=$(MMS$TARGET) $(SRCDIR)mxalias_main_help.txt

$(KITDIR)mx_alias_helplib.hlb :	$(KITDIR)mxalias.hlp
	library/help/create $(MMS$TARGET) $(MMS$SOURCE)
	@ set file/truncate $(MMS$TARGET)
