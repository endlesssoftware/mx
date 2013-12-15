$ IF p1 .EQS. ""
$ THEN doc_dir = "MG_KIT:[MX.DOC]"
$ ELSE doc_dir = p1
$ ENDIF
$ IF p2 .EQS. ""
$ THEN outfile = "MG_ETC:[]MX_DOC_LIST.TXT"
$ ELSE outfile = p2
$ ENDIF
$ create 'outfile
$ open/append mx_doc_list 'outfile
$ write mx_doc_list "!"
$ write mx_doc_list "! MX documentation files."
$ write mx_doc_list "!"
$ write mx_doc_list "MX_TMP INDEX.HTML			MX_INSTALL_ROOT:[DOC]"
$ call make_list "''DOC_DIR'MX*.PS"
$ call make_list "''DOC_DIR'MX*.PDF"
$ call make_list "''DOC_DIR'MX*.TXT"
$ call make_list "''DOC_DIR'MX*.HTML"
$ close mx_doc_list
$ write sys$output "''outfile' created"
$ exit
$ MAKE_LIST: SUBROUTINE
$  _Loop:
$	file = f$search(p1)
$	if file.eqs."" then exit
$	name = f$parse(file,"","","NAME")+f$parse(file,"","","TYPE")
$	write mx_doc_list f$fao("MX_TMP !32AS MX_INSTALL_ROOT:[DOC]", name)
$	goto _loop
$ ENDSUBROUTINE
