$ files = f$edit(p1,"COLLAPSE")
$ number = 0
$ loop:
$    file = f$element(number,",",files)
$    if file.eqs."," then goto _Bye
$    number = number + 1
$    write sys$output "Compressing ''file'...."
$    if f$parse(file,"","","DEVICE").eqs."MG_BIN_VAX:"
$    then libcmd = "LIBRARY/VAX"
$    else libcmd = "LIBRARY"
$    endif
$    'libcmd'/object/compress=block=0 'file'/out='file'
$    purgee/nolog 'file'
$    renamee/nolog 'file' ;1
$    goto loop
$_Bye:
$ exit
$!$ wild = "MG_BIN_AXP:[MX.*]*.OLB"
$!$ loop1:
$!$    file = f$search("''wild'")
$!$    if file.eqs."" then goto _Do_VAX
$!$    devdir = f$parse(file,"","","DEVICE")+f$parse(file,"","","DIRECTORY")
$!$    fname = f$parse(file,"","","NAME")+f$parse(file,"","","TYPE")
$!$    write sys$output "Compressing ''devdir'''fname'...."
$!$    library/object/compress=block=0 'file'/out='devdir''fname'
$!$    purgee/nolog 'devdir''fname'
$!$    renamee/nolog 'devdir''fname' ;1
$!$    goto loop1
$!$_Do_VAX:
$!$ wild = "MG_BIN_VAX:[MX.*]*.OLB"
$!$ loop2:
$!$    file = f$search("''wild'")
$!$    if file.eqs."" then goto _Exit
$!$    devdir = f$parse(file,"","","DEVICE")+f$parse(file,"","","DIRECTORY")
$!$    fname = f$parse(file,"","","NAME")+f$parse(file,"","","TYPE")
$!$    write sys$output "Compressing ''devdir'''fname'...."
$!$    library/object/vax/compress=block=0 'file'/out='devdir''fname'
$!$    purgee/nolog 'devdir''fname'
$!$    renamee/nolog 'devdir''fname' ;1
$!$    goto loop2
$!$_Exit:
$!$ exit
