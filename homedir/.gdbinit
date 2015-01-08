# JJ's gdb config
# Copyright (c) 2012-2015 Jonas Jelten <jj@sft.mx>


#set architecture i386:x86-64:intel
set history save
set history filename ~/.gdb_history
set disassembly-flavor intel
set disassemble-next-line auto
set print demangle on
set print asm-demangle off
set print pretty on
set print array on
set print symbol-filename on

#catch syscall ptrace

# don't confirm the exit
define hook-quit
	set confirm off
end

# utility functions
define nip
	ni
	x /10i $rip
end

# is pretty nice but very intrusive
#source ~/peda/peda.py
