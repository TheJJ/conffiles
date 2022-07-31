# JJ's gdb config
# Copyright (c) 2012-2018 Jonas Jelten <jj@sft.mx>
# Licensed GNU GPLv3 or later

#set architecture i386:x86-64:intel
set auto-load python-scripts on
set disassemble-next-line auto
set disassembly-flavor intel
set history expansion on
set history filename ~/.gdb_history
set history save
set history save on
set history size 10000
set pagination on
set print array on
set print asm-demangle off
set print demangle on
set print object on
set print pretty on
set print static-members off
set print symbol-filename off
set print union on
set print vtbl on
set python print-stack full
set trace-commands off
set breakpoint pending on
#set detach-on-fork off

#(sizeof(void *) == 8)
set $arch64bit = 1


#set prompt = gdb>> 
#set extended-prompt \e[1;32m= gdb>>\e[0m 
set prompt \001\033[1;32m\002= gdb>>\001\033[0m\002 


# don't confirm the exit
define hook-quit
	set confirm off
end


# executed, whenever execution was stopped:
# (alternatively, use `display` command)
define hook-stop
	#x /1i $pc
end

##########################

# stepping and machine status

define ip
	x /10i $pc
end
document ip
Dumps decoded instructions starting at instruction pointer.
Usage: ip
end

define nip
	ni
	x /10i $pc
end
document nip
Run next instruction and print 10 next instructions.
Usage: nip
end

define nips
	ni
	status-info
end
document nips
Run next instruction and print machine status.
Usage: nips
end

define sip
	si
	x /10i $pc
end
document sip
Step into next instruction and print 10 next instructions.
Usage: sip
end

define sips
	si
	status-info
end
document sips
Step into next instruction and print machine status.
Usage: sips
end


define stack-base-dump64
	set $_mempos = ($sp + $arg0)
	if ($_mempos == $rbp)
		printf "^"
	else
		if ($_mempos == $sp)
			printf ">"
		else
			printf " "
		end
	end
	printf "%#018lx", *((long*)$_mempos)
end
document stack-base-dump64
	internal usage. will dump *($sp+$arg0) and annotate $sp/$rbp
end

define stack4-dump64
	if ($arg0 == 0)
		printf "="
	else
		printf " "
	end
	stack-base-dump64 $arg0+0
	printf " "
	stack-base-dump64 $arg0+1
	printf " "
	stack-base-dump64 $arg0+2
	printf " "
	stack-base-dump64 $arg0+3
	printf "\n"
end
document stack4-dump64
	internal usage. will dump *($sp+$arg0+{0,1,2,3})
end

define status-info
	printf "\n"
	info symbol $pc
	echo \033[33;1m
	x/5i $pc
	echo \033[0m
	printf "\n"

	if ($arch64bit == 1)
		# x86_64.
		printf "rdi%16lx rsi%16lx rdx%16lx rcx%16lx\n", $rdi, $rsi, $rdx, $rcx
		printf "rax%16lx rbx%16lx r8 %16lx r9 %16lx\n", $rax, $rbx, $r8,  $r9
		printf "r10%16lx r11%16lx r12%16lx r13%16lx\n", $r10, $r11, $r12, $r13
		printf "rbp%16lx rsp%16lx r14%16lx r15%16lx\n", $rbp, $rsp, $r14, $r15
		info registers eflags
		printf "\n"
		stack4-dump64 -8
		stack4-dump64 -4
		stack4-dump64 0
		stack4-dump64 4
		stack4-dump64 8
		stack4-dump64 12
	else
		# x86.
		printf "eax: %8x      ebx: %8x      ecx: %8x      edx: %8x\n", $eax, $ebx, $ecx, $edx
		printf "edi: %8x      esi: %8x      ebp: %8x      esp: %8x\n", $edi, $esi, $ebp, $esp
		printf "\n"
		printf "  %#010x %#010x %#010x %#010x\n", *((int*)$sp - 4),  *((int*)$sp - 3),  *((int*)$sp - 2),  *((int*)$sp - 1)
		printf "=>%#010x %#010x %#010x %#010x\n", *((int*)$sp + 0),  *((int*)$sp + 1),  *((int*)$sp + 2),  *((int*)$sp + 3)
		printf "  %#010x %#010x %#010x %#010x\n", *((int*)$sp + 4),  *((int*)$sp + 5),  *((int*)$sp + 6),  *((int*)$sp + 7)
		printf "  %#010x %#010x %#010x %#010x\n", *((int*)$sp + 8),  *((int*)$sp + 9),  *((int*)$sp + 10), *((int*)$sp + 11)
		printf "  %#010x %#010x %#010x %#010x\n", *((int*)$sp + 12), *((int*)$sp + 13), *((int*)$sp + 14), *((int*)$sp + 15)
		printf "\n"
	end
end
document status-info
	Print useful information about the most important registers.
	Dump the current stack.
end


### Break and watchpoints

define sstart
	tbreak __libc_start_main
	run
end
document sstart
Run program and until __libc_start_main().
When there's no main e.g. in stripped executables.
end

define main
	tbreak main
	run
end
document main
Run program and break on main().
end


define bpt
	if $argc != 1
		help bpt
	else
		tbreak $arg0
	end
end
document bpt
Set up break at given location, which is deleted as soon as it hits.
Usage: bpt WHERE
(or just use `tb` directly..)

WHERE: line number, function name, or "*0x1234".
end


define bpm
	if $argc != 1
		help bpm
	else
		awatch $arg0
	end
end
document bpm
Memory read/write breakpoint
Usage: bpm WHERE

WHERE: $regval, or *address.
end


### Binary dumping and printing

define ascii_char
	if $argc != 1
		help ascii_char
	else
		set $_c = *(unsigned char *) ($arg0)
		if ($_c < 0x20 || $_c > 0x7E)
			if $_c == 0x0a
				printf "¬"
			else
				if $_c == 0x00
					printf "¤"
				else
					printf "."
				end
			end
		else
			printf "%c", $_c
		end
	end
end
document ascii_char
	Print ASCII value of byte at address ADDR.
	Print "." if the value is unprintable.
	Helper for `hexdump`.
	Usage: ascii_char ADDR
end

define dump_hextet
	if $argc != 1
		help dump_hextet
	else
		printf "%02X%02X%02X%02X %02X%02X%02X%02X  %02X%02X%02X%02X %02X%02X%02X%02X", \
			*(unsigned char*) ($arg0),     *(unsigned char*) ($arg0 + 1), \
			*(unsigned char*) ($arg0 + 2), *(unsigned char*) ($arg0 + 3), \
			*(unsigned char*) ($arg0 + 4), *(unsigned char*) ($arg0 + 5), \
			*(unsigned char*) ($arg0 + 6), *(unsigned char*) ($arg0 + 7), \
			*(unsigned char*) ($arg0 + 8), *(unsigned char*) ($arg0 + 9), \
			*(unsigned char*) ($arg0 + 10), *(unsigned char*) ($arg0 + 11), \
			*(unsigned char*) ($arg0 + 12), *(unsigned char*) ($arg0 + 13), \
			*(unsigned char*) ($arg0 + 14), *(unsigned char*) ($arg0 + 15)
	end
end
document dump_hextet
	Print sixteen hexadecimal bytes starting at address ADDR.
	Helper for `hexdump`.
	Usage: dump_hextet ADDR
end

define ascii_hextet
	if $argc != 1
		help ascii_hextet
	else
		set $_i_ahx = 0
		while ($_i_ahx < 0x10)
			if $_i_ahx == 0x8
				printf " "
			end
			set $_addr = $arg0 + $_i_ahx
			ascii_char $_addr
			set $_i_ahx++
		end
	end
end
document ascii_hextet
	Print sixteen bytes represented as ascii starting at address ADDR.
	Helper for `hexdump`.
	Usage: ascii_hextet ADDR
end

define dump_hexcombo
	if $argc != 1
		help dump_hexcombo
	else
		echo \033[1m
		if sizeof(void*) == 8
			printf "0x%016lX: ", $arg0
		else
			printf "0x%08X: ", $arg0
		end
		echo \033[0m

		dump_hextet $arg0
		printf " "

		echo \033[1m
		ascii_hextet $arg0
		echo \033[0m
		printf "\n"
	end
end
document dump_hexcombo
	Display a 16-byte hex/ASCII dump of memory at address ADDR.
	Helper for `hexdump`.
	Usage: dump_hexcombo ADDR
end

define hexdump
	if $argc == 1
		dump_hexcombo $arg0
	else
		if $argc == 2
			set $_i_hxd = 0
			while ($_i_hxd < $arg1)
				set $_hexpart_offset = ($_i_hxd * 0x10)
				set $_hexpart_addr = ($arg0 + $_hexpart_offset)
				dump_hexcombo $_hexpart_addr
				set $_i_hxd += 1
			end
		else
			help hexdump
		end
	end
end
document hexdump
	Display 16-byte hex/ASCII dumps of memory starting at address ADDR.
	Usage: hexdump ADDR [nr 16B-groups]
end


define search
	set $start   = (char *) $arg0
	set $end     = (char *) $arg1
	set $pattern = (short) $arg2
	set $p = $start
	while $p < $end
		if (*(short *) $p) == $pattern
			printf "pattern 0x%hx found at 0x%x\n", $pattern, $p
		end
		set $p++
	end
end
document search
	Search for the given pattern beetween $start and $end address.
	Usage: search <start> <end> <pattern>
end

define dis
	if $argc == 0
		disassemble
	end
	if $argc == 1
		disassemble $arg0
	end
	if $argc == 2
		disassemble $arg0 $arg1
	end
	if $argc > 2
		help dis
	end
end
document dis
Disassemble something.
dis <from> <to>

Without arg, disassemble the whole function around PC.
One arg, disassemble the whole function around this arg.
Two args, disassemble this memory range.
end

define xxd
	if $argc != 2
		help xxd
	else
		set pagination off
		dump binary memory /tmp/gdbdump.bin $arg0 $arg0+$arg1
		shell xxd -g 4 /tmp/gdbdump.bin
		shell rm -f /tmp/gdbdump.bin
		set pagination on
	end
end
document xxd
Usage: xxd <startaddress> <length>

Do a hexdump and try to show ascii-representations of each byte.
end


### Python helpers

define pyo
	# side effect of calling _PyObject_Dump is to dump the object's
	# info - assigning just prevents gdb from printing the
	# NULL return value
	set $_unused_void = _PyObject_Dump($arg0)
end
document pyo
	Prints a representation of the object to stderr, along with the
	number of reference counts it current has and the hex address the
	object is allocated at.  The argument must be a PyObject*
end

define pyg
	print _PyGC_Dump($arg0)
end
document pyg
	Print _PyGC_Dump(arg), arg must me a PyObject *.
end


### Random stuff

define less
python argc = $argc
python
import os
err_write = lambda f, s: f.write(s) if f else print(s)
try:
    f = None
    f = os.popen("less -S -i -R -M --shift 5", mode="w")
    f.write(
        gdb.execute(' '.join(f"$arg{i}" for i in range(0, argc)),
                    to_string=True)
    )
except gdb.error as e:
    err_write(f, f'GDB Error: {e}')
except Exception as e:
    err_write(f, f'Error {type(e)}: {e}')
finally:
    if f:
        f.close()
end
end
document less
	Run any gdb command piped into less.
	So you can finally search and page the output nicely.
end


### Qt debugging helpers

define printqs4
	printf "(Q4String)0x%x (length=%i): \"", &$arg0, $arg0.d->size

	set $i=0
	while $i < $arg0.d->size
		set $char=$arg0.d->data[$i++]
		if $char < 32 || $char > 127
			printf "\\u0x%04x", $char
		else
			printf "%c", (char)$char
		end
	end
	printf "\"\n"
end

define printqs5static
	set $data=$arg0.d
	printf "(Q5String)0x%x length=%i: \"", &$arg0, $data->size

	set $i=0
	set $chararray=(const ushort*)(((const char*)$data)+$data->offset)
	while $i < $data->size
		set $char=$chararray[$i++]
		if $char < 32 || $char > 127
			printf "\\u%04x", $char
		else
			printf "%c" , (char)$char
		end
	end
	printf "\"\n"
end

define printqs5dynamic
	set $data=(QStringData*)$arg0.d
	printf "(Q5String)0x%x length=%i: \"", &$arg0, $data->size

	set $i=0
	while $i < $data->size
		set $c=$data->data()[$i++]
		if $c < 32 || $c > 127
			printf "\\u%04x", $c
		else
			printf "%c", (char)$c
		end
	end
	printf "\"\n"
end

### Debugging helper scripts
# TODO: remove hardcoding...

python

# eigen
import sys
sys.path.insert(0, '/home/jj/devel/eigen/')
from eigen_printers import register_eigen_printers
register_eigen_printers(None)

# kernel debugging
gdb.execute('add-auto-load-safe-path /usr/src/linux/scripts/gdb/vmlinux-gdb.py')
end


### Voltron multi-window gdb
python
try:
    import voltron
    from path import Path

    entryfile = Path(voltron.__file__).dirname() / 'entry.py'
    gdb.execute('source {}'.format(entryfile))

except ImportError:
    pass
end
