# JJ's zshrc
# Copyright (c) 2011 - 2023 Jonas Jelten
#
# Released under GPLv3 or later.
#
# this config is supposed to be _unpersonalized_!
# it should run on any modern system,
# and has no weird hardcoded things working on one device only.
#
# if you need special device-specific settings (env vars, functions, etc)
# use shell files in $HOME/.config/profile/...
#
# custom programs can be placed in $HOME/bin/...
#
# features of this config:
# * completion
# * history
# * directorystack
# * funny aliases
# * dircolors for ls
# * ssh-agent client
# * colored prompt


# home bin dir path
local homebin="$HOME/bin"
if [[ -d $homebin && ! "$PATH" =~ .*":$homebin:?".* ]]; then
	export PATH="$PATH:$homebin"
fi

# ssh-agent launched via systemd user service
# systemctl --user enable --now ssh-agent.service
local sshagentsocket="$XDG_RUNTIME_DIR/ssh-agent.socket"
if [[ -r $sshagentsocket ]]; then
	export SSH_AUTH_SOCK=$sshagentsocket
fi
unset sshagentsocket

# environment variables
# basic tool config
export VISUAL="vim"
export EDITOR=$VISUAL
export LESS="-S -i -R -M --shift 5"
export PAGER="less"

# color gcc & cmake output
export CMAKE_COLOR_DIAGNOSTICS="ON"

# silence funny gtk warnings
export NO_AT_BRIDGE=1

# global python pycache directory
export PYTHONPYCACHEPREFIX=$HOME/.cache/python/

# this will log all ssl key exchanges
#export SSLKEYLOGFILE=$HOME/.ssl-log

pyrc="$HOME/.pythonrc.py"
if [[ -r $pyrc ]]; then
	export PYTHONSTARTUP=$pyrc
fi
unset pyrc

# locales
# test availability if these with `locale -a`
export LANG="en_US.UTF-8"
export LC_TIME="en_DK.UTF-8"
export LC_MONETARY="de_DE.UTF-8"
export LC_MEASUREMENT="de_DE.UTF-8"
export LC_ADDRESS="de_DE.UTF-8"
export LC_TELEPHONE="de_DE.UTF-8"
export LC_PAPER="de_DE.UTF-8"


### no more setting env vars after this point:
### non-interactive mode does not process further.
# we do this so for example `env -i -- zsh -c env` prints the correct env,
# even it is not interactive. this is used by emacs for example.
# (this works since .zshenv sources this file when not interactive)

[[ $- != *i* ]] && return


########################################
# init the zsh completion
autoload -U compinit
compinit

# init the bash compatibility completion
autoload -U bashcompinit
bashcompinit
########################################

############################
# aliases for stuff
############################

# non-gnu tools don't understand some options
ON_MAC=0
ON_LINUX=0
case `uname` in
Linux)
	ON_LINUX=1
	;;
Darwin)
	ON_MAC=1
	;;
esac

alias python3="python3 -q"
alias p3='python3'
alias p2='python2'
alias py3='p3'
alias py2='p2'
alias py='p3'
alias p='py'
alias ipy='ipython'
alias pytrace='python3 -m trace -g -t'
compdef py=python3 p3=python3 p=python3 p2=python2 py2=python2 pytrace=python3 ipy=ipython 2>/dev/null

alias emacsnw='emacs -nw'
alias emacsopen="emacsclient -n"      # reuse frame
alias eo=emacsopen
alias emacsopennew="emacsclient -n -c"  # new frame
alias eon=emacsopennew
alias emacsterm='emacsclient --tty'   # connect in current tty
alias emacscc="emacs -batch -no-site-file -f batch-byte-compile"  # compile some elisp file(s)
alias ecc=emacscc
alias indentemacs=$'emacs -q --no-site-file --batch -l .emacs.d/early-init.el --eval "(doom-require \'doom-start)" --eval "(batch-indent)"'

# also consider the gdbv function below which attaches gdb to the above commands
alias ......='cd ../../../'
alias ....='cd ../../'
alias ../../../='cd ../../../'
alias ../../='cd ../../'
alias ..='cd ..'
alias ag="ag -S"
alias aga='ag --passthrough'  # show all content, but highlight
alias anon=' unset HISTFILE'
alias b='bpy'
alias bc='bc -q -l'
alias bpy='bpython'
alias bridge='bridge --color'
alias cal='cal -m -w'
alias catzero="xargs -0 -L1 -a"  # cat a file like /proc/pid/environ or comm in lines
alias cd..='..'
alias chgrp='chgrp --preserve-root'
(( $ON_LINUX )) && alias chmod="chmod -c --preserve-root"
alias chown='chown --preserve-root'
alias cls="echo -en \\\\033c"  # clear screen and scrollback
alias cmatrix="cmatrix -a -b"
alias confgrep='grep -v -P "^\\s*($|#|;)"'   # good to strip conffile comments
alias curlws='curl --no-buffer --header "Connection: Upgrade" --header "Upgrade: websocket" --header "Sec-WebSocket-Key: bG9sd2Vic29ja2V0Y29ubg==" --header "Sec-WebSocket-Version: 13"'
alias dd="dd status=progress"
alias dmesg='dmesg -L'
alias doom='~/.emacs.d/bin/doom'
alias file='file -L'
alias g++as='g++as23'
alias g++c='g++std23'
alias g++std20='g++ -std=c++20 -Wall -Wextra -pedantic -pthread -fcoroutines'
alias g++std23='g++ -std=c++23 -Wall -Wextra -pedantic -pthread -fcoroutines'
alias g='git'
compdef g=git 2> /dev/null
alias gap='git add -p'
alias gdb='gdb -q'
alias gdbc='gdb -q -ex "target remote localhost:8888"'
alias gdbcx64='gdb -q -ex "set architecture i386:x86-64:intel" -ex "target remote localhost:8888"'
alias gdbs='gdbserver localhost:8888'
alias gdbsm='gdbserver --multi localhost:8888'
alias gdbso='gdbserver --once localhost:8888'
alias grep='grep --color=auto'
alias gschichten='fortune'
alias icat="kitty +kitten icat"
alias ip='ip --color'
alias ipb='ip --color --brief'
alias jsc="js -C ."  # json coloring
alias kittyssh="kitty +kitten ssh"
alias l='ls'
alias lZ='ll -Z'                    # selinux
alias la='ls -A'
alias lal='ls -la'
alias ldbind="LD_DEBUG=bindings ldd"  # show what symbols the linker binds
alias less="less $LESS"
alias lesse="less $LESS +G"
(( $ON_LINUX )) && alias ls='ls --color=auto'
(( $ON_MAC )) && alias ls='ls -G'
alias ll='ls -lhtr'                 # magic
alias lla='ls -la'
alias lol="fortune | ponysay"
alias lr='ls -R'                    # recursive ls
alias lrandom="ls | sort -R | head -n 1"
alias lt='ll -rt'                   # sort by date
alias lx='ll -BX'                   # sort by extension
alias lz='ll -rS'                   # sort by size
alias mkdir='mkdir -p -v'
alias more='less'
alias most='less'
alias mpvll='mpv --profile=low-latency --untimed'
alias nano='nano -w -S -F -A'
alias nautilus="nautilus"
alias nemo='nemo --no-desktop'
alias objdump='objdump -M intel-mnemonic -C'
alias open='xdg-open'
alias pdb="python3 -m pdb"
alias psc='ps xawf -eo pid,user,cgroup,args'
alias rcp="rsync -aHAXP --append-verify"  # yes, this shadows the real rcp
compdef rcp=rsync 2> /dev/null
alias rg='rg -S'
(( $ON_LINUX )) && alias rm='rm -I --one-file-system'
alias rmirror="rsync --recursive --links --perms --times --timeout 180 --safe-links --delete-after --delay-updates --delete --force"
compdef rmirror=rsync 2> /dev/null
alias rmvim="find -type f \( -name \*~ -or -name \*.swp -or -name \*.swo \) -delete"
alias scrollbottom="echo -en '\x1b]721;99999\x07'"    # urxvt even more special command
alias scrolltop="echo -en '\x1b]720;99999\x07'"       # urxvt special command
alias sqlite="sqlite3"
alias urldecode='python3 -c "import sys, urllib.parse as u; print(u.unquote(sys.argv[1]))"'
alias urlencode='python3 -c "import sys, urllib.parse as u; print(u.quote_plus(sys.argv[1]))"'
alias watch='watch -c'
alias xcliptosel="xclip -selection clipboard -o | xclip -i; xclip -selection clipboard -o"
alias xseltoclip="xclip -o | xclip -i -selection clipboard; xclip -o"

# valgrind awesomeness
alias vg="valgrind --leak-check=full --track-origins=yes --track-fds=yes"  # base
vgdb_pipe_prefix="/tmp/vgdb-pipe"
vgdb_pipe_option="--vgdb-prefix=$vgdb_pipe_prefix"
alias vga="vg $vgdb_pipe_option --show-leak-kinds=all"                     # analyze all
alias vgb="vg $vgdb_pipe_option --vgdb-error=0 --vgdb=full"                # (boot) for gdb, halt on startup and at every error
alias vge="valgrind --leak-check=no --track-origins=yes" # only memory errors
alias vgg="vg $vgdb_pipe_option --vgdb=full --vgdb-stop-at=startup"        # for gdb, halt only at startup
alias vgr="vg $vgdb_pipe_option --vgdb-error=1 --vgdb=full"                # (run)  for gdb, halt on every error

hash colordiff 2>/dev/null && alias diff='colordiff' || alias diff='diff --color=auto'

function g++as23() {
	g++ -std=c++23 -Wall -Wextra -pedantic -pthread -fcoroutines -S -masm=intel -o- $@ | c++filt
}


# vim
hash nvim 2>/dev/null && alias vim='nvim'
hash nvim 2>/dev/null && alias vimdiff='nvim -d'

if hash python3 2>/dev/null; then
	# wrap vim to support file:linenumner
	if [[ -x $homebin/viml ]]; then
		alias vim=viml
	fi

	# automatic environment import
	hash envrc 2>/dev/null && eval "$(envrc hook zsh)"
fi

## external tools
# z -> jump to directory.
# recommender system for directories through cd
# stores stuff in ~/.local/share/zoxide/db.zo
hash zoxide 2>/dev/null && eval "$(zoxide init zsh)"

## conffiles management - the whole $HOME is a git repo :)
# usual setup:
# - clone conffiles git repo
# - run bin/conffilesdeploy
#
# the git bare repo is in ~/.conffiles.git
alias conffiles="git --work-tree=$HOME --git-dir=$HOME/.conffiles.git"
compdef conffiles=git 2> /dev/null

# clone and deploy conffiles from some git url
function confclone() {
	test -d "$HOME/.conffiles.git" && echo "you already have ~/.conffiles.git repo" && return
	echo "cloning and deploying config files..."
	# --separate-git-dir=$HOME/.conffiles.git
	git clone $1 /tmp/conffiles-workdir  && \
		/tmp/conffiles-workdir/bin/conffilesdeploy
}

alias conffilestig="GIT_DIR=$HOME/.conffiles.git tig"
compdef conffilestig=tig 2> /dev/null


#####################################
# git-shortcuts
#####################################

# pretty ascii-graph for commit history
function git-g() {
	reldate="%C(bold green)(%ar)%C(reset)"
	fulldate="%C(bold cyan)%aD%C(reset)"
	author="%C(bold white)%aN%C(reset)"
	email="%C(yellow)%aE%C(reset)"
	commitmessage="%C(white)%s%C(reset)"
	commitpointer="%C(bold yellow)%d%C(reset)"
	commithash="%C(bold blue)%h%C(reset)"
	commitfullhash="%C(bold blue)%H%C(reset)"
	newline="%n"

	abouthash="$commithash"
	aboutcommit="$reldate $commitmessage"
	aboutauthor="$author"
	suffix=" $commitpointer"

	# if no args, dump all refs
	if [[ $# -eq 0 ]]; then
		treeish="--all"
	else
		treeish=""
	fi

	while [[ $# -gt 0 ]]; do
		case $1 in
			--)
				# pass everything else to git
				break
				;;
			-a|--all)
				treeish="--all"
				;;
			-e|--email)
				aboutauthor="$author <$email>"
				;;
			-h|--hash)
				abouthash="$commitfullhash"
				;;
			-l|--longdate)
				aboutcommit="$fulldate $reldate $commitpointer $newline          $commitmessage"
				suffix=""
				;;
			--help)
				echo "use the source, luke."
				return
				;;
			--*|-*)
				echo "unknown option '$1'"
				return
				;;
			*)
				# end of options
				break
				;;
		esac
		shift
	done

	format="format:$abouthash - $aboutcommit - $aboutauthor$suffix"
	git log --graph --color --abbrev-commit --decorate --format=$format $treeish $*
}

# completion for git-g
function _git-g() {
	# hmm TODO
	return
}
#compdef _git-g git-g


# linediffs with per-char highlight
function git-cmddiffhighlight() { cmd=$1; shift; git $cmd --color=always $* | diff-highlight | less -L -R }
function git-dh() { git-cmddiffhighlight diff $* }  # <- this is what you wanna use instead of git diff
function git-sh() { git-cmddiffhighlight show $* }  # <- this is for git show

# per-char diffs
alias git-di="git diff --color-words --word-diff-regex='[^[:space:]]|([[:alnum:]]|UTF_8_GUARD)+'"
alias git-die="git diff --word-diff-regex=. "



#####################################
# random functions
#####################################

# unix shadow password checking
# - generate a new hash with random seed for given password
# mksalt determines the algorithm: crypt.mksalt(crypt.METHOD_$x)
# {'SHA512': '6', 'SHA256': '5', 'BLOWFISH': '2b', 'MD5': '1'}
alias gencrypthash="python3 -c 'import crypt, getpass; print(crypt.crypt(getpass.getpass(\"passwd> \"), crypt.mksalt()))'"
# - check hash (either via argv0 or input) against given password
alias checkcrypthash="python3 -c 'import crypt, hmac, getpass, sys; hash=(sys.argv[1] if len(sys.argv) > 1 else input(\"hash> \")); k=(\"kay\" if hmac.compare_digest(crypt.crypt(getpass.getpass(\"passwd> \"), hash), hash) else \"no\"); print(k); exit(0 if k == \"kay\" else 1)'"

# communication with emacs vterm
function vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}


# find by name, optionally in dir
# fname "search" ["startdiretories" ...]
function fname() {
	local what=$1
	local srchdirs
	if [[ $# -ge 2 ]]; then
		shift
		srchdirs=("$@")
	else
		srchdirs=(".")
	fi
	find "${srchdirs[@]}" -iname "*$what*"
}

# find with gnu global tag search
# init in project root with `gtags`
function ftag() {
	global -ix --result=grep --color=always --path-style=shorter $@
}


# dump some interface and trafic remotely over ssh
# and live-display in wireshark-gui
function wiresharkremote() {
    if [[ $# -lt 2 ]]; then
        echo "usage: $0 <hostname> <interface> [filter...]"
        echo ""
        echo "call tcpdump over ssh, and display results live in wireshark"
        return
    fi
    server=$1
    shift
    interface=$1
    shift
    wireshark -kni <(ssh $server "sudo tcpdump -s 0 -n -w - -U -i $interface $@")
}

# ag for two strings in one line
# agl lol rofl -> better than ag lol | ag rofl
function agl() {
	ag "(($1.*$2)|($2.*$1))"
}

# mkdir and cd to it
function mcd() { mkdir -p "$1" && cd "$1"; }

# show glx/opengl renderer
function glxrenderer() { glxinfo | grep -E "(version |renderer )" }

# gentoo-package query
function xie() { eix -e $(eix --only-names $1 | dmenu -i -l 10) }

# set xterm title
function title() { print -Pn "\e]0;$1 \a" }

# get pid of a x window
function xpid() { xprop | grep PID | cut -d" " -f3 }

# message dialog in gtk
function popup() {
	python3 - $@ <<EOP
import argparse
import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk
cli = argparse.ArgumentParser()
cli.add_argument('message')
cli.add_argument('subtitle', nargs='?', default='')
args = cli.parse_args()
d = Gtk.MessageDialog(parent=None,
                      flags=0,
                      message_type=Gtk.MessageType.INFO,
                      buttons=Gtk.ButtonsType.OK,
                      text=args.message)
if args.subtitle:
    d.format_secondary_text(args.subtitle)
d.connect('delete-event', Gtk.main_quit)
d.connect('response', Gtk.main_quit)
d.show_all()
Gtk.main()
EOP
}

# gcc arch flags
function gccflags() {
	if [[ x$1 == "x" ]]; then
		local arch=native;
	else
		local arch=$1
		shift
	fi

	local tmpfile=$(mktemp)
	gcc -c -Q -march=$arch $* --help=target -o $tmpfile
	rm -f $tmpfile
	echo "----------------------------------"
	echo "=== expanded invocation:"
	gcc '-###' -e -v -march=$arch $* /usr/include/stdlib.h 2>&1
}

function gccinvok() {
	# e.g. invoke with -march=native or something like that
	echo "" | gcc -v -E $* - 2>&1 | grep cc1
}


# just return the fucking ip address
function getip() {
	qtype="A"
	if [[ "$1" == "-6" ]]; then
		qtype="AAAA"
		shift
	elif [[ "$1" == "-4" ]]; then
		qtype="A"
		shift
	fi
	local ip=$(dig +short "$qtype" "$1" | tail -n1)
	(test -n $ip && echo $ip) || return 1
}


# try pinging the host until it's reachable.
function tryping() {
	local timeout=1
	local interval=1
	local srv="$1"

	echo -e "starting   $(date -Is)"
	local i=0
	while true; do
		ping -q -W "$timeout" -c1 "$srv" > /dev/null
		if [ $? -eq 0 ]; then
			whence notify-send >/dev/null && notify-send "$srv is back!"
			tput bel
			echo -e "$srv is back!"
			break;
		fi
		printf "\rtry %3d at $(date -Is)" $i
		sleep "$interval"
		i=$((i + 1))
	done
}

# ping the default gateway
pingd () {
	if [[ $# -eq 1 && $1 == "-6" ]]; then
		ping -6 "$(ip -6 -j route list match ::/0 | jq '.[0].gateway' -r)"
	else
		ping "$(ip -j route list match 0.0.0.0/0 | jq '.[0].gateway' -r)"
	fi
}


# test many hosts if they respond
function isup() {
	command -v fping >/dev/null || (echo "fping missing"; return)
	fping -c1 -t100 "$@" 2>&1 | \
		awk -F"[:/]" '/rcv/ {print $1, $5}' | \
		sed 's/ 1/ \x1b[32mup\x1b[0m/g;s/ 0/ \x1b[31mdown\x1b[0m/g'
}


# connect to host=$1 port=$2 via tor and listen at $3
function torcat() {
	local torhost="localhost"
	local torport=9050

	local listenport=0
	local remoteport=0
	local host=0

	while getopts -o"l:p:h:" -l "help" -- "$@"; do
		echo $opt
		case $opt in
			l)
				listenport=$OPTARG
				;;
			p)
				remoteport=$OPTARG
				;;
			h)
				host=$OPTARG
				;;
			help)
				echo "Use the source, luke."
				;;
			\?)
				echo "Invalid option: -$OPTARG" >&2
				return
				;;
			:)
				echo "Option -$OPTARG requires an argument." >&2
				return
				;;
			--)
				shift
				break
		esac
	done

	echo "starting socat" >&2

	socat -ddd TCP4-LISTEN:${listenport},fork SOCKS4A:${torhost}:${host}:${remoteport},socksport=${torport}
}

# cat all files in current folder or matching pattern
function catall() {
	local list
	if [[ $# -ge 1 ]]; then
		list=("$@")
	else
		setopt nullglob
		list=(*)
	fi

	printf "\x1b\x5b\x34\x31m${#list[@]} files\x1b\x5bm\n\n"

	for f in "${list[@]}"; do
		if [[ ! -f $f ]]; then
			continue
		fi

		printf "+++ \x1b\x5b\x33\x32m${f}\x1b\x5bm +++\n"

		cat "$f";
		printf "\n"
	done
}

# cat a certificat chain, if you want the long version, pass -text
function catchain() {
	bundle="$1"
	shift
	openssl crl2pkcs7 -nocrl -certfile $bundle | openssl pkcs7 -print_certs -noout $@
}


# show the zsh completion file origin
function completionfile() {
	if [[ $# -ne 1 ]]; then
		echo "enter command name"; return 1;
	elif [[ $1 = "--help" ]]; then
		echo "usage: $0 command_name"; return 1;
	fi

	print -l ${^fpath}/_$1(N)
}

function _completionfile() {
	_arguments '1:command: _command_names -e'
}
compdef _completionfile completionfile


# less with highlighting
function lessh() {
	local theme=candy
	LESSOPEN="| /usr/bin/highlight -O xterm256 -s $theme %s" less $*
}
compdef lessh=less 2> /dev/null


# invoke gdb and attach it to valgrind
function gdbv() {(
	echo "launching gdb for valgrind..."
	setopt nullglob
	unsetopt bash_rematch
	local pipes=(${vgdb_pipe_prefix}-to-vgdb*)
	local pipe_pidmap=()
	pidoption=""  # select default pid (if there's only one pipe)
	if [ ${#pipes[@]} -eq 0 ]; then
		echo "no valgrind has opened a pipe"
		return
	elif [[ ${#pipes[@]} -ge 2 ]]; then
		echo "more pipes found, please select correct pid:"
		for ((i = 1, j=1; i <= ${#pipes[@]}; i++)); do
			if [[ "${pipes[i]}" =~ 'to-vgdb-from-([^-]+)-by-([^-]+)' ]]; then
				pid=$match[1]
				uid=$match[2]
				debugcmd=($(xargs -0 -L1 -a /proc/$pid/cmdline))
				((cmdstart=1))
				for cmdpart in ${debugcmd[2,-1]}; do
					if [[ "$cmdpart" =~ "^-" ]]; then
						((cmdstart++))
					else
						break
					fi
				done
				((cmdstart++))
				echo "[$j]: <$uid> $pid: ${debugcmd[$cmdstart,-1]}"
				pipe_pidmap[$j]=$pid

				((j++))
			else
				echo "failed parsing pipe filename: ${pipes[i]}"
				return
			fi
		done

		echo -n "enter pipe id [default=1]: "
		while true; do
			read selected_pipe_id
			cont=1
			case $selected_pipe_id in
				'')
					selected_pipe_id=1
					cont=0
					;;
				*[!0-9]*)
					echo -n "invalid id entered, try again: "
					;;
				*)
					if [ $selected_pipe_id -lt $j ]; then
						cont=0
					else
						echo -n "id not available, try again: "
					fi
					;;
			esac

			if [ $cont -eq 0 ]; then
				break
			fi
		done

		selected_pid="${pipe_pidmap[$selected_pipe_id]}"
		echo "selected pid: $selected_pid"
		pidoption="--pid=$selected_pid"
	fi
	gdb -ex "target remote | vgdb $vgdb_pipe_option $pidoption" $*
)}
compdef gdbv=gdb 2> /dev/null


# p12 certificate/key extracting
function p12extract() {
	local password
	local list

	if [[ $# -lt 1 ]]; then
		echo "missing .p12 file to extract"
		return
	fi
	list=("$@")

	for f in "${list[@]}"; do
		echo -n "Password to extract $f: "
		read -s password
		echo "\nextracting..."
		openssl pkcs12 -in "$f" -out "${f%.p12}.ca.pem"  -cacerts -nokeys -passin file:<(echo $password) || true
		openssl pkcs12 -in "$f" -out "${f%.p12}.crt.pem" -clcerts -nokeys -passin file:<(echo $password) || return
		openssl pkcs12 -in "$f" -out "${f%.p12}.key.pem" -clcerts -nodes -passin file:<(echo $password)
		chmod 400 "${f%.p12}.key.pem"
	done
}

# create a p12 file
function p12pack() {
	if [[ $# -lt 1 ]]; then
		echo "usage: p12pack <pemfile> <keyfile> [cafile]"
		return
	fi

	pemfile=$1
	keyfile=$2

	if [[ $# -eq 3 ]]; then
		cafile=$3
		caopts=('-CAfile' "$cafile")
	fi
	openssl pkcs12 -export -nodes ${caopts[@]} -in "$pemfile" -inkey "$keyfile" -out "${pemfile%.pem}.p12"
}

alias pemcat="openssl x509 -text -nocert -in"

function texformula() {
	formula="$@"
	pdflatex -output-directory=/tmp -jobname formula "\\def\\formula{$formula}\\documentclass[border=2pt]{standalone}\\usepackage{amsmath}\\usepackage{varwidth}\\begin{document}\\begin{varwidth}{\\linewidth}\\[ \\formula \\]\\end{varwidth}\\end{document}" || return 1
	echo -e "formula written to:\n/tmp/formula.pdf"
}

function git-authors() {
	git blame -w -M -C -C --line-porcelain "$1" | grep '^author ' | sort -f | uniq -ic | sort -h
}

function git-repo-authors() {
	git ls-tree -r -z --name-only HEAD -- "$1" | xargs -0 -n1 git blame -w -M -C -C --line-porcelain HEAD | grep  "^author " | sort -f | uniq -ic | sort -h
}

function supernice {
    ionice --ignore --class 3 nice -n 19 $@ || return 1
}

function netjail {
    supernice firejail --allow-debuggers --profile=nonet $@ || return 1
}

####################
# shell behavior and completion setup
####################

# if emacs tramp is using this shell,
# abort everything and be as dumb as possible
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# reports cpu/system/etc usage if running longer then n secs
REPORTTIME=1

# general shell options
setopt extended_glob longlistjobs completeinword completealiases hashlistall bash_rematch nohup nobeep
unsetopt autocd beep notify nomatch

# no rehash for directory contents
setopt nohashdirs

# print literal * and ? if the * or / do not glob-expand
unsetopt nullglob

# open file suffixes with the given commands
alias -s tex=vim
alias -s html=elinks
alias -s pdf=evince

# help function
autoload -U run-help
autoload run-help-git
autoload run-help-svn
autoload run-help-svk
alias help=run-help

# command history
HISTFILE=~/.zsh-histfile
HISTSIZE=80000
SAVEHIST=$HISTSIZE
HISTORY_IGNORE="(ls|pwd|exit|cd ..)"
setopt append_history share_history extended_history histverify histignorespace histignoredups
histsearch() { fc -lim "*$@*" 0 }

# directory history and stack
DIRSTACKSIZE=16
alias dh=' dirs -v'
setopt autopushd pushdminus pushdsilent pushdtohome pushdignoredups

# allow # in interactive shell
setopt interactivecomments

# zrcautoload zmv    # who needs mmv or rename?

# color variables
autoload -U colors && colors

# vcs info
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn hg
zstyle ':vcs_info:*' max-exports 4

# check vcs-dirtyness, takes some time in large repos..
zstyle ':vcs_info:*'            check-for-changes false
zstyle ':vcs_info:*'            get-revision    true
zstyle ':vcs_info:*'            stagedstr       "● "
zstyle ':vcs_info:*'            unstagedstr     "# "
zstyle ':vcs_info:(svn|hg):*'   branchformat    "%b:%r"

# replacing:
# %a=action %b=branch %c=stagedstr %u=unstagedstr %i=revision
# %R=basedir %r=reponame %S=subfolder %s=vcsname
zstyle ':vcs_info:*'            formats         "[%r/%b]"       "%c%u"
zstyle ':vcs_info:*'            actionformats   "[%r/%b =>%a]"  "%c%u"


##############################
# key binding
##############################

bindkey -e   # emacs-style

typeset -A key
key[Backspace]=${terminfo[kbs]}
key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}
key[ShiftLeft]=${terminfo[kLFT]}
key[ShiftRight]=${terminfo[kRIT]}
key[ShiftTab]=${terminfo[kcbt]}

bindkey  "${key[Backspace]}" backward-delete-char
bindkey  "${key[Home]}"      beginning-of-line
bindkey  "${key[End]}"       end-of-line
bindkey  "${key[Insert]}"    overwrite-mode
bindkey  "${key[Delete]}"    delete-char
bindkey  "${key[Up]}"        history-beginning-search-backward
bindkey  "${key[Down]}"      history-beginning-search-forward
bindkey  "${key[Left]}"      backward-char
bindkey  "${key[Right]}"     forward-char
bindkey  "${key[PageUp]}"    beginning-of-buffer-or-history
bindkey  "${key[PageDown]}"  end-of-buffer-or-history

# make sure term is in application mode when zle is active
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
	function zle-line-init () {
		printf '%s' "${terminfo[smkx]}"
	}
	function zle-line-finish () {
		printf '%s' "${terminfo[rmkx]}"
	}
	zle -N zle-line-init
	zle -N zle-line-finish
fi

if [[ "$TERM" != "xterm" ]]; then
	bindkey "^H" backward-kill-word
fi

# get keys combos by "cat" or ctrl-v key
bindkey "^R"    history-incremental-pattern-search-backward
bindkey "^T"    history-incremental-pattern-search-forward

#special keys for several terminals
bindkey "\e[1~"         beginning-of-line     # Home
bindkey "\e[2~"         quoted-insert         # Ins
bindkey "\e[3~"         delete-char           # Del
bindkey "\e[4~"         end-of-line           # End
bindkey "\e[5~"         beginning-of-history  # PageUp
bindkey "\e[6~"         end-of-history        # PageDown
bindkey "\e[7~"         beginning-of-line     # Home
bindkey "\e[8~"         end-of-line           # End
bindkey "\e[5C"         forward-word
bindkey "\e[5D"         backward-word
bindkey "\e\e[C"        forward-word
bindkey "\e\e[D"        backward-word
bindkey "^[[1;5C"       forward-word
bindkey "^[[1;5D"       backward-word
bindkey "\eOc"          emacs-forward-word
bindkey "\eOd"          emacs-backward-word
bindkey "\e[Z"          reverse-menu-complete # Shift+Tab
bindkey "\eOF"          end-of-line
bindkey "\eOH"          beginning-of-line
bindkey "\e[F"          end-of-line
bindkey "\e[H"          beginning-of-line
bindkey "\eOF"          end-of-line
bindkey "\eOH"          beginning-of-line
bindkey "^[d"           kill-word
bindkey "^[[3^"         kill-word
bindkey "^[[3;5~"       kill-word


###############################
# dircolors for ls
###############################

# man 5 dir_colors
imgc="95"
confc="91"
arc="33"
compc="92"
rawc="47;34"
export LS_COLORS="rs=00:no=00:di=01;36:ln=01;04;33:mh=04:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;35;01:or=01;09;31;40:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42;05:st=37;44:ex=32:*.xz=33:*.jpg=$imgc:*.png=$imgc:*.bmp=$imgc:*.gif=$imgc:*.cfg=$confc:*.ini=$confc:*.conf=$confc:*.cnf=$confc:*.pref=$confc:*rc=$confc:*.tar=$arc:*.zip=$arc:*.xz=$compc:*.gz=$compc:*.bz=$compc:*.lzma=$compc:*.gpg=44;93:*.img=$rawc:*.dat=$rawc:*core=31;04:*.bak=32"


#################################################
# termcap colors for man page highlights in less
#################################################

# man 5 termcap
export LESS_TERMCAP_mb=$'\E[1;5;31m'           # begin blinking (actually bold red)
export LESS_TERMCAP_md=$'\E[1;38;5;111m'       # begin bold
export LESS_TERMCAP_me=$'\E[0m'                # end so, us, mb, md, mr mode
export LESS_TERMCAP_so=$'\E[1;44;38;5;195m'    # begin standout-mode - info box, search string
export LESS_TERMCAP_se=$'\E[0m'                # end standout-mode
export LESS_TERMCAP_us=$'\E[4;38;5;155m'       # begin underline
export LESS_TERMCAP_ue=$'\E[0m'                # end underline
export LESS_TERMCAP_zzzreset="$reset_color"    # to make `env` output cleaner...


#############################
# zsh autocompletion
#############################

zstyle ':completion::complete:*' use-cache=1

zstyle ':completion:*' auto-description '%d'
zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*' completions 1
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort modification
zstyle ':completion:*' format "%{$fg[yellow]%}%d%{$reset_color%}"
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=1
zstyle ':completion:*' original true
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' select-prompt %S selection at %p%s
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' substitute 1
zstyle ':completion:*' verbose true

zstyle ':completion:*:descriptions' format "%{$fg[yellow]%}%B--- %d%b"
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format "%{$fg[red]%}No matches for:%{$reset_color%} %d"
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'

zstyle ':compinstall' filename '~/.zshrc'

# automatic rehash on completion
# has some performance impact of course
#zstyle ':completion:*:commands' rehash true

# manpages
zstyle ':completion:*:manuals'    separate-sections true
zstyle ':completion:*:manuals.*'  insert-sections   true
zstyle ':completion:*:man:*'      menu yes select

# processes
# unfortunately the _pgrep and _pkill impl doesn't use those yet...
zstyle ':completion:*:processes' force-list always
zstyle ':completion:*:processes' command 'ps -a -u $USER -o pid,user,cmd'
zstyle ':completion:*:processes' list-colors "=(#b) #([0-9]#)*=0=${color[green]}"
zstyle ':completion:*:processes-names' command 'ps c -u ${USER} -o command | sort | uniq'

# process killing
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'
zstyle ':completion:*:*:kill:*:processes' command 'ps -e --forest -o pid,user,tty,cmd | grep -v "]\$"'   # no kernel threads
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

# urls
zstyle ':completion:*:urls' local 'www' 'public_html' '/srv/http'

# cleanup some vars
unset homebin


DONTSETRPROMPT=1
setopt prompt_subst

precmd () {
	vcs_info
	# window title:
	print -Pn "\e]0;%n@%M: %~\a"
	psvar[1]=""
	psvar[2]="$vcs_info_msg_0_"
	psvar[3]="$vcs_info_msg_1_"
	psvar[4]="$vcs_info_msg_2_"
}

if [[ "$INSIDE_EMACS" == "vterm" ]]; then
    NEWLINE=$'\n'
else
    NEWLINE=''
    RPROMPT="%3v%4v%{$reset_color%}[%{$fg[yellow]%}%?%{$reset_color%}]%1v%{$fg[blue]%}:%{$fg[red]%}%l%{$reset_color%}"
fi

# best prompt ever!!11111
PROMPT="%B%{$fg[green]%}%n%{$fg[cyan]%}@%{$fg[blue]%}%m%b %{$fg[red]%}%~ %{$fg[yellow]%}%1v%2v%{$reset_color%}${NEWLINE}%# "

#############################################
# machine-specific config files
_machineconfdir="$HOME/.config/profile"
if [[ -d $_machineconfdir ]]; then
	for f in $_machineconfdir/*; do
		source $f
	done
fi
unset _machineconfdir
