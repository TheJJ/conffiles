# JJ's zshrc
# Copyright (c) 2011 - 2015 Jonas Jelten
#
# Released under GPLv3 or later.
#
# features of this config:
# * completion
# * history
# * directorystack
# * funny aliases
# * dircolors for ls
# * ssh-agent client
# * colored prompt

# if not running interactively, don't do anything!
[[ $- != *i* ]] && return

# source system profile
[[ -r /etc/profile ]] && source /etc/profile

# home bin dir path
homebindir="$HOME/bin"
if [[ -d $homebindir ]]; then
	export PATH="$PATH:$homebindir"
fi

# machine-specific files
machineconfdir="$HOME/.sftconf.d"
if [[ -d $machineconfdir ]]; then
	for f in $machineconfdir/*; do
		source $f
	done
fi

# ssh-agent launched via systemd user service
sshagentsocket="$XDG_RUNTIME_DIR/ssh-agent.socket"
if [[ -r $sshagentsocket ]]; then
	export SSH_AUTH_SOCK=$sshagentsocket

	sshkey="$HOME/.ssh/id_rsa"

	# if no key was added yet
	ssh-add -l > /dev/null
	if [[ ! $? -eq 0 ]]; then
		echo "no ssh"
		# alias ssh to agent-add, delete the alias when password was entered.
		alias ssh="ssh-add -l > /dev/null || ssh-add $sshkey && unalias ssh; ssh"
	fi > /dev/null
fi

# environment variables
export VISUAL="vim"
export EDITOR=$VISUAL
export PAGER="less -S -i"
export GCC_COLORS="yes"

# locales
export LANG="en_US.UTF-8"
export LC_TIME="en_DK.UTF-8"
export LC_MONETARY="de_DE.UTF-8"
export LC_MEASUREMENT="de_DE.UTF-8"
export LC_ADDRESS="de_DE.UTF-8"
export LC_TELEPHONE="de_DE.UTF-8"
export LC_PAPER="de_DE.UTF-8"


# append cwd to java class path
export CLASSPATH=.:$CLASSPATH

export GPSD_UNITS=metric

pyrc="$HOME/.pythonrc.py"
if [[ -r $pyrc ]]; then
	export PYTHONSTARTUP=$pyrc
fi

# this will log all ssl key exchanges
#export SSLKEYLOGFILE=$HOME/.ssl-log


# see man zsh, reports cpu/system/etc usage if running longer then n secs
REPORTTIME=1

############################
# aliases for stuff
############################

alias woman="man"
alias python3="python3 -q"
alias p3='python3'
alias p2='python2'
alias py3='p3'
alias py2='p2'
alias p='p3'
alias dmesg='dmesg -L'
alias youtube-dl='noglob youtube-dl'
alias em='emacs -nw'
alias objdump='objdump -M intel-mnemonic -C'
alias gdb='gdb -q'
alias gdbs='gdbserver --once localhost:8888'
alias gdbc='gdb -q -ex "target remote localhost:8888"'
alias bc='bc -q -l'
alias cp='cp --reflink=auto'
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias grepi='grep -i'
alias ag="ag -S"
alias more='less'
alias most='less'
alias less='less -R'
alias mkdir='mkdir -p -v'
alias nano='nano -w -S -F -A'
alias ..='cd ..'
alias ../../='cd ../../'
alias cd..='..'
alias cmatrix="cmatrix -a -b"
alias rd="cd $(pwd)"

# valgrind awesomeness
alias vg="valgrind --leak-check=full --track-origins=yes"
alias vga="vg --show-leak-kinds=all"
alias vgs="vg --vgdb-error=1"
alias vgg="vg --vgdb-error=0 --vgdb=full"
function gdbv() {
	args=""
	gdb -ex "set architecture i386:x86-64:intel" -ex "target remote | vgdb $args" $*
}
export vg="valgrind --leak-check=full --track-origins=yes --vgdb-error=0 --vgdb=full"

alias gschichten='fortune'
alias lol="fortune | ponysay"
alias sido='sudo'
alias psc='ps xawf -eo pid,user,cgroup,args'

alias l='ls'
alias la='ls -A'
alias ll='ls -l'
alias lal='ls -la'
alias lla='ls -la'
alias lr='ls -R'                    # recursive ls
alias lx='ll -BX'                   # sort by extension
alias lz='ll -rS'                   # sort by size
alias lt='ll -rt'                   # sort by date
alias lrandom="ls | sort -R | head -n 1"

alias rm='rm -I --one-file-system'
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

alias ls=' ls --color=auto'
alias nemo='nemo --no-desktop'

alias rmvim="find -type f \( -name \*~ -or -name \*.swp -or -name \*.swo \) -delete"
alias urlencode='python3 -c "import sys, urllib.parse as u; print(u.quote_plus(sys.argv[1]))"'
alias urldecode='python3 -c "import sys, urllib.parse as u; print(u.unquote(sys.argv[1]))"'

hash colordiff 2>/dev/null && alias diff='colordiff'

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

# find by name, optionally in dir
# fname "search" ["startdirectory"]
function fname() {
	if [[ $# -ge 2 ]]; then
		local dir=$1
		shift
		local what=$@
	else
		local dir="."
		local what="$@"
	fi
	find $dir -iname "*$what*"
}

# mkdir and cd to it
function mcd() { mkdir -p "$1" && cd "$1"; }

# show glx/opengl renderer
function glxrenderer() { glxinfo | grep -E "(version |renderer )" }

# gentoo-package query
function xie() { eix -e $(eix --only-names $1 | dmenu -i -l 10) }

# set xterm title
function title() { print -Pn "\e]0;$1 \a" }

# message dialog
function gtkdialog() {
	if [[ "x$1" != "x" ]]; then
		local title=$1
	else
		return
	fi

	if [[ "x$2" != "x" ]]; then
		local addition="d.format_secondary_text('$2');"
	else
		local addition=""
	fi

	python -c "from gi.repository import Gtk; d = Gtk.MessageDialog(None, 0, Gtk.MessageType.INFO, Gtk.ButtonsType.OK, '$title'); $addition d.connect('delete-event', Gtk.main_quit); d.connect('response', Gtk.main_quit); d.show_all(); Gtk.main();"
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

# run shell as user, with benefits of env_keep of sudo
function sudosh() {
	function usage() {
		echo "sudosh <user>"
		echo "run the current shell as another user"
	}
	if [[ $# -ge 2 ]]; then
		usage
		return
	elif [[ $1 == "-h" || $1 == "--help" ]]; then
		usage
		return
	elif [[ x$1 != "x" ]]; then
		user="$1"
	else
		user="root"
	fi

	sudo -u $user $SHELL
}

# connect to host=$1 port=$2 via tor and listen at $3
function torcat() {
	torhost="localhost"
	torport=9050

	listenport=0
	remoteport=0
	host=0

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


####################
# shell setup
####################

# general shell options
setopt extended_glob longlistjobs completeinword completealiases hashlistall bash_rematch nohup nobeep
unsetopt autocd beep notify nomatch

# no rehash for directory contents
setopt nohashdirs

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
HISTSIZE=10000000
SAVEHIST=$HISTSIZE
setopt append_history share_history extended_history histverify histignorespace histignoredups

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
zstyle ':vcs_info:*' enable git svn
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


jj-copy-region-as-kill () {
	zle copy-region-as-kill
	print -rn $CUTBUFFER | xsel -i
}
zle -N jj-copy-region-as-kill

jj-kill-region () {
	zle kill-region
	print -rn $CUTBUFFER | xsel -i
}
zle -N jj-kill-region

jj-yank () {
	CUTBUFFER=$(xsel -o)
	zle yank
}
zle -N jj-yank


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
bindkey "^S"    history-incremental-pattern-search-forward

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
#bindkey '^[w'           jj-copy-region-as-kill
#bindkey '^W'            jj-kill-region
#bindkey '^Y'            jj-yank


###############################
# dircolors for ls
###############################

#no   NORMAL, NORM          Global default, although everything should be something
#di   DIR                   Directory
#fi   FILE                  Normal file
#ln   SYMLINK, LINK, LNK    Symbolic link. If you set this to ‘target’ instead of a numerical value, the color is as for the file pointed to.
#pi   FIFO, PIPE            Named pipe
#so   SOCK                  Socket
#bd   BLOCK, BLK            Block device
#cd   CHAR, CHR             Character device
#or   ORPHAN                Symbolic link pointing to a non-existent file
#mi   MISSING               Non-existent file pointed to by a symbolic link (visible when you type ls -l)
#ex   EXEC                  Executable file (i.e. has ‘x’ set in permissions)
#su   SETUID                File that is setuid (u+s)
#sg   SETGID                File that is setgid (g+s)
#tw   STICKY_OTHER_WRITABLE Directory that is sticky and other-writable (+t,o+w)
#ow   OTHER_WRITABLE        Directory that is other-writable (o+w) and not sticky
#st   STICKY                Directory with the sticky bit set (+t) and not other-writable
#lc   LEFTCODE, LEFT        Opening terminal code
#rc   RIGHTCODE, RIGHT      Closing terminal code
#ec   ENDCODE, END          Non-filename text
#mh   MULTIHARDLINK         Regular file[s] with more than one link
#ca   CAPABILITY            File with capability
#*.extension                Every file using this extension e.g. *.jpg
#
#
# 0   = default colour
# 1   = bold
# 4   = underlined
# 5   = flashing text
# 6   = no change
# 7   = reverse field
# 8   = hidden (black)
# 9   = strikethrough (cool!)
# 10 - 29 = no change
# 30  = light green
# 31  = red
# 32  = green
# 33  = orange
# 34  = blue
# 35  = purple
# 36  = cyan
# 37  = grey
# 38  = underline
# 39  = no change
# 40  = black background
# 41  = red background
# 42  = green background
# 43  = orange background
# 44  = blue background
# 45  = purple background
# 46  = cyan background
# 47  = grey background
# 90  = dark grey
# 91  = light red
# 92  = light green
# 93  = yellow
# 94  = light blue
# 95  = light purple
# 96  = turquoise
# 100 = dark grey background
# 101 = light red background
# 102 = light green background
# 103 = yellow background
# 104 = light blue background
# 105 = light purple background
# 106 = turquoise background
#
# combine parameters like:
# di=5;31;42 => flashing red on green bg

imgc="95"
confc="91"
arc="33"
compc="92"
rawc="47;34"
export LS_COLORS="rs=00:no=00:di=01;36:ln=01;04;33:mh=04:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;35;01:or=01;09;31;40:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42;05:st=37;44:ex=32:*.xz=33:*.jpg=$imgc:*.png=$imgc:*.bmp=$imgc:*.gif=$imgc:*.cfg=$confc:*.ini=$confc:*.conf=$confc:*.cnf=$confc:*.pref=$confc:*rc=$confc:*.tar=$arc:*.zip=$arc:*.xz=$compc:*.gz=$compc:*.bz=$compc:*.lzma=$compc:*.gpg=44;93:*.img=$rawc:*.dat=$rawc:*core=31;04:*.bak=32"
#export LS_COLORS='rs=00:no=00:'


##########################################
# termcap colors for man page highlights
##########################################

# man 5 termcap
export TERMCAP_mb=$'\E[01;31m'       # begin blinking
export TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export TERMCAP_me=$'\E[0m'           # end mode
export TERMCAP_se=$'\E[0m'           # end standout-mode
export TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export TERMCAP_ue=$'\E[0m'           # end underline
export TERMCAP_us=$'\E[04;38;5;146m' # begin underline



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
zstyle ':completion:*' matcher-list '' '' '' ''
zstyle ':completion:*' menu select=1
zstyle ':completion:*' original true
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' select-prompt %S selection at %p%s
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' substitute 1
zstyle ':completion:*' verbose true
zstyle ':compinstall' filename '~/.zshrc'


# init the zsh completion
autoload -U compinit
compinit

# init the bash compatibility completion
autoload -U bashcompinit
bashcompinit

# manpages
zstyle ':completion:*:manuals'    separate-sections true
zstyle ':completion:*:manuals.*'  insert-sections   true
zstyle ':completion:*:man:*'      menu yes select

# processes
zstyle ':completion:*:processes'  command 'ps -au$USER'
zstyle ':completion:*:processes-names' command 'ps c -u ${USER} -o command | uniq'

# urls
zstyle ':completion:*:urls' local 'www' 'public_html' '/srv/http'

# host completion, guttenberg'd from grml config
test -r ~/.ssh/known_hosts && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
test -r /etc/hosts && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()
hosts=(
	$(hostname)
	"$_ssh_hosts[@]"
	"$_etc_hosts[@]"
	8.8.8.8
	2001:4860:4860::8888
	google.com
	127.0.0.1
	::1
	localhost
)
zstyle ':completion:*:hosts' hosts $hosts

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
	#test $vcs_info_msg_1_ && psvar[4]="$vcs_info_msg_1_" || psvar[4]=`pwd`
}

# best prompt ever!!11111
PROMPT="%B%{$fg[green]%}%n%{$fg[cyan]%}@%{$fg[blue]%}%m%b %{$fg[red]%}%~ %{$fg[yellow]%}%1v%2v%{$reset_color%}%# "
RPROMPT="%3v%4v%{$reset_color%}[%{$fg[yellow]%}%?%{$reset_color%}]%1v%{$fg[blue]%}:%{$fg[red]%}%l%{$reset_color%} "
