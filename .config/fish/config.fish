set -x COLORTERM truecolor

alias nf="neofetch | lolcat --freq=0.25"
alias ws="watch -n0.5 sensors"

alias nvc="rm ~/.local/share/nvim/swap/*"
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
export EDITOR=nvim;

bind \el 'echo; lsd -l; commandline -f repaint'

tty | grep "tty1"
if [ $status = 0 ]
	startx
end
