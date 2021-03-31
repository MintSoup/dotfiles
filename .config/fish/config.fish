set -x COLORTERM truecolor

alias nf="neofetch | lolcat --freq=0.25"
alias ws="watch -n0.5 sensors"

alias nvc="rm ~/.local/share/nvim/swap/*"
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
export EDITOR=nvim;

bind \el 'echo; lsd -l; commandline -f repaint'
function __fish_command_not_found_handler --on-event fish_command_not_found
    echo "fish: Unknown command '$argv'"
end

tty | grep "tty1"
if [ $status = 0 ]
	startx
end

