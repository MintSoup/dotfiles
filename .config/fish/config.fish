set -x COLORTERM truecolor

alias ws="watch -n0.5 sensors"

alias nvc="rm ~/.local/share/nvim/swap/*"
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
export EDITOR=nvim

bind \el 'echo; lsd -l; commandline -f repaint'
function __fish_command_not_found_handler --on-event fish_command_not_found
    echo "fish: Unknown command '$argv'"
end

tty | grep tty1
if [ $status = 0 ]
    startx
end

abbr -a -- p paru
abbr -a -- gpom 'git push origin main'
abbr -a -- rs 'rsync -azP'
abbr -a -- ls lsd
abbr -a -- nv nvim
abbr -a -- pss 'paru -Ss'
abbr -a -- ga. 'git add .'
abbr -a -- ps 'paru -S'
abbr -a -- gcm 'git commit -m'
abbr -a -- spc 'sudo pacman -S'
abbr -a -- ys 'youtube-dl --default-search "ytsearch"'
abbr -a -- d dotfiles
abbr -a -- mnt 'sudo mount /dev/sdb1 -t vfat -o umask=0000 /mnt'
abbr -a -- pr 'paru -Rns'
abbr -a -- ds 'dotfiles status'
abbr -a -- sx sxiv
abbr -a -- sp 'sudo pacman -'
abbr -a -- spr 'sudo pacman -Rns'
abbr -a -- spq 'sudo pacman -Q'
abbr -a -- k 'kubectl'
abbr -a -- km 'kubectl -n mirrord'
