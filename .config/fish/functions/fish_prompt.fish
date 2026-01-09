function fish_prompt
        set_color 51afef; echo -n $USER
        set_color normal
        set_color c678dd; echo -n @
        set_color e06c75; echo -n $hostname
        set_color 98c379; echo -n "" (prompt_pwd)

        set_color da8548; echo " \$ "
end
