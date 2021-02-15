function fish_prompt
        set_color 2397de; echo -n $USER
        set_color normal
        set_color ff00ff; echo -n @
        set_color ff7410; echo -n $hostname
        set_color 00ff00; echo -n "" (prompt_pwd)

        set_color ffff00; echo "\$ "
end
