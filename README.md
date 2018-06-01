In order to test, install swi-prolog
in order to know which package you need use one of the following command
    pacman -Ss swi (arch)
or
    apt-cache search swi (debian)
or
    eix swi (gentoo)

then install it
    pacman -S ...
or
    apt-get install ...
or
    emerge --ask ...


Then launch a terminal and cd to this directory and launch swipl exec.pl
then type

    module(exec).
    test(1, _, Y), exec(Y, StateFin).
