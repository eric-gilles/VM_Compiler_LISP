{ pkgs }: {
    deps = [
		pkgs.nodePackages.prettier
        pkgs.cowsay
        pkgs.clisp
    ];
}