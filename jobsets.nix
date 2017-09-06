{ nixpkgs, declInput }: let pkgs = import nixpkgs {}; in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
        "blog": {
            "enabled": 1,
            "hidden": false,
            "description": "blog",
            "nixexprinput": "blog",
            "nixexprpath": "release.nix",
            "checkinterval": 300,
            "schedulingshares": 1,
            "enableemail": false,
            "emailoverride": "",
            "keepnr": 5,
            "inputs": {
                "nixpkgs": { "type": "git", "value": "https://github.com/NixOS/nixpkgs.git release-17.03", "emailresponsible": false },
                "blog": { "type": "git", "value": "https://github.com/qfpl/blog", "emailresponsible": false },
                "reflex-tutorial": { "type": "git", "value": "https://github.com/qfpl/reflex-tutorial", "emailresponsible": false }
            }
        }
    }
    EOF
  '';
}
