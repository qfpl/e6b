{ nixpkgs, declInput }: let pkgs = import nixpkgs {}; in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
        "e6b": {
            "enabled": 1,
            "hidden": false,
            "description": "e6b",
            "nixexprinput": "e6b",
            "nixexprpath": "ci/ci.nix",
            "checkinterval": 300,
            "schedulingshares": 1,
            "enableemail": false,
            "emailoverride": "",
            "keepnr": 5,
            "inputs": {
                "e6b": { "type": "git", "value": "https://github.com/qfpl/e6b", "emailresponsible": false },
                "nixpkgs": { "type": "git", "value": "https://github.com/NixOS/nixpkgs.git release-18.09", "emailresponsible": false }
            }
        }
    }
    EOF
  '';
}
