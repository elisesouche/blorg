{ emacsPackages
}:

emacsPackages.trivialBuild {
  pname = "blorg";
  version = "0.1.0";
  src = ./.;
  packageRequires = [ emacsPackages.org ];
}
