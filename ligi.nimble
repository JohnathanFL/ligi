# Package

version       = "0.1.0"
author        = "JohnathanFL"
description   = "Another attempt at a Ligi compiler in Nim"
license       = "GPL-3.0"
srcDir        = "src"
installExt    = @["nim"]
bin           = @["ligi"]

backend       = "cpp"

# Dependencies

requires "nim >= 1.3.5"
