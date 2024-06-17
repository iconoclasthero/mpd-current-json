# v1.2.0.0
- Move literate Org Mode code to LITERATE.org file
- Move functions from executable source Main.hs to their own library
- Bump dependency versions for `aeson` and `bytestring`

# v1.1.0.2
[comment]: # (2023-10-23)
- Fixed cabal `build-depends` version bounds for Arch Linux dynamic
  building.

# v1.1.0.1
[comment]: # (2023-10-17)
- Added haddock comments
- Addressed `cabal check` warnings;
- setup for uploading as a Hackage package.

# v1.1.0.0
[comment]: # (2023-06-11)
- Remove `-h` from `--help` and use `-h` for `--host`
- Make `--help` option hidden in the help message

# v1.0.0.0
[comment]: # (2023-06-08)
Initial working version
- Added conditional tags printing, only non-empty values are printed
- Accept host, port and password
- Nested json objects for `status` and `tags`
- Added `elapsed_percent` key shortcut for `elapsed / duration * 100`

# v0.0.1.0
[comment]: # (2023-06-01)
- initial connection and parsing values
- First version. Released on an unsuspecting world.
