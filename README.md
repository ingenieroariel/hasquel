# hasquel
Hasquel utility to play with GeoNames CSVs.
It is intended to be used as Sample Haskell code for my interviews

## Development

head US.txt | nix-shell --run "runhaskell hasquel.hs"

This repo contains a 10 records US.txt file, but the original file can be found at http://download.geonames.org/export/dump/US.zip (It is called US.txt)

## Testing

`doctest hasquel.hs`

## Compilation

`nix-build release.nix` and execute by doing ./result/bin/hasquel


