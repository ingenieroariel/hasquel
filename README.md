# hasquel
Hasquel utility to play with GeoNames CSVs.
It is intended to be used as Sample Haskell code for my interviews

## Usage
<program> <geonames_file> <lat> <lon>

For example:

runhaskell hasquel.hs US.txt 40.69 -73.82

## Development

nix-shell --run "runhaskell hasquel.hs US.txt"

This repo contains a 10 records US.txt file, but the original file can be found at http://download.geonames.org/export/dump/US.zip (It is called US.txt)

## Testing

`doctest hasquel.hs`

## Compilation

`nix-build release.nix` and execute by doing ./result/bin/hasquel

## Real World Usage

This calculates distance from south richmod in queens to all other locations:

`runhaskell hasquel.hs US.txt 40.693618 -73.821014`
