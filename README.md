# DRY

This tool helps you abstract away common or repeated chunks of code by finding similar pieces.
It will allow you to avoid repeating yourself with less effort on your side.

It only works with ES5 for now (you can use babel-cli or similar tools to transpile more recent versions to ES5).
Use `es5` branch for that.

The `master` branch will hold the version that makes use of [Semantic](https://github.com/github/semantic)'s json output.
(Because as of now, Semantic [isn't published to hackage](https://github.com/github/semantic/issues/16) yet).

# How To Build

1. `git clone https://github.com/ch3rn0v/dry.git`
2. `cd dry`
3. `stack build`
4. `stack exec dry-exe <path_to_the_json_file> <path_to_the_output_csv_file>`

Where:

- `<path_to_the_json_file>` is an absolute path in double quotes
- `<path_to_the_output_csv_file>` is an absolute path in double quotes

Example:

`stack exec dry-exe "/Users/**_/dev/parsed.json" "/Users/_**/dev/analysis/results.csv"`

Instead of steps 3 and 4 you can just do:
`stack run dry-exe "/Users/**_/dev/parsed.json" "/Users/_**/dev/analysis/results.csv"`

# Acknowledgements

This tool wouldn't be possible without awesome Haskell community,
[@haskellru](https://t.me/haskellru) in particular.

You can find the list of libraries used in package.yaml
