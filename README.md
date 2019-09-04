# DRY

This tool helps you abstract away common or repeated chunks of code by finding similar pieces.
It allows you to avoid repeating yourself with less effort on your side.

The tool makes use of [Semantic](https://github.com/github/semantic)'s json output.
(Because as of now, Semantic [isn't published to hackage](https://github.com/github/semantic/issues/16) yet).

It supports all the languages that Semantic [supports](https://github.com/github/semantic#language-support)

# How To Build

1. `git clone https://github.com/ch3rn0v/dry.git`
2. `cd dry`
3. `stack build`

# How To Execute

1. `stack exec dry-exe <path_to_the_json_file> <path_to_the_output_csv_file>`

Where:

- `<path_to_the_json_file>` is an absolute path in double quotes
- `<path_to_the_output_csv_file>` is an absolute path in double quotes

Example:

`stack exec dry-exe "/Users/**/dev/parsed.json" "/Users/**/dev/analysis/results.csv"`

Instead of `stack build` followed by `stack exec` you can use `stack run`, like this:

`stack run dry-exe "/Users/**/dev/parsed.json" "/Users/**/dev/analysis/results.csv"`

# Acknowledgements

This tool wouldn't have been possible without awesome Haskell community,
[@haskellru](https://t.me/haskellru) in particular.

You can find the list of libraries used in package.yaml
