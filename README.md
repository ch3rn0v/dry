# DRY

This tool helps you abstract away common or repeated chunks of code by finding similar pieces.
It allows you to avoid repeating yourself with less effort on your side.

The tool makes use of [Semantic](https://github.com/github/semantic)'s json output.
(Because as of now, Semantic [isn't published to hackage](https://github.com/github/semantic/issues/16) yet).

It supports all the languages that Semantic [supports](https://github.com/github/semantic#language-support)

# How to Build AST in JSON

Before one can run DRY, it's required to first have an AST to analyze.
DRY expects one in particular json format, the one that Semantic outputs.

1. Pull [Semantic](https://github.com/github/semantic/packages/11609)'s container.

2. Run it, passing a command like this:

```
docker run -v /root/path/to/a/project:/r --entrypoint sh semantic -c "/usr/local/bin/semantic -- parse --json \`find /r/internal/path -name *.js\`" | tail -n 1 > /path/to/output.json`
```

Where:

- `/root/path/to/a/project` is the root path to the directory of the project one wants to analyze.
- `/r` is its root path within Docker's container.
- `find /r/internal/path -name *.js` is the command to find _all_ files that have extension ".js" and reside in `/root/path/to/a/project/internal/path`.
Notice that the command is wrapped in escaped backticks. Sematic accepts a list of filepaths to be parsed as an argument. `find` returns them, and backticks are required in order to pass `find`'s output as an argument to `semantic`. Finally, the backticks have to be escaped in order to make the command run within Docker's container (as opposed to the shell where the whole `docker run ...` command is invoked).

Alternatively, one can provide the root path to a source file to be analyzed, or a number of them. `find` is only used to enumerate them automatically.

# How to Build DRY

1. `git clone https://github.com/ch3rn0v/dry.git`
2. `cd dry`
3. `stack build`

# How to Execute DRY

`stack exec dry-exe <path_to_the_json_file> <path_to_the_output_csv_file>`

Where:

- `<path_to_the_json_file>` is an absolute path in double quotes
- `<path_to_the_output_csv_file>` is an absolute path in double quotes

Example:

`stack exec dry-exe "/Users/**/dev/parsed.json" "/Users/**/dev/analysis/results.csv"`

Instead of `stack build` followed by `stack exec` you can use `stack run`, like this:

`stack run dry-exe "/Users/**/dev/parsed.json" "/Users/**/dev/analysis/results.csv"`

# Example of DRY Output's Visualization

![DRY output's visualization](https://i.imgur.com/sSBulR8.png)
![DRY output's visualization](https://i.imgur.com/EQSUkwh.png)

[Source code for visualizations](https://github.com/ch3rn0v/dry-viz/blob/master/Function%20Similarity%20Analysis.ipynb)

# Acknowledgements

- This tool wouldn't have been possible without awesome Haskell community,
[@haskellru](https://t.me/haskellru) in particular.
- [Semantic](https://github.com/github/semantic)'s staff members helped a lot to run their tool.
- And a great thank you to [@python273](https://github.com/python273) who sped up the process by helping with Docker.

You can find the list of libraries used in package.yaml
