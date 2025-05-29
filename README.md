[![CI](https://github.com/lexitree-labs/LEDES/actions/workflows/ci.yml/badge.svg)](https://github.com/lexitree-labs/LEDES/actions/workflows/ci.yml)

[Documentation](https://lexitree-labs.github.io/LEDES/LEDES)

# LEDES

Legal Electronic Data Exchange Standard(https://ledes.org/)

Currently supports the parsing and validation of the following formats:

- LEDES98BI

- LEDES1998B

Two errors reporting modes are supported:

- eager: the first error encountered is bubbled up, fast path

- greedy: collects all errors, slow path

## Parsing example

```ocaml
open Ledes

let read_lines filename =
  let lines = ref [] in
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      lines := line :: !lines
    done;
    []
  with End_of_file ->
    close_in ic;
    List.rev !lines

let lines = read_lines "ledes98bi.txt" in
  let err = Ledes1998b.parse_and_validate_greedy lines in
  if err = [] then print_endline "successful" else print_endline @@ String.concat "\n" err;
```

## Command line utility

A CLI app is also included which supports processing a LEDES file or a data string

Download the latest release:

Linux: https://github.com/Lexitree-Labs/LEDES/releases/download/0.1.0/ledes_cli_linux

MACOS: https://github.com/Lexitree-Labs/LEDES/releases/download/0.1.0/ledes_cli_macos

WINDOWS: https://github.com/Lexitree-Labs/LEDES/releases/download/0.1.0/ledes_cli_windows.exe

```
NAME
       ledes_cli - A tool for parsing and validating LEDES file format
       Example: ledes_cli_linux -- --eager --LEDES1998B --file /path/to/ledes199b

SYNOPSIS
       ledes_cli [OPTION]…

Error collection options
       --eager
           Eager mode stops on first error encountered

       --greedy
           Greedy mode collects all error encountered

Data format options
       --LEDES1998B
           Process LEDES1998B data format

       --LEDES98BI
           Process Use LEDES98BI data format

Input options
       --file=FILE
           Specify data file location

       --line=LINE
           Specify data string
```

## CAVEAT

Certain field validations, e.g. field totals which are open to interpretations on both sending and receiving end are omitted. Due to certain LEDES standard inconsistencies, few
fields are validated in a more stricter fashion for total correctness.

## License

This project is licensed under the [MIT license].

[MIT license]: https://github.com/lexitree-labs/LEDES/blob/main/LICENSE

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in LEDES by you, shall be licensed as MIT, without any additional
terms or conditions.