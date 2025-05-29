open Cmdliner
open Ledes

type mode = Eager | Greedy
type format = LEDES1998B | LEDES98BI
type input = File of string | Line of string

let read_file filename =
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

let read_line s =
  let trim = String.trim s in
  String.split_on_char '\n' trim

let eager_ledes1998b lines = Ledes1998b.parse_and_validate_eager lines
let eager_ledes98bi lines = Ledes98bi.parse_and_validate_eager lines
let greedy_ledes1998b lines = Ledes1998b.parse_and_validate_greedy lines
let greedy_ledes98bi lines = Ledes98bi.parse_and_validate_greedy lines
let docs_mode = "Error collection options"
let docs_format = "Data format options"
let docs_input = "Input options"

let mode_term =
  let eager =
    Arg.(
      value & flag
      & info [ "eager" ] ~doc:"Eager mode stops on first error encountered"
          ~docs:docs_mode)
  in
  let greedy =
    Arg.(
      value & flag
      & info [ "greedy" ] ~doc:"Greedy mode collects all error encountered"
          ~docs:docs_mode)
  in
  Term.term_result
    Term.(
      const (fun e g ->
          match (e, g) with
          | true, false -> Ok Eager
          | false, true -> Ok Greedy
          | false, false ->
              Error (`Msg "Must specify one of --eager or --greedy")
          | true, true ->
              Error (`Msg "Cannot specify both --eager and --greedy"))
      $ eager $ greedy)

let format_term =
  let ledes1998b =
    Arg.(
      value & flag
      & info [ "LEDES1998B" ] ~doc:"Process LEDES1998B data format"
          ~docs:docs_format)
  in
  let ledes98bi =
    Arg.(
      value & flag
      & info [ "LEDES98BI" ] ~doc:"Process Use LEDES98BI data format"
          ~docs:docs_format)
  in
  Term.term_result
    Term.(
      const (fun o1 o2 ->
          match (o1, o2) with
          | true, false -> Ok LEDES1998B
          | false, true -> Ok LEDES98BI
          | false, false ->
              Error (`Msg "Must specify one of --LEDES1998B or --LEDES98BI")
          | true, true ->
              Error (`Msg "Cannot specify both --LEDES1998B and --LEDES98BI"))
      $ ledes1998b $ ledes98bi)

let input_term =
  let file =
    Arg.(
      value
      & opt (some string) None
      & info [ "file" ] ~docv:"FILE" ~doc:"Specify data file location"
          ~docs:docs_input)
  in
  let line =
    Arg.(
      value
      & opt (some string) None
      & info [ "line" ] ~docv:"LINE"
          ~doc:"Specify data string, assuming the lines a separated by '\n'"
          ~docs:docs_input)
  in
  Term.term_result
    Term.(
      const (fun f l ->
          match (f, l) with
          | Some path, None -> Ok (File path)
          | None, Some txt -> Ok (Line txt)
          | None, None -> Error (`Msg "Must provide either --file or --line")
          | Some _, Some _ ->
              Error (`Msg "Cannot provide both --file and --line"))
      $ file $ line)

let main_term =
  let open Term in
  const (fun mode opt input ->
      let lines =
        match input with File path -> read_file path | Line s -> read_line s
      in
      let processed =
        match (mode, opt) with
        | Eager, LEDES1998B ->
            eager_ledes1998b lines;
            []
        | Eager, LEDES98BI ->
            eager_ledes98bi lines;
            []
        | Greedy, LEDES1998B -> greedy_ledes1998b lines
        | Greedy, LEDES98BI -> greedy_ledes98bi lines
      in
      List.iter print_endline processed;
      ())
  $ mode_term $ format_term $ input_term

let cmd =
  let doc =
    "A tool for parsing and validating LEDES file format\n\
     Example: ledes_cli_linux -- --eager --LEDES1998B --file /path/to/ledes199b"
  in
  let info = Cmd.info "ledes_cli" ~version:"0.1.0" ~doc in
  Cmd.v info main_term

let main () = exit (Cmd.eval cmd)
let () = main ()
