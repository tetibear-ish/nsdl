let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let context_around src pos =
  let len = String.length src in
  let start = max 0 (pos - 30) in
  let stop = min len (pos + 30) in
  String.sub src start (stop - start)

let () =
  match Sys.argv with
  | [| _; path |] -> (
    let src = read_file path in
    let lexbuf = Lexing.from_string src in
    try
      let prog = Nsdl.Parser.program Nsdl.Lexer.token lexbuf in
      print_string (Nsdl.Pretty.program_to_string prog);
      Printf.printf "-- parsed %d top-level declaration(s) from %s\n" (List.length prog) path
    with
    | Nsdl.Lexer.Lex_error msg ->
      let pos = Lexing.lexeme_start lexbuf in
      Printf.eprintf "lex error in %s at byte %d: %s\n  ...%s...\n" path pos msg
        (context_around src pos);
      exit 1
    | Nsdl.Parser.Error ->
      let pos = Lexing.lexeme_start lexbuf in
      Printf.eprintf "parse error in %s at byte %d near %S\n  ...%s...\n" path pos
        (Lexing.lexeme lexbuf) (context_around src pos);
      exit 1
    | Failure msg ->
      Printf.eprintf "failure in %s: %s\n" path msg;
      exit 1)
  | _ ->
    Printf.eprintf "usage: nsdl <file.nsdl>\n";
    exit 1
