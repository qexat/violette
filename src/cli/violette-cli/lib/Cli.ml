open Cmdliner

module Toplevel = struct
  open Term.Syntax

  let hook ~show_styling =
    let toplevel = Toplevel.create ~show_styling () in
    Toplevel.run toplevel;
    0

  let term =
    let+ show_styling =
      Arg.(
        value
        & opt
            (enum
               [ ("never", `Never)
               ; ("always", `Always)
               ; ("auto", `Auto)
               ])
            `Auto
        & info
            [ "show-styling" ]
            ~doc:"print ANSI escape sequences")
    in
    hook ~show_styling

  let command = Cmd.v (Cmd.info "toplevel") term
end

let program =
  let doc = "The Violette programming language toolchain." in
  Cmd.group
    (Cmd.info "violette" ~version:"<insert version here>" ~doc)
    [ Toplevel.command ]

let main () = Cmd.eval' program
