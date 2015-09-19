type closure_dump

val closures : unit -> closure_dump
val string_of_delta : closure_dump -> closure_dump -> string

val report_leaks : ?closures:bool -> (unit -> 'a Lwt.t) -> 'a Lwt.t
