# ocaml-dump-closures
Collect closure code pointers (`Code_val`) from blocks in OCaml heap and detect leaks

Depends on objsize and lwt (the latter just for the convenience function
`Scan_heap.report_leaks`).
