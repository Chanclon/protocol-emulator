(** What the page shows for a paste, apart from js_of_ocaml, so a native build can be
    compared with the page's. *)

open! Core

module Report : sig
  type t =
    { verdict : string
    ; detail : string
    ; listing : string
    ; rows : string
    }
end

(** [generate.exe assemble -listing] and [-rows], with its flags that the page offers,
    naming whichever of the assembler, analyser and kernel refuses. *)
val check
  :  ?period:int
  -> single_capture_edge:bool
  -> capture_pin:int
  -> capture_falling:bool
  -> string
  -> Report.t

module Pio_report : sig
  type t =
    { verdict : string
    ; report : string
    }
end

(** [pio_check] on [source] with [spec]'s flags, one a line: "passed" where it exits 0,
    "failed" on any FAIL or ERROR, refused where it gives a usage error. *)
val check_pio : source:string -> spec:string -> Pio_report.t

module Diagram_report : sig
  type t =
    { verdict : string
    ; detail : string
    ; steps : string
    ; words : string
    ; lanes : string list
    ; levels : int array
    }
end

val cycle_ns : clock_hz:int -> float

(** The diagram compiled at [clock_hz], then the model of the core running it on [frames],
    a line a frame of 0s and 1s. *)
val run_diagram : source:string -> frames:string -> clock_hz:int -> Diagram_report.t
