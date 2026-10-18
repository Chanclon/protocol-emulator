open! Core
open! Hardcaml
open Hardcaml_lws
open! Hardcaml_waveterm
open Expect_test_helpers_core
open Protocol_emulator
module Harness = Hardcaml_test_harness.Lws_harness.Make (Host_fifo.I) (Host_fifo.O)

let ( <--. ) = Bits.( <--. )
let depth = 8

module Op = struct
  type t =
    { push : int option
    ; pop : bool
    ; flush : bool
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind push = Option.quickcheck_generator (Int.gen_incl 0 0xffff) in
    (* push-heavy, so the fifo reaches full and the corners around it *)
    let%bind pop = Int.gen_incl 0 2 >>| ( = ) 0 in
    let%map flush = Int.gen_incl 0 15 >>| ( = ) 0 in
    { push; pop; flush }
  ;;
end

(* The cycle's pop comes first, so a full fifo that pops takes the push; a flush loses the
   push with the rest. *)
let model_step queue { Op.push; pop; flush } =
  if flush
  then Queue.clear queue
  else (
    if pop then ignore (Queue.dequeue queue : int option);
    Option.iter push ~f:(fun word ->
      if Queue.length queue < depth then Queue.enqueue queue word))
;;

let drive (inputs : _ Host_fifo.I.t) { Op.push; pop; flush } =
  inputs.push.valid := Bits.of_bool (Option.is_some push);
  inputs.push.value <--. Option.value push ~default:0;
  inputs.pop := Bits.of_bool pop;
  inputs.flush := Bits.of_bool flush
;;

let clear (h @ local) (inputs : _ Host_fifo.I.t) =
  drive inputs { push = None; pop = false; flush = false };
  inputs.clocking.clear := Bits.vdd;
  Lws.step h;
  inputs.clocking.clear := Bits.gnd
;;

let%expect_test "the fifo matches a queue" =
  Harness.run
    ~random_initial_state:`All
    ~create:Host_fifo.hierarchical
    (fun (h @ local) ~inputs ~outputs ->
       let o = Before_and_after_edge.before_edge outputs in
       let check queue (op : Op.t) =
         let level = Bits.to_unsigned_int !(o.level) in
         require_equal ~message:"level" (module Int) level (Queue.length queue);
         require_equal
           ~message:"empty"
           (module Bool)
           (Bits.to_bool !(o.empty))
           (Queue.is_empty queue);
         (* [full] is low in a cycle that pops *)
         require_equal
           ~message:"full"
           (module Bool)
           (Bits.to_bool !(o.full))
           (Queue.length queue = depth && not op.pop);
         Option.iter (Queue.peek queue) ~f:(fun head ->
           require_equal
             ~message:"head"
             (module Int)
             (Bits.to_unsigned_int !(o.head))
             head)
       in
       (* [h] is local, so the trials are drawn first rather than run by [Quickcheck.test] *)
       let trials =
         Quickcheck.random_sequence
           ~seed:(`Deterministic "host_fifo")
           (List.quickcheck_generator Op.quickcheck_generator)
         |> Fn.flip Sequence.take 200
         |> Sequence.to_array
         |> Array.map ~f:Array.of_list
       in
       for trial = 0 to Array.length trials - 1 do
         clear h inputs;
         let queue = Queue.create () in
         for n = 0 to Array.length trials.(trial) - 1 do
           let op = trials.(trial).(n) in
           drive inputs op;
           Lws.step h;
           (* the outputs are those before the edge, so the state this op found *)
           check queue op;
           model_step queue op
         done
       done);
  [%expect {| |}]
;;

(* Fill it, pop and push together when full, push when full, then flush with a push. *)
let%expect_test "waveform of the corners at full and flush" =
  let display_rules =
    [ "push$valid"; "push$value"; "pop"; "flush"; "head"; "level"; "empty"; "full" ]
    |> List.map ~f:(fun name ->
      Display_rule.port_name_is name ~wave_format:(Bit_or Unsigned_int))
  in
  let idle = { Op.push = None; pop = false; flush = false } in
  let ops =
    List.init depth ~f:(fun n -> { idle with push = Some (n + 1) })
    @ [ idle
      ; { idle with pop = true; push = Some 9 }
      ; { idle with push = Some 10 }
      ; { idle with flush = true; push = Some 11 }
      ; idle
      ]
  in
  Harness.run
    ~create:Host_fifo.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:12
        ~display_width:92
        ~wave_width:1
        waves)
    (fun (h @ local) ~inputs ~outputs:_ ->
      clear h inputs;
      List.iter ops ~f:(fun op ->
        drive inputs op;
        Lws.step h);
      drive inputs idle;
      Lws.step ~n:2 h);
  [%expect
    {|
    ┌Signals───┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │push$valid││    ┌───────────────────────────────┐   ┌───────────┐                         │
    │          ││────┘                               └───┘           └───────────────          │
    │          ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───────────────          │
    │push$value││ 0  │1  │2  │3  │4  │5  │6  │7  │8  │0  │9  │10 │11 │0                        │
    │          ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───────────────          │
    │pop       ││                                        ┌───┐                                 │
    │          ││────────────────────────────────────────┘   └───────────────────────          │
    │flush     ││                                                ┌───┐                         │
    │          ││────────────────────────────────────────────────┘   └───────────────          │
    │          ││────────┬───────────────────────────────────┬───────┬───────────────          │
    │head      ││ 0      │1                                  │2      │0                        │
    │          ││────────┴───────────────────────────────────┴───────┴───────────────          │
    │          ││────────┬───┬───┬───┬───┬───┬───┬───┬───────────────┬───────────────          │
    │level     ││ 0      │1  │2  │3  │4  │5  │6  │7  │8              │0                        │
    │          ││────────┴───┴───┴───┴───┴───┴───┴───┴───────────────┴───────────────          │
    │empty     ││────────┐                                           ┌───────────────          │
    │          ││        └───────────────────────────────────────────┘                         │
    │full      ││                                    ┌───┐   ┌───────┐                         │
    │          ││────────────────────────────────────┘   └───┘       └───────────────          │
    └──────────┘└──────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
