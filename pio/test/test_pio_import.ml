open! Core

(* pio_import's verdict lines and exit code: 0 certified, 2 refused or not certified. *)
let run args =
  let command =
    String.concat
      ~sep:" "
      (("../bin/pio_import.exe" :: List.map args ~f:Filename.quote)
       @ [ "| grep '^; [a-z_0-9]* at\\|^; limit'; exit ${PIPESTATUS[0]}" ])
  in
  Out_channel.flush stdout;
  let status = Sys_unix.command ("bash -c " ^ Filename.quote command) in
  printf "exit %d\n" status
;;

let%expect_test "i2c at its own rate is refused on tHD;STA, by name" =
  run [ "pico_examples/i2c.pio"; "-program"; "i2c"; "-own-rate" ];
  [%expect
    {|
    ; i2c at k = 15, refused: tHD;STA
    ; limit: tHD;STA, SDA low to SCL low: the kernel proves 150 cycles, 3.125 us at 48 MHz, under 4.0 us (UM10204 Rev. 7, table 10, Standard-mode)
    exit 2
    |}]
;;

let%expect_test "-k and -own-rate together, or a rate the example has not, is a usage \
                 error"
  =
  run [ "pico_examples/i2c.pio"; "-program"; "i2c"; "-own-rate"; "-k"; "15" ];
  run [ "pico_examples/squarewave.pio"; "-own-rate" ];
  [%expect
    {|
    -k or -own-rate, not both
    exit 1
    squarewave: no rate of its own to run at
    exit 1
    |}]
;;
