exception A_implementer of string

type value=S of string | I of int
and gen_fct= ((string, value) Hashtbl.t -> ((in_channel -> value) * string * gen_fct option) list)

type bc = {
  ic : in_channel;
  mutable nbits : int;
  mutable bits : int;
}

(* several reading functions *)

let little_endian_read_ui16 in_channel=
  let premiere_partie=input_byte in_channel in
  let seconde_partie=input_byte in_channel in
    (seconde_partie lsl 8) lor premiere_partie

let little_endian_read_ui32 in_channel=
  let premiere_partie=little_endian_read_ui16 in_channel in
  let seconde_partie=little_endian_read_ui16 in_channel in
    (seconde_partie lsl 16) lor premiere_partie

let little_endian_read_ui8n n ic=
  match n with
    |1->I (input_byte ic)
    |2->I (little_endian_read_ui16 ic)
    |4->I (little_endian_read_ui32 ic)
    |n->raise (A_implementer ("read_ui8*"^(string_of_int n)^"\n"))

let big_endian_read_ui16 in_channel=
  let premiere_partie=input_byte in_channel in
    let seconde_partie=input_byte in_channel in
      (premiere_partie lsl 8) lor seconde_partie

let big_endian_read_ui32 in_channel=
  let premiere_partie=big_endian_read_ui16 in_channel in
  let seconde_partie=big_endian_read_ui16 in_channel in
    (premiere_partie lsl 16) lor seconde_partie

let big_endian_read_ui8n n ic=
  match n with
    |1->I (input_byte ic)
    |2->I (big_endian_read_ui16 ic)
    |4->I (big_endian_read_ui32 ic)
    |n->raise (A_implementer ("read_ui8"^(string_of_int n)^"\n"))

let rec little_endian_read_bits b n=
  if b.nbits >= n then
    let c = b.nbits - n in
    let k = (b.bits asr c) land ((1 lsl n) - 1) in
    b.nbits <- c;
    k
  else
    let k = input_byte b.ic in
    if b.nbits >= 24 then
      let c = 8 + b.nbits - n in
      let d = b.bits land ((1 lsl b.nbits) - 1) in
      let d = (d lsl (8 - c)) lor (k lsr c) in
      b.bits <- k;
      b.nbits <- c;
      d
    else begin
      b.bits <- (b.bits lsl 8) lor k;
      b.nbits <- b.nbits + 8;
      little_endian_read_bits b n;
    end

let rec big_endian_read_bits b n=
  if b.nbits >= n then
    let c = b.nbits - n in
    let k = (b.bits asr c) land ((1 lsl n) - 1) in
    b.nbits <- c;
    k
  else
    let k = input_byte b.ic in
    if b.nbits >= 24 then
      let c = 8 + b.nbits - n in
      let d = b.bits land ((1 lsl b.nbits) - 1) in
      let d = (d lsl (8 - c)) lor (k lsr c) in
      b.bits <- k;
      b.nbits <- c;
      d
    else begin			
      b.bits <- (b.bits lsl 8) lor k;
      b.nbits <- b.nbits + 8;
      big_endian_read_bits b n;
    end

let little_endian_read_uin n ic=I (little_endian_read_bits {ic=ic; bits=0; nbits=0} n)

let big_endian_read_uin n ic=I (big_endian_read_bits {ic=ic; bits=0; nbits=0} n)

(* real start *)

let input_string n in_channel=
  let str=String.create n in let _=input in_channel str 0 n in S str

(* header parse *)

let header_parse in_channel lst=
  let header_hashtbl = Hashtbl.create 30 in
  let rec header_parseRC=function
    |(read_function, field_name, generating_function)::q-> (
      let field_value=read_function in_channel in
      let _=Hashtbl.add header_hashtbl field_name field_value in
      let _=match generating_function with
        |Some (f:gen_fct) -> header_parseRC (f header_hashtbl)
        |None -> ()
      in header_parseRC q
      )
    |[]->()
  in let _=header_parseRC lst in (in_channel, header_hashtbl)

