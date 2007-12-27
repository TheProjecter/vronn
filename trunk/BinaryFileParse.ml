exception A_implementer of string
exception Value_not_equal_check_value of string

type value=S of string | I of int
and gen_fct= ((string, value) Hashtbl.t -> ((in_channel -> value) * string * gen_fct option) list)

let input_byte ic= (input_byte ic)

(* several reading functions *)

let little_endian_read_ui16 in_channel=
  let  premiere_partie=input_byte in_channel in
    let  seconde_partie=input_byte in_channel in
       ((seconde_partie lsl 8) lor premiere_partie)

let little_endian_read_ui32 in_channel=
  let  premiere_partie=little_endian_read_ui16 in_channel in
  let  seconde_partie=little_endian_read_ui16 in_channel in
       ((seconde_partie lsl 16) lor premiere_partie)

let little_endian_read_ui8n n ic=
  match n with
    |1->I (input_byte ic)
    |2->I (little_endian_read_ui16 ic)
    |4->I (little_endian_read_ui32 ic)
    |n->raise  (A_implementer ("read_ui8*"^(string_of_int n)^"\n"))

let big_endian_read_ui16 in_channel=
  let  premiere_partie=input_byte in_channel in
    let  seconde_partie=input_byte in_channel in
      ((premiere_partie lsl 8) lor seconde_partie)

let big_endian_read_ui32 in_channel=
  let  premiere_partie=big_endian_read_ui16 in_channel in
  let  seconde_partie=big_endian_read_ui16 in_channel in
      ((premiere_partie lsl 16) lor seconde_partie)

let big_endian_read_ui8n n ic=
  match n with
    |1->I (input_byte ic)
    |2->I (big_endian_read_ui16 ic)
    |4->I (big_endian_read_ui32 ic)
    |n->raise  (A_implementer ("read_ui8"^(string_of_int n)^"\n"))

(* real start *)

let input_string n in_channel=
  let str=String.create n in let _=input in_channel str 0 n in S str

(* header parse *)

let header_parse in_channel lst=
  let header_hashtbl = Hashtbl.create 30  in
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

