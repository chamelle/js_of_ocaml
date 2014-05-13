(* Js_of_ocaml toplevel
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Lwt
open Compiler

module Top : sig
  val setup : unit -> unit
  val exec : string -> unit
  val initialize : unit -> unit
end = struct
  let split_primitives p =
    let len = String.length p in
    let rec split beg cur =
      if cur >= len then []
      else if p.[cur] = '\000' then
        String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
      else
        split beg (cur + 1) in
    Array.of_list(split 0 0)

  let split_char sep p =
    let len = String.length p in
    let rec split beg cur =
      if cur >= len then
        [String.sub p beg (cur - beg)]
      else if p.[cur] = sep then
        String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
      else
        split beg (cur + 1) in
    split 0 0

  let setup () =
    Hashtbl.add Toploop.directive_table "enable" (Toploop.Directive_string Option.Optim.enable);
    Hashtbl.add Toploop.directive_table "disable" (Toploop.Directive_string Option.Optim.disable);
    Hashtbl.add Toploop.directive_table "debug_on" (Toploop.Directive_string Option.Debug.enable);
    Hashtbl.add Toploop.directive_table "debug_off" (Toploop.Directive_string Option.Debug.disable);
    Hashtbl.add Toploop.directive_table "tailcall" (Toploop.Directive_string (fun s ->
      let x = Option.Tailcall.of_string s in
      Option.Tailcall.set x));
    Topdirs.dir_directory "/cmis";
    let initial_primitive_count =
      Array.length (split_primitives (Symtable.data_primitive_names ())) in

    let compile s =
      let prims =
        split_primitives (Symtable.data_primitive_names ()) in
      let unbound_primitive p =
        try ignore (Js.Unsafe.eval_string p); false with _ -> true in
      let stubs = ref [] in
      Array.iteri
        (fun i p ->
           if i >= initial_primitive_count && unbound_primitive p then
             stubs :=
               Format.sprintf
                 "function %s(){caml_failwith(\"%s not implemented\")}" p p
               :: !stubs)
        prims;
      let output_program = Driver.from_string prims s in
      let b = Buffer.create 100 in
      output_program (Pretty_print.to_buffer b);
      let res = Buffer.contents b in
      let res = String.concat "" !stubs ^ res in
      res
    in
    Js.Unsafe.global##toplevelCompile <- compile (*XXX HACK!*)


  let refill_lexbuf s p buffer len =
    if !p = String.length s
    then 0
    else
      let len',nl =
        try String.index_from s !p '\n' - !p + 1,false
        with _ -> String.length s - !p,true in
      let len'' = min len len' in
      String.blit s !p buffer 0 len'';
      p:=!p + len'';
      len''

  let exec' s =
    let lb = Lexing.from_string s in
    try
      List.iter
        (fun phr ->
           if not (Toploop.execute_phrase false Format.std_formatter phr) then raise Exit)
        (!Toploop.parse_use_file lb)
    with
    | Exit -> ()
    | x    -> Errors.report_error Format.err_formatter x

  let exec s =
    let lb = Lexing.from_function (refill_lexbuf s (ref 0)) in
    try
      while true do
        try
          let phr = !Toploop.parse_toplevel_phrase lb in
          ignore(Toploop.execute_phrase true Format.std_formatter phr)
        with
          End_of_file ->
          raise End_of_file
        | x ->
          Errors.report_error Format.err_formatter x
      done
    with End_of_file ->
      flush_all ()

  let initialize () =
    Toploop.initialize_toplevel_env ();
    Toploop.input_name := "//toplevel//";
end

let trim s =
  let ws c = c = ' ' || c = '\t' || c = '\n' in
  let len = String.length s in
  let start = ref 0 in
  let stop = ref (len - 1) in
  while !start < len && (ws s.[!start])
  do incr start done;
  while !stop > !start && (ws s.[!stop])
  do decr stop done;
  String.sub s !start (!stop - !start + 1)

let run _ =
  let caml_chan = open_out "/dev/caml" in
  let caml_ppf = Format.formatter_of_out_channel caml_chan in
  let tmp = !Toploop.print_out_phrase in
  Toploop.print_out_phrase:= (fun fmt outcome -> tmp caml_ppf outcome);

  let execute p =
    let content = trim p in
    let len = String.length content in
    let content =
      if content = ""
      || (len > 2
          && content.[len - 1] = ';'
          && content.[len - 2] = ';')
      then p
      else p ^ ";;"
    in
    Top.exec content
  in

  let postMessage = Js.Unsafe.variable "postMessage" in

  let js_object = Js.Unsafe.variable "Object" in

  let send_string chan s =
    let o = jsnew js_object () in
    ignore begin
      Js.Unsafe.set o (Js.string chan) (Js.string s);
      Js.Unsafe.call postMessage (Js.Unsafe.variable "self") [|Js.Unsafe.inject o|]
    end
  in

  Sys_js.set_channel_flusher caml_chan (send_string "caml");
  Sys_js.set_channel_flusher stdout (send_string "stdout");
  Sys_js.set_channel_flusher stderr (send_string "stderr");

  Top.setup ();
  Top.initialize ();

  let onmessage event = execute (Js.to_string event##data##caml) in
  Js.Unsafe.set (Js.Unsafe.variable "self") (Js.string "onmessage") onmessage

let _ = run ()
