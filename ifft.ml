(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)
open Ifloat

let pi = of_float 3.14159265358979323846

let tpi = of_float 2.0 *. pi

let (<..) = Pervasives.(<)
let (<>..) = Pervasives.(<>)
let (<=..) = Pervasives.(<=)

let fft px py np =
  let i = ref 2 in
  let m = ref 1 in

  while !i <.. np do
    i := !i + !i;
    m := !m + 1
  done;

  let n = !i in

  if n <>.. np then begin
    for i = np+1 to n do
      px.(i) <- of_float 0.0;
      py.(i) <- of_float 0.0
    done;
    print_string "Use "; print_int n;
    print_string " point fft"; print_newline()
  end;

  let n2 = ref(n+n) in
  for k = 1 to !m-1 do
    n2 := !n2 / 2;
    let n4 = !n2 / 4 in
    let e  = tpi /. of_int !n2 in

    for j = 1 to n4 do
      let a = e *. of_int(j - 1) in
      let a3 = of_float 3.0 *. a in
      let cc1 = cos(a) in
      let ss1 = sin(a) in
      let cc3 = cos(a3) in
      let ss3 = sin(a3) in
      let is = ref j in
      let id = ref(2 * !n2) in

        while !is <.. n do
          let i0r = ref !is in
          while !i0r <.. n do
             let i0 = !i0r in
             let i1 = i0 + n4 in
             let i2 = i1 + n4 in
             let i3 = i2 + n4 in
             let r1 = px.(i0) -. px.(i2) in
             px.(i0) <- px.(i0) +. px.(i2);
             let r2 = px.(i1) -. px.(i3) in
             px.(i1) <- px.(i1) +. px.(i3);
             let s1 = py.(i0) -. py.(i2) in
             py.(i0) <- py.(i0) +. py.(i2);
             let s2 = py.(i1) -. py.(i3) in
             py.(i1) <- py.(i1) +. py.(i3);
             let s3 = r1 -. s2 in
             let r1 = r1 +. s2 in
             let s2 = r2 -. s1 in
             let r2 = r2 +. s1 in
             px.(i2) <- r1*.cc1 -. s2*.ss1;
             py.(i2) <- -.s2*.cc1 -. r1*.ss1;
             px.(i3) <- s3*.cc3 +. r2*.ss3;
             py.(i3) <- r2*.cc3 -. s3*.ss3;
             i0r := i0 + !id
          done;
          is := 2 * !id - !n2 + j;
          id := 4 * !id
        done
    done
  done;

(************************************)
(*  Last stage, length=2 butterfly  *)
(************************************)

  let is = ref 1 in
  let id = ref 4 in

  while !is <.. n do
    let i0r = ref !is in
    while !i0r <=.. n do
      let i0 = !i0r in
      let i1 = i0 + 1 in
      let r1 = px.(i0) in
      px.(i0) <- r1 +. px.(i1);
      px.(i1) <- r1 -. px.(i1);
      let r1 = py.(i0) in
      py.(i0) <- r1 +. py.(i1);
      py.(i1) <- r1 -. py.(i1);
      i0r := i0 + !id
    done;
    is := 2 * !id - 1;
    id := 4 * !id
  done;

(*************************)
(*  Bit reverse counter  *)
(*************************)

  let j = ref 1 in

  for i = 1 to n - 1 do
    if i <.. !j then begin
      let xt = px.(!j) in
      px.(!j) <- px.(i);
      px.(i) <- xt;
      let xt = py.(!j) in
      py.(!j) <- py.(i);
      py.(i) <- xt
    end;
    let k = ref(n / 2) in
    while !k <.. !j do
      j := !j - !k;
      k := !k / 2
    done;
    j := !j + !k
  done;

  n


let test np =
  let enp = of_int np in
  let npm = np / 2 - 1 in
  let pxr = Array.make (np+2) (of_float 0.0)
  and pxi = Array.make (np+2) (of_float 0.0) in
  let t = pi /. enp in
  pxr.(1) <- (enp -. of_float 1.0) *. of_float 0.5;
  pxi.(1) <- of_float 0.0;
  let n2 = np / 2 in
  pxr.(n2+1) <- of_float (-0.5);
  pxi.(n2+1) <- of_float  0.0;

  for i = 1 to npm do
      let j = np - i in
      pxr.(i+1) <- of_float (-0.5);
      pxr.(j+1) <- of_float (-0.5);
      let z = t *. of_int i in
      let y = of_float (-0.5)*.(cos(z)/.sin(z)) in
      pxi.(i+1) <- y;
      pxi.(j+1) <- -.y
  done;
(**
  print_newline();
  for i=0 to 15 do Printf.printf "%d  %f  %f\n" i pxr.(i+1) pxi.(i+1) done;
**)
  let _ = fft pxr pxi np in
(**
  for i=0 to 15 do Printf.printf "%d  %f  %f\n" i pxr.(i+1) pxi.(i+1) done;
**)
  let zr = ref (of_float 0.0) in
  let zi = ref (of_float 0.0) in
  let kr = ref 0 in
  let ki = ref 0 in
  for i = 0 to np-1 do
      let a = abs_float(pxr.(i+1) -. of_int i) in
      if !zr < a then begin
         zr := a;
         kr := i
      end;
      let a = abs_float(pxi.(i+1)) in
      if !zi < a then begin
         zi := a;
         ki := i
      end
  done;
  !zr, !zi

let run i = test (1 lsl i)
let check i (zr, zi) =
  if Ifloat.(abs_float zr <= of_float 1e-8 && abs_float zi <= of_float 1e-8)
  then Printf.eprintf "ok %d\n" i
  else Printf.eprintf "fail %d\n n:%d zr:%f zi:%f "
      i (1 lsl i)
       (to_float zr)
       (to_float zi)

let () =
  for i = 4 to 22 do
    check i (run i)
  done
