open Ifloat
let delta = sqrt epsilon

type vec = {x:ifloat; y:ifloat; z:ifloat}
let zero = {x=of_float 0.; y=of_float 0.; z=of_float 0.}
let ( *| ) s r = {x = s *. r.x; y = s *. r.y; z = s *. r.z}
let ( +| ) a b = {x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z}
let ( -| ) a b = {x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z}
let dot a b = a.x *. b.x +. a.y *. b.y +. a.z *. b.z
let length r = sqrt(dot r r)
let unitise r = of_float 1. /. length r *| r

let ray_sphere orig dir center radius =
  let v = center -| orig in
  let b = dot v dir in
  let d2 = (b *. b -. dot v v +. radius *. radius) in
  if d2 < of_float 0. then infinity else
  let d = sqrt d2 in
  let t1 = b -. d and t2 = b +. d in
  if t2>of_float 0. then if t1>of_float 0. then t1 else t2 else infinity

let rec intersect orig dir (l, _ as hit) (center, radius, scene) =
  match ray_sphere orig dir center radius, scene with
  | l', _ when l' >= l -> hit
  | l', [] -> l', unitise (orig +| l' *| dir -| center)
  | _, scenes -> intersects orig dir hit scenes
and intersects orig dir hit = function
  | [] -> hit
  | scene::scenes -> intersects orig dir (intersect orig dir hit scene) scenes

let light = unitise {x=of_float 1.; y=of_float 3.; z= of_float (-2.)} and ss = 4

let rec ray_trace dir scene =
  let l, n = intersect zero dir (infinity, zero) scene in
  let g = dot n light in
  if g <= of_float 0. then of_float 0. else
    let p = l *| dir +| delta *| n in
    if fst (intersect p light (infinity, zero) scene) < infinity then of_float 0. else g

let rec create level c r =
  let obj = c, r, [] in
  if Pervasives.(level = 1) then obj else
    let a = of_float 3. *. r /. sqrt (of_float 12.) in
    let aux x' z' = create (level - 1) (c +| {x=x'; y=a; z=z'}) (of_float 0.5 *. r) in
    c, of_float 3. *. r, [obj; aux (-.a) (-.a); aux a (-.a); aux (-.a) a; aux a a]

let level, n =
  try int_of_string Sys.argv.(1), int_of_string Sys.argv.(2) with _ -> 6, 512

let scene = create level {x=of_float 0.; y= of_float (-1.); z=of_float (4.)} (of_float 1.);;

Printf.printf "P5\n%d %d\n255\n" n n;;
for y = n - 1 downto 0 do
  for x = 0 to n - 1 do
    let g = ref (of_float 0.) in
    for dx = 0 to ss - 1 do
      for dy = 0 to ss - 1 do
        let aux x d = of_int x -. of_int n /. of_float 2. +. of_int d /. of_int ss in
        let dir = unitise {x=aux x dx; y=aux y dy; z=of_int n} in
        g := !g +. ray_trace dir scene
      done;
    done;
    let g = of_float 0.5 +. of_float 255. *. !g /. of_int (ss*ss) in
    Printf.printf "%c" (char_of_int (to_int g))
  done;
done
