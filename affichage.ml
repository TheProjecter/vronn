let rec segment img ((x1,y1) as p1) ((x2,y2) as p2) =
  let x,y= (x1+x2)/2,(y1+y2)/2 in
  Rgb24.set img x (img.Rgb24.height - y) {Color.r = 0; Color.g = 0; Color.b = 0};
  if abs(x1-x2) > 1 || abs(y1-y2) > 1 then (segment img p1 (x,y); segment img (x,y) p2)

let rec maxtaille lst = 
match lst with
	|[]->assert false
	| [t] -> t,1
	| t::q-> let om,ot=maxtaille q in max t om,ot+1

let affiche file list =
	let max,n = maxtaille list and curx,cury = ref 0, ref 0 in
	let img = Rgb24.make (5*n+20) (int_of_float (1000. *. max) + 20) {Color.r = 255; Color.g = 255; Color.b = 255}
	and outchan = open_out file in
	let rec affR lst =
		match lst with
      |[]->print_endline "Nothing done"
		  |[t]-> let y,x = int_of_float (1000. *. t) + 10,!curx+5 in segment img (!curx,!cury) (x,y) ; curx:=x; cury:=y ; Printf.fprintf outchan "%f" t
      |t::q-> let y,x = int_of_float (1000. *. t) + 10,!curx+5 in segment img (!curx,!cury) (x,y) ; curx:=x; cury:=y ; Printf.fprintf outchan "%f;" t; affR q
	in let b,a=(int_of_float (List.hd list *. 1000.)),10 in curx:=a; cury:=b; affR (List.tl list);
close_out outchan;
  Images.save (file ^ ".png") None [] (Images.Rgb24 img)
