gamme=( 3 4 5 6 )
note=( c d e f g a b )

gmax=$((${#gamme[@]}-1))
nmax=$((${#note[@]}-1))

if [[ "$1" = "1" ]]; then 
  for g1 in `seq 0 $((${#gamme[@]}-1))`; do
    for n1 in `seq 0 $((${#note[@]}-1))`; do
      midge -o `dirname $0`/${gamme[$g1]}${note[$n1]}.mid <<EOF
@head {
	\$time_sig 4/4
	\$tempo 80
}

@body {

		@channel 1 {
		\$patch 74 # flute
    \$length 4

		${note[$n1]}${gamme[$g1]}
	}
}
EOF
			
      timidity -Ow `dirname $0`/${gamme[$g1]}${note[$n1]}.mid
    done
  done
elif [[ "$1" = "2" ]]; then
  for g1 in `seq 0 $gmax`; do
    for n1 in `seq 0 $nmax`; do
      for g2 in `seq $g1 $gmax`; do
	for n2 in `seq $(if [[ "$g1" = "$g2" ]]; then expr $n1 + 1; else echo 0; fi) $nmax`; do
	  midge -o `dirname $0`/${gamme[$g1]}${note[$n1]},${gamme[$g2]}${note[$n2]}.mid <<EOF
@head {
	\$time_sig 4/4
	\$tempo 80
}

@body {

		@channel 1 {
		\$patch 74 # flute
    \$length 4

		( ${note[$n1]}${gamme[$g1]} ${note[$n2]}${gamme[$g2]} )
	}
}
EOF
			
	  timidity -Ow `dirname $0`/${gamme[$g1]}${note[$n1]},${gamme[$g2]}${note[$n2]}.mid

	done
      done
    done
  done
else echo "Usage : $0 X, où X est 0 ou 1 :
0 génère les notes isolées et 1 génère les couples"; fi