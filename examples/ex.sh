for f in `ls *.in`; do
	../render 400 400 100 $f $f $f.tga
done
