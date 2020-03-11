FILES=$(find . -type f -name "*.rkt" ! -name "wxme_converter.rkt")
#echo $FILES
for f in $FILES; do
  echo "Processing $f"
  racket ./wxme_converter.rkt $f > $f.tmp
  mv $f.tmp $f
done
