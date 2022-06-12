for i in *.fut
do
    futhark fmt $i >> output.txt
done
