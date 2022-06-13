for i in *.fut
do
    futhark fmt $i >> ../tests_fmt/output.txt
done
