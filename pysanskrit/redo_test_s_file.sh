test_s_file_one.sh '01'     1  9999
test_s_file_one.sh '02' 10000 19999
test_s_file_one.sh '03' 20000 29999
test_s_file_one.sh '04' 30000 39999
test_s_file_one.sh '05' 40000 49999
test_s_file_one.sh '06' 50000 59999
test_s_file_one.sh '07' 60000 69999
test_s_file_one.sh '08' 70000 79999

test_s_file_one.sh '09' 80000 89999
test_s_file_one.sh '10' 90000 99999
test_s_file_one.sh '11' 100000 109999

# /c/emacs-23.2/bin/emacs.exe --batch --file=../../lisp/start.el --eval='(eval-buffer)' --eval='(s-file-init-alt1 "tempin.txt" "prod/inputs" "temout.txt" "prod/outputs" 1 9999)' --eval='(progn (switch-to-buffer "*scratch*") (write-file "temp-scratch.txt"))'  >& temp-log.txt
