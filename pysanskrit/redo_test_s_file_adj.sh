test_s_file_one_adj.sh '01'     1  9999
test_s_file_one_adj.sh '02' 10000 19999
test_s_file_one_adj.sh '03' 20000 29999
test_s_file_one_adj.sh '04' 30000 39999
test_s_file_one_adj.sh '05' 40000 49999

# /c/emacs-23.2/bin/emacs.exe --batch --file=../../lisp/start.el --eval='(eval-buffer)' --eval='(s-file-init-alt1 "tempin.txt" "prod/inputs" "temout.txt" "prod/outputs" 1 9999)' --eval='(progn (switch-to-buffer "*scratch*") (write-file "temp-scratch.txt"))'  >& temp-log.txt
