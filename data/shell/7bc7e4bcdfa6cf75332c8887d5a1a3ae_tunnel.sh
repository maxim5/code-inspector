# simple tunnel to my ittc machine
#
echo please give the ssh.ittc.ku.edu pass
ssh -p62 andygill@ssh.ittc.ku.edu -L 8022:ireland:22 -N -f
