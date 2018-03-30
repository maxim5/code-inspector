# Example use to add datetime in filenames
#
# $ tar cvjf foobar-`datec`.tar.bz2 foobar
#
# foobar-20091008103727.tar.bz2
alias datec='date "+%Y%m%d%H%M%S"'
