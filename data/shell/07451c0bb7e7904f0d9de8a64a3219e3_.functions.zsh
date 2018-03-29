# text-to-speech
say() {
    if [[ "$1" =~ -[a-z]{2} ]]; then
        local lang=${1#-}
        local text="${*#$1}"
    else
        local lang=${LANG%_*}
        local text="$*"
    fi;
    local url="http://translate.google.com/translate_tts?ie=UTF-8&tl=$lang&q=$text"
    mpv --really-quiet "${url// /%20}"
}

# move to trash
rm() {
    if [ `\du -c $@ | tail -n 1 | cut -f 1` -lt 50000 ]; then
        mv -t ~/.trash -v $@
    else
        echo Sorry, too big.
    fi
}

# real rm
rrm() {
    /bin/rm $@
}

# cd between rared and unrared version of the same release
rartoggle() {
    [[ "$PWD" =~ (/data|$HOME)/archive/* ]] || return 1
    [[ "$PWD" =~ /data ]] && cd /data $HOME || cd $HOME /data
}

