#example of case
case $1 in
  "root" )
    echo "you inform root"
    ;;
  "normal" )
    echo "you informed normal user"
    ;;
  * )
    echo "you didnt informed anything :P"
esac

