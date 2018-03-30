# example of "up" command:
#
# $ pwd
# /home/joonas/work/stuffs/world_destroyer_script/src
# $ up stuf
# $ pwd
# /home/joonas/work/stuffs
function up()
{
	# pure bash variant
	dir="$PWD"; while ! echo "$dir" | egrep -qi "$1[^/]*$"; do olddir="$dir"; dir="$(dirname "$dir")"; [ "$dir" == "$olddir" ] && { echo "not found: $1" >&2 ; return 1; }; done; cd "$dir";

	# node.js variant
	## cd $(node -e "console.log((new RegExp('.+$1.*?/','i').exec('$PWD') || ['/match/not/found'])[0])")
}

