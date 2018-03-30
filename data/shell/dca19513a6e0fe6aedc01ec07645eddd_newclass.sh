#
#   Simple script to add a class to ZFL
#   Pass class name 'zfl_xxx' as single argument
#

CLASS=$1
if [ -z "$CLASS" ]; then
    echo "sh bin/newclass.sh zfl_xxx"
elif [ ! -d src/ ]; then
    echo "Please run in ZFL git root directory"
elif [ -f "src/$CLASS.cx" ]; then
    echo "Please don't try to create an existing class"
else
    echo "Creating files for class '$CLASS'..."
    UPPER=`echo $CLASS | tr [a-z] [A-Z]`

    echo "Creating new include/$CLASS.h..."
    sed "s/zfl_base/$CLASS/g; s/ZFL_BASE/$UPPER/g" < include/zfl_base.h > include/$CLASS.h
    sed "/zfl_base.h/a #include <$CLASS.h>" < include/zfl.h > include/zfl.1
    mv include/zfl.1 include/zfl.h

    echo "Creating new src/$CLASS.c..."
    sed "s/zfl_base/$CLASS/g; s/ZFL_BASE/$UPPER/g" < src/zfl_base.c > src/$CLASS.c
    sed "/zfl_base.h/a \    ../include/$CLASS.h \\\\" < src/Makefile.am > src/Makefile.1
    sed "/zfl_base.c/a \    $CLASS.c \\\\" < src/Makefile.1 > src/Makefile.2
    rm src/Makefile.1
    mv src/Makefile.2 src/Makefile.am

    sed "/zfl_base.h/a #include \"../include/$CLASS.h\"" < src/zfl_selftest.c > src/zfl_selftest.1
    sed "/zfl_base_test/a \    $CLASS\_test (verbose);" < src/zfl_selftest.1 > src/zfl_selftest.2
    rm src/zfl_selftest.1
    mv src/zfl_selftest.2 src/zfl_selftest.c

    echo "Creating new doc/$CLASS.txt..."
    sed "s/zfl_base/$CLASS/g; s/ZFL_BASE/$UPPER/g" < doc/zfl_base.txt > doc/$CLASS.txt
    sed "/zfl_base.7/a \    $CLASS.7 \\\\" < doc/Makefile.am > doc/Makefile.1
    mv doc/Makefile.1 doc/Makefile.am

    git add include/$CLASS.h src/$CLASS.c doc/Makefile.am src/Makefile.am include/zfl.h doc/$CLASS.txt src/zfl_selftest.c

    echo "Please edit new/modified code to fix up:"
    git status
fi
