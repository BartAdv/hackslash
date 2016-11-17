dir=$(cd -P -- "$(dirname -- "$0")" && pwd -P)/..
echo $dir

cd $dir/freeablo
git submodule init
git submodule update

cd $dir/freeablo
cmake .
make freeablo_lib
