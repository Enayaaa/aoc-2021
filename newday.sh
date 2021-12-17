#!/bin/bash
set -e 
set -o pipefail

if [ ! -e $1/main.hs ]
then
    mkdir $1
    touch $1/main.hs
    echo -e \
"\n\n\nprepare = lines\n\n\
solve1 = id\n\n\
solve2 = id\n\n\
main = do\n\
    file <- readFile \"test_input.txt\"\n\
    --file <- readFile \"input.txt\"\n\
    putStrLn \$ \"Part 1 :\" ++ (show . solve1 . prepare \$ file)\n\
    --putStrLn \$ \"Part 2 :\" ++ (show . solve2 . prepare \$ file)"\
> $1/main.hs
    cd $1
    touch test_input.txt input.txt
    nvim main.hs
else
    echo "day folder already exists!"
fi

