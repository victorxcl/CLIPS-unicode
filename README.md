CLIPS-unicode
=============

Make NASA CLIPS expert system tool to support unicode!

You can compile this source code by the steps below:

1. cd to the folder which contains clips and extra folders
2. gcc -O3 -Iclips -Iunicode clips/*.c unicode/*.c -o clips.out
3. copy `clips.out` to you PATH directory and renamed to `clips`, such as /usr/local/bin/clips


2014/9/28

You can compile this source code by the steps below:

1. cd clips
2. gcc -O3 *.c -oclips
3. copy clips to you PATH directory, such as /usr/local/bin

I have test it in my macbook pro in 2014/9/28.

2014/9/30

Add unicode folder. Now compile process are:

1. cd to the folder which contains clips and extra folders
2. gcc -O3 -Iclips -Iunicode clips/*.c unicode/*.c -o clips.out
3. copy `clips.out` to you PATH directory and renamed to `clips`, such as /usr/local/bin/clips

At first, I make all the files in unicode folders in a single folder `clips folder`, but I also
want unicode surport another languages, such as lua, and in the future to suport more languages in the
same way. So I split these files into different folders.

Add file `clips.hpp`. The main purpose is to make writing c++ extension easy. So added clips.hpp, 
and add samples to use this clips.hpp header.
