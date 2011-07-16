shell: Makefile tinyscheme shell.c
	gcc -o shell \
          -std=gnu99 \
          -DSTANDALONE=0 \
          -DUSE_INTERFACE=1 \
          -DUSE_MATH=0 \
          shell.c \
          tinyscheme/scheme.c

TINY_VER = 1.40
TINY_SUM = 776c42ac5528d15e897ef052fa4b0420
tinyscheme:
	wget -O tiny.tar.gz http://downloads.sourceforge.net/project/tinyscheme/tinyscheme/tinyscheme-${TINY_VER}/tinyscheme-${TINY_VER}.tar.gz && \
        echo "${TINY_SUM}  tiny.tar.gz" > tiny.sum && \
        md5sum -w -c tiny.sum && \
        zcat tiny.tar.gz | tar xv && \
        mv tinyscheme-${TINY_VER} tinyscheme && \
        rm tiny.tar.gz tiny.sum
