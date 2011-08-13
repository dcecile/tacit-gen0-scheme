shell: Makefile tinyscheme-src tinyscheme-bin.c
	gcc -o tinyscheme-bin \
          -std=gnu99 \
          -DSTANDALONE=0 \
          -DUSE_INTERFACE=1 \
          -DUSE_MATH=0 \
          -O3 \
          tinyscheme-bin.c \
          tinyscheme-src/scheme.c

TINY_VER = 1.40
TINY_SUM = 776c42ac5528d15e897ef052fa4b0420
tinyscheme-src:
	wget -O tiny.tar.gz http://downloads.sourceforge.net/project/tinyscheme/tinyscheme/tinyscheme-${TINY_VER}/tinyscheme-${TINY_VER}.tar.gz && \
        echo "${TINY_SUM}  tiny.tar.gz" > tiny.sum && \
        md5sum -w -c tiny.sum && \
        zcat tiny.tar.gz | tar xv && \
        mv tinyscheme-${TINY_VER} tinyscheme-src && \
        rm tiny.tar.gz tiny.sum
