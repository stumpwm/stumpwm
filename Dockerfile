FROM ubuntu:20.04

ENV HOME /root/

RUN apt-get update \
    && apt-get install -y sbcl curl build-essential autoconf git \
    && curl -O https://beta.quicklisp.org/quicklisp.lisp \
    && sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" \
    && sbcl --load "/root/quicklisp/setup.lisp"  --eval "(progn (setf ql-util::*do-not-prompt* t)(ql:add-to-init-file))" \
    && sbcl --eval "(progn (ql:quickload '(clx cl-ppcre alexandria fiasco)))"

COPY . .

RUN ./autogen.sh \
    && ./configure \
    && make
