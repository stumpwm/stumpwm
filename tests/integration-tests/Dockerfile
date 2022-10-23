FROM ubuntu:20.04

USER root
WORKDIR /root

RUN DEBIAN_FRONTEND=noninteractive apt-get -qy update \
    && DEBIAN_FRONTEND=noninteractive apt-get -qy install \
        sbcl curl build-essential autoconf git \
        farbfeld netpbm procps x11-apps x11-xserver-utils xdotool xvfb \
        xterm \
    && rm -rf /var/lib/apt/lists/* \
    && curl -O https://beta.quicklisp.org/quicklisp.lisp \
    && sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" \
    && sbcl --load "/root/quicklisp/setup.lisp"  --eval "(progn (setf ql-util::*do-not-prompt* t)(ql:add-to-init-file))" \
    && sbcl --eval "(progn (ql:quickload '(clx cl-ppcre alexandria fiasco)))"

COPY tests/integration-tests/container-scripts/* /usr/local/bin/
COPY . stumpwm

RUN cd stumpwm \
    && ./autogen.sh \
    && ./configure \
    && make \
    && make install

ENV DISPLAY=":7"
ARG userUID
RUN useradd -u $userUID -m -s /bin/bash user
USER user
WORKDIR /home/user
