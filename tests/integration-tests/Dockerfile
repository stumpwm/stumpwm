FROM ubuntu:20.04

ARG STUMPWM_CI=yes

USER root
WORKDIR /root

ENV DISPLAY=":7"
ARG userUID
RUN useradd -u $userUID -m -s /bin/bash user

COPY tests/integration-tests/container-scripts/install-deps .
RUN ./install-deps

COPY . stumpwm
RUN cd stumpwm \
    && mv tests/integration-tests/container-scripts/* /usr/local/bin/ \
    && mv tests/integration-tests/stumpwm.d /home/user/.stumpwm.d \
    && chown -R user:user /home/user/.stumpwm.d \
    && ./autogen.sh \
    && ./configure \
    && make \
    && make install

USER user
WORKDIR /home/user
