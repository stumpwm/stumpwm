FROM debian:buster
WORKDIR /root
COPY tests/integration-tests/container-scripts/install-* /usr/local/bin/
RUN install-packages farbfeld netpbm procps x11-apps x11-xserver-utils xdotool xvfb
RUN install-sbcl
RUN install-stumpwm-dependencies
RUN install-dependencies-for-test-cases
ENV DISPLAY=":7"
ARG userUID
RUN useradd -u $userUID -m -s /bin/bash user
COPY tests/integration-tests/container-scripts/* /usr/local/bin/
RUN mkdir -p stumpwm-src
COPY *.ac *.asd *.in *.lisp *.sh .*.el .*.yml [A-Z]* stumpwm-src/
RUN install-stumpwm
USER user
WORKDIR /home/user
