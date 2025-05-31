#same container that golang use
FROM buildpack-deps:buster-curl

LABEL maintainer="ANAGO Ronnel <anagoandy@gmail.com>"
WORKDIR /opt/vlang

ARG USE_LOCAL

RUN apt update && \
    DEBIAN_FRONTEND=noninteractive apt install -y --no-install-recommends gcc clang make git binutils && \
    apt clean && rm -rf /var/cache/apt/archives/* && \
    rm -rf /var/lib/apt/lists/*

COPY . /vlang-local

RUN if [ -z "${USE_LOCAL}" ] ; then \
      git clone --depth=1 https://github.com/vlang/v /opt/vlang && \
      rm -rf /vlang-local ; \
    else \
      mv /vlang-local/* . && \
      rm -rf /vlang-local ; \
    fi

RUN make && \
    ln -s /opt/vlang/v /usr/local/bin/v

CMD [ "v" ]
