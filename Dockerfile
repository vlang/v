#same container that golang use
FROM buildpack-deps:buster-curl

LABEL maintainer="ANAGO Ronnel <anagoandy@gmail.com>"
WORKDIR /opt/vlang
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends gcc clang make git && \
    apt-get clean && rm -rf /var/cache/apt/archives/* && \
    rm -rf /var/lib/apt/lists/*
COPY . .
RUN make && \
    ln -s /opt/vlang/v /usr/local/bin/v

CMD [ "v" ]
