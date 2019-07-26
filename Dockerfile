#same container that golang use
FROM buildpack-deps:buster-curl

LABEL maintainer="ANAGO Ronnel <anagoandy@gmail.com>"
WORKDIR /opt/vlang
RUN apt-get -yq update && \
    apt-get install -y --no-install-recommends gcc clang make && \
    rm -rf /var/lib/apt/lists/*
COPY . .
RUN make && \
    ln -s /opt/vlang/v /usr/local/bin/v

CMD [ "bash" ]
