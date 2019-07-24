#same container that golang use
FROM buildpack-deps:buster-curl

LABEL maintainer="ANAGO Ronnel <anagoandy@gmail.com>"
WORKDIR /etc/vlang
COPY . .
RUN apt-get -yq update && \
    apt-get install -y gcc clang make && \
    make && \
    ln -s /etc/vlang/v /usr/local/bin/v

CMD [ "bash" ]
