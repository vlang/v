#same container that golang use
FROM buildpack-deps:buster-curl

LABEL maintainer="ANAGO Ronnel <anagoandy@gmail.com>"
WORKDIR /v
COPY . .
RUN apt-get -yq update && \
    apt-get install -y gcc \
        make && \
    make && \
    ln -s /v/v /usr/local/bin/v

CMD [ "bash" ]

