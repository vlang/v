FROM thevlang/vlang:debian-build


COPY ./ /src/

WORKDIR /src

RUN make fresh_tcc CC=clang && make CC=clang

RUN /src/v /src/vlib/net/websocket/tests/autobahn/autobahn_server.v
RUN chmod +x /src/vlib/net/websocket/tests/autobahn/autobahn_server
ENTRYPOINT [ "/src/vlib/net/websocket/tests/autobahn/autobahn_server" ]
