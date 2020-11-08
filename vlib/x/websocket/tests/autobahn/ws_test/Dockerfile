FROM thevlang/vlang:buster-build


COPY ./ /src/

WORKDIR /src

RUN make CC=clang 

RUN /src/v /src/vlib/x/websocket/tests/autobahn/autobahn_server.v
RUN chmod +x /src/vlib/x/websocket/tests/autobahn/autobahn_server
ENTRYPOINT [ "/src/vlib/x/websocket/tests/autobahn/autobahn_server" ]
